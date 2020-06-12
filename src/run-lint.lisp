(defpackage #:sblint/run-lint
  (:use #:cl)
  (:import-from #:sblint/utilities/file-position
                #:file-position-to-line-and-column)
  (:import-from #:sblint/utilities/pathname
                #:file-in-directory-p
                #:make-relative-pathname)
  (:import-from #:sblint/utilities/streams
                #:with-muffled-streams)
  (:import-from #:sblint/utilities/compiler-aux
                #:ignorable-compiler-warning-p
                #:compiler-note-position)
  (:import-from #:sblint/utilities/logger
                #:*enable-logger*
                #:do-log)
  (:import-from #:sblint/utilities/asdf
                #:all-required-systems
                #:directory-asd-files
                #:asdf-target-system-locator
                #:ensure-uncached-file
                #:find-system-from-pathname-name)
  (:import-from #:sblint/utilities/error
                #:sblint-error
                #:sblint-compilation-error)
  (:import-from #:sblint/utilities/quicklisp
                #:in-quicklisp-directory-p
                #:install-required-systems)
  (:import-from #:uiop
                #:file-exists-p
                #:directory-exists-p
                #:subdirectories
                #:directory-files)
  (:export #:run-lint-file
           #:run-lint-asd
           #:run-lint-directory
           #:sblint-error
           #:*enable-logger*))
(in-package #:sblint/run-lint)


(defun get-location (condition)
  (let* ((context (sb-c::find-error-context nil))
         (file (or (and context
                        (sb-c::compiler-error-context-file-name context))
                   *load-truename*
                   *compile-file-truename*))
         (position (cond
                     ((and (typep file '(or string pathname))
                           context)
                      (compiler-note-position file context))
                     ((typep condition 'reader-error)
                      (let ((stream (stream-error-stream condition)))
                        (file-position stream)))
                     ((and (typep condition 'sb-c::encapsulated-condition)
                           (typep (sb-int:encapsulated-condition condition)
                                  'sb-c::input-error-in-compile-file))
                      ;; reader-error in compile-file
                      (let ((stream (slot-value (sb-int:encapsulated-condition
                                                 (sb-int:encapsulated-condition condition))
                                                'stream)))
                        (setq file
                              (SB-IMPL::FD-STREAM-FILE
                               (slot-value (slot-value (sb-int:encapsulated-condition condition) 'condition)
                                           'sb-int::stream)))
                        (file-position stream)))
                     (t nil))))
    (values file position)))

(defun condition-name-to-print (condition)
  (typecase condition
    (style-warning
     "style-warning")
    (otherwise
     (string-downcase (type-of condition)))))

(defun print-note (file position condition stream)
  (multiple-value-bind (line column)
      (file-position-to-line-and-column file position)
    (let ((*print-pretty* nil))
      (handler-case
          (format stream "~&~A:~A:~A: ~A: ~A~%"
                  (make-relative-pathname file)
                  line
                  column
                  (condition-name-to-print condition)
                  condition)
        (sb-int:simple-stream-error () (continue))))))

(defun call-with-handle-condition (handle-condition fn)
  (handler-bind ((sb-c:fatal-compiler-error handle-condition)
                 (sb-c:compiler-error handle-condition)
                 ;; Ignore compiler-note for now.
                 ;; Perhaps those notes could be shown by some command-line option.
                 ;; (sb-ext:compiler-note handle-condition)
                 (error handle-condition)
                 (warning handle-condition))
    (funcall fn)))

(defun file-in-directory-without-quicklisp-directory-p (file directory)
  (and (file-in-directory-p file directory)
       (not (in-quicklisp-directory-p file))))

(defun printable-note-p (position file directory)
  (and position
       (or (not directory)
           (and file
                (file-in-directory-without-quicklisp-directory-p file directory)))))


(defun make-error-map ()
  (make-hash-table :test 'equalp))


(defun num-errors (error-map)
  (hash-table-count error-map))


(defun run-lint-fn (fn &optional (stream *standard-output*)
                                 (error *error-output*)
                                 directory
                                 (error-map (make-error-map)))
  "Returns a number of errors collected during runing the function."
  (let* ((errout *error-output*)
         (*error-output* error))
    (labels ((handle-condition (condition)
               (let* ((*error-output* errout)
                      (sb-int:*print-condition-references* nil))
                 (multiple-value-bind (file position)
                     (get-location condition)
                   (cond
                     ((printable-note-p position file directory)
                      (let ((key (list file position (princ-to-string condition))))
                        (unless (gethash key error-map)
                          (setf (gethash key error-map) t)
                          (print-note file position condition stream))))
                     ((and (not (ignorable-compiler-warning-p condition))
                           (or (null file)
                               (null directory)
                               (file-in-directory-without-quicklisp-directory-p (ensure-uncached-file file) directory)))
                      (format *error-output*
                              "~&WARNING~@[ while loading '~A'~]:~% ~A~%"
                              file
                              condition)))))))
      (call-with-handle-condition #'handle-condition fn)
      (num-errors error-map))))

(defun ensure-dependencies-are-loaded (system)
  (let ((dependencies (all-required-systems (asdf:component-name system))))
    (when dependencies
      (do-log :info "Loading ~D ~:*system~[s~;~:;s~]:~%  ~{~A~^ ~}"
        (length dependencies)
        dependencies)
      (handler-bind ((warning #'muffle-warning))
        #+quicklisp
        (ql:quickload dependencies :silent t)
        #-quicklisp
        (mapc (lambda (name)
                (with-muffled-streams
                  (asdf:load-system name :verbose nil)))
              dependencies)))))

(defun run-lint-asd (asd-file &optional (stream *standard-output*))
  (do-log :info "Lint system ~A" (make-relative-pathname asd-file))

  (let ((file (probe-file asd-file)))
    (unless file
      (error "ASD file does not exist: '~A'" asd-file))

    (unless (equal (pathname-type file) "asd")
      (error "Not ASD file: '~A'" file))

    (asdf:load-asd file)
    (let* ((asdf:*system-definition-search-functions*
             (cons (asdf-target-system-locator (pathname-name file))
                   asdf:*system-definition-search-functions*))
           (system (find-system-from-pathname-name file))
           ;; Here we'll count errors.
           ;; We need it because run-lit-fn may signal error
           ;; and without error-map we'll never know how many
           ;; errors were there.
           (error-map (make-error-map)))

      #+quicklisp (install-required-systems (pathname-name file))
      (ensure-dependencies-are-loaded system)

      (let ((directory (make-pathname :defaults file
                                      :name nil
                                      :type nil))
            (errout *error-output*))
        (labels ((ignore-and-continue (e)
                   (let ((accept (find-restart 'asdf/action:accept e)))
                     (when accept
                       (invoke-restart accept)))
                   (let ((continue (find-restart 'continue e)))
                     (when continue
                       (invoke-restart continue))))
                 (handle-compile-error (e)
                   (let ((*error-output* errout))
                     (if (remove-if-not (lambda (comp)
                                          (file-in-directory-p
                                           (asdf:component-pathname comp)
                                           directory))
                                        (mapcar #'cdr
                                                (uiop/lisp-build::compile-condition-context-arguments e)))
                         (warn "Compilation failed in a system ~S."
                               (asdf:component-name system))
                         (ignore-and-continue e))))
                 (handle-compile-file-error (e)
                   (handle-compile-error e)
                   (return-from run-lint-asd
                     (num-errors error-map))))
          (run-lint-fn (lambda ()
                         (do-log :info "Loading a system: ~A" (asdf:component-name system))
                         (handler-bind ((asdf:compile-error #'handle-compile-error)
                                        #+asdf3
                                        (uiop:compile-file-error #'handle-compile-file-error)
                                        #+sbcl
                                        (sb-int:package-at-variance #'ignore-and-continue))
                           (with-muffled-streams
                             (asdf:load-system system :force t)))
                         (do-log :info "Done"))
                       stream
                       *error-output*
                       directory
                       error-map))))))

(defun run-lint-file (file &optional (stream *standard-output*))
  "Returns a number of errors in the given file or ASDF system"
  (do-log :info "Lint file ~A" (make-relative-pathname file))

  (unless (uiop:file-exists-p file)
    (error "File does not exist: '~A'" file))

  (when (equal (pathname-type file) "asd")
    (return-from run-lint-file
      (run-lint-asd file stream)))

  (handler-case
      (let* ((err (make-broadcast-stream))
             (num-errors (run-lint-fn
                          (lambda ()
                            (load file :verbose nil :print nil))
                          stream
                          err)))
        (unless (zerop num-errors)
          (error 'sblint-compilation-error
                 :file file
                 :message (get-output-stream-string err)))
        (values num-errors))
    (sb-c::input-error-in-load ()
      (return-from run-lint-file
        1))))

(defvar *global-enable-logger*)
(defun run-lint-directory (directory &optional (stream *standard-output*))
  "Returns a number of errors in the given directory."
  (let ((*global-enable-logger*
          (if (boundp '*global-enable-logger*)
              *global-enable-logger*
              *enable-logger*))
        (num-errors 0))
    (do-log :info "Lint directory '~A'" (make-relative-pathname directory))

    (unless (uiop:directory-exists-p directory)
      (error "Directory does not exist: '~A'" directory))

    (dolist (dir (uiop:subdirectories directory))
      (unless (in-quicklisp-directory-p dir)
        (let ((*enable-logger* nil))
          (incf num-errors
                (run-lint-directory dir stream)))))
    
    (let ((asdf:*central-registry* (cons directory asdf:*central-registry*)))
      (let ((*enable-logger* *global-enable-logger*))
        (dolist (file (directory-asd-files directory))
          (incf num-errors
                (run-lint-file file stream)))))

    (values num-errors)))
