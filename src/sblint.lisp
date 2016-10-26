(in-package #:cl-user)
(defpackage #:sblint
  (:use #:cl)
  (:import-from #:sblint/file-location
                #:compiler-source-path
                #:compiler-note-position
                #:file-position-to-line-and-column)
  (:import-from #:sblint/logger
                #:*enable-logger*
                #:do-log)
  (:import-from #:sblint/error
                #:sblint-error
                #:sblint-system-load-error
                #:sblint-compilation-error)
  (:import-from #:sblint/util
                #:make-relative-pathname
                #:condition-name-to-print
                #:install-required-systems
                #:all-required-systems
                #:directory-asd-files)
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
(in-package #:sblint)

(defun run-lint-asd (asd-file &optional (stream *standard-output*))
  (do-log :info "Lint system ~A" (make-relative-pathname asd-file))

  (let ((file (probe-file asd-file)))
    (unless file
      (error "ASD file does not exist: '~A'" asd-file))

    (unless (equal (pathname-type file) "asd")
      (error "Not ASD file: '~A'" file))

    (let ((*standard-output* (make-broadcast-stream))
          (*error-output* (make-broadcast-stream)))
      (load file :verbose nil :print nil))
    (let ((system (let ((*standard-output* (make-broadcast-stream))
                        (*error-output* (make-broadcast-stream)))
                    (asdf:find-system (pathname-name file) nil))))
      (unless system
        (error "System '~A' does not exist in '~A'."
               (pathname-name file)
               file))

      ;; Ensure dependencies are installed
      (install-required-systems (asdf:component-name system))
      ;; Ensure dependencies are loaded
      (let ((*standard-output* (make-broadcast-stream))
            (*error-output* (make-broadcast-stream))
            (*terminal-io* (make-broadcast-stream)))
        (mapc (lambda (name)
                (asdf:load-system name :verbose nil)) (all-required-systems (asdf:component-name system))))

      (handler-bind ((error
                       (lambda (e)
                         (error 'sblint-system-load-error
                                :system system
                                :real-error e))))
        (run-lint-fn (lambda ()
                       (let ((*standard-output* (make-broadcast-stream))
                             (*error-output* (make-broadcast-stream))
                             (*terminal-io* (make-broadcast-stream)))
                         (asdf:oos 'asdf:load-op system :force t :verbose nil)))
                     stream)))

    (values)))

(defun run-lint-file (file &optional (stream *standard-output*))
  (do-log :info "Lint file ~A" (make-relative-pathname file))

  (unless (uiop:file-exists-p file)
    (error "File does not exist: '~A'" file))

  (when (equal (pathname-type file) "asd")
    (return-from run-lint-file
      (run-lint-asd file stream)))

  (let ((err (make-broadcast-stream)))
    (unless (run-lint-fn (lambda ()
                           (load file :verbose nil :print nil))
                         stream
                         err)
      (error 'sblint-compilation-error
             :file file
             :message (get-output-stream-string err)))))

(defun run-lint-fn (fn &optional (stream *standard-output*) (error *error-output*))
  (let* ((errout *error-output*)
         (*error-output* error)
         (error-map (make-hash-table :test 'equalp)))
    (handler-bind ((warning
                     (lambda (condition)
                       (let* ((*error-output* errout)
                              (sb-int:*print-condition-references* nil)
                              (context (sb-c::find-error-context nil))
                              (file (and context
                                         (sb-c::compiler-error-context-file-name context)))
                              (position (cond
                                          (context (compiler-note-position
                                                    file
                                                    (compiler-source-path context)))
                                          ((typep condition 'reader-error)
                                           (let ((stream (stream-error-stream condition)))
                                             (file-position stream)))
                                          (t nil))))
                         (when (and position
                                    (not (gethash (list file position (princ-to-string condition)) error-map)))
                           (setf (gethash (list file position (princ-to-string condition)) error-map) t)
                           (multiple-value-bind (line column)
                               (file-position-to-line-and-column file position)
                             (let ((*print-pretty* nil))
                               (format stream "~&~A:~A:~A: ~A: ~A~%"
                                       (make-relative-pathname file)
                                       line
                                       column
                                       (condition-name-to-print condition)
                                       condition))))))))
      (funcall fn))))

(defun run-lint-directory (directory &optional (stream *standard-output*))
  (do-log :info "Lint directory '~A'" (make-relative-pathname directory))

  (unless (uiop:directory-exists-p directory)
    (error "Directory does not exist: '~A'" directory))

  (dolist (dir (uiop:subdirectories directory))
    (run-lint-directory dir stream))
  (dolist (file (directory-asd-files directory))
    (run-lint-file file stream))

  (values))
