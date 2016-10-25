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
                #:sblint-system-load-error
                #:sblint-compilation-error)
  (:import-from #:sblint/util
                #:make-relative-pathname)
  (:import-from #:uiop
                #:file-exists-p
                #:directory-exists-p
                #:subdirectories
                #:directory-files
                #:with-temporary-file)
  (:export #:run-lint-file
           #:run-lint-asd
           #:run-lint-directory
           #:*enable-logger*))
(in-package #:sblint)

(defun run-lint-asd (asd-file &optional (stream *standard-output*))
  (do-log :info "Lint system ~A" (make-relative-pathname asd-file))

  (let ((file (probe-file asd-file)))
    (unless file
      (error "ASD file does not exist: '~A'" asd-file))

    (unless (equal (pathname-type file) "asd")
      (error "Not ASD file: '~A'" file))

    (load file :verbose nil :print nil)
    (let ((system (asdf:find-system (pathname-name file) nil)))
      (unless system
        (error "System '~A' does not exist in '~A'."
               (pathname-name file)
               file))

      (handler-bind ((error
                       (lambda (e)
                         (error 'sblint-system-load-error
                                :system system
                                :real-error e))))
        #+quicklisp
        (ql:quickload (pathname-name file) :silent t)
        #-quicklisp
        (asdf:load-system (pathname-name file) :verbose nil))

      (labels ((component-cl-files (component)
                 (mapcan (lambda (child)
                           (typecase child
                             (asdf:parent-component
                              (component-cl-files child))
                             (asdf:cl-source-file
                              (list child))
                             (otherwise '())))
                         (asdf:component-children component))))
        (dolist (file-component (component-cl-files system))
          (run-lint-file (asdf:component-pathname file-component) stream))))

    (values)))

(defun run-lint-file (file &optional (stream *standard-output*))
  (do-log :info "Lint file ~A" (make-relative-pathname file))

  (unless (uiop:file-exists-p file)
    (error "File does not exist: '~A'" file))

  (when (equal (pathname-type file) "asd")
    (return-from run-lint-file
      (run-lint-asd file stream)))

  (let* ((errout *error-output*)
         (*error-output* (make-string-output-stream)))
    (unless
        (handler-bind ((warning
                         (lambda (condition)
                           (let* ((*error-output* errout)
                                  (sb-int:*print-condition-references* nil)
                                  (context (sb-c::find-error-context nil))
                                  (file (sb-c::compiler-error-context-file-name context))
                                  (position (compiler-note-position
                                             file
                                             (compiler-source-path context))))
                             (multiple-value-bind (line column)
                                 (file-position-to-line-and-column file position)
                               (format stream "~&~A:~A:~A: ~A~%"
                                       (make-relative-pathname file)
                                       line
                                       column
                                       condition))))))
          (uiop:with-temporary-file (:pathname fasl :element-type '(unsigned-byte 8) :direction :output)
            (compile-file file :output-file fasl :verbose nil :print nil)))
      (error 'sblint-compilation-error
             :file file
             :message (get-output-stream-string *error-output*)))))

(defun run-lint-directory (directory &optional (stream *standard-output*))
  (do-log :info "Lint directory '~A'" (make-relative-pathname directory))

  (unless (uiop:directory-exists-p directory)
    (error "Directory does not exist: '~A'" directory))

  (dolist (dir (uiop:subdirectories directory))
    (run-lint-directory dir stream))
  (dolist (file (uiop:directory-files directory))
    (run-lint-file file stream))

  (values))
