(defpackage #:sblint/utilities/quicklisp
  (:use #:cl)
  (:import-from #:sblint/error
                #:sblint-system-installation-error)
  (:import-from #:sblint/utilities/logger
                #:do-log
                #:*logger-stream*)
  (:import-from #:sblint/utilities/streams
                #:with-muffled-streams)
  (:import-from #:sblint/utilities/pathname
                #:in-directories-p)
  (:import-from #:sblint/utilities/asdf
                #:direct-dependencies
                #:all-required-systems
                #:directory-asd-files
                #:asdf-target-system-locator)
  (:export #:in-quicklisp-directory-p
           #:install-required-systems))
(in-package #:sblint/utilities/quicklisp)

(defparameter *quicklisp-directory-names*
  '(".qlot" "quicklisp"))

(defun in-quicklisp-directory-p (pathname)
  (in-directories-p pathname *quicklisp-directory-names*))

(defun install-required-systems (system-name)
  #-quicklisp (declare (ignore system-name))
  #+quicklisp
  (let ((required-system-names (direct-dependencies system-name)))
    (when required-system-names
      (dolist (name required-system-names)
        (let ((required (ql-dist:find-system (parse-dependency-form name))))
          (when required
            (do-log :info "Installing '~A'" required)
            (with-muffled-streams
              (handler-case
                  (ql-dist:ensure-installed required)
                (error (e)
                  (error 'sblint-system-installation-error :name name :real-error e))))))))))
