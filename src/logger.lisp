(in-package #:cl-user)
(defpackage #:sblint/logger
  (:use #:cl)
  (:export #:*enable-logger*
           #:do-log))
(in-package #:sblint/logger)

(defvar *enable-logger* nil)
(defvar *logger-stream* (make-synonym-stream '*error-output*))

(defun do-log (level message &rest args)
  (when *enable-logger*
    (format *logger-stream* "~&[~A] ~A~%"
            level
            (apply #'format nil message args))))
