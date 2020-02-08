(defpackage #:sblint/utilities/streams
  (:use #:cl)
  (:import-from #:sblint/utilities/logger
                #:*logger-stream*)
  (:export #:with-muffled-streams))
(in-package #:sblint/utilities/streams)

(defmacro with-muffled-streams (&body body)
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream))
         (*terminal-io* (make-two-way-stream *standard-input* (make-broadcast-stream)))
         (*logger-stream* *error-output*))
     ,@body))
