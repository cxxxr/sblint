(defpackage #:sblint/utilities/pathname
  (:use #:cl)
  (:export #:file-in-directory-p))
(in-package #:sblint/utilities/pathname)

(defun file-in-directory-p (file directory)
  (eql 0 (search (pathname-directory directory)
                 (pathname-directory file)
                 :test #'equal)))
