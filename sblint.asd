#|
  This file is a part of sblint project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  A linter for Common Lisp source code using SBCL

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sblint-asd
  (:use :cl :asdf))
(in-package :sblint-asd)

#+sbcl
(defsystem sblint
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:swank
               :uiop)
  :components ((:module "src"
                :components
                ((:file "sblint" :depends-on ("file-location" "error" "logger" "util"))
                 (:file "file-location")
                 (:file "logger")
                 (:file "error")
                 (:file "util"))))
  :description "A linter for Common Lisp source code using SBCL"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
#-sbcl
(error "SBLint requires SBCL")
