#+sbcl
(defsystem "sblint"
  :version "0.1.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("swank"
               "uiop")
  :components ((:module "src"
                :components
                ((:file "sblint" :depends-on ("file-location" "error" "logger" "util"))
                 (:file "file-location")
                 (:file "logger")
                 (:file "error")
                 (:file "util"))))
  :description "A linter for Common Lisp source code using SBCL")
#-sbcl
(error "SBLint requires SBCL")
