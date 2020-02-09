(defsystem "sblint-tests"
  :depends-on ("sblint" "rove" "split-sequence")
  :serial t
  :pathname "tests"
  :components ((:file "main"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
