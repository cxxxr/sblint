(defsystem "sblint-test"
  :depends-on ("sblint" "rove" "split-sequence")
  :serial t
  :pathname "tests"
  :components ((:file "sblint"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
