(defpackage #:sblint-test/sblint
  (:use #:cl #:rove))
(in-package #:sblint-test/sblint)

(defstruct (result (:constructor make-result (file line-number column message)))
  file
  line-number
  column
  message)

(defun lines (&rest strings)
  (with-output-to-string (out)
    (dolist (string strings)
      (write-line string out))))

(defun split-lines (text)
  (remove 0 (split-sequence:split-sequence #\newline text)
          :key #'length))

(defun parse-line (line)
  (multiple-value-bind (parts pos)
      (split-sequence:split-sequence #\: line :count 3)
    (destructuring-bind (file line-number column) parts
      (make-result file line-number column (string-left-trim " " (subseq line pos))))))

(defun parse-text (text)
  (mapcar #'parse-line (split-lines text)))

(defun match (actual expected)
  (every (lambda (fn)
           (equal (funcall fn actual)
                  (funcall fn expected)))
         (list #'result-file
               #'result-line-number
               #'result-column)))

(defun test-pathname (base-filename)
  (asdf:system-relative-pathname :sblint-test base-filename))

(defun preprocessing-expected-list (expected-list)
  (mapcar (lambda (expected)
            (apply #'make-result (append expected (list ""))))
          expected-list))

(defun run-lint-test (run-lint-fn base-filename expected-list)
  (let* ((text
           (with-output-to-string (*standard-output*)
             (funcall run-lint-fn
                      (test-pathname base-filename))))
         (actual-list (parse-text text))
         (expected-list (preprocessing-expected-list expected-list)))
    (ok (= (length actual-list) (length expected-list)))
    (dolist (actual actual-list)
      (if (some (lambda (expected)
                  (match actual expected))
                expected-list)
          (pass (format nil "Expect ~S" actual))
          (fail (format nil "actual: ~S~%expected-list: ~S" actual expected-list))))))

(deftest simple-test
  (let ((file "tests/example/simple.lisp"))
    (run-lint-test #'sblint:run-lint-file
                   file
                   `((,file "1" "0")
                     (,file "4" "0")))))

(deftest reader-error-in-compile-file-test
  (let ((file "tests/example/reader-error-case.lisp"))
    (run-lint-test #'sblint:run-lint-file
                   file
                   `((,file "2" "9"))))
  (let ((lisp-file "tests/example/foo/foo.lisp")
        (asd-file "tests/example/foo/foo.asd"))
    (run-lint-test #'sblint:run-lint-asd
                   asd-file
                   `((,lisp-file "2" "4")))))
