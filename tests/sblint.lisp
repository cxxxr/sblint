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

(defun run-lint-file-test (base-filename expected-list)
  (let* ((text
           (with-output-to-string (*standard-output*)
             (sblint:run-lint-file
              (test-pathname base-filename))))
         (actual-list (parse-text text)))
    (loop :for actual :in actual-list
          :for expected :in expected-list
          :do (ok (match actual (apply #'make-result
                                       (append (list base-filename)
                                               expected
                                               (list ""))))))))

(deftest simple-test
  (run-lint-file-test "tests/example/simple.lisp"
                      '(("1" "0")
                        ("4" "0"))))

(deftest reader-error-in-compile-file-test
  (run-lint-file-test "tests/example/reader-error-case.lisp"
                      '(("2" "9"))))
