(defpackage #:sblint/utilities/file-position
  (:use #:cl)
  (:export #:file-position-to-line-and-column))
(in-package #:sblint/utilities/file-position)

(defun file-position-to-line-and-column (file position)
  (let ((line 1)
        (column 0))
    (with-open-file (in file :direction :input :element-type 'character)
      (dotimes (i (1- position))
        (let ((char (read-char in)))
          (cond
            ((char= char #\Newline)
             (incf line)
             (setf column 0))
            ((char= char #\Return))
            (t (incf column))))))
    (values line column)))
