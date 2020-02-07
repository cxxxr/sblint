(defpackage #:sblint/file-location
  (:use #:cl)
  (:import-from #:swank)
  (:export #:compiler-note-position
           #:file-position-to-line-and-column))
(in-package #:sblint/file-location)

(defun compiler-source-path (context)
  "Return the source-path for the current compiler error.
Returns NIL if this cannot be determined by examining internal
compiler state."
  (cond ((sb-c::node-p context)
         (reverse
          (sb-c::source-path-original-source
           (sb-c::node-source-path context))))
        ((sb-c::compiler-error-context-p context)
         (reverse
          (sb-c::compiler-error-context-original-source-path context)))))

(defun compiler-note-position (file context)
  (let* ((source-path (compiler-source-path context))
         (position (swank/source-path-parser:source-path-file-position source-path file)))
    (when position
      (1+ position))))

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
