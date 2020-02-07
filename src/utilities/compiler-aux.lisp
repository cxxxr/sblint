(defpackage #:sblint/utilities/compiler-aux
  (:use #:cl)
  (:import-from #:swank)
  (:export #:compiler-note-position))
(in-package #:sblint/utilities/compiler-aux)

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
