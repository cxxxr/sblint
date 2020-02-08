(defpackage #:sblint/utilities/compiler-aux
  (:use #:cl)
  (:import-from #:swank)
  (:export #:ignorable-compiler-warning-p
           #:compiler-note-position))
(in-package #:sblint/utilities/compiler-aux)

(deftype ignorable-compiler-warning ()
  '(or #+asdf3.3 asdf/operate:recursive-operate
    ;; XXX: Actual redefinition should be warned, however it loads the same file twice when compile-time & load-time and it shows many redefinition warnings.
    sb-kernel:redefinition-warning
    uiop:compile-warned-warning))

(defun ignorable-compiler-warning-p (condition)
  (typep condition 'ignorable-compiler-warning))

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
