(in-package #:cl-user)
(defpackage #:sblint/util
  (:use #:cl)
  (:export #:make-relative-pathname
           #:condition-name-to-print))
(in-package #:sblint/util)

(defun make-relative-pathname (path &optional (base *default-pathname-defaults*))
  (when (uiop:relative-pathname-p path)
    (return-from make-relative-pathname path))

  (let ((path-dir (pathname-directory path))
        (base-dir (pathname-directory base)))
    (loop for path-part = (pop path-dir)
          for base-part = (pop base-dir)
          do (flet ((make-rel-path ()
                      (make-pathname :name (pathname-name path)
                                     :type (pathname-type path)
                                     :directory
                                     (append (list :relative)
                                             (make-list (1+ (length base-dir)) :initial-element :up)
                                             (if (eq path-part :home)
                                                 (cdr (pathname-directory (user-homedir-pathname)))
                                                 (list path-part))
                                             path-dir))))
               (if (equal path-part base-part)
                   nil ;; ignore
                   (return (make-rel-path)))
               (cond
                 ((and (null base-dir)
                       (null path-dir))
                  (return (file-namestring path)))
                 ((null base-dir)
                  (return (make-pathname
                           :name (pathname-name path)
                           :type (pathname-type path)
                           :directory (cons (if (eq path-part :absolute)
                                                :absolute
                                                :relative)
                                            path-dir))))
                 ((null path-dir)
                  (return
                    (make-pathname :name (pathname-name path)
                                   :type (pathname-type path)
                                   :directory
                                   (cons :relative
                                         (make-list (length base-dir) :initial-element :up))))))))))

(defun condition-name-to-print (condition)
  (typecase condition
    (style-warning
     "style-warning")
    (otherwise
     (string-downcase condition))))
