(defpackage #:sblint/utilities/pathname
  (:use #:cl)
  (:export #:file-in-directory-p
           #:in-directories-p
           #:make-relative-pathname))
(in-package #:sblint/utilities/pathname)

(defun file-in-directory-p (file directory)
  (eql 0 (search (pathname-directory directory)
                 (pathname-directory file)
                 :test #'equal)))

(defun in-directories-p (pathname directory-names)
  (some (lambda (name)
          (equal (car (last (pathname-directory pathname))) name))
        directory-names))

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
                  (return (if (uiop:file-pathname-p path)
                              (pathname (file-namestring path))
                              #P"./")))
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
