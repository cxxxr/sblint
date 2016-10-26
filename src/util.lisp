(in-package #:cl-user)
(defpackage #:sblint/util
  (:use #:cl)
  (:import-from #:sblint/error
                #:sblint-system-installation-error)
  (:import-from #:sblint/logger
                #:do-log)
  (:export #:make-relative-pathname
           #:condition-name-to-print
           #:all-required-systems
           #:install-required-systems))
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

(defun condition-name-to-print (condition)
  (typecase condition
    (style-warning
     "style-warning")
    (otherwise
     (string-downcase (type-of condition)))))

(defun all-required-systems (&rest systems)
  (labels ((sbcl-contrib-p (name)
             (let ((name (princ-to-string name)))
               (and (<= 3 (length name))
                    (string-equal name "sb-" :end1 3))))
           (system-dependencies (system-name)
             (unless (or (string-equal system-name "asdf")
                         (sbcl-contrib-p system-name))
               (let ((system (let ((*standard-output* (make-broadcast-stream))
                                   (*error-output* (make-broadcast-stream)))
                               (asdf:find-system system-name nil))))
                 (when system
                   (cons system-name
                         (mapcan #'system-dependencies (copy-list (asdf::component-sideway-dependencies system)))))))))
    (remove-if (lambda (name)
                 (member name systems :test #'string=))
               (mapcar #'string-downcase
                       (delete-duplicates (mapcan #'system-dependencies systems)
                                          :from-end t)))))

(defun install-required-systems (&rest systems)
  (let ((required (apply #'all-required-systems systems)))
    (dolist (name required)
      (do-log :info "Installing ~S" name)
      (let ((*standard-output* (make-broadcast-stream))
            (*error-output* (make-broadcast-stream)))
        #+quicklisp
        (handler-case
            (let ((system (ql::find-system name)))
              (if system
                  (ql-dist:ensure-installed system)
                  (or (asdf:find-system name nil)
                      (warn "Dependency ~S not found. Ignored." name))))
          (error (e)
            (error 'sblint-system-installation-error :name name :real-error e)))
        #-quicklisp
        (let ((system (asdf:find-system name nil)))
          (unless system
            (warn "Dependency ~S not found. Ignored." name)))))))

(defun directory-asd-files (&optional (directory *default-pathname-defaults*))
  "List ASD files in the DIRECTORY and sort them to load."
  (let* ((asd-files (uiop:directory-files directory "*.asd"))
         (system-names (mapcar #'pathname-name asd-files))
         (points (make-hash-table :test 'equal)))
    (dolist (file asd-files)
      (let ((*standard-output* (make-broadcast-stream))
            (*error-output* (make-broadcast-stream)))
        (load file :verbose nil :print nil))
      (let ((deps (all-required-systems (pathname-name file))))
        (setf (gethash (pathname-name file) points)
              (count-if (lambda (name)
                          (member name system-names :test #'string=))
                        deps))))
    (sort asd-files
          (lambda (a b)
            (< (gethash (pathname-name a) points)
               (gethash (pathname-name b) points))))))
