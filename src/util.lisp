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
             (declare (type simple-string name))
             (and (<= 3 (length name))
                  (string-equal name "sb-" :end1 3)))
           (system-dependencies (system-name)
             (unless (or (string-equal system-name "asdf")
                         (sbcl-contrib-p system-name))
               (let ((system (let ((*standard-output* (make-broadcast-stream))
                                   (*error-output* (make-broadcast-stream)))
                               (asdf:find-system system-name nil))))
                 (when system
                   (cons system-name
                         (loop for dep in (append (asdf:system-depends-on system)
                                                  (asdf:system-defsystem-depends-on system))
                               append (system-dependencies (string-downcase dep)))))))))
    (delete-if (lambda (dep)
                 (find dep systems :test #'string=))
               (delete-duplicates (mapcan #'system-dependencies systems)
                                  :test #'string=
                                  :from-end t))))

(defun install-required-systems (&rest systems)
  (let ((required (apply #'all-required-systems systems)))
    (do-log :info "Installing ~D ~:*system~[s~;~:;s~]:~%  ~{~A~^ ~}" (length required) required)
    (dolist (name required)
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
         (deps-map (make-hash-table :test 'equal)))
    (dolist (file asd-files)
      (let ((*standard-output* (make-broadcast-stream))
            (*error-output* (make-broadcast-stream)))
        (load file :verbose nil :print nil))
      (let* ((deps (all-required-systems (pathname-name file)))
             (deps (delete-if-not (lambda (name)
                                    (find name system-names :test #'string=))
                                  deps)))
        (setf (gethash (pathname-name file) deps-map) deps)))
    (labels ((to-load-systems (name)
               (cons name
                     (mapcan #'to-load-systems (gethash name deps-map)))))
      (let ((load-system-names
              (nreverse
               (delete-duplicates
                (mapcan (lambda (file)
                          (let ((*standard-output* (make-broadcast-stream))
                                (*error-output* (make-broadcast-stream)))
                            (load file :verbose nil :print nil))
                          (to-load-systems (pathname-name file)))
                        asd-files)
                :test #'string=))))
        (mapcar (lambda (name)
                  (make-pathname :name name
                                 :type "asd"
                                 :directory (pathname-directory directory)))
                load-system-names)))))
