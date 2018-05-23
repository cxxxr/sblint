(in-package #:cl-user)
(defpackage #:sblint/util
  (:use #:cl)
  (:import-from #:sblint/error
                #:sblint-system-installation-error)
  (:import-from #:sblint/logger
                #:do-log
                #:*logger-stream*)
  (:export #:with-muffled-streams
           #:make-relative-pathname
           #:condition-name-to-print
           #:all-required-systems
           #:install-required-systems
           #:directory-asd-files
           #:asdf-target-system-locator
           #:load-asd
           #:file-in-directory-p))
(in-package #:sblint/util)

(defmacro with-muffled-streams (&body body)
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream))
         (*terminal-io* (make-two-way-stream *standard-input* (make-broadcast-stream)))
         (*logger-stream* *error-output*))
     ,@body))

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

(defun direct-dependencies (system-name)
  (let ((system (with-muffled-streams
                  (asdf:find-system system-name))))
    (append (asdf:system-depends-on system)
            (asdf:system-defsystem-depends-on system))))

(defun all-required-systems (system-name)
  (labels ((sbcl-contrib-p (name)
             (declare (type simple-string name))
             (and (<= 3 (length name))
                  (string-equal name "sb-" :end1 3)))
           (system-dependencies (system-name)
             (unless (or (string-equal system-name "asdf")
                         (sbcl-contrib-p system-name))
               (cons system-name
                     (loop for dep in (direct-dependencies system-name)
                           append (system-dependencies
                                   (if (consp dep)
                                       (second dep)
                                       (string-downcase dep))))))))
    (delete system-name
            (delete-duplicates (system-dependencies system-name)
                               :test #'string=
                               :from-end t)
            :test #'string=)))

(defun install-required-systems (system-name)
  (declare (ignorable system-name))
  #+quicklisp
  (let ((required-system-names (direct-dependencies system-name)))
    (when required-system-names
      (do-log :info "Installing ~D ~:*system~[s~;~:;s~]:~%  ~{~A~^ ~}"
        (length required-system-names)
        required-system-names)
      (dolist (name required-system-names)
        (let ((required (ql-dist:find-system name)))
          (when required
            (with-muffled-streams
              (handler-case
                  (ql-dist:ensure-installed required)
                (error (e)
                  (error 'sblint-system-installation-error :name name :real-error e))))))))))

(defun directory-asd-files (&optional (directory *default-pathname-defaults*))
  "List ASD files in the DIRECTORY and sort them to load."
  (let* ((asd-files (uiop:directory-files directory "*.asd"))
         (system-names (mapcar #'pathname-name asd-files))
         (deps-map (make-hash-table :test 'equal)))
    (dolist (file asd-files)
      (load-asd file)
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
                          (load-asd file)
                          (to-load-systems (pathname-name file)))
                        asd-files)
                :test #'string=))))
        (mapcar (lambda (name)
                  (make-pathname :name name
                                 :type "asd"
                                 :directory (pathname-directory directory)))
                load-system-names)))))

(defun asdf-target-system-locator (system-name)
  (lambda (name)
    (when (and (string= name system-name)
               (asdf:system-registered-p system-name))
      (cdr (asdf:system-registered-p system-name)))))

(defun load-asd (file)
  (assert (string= (pathname-type file) "asd"))

  (with-muffled-streams
    #+quicklisp
    (handler-case
        (let ((*package* (find-package :asdf-user)))
          (handler-bind ((style-warning #'muffle-warning))
            (load file :verbose nil :print nil)))
      (asdf:missing-component (e)
        (ql:quickload (asdf/find-component:missing-requires e) :silent t)
        (load file :verbose nil :print nil)))
    #-quicklisp
    (load file :verbose nil :print nil)))

(defun file-in-directory-p (file directory)
  (eql 0 (search (pathname-directory directory)
                 (pathname-directory file)
                 :test #'equal)))
