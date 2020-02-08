(defpackage #:sblint/utilities/asdf
  (:use #:cl)
  (:import-from #:sblint/utilities/streams
                #:with-muffled-streams)
  (:import-from #:sblint/utilities/pathname
                #:file-in-directory-p
                #:make-relative-pathname)
  (:export #:direct-dependencies
           #:parse-dependency-form
           #:all-required-systems
           #:directory-asd-files
           #:asdf-target-system-locator
           #:ensure-uncached-file
           #:find-system-from-pathname-name))
(in-package #:sblint/utilities/asdf)

(defun direct-dependencies (system-name)
  (let ((system (handler-bind ((asdf:bad-system-name #'muffle-warning))
                  (with-muffled-streams
                    (asdf:find-system system-name)))))
    (append (asdf:system-depends-on system)
            (asdf:system-defsystem-depends-on system))))

(defun system-root-name (system-name)
  (subseq system-name 0 (position #\/ system-name)))

(defun parse-feature-expression (expr)
  (etypecase expr
    (cons (ecase (first expr)
            (:and (notany #'null
                          (mapcar #'parse-feature-expression
                                  (cdr expr))))
            (:or (notevery #'null
                           (mapcar #'parse-feature-expression
                                   (cdr expr))))
            (:not (not (parse-feature-expression (second expr))))))
    (symbol (find expr *features*))))

(defun parse-dependency-form (dep)
  (etypecase dep
    (cons
     (string-downcase
      (ecase (first dep)
        (:feature (if (parse-feature-expression (second dep))
                      (parse-dependency-form (third dep))
                      (return-from parse-dependency-form)))
        (:version (second dep))
        (:require (second dep)))))
    ((or string
         symbol)
     (string-downcase dep))))

(defun all-required-systems (system-name)
  (let ((appeared (make-hash-table :test 'equal)))
    (labels ((sbcl-contrib-p (name)
               (declare (type simple-string name))
               (and (<= 3 (length name))
                    (string-equal name "sb-" :end1 3)))
             (system-dependencies (system-name)
               (unless (or (string-equal system-name "asdf")
                           (sbcl-contrib-p system-name)
                           (gethash system-name appeared))
                 (setf (gethash system-name appeared) t)
                 (cons system-name
                       (loop for dep in (direct-dependencies system-name)
                             append (let ((depend-system (parse-dependency-form dep)))
                                      (when depend-system
                                        (system-dependencies depend-system))))))))
      (delete system-name
              (delete-duplicates (mapcar #'system-root-name (system-dependencies system-name))
                                 :test #'string=
                                 :from-end t)
              :test #'string=))))

(defun directory-asd-files (&optional (directory *default-pathname-defaults*))
  "List ASD files in the DIRECTORY and sort them to load."
  (let* ((asd-files (uiop:directory-files directory "*.asd"))
         (system-names (mapcar #'pathname-name asd-files))
         (deps-map (make-hash-table :test 'equal)))
    (dolist (file asd-files)
      (with-muffled-streams
        (asdf:load-asd file))
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
                          (asdf:load-asd file)
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
    (and (string= name system-name)
         #+asdf3.3
         (asdf:registered-system system-name)
         #-asdf3.3
         (cdr (asdf:system-registered-p system-name)))))

(defun ensure-uncached-file (file)
  (if (file-in-directory-p file asdf:*user-cache*)
      (let ((tmp (make-relative-pathname file asdf:*user-cache*)))
        (make-pathname :defaults file :directory (cons :absolute (cdr (pathname-directory tmp)))))
      file))

(defun find-system-from-pathname-name (file)
  (let ((system (with-muffled-streams
                  (asdf:find-system (pathname-name file) nil))))
    (unless system
      (error "System '~A' does not exist in '~A'."
             (pathname-name file)
             file))
    system))
