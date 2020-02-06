(defpackage sblint/error
  (:use #:cl)
  (:export #:sblint-error
           #:sblint-system-load-error
           #:sblint-compilation-error
           #:sblint-system-installation-error))
(in-package #:sblint/error)

(define-condition sblint-error (error) ())

(define-condition sblint-system-load-error (sblint-error)
  ((real-error :initarg :real-error)
   (system :initarg :system))
  (:report (lambda (condition stream)
             (with-slots (real-error system) condition
               (format stream "[ERROR] '~A' occurred while loading a system '~A':~%    ~A"
                       (type-of real-error)
                       (asdf:component-name system)
                       real-error)))))

(define-condition sblint-compilation-error (sblint-error)
  ((message :initarg :message)
   (file :initarg :file))
  (:report (lambda (condition stream)
             (with-slots (message file) condition
               (format stream "[ERROR] Compilation of '~A' has failed:~%~A"
                       file
                       message)))))

(define-condition sblint-system-installation-error (sblint-error)
  ((real-error :initarg :real-error)
   (name :initarg :name))
  (:report (lambda (condition stream)
             (with-slots (real-error name) condition
               (format stream "[ERROR] System ~S could not be installed with ~A:~%  ~A"
                       name
                       (type-of real-error)
                       real-error)))))
