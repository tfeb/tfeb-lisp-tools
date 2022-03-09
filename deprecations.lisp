;;;; Deprecations
;;;

(defpackage :org.tfeb.tools.deprecations
  (:use :cl)
  (:export
   #:*inhibit-deprecation-warnings*
   #:deprecation-warning
   #:deprecation-warning-thing
   #:deprecation-warning-what
   #:deprecation-warning-notice
   #:deprecation-warning-location
   #:with-deprecations
   #:map-deprecations
   #:clear-deprecations
   #:report-deprecations
   #:define-deprecated-function
   #:define-deprecated-generic-function
   ;; See below for why these can't work
   ;#:define-deprecated-variable
   ;#:define-deprecated-parameter
   ;#:define-deprecated-constant
   #:define-deprecated-macro
   #:define-deprecated-symbol-macro))

(in-package :org.tfeb.tools.deprecations)

(provide :org.tfeb.tools.deprecations)

(defvar *inhibit-deprecation-warnings* nil)

(defun pretty-what (what)
  (case what
    ((:function) "function")
    ((:generic-function) "generic function")
    ((:macro) "macro")
    ((:symbol-macro) "symbol macro")
    (otherwise "Illudium Q-36 Explosive Space Modulator")))

(define-condition deprecation-warning (style-warning)
  ((thing :initarg :thing
          :initform (error "must specify thing")
          :reader deprecation-warning-thing)
   (what :initarg :what
         :initform (error "must specify what")
         :reader deprecation-warning-what)
   (notice :initarg :notice
           :initform nil
           :reader deprecation-warning-notice)
   (location :initarg :location
             :initform *compile-file-truename*
             :reader deprecation-warning-location))
  (:report (lambda (dw stream)
             (format stream "deprecated ~A ~S~@[ in ~A~]~@[ (~A)~]"
                     (pretty-what (deprecation-warning-what dw))
                     (deprecation-warning-thing dw)
                     (deprecation-warning-location dw)
                     (deprecation-warning-notice dw)))))

(defun note-deprecated (thing what &key
                              (notice nil)
                              (location *compile-file-truename*))
  (pushnew (list thing what notice)
           (gethash *compile-file-truename* *deprecation-locations*)
           :test #'equal)
  (unless *inhibit-deprecation-warnings*
    (warn 'deprecation-warning
          :thing thing :what what
          :notice notice
          :location location)))

(defvar *deprecation-locations*
  (make-hash-table :test #'equal))

(defmacro with-deprecations ((&key (inhibit nil)) &body forms)
  `(let ((*inhibit-deprecation-warnings* ,inhibit)
         (*deprecation-locations* (make-hash-table :test #'equal)))
     ,@forms))

(defun map-deprecations (f)
  (maphash (lambda (location deprecations)
             ;; Get things in a good order
             (let ((things '()) (whats '()) (notices '()))
               (dolist (deprecation deprecations)
                 (destructuring-bind (thing what notice) deprecation
                   (push thing things)
                   (push what whats)
                   (push notice notices)))
             (funcall f location things whats notices)))
             *deprecation-locations*)
  (values))

(defun clear-deprecations ()
  (clrhash *deprecation-locations*)
  (values))

(defun report-deprecations (&key (stream t) (clear nil))
  (map-deprecations
   (lambda (location things whats notices)
     (format stream "~&~A:~%" (or location "(no file)"))
     (mapc (lambda (thing what notice)
             (format stream " ~A ~S~@[ (~A)~]~%" (pretty-what what) thing notice))
           things whats notices)))
  (when clear
    (clear-deprecations))
  (values))

;;; Functiony things
;;;

(defun make-deprecated-functiony-form (definer name arglist forms)
  ;; This won't work for methods because of qualifiers
  (let ((what (case definer
                ((defun) :function)
                ((defgeneric) :generic-function)
                (otherwise :thing)))
        (deprecation-notice (case definer
                              ((defgeneric)
                               (second (assoc ':documentation forms)))
                              (t
                               (if (and (> (length forms) 1) (stringp (first forms)))
                                   (first forms)
                                 nil)))))
    `(progn
       (,definer ,name ,arglist ,@forms)
       (define-compiler-macro ,name (&whole form &rest args)
         (declare (ignore args))
         (note-deprecated ',name ',what
                          :notice ',deprecation-notice)
         form))))

(defmacro define-deprecated-function (name args &body forms)
  (make-deprecated-functiony-form 'defun name args forms))

(defmacro define-deprecated-generic-function (name args &body forms)
  (make-deprecated-functiony-form 'defgeneric name args forms))

;;; No variably things
;;;
;;; Variables really can't work using this approach, anyway.  If you
;;; define a global symbol macro then binding will be lexical, which
;;; alters the semantics of code, which is hopeless.  You can't define
;;; symbol macros for symbols which have special declarations, so you
;;; can't make it so that bindings would be dynamic.  So I don't think
;;; there is any solution to this problem.  You could leave it in
;;; place for constants only but that would mean bindings for
;;; 'constants' work, which is also bad.  So the the code below is one
;;; way not to solve the problem.
;;;

#||
(defmacro deprecated-variable (name real-name what deprecation-notice)
  ;; When I wrote this I did not realise that SETF will expand macro
  ;; forms as places, so I expected assignmet would need another hack.
  ;; But it does.
  (note-deprecated name what
                   :notice deprecation-notice)
  real-name)

(defun make-deprecated-variably-form (definer name stuff)
  ;; A result of this approach is that SYMBOL-VALUE will not work,
  ;; because the real variable is stashed away
  (let ((what (case definer
                ((defvar) :variable)
                ((defparameter) :parameter)
                ((defconstant) :constant)
                (otherwise :thing)))
        (real-name (make-symbol (symbol-name name)))
        (deprecation-notice (second stuff)))
    `(progn
       (,definer ,real-name ,@stuff)
       (define-symbol-macro ,name
         (deprecated-variable ,name ,real-name ,what ,deprecation-notice)))))

(defmacro define-deprecated-variable (name &body stuff)
  (make-deprecated-variably-form 'defvar name stuff))

(defmacro define-deprecated-parameter (name &body stuff)
  (make-deprecated-variably-form 'defparameter name stuff))

(defmacro define-deprecated-constant (name &body stuff)
  (make-deprecated-variably-form 'defconstant name stuff))
||#

;;; Macroid things
;;;

(defmacro define-deprecated-macro (name args &body decls/forms)
  (let* ((forms (member-if (lambda (thing)
                             (or (not (consp thing))
                                 (not (eql (car thing) 'declare))))
                           decls/forms))
         (decls (ldiff decls/forms forms))
         (deprecation-notice (if (and (> (length decls/forms) 1)
                                      (stringp (first decls/forms)))
                                 (first decls/forms)
                               nil)))
    `(defmacro ,name ,args
       ,@decls
       (note-deprecated ',name ':macro
                        :notice ',deprecation-notice)
       ,@forms)))

(defmacro define-deprecated-symbol-macro (name expansion)
  `(define-symbol-macro ,name
     (progn
       (note-deprecated ',name ':symbol-macro)
       ,expansion)))
