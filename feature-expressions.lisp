;;;; Ensuring features are present
;;;

(defpackage :org.tfeb.tools.feature-expressions
  (:use :cl)
  (:export
   #:evaluate-feature-expression
   #:feature-case
   #:ensuring-features))

(in-package :org.tfeb.tools.feature-expressions)

(provide :org.tfeb.tools.feature-expressions)

;;; This is useful so you can conditionalise this code itself
(pushnew :org.tfeb.tools.feature-expressions *features*)

(defun featurep (thing)
  ;; evaluate an atomic feature expression
  (and (member (string thing) *features*
               :test #'string-equal :key #'symbol-name)
       t))

(defun evaluate-feature-expression (expression)
  ;; This should implement what #+ / #- implement, with the possible
  ;; exception that OR, AND and NOT need to be CL:OR, CL:AND, CL:NOT
  (typecase expression
    (cons
     (destructuring-bind (op . arguments) expression
       (case op
         (or
          (some #'evaluate-feature-expression arguments))
         (and
          (every #'evaluate-feature-expression arguments))
         (not
          (unless (= (length arguments) 1)
            (error "Bad feature expression"))
          (not (evaluate-feature-expression (first arguments))))
         (otherwise
          (error "unknown feature operator ~S" op)))))
    (t
     (featurep expression))))

(define-compiler-macro evaluate-feature-expression (&whole form expression)
  (if (constantp expression)
      (labels ((compile-expression (expression)
                 (typecase expression
                   (cons
                    (destructuring-bind (op . arguments) expression
                      (case op
                        ((or and)
                         `(,op ,@(mapcar #'compile-expression arguments)))
                        ((not)
                         (unless (= (length arguments) 1)
                           (error "Bad feature expression"))
                         `(not ,(compile-expression (first arguments))))
                        (otherwise
                         (error "unknown feature operator ~S" op)))))
                   (t `(featurep ',expression)))))
        (compile-expression (if (and (listp expression)
                                     (eql (first expression) 'quote)
                                     (= (length expression) 2))
                                (second expression)
                              expression)))
    form))

(defmacro feature-case (&body clauses)
  `(cond
    ,@(mapcar (lambda (clause)
                (unless (consp clause)
                  (error "illegal atomic clause"))
                (destructuring-bind (expression &rest forms) clause
                  (case expression
                    ((otherwise t)
                     `(t ,@forms))
                    (otherwise
                     `((evaluate-feature-expression ',expression)
                       ,@forms)))))
              clauses)))

(define-condition feature-error (error)
  ((feature :initarg :feature
            :reader feature-error-feature)
   (time :initarg :time
         :reader feature-error-time))
  (:report
   (lambda (mfe stream)
     (format stream "Feature expression ~S failed at ~S time"
             (feature-error-feature mfe)
             (feature-error-time mfe)))))

(defmacro ensuring-features (&body clauses)
  ;; The interface
  `(progn
     ,@(mapcar (lambda (clause)
                 (destructuring-bind (time . feature-expressions) clause
                   `(eval-when ,(if (eq time t)
                                    '(:compile-toplevel :load-toplevel :execute)
                                  time)
                      ,@(mapcar (lambda (feature-expression)
                                  `(unless (evaluate-feature-expression
                                            ',feature-expression)
                                     (error 'feature-error
                                            :feature ',feature-expression
                                            :time ',time)))
                                feature-expressions))))
               clauses)))

(ensuring-features
 ((:load-toplevel)
  :org.tfeb.tools.feature-expressions))
