;;;; Writing ASDF sysdcls for modules
;;;

(defpackage :org.tfeb.tools.asdf-module-sysdcls
  (:use :cl :asdf)
  (:export
   #:write-asdf-module-sysdcl
   #:define-asdf-module-sysdcls))

(in-package :org.tfeb.tools.asdf-module-sysdcls)

(provide :org.tfeb.tools.asdf-module-sysdcls)

(defun write-asdf-module-sysdcl (module/desc of
                                    &key
                                    (preface '())
                                    (default-pathname
                                     (or *load-pathname*
                                         *default-pathname-defaults*)))
  (multiple-value-bind (module module-preface)
      (etypecase module/desc
        (string (values module/desc '()))
        (list (values (car module/desc) (cdr module/desc))))
    (let* ((system-name (concatenate 'string of "." module))
           (sysdcl-name (make-pathname :name system-name
                                       :type "asd"
                                       :defaults default-pathname)))
      (with-open-file (sysdcl sysdcl-name
                              :direction :output
                              :if-exists :supersede)
        (with-standard-io-syntax
          (let ((*package* (load-time-value (find-package :asdf-user)))
                (*print-case* :downcase))
            (format sysdcl ";;;; Module ~A of ~A~%;;;~%"
                    system-name of)
            (format sysdcl "~%(in-package :asdf-user)~%")
            (pprint
             `(defsystem ,system-name
                ,@preface
                ,@module-preface
                :components
                ((:file ,module)))
             sysdcl)
            (terpri sysdcl))
          (values system-name sysdcl-name))))))

(defmacro define-asdf-module-sysdcls (of (&rest prefacery &key &allow-other-keys)
                                         &body modules/descs)
  `(progn
     ,@(mapcar (lambda (module/desc)
                 `(write-asdf-module-sysdcl
                   ',module/desc ',of
                   :preface ',prefacery))
               modules/descs)))
