;;;; Some rudimentary support for building modules
;;;

#-org.tfeb.tools.require-module
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "No require-module"))

(defpackage :org.tfeb.tools.build-modules
  (:use :cl :org.tfeb.tools.require-module)
  (:export
   #:compile-installed-modules))

(in-package :org.tfeb.tools.build-modules)

(provide :org.tfeb.tools.build-modules)

(defun pathname-matches-any-p (pathname wilds)
  ;; Does PATHNAME match any of a number of possible wildcards?
  (dolist (wild wilds nil)
    (when (pathname-match-p pathname
                            (merge-pathnames (pathname wild) pathname))
      (return-from pathname-matches-any-p t))))

(defun compile-installed-modules (prefix files &key
                                         (omit '() omitp)
                                         (only '() onlyp)
                                         (force nil)
                                         (verbose nil) (pretend nil))
  (let ((pfx (string-upcase (string prefix)))
        (compiled '()))
    (dolist (file files (nreverse compiled))
       (let ((path (pathname file)))
        (unless (and omitp (pathname-matches-any-p path
                                                   (if (listp omit)
                                                       omit
                                                     (list omit))))
          (when (or (not onlyp) (pathname-matches-any-p path
                                                        (if (listp only)
                                                            only
                                                          (list only))))
            (let ((module-name (format nil "~A.~A"
                                       pfx
                                       (string-upcase (pathname-name
                                                       (pathname file))))))
              (multiple-value-bind (load-file source-file source-date
                                              fasl-file fasl-date)
                  (locate-module module-name)
                (declare (ignore source-date fasl-date))
                (cond
                 ((not load-file)
                  (error "found nothing for ~A, from ~A" module-name file))
                 ((not source-file)
                  (warn "no source for ~A, from ~A, but fasl ~A"
                        module-name file fasl-file))
                 ((and (not force)
                       (equal load-file fasl-file))
                  (when verbose
                    (format *debug-io*
                            "~&~A ~A~% for ~A~% from ~A~% as fasl is newer~%"
                            (if pretend "Would skip" "Skipping")
                            source-file module-name file)))
                 (t
                  (when verbose
                    (format *debug-io*
                            "~&~A ~A~% for ~A~% from ~A~%"
                            (if pretend "Would compile" "Compiling")
                            source-file module-name path))
                  (push (list path source-file module-name) compiled)
                  (unless pretend
                    (compile-file source-file))))))))))))
