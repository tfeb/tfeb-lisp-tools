;;;; Install module providers
;;;
;;; This is reliant on require-modules: its job is to take the list of
;;; module providers it can build and copy the corresponding files
;;; into a suitable tree.
;;;

#-org.tfeb.tools.require-module
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "No require-module"))

(defpackage :org.tfeb.tools.install-providers
  (:use :cl :org.tfeb.tools.require-module)
  #+LispWorks
  (:import-from :lw #:copy-file)
  (:export
   #:install-providers))

(in-package :org.tfeb.tools.install-providers)

(provide-module :org.tfeb.tools.install-providers)

(defun module->directory-components (m &key (case ':preserve))
  (loop for start = 0 then (1+ next)
        for next = (position #\. m :start start)
        for c = (subseq m start next)
        collect (ecase case
                  ((:preserve) c)
                  ((:upcase) (string-upcase c))
                  ((:downcase) (string-downcase c)))
        until (null next)))

#-LispWorks
(defun copy-file (from to)
  ;; I'm sure there are now portability packages to do this but I do
  ;; not want to rely on them.  This is a naive implementation which
  ;; is not terrible.
  (with-open-file (out to :direction ':output
                       :if-exists ':supersede
                       :element-type '(unsigned-byte 8))
    (with-open-file (in from :direction ':input
                        :element-type '(unsigned-byte 8))
      (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
            for pos = (read-sequence buffer in)
            while (> pos 0)
            do (write-sequence buffer out :end pos)
            finally (return (values from to))))))

(defun install-providers (root &key (providers *module-providers* providersp)
                               (really nil)
                               (clear (and really (not providersp)))
                               (filter nil filterp))
  (loop with rp = (pathname root)
        for (module . sp) in providers
        for dc = (let ((try (module->directory-components
                             module
                             :case (typecase rp
                                     (logical-pathname ':upcase)
                                     (t ':downcase)))))
                   (if (and (not (null try))
                            (string-equal (car (last try)) (pathname-name sp)))
                       (butlast try)
                     try))
        for tp = (merge-pathnames
                  (make-pathname
                   :host (pathname-host rp)
                   :directory `(:relative ,@dc)
                   :version :newest ; hack
                   :defaults sp)
                  rp)
        when (or (not filterp) (funcall filter module sp tp))
        do
        (if really
          (progn
            (ensure-directories-exist tp)
            (copy-file sp tp))
          (format *standard-output*
                  "~&Would~% ensure dirs for ~A~% copy ~A~%   to ~A~%"
                  tp sp tp))
        and when really collect (cons sp tp)
        finally (when clear
                  (if (not providersp)
                      (setf *module-providers* '())
                    (warn "Can't clear providers as it's not *MODULE-PROVIDERS*")))))
