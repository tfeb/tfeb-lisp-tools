;;;; Some extremely rudimentary tests for require-module
;;;

(defpackage :org.tfeb.tools.require-module/test
  (:use :cl :org.tfeb.tools.require-module))

(in-package :org.tfeb.tools.require-module/test)

(clear-module-caches)

(let ((*module-path-descriptions* (module-path-descriptions-for-function
                                   (lambda () *load-truename*)
                                   '((:name :wild :type "lisp")))))
  (format *debug-io* "~&* Loading default~%")
  (require-module "rm.t")
  (format *debug-io* "~&* Loading with compilation~%")
  (require-module "rm.t" :compile t)
  (format *debug-io* "~&* Reload~%")
  (require-module "rm.t" :reload t))
