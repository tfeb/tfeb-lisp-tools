;;;; Some extremely rudimentary tests
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.binding
 #+Quicklisp
 ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.tools.require-module/test
  (:use :cl :org.tfeb.tools.require-module :org.shirakumo.parachute))

(in-package :org.tfeb.tools.require-module/test)

(clear-module-caches)

(defvar *mpds* (module-path-descriptions-for-function
                (lambda ()
                  (or *compile-file-truename* *load-truename*))
                '((:name :wild :type "lisp"))))

(format *debug-io* "~&* Loading default~%")
(require-module "rm.t" :module-path-descriptions *mpds*)

(format *debug-io* "~&* Loading with compilation~%")
(require-module "rm.t" :module-path-descriptions *mpds* :compile t)

(format *debug-io* "~&* Reload~%")
(require-module "rm.t" :module-path-descriptions *mpds* :reload t)
