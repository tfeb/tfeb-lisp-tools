;;;;
;;;

(in-package :org.tfeb.tools.require-module/test)

(format *debug-io* "~& - loading ~A" *load-truename*)

(require-module "x")
(require-module "y")
