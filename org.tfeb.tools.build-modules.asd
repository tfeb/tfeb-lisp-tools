;;;; Module org.tfeb.tools.build-modules of org.tfeb.tools
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.tools.build-modules"
  :description
  "A subsystem of the TFEB tools"
  :version
  "7.2.0"
  :author
  "Tim Bradshaw"
  :licence
  "MIT"
  :homepage
  "https://github.com/tfeb/tfeb-lisp-tools"
  :depends-on
  ("org.tfeb.tools.require-module")
  :components
  ((:file "build-modules")))
