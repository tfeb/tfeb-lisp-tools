;;;; Module org.tfeb.tools.require-module of org.tfeb.tools
;;;

(in-package :asdf-user)

(asdf/parse-defsystem:defsystem "org.tfeb.tools.require-module"
  :description
  "A subsystem of the TFEB tools"
  :version
  "8.0.1"
  :author
  "Tim Bradshaw"
  :licence
  "MIT"
  :homepage
  "https://github.com/tfeb/tfeb-lisp-tools"
  :components
  ((:file "require-module")))
