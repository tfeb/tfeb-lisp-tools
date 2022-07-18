;;;; Module org.tfeb.tools.install-providers of org.tfeb.tools
;;;

(in-package :asdf-user)

(asdf/parse-defsystem:defsystem "org.tfeb.tools.install-providers"
  :description
  "A subsystem of the TFEB tools"
  :version
  "8.0.0"
  :author
  "Tim Bradshaw"
  :licence
  "MIT"
  :homepage
  "https://github.com/tfeb/tfeb-lisp-tools"
  :depends-on
  ("org.tfeb.tools.require-module")
  :components
  ((:file "install-providers")))
