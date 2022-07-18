;;;; Module org.tfeb.tools.deprecations of org.tfeb.tools
;;;

(in-package :asdf-user)

(asdf/parse-defsystem:defsystem "org.tfeb.tools.deprecations"
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
  :components
  ((:file "deprecations")))
