;;;; Module org.tfeb.tools.deprecations of org.tfeb.tools
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.tools.deprecations"
  :description
  "A subsystem of the TFEB tools"
  :version
  (:read-file-line "VERSION")
  :author
  "Tim Bradshaw"
  :licence
  "MIT"
  :homepage
  "https://github.com/tfeb/tfeb-lisp-tools"
  :components
  ((:file "deprecations")))
