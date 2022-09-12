;;;; Module org.tfeb.tools.require-module of org.tfeb.tools
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.tools.require-module"
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
  ((:file "require-module")))
