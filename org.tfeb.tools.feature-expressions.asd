;;;; Module org.tfeb.tools.feature-expressions of org.tfeb.tools
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.tools.feature-expressions"
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
  ((:file "feature-expressions")))
