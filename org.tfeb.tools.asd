;;;; ASDF sysdcl for TFEB tools
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.tools"
  :description "TFEB tools"
  :version "1.0.0"
  :author "Tim Bradshaw"
  :licence "MIT"
  :homepage "https://github.com/tfeb/tfeb-lisp-tools"
  :components
  ((:file "require-module")
   (:file "install-providers" :depends-on ("require-module"))
   (:file "tools-cometh" :depends-on ("require-module"
                                      "install-providers"))))
