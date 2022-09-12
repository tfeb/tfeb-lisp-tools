;;;; ASDF sysdcl for TFEB tools
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.tools"
  :description "TFEB tools"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :licence "MIT"
  :homepage "https://github.com/tfeb/tfeb-lisp-tools"
  :in-order-to ((test-op (load-op "org.tfeb.tools/test")))
  :components
  ((:file "require-module")
   (:file "install-providers" :depends-on ("require-module"))
   (:file "build-modules" :depends-on ("require-module"))
   (:file "feature-expressions")
   (:file "deprecations")
   (:file "asdf-module-sysdcls")
   (:file "tools-cometh" :depends-on ("require-module"
                                      "install-providers"
                                      "build-modules"
                                      "feature-expressions"
                                      "deprecations"
                                      "asdf-module-sysdcls"))))

(defsystem "org.tfeb.tools/test"
  :description "TFEB tools tests"
  :version (:read-file-line "VERSION")
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/tfeb-lisp-tools"
  :depends-on ("org.tfeb.tools")
  :pathname "test/"
  :components
  ((:file "test-require-module")))
