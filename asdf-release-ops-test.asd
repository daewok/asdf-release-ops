;;;; asdf-release-ops Test System Definitions
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.

(asdf:load-system :asdf-release-ops)

(defsystem #:asdf-release-ops-test
  :description "Test system for asdf-release-ops."
  :license "BSD-2-Clause"
  :version (:read-file-form "version.lisp-expr")
  :pathname "test/"
  :serial t
  :components ((:file "package")
               (:file "lisp-curl"))
  :depends-on (#:asdf-release-ops
               #:fiveam))

(defsystem #:asdf-release-ops-test/lisp-curl
  :description "A test system that makes a curl-like executable (with far fewer options) using Drakma."
  :license "BSD-2-Clause"
  :version (:read-file-form "version.lisp-expr")
  :entry-point "asdf-release-ops-test/lisp-curl::main"
  :defsystem-depends-on (#:asdf-release-ops)
  :class "asdf-release-ops:release-system"
  :pathname "test/lisp-curl/"
  :release-program-file-name "lisp-curl"
  :release-staging-directory "build/release-staging/"
  :release-directory "releases/"
  :components ((:file "lisp-curl"))
  :depends-on (#:drakma))

(defmethod asdf-release-ops:program-static-image-features ((op asdf-release-ops:static-program-image-op)
                                                           (s (eql (asdf:find-system :asdf-release-ops-test/lisp-curl))))
  (list :cl+ssl-foreign-libs-already-loaded))
