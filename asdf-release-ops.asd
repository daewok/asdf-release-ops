;;;; asdf-release-ops System Definition
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.

(defsystem #:asdf-release-ops
  :description "A set of ASDF operations useful for releasing code."
  :license "BSD-2-Clause"
  :pathname "src/"
  :version "0.0.1"
  :components ((:file "package")
               (:file "variants" :depends-on ("package"))
               (:file "components" :depends-on ("package" "variants"))
               (:file "runtime-linking" :depends-on ("package"))
               (:file "build-ops" :depends-on ("package"
                                               "variants"
                                               "runtime-linking"
                                               "lisp-invocation"))
               (:file "release-ops" :depends-on ("package"
                                                 "variants"))
               (:file "lisp-invocation" :depends-on ("package"))))

(defsystem #:asdf-release-ops/lisp-invocation
  :description "A helper system. Simply used to load lisp-invocation when needed."
  :license "BSD-2-Clause"
  :depends-on (#:lisp-invocation))

(defsystem #:asdf-release-ops/cffi-toolchain
  :description "A helper system. Simply used to load cffi-toolchain when needed."
  :license "BSD-2-Clause"
  :depends-on (#:cffi-toolchain))

(defsystem #:asdf-release-ops/osicat
  :description "A helper system. Simply used to load osicat when needed."
  :license "BSD-2-Clause"
  :pathname "src/"
  :components ((:file "osicat"))
  :depends-on ((:feature (:not :os-windows) #:osicat)
               #:asdf-release-ops))

(defsystem #:asdf-release-ops/archive
  :description "A helper system. Simply used to load archive related dependencies when needed."
  :license "BSD-2-Clause"
  :pathname "src/"
  :components ((:file "release-archive-op"))
  :depends-on (#:archive
               #:salza2
               #:zippy
               #:asdf-release-ops))
