;;;; asdf-release-ops lisp-curl test
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.

(in-package #:asdf-release-ops-test)

(def-suite :asdf-release-ops)

(in-suite :asdf-release-ops)

(test lisp-curl-dynamic-executable
  (finishes
    (asdf:operate 'dynamic-program-op :asdf-release-ops-test/lisp-curl))
  (finishes
    (uiop:run-program (list (uiop:native-namestring (asdf:output-file 'dynamic-program-op :asdf-release-ops-test/lisp-curl))
                            "https://google.com")
                      :error-output :interactive)))

(test lisp-curl-static-executable
  (finishes
    (asdf:operate 'static-program-op :asdf-release-ops-test/lisp-curl))
  (finishes
    (uiop:run-program (list (uiop:native-namestring (asdf:output-file 'static-program-op :asdf-release-ops-test/lisp-curl))
                            "https://google.com")
                      :error-output :interactive)))

(test lisp-curl-dynamic-release-archive
  (finishes
    (asdf:operate 'dynamic-release-archive-op :asdf-release-ops-test/lisp-curl)))

(test lisp-curl-static-release-archive
  (finishes
    (asdf:operate 'static-release-archive-op :asdf-release-ops-test/lisp-curl)))
