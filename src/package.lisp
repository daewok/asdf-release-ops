;;;; Package definition for asdf-release-ops
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.

(uiop:define-package #:asdf-release-ops
    (:use #:cl)
  ;; components
  (:export #:release-system
           #:release-system-inferred-system
           #:release-system-license-op
           #:release-system-dependencies-license-op
           #:release-system-readme-op
           #:release-system-program-op)
  ;; Base ops
  (:export #:define-op
           #:define-op-variant
           #:matching-variant-of)
  ;; build-ops
  (:export #:perform-program-image-op
           #:dynamic-perform-program-image-op
           #:static-perform-program-image-op

           #:program-foreign-library-list-op
           #:static-program-foreign-library-list-op
           #:dynamic-program-foreign-library-list-op

           #:program-image-op
           #:dynamic-program-image-op
           #:static-program-image-op

           #:program-linkage-info-op
           #:dynamic-program-linkage-info-op
           #:static-program-linkage-info-op

           #:program-linkage-table-prelink-info-c-op
           #:dynamic-program-linkage-table-prelink-info-c-op
           #:static-program-linkage-table-prelink-info-c-op

           #:program-linkage-table-prelink-info-o-op
           #:dynamic-program-linkage-table-prelink-info-o-op
           #:static-program-linkage-table-prelink-info-o-op

           #:program-op
           #:dynamic-program-op
           #:static-program-op

           #:program-runtime-op
           #:dynamic-program-runtime-op
           #:static-program-runtime-op

           #:program-static-image-op
           #:dynamic-program-static-image-op
           #:static-program-static-image-op

           #:program-system-list-op
           #:dynamic-program-system-list-op
           #:static-program-system-list-op

           #:program-image-features
           #:program-static-image-features)

  ;; release-ops
  (:export #:release-archive-op
           #:dynamic-release-archive-op
           #:static-release-archive-op

           #:release-stage-op
           #:dynamic-release-stage-op
           #:static-release-stage-op))

(in-package #:asdf-release-ops)
