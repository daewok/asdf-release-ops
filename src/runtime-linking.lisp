;;;; Runtime Linking
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.

(in-package #:asdf-release-ops)

(defparameter *library-name-to-pkg-config-name*
  '())

(defgeneric runtime-linker-options (o s))

(defgeneric library-pathname-to-linker-option (o s pn))

(defgeneric library-pkg-config-name (o s pn)
  (:method (o s pn)
    (let* ((file-name (file-namestring pn))
           (libname (first (uiop:split-string file-name :separator '(#\.)))))
      (or (cdr (assoc file-name *library-name-to-pkg-config-name*
                      :test #'equal))
          (cdr (assoc libname *library-name-to-pkg-config-name*
                      :test #'equal))
          libname))))

(defun internal-library-pathnames (o s)
  (asdf::with-asdf-session (:override t)
    (let ((input-files (asdf:input-files (matching-variant-of o 'program-image-op) s)))
      (remove-if-not (lambda (pn)
                       (equal (pathname-type pn) (asdf/bundle:bundle-pathname-type :dll)))
                     input-files))))
