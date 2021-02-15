;;;; Release Ops
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.
;;;;
;;;; This file contains operations useful for creating releases.

(in-package #:asdf-release-ops)

(defclass downward-release-op (asdf:downward-operation)
  ()
  (:documentation
   "Mixin for any downward release operations. When applied to a
release-ops-system, it also applies the downward operation to the correct
release module."))
(defmethod asdf:component-depends-on ((o downward-release-op) (system release-system))
  (list* (list (asdf:downward-operation o) (release-system-release-module o system))
         (call-next-method)))

(define-release-op release-archive-op (asdf:selfward-operation)
  ()
  (:documentation
   "Create an archive for the release."))

(defmethod asdf:selfward-operation ((o release-archive-op))
  (matching-variant-of o 'release-stage-op))

(defmethod asdf:component-depends-on ((o release-archive-op) (s asdf:system))
  ;; There *seems* to be a bug that produces these warnings when S
  ;; :defsystem-depends-on a package-inferred-system.
  (handler-bind ((asdf/plan::dependency-not-done
                   #'muffle-warning))
    `((asdf:load-op ,(asdf:find-system "asdf-release-ops/archive"))
      ,@(mapcar (lambda (action)
                  (cons (car action) (list (cdr action))))
                (asdf/plan::collect-dependencies (asdf:make-operation
                                                  (matching-variant-of o 'release-stage-op))
                                                 s
                                                 :other-systems nil
                                                 :component-type t
                                                 :keep-component t
                                                 :keep-operation (matching-variant-of o 'release-stage-op)))
      ,@(call-next-method))))

(defmethod asdf:input-files ((o release-archive-op) (s asdf:system))
  (remove-duplicates (asdf/bundle:direct-dependency-files o s :key 'asdf:output-files)
                     :test #'equal))

(defgeneric release-archive-type (o s)
  (:method (o s)
    (if (uiop:os-windows-p)
        :zip
        :tar.gz)))

(defmethod asdf:output-files ((o release-archive-op) (s asdf:system))
  (values
   (list (uiop:subpathname (release-op-release-directory o s)
                           (uiop:strcat
                            (asdf:primary-system-name s)
                            "-"
                            (release-system-version-designator s)
                            "-"
                            (os-arch-tuple)
                            (ecase (release-archive-type o s)
                              (:zip
                               ".zip")
                              (:tar.gz
                               ".tar.gz")))))
   (or (not (null (release-system-release-directory s)))
       (release-op-ignore-output-translations-p o s))))


(define-release-op release-stage-op (downward-release-op)
  ()
  (:documentation
   "Stage all release objects into a folder in the hierarchy specified by the
system object."))
(defmethod asdf:downward-operation ((o release-stage-op))
  o)
(defmethod asdf:component-depends-on ((o release-stage-op) c)
  (list* `(asdf:load-op ,(asdf:find-system "asdf-release-ops/osicat"))
         (call-next-method)))

(defgeneric release-op-ignore-output-translations-p (o s)
  (:documentation
   "If T, ASDF output translations are not applied to the outputs of the
release actions.")
  (:method (o s)
    t))

(defgeneric release-op-build-action (o c)
  (:method ((o release-abstract-op) (c release-license-file))
    (uiop:if-let ((op (release-system-license-op o (asdf:component-system c))))
      (cons op (asdf:component-system c))))
  (:method ((o release-abstract-op) (c release-dependencies-license-file))
    (uiop:if-let ((op (release-system-dependencies-license-op o (asdf:component-system c))))
      (cons op (asdf:component-system c))))
  (:method ((o release-abstract-op) (c release-readme-file))
    (uiop:if-let ((op (release-system-readme-op o (asdf:component-system c))))
      (cons op (asdf:component-system c))))
  (:method ((o release-abstract-op) (c release-program-file))
    (uiop:if-let ((op (release-system-program-op o (asdf:component-system c))))
      (cons op (asdf:component-system c)))))

(defgeneric release-op-default-files (o c)
  (:method (o c)
    nil)
  (:method (o (c release-license-file))
    (let* ((system (asdf:component-system c))
           (system-license (release-system-release-license-file system)))
      (if system-license
          (list (uiop:subpathname (asdf:component-pathname system)
                                  system-license))
          (dolist (name '("LICENSE" "LICENSE.md" "LICENSE.org" "LICENSE.txt"
                          "LICENCE" "LICENCE.md" "LICENCE.org" "LICENCE.txt"
                          "COPYING" "COPYING.md" "COPYING.org" "COPYING.txt"))
            (let ((pn (asdf:system-relative-pathname system name)))
              (when (probe-file pn)
                (return (list pn))))))))
  (:method (o (c release-dependencies-license-file))
    (let ((system (asdf:component-system c)))
      (dolist (name '("BUNDLED-LICENSES" "BUNDLED-LICENSES.md" "BUNDLED-LICENSES.org" "BUNDLED-LICENSES.txt"
                      "BUNDLED-LICENCES" "BUNDLED-LICENCES.md" "BUNDLED-LICENCES.org" "BUNDLED-LICENCES.txt"
                      "BUNDLED-COPYING" "BUNDLED-COPYING.md" "BUNDLED-COPYING.org" "BUNDLED-COPYING.txt"))
        (let ((pn (asdf:system-relative-pathname system name)))
          (when (probe-file pn)
            (return (list pn)))))))
  (:method (o (c release-readme-file))
    (let* ((system (asdf:component-system c))
           (system-readme (release-system-release-readme-file system)))
      (if system-readme
          (list (uiop:subpathname (asdf:component-pathname system)
                                  system-readme))
          (dolist (name '("README" "README.md" "README.org" "README.txt"))
            (let ((pn (asdf:system-relative-pathname system name)))
              (when (probe-file pn)
                (return (list pn)))))))))

(defmethod asdf:output-files ((o release-stage-op) (c release-file))
  (values (list (asdf:component-pathname c))
          (release-op-ignore-output-translations-p o c)))

(defmethod asdf:input-files ((o release-stage-op) (c release-file))
  (let ((build-action (release-op-build-action o c)))
    (if build-action
        (asdf:output-files (car build-action) (cdr build-action))
        (or (release-op-default-files o c)
            (error "No files provided and one could not be found automatically.")))))

(defmethod asdf:perform ((o release-stage-op) (c release-file))
  (let ((input-files (asdf:input-files o c))
        (output-files (asdf:output-files o c)))
    (assert (= (length input-files) (length output-files)))
    (loop :for in :in input-files
          :for out :in output-files
          :do (funcall (uiop:find-symbol* :copy-file :asdf-release-ops) in out))))

(defmethod asdf:component-depends-on ((o release-stage-op) (c release-file))
  (let ((build-action (release-op-build-action o c)))
    (if build-action
        (list* (list (car build-action) (cdr build-action))
               (call-next-method))
        (call-next-method))))
