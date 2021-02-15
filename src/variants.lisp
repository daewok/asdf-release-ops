;;;; Variants
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.
;;;;
;;;; This file contains mixins and generic functions useful for defining and
;;;; selecting different variants of operations.

(in-package #:asdf-release-ops)

(defclass static-abstract-op ()
  ())
(defclass dynamic-abstract-op ()
  ())

(defclass build-abstract-op (asdf:operation)
  ())
(defclass static-build-abstract-op (static-abstract-op build-abstract-op)
  ())
(defclass dynamic-build-abstract-op (dynamic-abstract-op build-abstract-op)
  ())

(defclass release-abstract-op (asdf:operation)
  ())
(defclass static-release-abstract-op (static-abstract-op release-abstract-op)
  ())
(defclass dynamic-release-abstract-op (dynamic-abstract-op release-abstract-op)
  ())

(defgeneric release-op-key (o)
  (:documentation
   "Return a key describing the release op.")
  (:method ((o static-release-abstract-op))
    "static")
  (:method ((o dynamic-release-abstract-op))
    "dynamic"))

(defgeneric matching-variant-of (target-variant op-name))

(defgeneric abstract-operation-of (op)
  (:method ((op symbol))
    (abstract-operation-of (asdf:make-operation op))))

(defmethod asdf:component-depends-on ((op build-abstract-op) (c asdf:system))
  (append
   (cdr (assoc (abstract-operation-of op) (asdf::component-in-order-to c)))
   (call-next-method)))

(defmethod asdf:component-depends-on ((op release-abstract-op) (c asdf:system))
  (append
   (cdr (assoc (abstract-operation-of op) (asdf::component-in-order-to c)))
   (call-next-method)))

(defmacro define-build-op (op-name direct-superclasses direct-slots &rest options)
  (let ((static-name (intern (uiop:strcat (string 'static-) (string op-name))))
        (dynamic-name (intern (uiop:strcat (string 'dynamic-) (string op-name)))))
    `(progn
       (defclass ,op-name (build-abstract-op ,@direct-superclasses)
         ,direct-slots
         ,@options)

       (defclass ,static-name (static-build-abstract-op ,op-name)
         ()
         (:documentation
          ,(uiop:strcat "Static variant of " (string op-name) ".")))
       (defmethod matching-variant-of ((target-variant static-abstract-op) (op-name (eql ',op-name)))
         ',static-name)
       (defmethod matching-variant-of ((target-variant (eql :static)) (op-name (eql ',op-name)))
         ',static-name)
       (defmethod abstract-operation-of ((op ,static-name))
         ',op-name)

       (defclass ,dynamic-name (dynamic-build-abstract-op ,op-name)
         ()
         (:documentation
          ,(uiop:strcat "Dynamic variant of " (string op-name) ".")))
       (defmethod matching-variant-of ((target-variant dynamic-abstract-op) (op-name (eql ',op-name)))
         ',dynamic-name)
       (defmethod matching-variant-of ((target-variant (eql :dynamic)) (op-name (eql ',op-name)))
         ',dynamic-name)
       (defmethod abstract-operation-of ((op ,dynamic-name))
         ',op-name))))

(defmacro define-release-op (op-name direct-superclasses direct-slots &rest options)
  (let ((static-name (intern (uiop:strcat (string 'static-) (string op-name))))
        (dynamic-name (intern (uiop:strcat (string 'dynamic-) (string op-name)))))
    `(progn
       (defclass ,op-name (release-abstract-op ,@direct-superclasses)
         ,direct-slots
         ,@options)

       (defclass ,static-name (static-release-abstract-op ,op-name)
         ()
         (:documentation
          ,(uiop:strcat "Static variant of " (string op-name) ".")))
       (defmethod matching-variant-of ((target-variant static-abstract-op) (op-name (eql ',op-name)))
         ',static-name)
       (defmethod matching-variant-of ((target-variant (eql :static)) (op-name (eql ',op-name)))
         ',static-name)
       (defmethod abstract-operation-of ((op ,static-name))
         ',op-name)

       (defclass ,dynamic-name (dynamic-release-abstract-op ,op-name)
         ()
         (:documentation
          ,(uiop:strcat "Dynamic variant of " (string op-name) ".")))
       (defmethod matching-variant-of ((target-variant dynamic-abstract-op) (op-name (eql ',op-name)))
         ',dynamic-name)
       (defmethod matching-variant-of ((target-variant (eql :dynamic)) (op-name (eql ',op-name)))
         ',dynamic-name)
       (defmethod abstract-operation-of ((op ,dynamic-name))
         ',op-name))))
