;;;; Variants
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.
;;;;
;;;; This file contains mixins and generic functions useful for defining and
;;;; selecting different variants of operations.

(in-package #:asdf-release-ops)

(defvar *ops* nil
  "A list of build operations defined through DEFINE-OP. Each item is a
list. The first element is the name of the abstract operation, the second is
the name of the static variant, the third is the name of the dynamic variant.")

(defvar *variants* nil
  "A list of variants defined through DEFINE-OP-VARIANT. Each item is a
list. The first element is the name of the abstract operation, the second is
the name of the static variant, the third is the name of the dynamic variant.")

(defclass abstract-op (asdf:operation)
  ())
(defclass static-abstract-op (abstract-op)
  ())
(defclass dynamic-abstract-op (abstract-op)
  ())

(defgeneric op-variant-key (op)
  (:documentation
   "Return a key describing the variant of the op."))

(defgeneric matching-variant-of (target-variant op-name))

(defgeneric abstract-operation-of (op)
  (:method ((op symbol))
    (abstract-operation-of (asdf:make-operation op))))

(defmacro define-op (op-name direct-superclasses direct-slots &rest options)
  "Define an abstract operation as well as its static and dynamic variants."
  (let ((static-name (intern (uiop:strcat (string 'static-) (string op-name))))
        (dynamic-name (intern (uiop:strcat (string 'dynamic-) (string op-name)))))
    `(progn
       (defclass ,op-name (abstract-op ,@direct-superclasses)
         ,direct-slots
         ,@options)

       (defclass ,static-name (static-abstract-op ,op-name)
         ()
         (:documentation
          ,(uiop:strcat "Static variant of " (string op-name) ".")))
       (defmethod matching-variant-of ((target-variant static-abstract-op) (op-name (eql ',op-name)))
         ',static-name)
       (defmethod abstract-operation-of ((op ,static-name))
         ',op-name)
       (defmethod op-variant-key ((op ,static-name))
         "static")

       (defclass ,dynamic-name (dynamic-build-abstract-op ,op-name)
         ()
         (:documentation
          ,(uiop:strcat "Dynamic variant of " (string op-name) ".")))
       (defmethod matching-variant-of ((target-variant dynamic-abstract-op) (op-name (eql ',op-name)))
         ',dynamic-name)
       (defmethod abstract-operation-of ((op ,dynamic-name))
         ',op-name)
       (defmethod op-variant-key ((op ,dynamic-name))
         "dynamic")

       (pushnew (list ',op-name ',static-name ',dynamic-name) *ops*
                :test #'equal))))

(defmacro define-op-variant (variant-name)
  (let* ((variant-base-name (intern (uiop:strcat (string variant-name) (string '-abstract-op))))
         (variant-static-name (intern (uiop:strcat (string variant-name) (string '-static-abstract-op))))
         (variant-dynamic-name (intern (uiop:strcat (string variant-name) (string '-dynamic-abstract-op))))
         (dynamic-key (uiop:strcat (string variant-name) (string '-dynamic)))
         (static-key (uiop:strcat (string variant-name) (string '-static))))
    `(progn
       (defclass ,variant-base-name ()
         ())
       (defclass ,variant-static-name (,variant-base-name static-abstract-op)
         ())
       (defclass ,variant-dynamic-name (,variant-base-name dynamic-abstract-op)
         ())

       (defmethod op-variant-key ((op ,variant-dynamic-name))
         ,dynamic-key)
       (defmethod op-variant-key ((op ,variant-static-name))
         ,static-key)

       `@(loop
           :for (op static-op dynamic-op) :in *ops*
           :for variant-op-name := (intern (uiop:strcat (string variant-name) (string '-) (string op)))
           :for variant-op-static-name := (intern (uiop:strcat (string variant-name) (string '-) (string static-op)))
           :for variant-op-dynamic-name := (intern (uiop:strcat (string variant-name) (string '-) (string dynamic-op)))
           :append (list
                    `(defclass ,variant-op-name (,variant-base-name ,op)
                       ())
                    `(defclass ,variant-op-static-name (,variant-static-name ,static-op)
                       ())
                    `(defclass ,variant-op-dynamic-name (,variant-dynamic-name ,dynamic-op)
                       ())
                    `(defmethod matching-variant-of ((target-variant ,variant-static-name) (op-name (eql ',op)))
                       ',variant-op-static-name)
                    `(defmethod matching-variant-of ((target-variant ,variant-dynamic-name) (op-name (eql ',op)))
                       ',variant-op-dynamic-name)))

       (pushnew (list ',variant-base-name ',variant-static-name ',variant-dynamic-name) *variants*
                :test #'equal))))

(defmethod asdf:component-depends-on ((op abstract-op) (c asdf:system))
  (append
   (cdr (assoc (abstract-operation-of op) (asdf::component-in-order-to c)))
   (call-next-method)))
