;;;; Integration with lisp-invocation library
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.

(in-package #:asdf-release-ops)

(defun runtime-pathname ()
  (or
   #+sbcl sb-ext:*runtime-pathname*
   (warn "RUNTIME-PATHNAME unimplemented for this implementation. Falling back to default path.")))

(defun eval-in-lisp (forms &rest keys &key lisp-path image-path &allow-other-keys)
  (declare (ignore lisp-path image-path))
  (apply #'invoke-lisp
         :eval (uiop:with-safe-io-syntax (:package :asdf)
                 (let ((*print-pretty* nil)
                       (*print-case* :downcase)
                       (*print-readably* nil))
                   (apply #'format nil "'(~@{#.~S~^ ~})" forms)))
         keys))

(defun invoke-lisp (&rest keys &key lisp-path image-path &allow-other-keys)
  (let* ((arg-list (apply (uiop:find-symbol* :lisp-invocation-arglist :lisp-invocation)
                          :implementation-type (uiop:implementation-type)
                          :lisp-path (or lisp-path (runtime-pathname))
                          keys))
         (impl (funcall (uiop:find-symbol* :get-lisp-implementation :lisp-invocation)
                        (uiop:implementation-type)))
         (image-executable-p (funcall (uiop:find-symbol* :lisp-implementation-image-executable-p
                                                         :lisp-invocation)
                                      impl))
         (image-flag (funcall (uiop:find-symbol* :lisp-implementation-image-flag
                                                 :lisp-invocation)
                              impl)))
    ;;; lisp-invocation is nice, but it has too many guardrails to prevent
    ;;; mixing and matching runtimes and images...
    (when (and lisp-path image-path image-executable-p)
      (push image-flag arg-list)
      (push (uiop:native-namestring lisp-path) arg-list))

    (format t "; ~{~S~^ ~}~%" arg-list)

    (uiop:run-program arg-list
                      :output :interactive :error-output :interactive)))
