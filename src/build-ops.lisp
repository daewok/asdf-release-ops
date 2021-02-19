;;;; Build Ops
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.
;;;;
;;;; This file contains operations useful for building artifacts that are then
;;;; packaged or otherwise manipulated by release operations.

(in-package #:asdf-release-ops)

(defgeneric build-op-output-directory-pathname (o c))

(defmethod build-op-output-directory-pathname ((o static-build-abstract-op) (c asdf:system))
  (uiop:subpathname (asdf:component-pathname c)
                    (uiop:strcat (asdf:coerce-name c)
                                 "--asdf-release-ops-static/")
                    :type :directory))

(defmethod build-op-output-directory-pathname ((o dynamic-build-abstract-op) (c asdf:system))
  (uiop:subpathname (asdf:component-pathname c)
                    (uiop:strcat (asdf:coerce-name c)
                                 "--asdf-release-ops-dynamic/")
                    :type :directory))


(define-build-op program-system-list-op (asdf:selfward-operation)
  ()
  (:documentation
   "Create a file describing all systems loaded in the program. The file
contains a single list. Each element of the list describes a system. The first
element is the system name, the remainder is a plist containing :LICENSE and
:VERSION keys (more keys may be added in the future).

Depending on the Lisp implementation in use, this may or may not start a new
Lisp process to generate the list. Therefore, it is suggested that you do not
add dependencies or extra steps to this operation directly. Adding them to the
appropriate variant of PERFORM-PROGRAM-IMAGE-OP is likely what you want."))

(defmethod asdf:selfward-operation ((o program-system-list-op))
  (matching-variant-of o 'program-image-op))

(defmethod asdf:component-depends-on ((o program-system-list-op) (s asdf:system))
  (list* '(asdf:load-op "asdf-release-ops/lisp-invocation")
         (call-next-method)))

(defmethod asdf:output-files ((o program-system-list-op) (s asdf:system))
  (list (uiop:subpathname (build-op-output-directory-pathname o s)
                          "asdf-system-list"
                          :type "sexp")))

(defmethod asdf:perform ((o program-system-list-op) (s asdf:system))
  (let ((output-file (asdf:output-file o s)))
    (uiop:with-staging-pathname (output-file)
      (eval-in-lisp
       `((with-open-file (stream ,output-file
                                 :direction :output
                                 :if-exists :supersede)

           (prin1
            (loop
              :for asdf:system :in (sort (copy-list (asdf:already-loaded-systems))
                                         #'string<)
              :collect (list asdf:system
                             :license (asdf:system-licence (asdf:find-system asdf:system))
                             :version (asdf:system-version (asdf:find-system asdf:system))))
            stream)))
       :image-path (first (asdf:input-files o s))))))


(define-build-op program-foreign-library-list-op (asdf:selfward-operation)
  ()
  (:documentation
   "Produce a file containing a list of pathnames of libraries required by the
program. The file contains a single list. Each element of the list is a
pathname to a foreign library.

Depending on the Lisp implementation in use, this may or may not start a new
Lisp process to generate the list. Therefore, it is suggested that you do not
add dependencies or extra steps to this operation directly. Adding them to the
appropriate variant of PERFORM-PROGRAM-IMAGE-OP is likely what you want."))

(defmethod asdf:selfward-operation ((o program-foreign-library-list-op))
  (matching-variant-of o 'program-image-op))

(defmethod asdf:component-depends-on ((o program-foreign-library-list-op) (s asdf:system))
  (list* '(asdf:load-op "asdf-release-ops/lisp-invocation")
         (call-next-method)))

(defmethod asdf:output-files ((o program-foreign-library-list-op) (s asdf:system))
  (list (uiop:subpathname (build-op-output-directory-pathname o s)
                          "foreign-library-list"
                          :type "sexp")))

(defmethod asdf:perform ((o program-foreign-library-list-op) (s asdf:system))
  (let ((output-file (asdf:output-file o s)))
    (uiop:with-staging-pathname (output-file)
      (eval-in-lisp
       `((with-open-file (stream ,output-file
                                 :direction :output
                                 :if-exists :supersede)
           (prin1 (mapcar #'sb-alien::shared-object-pathname sb-alien::*shared-objects*)
                  stream)))
       :image-path (first (asdf:input-files o s))))))


(define-build-op program-linkage-info-op (asdf:selfward-operation)
  ()
  (:documentation
   "Produces a file containing the linkage info for the program. This file
contains a single plist. The key :LINKAGE-TABLE is an alist mapping linkage
info keys to indices. The key :UNDEFINED is a list of undefined entries. See
SB-SYS:*LINKAGE-INFO* for more details.

Depending on the Lisp implementation in use, this may or may not start a new
Lisp process to generate the list. Therefore, it is suggested that you do not
add dependencies or extra steps to this operation directly. Adding them to the
appropriate variant of PERFORM-PROGRAM-IMAGE-OP is likely what you want."))

(defmethod asdf:selfward-operation ((o program-linkage-info-op))
  (matching-variant-of o 'program-image-op))

(defmethod asdf:component-depends-on ((o program-linkage-info-op) (s asdf:system))
  (list* '(asdf:load-op "asdf-release-ops/lisp-invocation")
         (call-next-method)))

(defmethod asdf:output-files ((o program-linkage-info-op) (s asdf:system))
  (list (uiop:subpathname (build-op-output-directory-pathname o s)
                          "linkage-info"
                          :type "sexp")))

(defmethod asdf:perform ((o program-linkage-info-op) (s asdf:system))
  (let ((output-file (asdf:output-file o s)))
    (uiop:with-staging-pathname (output-file)
      (eval-in-lisp
       `((with-open-file (stream ,output-file
                                 :direction :output
                                 :if-exists :supersede)
           (prin1
            (list
             :linkage-table
             (loop
               :for key :being :the :hash-keys :in (car sb-sys:*linkage-info*) :using (hash-value idx)
               :collect (cons key idx))
             :undefined
             (cdr sb-sys:*linkage-info*))
            stream)))
       :image-path (first (asdf:input-files o s))))))


(define-build-op program-linkage-table-prelink-info-c-op (asdf:selfward-operation)
  ()
  (:documentation
   "Produce a prelink info C file."))

(defmethod asdf:selfward-operation ((o program-linkage-table-prelink-info-c-op))
  (matching-variant-of o 'program-linkage-info-op))

(defmethod asdf:output-files ((o program-linkage-table-prelink-info-c-op) (s asdf:system))
  (list (uiop:subpathname (build-op-output-directory-pathname o s)
                          "linkage-table-prelink-info"
                          :type "c")))

(defmethod asdf:perform ((o program-linkage-table-prelink-info-c-op) (s asdf:system))
  (let ((output-file (asdf:output-file o s)))
    (destructuring-bind (&key linkage-table undefined)
        (uiop:read-file-form (first (asdf:input-files o s)))
      (setf linkage-table (sort linkage-table #'< :key #'cdr))

      (uiop:with-staging-pathname (output-file)
        (with-open-file (stream output-file
                                :direction :output
                                :if-exists :supersede)
          ;; Needed for uintptr_t. We use the raw uintptr_t as we don't want to have
          ;; to include any SBCL headers just to get at lispobj.
          (format stream "#include <stdint.h>~%~%")

          ;; Write out the extern definitions. Everything is a void function
          ;; (even variables) because compilers don't like void
          ;; variables. Remove lisp_linkage_values as we need to write to it,
          ;; so we should use the actual type.
          (format stream "extern void ~{~A()~^, ~};~%~%"
                  (remove "lisp_linkage_values"
                          (mapcar (lambda (key)
                                    (if (listp key)
                                        (car key)
                                        key))
                                  (remove-if (lambda (key)
                                               (member key undefined :test #'equal))
                                             (mapcar #'car linkage-table)))
                          :test #'equal))

          (format stream "uintptr_t lisp_linkage_values[] = {~%")

          (format stream "  ~D,~%" (length linkage-table))

          (dolist (key (mapcar #'car linkage-table))
            (let* ((datap (listp key))
                   (name (if datap (car key) key)))
              (when datap
                ;; This is data, put -1 in to indicate that.
                (format stream "  (uintptr_t)-1,~%"))
              (if (member key undefined :test #'equal)
                  (format stream "  (uintptr_t)0,~%")
                  (format stream "  (uintptr_t)&~A,~%" name))))
          (format stream "};~%"))))))


(define-build-op program-linkage-table-prelink-info-o-op (asdf:selfward-operation)
  ()
  (:documentation
   "Produce an object file containing the linkage table prelink info."))

(defmethod asdf:selfward-operation ((o program-linkage-table-prelink-info-o-op))
  (matching-variant-of o 'program-linkage-table-prelink-info-c-op))

(defmethod asdf:component-depends-on ((o program-linkage-table-prelink-info-o-op) (s asdf:system))
  (list* '(asdf:load-op "asdf-release-ops/cffi-toolchain")
         (call-next-method)))

(defmethod asdf:output-files ((o program-linkage-table-prelink-info-o-op) (s asdf:system))
  (list (uiop:subpathname (build-op-output-directory-pathname o s)
                          "linkage-table-prelink-info"
                          :type "o")))

(defmethod asdf:perform ((o program-linkage-table-prelink-info-o-op) (s asdf:system))
  (let ((input-files (asdf:input-files o s))
        (output-file (asdf:output-file o s)))
    (funcall (uiop:find-symbol* :cc-compile :cffi-toolchain)
             output-file input-files (list "-Wno-builtin-declaration-mismatch"))))


(define-build-op program-image-op (asdf:non-propagating-operation
                                   asdf:bundle-op)
  ()
  (:documentation
   "Create an image with the program loaded and ready to be run.

This operation does not actually generate the image as part of its
PERFORM. Instead the appropriate variant of PERFORM-PROGRAM-IMAGE-OP, which may
or may not be PERFORM'ed in a separate process. Therefore it is suggested that
you do not add dependencies or extra steps to this operation directly."))

(defmethod asdf::bundle-type ((o program-image-op))
  :image)

(defmethod asdf:component-depends-on ((o program-image-op) (s asdf:system))
  (list* '(asdf:load-op "asdf-release-ops/lisp-invocation")
         (call-next-method)))

(defmethod asdf:output-files ((o program-image-op) (s asdf:system))
  (list (uiop:subpathname (build-op-output-directory-pathname o s)
                          (asdf:coerce-name s)
                          :type "core")))

;; TODO: Make this ignore "internal" files (files that are produced by an ASDF
;; action)
(defmethod asdf:input-files ((o program-image-op) (s asdf:system))
  (let ((all-actions
          (asdf/plan::collect-dependencies
           (asdf:make-operation (matching-variant-of o 'perform-program-image-op))
           s
           :other-systems t
           :component-type t
           :keep-component t
           :keep-operation t)))
    (remove-duplicates
     (loop
       :for (dop . dc) :in all-actions
       :appending (asdf:input-files dop dc))
     :test #'equal)))

(defgeneric program-image-features (o s)
  (:documentation
   "Return a list of features that should be pushed before starting to build
the image.")
  (:method (o s)
    nil))

(defmethod asdf:perform ((o program-image-op) (s asdf:system))
  (let ((name (asdf:coerce-name s))
        (core-output (asdf:output-file o s))
        (features (program-image-features o s)))
    (uiop:with-staging-pathname (core-output)
      (eval-in-lisp
       (list
        `(dolist (variable ',features)
          (pushnew variable *features*))
        '(require "asdf")
        '(in-package :asdf)
        ;; Configure ASDF!
        `(progn
           (setf asdf:*central-registry* ',asdf:*central-registry*)
           (asdf:initialize-source-registry ',asdf:*source-registry-parameter*)
           (asdf:initialize-output-translations ',asdf:*output-translations-parameter*)
           (asdf:upgrade-asdf)
           ,@(uiop:if-let ((ql-home
                            (symbol-value (uiop:find-symbol* '*quicklisp-home* 'ql-setup nil))))
               `((load ,(uiop:subpathname ql-home "setup.lisp")))))
        ;; Load asdf-release-ops
        '(asdf:load-system "asdf-release-ops")
        ;; Override methods
        `(progn
           (defmethod asdf:operation-done-p ((asdf:operation perform-program-image-op)
                                             (asdf:system (eql (asdf:find-system ,name))))
             nil)
           (defmethod asdf:output-files ((asdf:operation perform-program-image-op)
                                         (asdf:system (eql (asdf:find-system ,name))))
             (values (list ,core-output) t)))
        ;; PERFORM!
        `(asdf:operate ',(matching-variant-of o 'perform-program-image-op) ,name)
        ;; Quit if needed
        `(uiop:quit))))))


(define-build-op perform-program-image-op (asdf:image-op)
  ()
  (:documentation
   "Actually performs the operation described by PROGRAM-IMAGE-OP."))

(defmethod asdf:component-depends-on ((o perform-program-image-op) (s asdf:system))
  (list* '(asdf:load-op "asdf-release-ops")
         (call-next-method)))

(defmethod asdf:perform :before ((o perform-program-image-op) (s asdf:system))
  "Freeze the loaded ASDF systems."
  (dolist (component (asdf:already-loaded-systems))
    (asdf:register-preloaded-system component)))


;; TODO: Add link-op?
(define-build-op program-runtime-op (asdf:selfward-operation)
  ()
  (:documentation
   "Create a runtime for the program."))

(defmethod asdf:selfward-operation ((o static-program-runtime-op))
  (list 'asdf:load-op
        (matching-variant-of o 'program-linkage-table-prelink-info-o-op)
        (matching-variant-of o 'program-foreign-library-list-op)
        'asdf:monolithic-lib-op))

(defmethod asdf:selfward-operation ((o dynamic-program-runtime-op))
  (list 'asdf:load-op
        'asdf:monolithic-lib-op))

(defmethod asdf:component-depends-on ((o program-runtime-op) (s asdf:system))
  (list* '(asdf:load-op "asdf-release-ops/cffi-toolchain")
         (call-next-method)))

(defmethod asdf:output-files ((o program-runtime-op) (s asdf:system))
  (list (uiop:subpathname (build-op-output-directory-pathname o s)
                          "static-runtime"
                          :type (asdf::bundle-pathname-type :program))))

(defmethod runtime-linker-options ((o static-program-runtime-op) (s asdf:system))
  (let* ((library-list-file (asdf:output-file 'static-program-foreign-library-list-op s))
         (input-files (internal-library-pathnames o s))
         (library-list (set-difference
                        (uiop:read-file-form library-list-file)
                        input-files
                        :test #'equal)))
    (list*
     "-no-pie"
     "-static"
     (loop :for pn :in library-list
           :append (library-pathname-to-linker-option o s pn)))))

(defmethod library-pathname-to-linker-option ((o static-program-runtime-op) s pn)
  (let ((pkg-config-name (library-pkg-config-name o s pn)))
    (multiple-value-bind (out err-out code)
        (uiop:run-program `("pkg-config" "--static" "--libs" ,pkg-config-name)
                          :ignore-error-status t
                          :output '(:string :stripped t)
                          :error-output :interactive)
      (declare (ignore err-out))
      (if (zerop code)
          (remove "" (uiop:split-string out) :test #'equal)
          ;; Just hope for the best...
          (list (uiop:strcat "-l:" pkg-config-name ".a"))))))

(defmethod runtime-linker-options ((o dynamic-program-runtime-op) (s asdf:system))
  (let* ((library-list-file (asdf:output-file 'static-program-foreign-library-list-op s))
         (input-files (internal-library-pathnames o s))
         (library-list (set-difference
                        (uiop:read-file-form library-list-file)
                        input-files
                        :test #'equal)))
    (loop :for pn :in library-list
          :append (library-pathname-to-linker-option o s pn))))

(defmethod library-pathname-to-linker-option ((o dynamic-program-runtime-op) s pn)
  (let ((pkg-config-name (library-pkg-config-name o s pn)))
    (multiple-value-bind (out err-out code)
        ;; This will probably result in overlinking, but I don't know how to
        ;; fix that, other than specific rules for symbols/libs. For instance,
        ;; cl+ssl on Linux explicitly loads only libssl. But the dynamic linker
        ;; brings in libcrypto implicitly *and* symbols from libcrypto end up
        ;; in the linkage table because cl+ssl uses them. However, libcrypto
        ;; doesn't show up on our library list since it was only implicitly
        ;; loaded.
        (uiop:run-program `("pkg-config" "--static" "--libs" ,pkg-config-name)
                          :ignore-error-status t
                          :output '(:string :stripped t)
                          :error-output :interactive)
      (declare (ignore err-out))
      (if (zerop code)
          (remove "" (uiop:split-string out) :test #'equal)
          ;; Just hope for the best...
          ;;
          ;; TODO: Add another GF to compute link flag for a single library?
          (list (uiop:strcat "-l" (subseq pkg-config-name 3)))))))

(defmethod asdf:perform ((o static-program-runtime-op) (s asdf:system))
  (funcall (uiop:find-symbol* :link-lisp-executable :cffi-toolchain)
           (asdf:output-file o s)
           (append
            (asdf:output-files (matching-variant-of o 'program-linkage-table-prelink-info-o-op) s)
            (asdf:output-files 'asdf:monolithic-lib-op s)
            (runtime-linker-options o s))))

(defmethod asdf:perform ((o dynamic-program-runtime-op) (s asdf:system))
  (let ((input-files (asdf:output-files 'asdf:monolithic-lib-op s)))
    (if (uiop:os-windows-p)
        ;; CFFI has a long standing issue where flags such as -Wl,mswin64.def
        ;; do not get normalized. Try to make this work even in the face of
        ;; that bug by changing the current working directory.
        (uiop:with-current-directory ((uiop:lisp-implementation-directory))
          #1=(funcall (uiop:find-symbol* :link-lisp-executable :cffi-toolchain)
                      (asdf:output-file o s)
                      (when input-files
                        (funcall (uiop:find-symbol* :link-all-library :cffi-toolchain)
                                 (first input-files)))))
        #1#)))


(define-build-op program-static-image-op (asdf:selfward-operation)
  ()
  (:documentation
   "Takes an image produced by PROGRAM-IMAGE-OP and redump it, configured to
*not* try and load foreign libs that have been statically linked into the
runtime."))

(defmethod asdf:selfward-operation ((o program-static-image-op))
  (matching-variant-of o 'program-image-op))

(defmethod asdf:component-depends-on ((o program-static-image-op) (s asdf:system))
  (list* '(asdf:load-op "asdf-release-ops/lisp-invocation")
         (call-next-method)))

(defmethod asdf:output-files ((o program-static-image-op) (s asdf:system))
  (list (uiop:subpathname (build-op-output-directory-pathname o s)
                          (asdf:coerce-name s)
                          :type "static-core")))

(defmethod asdf:perform ((o static-program-static-image-op) (s asdf:system))
  (let ((name (asdf:coerce-name s))
        (core-output (asdf:output-file o s)))
    (uiop:with-staging-pathname (core-output)
      (eval-in-lisp
       (list
        '(dolist (lib sb-alien::*shared-objects*)
          (setf (sb-alien::shared-object-dont-save lib) t))
        ;; Override methods
        `(progn
           (defmethod asdf:operation-done-p ((asdf:operation asdf:image-op)
                                             (asdf:system (eql (asdf:find-system ,name))))
             nil)
           (defmethod asdf:output-files ((asdf:operation asdf:image-op)
                                         (asdf:system (eql (asdf:find-system ,name))))
             (values (list ,core-output) t)))
        ;; PERFORM!
        `(asdf:operate 'asdf:image-op ,name))
       :image-path (asdf:output-file (matching-variant-of o 'program-image-op) s)))))

(defmethod asdf:perform ((o dynamic-program-static-image-op) (s asdf:system))
  (let ((name (asdf:coerce-name s))
        (core-output (asdf:output-file o s))
        (internal-libraries (internal-library-pathnames o s)))
    (uiop:with-staging-pathname (core-output)
      (eval-in-lisp
       (list
        `(dolist (lib sb-alien::*shared-objects*)
          (when (member (sb-alien::shared-object-pathname lib) ',internal-libraries
                        :test #'equal)
            (setf (sb-alien::shared-object-dont-save lib) t)))
        ;; Override methods
        `(progn
           (defmethod asdf:operation-done-p ((asdf:operation asdf:image-op)
                                             (asdf:system (eql (asdf:find-system ,name))))
             nil)
           (defmethod asdf:output-files ((asdf:operation asdf:image-op)
                                         (asdf:system (eql (asdf:find-system ,name))))
             (values (list ,core-output) t)))
        ;; PERFORM!
        `(asdf:operate 'asdf:image-op ,name))
       :image-path (asdf:output-file (matching-variant-of o 'program-image-op) s)))))


(define-build-op program-op (asdf:selfward-operation)
  ()
  (:documentation
   "Create a progam."))

(defmethod asdf:selfward-operation ((o program-op))
  (list (matching-variant-of o 'program-static-image-op)
        (matching-variant-of o 'program-runtime-op)))

(defmethod asdf:component-depends-on ((o program-op) (s asdf:system))
  (list* '(asdf:load-op "asdf-release-ops/lisp-invocation")
         (call-next-method)))

(defmethod asdf:output-files ((o program-op) (s asdf:system))
  (let* ((build-op-p (eq (class-of o) (asdf::coerce-class (asdf::component-build-operation s)
                                                          :package :asdf/interface
                                                          :super 'asdf:operation
                                                          :error nil)))
         (build-pathname (when build-op-p (asdf::component-build-pathname s))))
    (values
     (list (or build-pathname
               (uiop:subpathname (build-op-output-directory-pathname o s)
                                 (asdf:coerce-name s)
                                 :type (asdf::bundle-pathname-type :program))))
     build-op-p)))

(defmethod asdf:perform ((o program-op) (s asdf:system))
  (let ((output-file (asdf:output-file o s))
        (system (asdf:coerce-name s)))
    (uiop:with-staging-pathname (output-file)
      (eval-in-lisp
       `((progn
           (defmethod asdf:output-files ((o asdf:program-op)
                                         (asdf:system (eql (asdf:find-system ,system))))
             (values (list ,output-file) t))
           (defmethod asdf:operation-done-p ((o asdf:program-op)
                                             (asdf:system (eql (asdf:find-system ,system))))
             nil))
         (asdf:operate 'asdf:program-op ,(asdf:coerce-name s)))
       :lisp-path (asdf:output-file (matching-variant-of o 'program-runtime-op) s)
       :image-path (asdf:output-file (matching-variant-of o 'program-static-image-op) s)))))
