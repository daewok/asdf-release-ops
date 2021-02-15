;;;; Components
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.

;; (uiop:define-package #:asdf-release-ops/components
;;     (:use #:cl)
;;   (:import-from #:asdf)
;;   (:export #:release-dependencies-license-file
;;            #:release-file
;;            #:release-license-file
;;            #:release-ops-system
;;            #:release-ops-system-release-directory
;;            #:release-ops-system-release-license-file
;;            #:release-ops-system-release-module
;;            #:release-ops-system-release-readme-file
;;            #:release-ops-system-release-staging-directory
;;            #:release-ops-package-inferred-system
;;            #:release-program-file
;;            #:release-readme-file))

(in-package #:asdf-release-ops)

(defclass release-system (asdf:system)
  ((release-structure-description
    :initarg :release-structure
    :accessor release-system-release-structure-description)
   (release-modules
    :reader release-system-release-modules
    :initform (make-hash-table :test 'equal))

   (release-license-file
    :initform nil
    :initarg :release-license-file
    :reader release-system-release-license-file)
   (release-readme-file
    :initform nil
    :initarg :release-readme-file
    :reader release-system-release-readme-file)

   (release-directory
    :initform nil
    :initarg :release-directory
    :reader release-system-release-directory)
   (release-staging-directory
    :initform nil
    :initarg :release-staging-directory
    :reader release-system-release-staging-directory))
  (:documentation
   "A system class for seamlessly using ASDF release ops."))
(defclass release-system-inferred-system (release-system asdf:package-inferred-system)
  ()
  (:documentation
   "Combination of PACKAGE-INFERRED-SYSTEM and RELEASE-OPS-SYSTEM."))

(defmethod slot-unbound (class
                         (system release-system)
                         (slot-name (eql 'release-structure-description)))
  (setf (slot-value system slot-name)
        `((:module "bin"
           :components
           ((:program-file ,(asdf:primary-system-name system))))
          (:module "share"
           :components
           ( ;; (:man-directory "man")
            (:module "doc"
             :components
             ((:module ,(asdf:primary-system-name system)
               :append-version t
               :components ((:license-file "LICENSE")
                            (:readme-file "README")
                            (:dependencies-license-file "BUNDLED-LICENSES"))))))))))

(defgeneric release-system-release-module (o s)
  (:documentation
   "Return the RELEASE-MODULE corresponding to the given release op.")
  (:method (o (s release-system))
    (let* ((key (release-op-key o))
           (module (gethash key (release-system-release-modules s)))
           (description (release-system-release-structure-description s)))
    (when (or (null module)
              (not (equal (car module) description)))
      (setf module (cons description (make-release-module s key description)))
      (setf (gethash key (release-system-release-modules s)) module))
    (cdr module))))

(defclass release-module (asdf:module)
  ((append-version
    :initform nil
    :initarg :append-version
    :reader release-module-append-version)))

(defmethod asdf:component-relative-pathname ((mod release-module))
  (let ((base (or (and (slot-boundp mod 'asdf/component:relative-pathname)
                       (slot-value mod 'asdf/component:relative-pathname))
                  (asdf:component-name mod))))
    (uiop:parse-unix-namestring
     (if (release-module-append-version mod)
         (uiop:strcat base "-" (asdf:system-version (asdf:component-system mod)))
         base)
     :want-relative t
     :type :directory
     :defaults (asdf/component:component-parent-pathname mod))))

(defun os-arch-tuple ()
  "Return a string of the form $OS-$ARCH."
  (concatenate 'string
               (string-downcase (uiop:operating-system))
               "-"
               #+x86-64 "amd64"
               #+x86 "x86"
               #+arm "arm"
               #+arm64 "arm64"))

(defgeneric release-system-version-designator (s)
  (:method ((s asdf:system))
    (asdf:system-version s)))

(defgeneric release-system-staging-directory (key s)
  (:method (key (s release-system))
    (let ((base-dir (or (release-system-release-staging-directory s)
                        (uiop:strcat (asdf:primary-system-name s)
                                     "--asdf-release-ops-staging/"))))
      (merge-pathnames (uiop:parse-unix-namestring
                        (uiop:strcat key
                                     "/"
                                     (asdf:primary-system-name s)
                                     "-"
                                     (release-system-version-designator s)
                                     "-"
                                     (os-arch-tuple)
                                     "/"))
                       (uiop:ensure-directory-pathname
                        (uiop:parse-unix-namestring base-dir :type :directory))))))

(defgeneric release-op-release-directory (o s)
  (:method (o (s release-system))
    (let ((base-dir (or (release-system-release-directory s)
                        (uiop:strcat (asdf:primary-system-name s)
                                     "--asdf-release-ops-staging/"))))
      (merge-pathnames (uiop:parse-unix-namestring
                        (uiop:strcat (release-op-key o)
                                     "/"))
                       (uiop:ensure-directory-pathname
                        (uiop:parse-unix-namestring base-dir :type :directory))))))


;; * Release files

(defclass release-file (asdf:file-component)
  ((type
    :initarg :type
    :accessor asdf:file-type)))

;; (defgeneric release-file-staging-pathname (o c)
;;   (:documentation
;;    "Returns the staging pathname for the release file.")
;;   (:method (o (c release-file))
;;     ()
;;     ()
;;     ))

(defmethod asdf:perform ((o asdf:compile-op) (c release-file))
  nil)
(defmethod asdf:perform ((o asdf:load-op) (c release-file))
  nil)

;; * Program file

(defclass release-program-file (release-file)
  ((type
    :initform (asdf/bundle:bundle-pathname-type :program))))

;; * License files

(defclass release-license-file (release-file)
  ((type
    :initform nil)))

;; * Dependency license file

(defclass release-dependencies-license-file (release-file)
  ((type
    :initform nil)))

;; * README files

(defclass release-readme-file (release-file)
  ((type
    :initform nil)))

(defgeneric release-component-class-by-type (parent type))

(defmethod release-component-class-by-type (parent (type (eql :module)))
  'release-module)
(defmethod release-component-class-by-type (parent (type (eql :program-file)))
  'release-program-file)
(defmethod release-component-class-by-type (parent (type (eql :license-file)))
  'release-license-file)
(defmethod release-component-class-by-type (parent (type (eql :dependencies-license-file)))
  'release-dependencies-license-file)
(defmethod release-component-class-by-type (parent (type (eql :readme-file)))
  'release-readme-file)

(defgeneric make-release-module (system key description))

(defmethod make-release-module ((system release-system) key description)
  (parse-release-module-form system (list :module key
                                          :pathname (release-system-staging-directory key system)
                                          :components description)))

(defgeneric parse-release-module-form (parent form)
  (:method (parent form)
    (destructuring-bind (type name &rest initargs &key components &allow-other-keys)
        form
      (let ((class (release-component-class-by-type parent type)))
        (if (subtypep class 'asdf:parent-component)
            (let* ((out (apply #'make-instance class
                               :parent parent
                               :name name
                               (uiop:remove-plist-key :components initargs)))
                   (children (mapcar (lambda (c) (parse-release-module-form out c)) components)))
              (setf (asdf:component-children out) children)
              (asdf::compute-children-by-name out)
              out)
            (apply #'make-instance class :parent parent :name name initargs))))))


;; * Build operations

(defgeneric release-system-license-op (o s)
  (:method (o s)
    nil))

(defgeneric release-system-dependencies-license-op (o s)
  (:method (o s)
    nil))

(defgeneric release-system-readme-op (o s)
  (:method (o s)
    nil))

(defgeneric release-system-program-op (o s)
  (:method (o s)
    (matching-variant-of o 'program-op)))
