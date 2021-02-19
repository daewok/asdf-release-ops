;;;; Release Archive Op
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.
;;;;
;;;; This is in a spearate file because it has external dependencies

(in-package #:asdf-release-ops)

(defmethod asdf:perform ((o release-archive-op) (s asdf:system))
  (let* ((staging-directory (asdf:component-pathname (release-system-release-module o s)))
         (parent-directory (uiop:pathname-parent-directory-pathname staging-directory))
         (input-files (mapcar (lambda (p) (uiop:enough-pathname p parent-directory))
                              (asdf:input-files o s)))
         (*default-pathname-defaults* parent-directory)
         (output-pn (asdf:output-file o s)))
    (uiop:with-staging-pathname (output-pn)
      (ecase (release-archive-type o s)
        (:zip
         (org.shirakumo.zippy:compress-zip staging-directory output-pn
                                           :if-exists :supersede))
        (:tar.gz
         ;; Ugh. Does there really not exist a compressing stream interface to
         ;; salza2 like chipz has??
         (uiop:with-temporary-file (:stream stream
                                    :pathname tar-pn
                                    :directory parent-directory
                                    :type "tar"
                                    :direction :output
                                    :element-type '(unsigned-byte 8))
           (archive::create-tar-file stream input-files)
           :close
           (salza2:gzip-file tar-pn output-pn :if-exists :supersede)))))))
