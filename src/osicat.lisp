;;;; Integration with osicat
;;;;
;;;; This software is part of asdf-release-ops. See README.org for more
;;;; information. See LICENSE for license information.

(in-package #:asdf-release-ops)

#-os-windows
(defun copy-file (in out)
  (let ((kind (osicat:file-kind in)))
    (ecase kind
      (:regular-file
       (uiop:copy-file in out)
       (setf (osicat:file-permissions out) (osicat:file-permissions in)))
      (:symbolic-link
       (osicat:make-link out :target (osicat:read-link in))))))

#+os-windows
(defun copy-file (in out)
  (uiop:copy-file in out))
