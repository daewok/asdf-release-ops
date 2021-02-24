(uiop:define-package #:asdf-release-ops-test/lisp-curl
    (:use #:cl
          #:drakma))

(in-package #:asdf-release-ops-test/lisp-curl)

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (format t "~A~%" (drakma:http-request (first args)))))
