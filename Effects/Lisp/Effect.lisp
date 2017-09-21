(defpackage :led/effect
  (:use :common-lisp)
  (:export #:make-effect))
(in-package :led/effect)

(defstruct effect
  )

(defun begin-frame ()
  (format t "lisp begin-frame~%"))

(defun shader ()
  (format t "lisp shader~%"))

(defun end-frame ()
  (format t "lisp end-frame~%"))
