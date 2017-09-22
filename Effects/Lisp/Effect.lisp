(defpackage :led/effect
  (:use :common-lisp)
  (:export #:make-effect #:begin-frame #:shader #:end-frame))
(in-package :led/effect)

(defstruct effect)

(defun begin-frame ())

(defun shader (pixel)
  (setf (aref pixel 0) 1.0)
  pixel)

(defun end-frame ())
