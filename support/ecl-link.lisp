(defpackage :led/support/ecl-link
  (:use :common-lisp))
(in-package :led/support/ecl-link)

(require :cmp)

(c:build-static-library
 (subseq (si:argv 3) 3)
 :lisp-files (loop :for i :from 4 :below (si:argc) :collect (si:argv i)))
