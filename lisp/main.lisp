(defpackage :led/main
  (:use :common-lisp)
  (:import-from :led/proto)
  (:import-from :protobufs)
  (:import-from :cl-cli)
  (:import-from :swank)
  (:export #:main))
(in-package :led/main)

(defvar *help* nil)
(defvar *swank* nil)
(defvar *control-file* "/dev/null")
(defvar *die-on-error* t)

(defparameter *options*
  `((*help* ,*help* "Show help" :long "--help")
    (*swank* ,*swank* "Get swanky" :long "--swank")
    (*control-file* ,*control-file* "Where to issue commands" :long "--control-file" :params ("PATH"))))

(defvar *control-stream* (make-broadcast-stream))

(defun repl ()
  (with-open-file (s *control-file* :direction :output :if-exists :append :element-type '(unsigned-byte 8))
    (setf *control-stream* s)
    (cond
      (*swank*
       (swank:create-server :port 4005)
       (let ((*die-on-error* nil))
         (loop (sleep 999999999))))
      (t
       (let ((*die-on-error* nil))
         (loop
            (format t "~A> " (package-name *package*))
            (finish-output)
            (let* ((form (read))
                   (values (multiple-value-list (eval form))))
              (loop :for value :in values :do
                 (format t "~W~%" value))
              (finish-output))))))))

(defun write-effect-config (e)
  (setf e (to-compound e))
  (let* ((bytes (protobufs:serialize-object-to-bytes e 'led/proto:compound-effect))
         (count (length bytes)))
    (labels
        ((count-byte (n)
           (logand 255 (ash count (- (* 8 n))))))
      (loop :for byte :from 3 :downto 0 :do
         (write-byte (count-byte byte) *control-stream*)))
    (write-sequence bytes *control-stream*)
    (finish-output *control-stream*))
  (protobufs:print-text-format e nil))

(defun rainbow (&rest args)
  (apply 'make-instance 'led/proto:rainbow-cylinder args))

(defgeneric to-compound (effect))
(defmethod to-compound ((e led/proto:rainbow-cylinder))
  (to-compound (make-instance 'led/proto:basic-effect :rainbow e)))
(defmethod to-compound ((e led/proto:basic-effect))
  (make-instance 'led/proto:compound-effect :basic e))
(defmethod to-compound ((e led/proto:sum))
  (make-instance 'led/proto:compound-effect :sum e))
(defmethod to-compound ((e led/proto:pipeline))
  (make-instance 'led/proto:compound-effect :pipeline e))
(defmethod to-compound ((e led/proto:compound-effect))
  e)

(defun main ()
  (handler-bind
      ((error (lambda (e)
                (when *die-on-error*
                  (uiop:print-condition-backtrace e)
                  (uiop:quit 1)))))
    (multiple-value-bind (vars vals) (cl-cli:parse-cli (uiop:raw-command-line-arguments) *options*)
      (cl-cli:with-environment vars vals
        (when *help*
          (cl-cli:help *options* nil :prog-name (uiop:argv0)))
        (repl)))))
