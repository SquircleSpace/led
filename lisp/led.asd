(defsystem "led"
  :class :package-inferred-system
  :description "LED stuff.  Let there be light!"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :depends-on ("cl-protobufs" "led/effect" "led/main" "led/proto" "swank")
  :output-files (monolithic-lib-op (o c) (values (list (merge-pathnames #P"liblispLogic.a" *load-truename*)) t))
  :output-files (program-op (o c) (values (list (make-pathname :name "control" :type nil :defaults *load-truename*)) t))
  :entry-point "led/main:main")

(register-system-packages "cl-protobufs" '(:protobufs))
