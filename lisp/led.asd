(defsystem "led"
  :class :package-inferred-system
  :description "LED stuff.  Let there be light!"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :depends-on ("led/effect" "led/main")
  :output-files (monolithic-lib-op (o c) (values (list (merge-pathnames #P"liblispLogic.a" *load-truename*)) t)))
