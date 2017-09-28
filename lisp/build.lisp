(require :asdf)

(push (make-pathname :name nil :type nil :defaults *load-truename*)
      asdf:*central-registry*)

(asdf:oos 'asdf:monolithic-lib-op :led)
(uiop:quit)
