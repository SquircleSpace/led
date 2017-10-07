(require :asdf)

(push (make-pathname :name nil :type nil :defaults *load-truename*)
      asdf:*central-registry*)

(asdf:oos 'asdf:program-op :led)
(uiop:quit)
