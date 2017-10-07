(let ((*standard-output* (make-broadcast-stream)))
  (require :asdf))

(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :cl-protobufs))

(let ((schema (protobufs:parse-schema-from-stream *standard-input*)))
  (protobufs:write-schema schema :type :lisp))
