#!/usr/bin/env bash

for PROTO in *.proto; do
    echo ": $PROTO |> !proto_cxx |> %B.pb.h %B.pb.cc <protostuff>"
    STEM="${PROTO%.*}"
    echo ": ${STEM}.pb.cc | ${STEM}.pb.h |> !cxx |> {effect_objects}"
    echo ": $PROTO |> ^ GEN LISP %f^ { echo 'option (lisp_package) = \"LED/PROTO\";' ; cat %f ; } > %o |> ../lisp/$PROTO"
    echo ": ../lisp/$PROTO |> ^ PROTOC-LISP %f^ sbcl --script ../lisp/protoc.lisp > %o < %f |> ../lisp/%B.lisp {lispproto}"
done
echo ": {lispproto} |> ^ COMBINE PROTO %f^ { echo '(defpackage :led/proto (:import-from :protobufs))' ; cat %f ; } > %o |> ../lisp/proto.lisp ../lisp/<protofiles>"
