#!/usr/bin/env bash

for PROTO in *.proto; do
    echo ": $PROTO |> !proto_cxx |> %B.pb.h %B.pb.cc <protostuff>"
    STEM="${PROTO%.*}"
    echo ": ${STEM}.pb.cc | ${STEM}.pb.h |> !cxx |> {effect_objects}"
done
