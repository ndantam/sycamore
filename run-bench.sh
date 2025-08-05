#!/bin/sh

sbcl --dynamic-space-size 8192 \
     --eval "(require :sycamore)" \
     --load "./bench/bench.lisp" \
     --eval "(sycamore::run-bench \"$PWD/bench-data\")" \
     --quit
