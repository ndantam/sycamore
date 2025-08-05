#!/bin/sh

sbcl --eval "(require :sycamore)" \
     --load "./bench/bench.lisp" \
     --eval "(sycamore::bench-file \"bench.md\" \"$PWD/bench-data/\")" \
     --quit
