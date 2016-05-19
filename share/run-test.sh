#!/bin/sh

if [ -z "$TOP_SRCDIR" ]; then
    TOP_SRCDIR=`pwd`
else
    TOP_SRCDIR=`realpath "$TOP_SRCDIR"`
fi

sbcl --script <<EOF

(load "$TOP_SRCDIR/share/load-quicklisp.lisp")
(push "$TOP_SRCDIR/src/" asdf:*central-registry*)

(ql:quickload :sycamore)
(ql:quickload :lisp-unit)

(load "$TOP_SRCDIR/share/test.lisp")
(in-package :sycamore)
(lisp-unit:run-tests)


#+sbcl
(sb-ext:quit)
EOF
