#!/bin/sh

sbcl --script <<EOF

(load "share/load-quicklisp.lisp")
(push (make-pathname :directory '(:relative "src"))
      asdf:*central-registry*)

(ql:quickload :sycamore)
(ql:quickload :lisp-unit)


(load "share/test.lisp")
(in-package :sycamore)
(lisp-unit:run-tests)


#+sbcl
(sb-ext:quit)
EOF
