;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2012, Georgia Tech Research Corporation
;;;; All rights reserved.
;;;;
;;;; Author(s): Neil T. Dantam <ntd@gatech.edu>
;;;; Georgia Tech Humanoid Robotics Lab
;;;; Under Direction of Prof. Mike Stilman
;;;;
;;;; This file is provided under the following "BSD-style" License:
;;;;
;;;;   Redistribution and use in source and binary forms, with or
;;;;   without modification, are permitted provided that the following
;;;;   conditions are met:
;;;;   * Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;;   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;;   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;;   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;;;   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;;;   EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.




(asdf:defsystem sycamore
  :version "0.0.20120604"
  :description "Fast, purely functional data structures"
  :depends-on (:cl-ppcre :alexandria)
  :license :bsd-3
  :homepage "http://ndantam.github.io/sycamore"
  :source-control "https://github.com/ndantam/sycamore"
  :author "Neil T. Dantam"
  :weakly-depends-on (:lisp-unit :cl-fuzz)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "heap" :depends-on ("util"))
               (:file "queue" :depends-on ("util"))
               (:file "array" :depends-on ("util"))
               (:file "binary" :depends-on ("util" "array"))
               (:file "wb-tree" :depends-on ("binary" "array"))

               (:module bits
                :depends-on ("package")
                :components
                ((:file "bits-generic")
                 #+(and sbcl x86-64)
                 (:file "bits-sbcl-x86-64")))

               (:file "hamt" :depends-on ("array" bits))
               ;;(:file "ttree" :depends-on ("avl"))
               (:file "interfaces" :depends-on ("wb-tree" "hamt"))
               (:file "rope" :depends-on ("util"))
               (:file "cgen" :depends-on ("rope")))
  :long-description
  "Sycamore is a fast, purely functional data structure library in
Common Lisp.  It include hash array mapped tries, weight-balanced
binary trees, set and map (dictionary) interfaces, pairing heaps, and
amortized queues."  )
