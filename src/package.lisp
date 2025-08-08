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

(defpackage :sycamore-util
  (:use :cl :alexandria)
  (:export
   unsigned-fixnum
   fixnum-compare
   double-compare
   string-compare
   bit-vector-compare
   fixnum-compare
   gsymbol-compare
   fold-n
   fold

   cond-compare
   if-less-eq-compare
   or-compare

   output-dot

   with-temp-array
   with-timing
   ))


(defpackage :sycamore
  (:use :cl :sycamore-util :alexandria)
  (:export
   ;; tree sets
   make-tree-set tree-set tree-set-p
   map-tree-set fold-tree-set do-tree-set
   empty-tree-set
   tree-set-list
   tree-set-count
   tree-set-ref tree-set-position
   tree-set-insert tree-set-remove
   tree-set-replace
   tree-set-insertf
   tree-set-remove-min tree-set-remove-max tree-set-remove-position
   tree-set-union tree-set-intersection tree-set-difference
   tree-set-member-p tree-set-find
   tree-set-equal-p tree-set-subset-p tree-set-intersection-p tree-set-compare
   tree-set-intersection-difference
   tree-set-intern
   tree-set-max tree-set-min
   ;; tree map
   tree-map
   make-tree-map tree-map-insert tree-map-remove tree-map-find
   empty-tree-map
   tree-map-contains
   tree-map-insertf
   map-tree-map tree-map-count
   fold-tree-map
   do-tree-map
   alist-tree-map hash-table-tree-map
   tree-map-alist tree-map-hash-table
   tree-map-keys tree-map-values
   tree-map-insert-map
   tree-map-insert-alist
   tree-map-insert-hash-table
   ;; queues
   make-amortized-queue amortized-queue amortized-queue-empty-p
   amortized-enqueue amortized-dequeue amortized-queue-push
   amortized-queue-list
   ;; Ropes
   *rope-print*
   %rope rope ropep
   subrope
   rope-length rope-ref
   rope-string rope-write
   rope-pathname
   rope-compare-lexographic rope-compare-fast
   sexp-rope
   object-rope
   rope-map
   rope-split
   rope-parenthesize
   output-rope
   :rope=
   :rope/=
   :rope<
   :rope<=
   :rope>
   :rope>=

   ;; hash-set
   :hash-set
   :make-hash-set
   :hash-set-find
   :hash-set-empty-p
   :hash-set-insert
   :hash-set-remove
   :hash-set-union
   :hash-set-intersection
   :hash-set-difference
   :hash-set-intersection-p
   :hash-set-subset-p
   :list-hash-set
   :hash-set-list
   :map-hash-set
   :fold-hash-set
   :fold-right-hash-set

   ;; hash-map
   :hash-map
   :make-hash-map
   :empty-hash-map
   :hash-map-insert
   :hash-map-remove
   :hash-map-find
   :hash-map-contains
   :map-hash-map
   :do-hash-map
   :fold-hash-map
   :hash-map-alist
   :alist-hash-map
   :hash-map-values
   :hash-map-keys
   :hash-map-hash-table
   :hash-table-hash-map

   ;; misc
   or-compare))

(defpackage :sycamore-cgen
  (:use :cl :sycamore-util :sycamore :alexandria)
  (:export
   :cgen-include-local
   :cgen-include-system
   :cgen-call
   :cgen-defun
   :cgen-exp
   :cgen-if
   :cgen-while
   :cgen-block
   :cgen-equal
   :cgen-assign
   :cgen-stmt
   :cgen-return
   :cgen-string
   :cgen-declare
   :cgen-declare-array
   :cgen-array-initializer
   :cgen-double-float
   :cgen-single-float
   :cgen-=
   :cgen-+
   :cgen--
   :cgen-*
   :cgen-/
   :cgen-&&
   :cgen-&
   :cgen-addr
   :cgen-deref
   :cgen-sizeof
   :cgen-subscript
   :cgen-identifier
   :cgen-bit-or
   :cgen-log-or
   :cgen-comment
   :cgen-line-comment
   ;; convenience
   :cgen-call-stmt
   :cgen-assign-stmt
   :cgen-declare-fun
   ))
