;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2014, Georgia Tech Research Corporation
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

(in-package :sycamore)

(defvar *time-list-1*)
(defvar *time-list-2*)

(defun time-general (build
                     &key insert remove union intersection difference (output *standard-output*) name)
  (let ((*standard-output* output)
        (list-1 *time-list-1*)
        (list-2 *time-list-2*)
        (obj-1)
        (obj-2))
    (labels ((pre-test (test-name)
               (format output "~&~%: ~A: ~A  :" name test-name)
               #+sbcl
               (sb-ext:gc)))
    (if name
        (format t "~&: Benchmarks Results for ~A :" name)
        (format t "~&: Benchmarks Results :" ))
    ;; build
    (pre-test "build object 1")
    (setq obj-1 (time (funcall build list-1)))

    (pre-test "build object 2")
    (setq obj-2 (time (funcall build list-2)))

    ;; insert
    (when insert
      (pre-test "insert 2 into 1")
      (time (loop for x in list-2
               for y =  (funcall insert obj-1 x) then
                 (funcall insert y x)))

      (pre-test "insert 1 into 2")
      (time (loop for x in list-1
               for y =  (funcall insert obj-2 x) then
                 (funcall insert y x))))
    ;; remove
    (when insert
      (format output "~&~%: ~A: Remove 2 from 1 :" name)
      (pre-test "remove 2 from 1")
      (time (loop for x in list-2
               for y =  (funcall remove obj-1 x) then
                 (funcall insert y x)))

      (pre-test "remove 1 from 2")
      (time (loop for x in list-1
               for y =  (funcall remove obj-2 x) then
                 (funcall insert y x))))

    ;; union

    (when union
      (format output "~&~%: ~A: (union 1 2):" name)
      (pre-test "union 1 2")
      (time (funcall union obj-1 obj-2))
      (pre-test "union 2 1")
      (time (funcall union obj-2 obj-1)))

    ;; intersection
    (when intersection
      (pre-test "intersection 1 2")
      (time (funcall intersection obj-1 obj-2))
      (pre-test "intersection 2 1")
      (time (funcall intersection obj-2 obj-1)))

    ;; difference
    (when difference
      (pre-test "difference 1 2")
      (time (funcall difference obj-1 obj-2))
      (pre-test "difference 2 1")
      (time (funcall difference obj-2 obj-1)))
    ))
  nil)


(defun time-make-lists (count max)
  ;; TODO: return arrays to pass to C benchmarks
  (flet ((make-it ()
           (loop for i below count collect (random max))))
    (setq *time-list-1* (make-it))
    (setq *time-list-2* (make-it)))
  nil)


(defun time-avl ()
  ;; build
  (let ((compare (lambda (a b)
                   (declare (type fixnum a b))
                         (- a b))))
    (time-general (lambda (a) (build-avl-tree compare
                                              nil a))
                :insert (lambda (obj x)
                          (avl-tree-insert obj x compare))
                :remove (lambda (obj x)
                          (avl-tree-remove obj x compare))
                :union (lambda (x y)
                         (avl-tree-union x y compare))
                :intersection (lambda (x y)
                                (avl-tree-intersection x y compare))
                :difference (lambda (x y)
                              (avl-tree-difference x y compare))
                :name "SYCAMORE:AVL")))


(defun time-fset ()
  (time-general (lambda (a) (fold #'fset:with (fset:empty-set) a))
                :insert (lambda (obj x)
                          (fset:with obj x))
                :remove (lambda (obj x)
                          (fset:less obj x))
                :union #'fset:union
                :intersection #'fset:intersection
                :difference #'fset:set-difference-2
                :name "FSET"))

(defun time-all (&optional count max)
  (when (and count max)
    (time-make-lists count max))
  (time-avl)
  (time-fset)
  nil)
