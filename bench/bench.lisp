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


(in-package :sycamore)


(defparameter *bench-data-file-1*
  (make-pathname :directory '(:absolute "tmp") :name "sycamore-bench-1" :type "dat"))

(defparameter *bench-data-file-2*
  (make-pathname :directory '(:absolute "tmp") :name "sycamore-bench-2" :type "dat"))

(defun bench-generate-data (&key
                            (output-1 *bench-data-file-1*)
                            (output-2 *bench-data-file-2*)
                            (count-1 (expt 2 18))
                            (max-1 (* 2 count-1))
                            (count-2 count-1)
                            (max-2 max-1))
  (flet ((emit (count max output)
           (with-open-file (s output :direction :output :if-exists :supersede :if-does-not-exist :create)
             (format s "~{~&~D~}"
                     (loop for i below count
                        collect (random max))))))
    (emit count-1 max-1 output-1)
    (emit count-2 max-2 output-2)))

(defun bench-load (pathname)
  (with-open-file (s pathname :direction :input)
    (loop for i = (read s nil nil)
       while i
       collect i)))

(defun time-general (build
                     &key
                       (list-1 (bench-load *bench-data-file-1*))
                       (list-2 (bench-load *bench-data-file-2*))
                       insert remove union intersection difference (output *standard-output*) name)
  (let ((*standard-output* output)
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

(defun time-all (&key count (max count))
  (when (and count max)
    (bench-generate-data :count-1 count :max-1 max))
  (time-avl)
  (time-fset)
  nil)
