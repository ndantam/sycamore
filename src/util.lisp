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

(deftype positive-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(defun fixnum-compare (a b)
  (declare (type fixnum a b))
  (cond ((< a b) -1)
        ((> a b) 1)
        (t 0)))

(defun fold-n (function initial-value sequences)
  "Fold `FUNCTION' over each sequence in `SEQUENCES'."
  (let ((value initial-value))
    (flet ((fun2 (&rest args)
             (declare (dynamic-extent args))
             (setq value (apply function value args))))
      (apply #'map nil #'fun2 sequences)
      value)))

(defun fold-1 (function initial-value sequence)
  "Fold `FUNCTION' over each value in `SEQUENCE'."
  (declare (type function function))
  (etypecase sequence
    (list
     (loop
        for x in sequence
        for y = (funcall function initial-value x) then
          (funcall function y x)
        finally (return y)))
    (simple-vector
     (let ((y initial-value))
       (dotimes (i (length sequence))
         (setq y (funcall function y (svref sequence i))))
       y))
    (sequence
     (reduce function sequence :initial-value initial-value))))

(declaim (inline fold))
(defun fold (function initial-value &rest sequences)
  (declare (type function function)
           (dynamic-extent sequences))
  (destructuring-bind (sequence . more-sequences) sequences
    (if more-sequences
        (fold-n function initial-value sequences)
        (fold-1 function initial-value sequence))))

(defmacro cond-compare ((value1 value2 compare) lt-case eq-case gt-case)
  (alexandria:with-gensyms (c)
    `(let ((,c (funcall ,compare ,value1 ,value2)))
       (declare (type fixnum ,c))
       (cond
         ((< ,c 0)
          ,lt-case)
         ((> ,c 0)
          ,gt-case)
         (t ,eq-case)))))

(defmacro if-less-eq-compare ((value1 value2 compare) lt-eq-case gt-case)
  `(if (<= (funcall ,compare ,value1 ,value2) 0)
       (progn ,lt-eq-case)
       (progn ,gt-case)))

(defmacro or-compare (&rest comparisons)
  "Short-circuit evaluatation of arguments, returning the first one that is nonzero."
  (cond
    ((null comparisons) 0)
    ((null (cdr comparisons))
     (car comparisons))
     (t
      (alexandria:with-gensyms (i)
        `(let ((,i ,(car comparisons)))
           (declare (type fixnum ,i))
           (if (zerop ,i)
               (or-compare ,@(cdr comparisons))
               ,i))))))

(defun vector-range (start end)
  (apply #'vector (loop for i from start below end collect i)))
