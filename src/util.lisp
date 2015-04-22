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

(in-package :sycamore-util)

(in-package :sycamore)

(deftype unsigned-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(defun fixnum-compare (a b)
  "Compare two fixnums"
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
     (let ((y initial-value))
       (dolist (x sequence)
         (setq y (funcall function y x)))
       y))
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
  (with-gensyms (c)
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
      (with-gensyms (i)
        `(let ((,i ,(car comparisons)))
           (declare (type fixnum ,i))
           (if (zerop ,i)
               (or-compare ,@(cdr comparisons))
               ,i))))))

(defun vector-range (start end)
  (apply #'vector (loop for i from start below end collect i)))


(declaim (inline  string-compare-inline))
(defun string-compare-inline (a b)
  (let ((n-a (length a))
        (n-b (length b)))
    (dotimes (i (min n-a n-b))
      (let ((c-a (aref a i))
            (c-b (aref b i)))
        (unless (eql c-a c-b)
          (return-from string-compare-inline (- (char-code c-a)
                                                (char-code c-b))))))
    (- n-a n-b)))

(defun string-compare (a b)
  (etypecase a
    (simple-string
     (etypecase b
       (simple-string (string-compare-inline a b))
       (string (string-compare-inline a b))))
    (string
     (etypecase b
       (simple-string (string-compare-inline a b))
       (string (string-compare-inline a b))))))

(defun bit-vector-compare (a b)
  "Compare bitvectors `A' and `B'."
  (declare (type simple-bit-vector a b)
           (optimize (speed 3) (safety 0)))
  (let* ((n-a (length a))
         (n-b (length b)))
    (or-compare (fixnum-compare n-a n-b)
                (let ((i (mismatch a b)))
                  (if i
                      (let ((x (aref a i))
                            (y (aref b i)))
                        (- x y))
                      0)))))



(defun gsymbol-compare-atom (a b)
  (declare (optimize (speed 3) (safety 0)))
  (if (eq a b)
      0
      (etypecase a
    (fixnum
     (etypecase b
       (fixnum (if (< a b) -1 1))
       (character 1)
       (string 1)
       (symbol 1)))
    (character
     (etypecase b
       (fixnum -1)
       (character (if (< (char-code a) (char-code b))
                      -1 1))
       (string 1)
       (symbol 1)))
    (string
     (etypecase b
       (fixnum -1)
       (character -1)
       (string (string-compare a b))
       (symbol 1)))
    (symbol
     (etypecase b
       (fixnum -1)
       (character -1)
       (string -1)
       (symbol (cond ((string< a b) -1)
                     ((string> a b) 1)
                     (t 0))))))))

(defun gsymbol-compare (a b)
  (etypecase a
    (null (if b -1 0))
    (atom (etypecase b
            (null 1)
            (atom (gsymbol-compare-atom a b))
            (list -1)))
    (cons
     (etypecase b
       (atom 1)
       (list (or-compare (gsymbol-compare (car a) (car b))
                         (gsymbol-compare (cdr a) (cdr b))))))))


(defun strcat (&rest args)
  (apply #'concatenate 'string (map 'list #'string args)))

#+sbcl
(defun output-dot-file (program output function lang)
  "Run `dot' on the output of FUNCTION.
OUTPUT: output filename
FUNCTION: (lambda (stream)) => nil, prints dot on STREAM
LANG: language output for dot, (or pdf ps eps png)"
  (let ((p (sb-ext:run-program program (list (concatenate 'string "-T" lang))
                               :wait nil :search t :input :stream :output output
                               :if-output-exists :supersede)))
    (funcall function (sb-ext:process-input p))
    (close (sb-ext:process-input p))
    (sb-ext:process-wait p)
    (sb-ext:process-close p)))


(defun output-dot (output function &key
                   (program "dot")
                   (lang (and (stringp output) (car (last (ppcre:split "\\." output))))))
  "Produce graphiz output, dispatching on type of OUTPUT.
OUTPUT:  (or filename stream t nil)
FUNCTION: (lambda (stream)) => nil, prints dot text on STREAM
LANG: language output for dot, (or pdf ps eps png)"
  (cond
    ((streamp output)
     (funcall function output))
    ((null output)
     (with-output-to-string (s) (output-dot s function)))
    ((eq output t) (output-dot *standard-output* function))
    ((stringp output)
     (output-dot-file program output function lang))
    (t (error "Unknown output: ~A" output))))
