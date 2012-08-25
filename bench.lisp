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


(defparameter *bench-data-file*
  (make-pathname :directory '(:absolute "tmp") :name "sycamore-bench" :type "dat"))

(defun bench-generate-data (&key
                            (output *bench-data-file*)
                            (count 1000000)
                            (max count))
  (with-open-file (s output :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format s "~{~&~D~}"
            (loop for i below count
               collect (random max)))))


(defun bench-avl ()
  ;(declare (optimize (speed 3) (safety 0)))
  (let* ((data)
         (tree)
         (compare  (lambda (x y)
                     (declare (type fixnum x y))
                     (declare (optimize (speed 3) (safety 0)))
                     (the fixnum (- x y))))
         (builder (avl-tree-builder compare)))
    (with-open-file (s *bench-data-file* :direction :input)
      (setq data
            (loop for i = (read s nil nil)
               while i
               collect i)))
    (format t "~&CREATE~&")
    (format t "~&------~&")
    (time (setq tree (fold builder nil data)))
    (format t "~&SEARCH~&")
    (format t "~&-----~&")
    (time (loop for k in data
             do (binary-tree-find tree k compare)))

    (let ((data-0 (loop with len = (length data)
                     for i below (/ len 2)
                     for k in data
                     collect k))
          (data-1 (nthcdr (/ (length data) 2) data)))
      (let ((tree-1 (fold builder nil data-0))
            (tree-2 (fold builder nil data-1)))
        (format t "~&Union~&")
        (format t "~&-----~&")
        (time (avl-tree-union tree-1 tree-2 compare))))
    nil))
