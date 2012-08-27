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


;;;;;;;;;;;
;; QUEUE ;;
;;;;;;;;;;;
(defun queue-fuzz-generator ()
  (loop for i below (random (expt 2 10))
     collect
       (if (zerop (random 2))
           `(:enqueue ,(random (expt 2 10)))
           '(:dequeue))))

(defun queue-fuzz-tester (fuzz)
  (fuzz::do-operations
      ((queue list) (list (make-amortized-queue) nil))
      fuzz
    ((:enqueue arg)
     (list (amortized-enqueue queue arg)
           (append list (list arg))))
    ((:dequeue)
     (multiple-value-bind (q v) (amortized-dequeue queue)
       (and (equal v (car list))
            (list q (cdr list)))))))

(defun run-queue-tests (&key (count 1))
  (fuzz:run-tests #'queue-fuzz-generator
                  #'queue-fuzz-tester
                  :count count))

;;;;;;;;;;
;; Heap ;;
;;;;;;;;;;

(defun heap-fuzz-generator ()
  (loop for i below (random (expt 2 12))
     collect
       (case (random 3)
         (0 `(:insert ,(random (expt 2 12))))
         (1 '(:find-min))
         (2 `(:remove-min)))))

(defun heap-fuzz-tester (fuzz)
  (fuzz::do-operations
      ((list pairing-heap tree-heap) (list nil nil (make-tree-heap #'identity)))
      fuzz
    ((:insert arg)
     (list (cons arg list)
           (pairing-heap-insert pairing-heap arg #'-)
           (tree-heap-insert tree-heap arg)))
    ((:find-min)
     (if list
         (let ((new-list (sort list #'<)))
           (and (equal (car new-list)
                       (pairing-heap-find-min pairing-heap))
                (equal (car new-list)
                       (tree-heap-find-min tree-heap))
                (list new-list pairing-heap tree-heap)))
         (and (null pairing-heap)
              (tree-heap-empty-p tree-heap)
              (list list pairing-heap tree-heap))))
    ((:remove-min)
     (if list
         (multiple-value-bind (ph-1 pmin) (pairing-heap-remove-min pairing-heap #'-)
           (multiple-value-bind (th-1 tmin) (tree-heap-remove-min tree-heap)
             (destructuring-bind (lmin &rest lh-1) (sort list #'<)
               (and (equal lmin pmin)
                    (equal lmin tmin)
                    (list lh-1 ph-1 th-1)))))
         (and (null pairing-heap)
              (tree-heap-empty-p tree-heap)
              (list list pairing-heap tree-heap))))))

(defun run-heap-tests (&key (count 1))
  (fuzz:run-tests #'heap-fuzz-generator
                  #'heap-fuzz-tester
                  :count count))
