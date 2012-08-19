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

;;;;;;;;;;;;;;;;;;
;; Pairing Heap ;;
;;;;;;;;;;;;;;;;;;

(defstruct (pairing-heap)
  root
  next
  children)

;; Invariants:
;;  - all values in children after root
;;  - values in next possibly before root

(defconstant +pairing-heap-max-array-length+ 8)

(defun pairing-heap-find-min (heap)
  (pairing-heap-root heap))

(defun root-pairing-heap (root children)
  (make-pairing-heap :root root
                     :children children))

(defun pairing-heap-merge (heap1 heap2 compare &optional next)
  (declare (type (or null pairing-heap) heap1 heap2)
           (type function compare))
  (cond
    ((null heap1) heap2)
    ((null heap2) heap1)
    (t (multiple-value-bind (root-heap child-heap)
           (if-less-eq-compare ((pairing-heap-find-min heap1) (pairing-heap-find-min heap2) compare)
                               (values heap1 heap2)
                               (values heap2 heap1))
         (make-pairing-heap :root (pairing-heap-root root-heap)
                            :children (make-pairing-heap :root (pairing-heap-root child-heap)
                                                         :children (pairing-heap-children child-heap)
                                                         :next (pairing-heap-children root-heap))
                            :next next)))))

(defun pairing-heap-insert (heap element compare)
  (etypecase heap
    (null
     (root-pairing-heap element nil))
    (pairing-heap
     (let ((c (funcall compare element (pairing-heap-root heap))))
       (if (<= c 0)
           (root-pairing-heap element heap)
           (root-pairing-heap (pairing-heap-root heap)
                              ;; TODO: In this case, we could save 1 word/value
                              ;;       by consing onto the next slot instead of making
                              ;;       a new pairing heap struct.  Update merge-pairs accordingly
                              (make-pairing-heap :root element
                                                 :next (pairing-heap-children heap))))))))

(defun pairing-heap-builder (compare)
  (lambda (heap element) (pairing-heap-insert heap element compare)))

(defun pairing-heap-merge-pairs (heaps compare)
  (declare (type (or null pairing-heap) heaps)
           (type function compare))
  (labels ((merge-left (pairs heaps)
             (if heaps
                 (let ((h2 (pairing-heap-next heaps)))
                   (if h2
                       (merge-left (pairing-heap-merge heaps h2 compare pairs)
                                   (pairing-heap-next h2))
                       (merge-right (pairing-heap-merge heaps pairs compare (when pairs (pairing-heap-next pairs))))))
                 (when pairs (merge-right pairs))))
           (merge-right (heaps)
             (assert heaps)
             (let ((h2 (pairing-heap-next heaps)))
               (if h2
                   (merge-right (pairing-heap-merge heaps h2 compare (pairing-heap-next h2)))
                   heaps))))
    (merge-left nil heaps)))

(defun pairing-heap-remove-min (heap compare)
  (let ((value (pairing-heap-root heap)))
    (values (pairing-heap-merge-pairs (pairing-heap-children heap) compare)
            value)))

(defun pairing-heap-list (heap compare)
  (loop for h = heap then (pairing-heap-remove-min h compare)
     while h
     collect (pairing-heap-find-min h)))
