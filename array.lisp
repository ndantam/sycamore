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


;;;;;;;;;;;;
;; Arrays ;;
;;;;;;;;;;;

(defun array-tree-position (vector value compare &optional (start 0) (end (length vector)))
  (declare (type function compare)
           (type fixnum start end)
           (type simple-vector vector))
  (declare (type fixnum start end))
  (if (>= start end)
      nil
      (let* ((i (ash (+ start end) -1))
             (c (funcall compare value (aref vector i))))
        (declare (type fixnum c))
        (cond
          ((< c 0) (array-tree-position vector value compare start i))
          ((> c 0) (array-tree-position vector value compare (1+ i) end))
          (t i)))))

(defun array-tree-search (vector value compare &optional (start 0) (end (length vector)))
  (let ((i (array-tree-position vector value compare start end)))
    (if i
        (values (aref vector i) t)
        (values nil nil))))


(defun array-tree-set (vector value i)
  (declare (type fixnum i)
           (type simple-vector vector))
  (let ((new-vector (make-array (length vector))))
    (replace new-vector vector :end2 i)
    (setf (aref new-vector i) value)
    (replace new-vector vector :start1 (1+ i) :start2 (1+ i))
    new-vector))

(defun array-tree-insert-at (vector value i &optional (start 0) (end (length vector)))
  (declare (type simple-vector vector)
           (type fixnum i start end))
  (let ((new-vector (make-array (1+ (- end start))))
        (j (- i start)))
    (when (> i start)
      (replace new-vector vector :start2 start :end2 i))
    (setf (aref new-vector j) value)
    (when (< i end)
      (replace new-vector vector :start1 (1+ j) :start2 i :end2 end))
    new-vector))

(defun array-tree-insert-position (vector value compare &optional (start 0) (end (length vector)))
  (declare (type fixnum start end)
           (type simple-vector vector)
           (type function compare))
  (if (>= start end)
      (values start nil)
      (let* ((i (ash (+ start end) -1))
             (c (funcall compare value (aref vector i))))
        (declare (type fixnum i c))
        (cond
          ((< c 0) (array-tree-insert-position vector value compare start i))
          ((> c 0) (array-tree-insert-position vector value compare (1+ i) end))
          (t (values i t))))))

(defun array-tree-insert (vector value compare)
  "Insert `value' in order into `original-array', nondestructive."
  (multiple-value-bind (position present) (array-tree-insert-position vector value compare)
    (if present
        (array-tree-set vector value position)
        (array-tree-insert-at vector value position))))

(defun array-tree-builder (compare)
  (lambda (array value)
    (array-tree-insert array value compare)))

(defun array-tree-remove (vector value compare)
  (let ((i (array-tree-position vector value compare)))
    (if i
        (let ((new-vector (make-array (1- (length vector)))))
          (when (> i 0)
            (replace new-vector vector :start2 0 :end2 i))
          (when (< i (1- (length vector)))
            (replace new-vector vector :start1 i :start2 (1+ i)))
          new-vector)
        vector)))

(defun array-tree-count-unique (vector-1 vector-2 compare)
  (declare (type simple-vector vector-1 vector-2)
           (type function compare))
  "Count number of unique elements between vector-1 and vector-2"
  (labels ((rec (i j count)
             (declare (type fixnum i j count))
             (cond
               ((= i (length vector-1))
                (+ count (- (length vector-2) j)))
               ((= j (length vector-2))
                (+ count (- (length vector-1) i)))
               (t
                (let ((c (funcall compare (aref vector-1 i) (aref vector-2 j))))
                  (declare (type fixnum c))
                  (cond
                    ((< c 0) (rec (1+ i) j (1+ count)))
                    ((> c 0) (rec  i (1+ j) (1+ count)))
                    (t (rec (1+ i) (1+ j) count))))))))
    (rec 0 0 0)))

(defun array-tree-split-at (tree position)
  (values (when (> position 0) (subseq tree 0 position))
          (aref tree position)
          (when (< position (1- (length tree)))
            (subseq tree (1+ position)))))

(defun array-tree-split (tree x compare)
  (let ((n (length tree)))
    (multiple-value-bind (position present) (array-tree-insert-position tree x compare)
      (values (when (> position 0) (subseq tree 0 position))
              present
              (let ((i (if present (1+ position) position)))
                (when (< i n)
                  (subseq tree i)))))))

(defun array-tree-intersection (tree1 tree2 compare)
  (let ((array (make-array 0 :adjustable t :fill-pointer t)))
    (labels ((rec (i j)
               (when (and (< i (length tree1))
                          (< j (length tree2)))
                 (let ((c (funcall compare (aref tree1 i) (aref tree2 j))))
                   (cond ((< c 0)
                          (rec (1+ i) j))
                         ((> c 0)
                          (rec i (1+ j)))
                         (t
                          (vector-push-extend (aref tree1 i) array)
                          (rec (1+ i) (1+ j))))))))
      (rec 0 0)
      ;; make it a simple array
      (let ((n (length array)))
        (if (> n 0)
            (replace (make-array n) array)
            nil)))))



;; (defun array-tree-insert-split (array value compare)
;;   (let* ((n (length array))
;;          (n/2 (ash n -1)))
;;          (i (array-tree-position value array compare)))
;;     (if (and i (< i n/2))
;;         (values (array-tree-insert value array compare 0 n/2 i)
;;                 (subseq array n/2))
;;         (values (subseq array 0 n/2)
;;                 (array-tree-insert value array compare n/2 n i)))))
