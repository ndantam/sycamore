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

;;;;;;;;;;;;;;;
;;  T Trees  ;;
;;;;;;;;;;;;;;;

;; Reuse the AVL-Tree structure
;; The binary-tree value slot now holds a sorted array of values

(defconstant +t-tree-max-array-length+ 8)

(defun make-t-tree (left data right)
  (declare (type simple-vector data))
  (%make-avl-tree (+ (length data) (avl-tree-count left) (avl-tree-count right))
                  left data right))

(defun map-t-tree-nil (function tree)
  (when tree
    (map-t-tree-nil function (binary-tree-left tree))
    (map nil function (binary-tree-value tree))
    (map-t-tree-nil function (binary-tree-right tree))))

(defun map-t-tree (result-type function tree)
  (cond
    ((null result-type)
     (map-t-tree-nil function tree))
    ((eq 'list result-type)
     (let* ((c (cons nil nil))
            (k c))
       (map-t-tree-nil (lambda (x)
                         (rplacd k (cons (funcall function x) nil))
                         (setq k (cdr k)))
                       tree)
       (cdr c)))
    (t (error "Unknown result-type: ~A" result-type))))



(defun find-t-tree (value tree compare)
  (declare (type function compare))
  (if (null tree)
      nil
      (let* ((d (binary-tree-value tree))
             (len (1- (length d)))
             (c0 (funcall compare value (aref d 0)))
             (c1 (funcall compare value (aref d len))))
        (declare (type fixnum c0 c1 len)
                 (type simple-vector d))
        (cond
          ((< c0 0) (find-t-tree (binary-tree-left tree) value compare))
          ((> c1 0) (find-t-tree (binary-tree-right tree) value compare))
          ((= c0 0) (aref d 0))
          ((= c1 0) (aref d len))
          (t (array-tree-search d value compare 1 len))))))

(defun t-tree-array-insert-split (array value position)
  (declare (type simple-vector array)
           (type fixnum position))
  (let* ((n (length array))
         (n/2 (ash n -1)))
    (if (< position (1+ n/2))
        (values (array-tree-insert-at array value position 0 n/2)
                (subseq array n/2 n))
        (values (subseq array 0 n/2)
                (array-tree-insert-at array value position n/2)))))

(defun balance-t-tree (left value right)
  (balance-general-avl-tree #'make-t-tree 3 left value right))

(defun t-tree-insert (tree value compare)
  "Insert `value' into `tree' returning new tree."
  (declare (type function compare))
  ;;(declare (optimize (speed 3) (safety 0)))
  (if (null tree)
      (make-t-tree nil (vector value) nil)
      (labels ((insert (tree value)
                 (with-avl-tree (l d r) tree
                   (declare (type simple-vector d))
                   (let* ((len (length d))
                          (c0 (funcall compare value (aref d 0)))
                          (c1 (funcall compare value (aref d (1- len)))))
                     (declare (type fixnum len c0 c1))
                     (cond
                       ;; recurse left
                       ((and l (< c0 0))
                        (balance-t-tree (insert l value) d r))
                       ;; recurse right
                       ((and r (> c1 0))
                        (balance-t-tree l d (insert r value)))
                       ;; insert here
                       (t
                        (multiple-value-bind (position present) (array-tree-insert-position d value compare)
                          (cond
                            ;; replace element
                            (present
                             (make-t-tree l (array-tree-set d value position) r))
                            ;; insert into array
                            ((< len +t-tree-max-array-length+)
                             (make-t-tree l (array-tree-insert-at d value  position) r))
                            ;; split left
                            ((null l)
                             (multiple-value-bind (a0 a1) (t-tree-array-insert-split d value position)
                               (balance-t-tree (make-t-tree nil a0 nil)
                                                         a1 r)))
                            ;; split right
                            ((null r)
                             (multiple-value-bind (a0 a1) (t-tree-array-insert-split d value position)
                               (balance-t-tree l a0 (make-t-tree nil a1 nil))))
                            ;; insert min to left
                            ((avl-tree-smaller l r)
                             (balance-t-tree (insert l (aref d 0))
                                             (array-tree-insert-at d value position 1)
                                             r))
                            ;; insert max to right
                            (t
                             (balance-t-tree l (array-tree-insert-at d value position 0  (1- len))
                                             (insert r (aref d (1- len)))))))))))))
        (insert tree value))))


(defun t-tree-builder (compare)
  (lambda (tree value) (t-tree-insert tree value compare)))

;; (defun absorb-t-tree (left value right)
;;   (let ((len (length value)))
;;     (cond
;;       ;; absorb left leaf
;;       ((and left (binary-tree-leaf-p left)
;;             (<= (+ len (length (binary-tree-value left)))
;;                 +t-tree-max-array-length+))
;;        )
;;       ;; absorb right leaf
;;       ((and right (binary-tree-leaf-p left)
;;              (<= (+ len (length (binary-tree-value left)))
;;                  +t-tree-max-array-length+))
;;         )

;;     ;; absorb left half-leaf
;;     ;; absorb right half-leaf

;; (defun t-tree-remove-pos (tree value pos)
;;   (cond
;;     ;; delete empty leaf
;;     ((and (null l) (null r) (= 1 len)) nil)
;;     ;; absorb half left
;;     ;; absorb half right


;; remove from inner
    ;; above min - done
    ;; below min - pull in a leaf
;; remove from half leaf
    ;; leaf-mergable
    ;; above min
    ;; below min
;; remove from leaf
    ;; above min
    ;; empty leaf
    ;; below min


;; (defun t-tree-remove (tree value compare)
;;   "Remove `value' from `tree' returning new tree."
;;   (when tree
;;     (with-avl-tree (l d r) tree
;;       (let* ((len (length d))
;;              (c0 (funcall compare value (aref d 0)))
;;              (c1 (funcall compare value (aref d (1- len)))))
;;           (cond
;;             ;; recurse left
;;             ((< c0 0)
;;              (balance-avl-tree (t-tree-remove l value compare) d r))
;;             ;; recurse right
;;             ((> c1 0)
;;              (balance-avl-tree l d (t-tree-remove r value compare)))
;;             (t
;;              (let ((pos (position-if (lambda (x) (zerop (funcall compare value x))) d)))
;;                (if pos
;;                    (cond
;;                      ;; delete empty leaf
;;                      ((and (null l) (null r) (= 1 len)) nil)
;;                      )
;;                    ;; not in tree
;;                    tree))))))))


(defun t-tree-dot (tree &key output)
  (output-dot output
              (lambda (s)
                (let ((i -1))
                  (labels ((helper (parent tree)
                             (let ((x (incf i)))
                               (format s "~&  ~A[label=\"~{~A~^, ~} (~D)\"~:[shape=none~;shape=box~]];~&"
                                       x (if tree
                                             (map 'list #'identity (binary-tree-value tree))
                                             nil)
                                       (avl-tree-count tree)
                                       tree)
                               (when parent
                                 (format s "~&  ~A -> ~A;~&"
                                         parent x))
                               (when tree
                                 (helper x (binary-tree-left tree))
                                 (helper x (binary-tree-right tree))))))
                    (format s "~&digraph {  ~&")
                    (helper nil tree)
                    (format s "~&}~&"))))))
