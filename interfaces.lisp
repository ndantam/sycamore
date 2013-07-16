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
;; TREE-MAPS ;;
;;;;;;;;;;;;;;;

(defstruct (tree-map (:constructor %make-tree-map (compare root)))
  compare
  (root nil))

(defun make-tree-map (compare)
  "Create a new tree-map."
  (declare (type function compare))
  (%make-tree-map (lambda (pair-1 pair-2)
                    (funcall compare (car pair-1) (car pair-2)))
                  nil))

(defun tree-map-insert (tree-map key value)
  "Insert KEY=>VALUE into TREE-MAP, returning the new tree-map."
  (%make-tree-map (tree-map-compare tree-map)
                  (avl-tree-replace (tree-map-root tree-map)
                                    (cons key value)
                                    (tree-map-compare tree-map))))

(defun tree-map-remove (tree-map key)
  "Insert KEY from TREE-MAP, returning the new tree-map."
  (%make-tree-map (tree-map-compare tree-map)
                  (avl-tree-remove (tree-map-root tree-map)
                                   (cons key nil)
                                   (tree-map-compare tree-map))))

(defun tree-map-find (tree-map key)
  "Find value indexed by KEY in TREE-MAP."
  (multiple-value-bind (cons present)
      (binary-tree-find (tree-map-root tree-map)
                        (cons key nil)
                        (tree-map-compare tree-map))
    (if present
        (values (cdr cons) t)
        (values nil nil))))


(defun map-tree-map (order result-type function tree-map)
  "Apply FUNCTION to all elements in TREE-MAP.
ORDER: (or :inorder :preorder :postorder).
RESULT-TYPE: (or nil 'list).
FUNCTION: (lambda (key value))."
  (declare (type function function))
  (%make-tree-map (tree-map-compare tree-map)
                  (map-binary-tree order result-type
                                   (lambda (pair) (funcall function (car pair) (cdr pair)))
                                   (tree-map-root tree-map))))

;;;;;;;;;;;;;;;
;; TREE-SET ;;
;;;;;;;;;;;;;;;

(defstruct (tree-set (:constructor %make-tree-set (%compare root)))
  %compare
  root)

(defun make-tree-set (compare)
  "Create a new tree-set."
  (%make-tree-set compare nil))

(defun tree-set (compare &rest args)
  "Create a new tree-set containing all items in ARGS."
  (%make-tree-set compare
                  (fold (lambda (tree x) (avl-tree-insert tree x compare))
                        nil
                        args)))

(defun tree-set-count (set)
  "Number of elements in SET."
  (avl-tree-count (tree-set-root set)))

(defun map-tree-set (result-type function set)
  "Apply FUNCTION to every element of SET."
  (map-binary-tree :inorder result-type function (tree-set-root set)))

(defun fold-tree-set (function initial-value set)
  "Fold FUNCTION over every element of SET."
  (fold-binary-tree :inorder function initial-value (tree-set-root set)))


(defun tree-set-remove-min (set)
"Remove minimum element of SET."
  (multiple-value-bind (tree item) (avl-tree-remove-min (tree-set-root set))
    (values (%make-tree-set (tree-set-%compare set) tree)
            item)))

(defun tree-set-remove-max (set)
"Remove maximum element of SET."
  (multiple-value-bind (tree item) (avl-tree-remove-max (tree-set-root set))
    (values (%make-tree-set (tree-set-%compare set) tree)
            item)))

(defun tree-set-remove-position (set i)
"Remove element of SET and position I."
  (multiple-value-bind (tree item)
      (avl-tree-remove-position (tree-set-root set) i (tree-set-%compare set))
    (values (%make-tree-set (tree-set-%compare set) tree)
            item)))


(defmacro def-tree-set-item-op (name implementation-name doc)
  `(defun ,name (set item)
     ,doc
     (%make-tree-set (tree-set-%compare set)
                     (,implementation-name (tree-set-root set)
                                           item
                                           (tree-set-%compare set)))))

(def-tree-set-item-op tree-set-insert avl-tree-insert
  "Insert ITEM into SET.")

(def-tree-set-item-op tree-set-remove avl-tree-remove
  "Remove ITEM from SET.")

(defun tree-set-member-p (set item)
  "Is ITEM a member of SET?"
  (binary-tree-member-p (tree-set-root set) item (tree-set-%compare set)))


(defun tree-set-find (set item)
  "Find ITEM in SET"
  (binary-tree-find (tree-set-root set) item (tree-set-%compare set)))

(defmacro def-tree-set-binop (name implementation-name doc)
  `(defun ,name (set-1 set-2)
     ,doc
     (%make-tree-set (tree-set-%compare set-1)
                     (,implementation-name (tree-set-root set-1)
                                           (tree-set-root set-2)
                                           (tree-set-%compare set-1)))))

(def-tree-set-binop tree-set-union avl-tree-union
  "Union of SET-1 and SET-2.")
(def-tree-set-binop tree-set-intersection avl-tree-intersection
  "Intersection of SET-1 and SET-2.")
(def-tree-set-binop tree-set-difference avl-tree-difference
  "Difference of SET-1 and SET-2.")

(defun tree-set-equal-p (set-1 set-2)
  "Do SET-1 and SET-2 contain the same elements?"
  (binary-tree-equal (tree-set-root set-1)
                     (tree-set-root set-2)
                     (tree-set-%compare set-1)))

(defun tree-set-subset-p (set-1 set-2)
  "Is SET-1 as subset of SET-2?"
  (avl-tree-subset (tree-set-root set-1)
                  (tree-set-root set-2)
                  (tree-set-%compare set-1)))

(defun tree-set-compare (tree-1 tree-2)
  "Order relation on sets."
  (avl-tree-compare (tree-set-root tree-1) (tree-set-root tree-2)
                    (tree-set-%compare tree-1)))

;;;;;;;;;;;;;;;
;; Tree-Heap ;;
;;;;;;;;;;;;;;;

;;; This was a bad idea

;; (defstruct (tree-heap (:constructor %make-tree-heap (root cost)))
;;   root
;;   cost)

;; (defun new-tree-heap (heap root)
;;   (%make-tree-heap root (tree-heap-cost heap)))

;; (defun make-tree-heap (cost-function)
;;   (%make-tree-heap nil cost-function))

;; (defun tree-heap-compare (a b)
;;   (let ((c-a (car a))
;;         (c-b (car b)))
;;     (cond
;;       ((> c-a c-b) 1)
;;       ((< c-a c-b) -1)
;;       ((equalp (cdr a) (cdr b)) 0)
;;       (t -1))))

;; (defun tree-heap-empty-p (heap)
;;   "Is HEAP empty?"
;;   (null (tree-heap-root heap)))

;; (defun tree-heap-insert (heap value &optional (cost (funcall (tree-heap-cost heap) value)))
;;   "Insert VALUE into HEAP."
;;   (new-tree-heap heap
;;                  (avl-tree-reinsert (tree-heap-root heap)
;;                                     (cons cost value)
;;                                     #'tree-heap-compare)))

;; (defun tree-heap-find-min (heap)
;;   (cdr (binary-tree-min (tree-heap-root heap))))

;; (defun tree-heap-find-max (heap)
;;   (cdr (binary-tree-max (tree-heap-root heap))))

;; (defun tree-heap-remove-min (heap)
;;   (multiple-value-bind (root value) (avl-tree-remove-min (tree-heap-root heap))
;;     (values (new-tree-heap heap root) (cdr value))))

;; (defun tree-heap-remove-max (heap)
;;   (multiple-value-bind (root value ) (avl-tree-remove-max (tree-heap-root heap))
;;     (values (new-tree-heap heap root) (cdr value))))

;; (defun tree-heap-construct (cost-function elements)
;;   (fold #'tree-heap-insert (make-tree-heap cost-function)
