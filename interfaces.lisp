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
  (let ((node (binary-tree-search-node (tree-map-root tree-map)
                                       (cons key nil)
                                       (tree-map-compare tree-map))))
    (if node
        (values (binary-tree-value node) t)
        (values nil nil))))


(defun map-tree-map (order result-type function tree-map)
  "Apply FUNCTION to all elements in TREE-MAP.
ORDER: (or :inorder :preorder :postorder
RESULT-TYPE: (or nil 'list)
FUNCTION: (lambda (key value))"
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
  (%make-tree-set compare
                  (fold (lambda (tree x) (avl-tree-insert tree x compare))
                        nil
                        args)))

(defun tree-set-count (set)
  (avl-tree-count (tree-set-root set)))

(defun map-tree-set (result-type function set)
  (map-binary-tree :inorder result-type function (tree-set-root set)))

(defun fold-tree-set (function initial-value set)
  (fold-binary-tree :inorder function initial-value (tree-set-root set)))


(defun tree-set-remove-min (set)
  (multiple-value-bind (tree item) (avl-tree-remove-min (tree-set-root set))
    (values (%make-tree-set (tree-set-%compare set) tree)
            item)))

(defun tree-set-remove-max (set)
  (multiple-value-bind (tree item) (avl-tree-remove-max (tree-set-root set))
    (values (%make-tree-set (tree-set-%compare set) tree)
            item)))


(defmacro def-tree-set-item-op (name implementation-name)
  `(defun ,name (set item)
     (%make-tree-set (tree-set-%compare set)
                     (,implementation-name (tree-set-root set)
                                           item
                                           (tree-set-%compare set)))))

(def-tree-set-item-op tree-set-insert avl-tree-insert)
(def-tree-set-item-op tree-set-remove avl-tree-remove)

(defun tree-set-member-p (set item)
  (binary-tree-member-p (tree-set-root set) item (tree-set-%compare set)))

(defmacro def-tree-set-binop (name implementation-name)
  `(defun ,name (set-1 set-2)
     (%make-tree-set (tree-set-%compare set-1)
                     (,implementation-name (tree-set-root set-1)
                                           (tree-set-root set-2)
                                           (tree-set-%compare set-1)))))

(def-tree-set-binop tree-set-union avl-tree-union)
(def-tree-set-binop tree-set-intersection avl-tree-intersection)
(def-tree-set-binop tree-set-difference avl-tree-difference)

(defun tree-set-equal-p (set-1 set-2)
  (binary-tree-equal (tree-set-root set-1)
                     (tree-set-root set-2)
                     (tree-set-%compare set-1)))

(defun tree-set-subset-p (set-1 set-2)
  (avl-tree-subset (tree-set-root set-1)
                  (tree-set-root set-2)
                  (tree-set-%compare set-1)))

(defun tree-set-compare (tree-1 tree-2)
  (avl-tree-compare (tree-set-root tree-1) (tree-set-root tree-2)
                    (tree-set-%compare tree-1)))

;;;;;;;;;;;;;;;
;; Tree-Heap ;;
;;;;;;;;;;;;;;;


(defstruct (tree-heap (:constructor %make-tree-heap (root cost)))
  root
  cost)

(defun new-tree-heap (heap root)
  (%make-tree-heap root (tree-heap-cost heap)))

(defun make-tree-heap (cost-function)
  (%make-tree-heap nil cost-function))

(defun tree-heap-compare (a b)
  (let ((c-a (car a))
        (c-b (car b)))
    (cond
      ((> c-a c-b) 1)
      ((< c-a c-b) -1)
      ((equalp (cdr a) (cdr b)) 0)
      (t -1))))

(defun tree-heap-insert (heap value &optional (cost (funcall (tree-heap-cost heap) value)))
  (new-tree-heap heap
                 (avl-tree-insert (tree-heap-root heap)
                                  (cons cost value)
                                  #'tree-heap-compare)))

(defun tree-heap-find-min (heap)
  (cdr (binary-tree-min (tree-heap-root heap))))

(defun tree-heap-find-max (heap)
  (cdr (binary-tree-max (tree-heap-root heap))))

(defun tree-heap-remove-min (heap)
  (multiple-value-bind (root value) (avl-tree-remove-min (tree-heap-root heap))
    (values (new-tree-heap heap root) (cdr value))))

(defun tree-heap-remove-max (heap)
  (multiple-value-bind (root value ) (avl-tree-remove-max (tree-heap-root heap))
    (values (new-tree-heap heap root) (cdr value))))
