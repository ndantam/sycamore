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


;;;;;;;;;;;;;;;;;;;;
;; Top containter ;;
;;;;;;;;;;;;;;;;;;;;
(defstruct (root-tree (:constructor %make-aux-tree (%compare root)))
  %compare
  (root nil))


(defun make-aux-compare (compare)
  (lambda (pair-1 pair-2)
    (funcall compare (car pair-1) (car pair-2))))


;;;;;;;;;;;;;;;
;; TREE-MAPS ;;
;;;;;;;;;;;;;;;

(defstruct (tree-map (:constructor %make-tree-map (compare root)))
  compare
  (root nil))

(defun make-tree-map (compare)
  "Create a new tree-map."
  (declare (type function compare))
  (%make-tree-map (make-aux-compare compare)
                  nil))

(defun tree-map-insert (tree-map key value)
  "Insert KEY=>VALUE into TREE-MAP, returning the new tree-map."
  (%make-tree-map (tree-map-compare tree-map)
                  (wb-tree-replace (tree-map-root tree-map)
                                    (cons key value)
                                    (tree-map-compare tree-map))))

(defun tree-map-remove (tree-map key)
  "Insert KEY from TREE-MAP, returning the new tree-map."
  (%make-tree-map (tree-map-compare tree-map)
                  (wb-tree-remove (tree-map-root tree-map)
                                   (cons key nil)
                                   (tree-map-compare tree-map))))

(defun tree-map-find (tree-map key &optional default)
  "Find value indexed by KEY in TREE-MAP."
  (multiple-value-bind (cons present)
      (binary-tree-find (tree-map-root tree-map)
                        (cons key nil)
                        (tree-map-compare tree-map))
    (if present
        (values (cdr cons) (car cons) t)
        (values default key nil))))


(defun map-tree-map (order result-type function tree-map)
  "Apply FUNCTION to all elements in TREE-MAP.
ORDER: (or :inorder :preorder :postorder).
RESULT-TYPE: (or nil 'list).
FUNCTION: (lambda (key value))."
  (declare (type function function))
  (let ((result (map-binary-tree order (if (eq result-type 'tree-map)
                                           'list
                                             result-type)
                                 (lambda (pair) (funcall function (car pair) (cdr pair)))
                                 (tree-map-root tree-map))))
    (when result-type
      (ecase result-type
        (list result)
        (treemap
         (assert nil))))))

(defun fold-tree-map (function initial-value tree-map)
  "Fold FUNCTION over members of the map
FUNCTION: (lambda (accumulated-value key value))."
  (fold-binary-tree :inorder (lambda (accum pair) (funcall function accum (car pair) (cdr pair)))
                    initial-value (tree-map-root tree-map)))

(defun tree-map-count (map)
  "Number of elements in MAP."
  (wb-tree-count (tree-map-root map)))

;;;;;;;;;;;;;;;
;; TREE-SET ;;
;;;;;;;;;;;;;;;

(defstruct (tree-set (:constructor %make-tree-set (%compare %root)))
  %compare
  %root)

(defun tree-set-root (set)
  (etypecase set
    (tree-set (tree-set-%root set))))

(defun make-tree-set (compare)
  "Create a new tree-set."
  (%make-tree-set compare nil))

(defun tree-set (compare &rest args)
  "Create a new tree-set containing all items in ARGS."
  (%make-tree-set compare
                  (fold (lambda (tree x) (wb-tree-insert tree x compare))
                        nil
                        args)))

(defun tree-set-count (set)
  "Number of elements in SET."
  (wb-tree-count (tree-set-root set)))

(defun map-tree-set (result-type function set)
  "Apply FUNCTION to every element of SET."
  (map-binary-tree :inorder result-type function (when set (tree-set-root set))))

(defmacro do-tree-set ((var set &optional result) &body body)
  `(progn
     (map-tree-set nil (lambda (,var)
                         ,@body)
                   ,set)
     ,result))

(defun fold-tree-set (function initial-value set)
  "Fold FUNCTION over every element of SET."
  (fold-binary-tree :inorder function initial-value (tree-set-root set)))


(defun tree-set-remove-min (set)
  "Remove minimum element of SET."
  (declare (type tree-set set))
  (multiple-value-bind (tree item) (wb-tree-remove-min (tree-set-root set))
    (values (%make-tree-set (tree-set-%compare set) tree)
            item)))

(defun tree-set-remove-max (set)
"Remove maximum element of SET."
  (multiple-value-bind (tree item) (wb-tree-remove-max (tree-set-root set))
    (values (%make-tree-set (tree-set-%compare set) tree)
            item)))

(defun tree-set-remove-position (set i)
"Remove element of SET and position I."
  (multiple-value-bind (tree item)
      (wb-tree-remove-position (tree-set-root set) i (tree-set-%compare set))
    (values (%make-tree-set (tree-set-%compare set) tree)
            item)))


(defmacro def-tree-set-item-op (name implementation-name doc)
  `(defun ,name (set item)
     ,doc
     (%make-tree-set (tree-set-%compare set)
                     (,implementation-name (tree-set-root set)
                                           item
                                           (tree-set-%compare set)))))

(def-tree-set-item-op tree-set-insert wb-tree-insert
  "Insert ITEM into SET.")

(def-tree-set-item-op tree-set-remove wb-tree-remove
  "Remove ITEM from SET.")

(defun tree-set-member-p (set item)
  "Is ITEM a member of SET?"
  (binary-tree-member-p (tree-set-root set) item (tree-set-%compare set)))


(defun tree-set-find (set item)
  "Find ITEM in SET"
  (binary-tree-find (tree-set-root set) item (tree-set-%compare set)))

(defun tree-set-intern (set item)
  "Add item to set, unless it already exists.
RETURNS: (values NEW-SET NEW-ITEM)"
  (multiple-value-bind (set-item exists)
      (tree-set-find set item)
    (if exists
        (values set set-item)
        (values (tree-set-insert set item)
                item))))


(defmacro def-tree-set-binop (name implementation-name doc)
  `(defun ,name (set-1 set-2)
     ,doc
     (%make-tree-set (tree-set-%compare set-1)
                     (,implementation-name (tree-set-root set-1)
                                           (tree-set-root set-2)
                                           (tree-set-%compare set-1)))))

(def-tree-set-binop tree-set-union wb-tree-union
  "Union of SET-1 and SET-2.")
(def-tree-set-binop tree-set-intersection wb-tree-intersection
  "Intersection of SET-1 and SET-2.")
(def-tree-set-binop tree-set-difference wb-tree-difference
  "Difference of SET-1 and SET-2.")

(defun tree-set-intersection-difference (tree-1 tree-2)
  "Simultanously compute intersection and difference."
  (let ((compare (tree-set-%compare tree-1)))
    (multiple-value-bind (i d)
        (wb-tree-intersection-difference (tree-set-root tree-1)
                                          (tree-set-root tree-2)
                                          compare)
      (values (%make-tree-set compare i)
              (%make-tree-set compare d)))))

(defun tree-set-equal-p (set-1 set-2)
  "Do SET-1 and SET-2 contain the same elements?"
  (binary-tree-equal (tree-set-root set-1)
                     (tree-set-root set-2)
                     (tree-set-%compare set-1)))

(defun tree-set-subset-p (set-1 set-2)
  "Is SET-1 as subset of SET-2?"
  (wb-tree-subset (tree-set-root set-1)
                  (tree-set-root set-2)
                  (tree-set-%compare set-1)))

;(declaim (ftype (function (tree-set tree-set) fixnum) tree-set-compare))
(defun tree-set-compare (tree-1 tree-2)
  "Order relation on sets."
  (wb-tree-compare (tree-set-root tree-1) (tree-set-root tree-2)
                    (tree-set-%compare tree-1)))

(defun tree-set-list (set)
  "Return list of elements in `SET' in comparison order."
  (let ((c (cons nil nil)))
    (declare (dynamic-extent c))
    (fold-tree-set (lambda (cons x)
                     (let ((cons-2 (cons x nil)))
                       (rplacd cons cons-2)
                       cons-2))
                   c set)
    (cdr c)))

(defun tree-set-position (set value)
  "Return the position of `VALUE' in `SET' or NIL."
  (wb-tree-position (tree-set-root set) value (tree-set-%compare set)))

(defun tree-set-ref (set subscript)
  "Return the element of `SET' at position `SUBSCRIPT'."
  (wb-tree-ref (tree-set-root set) subscript))

;;;;;;;;;;;;;;;
;; Tree-Bag  ;;
;;;;;;;;;;;;;;;

(defstruct (tree-bag (:constructor %make-tree-bag (%compare root))
                     (:include root-tree)))

(defun tree-bag-increment (value)
  (values
   (let ((key (car value))
         (count (cdr value)))
     (declare (type unsigned-fixnum count))
     (cons key (1+ count)))
   ;; always present
   t))

(defun tree-bag-decrement (value)
  (let ((key (car value))
        (count (cdr value)))
    (declare (type unsigned-fixnum count))
    (if (= count 0)
        (values nil nil)
        (values (cons key (1- count))))))

(defun %tree-bag-insert (tree compare x)
  (let ((x (cons x 0)))
    (wb-tree-modify tree x compare #'tree-bag-increment x)))

(defun tree-bag (compare &rest args)
  (let ((compare (make-aux-compare compare)))
    (%make-tree-bag compare
                    (fold (lambda (tree x) (%tree-bag-insert tree compare x))
                          nil
                          args))))

(defun tree-bag-insert (tb x)
  (let ((x (cons x 0))
        (compare (tree-bag-%compare tb)))
    (%make-tree-bag compare
                    (wb-tree-modify (tree-bag-root tb)
                                     x compare #'tree-bag-increment x))))

(defun tree-bag-count (bag key)
  "Return count of `KEY' in `BAG'."
  (multiple-value-bind (cons present)
      (let ((key (cons key 0)))
        (binary-tree-find (tree-bag-root bag) key (tree-bag-%compare bag)))
    (if present
        (cdr cons)
        0)))


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
;;                  (wb-tree-reinsert (tree-heap-root heap)
;;                                     (cons cost value)
;;                                     #'tree-heap-compare)))

;; (defun tree-heap-find-min (heap)
;;   (cdr (binary-tree-min (tree-heap-root heap))))

;; (defun tree-heap-find-max (heap)
;;   (cdr (binary-tree-max (tree-heap-root heap))))

;; (defun tree-heap-remove-min (heap)
;;   (multiple-value-bind (root value) (wb-tree-remove-min (tree-heap-root heap))
;;     (values (new-tree-heap heap root) (cdr value))))

;; (defun tree-heap-remove-max (heap)
;;   (multiple-value-bind (root value ) (wb-tree-remove-max (tree-heap-root heap))
;;     (values (new-tree-heap heap root) (cdr value))))

;; (defun tree-heap-construct (cost-function elements)
;;   (fold #'tree-heap-insert (make-tree-heap cost-function)
