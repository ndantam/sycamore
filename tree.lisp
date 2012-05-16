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


(in-package :motion-grammar)


;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC BINARY TREES ;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (binary-tree (:constructor make-binary-tree (left value right)))
  left
  value
  right)


(defun map-binary-tree-inorder (function tree)
  (when tree
    (map-binary-tree-inorder function (binary-tree-left tree))
    (funcall function (binary-tree-value tree))
    (map-binary-tree-inorder function (binary-tree-right tree))))

(defun map-binary-tree-preorder (function tree)
  (when tree
    (funcall function (binary-tree-value tree))
    (map-binary-tree-preorder function (binary-tree-left tree))
    (map-binary-tree-preorder function (binary-tree-right tree))))

(defun map-binary-tree-postorder (function tree)
  (when tree
    (map-binary-tree-postorder function (binary-tree-left tree))
    (map-binary-tree-postorder function (binary-tree-right tree))
    (funcall function (binary-tree-value tree))))

(defun map-binary-tree-nil (order function tree)
  (ecase order
    (:inorder (map-binary-tree-inorder function tree))
    (:postorder (map-binary-tree-postorder function tree))
    (:preorder (map-binary-tree-preorder function tree))))

(defun map-binary-tree-list (order function tree)
  (let* ((c (cons nil nil))
         (k c))
    (map-binary-tree-nil order (lambda (x)
                                 (rplacd k (cons (funcall function x) nil))
                                 (setq k (cdr k)))
                         tree)
    (cdr c)))

(defun map-binary-tree (order result-type function tree)
  "Map elements of tree.
ORDER: (or :inorder :preorder :postorder)
RESULT-TYPE: (or 'list nil)"
  (cond
    ((null result-type)
     (map-binary-tree-nil order function tree))
    ((eq 'list result-type)
     (map-binary-tree-list order function tree))
    (t (error "Unknown result-type: ~A" result-type))))

(defun fold-binary-tree (order function initial-value tree)
  (let ((v initial-value))
    (map-binary-tree-nil order
                         (lambda (x) (setq v (funcall function v x)))
                         tree)))

(defun binary-tree-search-node (tree value compare)
  "Return the node of TREE containing VALUE or NIL of not present."
  (when tree
    (do* ((tree-1 tree (if (< c 0) (binary-tree-left tree) (binary-tree-right tree)))
          (c (funcall compare value (binary-tree-value tree))
             (funcall compare value (binary-tree-value tree-1))))
         ((or (zerop c)
              (null tree-1)) tree-1))))

(defun binary-tree-left-left (tree)
  (binary-tree-left (binary-tree-left tree)))

(defun binary-tree-left-right (tree)
  (binary-tree-left (binary-tree-right tree)))

(defun binary-tree-right-left (tree)
  (binary-tree-right (binary-tree-left tree)))

(defun binary-tree-right-right (tree)
  (binary-tree-right (binary-tree-right tree)))

(defun binary-tree-value-left (tree)
  (binary-tree-value (binary-tree-left tree)))

(defun binary-tree-value-right (tree)
  (binary-tree-value (binary-tree-right tree)))

(defun binary-tree-dot (tree &key output)
  (output-dot output
              (lambda (s)
                (let ((i -1))
                  (labels ((helper (parent tree)
                             (let ((x (incf i)))
                               (format s "~&  ~A[label=\"~A\"~:[shape=none~;~]];~&"
                                       x (if tree
                                             (binary-tree-value tree)
                                             nil)
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

(defun binary-tree-min (tree)
  "Return minimum (leftmost) value of TREE."
  (do ((tree tree (binary-tree-left tree)))
      ((null (binary-tree-left tree)) (binary-tree-value tree))))

(defun binary-tree-max (tree)
  "Return maximum (rightmost) value of TREE."
  (do ((tree tree (binary-tree-right tree)))
      ((null (binary-tree-right tree)) (binary-tree-value tree))))

(defun binary-tree-count (tree)
  "Number of elements in TREE."
  (if tree
      (+ 1
         (binary-tree-count (binary-tree-left tree))
         (binary-tree-count (binary-tree-right tree)))
      0))

;;;;;;;;;;;
;;  AVL  ;;
;;;;;;;;;;;

(defstruct (avl-tree
             (:include binary-tree)
             (:constructor %make-avl-tree (height left value right)))
  (height 0 :type fixnum))

(defun make-avl-tree (left value right)
  (%make-avl-tree (1+ (max (if left (avl-tree-height left) 0)
                           (if right (avl-tree-height right) 0)))
                  left value right))


(defun right-avl-tree (left value right)
  (make-avl-tree (binary-tree-left left)
                 (binary-tree-value left)
                 (make-avl-tree (binary-tree-right left)
                                value
                                right)))

(defun avl-tree-rotate-right (tree)
  (right-avl-tree (binary-tree-left tree)
                  (binary-tree-value tree)
                  (binary-tree-right tree)))

(defun left-avl-tree (left value right)
  (make-avl-tree (make-avl-tree left
                                value
                                (binary-tree-left right))
                 (binary-tree-value right)
                 (binary-tree-right right)))

(defun avl-tree-rotate-left (tree)
  (left-avl-tree (binary-tree-left tree)
                 (binary-tree-value tree)
                 (binary-tree-right tree)))

(defun avl-tree-balance (tree)
  (balance-avl-tree (binary-tree-left tree)
                    (binary-tree-value tree)
                    (binary-tree-right tree)))

(defun balance-avl-tree (left value right)
                                        ;(format t "~&Balance-avl-tree~&")
  (labels ((height (tree) (if tree (avl-tree-height tree) 0)))
    (let ((d (- (height right) (height left))))
      (cond
        ;; just right
        ((or (= 0 d)
             (= -1 d)
             (= 1 d))
         (make-avl-tree left value right))
        ;; left too tall
        ((= -2 d)
         (let ((d (- (height (binary-tree-right left))
                     (height (binary-tree-left left)))))
           (cond
             ((= 1 d)
              (right-avl-tree (avl-tree-rotate-left left)
                              value
                              right))
             ((= -1 d)
              (right-avl-tree left value right))
             (t
              (avl-tree-balance (right-avl-tree left value right))))))
        ;; right too tall
        ((= 2 d)
         (let ((d (- (height (binary-tree-right right))
                     (height (binary-tree-left right)))))
           (cond
             ((= 1 d)
              (left-avl-tree left value right))
             ((= -1 d)
              (left-avl-tree left value (avl-tree-rotate-right right)))
             (t
              (avl-tree-balance (left-avl-tree left value right))))))
        ;; left much too tall
        ((> -2 d)
         (balance-avl-tree (binary-tree-left left)
                           (binary-tree-value left)
                           (balance-avl-tree (binary-tree-right left)
                                             value
                                             right)))
        ;; right much too tall
        ((< 2 d)
         (balance-avl-tree (balance-avl-tree left value (binary-tree-left right))
                           (binary-tree-value right)
                           (binary-tree-right right)))
        (t (error "Unbalanceble tree: ~A ~A" left right))))))


(defmacro with-avl-tree-compare ((value tree compare)
                                 null-case less-case equal-case greater-case)
  "Compare VALUE to value of TREE and execute the corresponding case."
  (alexandria:with-gensyms (c tree-sym)
    `(let ((,tree-sym ,tree))
       (if (null ,tree-sym)
           ,null-case
           (let ((,c (funcall ,compare ,value (binary-tree-value ,tree-sym))))
             (cond
               ((< ,c 0) ,less-case)
               ((> ,c 0) ,greater-case)
               (t ,equal-case)))))))

(defun avl-tree-insert (tree value compare)
  "Insert VALUE into TREE, returning new tree."
  (with-avl-tree-compare (value tree compare)
    (make-avl-tree nil value nil)
    (balance-avl-tree (avl-tree-insert (avl-tree-left tree) value compare)
                      (binary-tree-value tree)
                      (binary-tree-right tree))
    tree
    (balance-avl-tree (binary-tree-left tree)
                      (binary-tree-value tree)
                      (avl-tree-insert (avl-tree-right tree) value compare))))


(defun avl-tree-remove-min (tree)
  "Insert minimum element of TREE, returning new tree."
  (let ((left (binary-tree-left tree)))
    (if left
        (balance-avl-tree (avl-tree-remove-min left)
                          (binary-tree-value tree)
                          (binary-tree-right tree))
        (binary-tree-right tree))))

(defun avl-tree-concatenate (tree-1 tree-2)
  "Concatenate TREE-1 and TREE-2."
  (cond
    ((null tree-1) tree-2)
    ((null tree-2) tree-1)
    (t (balance-avl-tree tree-1 (binary-tree-min tree-2) (avl-tree-remove-min tree-2)))))

(defun avl-tree-split (tree x compare)
  (with-avl-tree-compare (x tree compare)
    (values nil nil nil)
    (multiple-value-bind (left-left present right-left)
        (avl-tree-split (binary-tree-left tree) x compare)
      (values left-left present (balance-avl-tree right-left
                                                  (binary-tree-value tree)
                                                  (binary-tree-right tree))))
    (values (binary-tree-left tree) t (binary-tree-right tree))
    (multiple-value-bind (left-right present right-right)
        (avl-tree-split (binary-tree-right tree) x compare)
      (values (balance-avl-tree (binary-tree-left tree)
                                (binary-tree-value tree)
                                left-right)
              present
              right-right))))

(defun avl-tree-remove (tree x compare)
  "Remove X from TREE, returning new tree."
  (with-avl-tree-compare (x tree compare)
    nil
    (balance-avl-tree (avl-tree-remove (avl-tree-left tree) x compare)
                      (binary-tree-value tree)
                      (binary-tree-right tree))
    (avl-tree-concatenate (avl-tree-left tree)
                          (avl-tree-right tree))
    (balance-avl-tree (avl-tree-left tree)
                      (avl-tree-value tree)
                      (avl-tree-remove (avl-tree-right tree) x compare))))


;;;;;;;;;;;;;;;
;; RED-BLACK ;;
;;;;;;;;;;;;;;;

;; Based on Chris Okasaki's Functional red-black trees
;; (defstruct (red-black
;;              (:include binary-tree)
;;              (:constructor make-red-black (red left value right)))
;;   (red nil :type boolean))

;; (defun red-black-redp (tree)
;;   (red-black-red tree))

;; (defun red-black-blackp (tree)
;;   (not (red-black-redp tree)))

;; (defun red-black-color (red tree)
;;   (make-red-black red
;;                   (red-black-left tree)
;;                   (red-black-value tree)
;;                   (red-black-right tree)))

;; (defun balance-red-black (red left value right)
;;   (labels ((when-red (tree)
;;              (when (red-black-p tree) (red-black-redp tree)))
;;            (balanced-tree (a x b y c z d)
;;              ;(declare (type red-black a b c d))
;;              (make-red-black t
;;                              (make-red-black nil a x b)
;;                              y
;;                              (make-red-black nil c z d))))
;;     (let* ((b (null red))
;;            (l (when-red left))
;;            (r (when-red right))
;;            (ll (when-red (when (red-black-p left)  (red-black-left left ))))
;;            (lr (when-red (when (red-black-p left)  (red-black-right left))))
;;            (rl (when-red (when (red-black-p right) (red-black-left right))))
;;            (rr (when-red (when (red-black-p right) (red-black-right right)))))
;;       (declare (type boolean b l r ll lr rl rr))
;;       (cond
;;         ((and b l ll)
;;          (balanced-tree (binary-tree-left-left left)
;;                         (binary-tree-value-left left)
;;                         (binary-tree-right-left left)
;;                         (binary-tree-value left)
;;                         (binary-tree-right left)
;;                         value
;;                         right))
;;         ((and b l lr)
;;          (balanced-tree (binary-tree-left left)
;;                         (binary-tree-value left)
;;                         (binary-tree-left-right left)
;;                         (binary-tree-value-right left)
;;                         (binary-tree-right-right left)
;;                         value
;;                         right ))
;;         ((and b r rl)
;;          (balanced-tree left
;;                         value
;;                         (binary-tree-left-left right)
;;                         (binary-tree-value-left right)
;;                         (binary-tree-right-left right)
;;                         (binary-tree-value right)
;;                         (binary-tree-right right)))
;;         ((and b r rr)
;;          (balanced-tree left
;;                         value
;;                         (binary-tree-left right)
;;                         (binary-tree-value right)
;;                         (binary-tree-left-right right)
;;                         (binary-tree-value-right right)
;;                         (binary-tree-right-right right)))
;;         (t
;;          (make-red-black red left value right))))))

;; (defun red-black-insert (value tree compare test)
;;   (labels ((ins (tree)
;;              (cond
;;                ((null tree) (make-red-black t nil value nil))
;;                ((funcall compare value (red-black-value tree))
;;                 (balance-red-black (red-black-red tree)
;;                                    (ins (red-black-left tree))
;;                                    (red-black-value tree)
;;                                    (red-black-right tree)))
;;                ((funcall test value (red-black-value tree))
;;                 tree)
;;                (t (balance-red-black (red-black-red tree)
;;                                      (red-black-left tree)
;;                                      (red-black-value tree)
;;                                      (ins (red-black-right tree)))))))
;;     (red-black-color nil (ins tree))))
