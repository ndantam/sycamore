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


(defun map-tree-inorder-nil (function tree)
  (when tree
    (map-tree-inorder-nil function (binary-tree-left tree))
    (funcall function (binary-tree-value tree))
    (map-tree-inorder-nil function (binary-tree-right tree))))

(defun map-tree-inorder-list (function tree)
  (when tree
    (nconc (map-tree-list function (binary-tree-left tree))
           (cons (funcall function (binary-tree-value tree))
                 (map-tree-list function (binary-tree-right tree))))))

(defun map-tree-inorder (result-type function tree)
  (cond
    ((null result-type)
     (map-tree-inorder-nil function tree))
    ((eq 'list result-type)
     (map-tree-inorder-list function tree))
    (t (error "Unknown result-type: ~A" result-type))))

(defun tree-search-node (value tree less-function eq-function)
  (cond
    ((null tree)
     nil)
    ((funcall less-function value (binary-tree-value tree))
     (tree-search-node value (binary-tree-left tree) less-function eq-function))
    ((funcall eq-function value (binary-tree-value tree))
     tree)
    (t
     (tree-search-node value (binary-tree-right tree) less-function eq-function))))


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
                               (format s "~&  ~A[label=\"~A\"];~&"
                                       x (if tree
                                             (binary-tree-value tree)
                                             nil))
                               (when parent
                                 (format s "~&  ~A -> ~A;~&"
                                         parent x))
                               (when tree
                                   (helper x (binary-tree-left tree))
                                   (helper x (binary-tree-right tree))))))
                    (format s "~&digraph {  ~&")
                    (helper nil tree)
                    (format s "~&}~&"))))))

;;;;;;;;;;;;;;;
;; RED-BLACK ;;
;;;;;;;;;;;;;;;

;; Based on Chris Okasaki's Function red-black trees
(defstruct (red-black
             (:include binary-tree)
             (:constructor make-red-black (red left value right)))
  (red nil :type boolean))

(defun red-black-redp (tree)
  (red-black-red tree))

(defun red-black-blackp (tree)
  (not (red-black-redp tree)))

(defun red-black-color (red tree)
  (make-red-black red
                  (red-black-left tree)
                  (red-black-value tree)
                  (red-black-right tree)))

(defun balance-red-black (red left value right)
  (labels ((when-red (tree)
             (when (red-black-p tree) (red-black-redp tree)))
           (balanced-tree (a x b y c z d)
             ;(declare (type red-black a b c d))
             (make-red-black t
                             (make-red-black nil a x b)
                             y
                             (make-red-black nil c z d))))
    (let* ((b (null red))
           (l (when-red left))
           (r (when-red right))
           (ll (when-red (when (red-black-p left)  (red-black-left left ))))
           (lr (when-red (when (red-black-p left)  (red-black-right left))))
           (rl (when-red (when (red-black-p right) (red-black-left right))))
           (rr (when-red (when (red-black-p right) (red-black-right right)))))
      (declare (type boolean b l r ll lr rl rr))
      (cond
        ((and b l ll)
         (balanced-tree (binary-tree-left-left left)
                        (binary-tree-value-left left)
                        (binary-tree-right-left left)
                        (binary-tree-value left)
                        (binary-tree-right left)
                        value
                        right))
        ((and b l lr)
         (balanced-tree (binary-tree-left left)
                        (binary-tree-value left)
                        (binary-tree-left-right left)
                        (binary-tree-value-right left)
                        (binary-tree-right-right left)
                        value
                        right ))
        ((and b r rl)
         (balanced-tree left
                        value
                        (binary-tree-left-left right)
                        (binary-tree-value-left right)
                        (binary-tree-right-left right)
                        (binary-tree-value right)
                        (binary-tree-right right)))
        ((and b r rr)
         (balanced-tree left
                        value
                        (binary-tree-left right)
                        (binary-tree-value right)
                        (binary-tree-left-right right)
                        (binary-tree-value-right right)
                        (binary-tree-right-right right)))
        (t
         (make-red-black red left value right))))))

(defun red-black-insert (value tree less-function eq-function)
  (labels ((ins (tree)
             (cond
               ((null tree) (make-red-black t nil value nil))
               ((funcall less-function value (red-black-value tree))
                (red-black-balance (red-black-red tree)
                                   (ins (red-black-left tree))
                                   (red-black-value tree)
                                   (red-black-right tree)))
               ((funcall eq-function value (red-black-value tree))
                tree)
               (t (red-black-balance (red-black-red tree)
                                     (red-black-left tree)
                                     (red-black-value tree)
                                     (ins (red-black-right tree)))))))
    (red-black-color nil (ins tree))))


;;;;;;;;;;;;;;;
;; AVL       ;;
;;;;;;;;;;;;;;;

(defstruct (avl
             (:include binary-tree)
             (:constructor %make-avl (height left value right)))
  (height 0 :type fixnum))

(defun make-avl (left value right)
  (%make-avl (1+ (max (if left (avl-height left) 0)
                      (if right (avl-height right) 0)))
             left value right))


(defun right-avl (left value right)
  (make-avl (binary-tree-left left)
            (binary-tree-value left)
            (make-avl (binary-tree-right left)
                      value
                      right)))

(defun avl-rotate-right (tree)
  (right-avl (binary-tree-left tree)
             (binary-tree-value tree)
             (binary-tree-right tree)))

(defun left-avl (left value right)
  (make-avl (make-avl left
                      value
                      (binary-tree-left right))
            (binary-tree-value right)
            (binary-tree-right right)))

(defun avl-rotate-left (tree)
  (left-avl (binary-tree-left tree)
            (binary-tree-value tree)
            (binary-tree-right tree)))

(defun avl-balance (tree)
  (balance-avl (binary-tree-left tree)
               (binary-tree-value tree)
               (binary-tree-right tree)))

(defun balance-avl (left value right)
  ;(format t "~&Balance-avl~&")
  (labels ((height (tree) (if tree (avl-height tree) 0)))
    (let ((d (- (height right) (height left))))
      (cond
        ;; just right
        ((or (= 0 d)
             (= -1 d)
             (= 1 d))
         (make-avl left value right))
        ;; left too tall
        ((= -2 d)
         (let ((d (- (height (binary-tree-right left))
                     (height (binary-tree-left left)))))
           (cond
             ((= 1 d)
              (right-avl (avl-rotate-left left)
                         value
                         right))
             ((= -1 d)
              (right-avl left value right))
             (t
              (avl-balance (right-avl left value right))))))
        ;; right too tall
         ((= 2 d)
          (let ((d (- (height (binary-tree-right right))
                      (height (binary-tree-left right)))))
            (cond
              ((= 1 d)
               (left-avl left value right))
              ((= -1 d)
               (left-avl left value (avl-rotate-right right)))
              (t
               (avl-balance (left-avl left value right))))))
         ;; left much too tall
         ((> -2 d) ;; rotate right
          (let ((x (avl-balance (right-avl left value right))))
            (balance-avl (avl-balance (binary-tree-left x))
                         (binary-tree-value x)
                         (binary-tree-right x))))
         ;; right much too tall
         ((< 2 d)
          (let ((x (avl-balance (left-avl left value right))))
            (balance-avl (avl-balance (binary-tree-left x))
                         (binary-tree-value x)
                         (binary-tree-right x))))
         (t (error "Unbalanceble tree: ~A ~A" left right))))))


(defun avl-insert (value tree less-function eq-function)
  (cond
    ((null tree)
     (make-avl nil value nil))
    ((funcall less-function value (binary-tree-value tree))
     (balance-avl (avl-insert value (avl-left tree) less-function eq-function)
                  (binary-tree-value tree)
                  (binary-tree-right tree)))
    ((funcall eq-function value (binary-tree-value tree))
     tree)
    (t
     (balance-avl (binary-tree-left tree)
                  (binary-tree-value tree)
                  (avl-insert value (avl-right tree) less-function eq-function)))))

(defun avl-insert-blind (value tree less-function eq-function)
  (cond
    ((null tree)
     (make-avl nil value nil))
    ((funcall less-function value (binary-tree-value tree))
     (make-avl (avl-insert value (avl-left tree) less-function eq-function)
               (binary-tree-value tree)
               (binary-tree-right tree)))
    ((funcall eq-function value (binary-tree-value tree))
     tree)
    (t
     (make-avl (binary-tree-left tree)
               (binary-tree-value tree)
               (avl-insert value (avl-right tree) less-function eq-function)))))
