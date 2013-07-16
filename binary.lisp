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

;;(declaim (optimize (speed 3) (safety 0)))



;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC BINARY TREES ;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (binary-tree (:constructor make-binary-tree (left value right)))
  left
  value
  right)


(defun map-binary-tree-inorder (function tree)
  (declare (type function function))
  (etypecase tree
    (binary-tree
     (map-binary-tree-inorder function (binary-tree-left tree))
     (funcall function (binary-tree-value tree))
     (map-binary-tree-inorder function (binary-tree-right tree)))
    (simple-vector
     (dotimes (i (length tree))
       (funcall function (aref tree i))))
    (null nil)))

(defun map-binary-tree-preorder (function tree)
  (declare (type function function))
  (when tree
    (funcall function (binary-tree-value tree))
    (map-binary-tree-preorder function (binary-tree-left tree))
    (map-binary-tree-preorder function (binary-tree-right tree))))

(defun map-binary-tree-postorder (function tree)
  (declare (type function function))
  (when tree
    (map-binary-tree-postorder function (binary-tree-left tree))
    (map-binary-tree-postorder function (binary-tree-right tree))
    (funcall function (binary-tree-value tree))))

(defun map-binary-tree-nil (order function tree)
  (declare (type function function))
  (ecase order
    (:inorder (map-binary-tree-inorder function tree))
    (:postorder (map-binary-tree-postorder function tree))
    (:preorder (map-binary-tree-preorder function tree))))

(defun map-binary-tree-list (order function tree)
  (declare (type function function))
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
  (declare (type function function))
  (let ((v initial-value))
    (map-binary-tree-nil order
                         (lambda (x) (setq v (funcall function v x)))
                         tree)
    v))

(defun binary-tree-search-node (tree value compare)
  (declare (type function compare))
  "Return the node of TREE containing VALUE or NIL of not present."
  (labels ((rec (tree)
             (etypecase tree
               (binary-tree
                (let ((c (funcall compare value (binary-tree-value tree))))
                  (declare (type fixnum c))
                  (cond ((< c 0) (rec (binary-tree-left tree)))
                        ((> c 0) (rec (binary-tree-right tree)))
                       (t tree))))
               (simple-vector
                (when (array-tree-position tree value compare)
                  tree))
               (null nil))))
    (rec tree)))

;; (do* ((c 1 (funcall compare value (binary-tree-value tree-1)))
;;         (tree-1 tree
;;                 (cond ((< c 0) (binary-tree-left tree-1))
;;                       ((> c 0) (binary-tree-right tree-1))
;;                       (t tree-1))))
;;        ((or (zerop c) (null tree-1)) tree-1))

(defun binary-tree-find (tree value compare)
  (declare (type function compare))
  "Return the node of TREE containing VALUE or NIL of not present."
  (labels ((rec (tree)
             (etypecase tree
               (binary-tree
                (cond-compare (value (binary-tree-value tree) compare)
                   (rec (binary-tree-left tree))
                   (values (binary-tree-value tree) t)
                   (rec (binary-tree-right tree))))
               (simple-vector
                (let ((i (array-tree-position tree value compare)))
                  (if i
                      (values (aref tree i) t)
                      (values nil nil))))
               (null (values nil nil)))))
    (rec tree)))


(defun binary-tree-member-p (tree value compare)
  (multiple-value-bind (value present)
      (binary-tree-find tree value compare)
    (declare (ignore value))
    present))

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

(defun binary-tree-leaf-p (tree)
  (and (null (binary-tree-left tree))
       (null (binary-tree-right tree))))

(defun binary-tree-half-leaf-p (tree)
  (or (and (null (binary-tree-left tree))
           (binary-tree-right tree))
      (and (binary-tree-left tree)
           (null (binary-tree-right tree)))))



#+sbcl
(defun output-dot-file (program output function lang)
  "Run `dot' on the output of FUNCTION.
OUTPUT: output filename
FUNCTION: (lambda (stream)) => nil, prints dot on STREAM
LANG: language output for dot, (or pdf ps eps png)"
  (let ((p (sb-ext:run-program program (list (concatenate 'string "-T" lang))
                               :wait nil :search t :input :stream :output output
                               :if-output-exists :supersede)))
    (funcall function (sb-ext:process-input p))
    (close (sb-ext:process-input p))
    (sb-ext:process-wait p)
    (sb-ext:process-close p)))


(defun output-dot (output function &key
                   (program "dot")
                   (lang (and (stringp output) (car (last (ppcre:split "\\." output))))))
  "Produce graphiz output, dispatching on type of OUTPUT.
OUTPUT:  (or filename stream t nil)
FUNCTION: (lambda (stream)) => nil, prints dot text on STREAM
LANG: language output for dot, (or pdf ps eps png)"
  (cond
    ((streamp output)
     (funcall function output))
    ((null output)
     (with-output-to-string (s) (output-dot s function)))
    ((eq output t) (output-dot *standard-output* function))
    ((stringp output)
     (output-dot-file program output function lang))
    (t (error "Unknown output: ~A" output))))


(defun binary-tree-dot (tree &key output (node-label-function #'binary-tree-value))
  (output-dot output
              (lambda (s)
                (let ((i -1))
                  (labels ((helper (parent tree)
                             (let ((x (incf i)))
                               (etypecase tree
                                 (binary-tree (format s "~&  ~A[label=\"~A\"];~&"
                                               x (funcall node-label-function tree)))
                                 (null (format s "~&  ~A[label=\"nil\" shape=none];~&" x))
                                 (simple-vector
                                  (format s "~&  ~A[label=\"~{~A~^, ~}\",shape=box];~&"
                                          x (loop for k across tree collect k))))
                               (when parent
                                 (format s "~&  ~A -> ~A;~&"
                                         parent x))
                               (when (binary-tree-p tree)
                                 (helper x (binary-tree-left tree))
                                 (helper x (binary-tree-right tree))))))
                    (format s "~&digraph {  ~&")
                    (helper nil tree)
                    (format s "~&}~&"))))))

(defun binary-tree-min (tree)
  "Return minimum (leftmost) value of TREE."
  (etypecase tree
    (binary-tree
     (binary-tree-min (binary-tree-left tree)))
    (simple-vector (svref tree 0))
    (null nil)))

(defun binary-tree-max (tree)
  "Return maximum (rightmost) value of TREE."
  (etypecase tree
    (binary-tree
     (binary-tree-max (binary-tree-right tree)))
    (simple-vector (svref tree (1- (length tree))))
    (null nil)))


(defun binary-tree-count (tree)
  "Number of elements in TREE."
  (if tree
      (+ 1
         (binary-tree-count (binary-tree-left tree))
         (binary-tree-count (binary-tree-right tree)))
      0))


(declaim (ftype (function ((or array binary-tree) (or array binary-tree) function) fixnum) binary-tree-compare))
(defun binary-tree-compare (tree-1 tree-2 compare)
  (declare (type function compare))
  ;; O(log(n)) space, O(min(m,n)) time
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((stack (make-array 0;(ash (avl-tree-count tree-1) (- 0 +avl-tree-max-array-length+ 1))
                           :fill-pointer 0 :adjustable t))
        (i 0))
    (declare (type fixnum i))
    (labels ((push-left (k)
               (etypecase k
                 (binary-tree
                  (vector-push-extend k stack)
                  (push-left (binary-tree-left k)))
                 (simple-vector
                  (when (< 0 (length k)) (vector-push-extend k stack )))
                 (null)))
             (pop-val ()
               (let ((val (aref stack (1- (length stack)))))
                 (etypecase val
                   (binary-tree
                    (let ((tree (vector-pop stack)))
                      (push-left (binary-tree-right tree))
                      (binary-tree-value tree)))
                   (simple-vector
                    (prog1 (aref val i)
                      (incf i)
                      (when (>= i (length val))
                        (setq i 0)
                        (vector-pop stack))))))))
      (push-left tree-1)
      (map-binary-tree-inorder (lambda (y)
                                 (when (zerop (length stack)) ;; tree-1 was shorter
                                   (return-from binary-tree-compare 1))
                                 (let ((c (funcall compare (pop-val) y)))
                                   (declare (type fixnum c))
                                   (unless (zerop c)
                                     (return-from binary-tree-compare c))))
                               tree-2))
    (if (zerop (length stack))
        ;; equal sizes
        0
        ;; tree-1 taller
        -1)))


(defun binary-tree-equal (tree-1 tree-2 compare)
  (zerop (the fixnum (binary-tree-compare tree-1 tree-2 compare))))



  ;;   (labels ((collect-left (k list)
  ;;              (if k
  ;;                  (collect-left (binary-tree-left k) (cons k list))
  ;;                  list))
  ;;            (rec (tree list)
  ;;              (if (null tree)
  ;;                  list
  ;;                  (let ((list (rec (binary-tree-left tree) list))) ; left
  ;;                    (if (and list
  ;;                             (zerop (funcall compare (binary-tree-value tree)
  ;;                                             (binary-tree-value (car list))))) ; root
  ;;                        (rec (binary-tree-right tree) ;right
  ;;                             (collect-left (binary-tree-right (car list)) (cdr list)))
  ;;                        t)))))
  ;;     (not (rec tree-1 (collect-left tree-2 nil))))


(defun binary-tree-from-list (list)
  (when list
    (destructuring-bind (value &optional left right) list
      (make-binary-tree (binary-tree-from-list left)
                        value
                        (binary-tree-from-list right)))))

(defun binary-tree-every (predicate tree)
  (declare (type function predicate))
  (or (null tree)
      (and (funcall predicate (binary-tree-value tree))
           (binary-tree-every predicate (binary-tree-left tree))
           (binary-tree-every predicate (binary-tree-left tree)))))

(defun binary-tree-some (predicate tree)
  (declare (type function predicate))
  (and tree
       (or (funcall predicate (binary-tree-value tree))
           (binary-tree-some predicate (binary-tree-left tree))
           (binary-tree-some predicate (binary-tree-right tree)))))
