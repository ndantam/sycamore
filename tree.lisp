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

;;(declaim (optimize (speed 3) (safety 0)))

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


;; (defun array-tree-insert-split (array value compare)
;;   (let* ((n (length array))
;;          (n/2 (ash n -1)))
;;          (i (array-tree-position value array compare)))
;;     (if (and i (< i n/2))
;;         (values (array-tree-insert value array compare 0 n/2 i)
;;                 (subseq array n/2))
;;         (values (subseq array 0 n/2)
;;                 (array-tree-insert value array compare n/2 n i)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; BASIC BINARY TREES ;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (binary-tree (:constructor make-binary-tree (left value right)))
  left
  value
  right)

(defun map-binary-tree-inorder (function tree)
  (declare (type function function))
  (when tree
    (map-binary-tree-inorder function (binary-tree-left tree))
    (funcall function (binary-tree-value tree))
    (map-binary-tree-inorder function (binary-tree-right tree))))

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
                         tree)))

(defun binary-tree-search-node (tree value compare)
  (declare (type function compare))
  "Return the node of TREE containing VALUE or NIL of not present."
  (labels ((rec (tree)
             (when tree
               (let ((c (funcall compare value (binary-tree-value tree))))
                 (cond ((< c 0) (rec (binary-tree-left tree)))
                       ((> c 0) (rec (binary-tree-right tree)))
                       (t tree))))))
    (rec tree)))

;; (do* ((c 1 (funcall compare value (binary-tree-value tree-1)))
;;         (tree-1 tree
;;                 (cond ((< c 0) (binary-tree-left tree-1))
;;                       ((> c 0) (binary-tree-right tree-1))
;;                       (t tree-1))))
;;        ((or (zerop c) (null tree-1)) tree-1))

(defun binary-tree-member-p (tree value compare)
  (when (binary-tree-search-node tree value compare)
    t))

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

(defun binary-tree-subset (tree-1 tree-2 compare)
  (declare (type function compare))
  (labels ((rec (tree-1 tree-2)
             (cond
               ((null tree-1) t)
               ((null tree-2) nil)
               (t
                (let ((c (funcall compare (binary-tree-value tree-1) (binary-tree-value tree-2))))
                  (declare (type fixnum c))
                  (cond
                    ((< c 0) ; v1 < v2
                     (and (rec (make-binary-tree (binary-tree-left tree-1)
                                                 (binary-tree-value tree-1)
                                                 nil)
                               (binary-tree-left tree-2))
                          (rec (binary-tree-right tree-1)
                               tree-2)))
                    ((> c 0) ; v1 > v2
                     (and (rec (make-binary-tree nil
                                                 (binary-tree-value tree-1)
                                                 (binary-tree-right tree-1))
                               (binary-tree-right tree-2))
                          (rec (binary-tree-left tree-1)
                               tree-2)))
                    (t (and (rec (binary-tree-left tree-1)
                                 (binary-tree-left tree-2))
                            (rec (binary-tree-right tree-1)
                                 (binary-tree-right tree-2))))))))))
    (rec tree-1 tree-2)))



(defun binary-tree-equal (tree-1 tree-2 compare)
  (declare (type function compare))
  ;; O(log(n)) space, O(min(m,n)) time
  (let ((stack))
    (labels ((push-left (k)
               (do ((x k (binary-tree-left x)))
                   ((null x))
                 (push x stack))))
      (push-left tree-1)
      (map-binary-tree-inorder (lambda (x)
                                 (if (or (null stack)
                                         (not (zerop (funcall compare x (binary-tree-value (car stack))))))
                                     (return-from binary-tree-equal
                                       nil)
                                     (push-left (binary-tree-right (pop stack)))))
                               tree-2))
    (not stack)))


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

;;;;;;;;;;;
;;  AVL  ;;
;;;;;;;;;;;

;; SEE: Adams, Stephen. Implementing Sets Efficiantly in a Functional Language

;; TODO: use weights instead of heights.  More useful for indexing and
;; balancing, and we're already paying the full slot for height anyway

;(defconstant +avl-tree-max-array-length+ 16)
;(defparameter +avl-tree-rebalance-log+ 2)  ;; power two difference for rebalancing

(defstruct (avl-tree
             (:include binary-tree)
             (:constructor %make-avl-tree (weight left value right)))
  (weight 0 :type fixnum))

(defun avl-tree-count (tree)
  (etypecase tree
    (avl-tree (avl-tree-weight tree))
    (simple-vector (length tree))
    (null 0)))


(defun make-avl-tree (left value right)
  (%make-avl-tree (the fixnum (+ 1
                                 (avl-tree-count left)
                                 (avl-tree-count right)))
                  left value right))

(defmacro with-avl-tree((left value right &optional weight) tree &body body)
  (alexandria:with-gensyms (tree-sym)
    `(let ((,tree-sym ,tree))
       (let ((,left (binary-tree-left ,tree-sym))
             (,value (binary-tree-value ,tree-sym))
             (,right (binary-tree-right ,tree-sym))
             ,@(when weight `(,weight (avl-tree-weight ,tree-sym))))
         ,@body))))

(defun right-avl-tree (constructor left value right)
  "Right rotation"
  (declare (type function constructor))
  (funcall constructor
           (binary-tree-left left)
           (binary-tree-value left)
           (funcall constructor
                    (binary-tree-right left)
                    value
                    right)))

(defun left-avl-tree (constructor left value right)
  "Left rotation"
  (declare (type function constructor))
  (funcall constructor
           (funcall constructor
                    left
                    value
                    (binary-tree-left right))
           (binary-tree-value right)
           (binary-tree-right right)))

(defun left-right-avl-tree (constructor left value right)
  "Right rotation then left rotation"
  (declare (type function constructor))
  (funcall constructor
           (funcall constructor left
                    value
                    (binary-tree-left-left right))
           (binary-tree-value-left right)
           (funcall constructor
                    (binary-tree-right-left right)
                    (binary-tree-value right)
                    (binary-tree-right right))))

(defun right-left-avl-tree (constructor left value right)
  "Left rotation then right rotation"
  (declare (type function constructor))
  (funcall constructor
           (funcall constructor
                    (binary-tree-left left)
                    (binary-tree-value left)
                    (binary-tree-left-right left))
           (binary-tree-value-right left)
           (funcall constructor
                    (binary-tree-right-right left)
                    value
                    right)))

(defun balance-general-avl-tree (constructor rebalance-log left value right)
  (declare (type function constructor))
  ;;(format t "~&Balance-avl-tree~&")
  (let ((w-l (avl-tree-count left))
        (w-r (avl-tree-count right)))
    (cond
      ;; left too tall
      ((> w-l (ash w-r rebalance-log))
       (if (> (avl-tree-count (binary-tree-right left))
              (avl-tree-count (binary-tree-left left)))
           (right-left-avl-tree constructor left value right)
           (right-avl-tree constructor left value right)))
      ;; right too tall
      ((> w-r (ash w-l rebalance-log))
       (if (< (avl-tree-count (binary-tree-right right))
              (avl-tree-count (binary-tree-left right)))
           (left-right-avl-tree constructor left value right)
           (left-avl-tree constructor left value right)))
      ;; close enough
      (t
       (funcall constructor left value right)))))

(defun balance-avl-tree (left value right)
  (balance-general-avl-tree #'make-avl-tree 2 left value right))

(defun avl-tree-smaller (tree-1 tree-2)
  "Is `tree-1' shorter than `tree-2'?"
  (cond
    ((null tree-2) nil)
    ((null tree-1) t)
    (t (< (avl-tree-count tree-1)
          (avl-tree-count tree-2)))))

(defmacro cond-avl-tree-compare ((value tree compare)
                                 null-case less-case equal-case greater-case)
  "Compare VALUE to value of TREE and execute the corresponding case."
  (alexandria:with-gensyms (c tree-sym)
    `(let ((,tree-sym ,tree))
       (if (null ,tree-sym)
           ,null-case
           (let ((,c (funcall ,compare ,value (binary-tree-value ,tree-sym))))
             (declare (type fixnum ,c))
             (cond
               ((< ,c 0) ,less-case)
               ((> ,c 0) ,greater-case)
               (t ,equal-case)))))))

(defmacro cond-avl-tree-heights ((left right)
                                 null-left-case null-right-case left-short-case equiv-case right-short-case)
  (alexandria:with-gensyms (left-sym right-sym)
    `(let ((,left-sym ,left)
           (,right-sym ,right))
       (cond
         ((null ,left-sym)
          ,null-left-case)
         ((null ,right-sym)
          ,null-right-case)
         ((< (avl-tree-count ,left-sym)
             (ash (avl-tree-count ,right-sym) 2)) ;; FIXME
          ,left-short-case)
         ((< (avl-tree-count ,right-sym)
             (ash (avl-tree-count ,left-sym) 2)) ;; FIXME
          ,right-short-case)
         (t ,equiv-case)))))

(defun avl-tree-insert (tree value compare)
  "Insert VALUE into TREE, returning new tree."
  (declare (type function compare))
  (cond-avl-tree-compare (value tree compare)
    (make-avl-tree nil value nil)
    (balance-avl-tree (avl-tree-insert (avl-tree-left tree) value compare)
                      (binary-tree-value tree)
                      (binary-tree-right tree))
    tree
    (balance-avl-tree (binary-tree-left tree)
                      (binary-tree-value tree)
                      (avl-tree-insert (avl-tree-right tree) value compare))))


(defun avl-tree-builder (compare)
  (lambda (tree value) (avl-tree-insert tree value compare)))


(defun avl-tree-remove-min (tree)
  "Insert minimum element of TREE, returning new tree."
  (let ((left (binary-tree-left tree)))
    (if left
        (balance-avl-tree (avl-tree-remove-min left)
                          (binary-tree-value tree)
                          (binary-tree-right tree))
        (binary-tree-right tree))))

(defun avl-tree-remove-max (tree)
  "Insert minimum element of TREE, returning new tree."
  (let ((right (binary-tree-right tree)))
    (if right
        (balance-avl-tree (binary-tree-left tree)
                          (binary-tree-value tree)
                          (avl-tree-remove-max (binary-tree-right tree)))
        (binary-tree-left tree))))


(defun join-avl-tree (left value right compare)
  (cond-avl-tree-heights (left right)
                         (avl-tree-insert right value compare)
                         (avl-tree-insert left value compare)
                         (balance-avl-tree (join-avl-tree left value (binary-tree-left right) compare)
                                           (binary-tree-value right)
                                           (binary-tree-right right))
                         (balance-avl-tree left value right)
                         (balance-avl-tree (binary-tree-left left)
                                           (binary-tree-value left)
                                           (join-avl-tree (binary-tree-right left) value right compare))))

(defun avl-tree-concatenate (tree-1 tree-2)
  "Concatenate TREE-1 and TREE-2."
  (cond-avl-tree-heights (tree-1 tree-2)
                         tree-2
                         tree-1
                         (balance-avl-tree (avl-tree-concatenate tree-1 (binary-tree-left tree-2))
                           (binary-tree-value tree-2)
                           (binary-tree-right tree-2))
                         (balance-avl-tree tree-1 (binary-tree-min tree-2) (avl-tree-remove-min tree-2))
                         (balance-avl-tree (binary-tree-left tree-1)
                                           (binary-tree-value tree-1)
                                           (avl-tree-concatenate (binary-tree-right tree-1) tree-2))))

(defun avl-tree-split (tree x compare)
  (declare (type function compare))
  (cond-avl-tree-compare (x tree compare)
    (values nil nil nil)
    (multiple-value-bind (left-left present right-left)
        (avl-tree-split (binary-tree-left tree) x compare)
      (values left-left present (join-avl-tree right-left
                                               (binary-tree-value tree)
                                               (binary-tree-right tree)
                                               compare)))
    (values (binary-tree-left tree) t (binary-tree-right tree))
    (multiple-value-bind (left-right present right-right)
        (avl-tree-split (binary-tree-right tree) x compare)
      (values (join-avl-tree (binary-tree-left tree)
                             (binary-tree-value tree)
                             left-right
                             compare)
              present
              right-right))))


(defun avl-tree-remove (tree x compare)
  "Remove X from TREE, returning new tree."
  (declare (type function compare))
  (cond-avl-tree-compare (x tree compare)
    nil
    (balance-avl-tree (avl-tree-remove (avl-tree-left tree) x compare)
                      (binary-tree-value tree)
                      (binary-tree-right tree))
    (avl-tree-concatenate (avl-tree-left tree)
                          (avl-tree-right tree))
    (balance-avl-tree (avl-tree-left tree)
                      (avl-tree-value tree)
                      (avl-tree-remove (avl-tree-right tree) x compare))))


(defun avl-tree-trim (tree lo hi compare)
  "Return subtree rooted between `lo' and `hi'."
  (declare (type function compare))
  ;(declare (optimize (speed 3) (safety 0)))
  (cond
    ((null tree) nil)
    ((< (the fixnum (funcall compare (binary-tree-value tree) lo)) 0)
     (avl-tree-trim (binary-tree-right tree) lo hi compare))
    ((< (the fixnum (funcall compare hi (binary-tree-value tree))) 0)
     (avl-tree-trim (binary-tree-left tree) lo hi compare))
    (t tree)))

;; root between lo and +infinity
(defun avl-tree-trim-lo (tree lo compare)
  (declare (type function compare))
  (cond
    ((null tree) nil)
    ((< (funcall compare lo (binary-tree-value tree)) 0)
     tree)
    (t (avl-tree-trim-lo (avl-tree-right tree) lo compare))))


;; root between -infinity and hi
(defun avl-tree-trim-hi (tree hi compare)
  (declare (type function compare))
  (cond
    ((null tree) nil)
    ((> (funcall compare hi (binary-tree-value tree)) 0)
     tree)
    (t (avl-tree-trim-hi (avl-tree-left tree) hi compare))))



(defun avl-tree-split-less (tree x compare)
  "Everything in tree before than x"
  (declare (type function compare))
  (cond-avl-tree-compare (x tree compare)
                         nil
                         (avl-tree-split-less (binary-tree-left tree) x compare)
                         (binary-tree-left tree)
                         (join-avl-tree (binary-tree-left tree)
                                        (binary-tree-value tree)
                                        (avl-tree-split-less (binary-tree-right tree) x compare)
                                        compare)))


(defun avl-tree-split-greater (tree x compare)
  "Everything in tree after than x"
  (declare (type function compare))
  ;(declare (optimize (speed 3) (safety 0)))
  (cond-avl-tree-compare (x tree compare)
                         nil
                         (join-avl-tree (avl-tree-split-greater (binary-tree-left tree) x compare)
                                        (binary-tree-value tree)
                                        (binary-tree-right tree)
                                        compare)
                         (binary-tree-right tree)
                         (avl-tree-split-greater (binary-tree-right tree) x compare)))


;; tree-2 rooted between lo and hi
(defun avl-tree-uni-bd (tree-1 tree-2 lo hi compare)
  (declare (type function compare))
  (let ((tree-2 (avl-tree-trim tree-2 lo hi compare)))
    (cond
      ((null tree-2) tree-1)
      ((null tree-1)
       (join-avl-tree (avl-tree-split-greater (avl-tree-left tree-2) lo compare)
                      (avl-tree-value tree-2)
                      (avl-tree-split-less (avl-tree-right tree-2) hi compare)
                      compare))
      (t (join-avl-tree (avl-tree-uni-bd (avl-tree-left tree-1)
                                         tree-2 lo (avl-tree-value tree-1) compare)
                        (avl-tree-value tree-1)
                        (avl-tree-uni-bd (avl-tree-right tree-1)
                                         tree-2 (avl-tree-value tree-1) hi compare)
                        compare)))))

;; tree-2 between -inf and hi
(defun avl-tree-uni-hi (tree-1 tree-2 hi compare)
  (let ((tree-2 (avl-tree-trim-hi tree-2 hi compare)))
    (cond
      ((null tree-2) tree-1)
      ((null tree-1) (avl-tree-split-less tree-2 hi compare))
      (t (join-avl-tree (avl-tree-uni-hi (avl-tree-left tree-1) tree-2 (avl-tree-value tree-1) compare)
                        (avl-tree-value tree-1)
                        (avl-tree-uni-bd (avl-tree-right tree-1) tree-2 (avl-tree-value tree-1) hi compare)
                        compare)))))

;; tree-2 between lo and +inf
(defun avl-tree-uni-lo (tree-1 tree-2 lo compare)
  (let ((tree-2 (avl-tree-trim-lo tree-2 lo compare)))
    (cond
      ((null tree-2) tree-1)
      ((null tree-1) (avl-tree-split-greater tree-2 lo compare))
      (t (join-avl-tree (avl-tree-uni-bd (avl-tree-left tree-1) tree-2 lo (avl-tree-value tree-1) compare)
                        (avl-tree-value tree-1)
                        (avl-tree-uni-lo (avl-tree-right tree-1) tree-2 (avl-tree-value tree-1) compare)
                        compare)))))

(defun avl-tree-hedge-union (tree-1 tree-2 compare)
  (declare (type function compare))
  (cond
    ((null tree-1) tree-2)
    ((null tree-2) tree-1)
    (t (with-avl-tree (l1 v1 r1) tree-1
         (join-avl-tree (avl-tree-uni-hi l1 tree-2 v1 compare)
                        v1
                        (avl-tree-uni-lo r1 tree-2 v1 compare)
                        compare)))))


(defun avl-tree-union (tree-1 tree-2 compare)
  (declare (type function compare))
  (cond
    ((null tree-1) tree-2)
    ((null tree-2) tree-1)
    ((= 1 (avl-tree-count tree-2))
     (avl-tree-insert tree-1 (avl-tree-value tree-2) compare))
    ((= 1 (avl-tree-count tree-1))
     (avl-tree-insert tree-2 (avl-tree-value tree-1) compare))
    ((>= (avl-tree-count tree-2)
         (avl-tree-count tree-1))
     (multiple-value-bind (left-2 p-2 right-2) (avl-tree-split tree-2 (avl-tree-value tree-1) compare)
       (declare (ignore p-2))
       (join-avl-tree (avl-tree-union (binary-tree-left tree-1) left-2 compare)
                      (avl-tree-value tree-1)
                      (avl-tree-union (binary-tree-right tree-1) right-2 compare)
                      compare)))
    (t
     (multiple-value-bind (left-1 p-1 right-1) (avl-tree-split tree-1 (avl-tree-value tree-2) compare)
       (declare (ignore p-1))
       (join-avl-tree (avl-tree-union left-1 (binary-tree-left tree-2) compare)
                      (avl-tree-value tree-2)
                      (avl-tree-union right-1 (binary-tree-right tree-2) compare)
                      compare)))))

(defun avl-tree-intersection (tree-1 tree-2 compare)
  (cond
    ((or (null tree-1)
         (null tree-2))
     nil)
    ;; next two cases are a premature optimization
    ((= 1 (avl-tree-count tree-1))
     (when (binary-tree-search-node tree-2 (binary-tree-value tree-1) compare)
       (make-avl-tree nil (binary-tree-value tree-1) nil)))
    ((= 1 (avl-tree-count tree-2))
     (when (binary-tree-search-node tree-1 (binary-tree-value tree-2) compare)
       (make-avl-tree nil (binary-tree-value tree-2) nil)))
    ;; general case
    (t (multiple-value-bind (left-2 present right-2)
           (avl-tree-split tree-2 (avl-tree-value tree-1) compare)
         (let ((i-left (avl-tree-intersection (avl-tree-left tree-1) left-2 compare))
               (i-right (avl-tree-intersection (avl-tree-right tree-1) right-2 compare)))
           (if present
               (join-avl-tree i-left (avl-tree-value tree-1) i-right compare)
               (avl-tree-concatenate i-left i-right)))))))

(defun avl-tree-difference (tree-1 tree-2 compare)
  (declare (type function compare))
  (cond
    ((null tree-1) nil)
    ((null tree-2) tree-1)
    ;; next cases is a premature optimization
    ((= 1 (avl-tree-count tree-2))
     (avl-tree-remove tree-1 (binary-tree-value tree-2) compare))
    ;; general case
    (t (multiple-value-bind (left-2 present right-2)
           (avl-tree-split tree-2 (binary-tree-value tree-1) compare)
         (let ((left (avl-tree-difference (binary-tree-left tree-1) left-2 compare))
               (right (avl-tree-difference (binary-tree-right tree-1) right-2 compare)))
           (if present
               (avl-tree-concatenate left right)
               (join-avl-tree left (binary-tree-value tree-1) right compare)))))))

(defun avl-tree-dot (tree &key output)
  (output-dot output
              (lambda (s)
                (let ((i -1))
                  (labels ((helper (parent tree)
                             (let ((x (incf i)))
                               (format s "~&  ~A[label=\"~A (~D)\"~:[shape=none~;~]];~&"
                                       x (if tree
                                             (binary-tree-value tree)
                                             "")
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
                  (avl-tree-insert (tree-map-root tree-map)
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

(defstruct (tree-set (:constructor %make-tree-set (compare root)))
  compare
  root)

(defun make-tree-set (compare)
  "Create a new tree-set."
  (%make-tree-set compare nil))

(defun tree-set (compare &rest args)
  (%make-tree-set compare
                  (fold (lambda (tree x) (avl-tree-insert tree x compare))
                        nil
                        args)))

(defun map-tree-set (result-type function set)
  (map-binary-tree :inorder result-type function (tree-set-root set)))

(defmacro def-tree-set-item-op (name implementation-name)
  `(defun ,name (set item)
     (%make-tree-set (tree-set-compare set)
                     (,implementation-name (tree-set-root set)
                                           item
                                           (tree-set-compare set)))))

(def-tree-set-item-op tree-set-insert avl-tree-insert)
(def-tree-set-item-op tree-set-remove avl-tree-remove)

(defun tree-set-member-p (set item)
  (binary-tree-member-p (tree-set-root set) item (tree-set-compare set)))

(defmacro def-tree-set-binop (name implementation-name)
  `(defun ,name (set-1 set-2)
     (%make-tree-set (tree-set-compare set-1)
                     (,implementation-name (tree-set-root set-1)
                                           (tree-set-root set-2)
                                           (tree-set-compare set-1)))))

(def-tree-set-binop tree-set-union avl-tree-union)
(def-tree-set-binop tree-set-intersection avl-tree-intersection)
(def-tree-set-binop tree-set-difference avl-tree-difference)

(defun tree-set-equal (set-1 set-2)
  (binary-tree-equal (tree-set-root set-1)
                     (tree-set-root set-2)
                     (tree-set-compare set-1)))

(defun tree-set-subset (set-1 set-2)
  (binary-tree-subset (tree-set-root set-1)
                      (tree-set-root set-2)
                      (tree-set-compare set-1)))



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
