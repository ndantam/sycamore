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

;;;;;;;;;;;
;;  AVL  ;;
;;;;;;;;;;;

;; SEE: Adams, Stephen. Implementing Sets Efficiantly in a Functional Language

;; All leaf nodes are simple vectors.
;; We can assume a binary tree node will never have NULL left/right values

(defconstant +avl-tree-max-array-length+ 8)
(defparameter +avl-tree-rebalance-log+ 2)  ;; power two difference for rebalancing
(declaim (type (integer 2 2) +avl-tree-rebalance-log+))

(defstruct (avl-tree
             (:include binary-tree)
             (:constructor %make-avl-tree (weight left value right)))
  (weight 0 :type (integer 0 #.most-positive-fixnum)))

(declaim (ftype (function (t) (integer 0 #.most-positive-fixnum)) avl-tree-count))
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

(defmacro with-avl-tree ((left value right &optional count) tree &body body)
  (alexandria:with-gensyms (tree-sym l-sym v-sym r-sym)
    `(let ((,tree-sym ,tree))
       (multiple-value-bind (,left ,value ,right ,@(when count (list count)))
           (etypecase ,tree-sym
             (binary-tree (values (binary-tree-left ,tree-sym)
                                  (binary-tree-value ,tree-sym)
                                  (binary-tree-right ,tree-sym)
                                  ,@(when count `((avl-tree-weight ,tree-sym)))))
             (simple-vector (with-array-tree (,l-sym ,v-sym ,r-sym) ,tree-sym
                              (values ,l-sym ,v-sym ,r-sym
                                      ,@(when count `((length ,tree-sym)))))))
         ,@body))))


(defmacro with-avl-trees ((left1 value1 right1 &optional count1) tree1
                          (left2 value2 right2 &optional count2) tree2
                          &body body)
  `(with-avl-tree (,left1 ,value1 ,right1 ,count1) ,tree1
     (with-avl-tree (,left2 ,value2 ,right2 ,count2) ,tree2
         ,@body)))

(defun avl-tree-list (tree) (map-binary-tree :inorder 'list #'identity tree))

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

(defun balance-avl-tree-array-pair (left value right)
  (let* ((w-l (length left))
         (w-r (length right))
         (w-lr (+ w-l w-r))
         (n-a (ash w-lr -1))
         (new-left (make-array n-a))
         (new-value)
         (new-right (make-array (- w-lr n-a))))
    (replace new-left left)
    (cond
      ((< w-l n-a) ;; left smaller
       (setf (aref new-left w-l) value)                 ;; fill value into new-left
       (replace new-left right :start1 (+ w-l 1))       ;; fill right into new-left
       (let ((start (- n-a 1 w-l)))
         (setq new-value (aref right start))            ;; set new-value
         (replace new-right right :start2 (1+ start)))) ;; fill right
      ((> w-l n-a) ;; left bigger
       (setq new-value (aref left n-a))                 ;; set new-value
       (replace new-right left :start2 (+ n-a 1))       ;; fill new-right with rest of left
       (let ((start (- w-l n-a 1)))
         (setf (aref new-right start) value)
         (replace new-right right :start1 (1+ start)))) ;; fill rest of new-right with right
      (t ;equal, shouldn't actually happen
       (error "Why balance equal arrays?")))
    (make-avl-tree new-left new-value new-right)))

;; TODO: specialize this function based on adding/subtracting from left/right
(defun balance-general-avl-tree (constructor left value right)
  (declare (type function constructor))
  ;;(declare (optimize (speed 3) (safety 0)))
  ;;(format t "~&Balance-avl-tree~&")
  (let* ((w-l (avl-tree-count left))
         (w-r (avl-tree-count right))
         (w-t (+ w-l w-r 1)))
    (declare (type fixnum w-t))
    (cond
      ;; condense to one vector
      ((<= w-t +avl-tree-max-array-length+)
       (let ((i -1)
             (array (make-array w-t)))
         (declare (type fixnum i))
         (labels ((gather (x) (setf (aref array (incf i)) x)))
           (map-binary-tree :inorder nil #'gather left)
           (gather value)
           (map-binary-tree :inorder nil #'gather right))
         array))
      ;; left too tall
      ((> w-l (ash w-r +avl-tree-rebalance-log+))
       (etypecase left
         (avl-tree (if (and (> (avl-tree-count (binary-tree-right left))
                               (avl-tree-count (binary-tree-left left)))
                            (avl-tree-p (binary-tree-right left)))
                       (right-left-avl-tree constructor left value right)
                       (right-avl-tree constructor left value right)))
         (simple-vector (balance-avl-tree-array-pair left value right))))
      ;; right too tall
      ((> w-r (ash w-l +avl-tree-rebalance-log+))
       (etypecase right
         (avl-tree (if (and (< (avl-tree-count (binary-tree-right right))
                               (avl-tree-count (binary-tree-left right)))
                            (avl-tree-p (binary-tree-left right)))
                       (left-right-avl-tree constructor left value right)
                       (left-avl-tree constructor left value right)))
         (simple-vector (balance-avl-tree-array-pair left value right))))
      ;; close enough
      (t
       (funcall constructor left value right)))))

(defun balance-avl-tree (left value right)
  (balance-general-avl-tree #'make-avl-tree left value right))

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

(defmacro cond-avl-tree-vector-compare ((value tree compare)
                                        null-case vector-case less-case equal-case greater-case)
  "Compare VALUE to value of TREE and execute the corresponding case."
  (alexandria:with-gensyms (c tree-sym)
    `(let ((,tree-sym ,tree))
       (etypecase ,tree-sym
         (avl-tree
          (let ((,c (funcall ,compare ,value (binary-tree-value ,tree-sym))))
            (declare (type fixnum ,c))
            (cond
              ((< ,c 0) ,less-case)
              ((> ,c 0) ,greater-case)
              (t ,equal-case))))
         (simple-vector ,vector-case)
         (null ,null-case)))))


;; (defmacro cond-avl-tree-heights ((left right)
;;                                  null-left-case null-right-case left-short-case equiv-case right-short-case)
;;   (alexandria:with-gensyms (left-sym right-sym)
;;     `(let ((,left-sym ,left)
;;            (,right-sym ,right))
;;        (cond
;;          ((null ,left-sym)
;;           ,null-left-case)
;;          ((null ,right-sym)
;;           ,null-right-case)
;;          ((< (avl-tree-count ,left-sym)
;;              (ash (avl-tree-count ,right-sym) 2)) ;; FIXME
;;           ,left-short-case)
;;          ((< (avl-tree-count ,right-sym)
;;              (ash (avl-tree-count ,left-sym) 2)) ;; FIXME
;;           ,right-short-case)
;;          (t ,equiv-case)))))

(defun avl-tree-insert-vector (tree value compare)
  (declare (type simple-vector tree))
  (multiple-value-bind (i present)
      (array-tree-insert-position tree value compare)
    (declare (type fixnum i))
    (let* ((n (length tree))
           (n/2 (ash n -1)))
      (declare (type fixnum n n/2))
      (cond
        (present tree)
        ((< n +avl-tree-max-array-length+)
         (array-tree-insert-at tree value i))
        ((< i n/2)
         (make-avl-tree (array-tree-insert-at tree value i 0 (1- n/2))
                        (aref tree (1- n/2))
                        (subseq tree  n/2)))
        ((> i n/2)
         (make-avl-tree (subseq tree 0 n/2)
                        (aref tree n/2)
                        (array-tree-insert-at tree value i (1+ n/2))))
        (t ;; (= i n/2)
         (make-avl-tree (subseq tree 0 n/2)
                        value
                        (subseq tree  n/2)))))))


(defun avl-tree-insert (tree value compare)
  "Insert VALUE into TREE, returning new tree."
  (declare (type function compare))
  ;;(declare (optimize (speed 3) (safety 0)))
  (etypecase tree
    (avl-tree
     (with-avl-tree (l v r) tree
       (cond-compare (value v compare)
                     (balance-avl-tree (avl-tree-insert l value compare)
                                       v r)
                     tree
                     (balance-avl-tree l v
                                       (avl-tree-insert r value compare)))))
    (simple-vector (avl-tree-insert-vector tree value compare))
    (null (vector value))))



(defun avl-tree-replace-vector (tree value compare)
  (declare (type simple-vector tree))
  (multiple-value-bind (i present)
      (array-tree-insert-position tree value compare)
    (declare (type fixnum i))
    (let* ((n (length tree))
           (n/2 (ash n -1)))
      (declare (type fixnum n n/2))
      (cond
        (present
         (array-tree-set tree value i))
        ((< n +avl-tree-max-array-length+)
         (array-tree-insert-at tree value i))
        ((< i n/2)
         (make-avl-tree (array-tree-insert-at tree value i 0 (1- n/2))
                        (aref tree (1- n/2))
                        (subseq tree  n/2)))
        ((> i n/2)
         (make-avl-tree (subseq tree 0 n/2)
                        (aref tree n/2)
                        (array-tree-insert-at tree value i (1+ n/2))))
        (t ;; (= i n/2)
         (make-avl-tree (subseq tree 0 n/2)
                        value
                        (subseq tree  n/2)))))))


(defun avl-tree-replace (tree value compare)
  "Insert VALUE into TREE, returning new tree."
  (declare (type function compare))
  ;;(declare (optimize (speed 3) (safety 0)))
  (etypecase tree
    (avl-tree
     (with-avl-tree (l v r) tree
       (cond-compare (value v compare)
                     (balance-avl-tree (avl-tree-replace l value compare)
                                       v r)
                     (make-avl-tree l value r)
                     (balance-avl-tree l v
                                       (avl-tree-replace r value compare)))))
    (simple-vector (avl-tree-replace-vector tree value compare))
    (null (vector value))))


(defun avl-tree-reinsert-vector (tree value compare)
  (declare (type simple-vector tree))
  (let ((i (array-tree-insert-position tree value compare)))
    (declare (type fixnum i))
    (let* ((n (length tree))
           (n/2 (ash n -1)))
      (declare (type fixnum n n/2))
      (cond
        ((< n +avl-tree-max-array-length+)
         (array-tree-insert-at tree value i))
        ((< i n/2)
         (make-avl-tree (array-tree-insert-at tree value i 0 (1- n/2))
                        (aref tree (1- n/2))
                        (subseq tree  n/2)))
        ((> i n/2)
         (make-avl-tree (subseq tree 0 n/2)
                        (aref tree n/2)
                        (array-tree-insert-at tree value i (1+ n/2))))
        (t ;; (= i n/2)
         (make-avl-tree (subseq tree 0 n/2)
                        value
                        (subseq tree  n/2)))))))


(defun avl-tree-reinsert (tree value compare)
  "Insert VALUE into TREE, returning new tree."
  (declare (type function compare))
  ;;(declare (optimize (speed 3) (safety 0)))
  (etypecase tree
    (avl-tree
     (with-avl-tree (l v r) tree
       (cond-compare (value v compare)
                     (balance-avl-tree (avl-tree-reinsert l value compare)
                                       v r)
                     (if (< (avl-tree-count l) (avl-tree-count r))
                         (balance-avl-tree (avl-tree-reinsert l value compare) v r)
                         (balance-avl-tree l v (avl-tree-reinsert r value compare)))
                     (balance-avl-tree l v
                                       (avl-tree-reinsert r value compare)))))
    (simple-vector (avl-tree-reinsert-vector tree value compare))
    (null (vector value))))


(defun avl-tree-builder (compare)
  (lambda (tree value) (avl-tree-insert tree value compare)))

(defun avl-tree (compare &rest elements)
  (fold (avl-tree-builder compare) nil elements))

(defun avl-tree-remove-min (tree)
  "Remove minimum element of TREE, returning element and tree."
  (labels ((rec-tree (tree)
             (with-avl-tree (l v r) tree
               (if l
                   (multiple-value-bind (new-left min)
                       (etypecase l
                         (binary-tree (rec-tree l))
                         (simple-vector (min-vector l)))
                     (values (balance-avl-tree new-left v r)
                             min))
                   (values r v))))
           (min-vector (tree)
             (case (length tree)
               (0 (values nil nil))
               (1 (values nil (aref tree 0)))
               (otherwise (values (subseq tree 1) (aref tree 0))))))
    (etypecase tree
      (binary-tree (rec-tree tree))
      (simple-vector (min-vector tree))
      (null (values nil nil)))))

(defun avl-tree-remove-max (tree)
  "Remove minimum element of TREE, returning element and tree."
  (labels ((rec-tree (tree)
             (with-avl-tree (l v r) tree
               (if r
                   (multiple-value-bind (new-right max)
                       (etypecase r
                         (binary-tree (rec-tree r))
                         (simple-vector (max-vector r)))
                     (values (balance-avl-tree l v new-right)
                             max))
                   (values l v))))
           (max-vector (tree)
             (let ((n (1- (length tree))))
               (case n
                 (-1 (values nil nil))
                 (0 (values nil (aref tree 0)))
                 (otherwise (values (subseq tree 0 n) (aref tree n)))))))
    (etypecase tree
      (binary-tree (rec-tree tree))
      (simple-vector (max-vector tree))
      (null (values nil nil)))))


  ;;     (values nil nil)))
  ;; (etypecase tree
  ;;   (avl-tree
  ;;    (multiple-value-bind (new-left x) (avl-tree-remove-min (binary-tree-left tree))
  ;;      (values (balance-avl-tree new-left
  ;;                                (binary-tree-value tree)
  ;;                                (binary-tree-right tree))
  ;;              x)))
  ;;   (simple-vector (case (length tree)
  ;;                    (0 (values nil nil))
  ;;                    (1 (values nil (aref tree 0)))
  ;;                    (otherwise (values (subseq tree 1) (aref tree 0)))))
  ;;   (null (values nil nil))))

;; (defun avl-tree-remove-max (tree)
;;   "Remove maximum element of TREE, returning element and tree."
;;   (etypecase tree
;;     (avl-tree
;;      (multiple-value-bind (new-right x) (avl-tree-remove-max (binary-tree-right tree))
;;        (values (balance-avl-tree (binary-tree-left tree)
;;                                  (binary-tree-value tree)
;;                                  new-right)
;;                x)))
;;     (simple-vector (let ((n (1- (length tree))))
;;                      (case n
;;                        (-1 (values nil nil))
;;                        (0 (values nil (aref tree 0)))
;;                        (otherwise (values (subseq tree 0 n) (aref tree n))))))
;;     (null (values nil nil))))


;; (defun join-avl-tree (left value right compare)
;;   (cond-avl-tree-heights (left right)
;;                          (avl-tree-insert right value compare)
;;                          (avl-tree-insert left value compare)
;;                          (balance-avl-tree (join-avl-tree left value (binary-tree-left right) compare)
;;                                            (binary-tree-value right)
;;                                            (binary-tree-right right))
;;                          (balance-avl-tree left value right)
;;                          (balance-avl-tree (binary-tree-left left)
;;                                            (binary-tree-value left)
;;                                            (join-avl-tree (binary-tree-right left) value right compare))))

(defun join-avl-tree (left value right compare)
  (cond
    ((null left) (avl-tree-insert right value compare))
    ((null right) (avl-tree-insert left value compare))
    ((simple-vector-p left)
     (avl-tree-insert (reduce (avl-tree-builder compare) left :initial-value right)
                      value compare))
    ((simple-vector-p right)
     (avl-tree-insert (reduce (avl-tree-builder compare) right :initial-value left)
                      value compare))
    (t
     (with-avl-trees
         (l1 v1 r1 c1) left
         (l2 v2 r2 c2) right
       (cond
         ((> c2 (ash c1 +avl-tree-rebalance-log+))
          (balance-avl-tree (join-avl-tree left value l2 compare)
                            v2
                            r2))
         ((> c1 (ash c2 +avl-tree-rebalance-log+))
          (balance-avl-tree l1
                            v1
                            (join-avl-tree r1 value right compare)))
         (t (balance-avl-tree left value right)))))))

;; (defun avl-tree-concatenate (tree-1 tree-2)
;;   "Concatenate TREE-1 and TREE-2."
;;   (cond-avl-tree-heights (tree-1 tree-2)
;;                          tree-2
;;                          tree-1
;;                          (balance-avl-tree (avl-tree-concatenate tree-1 (binary-tree-left tree-2))
;;                            (binary-tree-value tree-2)
;;                            (binary-tree-right tree-2))
;;                          (multiple-value-bind (min tree) (avl-tree-remove-min tree-2)
;;                            (balance-avl-tree tree-1 min tree))
;;                          (balance-avl-tree (binary-tree-left tree-1)
;;                                            (binary-tree-value tree-1)
;;                                            (avl-tree-concatenate (binary-tree-right tree-1) tree-2))))


(defun avl-tree-concatenate (tree-1 tree-2 compare)
  "Concatenate TREE-1 and TREE-2."
  (cond
    ((null tree-1) tree-2)
    ((null tree-2) tree-1)
    ((simple-vector-p tree-1)
     (reduce (avl-tree-builder compare) tree-1 :initial-value tree-2))
    ((simple-vector-p tree-2)
     (reduce (avl-tree-builder compare) tree-2 :initial-value tree-1))
    ((< (avl-tree-weight tree-1) (avl-tree-weight tree-2))
     (balance-avl-tree (avl-tree-concatenate tree-1 (binary-tree-left tree-2) compare)
                       (binary-tree-value tree-2)
                       (binary-tree-right tree-2)))
    ((< (avl-tree-weight tree-2) (avl-tree-weight tree-1))
     (balance-avl-tree (binary-tree-left tree-1)
                       (binary-tree-value tree-1)
                       (avl-tree-concatenate (binary-tree-right tree-1) tree-2 compare)))
    (t (balance-avl-tree tree-1 (binary-tree-min tree-2) (avl-tree-remove-min tree-2)))))

;; (defun avl-tree-split (tree x compare)
;;   (declare (type function compare))
;;   (cond-avl-tree-compare (x tree compare)
;;     (values nil nil nil)
;;     (multiple-value-bind (left-left present right-left)
;;         (avl-tree-split (binary-tree-left tree) x compare)
;;       (values left-left present (join-avl-tree right-left
;;                                                (binary-tree-value tree)
;;                                                (binary-tree-right tree)
;;                                                compare)))
;;     (values (binary-tree-left tree) t (binary-tree-right tree))
;;     (multiple-value-bind (left-right present right-right)
;;         (avl-tree-split (binary-tree-right tree) x compare)
;;       (values (join-avl-tree (binary-tree-left tree)
;;                              (binary-tree-value tree)
;;                              left-right
;;                              compare)
;;               present
;;               right-right))))


(defun avl-tree-split (tree x compare)
  (declare (type function compare))
  (etypecase tree
    (avl-tree
     (with-avl-tree (l v r) tree
       (cond-compare (x v compare)
                     (multiple-value-bind (left-left present right-left)
                         (avl-tree-split l x compare)
                       (values left-left present (join-avl-tree right-left
                                                                v r compare)))
                     (values (binary-tree-left tree) t (binary-tree-right tree))
                     (multiple-value-bind (left-right present right-right)
                         (avl-tree-split r x compare)
                       (values (join-avl-tree l v left-right compare)
                               present right-right)))))
    (simple-vector
     (array-tree-split tree x compare))
    (null
     (values nil nil nil))))


;; (defun avl-tree-remove (tree x compare)
;;   "Remove X from TREE, returning new tree."
;;   (declare (type function compare))
;;   (cond-avl-tree-compare (x tree compare)
;;     nil
;;     (balance-avl-tree (avl-tree-remove (avl-tree-left tree) x compare)
;;                       (binary-tree-value tree)
;;                       (binary-tree-right tree))
;;     (avl-tree-concatenate (avl-tree-left tree)
;;                           (avl-tree-right tree))
;;     (balance-avl-tree (avl-tree-left tree)
;;                       (avl-tree-value tree)
;;                       (avl-tree-remove (avl-tree-right tree) x compare))))


(defun avl-tree-remove (tree x compare)
  "Remove X from TREE, returning new tree."
  (declare (type function compare))
  (cond-avl-tree-vector-compare
   (x tree compare)
   nil
   (array-tree-remove tree x compare)
   (balance-avl-tree (avl-tree-remove (avl-tree-left tree) x compare)
                     (binary-tree-value tree)
                     (binary-tree-right tree))
   (avl-tree-concatenate (avl-tree-left tree)
                         (avl-tree-right tree)
                         compare)
   (balance-avl-tree (avl-tree-left tree)
                     (avl-tree-value tree)
                     (avl-tree-remove (avl-tree-right tree) x compare))))


(defun avl-tree-remove-position (tree i compare)
  "Remove I'th element of TREE and return (values new-tree element)."
  (declare (type fixnum i))
  (etypecase tree
    (simple-vector (values (array-tree-remove-position tree i) (aref tree i)))
    (avl-tree
     (with-avl-tree (l v r) tree
       (let ((w-l (avl-tree-count l)))
         (declare (type fixnum w-l))
         (cond
           ((< i w-l) (balance-avl-tree (avl-tree-remove-position l i compare )
                                        v r))
           ((> i w-l) (balance-avl-tree l v
                                        (avl-tree-remove-position r (- i w-l 1) compare )))
           (t (avl-tree-concatenate l r compare))))))))


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
    ;; special-casing the vector gives big speedup
    ((simple-vector-p tree-1)
     (reduce (avl-tree-builder compare) tree-1 :initial-value tree-2))
    ((simple-vector-p tree-2)
     (reduce (avl-tree-builder compare) tree-2 :initial-value tree-1))
    (t
     (multiple-value-bind (tree-1 tree-2) ;; normalize sizes, faster to split the smaller tree
         (if (<= (avl-tree-count tree-2)
                 (avl-tree-count tree-1))
             (values tree-1 tree-2)
             (values tree-2 tree-1))
       (with-avl-tree (l1 v1 r1) tree-1
         (multiple-value-bind (l2 p-2 r2) (avl-tree-split tree-2 v1 compare)
           (declare (ignore p-2))
           (join-avl-tree (avl-tree-union l1 l2 compare)
                          v1
                          (avl-tree-union r1 r2 compare)
                          compare)))))))

(defun avl-tree-intersection (tree-1 tree-2 compare)
  (labels ((fold-keep (vector tree)
             (fold (lambda (new-tree x)
                     (if (binary-tree-member-p tree x compare)
                         (avl-tree-insert new-tree x compare)
                         new-tree))
                   nil vector)))
    (cond
      ((or (null tree-1)
           (null tree-2))
       nil)
      ;; base intersection
      ((and (simple-vector-p tree-1)
            (simple-vector-p tree-2))
       (array-tree-intersection tree-1 tree-2 compare))
      ;; keep all in tree-1 that are in tree-2
      ((simple-vector-p tree-1)
       (fold-keep tree-1 tree-2))
      ;; keep all in tree-2 that are in tree-1
      ((simple-vector-p tree-2)
       (fold-keep tree-2 tree-1))
      ;; general case
      (t
       (multiple-value-bind (tree-1 tree-2) ;; normalize sizes, faster to split the smaller tree
           (if (<= (avl-tree-count tree-2)
                   (avl-tree-count tree-1))
               (values tree-1 tree-2)
               (values tree-2 tree-1))
         (with-avl-tree (l1 v1 r1) tree-1
           (multiple-value-bind (l2 present r2)
               (avl-tree-split tree-2 v1 compare)
             (let ((l-i (avl-tree-intersection l1 l2 compare))
                   (r-i (avl-tree-intersection r1 r2 compare)))
               (if present
                   (join-avl-tree l-i v1 r-i compare)
                   (avl-tree-concatenate l-i r-i compare))))))))))

(defun avl-tree-difference (tree-1 tree-2 compare)
  (declare (type function compare))
  (cond
    ((null tree-1) nil)
    ((null tree-2) tree-1)
    ((simple-vector-p tree-1)
     (fold (lambda (tree x)
             (if (binary-tree-member-p tree-2 x compare)
                 tree
                 (array-tree-insert tree x compare)))
           (vector) tree-1))
    ;; general case
    (t (multiple-value-bind (left-2 present right-2)
           (avl-tree-split tree-2 (binary-tree-value tree-1) compare)
         (let ((left (avl-tree-difference (binary-tree-left tree-1) left-2 compare))
               (right (avl-tree-difference (binary-tree-right tree-1) right-2 compare)))
           (if present
               (avl-tree-concatenate left right compare)
               (join-avl-tree left (binary-tree-value tree-1) right compare)))))))



(defun avl-tree-subset (tree-1 tree-2 compare)
  (declare (type function compare))
  (labels ((rec (tree-1 tree-2)
             (cond
               ((> (avl-tree-count tree-1)
                   (avl-tree-count tree-2))
                nil)
               ((null tree-1) t)
               ((null tree-2) nil)
               ;; ((simple-vector-p tree-1)
               ;;  (every (lambda (x) (binary-tree-member-p tree-2 x compare)) tree-1))
               ;; ((simple-vector-p tree-2)
               ;;  (map-binary-tree :inorder nil
               ;;                   (lambda (x) (unless (array-tree-position tree-2 x compare)
               ;;                            (return-from rec nil)))
               ;;                   tree-1)
               ;;  t)
               (t
                (with-avl-trees
                    (l1 v1 r1) tree-1
                    (l2 v2 r2) tree-2
                  (cond-compare (v1 v2 compare)
                                ;; v1 < v2
                                (and (rec (make-avl-tree l1 v1 nil)
                                          l2)
                                     (rec r1 tree-2))
                                ;; v1 = v2
                                (and (rec l1 l2)
                                     (rec r1 r2))
                                ;; v1 > v2
                                (and (rec (make-avl-tree nil v1 r1)
                                          r2)
                                     (rec l1 tree-2))))))))
    (rec tree-1 tree-2)))


(declaim (ftype (function ((or avl-tree array) (or avl-tree array) function) fixnum) avl-tree-compare))
(defun avl-tree-compare (tree-1 tree-2 compare)
  (declare (type function compare))
  ;; O(log(n)) space, O(min(m,n)) time
  (cond
    ((and tree-1 tree-2)
     (let ((n1 (avl-tree-count tree-1))
           (n2 (avl-tree-count tree-2)))
       (cond  ;; first, order by count since that's O(1)
         ((< n1 n2) -1)
         ((> n1 n2) 1)
         (t (binary-tree-compare tree-1 tree-2 compare)))))
    (tree-1 1)
    (tree-2 -2)
    (t 0)))

(defun avl-tree-dot (tree &key output)
  (binary-tree-dot tree
                   :output output
                   :node-label-function (lambda (node)
                                          (format nil "~A (~D)"
                                                  (binary-tree-value node)
                                                  (avl-tree-weight node)))))
