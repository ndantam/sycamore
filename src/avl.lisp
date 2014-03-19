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

;; Ok, these aren't really AVL trees since we explicitly store element
;; count.

;; SEE: Adams, Stephen. Implementing Sets Efficiantly in a Functional Language

;; All leaf nodes are simple vectors.
;; We can assume a binary tree node will never have NULL left/right values

;;; BALANCING CONSTRAINT:
;;;   (< (+ minlen maxlen 1) (ash min rebalance-log))

(defconstant +avl-tree-max-array-length+ 16)
(defconstant +avl-tree-min-array-length+ 6)
(defparameter +avl-tree-rebalance-log+ 2)  ;; power two difference for rebalancing
(declaim (type (integer 2 2) +avl-tree-rebalance-log+))

(assert (< (+ +avl-tree-min-array-length+
              +avl-tree-max-array-length+
              1)
           (ash +avl-tree-min-array-length+  +avl-tree-rebalance-log+)))


(defmacro with-temp-avl-array ((var &optional (size +avl-tree-max-array-length+)) &body body)
  `(let ((,var (make-array ,size)))
     (declare (dynamic-extent ,var))
     ,@body))

(declaim (inline %make-avl-tree
                 make-avl-tree
                 left-avl-tree
                 right-avl-tree
                 left-right-avl-tree
                 right-left-avl-tree))

(defstruct (avl-tree
             (:include binary-tree)
             (:constructor %make-avl-tree (weight left value right)))
  (weight 0 :type positive-fixnum))


(defparameter *avl-tree-print-depth* 0)
(defparameter *avl-tree-print-max* 512)

(defun %avl-tree (&key weight left value right)
  (%make-avl-tree weight left value right))

(defmethod print-object ((obj avl-tree) stream)
  (let ((indent (make-string (* 2 *avl-tree-print-depth*) :initial-element #\Space)))
    (if (< (avl-tree-weight obj) *avl-tree-print-max*)
        (progn
          (format stream "~&~A" indent)
          (format stream "(%avl-tree :WEIGHT ~D" (avl-tree-weight obj))
          (format stream "~&~A  :VALUE ~A~%" indent (avl-tree-value obj))
          (let ((*avl-tree-print-depth* (1+ *avl-tree-print-depth*)))
            (format stream "~&~A  :LEFT ~A"  indent (avl-tree-left obj)))
          (let ((*avl-tree-print-depth* (1+ *avl-tree-print-depth*)))
            (format stream "~&~A  :RIGHT ~A)" indent (avl-tree-right obj))))
        (print-unreadable-object (obj stream :type t :identity t)
          (format stream ":WEIGHT ~D" (avl-tree-weight obj))
          (format stream "~&~A  :VALUE ~A~%" indent (avl-tree-value obj))))))

(declaim (ftype (function (t) positive-fixnum) avl-tree-count))
(declaim (inline avl-tree-count))

(defun avl-tree-count (tree)
  ;; don't complain when SBCL type-inference does its job
  #+sbcl(declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
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

(defun avl-tree-serialize (x)
  (etypecase x
    (avl-tree
     (with-avl-tree (l v r) x
       `(make-avl-tree ,(avl-tree-serialize l)
                       ,v
                       ,(avl-tree-serialize r))))
    (simple-vector `(vector ,@(loop for x across x collect x)))
    (null nil)))


(defun avl-tree-balanced-p (x)
  (etypecase x
    (avl-tree
     (with-avl-tree (l v r c) x
       (declare (ignore v))
       (let* ((l-c (avl-tree-count l))
              (r-c (avl-tree-count r))
              (power +avl-tree-rebalance-log+)
              (balanced (and (<= l-c (ash r-c power))
                             (<= r-c (ash l-c power)))))
         (multiple-value-bind (l-b l-n) (avl-tree-balanced-p l)
           (declare (type fixnum l-n))
           (multiple-value-bind (r-b r-n) (avl-tree-balanced-p r)
             (declare (type fixnum r-n))
             ;; (unless balanced
             ;;   (format t "~&l-c: ~A, r-c: ~A" l-c r-c)
             ;;   (print x))
             (values (and balanced
                          l-b
                          r-b
                          (= c (+ l-c r-c 1))
                          (= l-n l-c)
                          (= r-n r-c))
                     (+ l-n r-n 1)))))))
    (simple-vector (values t (length x)))
    (null (values t 0))))

(defmacro check-avl-balance (tree)
  (let ((x (gensym)))
  `(let ((,x ,tree))
     ;; uncomment this line to enable excessive balance checks
     ;;(assert (avl-tree-balanced-p ,x))
     ,x)))


(defun avl-tree-ref (tree subscript)
  "Return the element of `TREE' at position `SUBSCRIPT'.

Leftmost (least) element of TREE has SUBSCRIPT of zero."
  (declare (type positive-fixnum subscript))
  (labels ((rec (tree subscript)
             (declare (type positive-fixnum subscript))
             (etypecase tree
               (avl-tree
                (with-avl-tree (l v r) tree
                  (let ((lw (avl-tree-count l)))
                    (cond
                      ((< subscript lw)
                       (rec l subscript))
                      ((> subscript lw)
                       (rec r (- subscript lw 1)))
                      (t v)))))
               (simple-vector (aref tree subscript))
               (null (error "Cannot index NIL")))))
    (assert (< subscript (avl-tree-count tree)))
    (rec tree subscript)))


(defmacro with-avl-trees ((left1 value1 right1 &optional count1) tree1
                          (left2 value2 right2 &optional count2) tree2
                          &body body)
  `(with-avl-tree (,left1 ,value1 ,right1 ,count1) ,tree1
     (with-avl-tree (,left2 ,value2 ,right2 ,count2) ,tree2
         ,@body)))

(defun avl-tree-list (tree) (map-binary-tree :inorder 'list #'identity tree))



(defun right-avl-tree (left value right)
  "Right rotation"
  (make-avl-tree (binary-tree-left left)
                 (binary-tree-value left)
                 (make-avl-tree (binary-tree-right left) value right)))

(defun left-avl-tree (left value right)
  "Left rotation"
  (make-avl-tree (make-avl-tree left value (binary-tree-left right))
                 (binary-tree-value right)
                 (binary-tree-right right)))

(defun left-right-avl-tree (left value right)
  "Right rotation then left rotation"
  (make-avl-tree (make-avl-tree left value (binary-tree-left-left right))
                 (binary-tree-value-left right)
                 (make-avl-tree (binary-tree-right-left right)
                                (binary-tree-value right)
                                (binary-tree-right right))))

(defun right-left-avl-tree (left value right)
  "Left rotation then right rotation"
  (make-avl-tree (make-avl-tree (binary-tree-left left)
                                (binary-tree-value left)
                                (binary-tree-left-right left))
                 (binary-tree-value-right left)
                 (make-avl-tree (binary-tree-right-right left)
                                value
                                right)))


(defun avl-tree-concatenate-array (left right)
  (declare (type simple-vector left right))
  (let* ((w-l (length left))
         (w-r (length right))
         (w-lr (+ w-l w-r))
         (n-a (ash w-lr -1)))
    (cond
      ;; collapse
      ((<= w-lr +avl-tree-max-array-length+)
       (let ((new-array (make-array w-lr)))
         (replace new-array left)
         (replace new-array right :start1 w-l)
         new-array))
      ;; right bigger
      ((> w-r w-l)
       (cond ((or (> w-r (ash w-l +avl-tree-rebalance-log+))
                  (< w-l +avl-tree-min-array-length+))
              ;; reshape
              (let ((new-left (make-array n-a))
                    (new-right (make-array (- w-lr n-a 1))))
                (replace new-left left)
                (replace new-left right :start1 w-l)       ;; fill right into new-left
                (let ((start (- n-a w-l)))
                  (replace new-right right :start2 (1+ start))
                  (make-avl-tree new-left (aref right start) new-right))))
             (t (make-avl-tree (subseq left 0 (1- w-l)) (aref left (1- w-l)) right))))
      ;; left bigger or equal
      (t;(> w-l w-r)
       (cond ((or (> w-l (ash w-r +avl-tree-rebalance-log+))
                  (< w-r +avl-tree-min-array-length+))
              ;; reshape
              (let ((new-left (make-array n-a))
                    (new-right (make-array (- w-lr n-a 1))))
                (replace new-left left)
                (replace new-right left :start2 (1+ n-a))       ;; fill new-right with rest of left
                (let ((start (- w-l n-a 1)))
                  (replace new-right right :start1 start)) ;; fill rest of new-right with right
                (make-avl-tree new-left (aref left n-a) new-right)))
             (t (make-avl-tree (subseq left 0 (1- w-l)) (aref left (1- w-l)) right)))))))

(defun balance-avl-tree-array-pair (left value right)
  (declare (type simple-vector left right))
  ;;(print (list 'balance-avl-tree-array-pair left value right))
  (let* ((w-l (length left))
         (w-r (length right))
         (w-lr (+ w-l w-r))
         (n-a (ash w-lr -1)))
    (cond
      ((and (< w-lr +avl-tree-max-array-length+)
            (or (< w-l +avl-tree-min-array-length+)
                (< w-r +avl-tree-min-array-length+)))
       ;; collapse
       (let ((new-array (make-array (1+ w-lr))))
         (replace new-array left)
         (setf (aref new-array w-l) value)
         (replace new-array right :start1 (1+ w-l))
         new-array))
      ;; reshape
      (t
       (cond
         ;; right too big
         ((or (> w-r (ash w-l +avl-tree-rebalance-log+))
              (< w-l +avl-tree-min-array-length+))
          (let ((new-left (make-array n-a))
                (new-right (make-array (- w-lr n-a))))
            (replace new-left left)
            (setf (aref new-left w-l) value)                 ;; fill value into new-left
            (replace new-left right :start1 (+ w-l 1))       ;; fill right into new-left
            (let ((start (- n-a 1 w-l)))
              (replace new-right right :start2 (1+ start))
              (make-avl-tree new-left (aref right start) new-right)))) ;; fill right
         ;; left to big
         ((or (> w-l (ash w-r +avl-tree-rebalance-log+))
              (< w-r +avl-tree-min-array-length+))
          (let ((new-left (make-array n-a))
                (new-right (make-array (- w-lr n-a))))
            (replace new-left left)
            (replace new-right left :start2 (+ n-a 1))       ;; fill new-right with rest of left
            (let ((start (- w-l n-a 1)))
              (setf (aref new-right start) value)
              (replace new-right right :start1 (1+ start))) ;; fill rest of new-right with right
            (make-avl-tree new-left (aref left n-a) new-right)))
         ;; close enough
         (t (make-avl-tree left value right)))))))


(defun balance-avl-tree (left value right)
  ;;(declare (optimize (speed 3) (safety 0)))
;;  (print (list 'balance-avl-tree left value right))
  (labels ((balance-t-v ()
             (if (> (avl-tree-weight left) (ash (length right) +avl-tree-rebalance-log+))
                 (with-avl-tree (l-l v-l r-l) left
                   (if (> (avl-tree-count r-l)
                          (avl-tree-count l-l))
                       (etypecase r-l
                         (avl-tree (right-left-avl-tree left value right))
                         (simple-vector  (make-avl-tree l-l v-l
                                                        (balance-avl-tree-array-pair r-l value right))))
                       (right-avl-tree left value right)))
                 (make-avl-tree left value right)))
           (balance-v-t ()
             (if (> (avl-tree-weight right) (ash (length left) +avl-tree-rebalance-log+))
                 (with-avl-tree (l-r v-r r-r) right
                   (if (< (avl-tree-count r-r)
                          (avl-tree-count l-r))
                       (etypecase l-r
                         (avl-tree (left-right-avl-tree left value right))
                         (simple-vector
                          (make-avl-tree (balance-avl-tree-array-pair left value l-r)
                                         v-r r-r)))
                       (left-avl-tree left value right)))
                 (make-avl-tree left value right)))
           (balance-t-t ()
             (let ((w-l (avl-tree-weight left))
                   (w-r (avl-tree-weight right)))
               (cond
                 ;; left too tall
                 ((> w-l (ash w-r +avl-tree-rebalance-log+))
                  (with-avl-tree (l-l v-l r-l) left
                    (if (> (avl-tree-count r-l)
                           (avl-tree-count l-l))
                        (etypecase r-l
                          (avl-tree (right-left-avl-tree left value right))
                          (simple-vector (make-avl-tree l-l v-l
                                                        (balance-avl-tree-array-pair r-l value right))))
                        (right-avl-tree left value right))))
                 ;; right too tall
                 ((> w-r (ash w-l +avl-tree-rebalance-log+))
                  (with-avl-tree (l-r v-r r-r) right
                    (if (< (avl-tree-count r-r)
                           (avl-tree-count l-r))
                        (etypecase l-r
                          (avl-tree (left-right-avl-tree left value right))
                          (simple-vector (make-avl-tree (balance-avl-tree-array-pair left value l-r)
                                                        v-r r-r)))
                        (left-avl-tree left value right))))
                 ;; close enough
                 (t
                  (make-avl-tree left value right))))))
    ;; Type dispatching
    (let ((result
           (etypecase left
             (avl-tree
              (etypecase right
                (avl-tree (balance-t-t))
                (simple-vector (balance-t-v))
                (null (avl-tree-insert-max left value))))
             (simple-vector
              (etypecase right
                (avl-tree (balance-v-t))
                (simple-vector (balance-avl-tree-array-pair left value right))
                (null  (avl-tree-insert-max-array left value))))
             (null  (avl-tree-insert-min-array right value)))))
      (check-avl-balance result)
      result)))

(defun avl-tree-insert-min-array (tree value)
  (declare (simple-vector tree))
  (let ((n (length tree)))
    (if (>= n (1- +avl-tree-max-array-length+))
        ;; split
        (let* ((na (ash n -1))
               (left (make-array na))
               (right (make-array (- n na))))
          (setf (aref left 0) value)
          (replace left tree :start1 1)
          (replace right tree :start2 na)
          (make-avl-tree left (aref tree (1- na)) right))
        ;; single array
        (let ((new-array (make-array (1+ n))))
          (setf (aref new-array 0) value)
          (replace new-array tree :start1 1)))))

(defun avl-tree-insert-min (tree value)
  (etypecase tree
    (avl-tree (with-avl-tree (l v r) tree
                (balance-avl-tree (avl-tree-insert-min l value)
                                  v r)))
    (simple-vector
     (avl-tree-insert-min-array tree value))
    (null (vector value))))

(defun avl-tree-insert-max-array (tree value)
  (declare (simple-vector tree))
  (let ((n (length tree)))
    (if (>= n (1- +avl-tree-max-array-length+))
        ;; split
        (let* ((na (ash n -1))
               (nr (- n na))
               (left (make-array na))
               (right (make-array nr)))
          (setf (aref right (1- nr)) value)
          (replace left tree)
          (replace right tree :start2 (1+ na))
          (make-avl-tree left (aref tree na) right))
        ;; single array
        (let ((new-array (make-array (1+ n))))
          (setf (aref new-array n) value)
          (replace new-array tree)))))

(defun avl-tree-insert-max (tree value)
  (etypecase tree
    (avl-tree (with-avl-tree (l v r) tree
                (balance-avl-tree l v
                                  (avl-tree-insert-max r value))))
    (simple-vector
     (avl-tree-insert-max-array tree value))
    (null (vector value))))


(defun balance-avl-tree-left (old-tree old-left left value right)
  (if (eq old-left left)
      old-tree
      (balance-avl-tree left value right)))

(defun balance-avl-tree-right (old-tree old-right left value right)
  (if (eq old-right right)
      old-tree
      (balance-avl-tree left value right)))


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
                     (balance-avl-tree-left tree l
                                            (avl-tree-insert l value compare)
                                            v r)
                     tree
                     (balance-avl-tree-right tree r
                                             l v
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


(defun avl-tree-modify-vector (tree value compare modify default)
  (declare (type simple-vector tree)
           (type function modify))
  (multiple-value-bind (i present)
      (array-tree-insert-position tree value compare)
    (declare (type fixnum i))
    (if present
        ;; replace in tree
        (array-tree-set tree (funcall modify (aref tree i)) i)
        ;; moidfy default value
        (multiple-value-bind (value present) (funcall modify default)
          (if present
              ;; insert new value
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
                                  (subseq tree  n/2)))))
              ;; nothing to insert
              tree)))))


(defun avl-tree-modify (tree value compare modify &optional default)
  "Modify `VALUE' in `TREE', returning new tree."
  (declare (type function compare modify))
  (etypecase tree
    (avl-tree
     (with-avl-tree (l v r) tree
       (cond-compare (value v compare)
                     (balance-avl-tree (avl-tree-modify l value compare modify default)
                                       v r)
                     (multiple-value-bind (value present) (funcall modify v)
                       (if present
                           (make-avl-tree l value r)
                           (avl-tree-concatenate l r v)))
                     (balance-avl-tree l v
                                       (avl-tree-modify r value compare modify default)))))
    (simple-vector (avl-tree-modify-vector tree value compare modify default))
    (null (multiple-value-bind (value present) (funcall modify default)
            (when present (vector value))))))


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

(defun build-avl-tree (compare initial-tree elements)
  (fold (avl-tree-builder compare) initial-tree elements))

(defun avl-tree (compare &rest elements)
  (build-avl-tree compare nil elements))

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

;; TODO: insert array into tree


      ;; ;; TODO: build two leaf arrays in single step
      ;; ((< l-l l-r)
      ;;  (avl-tree-insert (build-avl-tree compare right left)
      ;;                   value compare))
      ;; (t
      ;;  (avl-tree-insert (build-avl-tree compare left right)
      ;;                   value compare)))))


(defun join-avl-tree (left value right compare)
  (let ((result
         (etypecase left
           (avl-tree
            (with-avl-tree (l1 v1 r1 c1) left
              (etypecase right
                (avl-tree
                 (with-avl-tree (l2 v2 r2 c2) right
                   (cond
                     ((> c2 (ash c1 +avl-tree-rebalance-log+))
                      (balance-avl-tree (join-avl-tree left value l2 compare)
                                        v2 r2))
                     ((> c1 (ash c2 +avl-tree-rebalance-log+))
                      (balance-avl-tree l1 v1
                                        (join-avl-tree r1 value right compare)))
                     (t
                        (make-avl-tree left value right)))))
                (simple-vector
                 (balance-avl-tree l1 v1 (join-avl-tree r1 value right compare)))
                (null (avl-tree-insert-max left value)))))
           (simple-vector
            (etypecase right
              (avl-tree
               (with-avl-tree (l2 v2 r2) right
                 (balance-avl-tree (join-avl-tree left value l2 compare) v2 r2)))
              (simple-vector (balance-avl-tree-array-pair left value right))
              (null (avl-tree-insert-max left value))))
           (null (avl-tree-insert-min right value)))))
    (check-avl-balance result)
    result))

(defun join-avl-tree-left (tree old-left left value right compare)
  (if (eq old-left left)
      tree
      (join-avl-tree left value right compare)))

(defun join-avl-tree-right (tree old-right left value right compare)
  (if (eq old-right right)
      tree
      (join-avl-tree left value right compare)))

(defun join-avl-tree-left-right (tree old-left old-right left value right compare)
  (if (and (eq old-left left)
           (eq old-right right))
      tree
      (join-avl-tree left value right compare)))

(defun avl-tree-concatenate (tree-1 tree-2 compare)
  "Concatenate TREE-1 and TREE-2."
  (let ((result
         (etypecase tree-1
           (avl-tree (etypecase tree-2
                       (avl-tree (with-avl-tree (l1 v1 r1) tree-1
                                   (join-avl-tree l1 v1
                                                  (avl-tree-concatenate r1 tree-2 compare)
                                                  compare)))
                       (simple-vector (with-avl-tree (l1 v1 r1) tree-1
                                        (join-avl-tree l1 v1
                                                       (avl-tree-concatenate r1 tree-2 compare)
                                                       compare)))
                       (null  tree-1)))
           (simple-vector (etypecase tree-2
                            (avl-tree (with-avl-tree (l2 v2 r2) tree-2
                                        (join-avl-tree (avl-tree-concatenate tree-1 l2 compare)
                                                       v2 r2 compare)))
                            (simple-vector (avl-tree-concatenate-array tree-1 tree-2))
                            (null tree-1)))
           (null tree-2))))
    (check-avl-balance result)
    result))




;; (defun avl-tree-concatenate-array (left right compare)
;;   (declare ;(ignore compare)
;;            (type simple-vector left right))
;;   (let ((l-l (length left))
;;         (l-r (length right)))
;;     (cond
;;       ((< (+ l-l l-r) (1- +avl-tree-max-array-length+))
;;        (let ((new-array (make-array  (+ l-l l-r))))
;;          (replace new-array left)
;;          (replace new-array right :start1 l-l)
;;          new-array))
;;       ;; TODO: build two leaf arrays in single step
;;       ((< l-l l-r)
;;         (build-avl-tree compare right left))
;;       (t
;;        (build-avl-tree compare left right)))))


;; (defun avl-tree-concatenate (tree-1 tree-2 compare)
;;   "Concatenate TREE-1 and TREE-2."
;;   (etypecase tree-1
;;     (avl-tree (etypecase tree-2
;;                 (avl-tree
;;                  (with-avl-trees
;;                      (l1 v1 r1 c1) tree-1
;;                      (l2 v2 r2 c2) tree-2
;;                    (cond
;;                      ((< c1 c2)
;;                       (balance-avl-tree (avl-tree-concatenate tree-1 l2 compare)
;;                                         v2
;;                                         r2))
;;                      ((< c2 c1)
;;                       (balance-avl-tree l1 v1
;;                                         (avl-tree-concatenate r1 tree-2 compare)))
;;                      (t (balance-avl-tree tree-1 (binary-tree-min tree-2)
;;                                           (avl-tree-remove-min tree-2))))))
;;                 (simple-vector (build-avl-tree compare tree-1 tree-2))
;;                 (null tree-1)))
;;     (simple-vector (etypecase tree-2
;;                      (avl-tree (build-avl-tree compare tree-2 tree-1))
;;                      (simple-vector (avl-tree-concatenate-array tree-1 tree-2 compare))
;;                                         ;(build-avl-tree compare tree-2 tree-1))
;;                      (null tree-1)))
;;     (null tree-2)))


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

(defmacro with-avl-tree-split ((left present right) tree x compare
                               &body body)
  `(multiple-value-bind (,left ,present ,right)
       (avl-tree-split ,tree ,x ,compare)
     ,@body))

(defun avl-tree-split (tree x compare)
  (declare (type function compare))
  (etypecase tree
    (avl-tree
     (with-avl-tree (l v r) tree
       (cond-compare (x v compare)
                     (with-avl-tree-split (left-left present right-left) l x compare
                       (values left-left present (join-avl-tree right-left
                                                                v r compare)))
                     (values (binary-tree-left tree) t (binary-tree-right tree))
                     (with-avl-tree-split (left-right present right-right) r x compare
                       (values (join-avl-tree l v left-right compare)
                               present right-right)))))
    (simple-vector
     (array-tree-split tree x compare))
    (null
     (values nil nil nil))))

(defun avl-tree-midpoint (tree compare)
  (declare (type function compare))
  (let ((v (avl-tree-ref tree (ash (avl-tree-count tree) -1))))
    (multiple-value-bind (l p r)
        (avl-tree-split tree v compare)
      (assert p)
      (values l v r))))

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
  (etypecase tree
    (avl-tree (with-avl-tree (l v r) tree
                (cond-compare (x v compare)
                              (balance-avl-tree-left tree l
                                                     (avl-tree-remove l x compare) v r)
                              (avl-tree-concatenate l r compare)
                              (balance-avl-tree-right tree r
                                                      l v (avl-tree-remove r x compare)))))
    (simple-vector (array-tree-remove tree x compare))
    (null tree)))



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


(defun avl-tree-union-array (tree-1 tree-2 compare)
  "Merge two arrays into and avl-tree"
  (declare (type simple-vector tree-1 tree-2)
           (type function compare))
           ;(optimize (speed 3) (safety 0)))
  (let* ((l-1 (length tree-1))
         (l-2 (length tree-2))
         (l-x (+ l-1 l-2))
         (n (* 2 +avl-tree-max-array-length+)))
    (with-temp-avl-array (x n)
      (assert (<= l-x n))
      (do ((i 0)
           (j 0)
           (k 0 (1+ k)))
          ((and (= i l-1)
                (= j l-2))
           ;; RESULT
           (cond ((> k +avl-tree-max-array-length+)
                  (multiple-value-call #'make-avl-tree
                    (array-tree-split-at x (ash k -1) 0 k)))
                 ((= k l-1) tree-1)
                 (t (subseq x 0 k))))
        (declare (type fixnum i j k))
        (cond
          ((= i l-1)
           (replace x tree-2 :start1 k :start2 j)
           (incf k (- l-2 j 1))
           (setq j l-2))
          ((= j l-2)
           (replace x tree-1 :start1 k :start2 i)
           (incf k (- l-1 i 1))
           (setq i l-1))
          (t
           (cond-compare ((aref tree-1 i) (aref tree-2 j) compare)
                         (progn (setf (aref x k) (aref tree-1 i))
                                (incf i))
                         (progn (setf (aref x k) (aref tree-2 j))
                                (incf i)
                                (incf j))
                         (progn (setf (aref x k) (aref tree-2 j))
                                (incf j)))))))))

(defun avl-tree-union-tree-vector (tree vector compare &optional (start 0) (end (length vector)))
  (declare (type avl-tree tree)
           (type simple-vector vector)
           (type function compare)
           (type fixnum start end))
  (do ((tree tree)
       (i start (1+ i)))
      ((>= i end) tree)
    (declare (type fixnum i))
    (setq tree (avl-tree-insert tree (aref vector i) compare))))

(defun avl-tree-union (tree-1 tree-2 compare)
  (declare (type function compare))
  (etypecase tree-1
    (avl-tree (etypecase tree-2
                (avl-tree
                 ;; normalize sizes, faster to split the smaller tree
                 (multiple-value-bind (tree-1 tree-2)
                     (if (<= (avl-tree-weight tree-2)
                             (avl-tree-weight tree-1))
                         (values tree-1 tree-2)
                         (values tree-2 tree-1))
                   (with-avl-tree (l1 v1 r1) tree-1
                     (multiple-value-bind (l2 p-2 r2) (avl-tree-split tree-2 v1 compare)
                       (declare (ignore p-2))
                       (join-avl-tree-left-right tree-1 l1 r1
                                           (avl-tree-union l1 l2 compare)
                                           v1
                                           (avl-tree-union r1 r2 compare)
                                           compare)))))
                (simple-vector (avl-tree-union-tree-vector tree-1 tree-2 compare))
                (null tree-1)))
    (simple-vector (etypecase tree-2
                     (avl-tree (avl-tree-union-tree-vector tree-2 tree-1 compare))
                     (simple-vector (avl-tree-union-array tree-1 tree-2 compare))
                     (null tree-1)))
    (null tree-2)))

(defun avl-tree-array-intersection (tree1 tree2 compare)
  (declare (type simple-vector tree1 tree2)
           (type function compare))
  (let ((l-1 (length tree1))
        (l-2 (length tree2)))
    (with-temp-avl-array (array)
      (assert (and (<= l-1 +avl-tree-max-array-length+)
                   (<= l-2 +avl-tree-max-array-length+)))
      (do ((i 0)
           (j 0)
           (k 0))
          ((or (>= i l-1)
               (>= j l-2))
           (cond ((= k l-1) tree1)
                 ((= k l-2) tree2)
                 ((zerop k) nil)
                 (t (subseq array 0 k))))
        (cond-compare ((aref tree1 i) (aref tree2 j) compare)
                      (incf i)
                      (progn
                        (setf (aref array k) (aref tree1 i))
                        (incf i)
                        (incf j)
                        (incf k))
                      (incf j))))))

(defun avl-tree-intersection-tree-array (tree array compare)
  (declare (type simple-vector array))
  (let* ((l-v (length array)))
    (with-temp-avl-array (new-array)
      (assert (<= l-v +avl-tree-max-array-length+))
      (do ((i 0 (1+ i))
           (k 0))
          ((= i l-v)
           (cond
             ((= k l-v)
              array) ;; all elements in array retained
             ((zerop k) nil)
             (t (subseq new-array 0 k))))
        (let ((x (aref array i)))
          (when (binary-tree-member-p tree x compare)
            (setf (aref new-array k) x)
            (incf k)))))))

(defun avl-tree-intersection (tree-1 tree-2 compare)
  (etypecase tree-1
    (avl-tree (etypecase tree-2
                (avl-tree
                 (multiple-value-bind (tree-1 tree-2) ;; normalize sizes, faster to split the smaller tree
                     (if (<= (avl-tree-weight tree-2)
                             (avl-tree-weight tree-1))
                         (values tree-1 tree-2)
                         (values tree-2 tree-1))
                   (with-avl-tree (l1 v1 r1) tree-1
                     (multiple-value-bind (l2 present r2)
                         (avl-tree-split tree-2 v1 compare)
                       (let ((l-i (avl-tree-intersection l1 l2 compare))
                             (r-i (avl-tree-intersection r1 r2 compare)))
                         (let ((result
                                (if present
                                    (join-avl-tree l-i v1 r-i compare)
                                    (avl-tree-concatenate l-i r-i compare))))
                           ;(unless (avl-tree-balanced-p result)
                             ;(print (list l-i v1 present r-i))
                             ;(assert nil))

                           result))))))
                (simple-vector (avl-tree-intersection-tree-array tree-1 tree-2 compare))
                (null nil)))
    (simple-vector (etypecase tree-2
                     (avl-tree (avl-tree-intersection-tree-array tree-2 tree-1 compare))
                     (simple-vector (avl-tree-array-intersection tree-1 tree-2 compare))
                     (null nil)))
    (null nil)))

(defun avl-tree-difference (tree-1 tree-2 compare)
  (declare (type function compare))
  (etypecase tree-1
    (avl-tree (etypecase tree-2
                (avl-tree
                 (with-avl-tree (l1 v1 r1) tree-1
                   (multiple-value-bind (left-2 present right-2)
                       (avl-tree-split tree-2 v1 compare)
                     (let ((left (avl-tree-difference l1 left-2 compare))
                           (right (avl-tree-difference r1 right-2 compare)))
                       (if present
                           (avl-tree-concatenate left right compare)
                           (join-avl-tree-left-right tree-1 l1 r1
                                                     left v1 right compare))))))
                (simple-vector
                 (fold (lambda (tree x)
                         (avl-tree-remove tree x compare))
                       tree-1 tree-2))
                (null tree-1)))
    (simple-vector (etypecase tree-2
                     (avl-tree (with-temp-avl-array (new-array)
                                 (let ((l-1 (length tree-1)))
                                   (do ((i 0 (1+ i))
                                        (k 0))
                                       ((= i l-1)
                                        (if (zerop k)
                                            nil
                                            (subseq new-array 0 k)))
                                     (let ((x (aref tree-1 i)))
                                       (unless (binary-tree-member-p tree-2 x compare)
                                         (setf (aref new-array k) x)
                                         (incf k)))))))
                     (simple-vector (with-temp-avl-array (new-array)
                                      ;; TODO: can we do better?
                                      (let ((l-1 (length tree-1)))
                                        (do ((i 0 (1+ i))
                                             (k 0))
                                            ((= i l-1)
                                             (if (zerop k)
                                                 nil
                                                 (subseq new-array 0 k)))
                                          (let ((x (aref tree-1 i)))
                                            (unless (binary-tree-member-p tree-2 x compare)
                                              (setf (aref new-array k) x)
                                              (incf k)))))))
                     (null tree-1)))
    (null nil)))

(defun avl-tree-intersection-difference (tree-1 tree-2 compare)
  (declare (type function compare))
  (etypecase tree-1
    (avl-tree (etypecase tree-2
                (avl-tree
                 (with-avl-tree (l1 v1 r1) tree-1
                   (multiple-value-bind (l2 present r2)
                       (avl-tree-split tree-2 v1 compare)
                     (multiple-value-bind (l-i l-d) (avl-tree-intersection-difference l1 l2 compare)
                       (multiple-value-bind (r-i r-d) (avl-tree-intersection-difference r1 r2 compare)
                         (if present
                             (values (join-avl-tree l-i v1 r-i compare)
                                     (avl-tree-concatenate l-d r-d compare))
                             (values (avl-tree-concatenate l-i r-i compare)
                                     (join-avl-tree l-d (binary-tree-value tree-1) r-d compare))))))))
                (t (values (avl-tree-intersection tree-1 tree-2 compare)
                           (avl-tree-difference tree-1 tree-2 compare)))))
    (t (values (avl-tree-intersection tree-1 tree-2 compare)
               (avl-tree-difference tree-1 tree-2 compare)))))



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


(declaim (ftype (function ((or avl-tree array null) (or avl-tree array null) function)
                          fixnum)
                avl-tree-compare))
(defun avl-tree-compare (tree-1 tree-2 compare)
  (declare (type function compare))
  ;; O(log(n)) space, O(min(m,n)) time
  (cond
    ((eq tree-1 tree-2) 0)
    ((and tree-1 tree-2)
     (let ((n1 (avl-tree-count tree-1))
           (n2 (avl-tree-count tree-2)))
       (cond  ;; first, order by count since that's O(1)
         ((< n1 n2) -1)
         ((> n1 n2) 1)
         ;((< n1 +avl-tree-max-array-length+)
         ((and (simple-vector-p tree-1)
               (simple-vector-p tree-2))
          (let ((i (array-tree-compare tree-1 tree-2 compare)))
            i))
         (t
          ;; try binary again
          ;(binary-tree-compare tree-1 tree-2 compare)))))
          (multiple-value-bind (l1 m1 r1)
              (avl-tree-midpoint tree-1 compare)
            (multiple-value-bind (l2 m2 r2)
                (avl-tree-midpoint tree-2 compare)
              (or-compare (funcall compare m1 m2)
                          (avl-tree-compare l1 l2 compare)
                          (avl-tree-compare r1 r2 compare))))))))
    (tree-1 1)
    (tree-2 -1)
    (t 0)))


(defun avl-tree-dot (tree &key output)
  (binary-tree-dot tree
                   :output output
                   :node-label-function (lambda (node)
                                          (format nil "~A (~D)"
                                                  (binary-tree-value node)
                                                  (avl-tree-weight node)))))
