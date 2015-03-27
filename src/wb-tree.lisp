;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2012-2014, Georgia Tech Research Corporation
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
;;  WB  ;;
;;;;;;;;;;;

;; SEE: Adams, Stephen. Implementing Sets Efficiantly in a Functional Language

;; All leaf nodes are simple vectors.
;; We can assume a binary tree node will never have NULL left/right values

;;; BALANCING CONSTRAINT:
;;;   (< (+ minlen maxlen 1) (ash min rebalance-log))

(defconstant +wb-tree-max-array-length+ 16)
(defconstant +wb-tree-min-array-length+ 6)
(defparameter +wb-tree-rebalance-log+ 2)  ;; power two difference for rebalancing
(declaim (type (integer 2 2) +wb-tree-rebalance-log+))

(assert (< (+ +wb-tree-min-array-length+
              +wb-tree-max-array-length+
              1)
           (ash +wb-tree-min-array-length+  +wb-tree-rebalance-log+)))


(defmacro with-temp-wb-array ((var &optional (size +wb-tree-max-array-length+)) &body body)
  `(let ((,var (make-array ,size)))
     (declare (dynamic-extent ,var))
     ,@body))

(declaim (inline %make-wb-tree
                 make-wb-tree
                 left-wb-tree
                 right-wb-tree
                 left-right-wb-tree
                 right-left-wb-tree))

(defstruct (wb-tree
             (:include binary-tree)
             (:constructor %make-wb-tree (weight left value right)))
  (weight 0 :type unsigned-fixnum))


(defparameter *wb-tree-print-depth* 0)
(defparameter *wb-tree-print-max* 512)

(defun %wb-tree (&key weight left value right)
  (%make-wb-tree weight left value right))

(defmethod print-object ((obj wb-tree) stream)
  (let ((indent (make-string (* 2 *wb-tree-print-depth*) :initial-element #\Space)))
    (if (< (wb-tree-weight obj) *wb-tree-print-max*)
        (progn
          (format stream "~&~A" indent)
          (format stream "(%wb-tree :WEIGHT ~D" (wb-tree-weight obj))
          (format stream "~&~A  :VALUE ~A~%" indent (wb-tree-value obj))
          (let ((*wb-tree-print-depth* (1+ *wb-tree-print-depth*)))
            (format stream "~&~A  :LEFT ~A"  indent (wb-tree-left obj)))
          (let ((*wb-tree-print-depth* (1+ *wb-tree-print-depth*)))
            (format stream "~&~A  :RIGHT ~A)" indent (wb-tree-right obj))))
        (print-unreadable-object (obj stream :type t :identity t)
          (format stream ":WEIGHT ~D" (wb-tree-weight obj))
          (format stream "~&~A  :VALUE ~A~%" indent (wb-tree-value obj))))))

(declaim (ftype (function (t) unsigned-fixnum) wb-tree-count))
(declaim (inline wb-tree-count))

(defun wb-tree-count (tree)
  ;; don't complain when SBCL type-inference does its job
  #+sbcl(declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
  (etypecase tree
    (wb-tree (wb-tree-weight tree))
    (simple-vector (length tree))
    (null 0)))

(defun make-wb-tree (left value right)
  (%make-wb-tree (the fixnum (+ 1
                                 (wb-tree-count left)
                                 (wb-tree-count right)))
                  left value right))

(defmacro with-wb-tree ((left value right &optional count) tree &body body)
  (with-gensyms (tree-sym l-sym v-sym r-sym)
    `(let ((,tree-sym ,tree))
       (multiple-value-bind (,left ,value ,right ,@(when count (list count)))
           (etypecase ,tree-sym
             (binary-tree (values (binary-tree-left ,tree-sym)
                                  (binary-tree-value ,tree-sym)
                                  (binary-tree-right ,tree-sym)
                                  ,@(when count `((wb-tree-weight ,tree-sym)))))
             (simple-vector (with-array-tree (,l-sym ,v-sym ,r-sym) ,tree-sym
                              (values ,l-sym ,v-sym ,r-sym
                                      ,@(when count `((length ,tree-sym)))))))
         ,@body))))

(defun wb-tree-serialize (x)
  (etypecase x
    (wb-tree
     (with-wb-tree (l v r) x
       `(make-wb-tree ,(wb-tree-serialize l)
                       ,v
                       ,(wb-tree-serialize r))))
    (simple-vector `(vector ,@(loop for x across x collect x)))
    (null nil)))


(defun wb-tree-balanced-p (x)
  (etypecase x
    (wb-tree
     (with-wb-tree (l v r c) x
       (declare (ignore v))
       (let* ((l-c (wb-tree-count l))
              (r-c (wb-tree-count r))
              (power +wb-tree-rebalance-log+)
              (balanced (and (<= l-c (ash r-c power))
                             (<= r-c (ash l-c power)))))
         (multiple-value-bind (l-b l-n) (wb-tree-balanced-p l)
           (declare (type fixnum l-n))
           (multiple-value-bind (r-b r-n) (wb-tree-balanced-p r)
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

(defmacro check-wb-balance (tree)
  (let ((x (gensym)))
  `(let ((,x ,tree))
     ;; uncomment this line to enable excessive balance checks
     ;;(assert (wb-tree-balanced-p ,x))
     ,x)))


(defun wb-tree-ref (tree subscript)
  "Return the element of `TREE' at position `SUBSCRIPT'.

Leftmost (least) element of TREE has SUBSCRIPT of zero."
  (declare (type unsigned-fixnum subscript))
  (labels ((rec (tree subscript)
             (declare (type unsigned-fixnum subscript))
             (etypecase tree
               (wb-tree
                (with-wb-tree (l v r) tree
                  (let ((lw (wb-tree-count l)))
                    (cond
                      ((< subscript lw)
                       (rec l subscript))
                      ((> subscript lw)
                       (rec r (- subscript lw 1)))
                      (t v)))))
               (simple-vector (aref tree subscript))
               (null (error "Cannot index NIL")))))
    (assert (< subscript (wb-tree-count tree)))
    (rec tree subscript)))


(defmacro with-wb-trees ((left1 value1 right1 &optional count1) tree1
                          (left2 value2 right2 &optional count2) tree2
                          &body body)
  `(with-wb-tree (,left1 ,value1 ,right1 ,count1) ,tree1
     (with-wb-tree (,left2 ,value2 ,right2 ,count2) ,tree2
         ,@body)))

(defun wb-tree-list (tree) (map-binary-tree :inorder 'list #'identity tree))



(defun right-wb-tree (left value right)
  "Right rotation"
  (make-wb-tree (binary-tree-left left)
                 (binary-tree-value left)
                 (make-wb-tree (binary-tree-right left) value right)))

(defun left-wb-tree (left value right)
  "Left rotation"
  (make-wb-tree (make-wb-tree left value (binary-tree-left right))
                 (binary-tree-value right)
                 (binary-tree-right right)))

(defun left-right-wb-tree (left value right)
  "Right rotation then left rotation"
  (make-wb-tree (make-wb-tree left value (binary-tree-left-left right))
                 (binary-tree-value-left right)
                 (make-wb-tree (binary-tree-right-left right)
                                (binary-tree-value right)
                                (binary-tree-right right))))

(defun right-left-wb-tree (left value right)
  "Left rotation then right rotation"
  (make-wb-tree (make-wb-tree (binary-tree-left left)
                                (binary-tree-value left)
                                (binary-tree-left-right left))
                 (binary-tree-value-right left)
                 (make-wb-tree (binary-tree-right-right left)
                                value
                                right)))


(defun wb-tree-concatenate-array (left right)
  (declare (type simple-vector left right))
  (let* ((w-l (length left))
         (w-r (length right))
         (w-lr (+ w-l w-r))
         (n-a (ash w-lr -1)))
    (cond
      ;; collapse
      ((<= w-lr +wb-tree-max-array-length+)
       (let ((new-array (make-array w-lr)))
         (replace new-array left)
         (replace new-array right :start1 w-l)
         new-array))
      ;; right bigger
      ((> w-r w-l)
       (cond ((or (> w-r (ash w-l +wb-tree-rebalance-log+))
                  (< w-l +wb-tree-min-array-length+))
              ;; reshape
              (let ((new-left (make-array n-a))
                    (new-right (make-array (- w-lr n-a 1))))
                (replace new-left left)
                (replace new-left right :start1 w-l)       ;; fill right into new-left
                (let ((start (- n-a w-l)))
                  (replace new-right right :start2 (1+ start))
                  (make-wb-tree new-left (aref right start) new-right))))
             (t (make-wb-tree (subseq left 0 (1- w-l)) (aref left (1- w-l)) right))))
      ;; left bigger or equal
      (t;(> w-l w-r)
       (cond ((or (> w-l (ash w-r +wb-tree-rebalance-log+))
                  (< w-r +wb-tree-min-array-length+))
              ;; reshape
              (let ((new-left (make-array n-a))
                    (new-right (make-array (- w-lr n-a 1))))
                (replace new-left left)
                (replace new-right left :start2 (1+ n-a))       ;; fill new-right with rest of left
                (let ((start (- w-l n-a 1)))
                  (replace new-right right :start1 start)) ;; fill rest of new-right with right
                (make-wb-tree new-left (aref left n-a) new-right)))
             (t (make-wb-tree (subseq left 0 (1- w-l)) (aref left (1- w-l)) right)))))))

(defun balance-wb-tree-array-pair (left value right)
  (declare (type simple-vector left right))
  ;;(print (list 'balance-wb-tree-array-pair left value right))
  (let* ((w-l (length left))
         (w-r (length right))
         (w-lr (+ w-l w-r))
         (n-a (ash w-lr -1)))
    (cond
      ((and (< w-lr +wb-tree-max-array-length+)
            (or (< w-l +wb-tree-min-array-length+)
                (< w-r +wb-tree-min-array-length+)))
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
         ((or (> w-r (ash w-l +wb-tree-rebalance-log+))
              (< w-l +wb-tree-min-array-length+))
          (let ((new-left (make-array n-a))
                (new-right (make-array (- w-lr n-a))))
            (replace new-left left)
            (setf (aref new-left w-l) value)                 ;; fill value into new-left
            (replace new-left right :start1 (+ w-l 1))       ;; fill right into new-left
            (let ((start (- n-a 1 w-l)))
              (replace new-right right :start2 (1+ start))
              (make-wb-tree new-left (aref right start) new-right)))) ;; fill right
         ;; left to big
         ((or (> w-l (ash w-r +wb-tree-rebalance-log+))
              (< w-r +wb-tree-min-array-length+))
          (let ((new-left (make-array n-a))
                (new-right (make-array (- w-lr n-a))))
            (replace new-left left)
            (replace new-right left :start2 (+ n-a 1))       ;; fill new-right with rest of left
            (let ((start (- w-l n-a 1)))
              (setf (aref new-right start) value)
              (replace new-right right :start1 (1+ start))) ;; fill rest of new-right with right
            (make-wb-tree new-left (aref left n-a) new-right)))
         ;; close enough
         (t (make-wb-tree left value right)))))))


(defun balance-wb-tree (left value right)
  ;;(declare (optimize (speed 3) (safety 0)))
;;  (print (list 'balance-wb-tree left value right))
  (labels ((balance-t-v ()
             (if (> (wb-tree-weight left) (ash (length right) +wb-tree-rebalance-log+))
                 (with-wb-tree (l-l v-l r-l) left
                   (if (> (wb-tree-count r-l)
                          (wb-tree-count l-l))
                       (etypecase r-l
                         (wb-tree (right-left-wb-tree left value right))
                         (simple-vector  (make-wb-tree l-l v-l
                                                        (balance-wb-tree-array-pair r-l value right))))
                       (right-wb-tree left value right)))
                 (make-wb-tree left value right)))
           (balance-v-t ()
             (if (> (wb-tree-weight right) (ash (length left) +wb-tree-rebalance-log+))
                 (with-wb-tree (l-r v-r r-r) right
                   (if (< (wb-tree-count r-r)
                          (wb-tree-count l-r))
                       (etypecase l-r
                         (wb-tree (left-right-wb-tree left value right))
                         (simple-vector
                          (make-wb-tree (balance-wb-tree-array-pair left value l-r)
                                         v-r r-r)))
                       (left-wb-tree left value right)))
                 (make-wb-tree left value right)))
           (balance-t-t ()
             (let ((w-l (wb-tree-weight left))
                   (w-r (wb-tree-weight right)))
               (cond
                 ;; left too tall
                 ((> w-l (ash w-r +wb-tree-rebalance-log+))
                  (with-wb-tree (l-l v-l r-l) left
                    (if (> (wb-tree-count r-l)
                           (wb-tree-count l-l))
                        (etypecase r-l
                          (wb-tree (right-left-wb-tree left value right))
                          (simple-vector (make-wb-tree l-l v-l
                                                        (balance-wb-tree-array-pair r-l value right))))
                        (right-wb-tree left value right))))
                 ;; right too tall
                 ((> w-r (ash w-l +wb-tree-rebalance-log+))
                  (with-wb-tree (l-r v-r r-r) right
                    (if (< (wb-tree-count r-r)
                           (wb-tree-count l-r))
                        (etypecase l-r
                          (wb-tree (left-right-wb-tree left value right))
                          (simple-vector (make-wb-tree (balance-wb-tree-array-pair left value l-r)
                                                        v-r r-r)))
                        (left-wb-tree left value right))))
                 ;; close enough
                 (t
                  (make-wb-tree left value right))))))
    ;; Type dispatching
    (let ((result
           (etypecase left
             (wb-tree
              (etypecase right
                (wb-tree (balance-t-t))
                (simple-vector (balance-t-v))
                (null (wb-tree-insert-max left value))))
             (simple-vector
              (etypecase right
                (wb-tree (balance-v-t))
                (simple-vector (balance-wb-tree-array-pair left value right))
                (null  (wb-tree-insert-max-array left value))))
             (null  (wb-tree-insert-min-array right value)))))
      (check-wb-balance result)
      result)))

(defun wb-tree-insert-min-array (tree value)
  (declare (simple-vector tree))
  (let ((n (length tree)))
    (if (>= n (1- +wb-tree-max-array-length+))
        ;; split
        (let* ((na (ash n -1))
               (left (make-array na))
               (right (make-array (- n na))))
          (setf (aref left 0) value)
          (replace left tree :start1 1)
          (replace right tree :start2 na)
          (make-wb-tree left (aref tree (1- na)) right))
        ;; single array
        (let ((new-array (make-array (1+ n))))
          (setf (aref new-array 0) value)
          (replace new-array tree :start1 1)))))

(defun wb-tree-insert-min (tree value)
  (etypecase tree
    (wb-tree (with-wb-tree (l v r) tree
                (balance-wb-tree (wb-tree-insert-min l value)
                                  v r)))
    (simple-vector
     (wb-tree-insert-min-array tree value))
    (null (vector value))))

(defun wb-tree-insert-max-array (tree value)
  (declare (simple-vector tree))
  (let ((n (length tree)))
    (if (>= n (1- +wb-tree-max-array-length+))
        ;; split
        (let* ((na (ash n -1))
               (nr (- n na))
               (left (make-array na))
               (right (make-array nr)))
          (setf (aref right (1- nr)) value)
          (replace left tree)
          (replace right tree :start2 (1+ na))
          (make-wb-tree left (aref tree na) right))
        ;; single array
        (let ((new-array (make-array (1+ n))))
          (setf (aref new-array n) value)
          (replace new-array tree)))))

(defun wb-tree-insert-max (tree value)
  (etypecase tree
    (wb-tree (with-wb-tree (l v r) tree
                (balance-wb-tree l v
                                  (wb-tree-insert-max r value))))
    (simple-vector
     (wb-tree-insert-max-array tree value))
    (null (vector value))))


(defun balance-wb-tree-left (old-tree old-left left value right)
  (if (eq old-left left)
      old-tree
      (balance-wb-tree left value right)))

(defun balance-wb-tree-right (old-tree old-right left value right)
  (if (eq old-right right)
      old-tree
      (balance-wb-tree left value right)))


(defun wb-tree-smaller (tree-1 tree-2)
  "Is `tree-1' shorter than `tree-2'?"
  (cond
    ((null tree-2) nil)
    ((null tree-1) t)
    (t (< (wb-tree-count tree-1)
          (wb-tree-count tree-2)))))

(defmacro cond-wb-tree-compare ((value tree compare)
                                 null-case less-case equal-case greater-case)
  "Compare VALUE to value of TREE and execute the corresponding case."
  (with-gensyms (c tree-sym)
    `(let ((,tree-sym ,tree))
       (if (null ,tree-sym)
           ,null-case
           (let ((,c (funcall ,compare ,value (binary-tree-value ,tree-sym))))
             (declare (type fixnum ,c))
             (cond
               ((< ,c 0) ,less-case)
               ((> ,c 0) ,greater-case)
               (t ,equal-case)))))))

(defmacro cond-wb-tree-vector-compare ((value tree compare)
                                        null-case vector-case less-case equal-case greater-case)
  "Compare VALUE to value of TREE and execute the corresponding case."
  (with-gensyms (c tree-sym)
    `(let ((,tree-sym ,tree))
       (etypecase ,tree-sym
         (wb-tree
          (let ((,c (funcall ,compare ,value (binary-tree-value ,tree-sym))))
            (declare (type fixnum ,c))
            (cond
              ((< ,c 0) ,less-case)
              ((> ,c 0) ,greater-case)
              (t ,equal-case))))
         (simple-vector ,vector-case)
         (null ,null-case)))))

(defun wb-tree-insert-vector (tree value compare)
  (declare (type simple-vector tree))
  (multiple-value-bind (i present)
      (array-tree-insert-position tree value compare)
    (declare (type fixnum i))
    (let* ((n (length tree))
           (n/2 (ash n -1)))
      (declare (type fixnum n n/2))
      (cond
        (present tree)
        ((< n +wb-tree-max-array-length+)
         (array-tree-insert-at tree value i))
        ((< i n/2)
         (make-wb-tree (array-tree-insert-at tree value i 0 (1- n/2))
                        (aref tree (1- n/2))
                        (subseq tree  n/2)))
        ((> i n/2)
         (make-wb-tree (subseq tree 0 n/2)
                        (aref tree n/2)
                        (array-tree-insert-at tree value i (1+ n/2))))
        (t ;; (= i n/2)
         (make-wb-tree (subseq tree 0 n/2)
                        value
                        (subseq tree  n/2)))))))


(defun wb-tree-insert (tree value compare)
  "Insert VALUE into TREE, returning new tree."
  (declare (type function compare))
  ;;(declare (optimize (speed 3) (safety 0)))
  (etypecase tree
    (wb-tree
     (with-wb-tree (l v r) tree
       (cond-compare (value v compare)
                     (balance-wb-tree-left tree l
                                            (wb-tree-insert l value compare)
                                            v r)
                     tree
                     (balance-wb-tree-right tree r
                                             l v
                                             (wb-tree-insert r value compare)))))
    (simple-vector (wb-tree-insert-vector tree value compare))
    (null (vector value))))




(defun wb-tree-replace-vector (tree value compare)
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
        ((< n +wb-tree-max-array-length+)
         (array-tree-insert-at tree value i))
        ((< i n/2)
         (make-wb-tree (array-tree-insert-at tree value i 0 (1- n/2))
                        (aref tree (1- n/2))
                        (subseq tree  n/2)))
        ((> i n/2)
         (make-wb-tree (subseq tree 0 n/2)
                        (aref tree n/2)
                        (array-tree-insert-at tree value i (1+ n/2))))
        (t ;; (= i n/2)
         (make-wb-tree (subseq tree 0 n/2)
                        value
                        (subseq tree  n/2)))))))


(defun wb-tree-replace (tree value compare)
  "Insert VALUE into TREE, returning new tree."
  (declare (type function compare))
  ;;(declare (optimize (speed 3) (safety 0)))
  (etypecase tree
    (wb-tree
     (with-wb-tree (l v r) tree
       (cond-compare (value v compare)
                     (balance-wb-tree (wb-tree-replace l value compare)
                                       v r)
                     (make-wb-tree l value r)
                     (balance-wb-tree l v
                                       (wb-tree-replace r value compare)))))
    (simple-vector (wb-tree-replace-vector tree value compare))
    (null (vector value))))


(defun wb-tree-modify-vector (tree value compare modify default)
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
                  ((< n +wb-tree-max-array-length+)
                   (array-tree-insert-at tree value i))
                  ((< i n/2)
                   (make-wb-tree (array-tree-insert-at tree value i 0 (1- n/2))
                                  (aref tree (1- n/2))
                                  (subseq tree  n/2)))
                  ((> i n/2)
                   (make-wb-tree (subseq tree 0 n/2)
                                  (aref tree n/2)
                                  (array-tree-insert-at tree value i (1+ n/2))))
                  (t ;; (= i n/2)
                   (make-wb-tree (subseq tree 0 n/2)
                                  value
                                  (subseq tree  n/2)))))
              ;; nothing to insert
              tree)))))


(defun wb-tree-modify (tree value compare modify &optional default)
  "Modify `VALUE' in `TREE', returning new tree."
  (declare (type function compare modify))
  (etypecase tree
    (wb-tree
     (with-wb-tree (l v r) tree
       (cond-compare (value v compare)
                     (balance-wb-tree (wb-tree-modify l value compare modify default)
                                       v r)
                     (multiple-value-bind (value present) (funcall modify v)
                       (if present
                           (make-wb-tree l value r)
                           (wb-tree-concatenate l r v)))
                     (balance-wb-tree l v
                                       (wb-tree-modify r value compare modify default)))))
    (simple-vector (wb-tree-modify-vector tree value compare modify default))
    (null (multiple-value-bind (value present) (funcall modify default)
            (when present (vector value))))))


(defun wb-tree-reinsert-vector (tree value compare)
  (declare (type simple-vector tree))
  (let ((i (array-tree-insert-position tree value compare)))
    (declare (type fixnum i))
    (let* ((n (length tree))
           (n/2 (ash n -1)))
      (declare (type fixnum n n/2))
      (cond
        ((< n +wb-tree-max-array-length+)
         (array-tree-insert-at tree value i))
        ((< i n/2)
         (make-wb-tree (array-tree-insert-at tree value i 0 (1- n/2))
                        (aref tree (1- n/2))
                        (subseq tree  n/2)))
        ((> i n/2)
         (make-wb-tree (subseq tree 0 n/2)
                        (aref tree n/2)
                        (array-tree-insert-at tree value i (1+ n/2))))
        (t ;; (= i n/2)
         (make-wb-tree (subseq tree 0 n/2)
                        value
                        (subseq tree  n/2)))))))


(defun wb-tree-reinsert (tree value compare)
  "Insert VALUE into TREE, returning new tree."
  (declare (type function compare))
  ;;(declare (optimize (speed 3) (safety 0)))
  (etypecase tree
    (wb-tree
     (with-wb-tree (l v r) tree
       (cond-compare (value v compare)
                     (balance-wb-tree (wb-tree-reinsert l value compare)
                                       v r)
                     (if (< (wb-tree-count l) (wb-tree-count r))
                         (balance-wb-tree (wb-tree-reinsert l value compare) v r)
                         (balance-wb-tree l v (wb-tree-reinsert r value compare)))
                     (balance-wb-tree l v
                                       (wb-tree-reinsert r value compare)))))
    (simple-vector (wb-tree-reinsert-vector tree value compare))
    (null (vector value))))


(defun wb-tree-builder (compare)
  (lambda (tree value) (wb-tree-insert tree value compare)))

(defun build-wb-tree (compare initial-tree elements)
  (fold (wb-tree-builder compare) initial-tree elements))

(defun wb-tree (compare &rest elements)
  (build-wb-tree compare nil elements))

(defun wb-tree-remove-min (tree)
  "Remove minimum element of TREE, returning element and tree."
  (labels ((rec-tree (tree)
             (with-wb-tree (l v r) tree
               (if l
                   (multiple-value-bind (new-left min)
                       (etypecase l
                         (binary-tree (rec-tree l))
                         (simple-vector (min-vector l)))
                     (values (balance-wb-tree new-left v r)
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

(defun wb-tree-remove-max (tree)
  "Remove minimum element of TREE, returning element and tree."
  (labels ((rec-tree (tree)
             (with-wb-tree (l v r) tree
               (if r
                   (multiple-value-bind (new-right max)
                       (etypecase r
                         (binary-tree (rec-tree r))
                         (simple-vector (max-vector r)))
                     (values (balance-wb-tree l v new-right)
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
  ;;   (wb-tree
  ;;    (multiple-value-bind (new-left x) (wb-tree-remove-min (binary-tree-left tree))
  ;;      (values (balance-wb-tree new-left
  ;;                                (binary-tree-value tree)
  ;;                                (binary-tree-right tree))
  ;;              x)))
  ;;   (simple-vector (case (length tree)
  ;;                    (0 (values nil nil))
  ;;                    (1 (values nil (aref tree 0)))
  ;;                    (otherwise (values (subseq tree 1) (aref tree 0)))))
  ;;   (null (values nil nil))))

;; (defun wb-tree-remove-max (tree)
;;   "Remove maximum element of TREE, returning element and tree."
;;   (etypecase tree
;;     (wb-tree
;;      (multiple-value-bind (new-right x) (wb-tree-remove-max (binary-tree-right tree))
;;        (values (balance-wb-tree (binary-tree-left tree)
;;                                  (binary-tree-value tree)
;;                                  new-right)
;;                x)))
;;     (simple-vector (let ((n (1- (length tree))))
;;                      (case n
;;                        (-1 (values nil nil))
;;                        (0 (values nil (aref tree 0)))
;;                        (otherwise (values (subseq tree 0 n) (aref tree n))))))
;;     (null (values nil nil))))


;; (defun join-wb-tree (left value right compare)
;;   (cond-wb-tree-heights (left right)
;;                          (wb-tree-insert right value compare)
;;                          (wb-tree-insert left value compare)
;;                          (balance-wb-tree (join-wb-tree left value (binary-tree-left right) compare)
;;                                            (binary-tree-value right)
;;                                            (binary-tree-right right))
;;                          (balance-wb-tree left value right)
;;                          (balance-wb-tree (binary-tree-left left)
;;                                            (binary-tree-value left)
;;                                            (join-wb-tree (binary-tree-right left) value right compare))))

;; TODO: insert array into tree


      ;; ;; TODO: build two leaf arrays in single step
      ;; ((< l-l l-r)
      ;;  (wb-tree-insert (build-wb-tree compare right left)
      ;;                   value compare))
      ;; (t
      ;;  (wb-tree-insert (build-wb-tree compare left right)
      ;;                   value compare)))))


(defun join-wb-tree (left value right compare)
  (let ((result
         (etypecase left
           (wb-tree
            (with-wb-tree (l1 v1 r1 c1) left
              (etypecase right
                (wb-tree
                 (with-wb-tree (l2 v2 r2 c2) right
                   (cond
                     ((> c2 (ash c1 +wb-tree-rebalance-log+))
                      (balance-wb-tree (join-wb-tree left value l2 compare)
                                        v2 r2))
                     ((> c1 (ash c2 +wb-tree-rebalance-log+))
                      (balance-wb-tree l1 v1
                                        (join-wb-tree r1 value right compare)))
                     (t
                        (make-wb-tree left value right)))))
                (simple-vector
                 (balance-wb-tree l1 v1 (join-wb-tree r1 value right compare)))
                (null (wb-tree-insert-max left value)))))
           (simple-vector
            (etypecase right
              (wb-tree
               (with-wb-tree (l2 v2 r2) right
                 (balance-wb-tree (join-wb-tree left value l2 compare) v2 r2)))
              (simple-vector (balance-wb-tree-array-pair left value right))
              (null (wb-tree-insert-max left value))))
           (null (wb-tree-insert-min right value)))))
    (check-wb-balance result)
    result))

(defun join-wb-tree-left (tree old-left left value right compare)
  (if (eq old-left left)
      tree
      (join-wb-tree left value right compare)))

(defun join-wb-tree-right (tree old-right left value right compare)
  (if (eq old-right right)
      tree
      (join-wb-tree left value right compare)))

(defun join-wb-tree-left-right (tree old-left old-right left value right compare)
  (if (and (eq old-left left)
           (eq old-right right))
      tree
      (join-wb-tree left value right compare)))

(defun wb-tree-concatenate (tree-1 tree-2 compare)
  "Concatenate TREE-1 and TREE-2."
  (let ((result
         (etypecase tree-1
           (wb-tree (etypecase tree-2
                       (wb-tree (with-wb-tree (l1 v1 r1) tree-1
                                   (join-wb-tree l1 v1
                                                  (wb-tree-concatenate r1 tree-2 compare)
                                                  compare)))
                       (simple-vector (with-wb-tree (l1 v1 r1) tree-1
                                        (join-wb-tree l1 v1
                                                       (wb-tree-concatenate r1 tree-2 compare)
                                                       compare)))
                       (null  tree-1)))
           (simple-vector (etypecase tree-2
                            (wb-tree (with-wb-tree (l2 v2 r2) tree-2
                                        (join-wb-tree (wb-tree-concatenate tree-1 l2 compare)
                                                       v2 r2 compare)))
                            (simple-vector (wb-tree-concatenate-array tree-1 tree-2))
                            (null tree-1)))
           (null tree-2))))
    (check-wb-balance result)
    result))

(defmacro with-wb-tree-split ((left present right) tree x compare
                               &body body)
  `(multiple-value-bind (,left ,present ,right)
       (wb-tree-split ,tree ,x ,compare)
     ,@body))

(defun wb-tree-split (tree x compare)
  (declare (type function compare))
  (etypecase tree
    (wb-tree
     (with-wb-tree (l v r) tree
       (cond-compare (x v compare)
                     (with-wb-tree-split (left-left present right-left) l x compare
                       (values left-left present (join-wb-tree right-left
                                                                v r compare)))
                     (values (binary-tree-left tree) t (binary-tree-right tree))
                     (with-wb-tree-split (left-right present right-right) r x compare
                       (values (join-wb-tree l v left-right compare)
                               present right-right)))))
    (simple-vector
     (array-tree-split tree x compare))
    (null
     (values nil nil nil))))

(defun wb-tree-midpoint (tree compare)
  (declare (type function compare))
  (let ((v (wb-tree-ref tree (ash (wb-tree-count tree) -1))))
    (multiple-value-bind (l p r)
        (wb-tree-split tree v compare)
      (assert p)
      (values l v r))))

(defun wb-tree-remove (tree x compare)
  "Remove X from TREE, returning new tree."
  (declare (type function compare))
  (etypecase tree
    (wb-tree (with-wb-tree (l v r) tree
                (cond-compare (x v compare)
                              (balance-wb-tree-left tree l
                                                     (wb-tree-remove l x compare) v r)
                              (wb-tree-concatenate l r compare)
                              (balance-wb-tree-right tree r
                                                      l v (wb-tree-remove r x compare)))))
    (simple-vector (array-tree-remove tree x compare))
    (null tree)))



(defun wb-tree-remove-position (tree i compare)
  "Remove I'th element of TREE and return (values new-tree element)."
  (declare (type fixnum i))
  (etypecase tree
    (simple-vector (values (array-tree-remove-position tree i) (aref tree i)))
    (wb-tree
     (with-wb-tree (l v r) tree
       (let ((w-l (wb-tree-count l)))
         (declare (type fixnum w-l))
         (cond
           ((< i w-l) (balance-wb-tree (wb-tree-remove-position l i compare )
                                        v r))
           ((> i w-l) (balance-wb-tree l v
                                        (wb-tree-remove-position r (- i w-l 1) compare )))
           (t (wb-tree-concatenate l r compare))))))))


(defun wb-tree-position (tree value compare)
  "Return the position of `VALUE' in `TREE' or nil."
  (declare (type function compare))
  (labels ((rec (tree i)
             (etypecase tree
               (simple-vector
                (let ((j (array-tree-position tree value compare)))
                  (if j (+ i j) nil)))
               (wb-tree
                (with-wb-tree (l v r) tree
                  (cond-compare (value v compare)
                                (rec l i)
                                (+ i (wb-tree-count l))
                                (rec r (+ i (wb-tree-count l) 1))))))))
    (rec tree 0)))



(defun wb-tree-union-array (tree-1 tree-2 compare)
  "Merge two arrays into and wb-tree"
  (declare (type simple-vector tree-1 tree-2)
           (type function compare))
           ;(optimize (speed 3) (safety 0)))
  (let* ((l-1 (length tree-1))
         (l-2 (length tree-2))
         (l-x (+ l-1 l-2))
         (n (* 2 +wb-tree-max-array-length+)))
    (with-temp-wb-array (x n)
      (assert (<= l-x n))
      (do ((i 0)
           (j 0)
           (k 0 (1+ k)))
          ((and (= i l-1)
                (= j l-2))
           ;; RESULT
           (cond ((> k +wb-tree-max-array-length+)
                  (multiple-value-call #'make-wb-tree
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

(defun wb-tree-union-tree-vector (tree vector compare &optional (start 0) (end (length vector)))
  (declare (type wb-tree tree)
           (type simple-vector vector)
           (type function compare)
           (type fixnum start end))
  (do ((tree tree)
       (i start (1+ i)))
      ((>= i end) tree)
    (declare (type fixnum i))
    (setq tree (wb-tree-insert tree (aref vector i) compare))))

(defun wb-tree-union (tree-1 tree-2 compare)
  (declare (type function compare))
  (etypecase tree-1
    (wb-tree (etypecase tree-2
                (wb-tree
                 ;; normalize sizes, faster to split the smaller tree
                 (multiple-value-bind (tree-1 tree-2)
                     (if (<= (wb-tree-weight tree-2)
                             (wb-tree-weight tree-1))
                         (values tree-1 tree-2)
                         (values tree-2 tree-1))
                   (with-wb-tree (l1 v1 r1) tree-1
                     (multiple-value-bind (l2 p-2 r2) (wb-tree-split tree-2 v1 compare)
                       (declare (ignore p-2))
                       (join-wb-tree-left-right tree-1 l1 r1
                                           (wb-tree-union l1 l2 compare)
                                           v1
                                           (wb-tree-union r1 r2 compare)
                                           compare)))))
                (simple-vector (wb-tree-union-tree-vector tree-1 tree-2 compare))
                (null tree-1)))
    (simple-vector (etypecase tree-2
                     (wb-tree (wb-tree-union-tree-vector tree-2 tree-1 compare))
                     (simple-vector (wb-tree-union-array tree-1 tree-2 compare))
                     (null tree-1)))
    (null tree-2)))

(defun wb-tree-array-intersection (tree1 tree2 compare)
  (declare (type simple-vector tree1 tree2)
           (type function compare))
  (let ((l-1 (length tree1))
        (l-2 (length tree2)))
    (with-temp-wb-array (array)
      (assert (and (<= l-1 +wb-tree-max-array-length+)
                   (<= l-2 +wb-tree-max-array-length+)))
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

(defun wb-tree-intersection-tree-array (tree array compare)
  (declare (type simple-vector array))
  (let* ((l-v (length array)))
    (with-temp-wb-array (new-array)
      (assert (<= l-v +wb-tree-max-array-length+))
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

(defun wb-tree-intersection (tree-1 tree-2 compare)
  (etypecase tree-1
    (wb-tree (etypecase tree-2
                (wb-tree
                 (multiple-value-bind (tree-1 tree-2) ;; normalize sizes, faster to split the smaller tree
                     (if (<= (wb-tree-weight tree-2)
                             (wb-tree-weight tree-1))
                         (values tree-1 tree-2)
                         (values tree-2 tree-1))
                   (with-wb-tree (l1 v1 r1) tree-1
                     (multiple-value-bind (l2 present r2)
                         (wb-tree-split tree-2 v1 compare)
                       (let ((l-i (wb-tree-intersection l1 l2 compare))
                             (r-i (wb-tree-intersection r1 r2 compare)))
                         (let ((result
                                (if present
                                    (join-wb-tree l-i v1 r-i compare)
                                    (wb-tree-concatenate l-i r-i compare))))
                           ;(unless (wb-tree-balanced-p result)
                             ;(print (list l-i v1 present r-i))
                             ;(assert nil))

                           result))))))
                (simple-vector (wb-tree-intersection-tree-array tree-1 tree-2 compare))
                (null nil)))
    (simple-vector (etypecase tree-2
                     (wb-tree (wb-tree-intersection-tree-array tree-2 tree-1 compare))
                     (simple-vector (wb-tree-array-intersection tree-1 tree-2 compare))
                     (null nil)))
    (null nil)))

(defun wb-tree-difference (tree-1 tree-2 compare)
  (declare (type function compare))
  (etypecase tree-1
    (wb-tree (etypecase tree-2
                (wb-tree
                 (with-wb-tree (l1 v1 r1) tree-1
                   (multiple-value-bind (left-2 present right-2)
                       (wb-tree-split tree-2 v1 compare)
                     (let ((left (wb-tree-difference l1 left-2 compare))
                           (right (wb-tree-difference r1 right-2 compare)))
                       (if present
                           (wb-tree-concatenate left right compare)
                           (join-wb-tree-left-right tree-1 l1 r1
                                                     left v1 right compare))))))
                (simple-vector
                 (fold (lambda (tree x)
                         (wb-tree-remove tree x compare))
                       tree-1 tree-2))
                (null tree-1)))
    (simple-vector (etypecase tree-2
                     (wb-tree (with-temp-wb-array (new-array)
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
                     (simple-vector (with-temp-wb-array (new-array)
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

(defun wb-tree-intersection-difference (tree-1 tree-2 compare)
  (declare (type function compare))
  (etypecase tree-1
    (wb-tree (etypecase tree-2
                (wb-tree
                 (with-wb-tree (l1 v1 r1) tree-1
                   (multiple-value-bind (l2 present r2)
                       (wb-tree-split tree-2 v1 compare)
                     (multiple-value-bind (l-i l-d) (wb-tree-intersection-difference l1 l2 compare)
                       (multiple-value-bind (r-i r-d) (wb-tree-intersection-difference r1 r2 compare)
                         (if present
                             (values (join-wb-tree l-i v1 r-i compare)
                                     (wb-tree-concatenate l-d r-d compare))
                             (values (wb-tree-concatenate l-i r-i compare)
                                     (join-wb-tree l-d (binary-tree-value tree-1) r-d compare))))))))
                (t (values (wb-tree-intersection tree-1 tree-2 compare)
                           (wb-tree-difference tree-1 tree-2 compare)))))
    (t (values (wb-tree-intersection tree-1 tree-2 compare)
               (wb-tree-difference tree-1 tree-2 compare)))))



(defun wb-tree-subset (tree-1 tree-2 compare)
  (declare (type function compare))
  (labels ((rec (tree-1 tree-2)
             (cond
               ((> (wb-tree-count tree-1)
                   (wb-tree-count tree-2))
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
                (with-wb-trees
                    (l1 v1 r1) tree-1
                    (l2 v2 r2) tree-2
                  (cond-compare (v1 v2 compare)
                                ;; v1 < v2
                                (and (rec (make-wb-tree l1 v1 nil)
                                          l2)
                                     (rec r1 tree-2))
                                ;; v1 = v2
                                (and (rec l1 l2)
                                     (rec r1 r2))
                                ;; v1 > v2
                                (and (rec (make-wb-tree nil v1 r1)
                                          r2)
                                     (rec l1 tree-2))))))))
    (rec tree-1 tree-2)))


(declaim (ftype (function ((or wb-tree array null) (or wb-tree array null) function)
                          fixnum)
                wb-tree-compare))
(defun wb-tree-compare (tree-1 tree-2 compare)
  (declare (type function compare))
  ;; O(log(n)) space, O(min(m,n)) time
  (cond
    ((eq tree-1 tree-2) 0)
    ((and tree-1 tree-2)
     (let ((n1 (wb-tree-count tree-1))
           (n2 (wb-tree-count tree-2)))
       (cond  ;; first, order by count since that's O(1)
         ((< n1 n2) -1)
         ((> n1 n2) 1)
         ;((< n1 +wb-tree-max-array-length+)
         ((and (simple-vector-p tree-1)
               (simple-vector-p tree-2))
          (let ((i (array-tree-compare tree-1 tree-2 compare)))
            i))
         (t
          ;; try binary again
          ;(binary-tree-compare tree-1 tree-2 compare)))))
          (multiple-value-bind (l1 m1 r1)
              (wb-tree-midpoint tree-1 compare)
            (multiple-value-bind (l2 m2 r2)
                (wb-tree-midpoint tree-2 compare)
              (or-compare (funcall compare m1 m2)
                          (wb-tree-compare l1 l2 compare)
                          (wb-tree-compare r1 r2 compare))))))))
    (tree-1 1)
    (tree-2 -1)
    (t 0)))


(defun wb-tree-dot (tree &key output)
  (binary-tree-dot tree
                   :output output
                   :node-label-function (lambda (node)
                                          (format nil "~A (~D)"
                                                  (binary-tree-value node)
                                                  (wb-tree-weight node)))))
