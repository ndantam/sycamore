;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2012, Georgia Tech Research Corporation
;;;; Copyright (c) 2025, Colorado School of Mines
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

;(declaim (optimize (speed 3) (safety 0)))

;;;;;;;;;;;;;;;;;;;;
;; Top containter ;;
;;;;;;;;;;;;;;;;;;;;
(defstruct (root-tree (:constructor %make-aux-tree (%compare root)))
  %compare
  (root nil))


(defun make-aux-compare (compare)
  (declare (type function compare))
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


(defun empty-tree-map (tree-map)
  "Create a new empty tree-map."
  (%make-tree-map (tree-map-compare tree-map)
                  nil))

(defun tree-map-insert (tree-map key value)
  "Insert KEY=>VALUE into TREE-MAP, returning the new tree-map."
  (%make-tree-map (tree-map-compare tree-map)
                  (wb-tree-replace (tree-map-root tree-map)
                                    (cons key value)
                                    (tree-map-compare tree-map))))

(defun (setf tree-map-find) (value map key)
  "Destructively insert value item into map."
  (setf (tree-map-root map)
        (wb-tree-replace (tree-map-root map)
                         (cons key value)
                         (tree-map-compare map))))


(defmacro tree-map-insertf (place key value)
  "Insert KEY=>VALUE into the tree map at PLACE, store at place."
  `(progn
     (setf ,place
           (tree-map-insert ,place ,key ,value))))

(defun tree-map-remove (tree-map key)
  "Insert KEY from TREE-MAP, returning the new tree-map."
  (%make-tree-map (tree-map-compare tree-map)
                  (wb-tree-remove (tree-map-root tree-map)
                                   (cons key nil)
                                   (tree-map-compare tree-map))))

(defun tree-map-find (tree-map key &optional default)
  "Find value indexed by KEY in TREE-MAP."
  (let ((map-key (cons key nil)))
    (declare (dynamic-extent map-key))
    (multiple-value-bind (cons present)
        (binary-tree-find (tree-map-root tree-map)
                          map-key
                          (tree-map-compare tree-map))
      (if present
          (values (cdr cons) (car cons) t)
          (values default key nil)))))

(defun tree-map-contains (tree-map key)
  "Test if a key is present in tree-map"
  (let ((key (cons key nil)))
    (declare (dynamic-extent key))
    (binary-tree-member-p (tree-map-root tree-map)
                          key
                          (tree-map-compare tree-map))))

(defun map-tree-map (order result-type function tree-map)
  "Apply FUNCTION to all elements in TREE-MAP.
ORDER: (or :inorder :preorder :postorder).
RESULT-TYPE: (or nil 'list).
FUNCTION: (FUNCTION key value) -> result"
  (declare (type function function))
  (let ((result
         (flet ((helper (pair)
                  (funcall function (car pair) (cdr pair))))
           (declare (dynamic-extent (function helper)))
           (map-binary-tree order (if (eq result-type 'tree-map)
                                      'list
                                      result-type)
                            #'helper
                            (tree-map-root tree-map)))))
    (when result-type
      (ecase result-type
        (list result)
        (treemap
         (assert nil))))))

(defmacro do-tree-map (((key value) map &optional result) &body body)
  `(progn
     (map-tree-map :inorder nil
                   (lambda (,key ,value)
                     ,@body)
                   ,map)
     ,result))

(defun fold-tree-map (function initial-value tree-map)
  "Fold FUNCTION over members of the map
FUNCTION: (lambda (accumulated-value key value))."
  (declare (type function function))
  (flet ((helper (accum pair)
           (funcall function accum (car pair) (cdr pair))))
    (declare (dynamic-extent (function helper)))
    (fold-binary-tree :inorder #'helper
                      initial-value (tree-map-root tree-map))))

(defun tree-map-count (map)
  "Number of elements in MAP."
  (wb-tree-count (tree-map-root map)))

(defun tree-map-insert-map (tree-map other-map)
  "Insert all elements of OTHER-MAP into TREE-MAP"
  (assert (eq (tree-map-compare tree-map)
              (tree-map-compare other-map)))
  (fold-tree-map (lambda (map key value)
                   (tree-map-insert map key value))
                 tree-map
                 other-map))

(defun tree-map-insert-alist (tree-map alist)
  "Insert all elements of ALIST into TREE-MAP"
  (fold (lambda (map elt) (tree-map-insert map (car elt) (cdr elt)))
        tree-map alist))

(defun alist-tree-map (alist compare)
  "Returns a tree-map containing the keys and values of the association list ALIST."
  (tree-map-insert-alist (make-tree-map compare)
                         alist))

(defun tree-map-insert-hash-table (tree-map hash-table)
  "Insert all elements of HASH-TABLE into TREE-MAP"
  (flet ((helper (key value)
           (setf (tree-map-find tree-map key) value)))
    (declare (dynamic-extent (function helper)))
    (maphash #'helper hash-table))
  tree-map)

(defun hash-table-tree-map (hash-table compare)
  "Returns a tree-map containing the keys and values of the hash-table list HASH-TABLE."
  (tree-map-insert-hash-table (make-tree-map compare) hash-table))

(defun tree-map-alist (tree-map)
  "Returns an association list containging the keys and values of tree-map TREE-MAP."
  (declare (type tree-map tree-map))
  (map-tree-map :inorder 'list #'cons tree-map))

(defun tree-map-hash-table (tree-map &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the tree-map TREE-MAP.
Hash table is initialized using the HASH-TABLE-INITARGS."
  (declare (type tree-map tree-map)
           (dynamic-extent hash-table-initargs))
  (fold-tree-map (lambda (hash key value)
                   (setf (gethash key hash) value)
                   hash)
                 (apply #'make-hash-table hash-table-initargs)
                 tree-map))


(defun tree-map-values (tree-map)
  (fold-tree-map (lambda (a k v)
                   (declare (ignore k))
                   (cons v a))
                 nil tree-map))

(defun tree-map-keys (tree-map)
  (fold-tree-map (lambda (a k v)
                   (declare (ignore v))
                   (cons k a))
                 nil tree-map))


(defmethod print-object ((object tree-map) stream)
  (print-unreadable-object (object stream :type t :identity nil)

    ;; (write (tree-map-alist object)
    ;;        :stream stream)

    ;; (format stream "~@<{~;~:{{~A: ~A}~^ ~}~;}~:@>"
    ;;         (map-tree-map :inorder 'list #'list object))

    (pprint-logical-block (stream (map-tree-map :inorder 'list #'list object)
                                  :prefix "{" :suffix "}")
      (do () (nil)
        (pprint-exit-if-list-exhausted)
        (let ((x (pprint-pop)))
          (pprint-newline :fill stream)
          (pprint-indent :block 0 stream)
          (pprint-logical-block (stream x)
            (write (car x) :stream stream)
            (write-char #\: stream)
            (write-char #\Space stream)
            (write (cadr x) :stream stream))
          (pprint-exit-if-list-exhausted)
          (write-char #\, stream)
          (write-char #\Space stream))))))

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

(defun empty-tree-set (tree-set)
  "Create a new empty tree-set."
  (%make-tree-set (tree-set-%compare tree-set)
                  nil))

(defun tree-set (compare &rest args)
  "Create a new tree-set containing all items in ARGS."
  (flet ((helper (tree x)
           (wb-tree-insert tree x compare)))
    (declare (dynamic-extent (function helper)))
    (%make-tree-set compare
                    (fold #'helper nil args))))

(defun tree-set-count (set)
  "Number of elements in SET."
  (wb-tree-count (tree-set-root set)))

(defun map-tree-set (result-type function set)
  "Apply FUNCTION to every element of SET."
  (map-binary-tree :inorder result-type function (when set (tree-set-root set))))

(defmacro do-tree-set ((var set &optional result) &body body)
  (with-gensyms (helper)
    `(progn
       (flet ((,helper (,var)
                ,@body))
         (declare (dynamic-extent (function ,helper)))
         (map-tree-set nil #',helper ,set))
       ,result)))

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

(def-tree-set-item-op tree-set-replace wb-tree-replace
  "Replace ITEM into SET.")

(def-tree-set-item-op tree-set-remove wb-tree-remove
  "Remove ITEM from SET.")

(defmacro tree-set-insertf (place item)
  "Insert INTER into the tree set at PLACE, store at PLACE."
  `(progn
     (setf ,place
           (tree-set-insert ,place ,item))))

(defun (setf tree-set-find) (item set)
  "Destructively insert item into set."
  (setf (tree-set-%root set)
        (wb-tree-replace (tree-set-%root set) item
                         (tree-set-%compare set))))

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
  "Is SET-1 a subset of SET-2?
RETURNS: T or NIL"
  (wb-tree-subset (tree-set-root set-1)
                  (tree-set-root set-2)
                  (tree-set-%compare set-1)))

(defun tree-set-intersection-p (set-1 set-2)
  "Do SET-1 and SET-2 intersect?
RETURNS: T or NIL"
  (wb-tree-intersection-p (tree-set-root set-1)
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

(defun tree-set-max (set)
  "Return the greatest item in SET."
  (binary-tree-max (tree-set-root set)))

(defun tree-set-min (set)
  "Return the lest item in SET."
  (binary-tree-min (tree-set-root set)))

(defmethod print-object ((object tree-set) stream)
  (print-unreadable-object (object stream :type t :identity nil)
                                        ;(print (tree-set-list object))
    ;; Use format instead
    ;; (pprint-logical-block (stream (tree-set-list object) :prefix "{" :suffix "}")
    ;;   (pprint-logical-block (stream (tree-set-list object) :prefix "{" :suffix "}")
    ;;     (do () (nil)
    ;;       (pprint-exit-if-list-exhausted)
    ;;       (let ((x (pprint-pop)))
    ;;         (pprint-newline :fill stream)
    ;;         (pprint-indent :block 0 stream)
    ;;         (write-char #\Space stream)
    ;;         (write x :stream stream)))))

    ;; Doesn't get the spaces right
    ;; (do-tree-set (x object)
    ;;   (pprint-newline :fill stream)
    ;;   (pprint-indent :block 0 stream)
    ;;   (write-char #\Space stream)
    ;;   (write x :stream stream))

    (format stream "~@<{~;~{~A~^ ~}~;}~:@>"
            (tree-set-list object))))


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


;;;;;;;;;;;;;;
;; Hash-Set ;;
;;;;;;;;;;;;;;

(defstruct (hash-set (:constructor %make-hash-set (%functions root)))
  (%functions (cons nil nil) :type cons)
  root)

(defmacro %with-hash-set ((hash-function test) hash-set &body body)
  (with-gensyms (c)
    `(let ((,c (hash-set-%functions ,hash-set)))
       (let ((,hash-function (car ,c))
             (,test (cdr ,c)))
         ,@body))))

(declaim (inline %set-hash-set))
(defun %set-hash-set (set root)
  (%make-hash-set (hash-set-%functions set)
                  root))

(declaim (inline %update-hash-set))
(defun %update-hash-set (set root)
  (if (eq (hash-set-root set)
          root)
      set
      (%set-hash-set set root)))


(defun %hash-function (test)
  "Attempt to find a hash function for TEST"
  (cond
    ;; SXHASH works for EQL, EQUAL, EQ
    ((or (eq test #'eql)
         (eq test #'equal)
         (eq test #'eq))
     #'sxhash)
    ;; SXHASH DOES NOT work for EQUALP and =. Use SBCL's PSXHASH when
    ;; we can.
    #+sbcl
    ((or (eq test #'equalp)
         (eq test #'=))
     #'sb-impl::psxhash)
    ;; Other test.  The caller needs to provide a hash function.
    (t (error (format nil "Unknown :HASH-FUNCTION for :TEST ~A" test)))))

(let ((c-eql (cons (%hash-function #'eql) #'eql))
      (c-equal (cons (%hash-function #'equal) #'equal))
      (c-eq (cons (%hash-function #'eq) #'eq))
      #+sbcl
      (c-equalp (cons (%hash-function #'equalp) #'equalp))
      #+sbcl
      (c-= (cons (%hash-function #'=) #'=)))
  (defun %hash-set-functions (hash-function test key)
    (cond
      (key ; No descriptor sharing with keys.  But caller can create a
           ; base empty instance to share.
       (cons (let ((f (or hash-function (%hash-function test))))
               (lambda (item)
                 (funcall f
                          (funcall key item))))
             (lambda (item-1 item-2)
               (funcall test
                        (funcall key item-1)
                        (funcall key item-2)))))
      ((null hash-function)
        (cond ; try to share a function descriptor
          ((eq test #'eql) c-eql)
          ((eq test #'equal) c-equal)
          ((eq test #'eq) c-eq)
          #+sbcl
          ((eq test #'equalp) c-equalp)
          #+sbcl
          ((eq test #'=) c-=)
          (t (cons (%hash-function test)
                   test))))
      (t ; General
       (cons hash-function test)))))


(defun make-hash-set (&key (test #'eql) hash-function key)
  "Create and return new HASH-SET.  The keywords are as follows:

  :TEST
    Function to compare items: (TEST item-1 item-2) -> BOOLEAN.

  :HASH-FUNCTION
    Function to compute hash codes:
    (HASH-FUNCTION item) -> non-negative-fixnum.
    If not provided, MAKE-HASH-SET will try to find a valid function
    for TEST.

  :KEY
    If provided, apply `TEST' and `HASH-FUNCTION' to `(FUNCALL KEY
    ITEM)'.

Note 1: It is required that when (TEST ITEM-1 ITEM-2) is true,
HASH-FUNCTION will return the same value for both ITEM-1 and ITEM-2.

Note 2: ANSI Common Lisp DOES NOT require #'SXHASH to return the same
value for two numbers that are #'= but of different types, or for two
objects that are #'EQUALP, but not #'EQUAL. MAKE-HASH-SET will attempt
to use implementation-specific hashing functions for those
tests. However, if either the implementation specific function is not
known to MAKE-HASH-SET or the caller is using some other TEST, then
the caller must provide a valid HASH-FUNCTION."
  (declare (type function test)
           (type (or function null) hash-function key))
  (%make-hash-set (%hash-set-functions hash-function test key)
                  nil))

(defun hash-set-find (set item &optional default)
  "Search `SET' for `ITEM'.
RETURNS: (values ITEM T) if `ITEM' is in `SET', or
         (values DEFAULT NIL) if `ITEM' is not in `SET'."
  (if-let ((root (hash-set-root set)))
    (multiple-value-bind (result present)
        (%with-hash-set (hash-function test) set
          (hamt-set-find root item
                         (funcall hash-function item)
                         test))
      (if present
          (values result t)
          (values default nil)))
    (values default nil)))

(defun hash-set-member-p (set item)
  "Test if `ITEM' is a member of `SET'
RETURNS: T if `ITEM' is in `SET', or
         NIL if `ITEM' is not in `SET'."
  (multiple-value-bind (elt has) (hash-set-find set item)
    (declare (ignore elt))
    has))

(defun hash-set-empty-p (set)
  "Test if `SET' is empty.
RETURNS: T if `SET' contains zero items, or
         NIL `SET' contains some items."
  (if (hash-set-root set)
      t
      nil))

(defun hash-set-insert (set item)
  "Insert `ITEM' into `SET'."
  (%with-hash-set (hash-function test) set
    (let ((hash-code (funcall hash-function item)))
      (if-let ((root (hash-set-root set)))
        ;; Non-empty
        (%update-hash-set set
                          (hamt-set-insert root item
                                           hash-code
                                           test))
        ;; empty, make a singleton
        (%set-hash-set set
                       (hamt-set-layer-singleton hash-code item))))))

(defun hash-set-remove (set item)
  "Remove `ITEM' from `SET'."
  (if-let ((root (hash-set-root set)))
    ;; remove it
    (%with-hash-set (hash-function test) set
      (%update-hash-set set
                        (hamt-set-remove root item
                                         (funcall hash-function item)
                                         test)))
    ;; Set is empty, just return it
    set))

(defun hash-set-union (set-1 set-2)
  "Union of `SET-1' and `SET-2'."
  (if-let ((root-1 (hash-set-root set-1)))
    (if-let ((root-2 (hash-set-root set-2)))
      (%with-hash-set (hash-function test) set-1
        (declare (ignore hash-function))
        (%set-hash-set set-1
                       (hamt-set-union root-1 root-2 test)))
      ;; null root-2
      set-1)
    ;; null root-1
    set-2))

(defun hash-set-intersection (set-1 set-2)
  "Intersection of `SET-1' and `SET-2'."
  (if-let ((root-1 (hash-set-root set-1)))
    (if-let ((root-2 (hash-set-root set-2)))
      (%with-hash-set (hash-function test) set-1
        (declare (ignore hash-function))
        (%set-hash-set set-1
                       (hamt-set-intersection root-1 root-2 test)))
      ;; null root-2
      set-2)
    ;; null root-1
    set-1))

(defun list-hash-set (list &key (test #'eql) hash-function key)
  "Construct a hash-set from a list of elements."
  (declare (type function test)
           (type (or function null) hash-function key))
  (let ((set (%make-hash-set (%hash-set-functions hash-function test key)
                             nil)))
    (%with-hash-set (hash-function test) set
      (when list
        (let ((root (let ((elt (car list)))
                      (hamt-set-layer-singleton (funcall hash-function elt)
                                                elt))))
          (dolist (elt (cdr list))
            (setq root (hamt-set-ninsert root elt
                                         (funcall hash-function elt) test)))
          (setf (hash-set-root set) root))))
    set))

(defun hash-set-list (set)
  "Return a list of elements in `SET'."
  (hamt-set-fold-right #'cons (hash-set-root set) nil))

(defun map-hash-set (result-type function set)
  "Apply `FUNCTION' to all elements in `SET'.
RESULT-TYPE: (or nil 'list 'vector)
FUNCTION: (lambda (element))."
  (hamt-set-map result-type function
                (hash-set-root set)))

(defun fold-hash-set (function initial-value set)
  "Fold `FUNCTION' over every element of `SET'.
FUNCTION: (lambda (initial-value element)) -> next-initial-value.
INITIAL-VALUE: value passed as first argument to initial call to `FUNCTION'.
SET: a hash-set whose elements are past as the second argument in each call to `FUNCTION'."
  (hamt-set-fold-left function initial-value
                      (hash-set-root set)))

(defun fold-right-hash-set (function set initial-value)
  "Fold `FUNCTION' over every element of `SET'.
FUNCTION: (lambda (element initial-value)) -> next-initial-value.
SET: a hash-set whose elements are past as the first argument in each call to `FUNCTION'.
INITIAL-VALUE: value passed as second argument to initial call to `FUNCTION'."
  (hamt-set-fold-left function initial-value
                      (hash-set-root set)))

(defmethod print-object ((object hash-set) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~@<{~;~{~A~^ ~}~;}~:@>"
            (hash-set-list object))))

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
