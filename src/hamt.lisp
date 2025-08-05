;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2025, Colorado School of Mines
;;;;
;;;; All rights reserved.
;;;;
;;;; Author(s): Neil T. Dantam <ndantam@mines.edu>
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

;; (declaim (optimize (speed 3) (safety 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Bit Operations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +hamt-bits+
  ;; Try to keep the bitmap as a fixnum.  Do fixnums have 32 bits we
  ;; can use?
  (if (< (expt 2 32) most-positive-fixnum)
      5
      4))
(defconstant +hamt-size+ (expt 2 +hamt-bits+))
(defconstant +hamt-mask+ (1- +hamt-size+))
(defconstant +hamt-maxdepth+ (1+ (ceiling (/ (integer-length most-positive-fixnum)
                                             +hamt-bits+))))

(deftype hamt-bitmap () `(unsigned-byte ,(expt 2 +hamt-bits+)))
(deftype hamt-depth () `(unsigned-byte ,(integer-length +hamt-maxdepth+)))
(deftype hamt-bit () `(unsigned-byte ,+hamt-bits+))
(deftype hamt-bit-depth ()
  `(unsigned-byte ,(integer-length (integer-length most-positive-fixnum))))
(deftype hamt-index () `(unsigned-byte ,(1+ +hamt-bits+)))
(deftype hamt-hash-code () `non-negative-fixnum)

(declaim (inline hamt-popcount))
(defun hamt-popcount (n)
  (declare (optimize (speed 3) (safety 0))
           (type hamt-bitmap n))
  (the non-negative-fixnum (%bit-popcnt n)))

(declaim (inline hamt-bitmap-least))
(defun hamt-bitmap-least (bitmap)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum bitmap))
  (the hamt-bit (%bit-ctz bitmap)))

(declaim (inline hamt-bit))
(defun hamt-bit (hash-code)
  (declare (optimize (speed 3) (safety 0))
           (type hamt-hash-code hash-code))
  (the hamt-bit (ldb (byte +hamt-bits+ 0) hash-code)))

(declaim (inline hamt-index))
(defun hamt-index (bitmap bit)
  (declare (optimize (speed 3) (safety 0))
           (type hamt-bitmap bitmap)
           (type hamt-bit bit))
  (let (;; (b (logandc2 bitmap (ash -1 bit)))
        ;; (b (logand bitmap (1- (ash 1 bit))))
        (b (ldb (byte bit 0) bitmap))
        )
    (1+ (hamt-popcount b))))


;; Count depths by the number of bits into the hash-code (vs. count of
;; layers traversed).
(declaim (inline hamt-subhash-bit-depth))
(defun hamt-bit-depth-subhash (hash-code bit-depth)
  (declare (optimize (speed 3) (safety 0))
           (type hamt-hash-code hash-code)
           (type hamt-bit-depth bit-depth))
  (the hamt-hash-code
       (ash hash-code (the non-positive-fixnum (- bit-depth)))))

(declaim (inline hamt-subhash))
(defun hamt-subhash (hash-code)
  (declare (optimize (speed 3) (safety 0))
           (type hamt-hash-code hash-code))
  (ash hash-code (- +hamt-bits+)))


;;;;;;;;;;;;;
;;; Types ;;;
;;;;;;;;;;;;;

;;; Layers
;;;
;;; Represent layers with simple vectors to avoid indirection of a
;;; separate table.  The first element is the bitmap.  Succesive
;;; elements are the layer's children.
;;;
;;; Entry hashes and keys are stored in cons cells or structs
;;; referenced from the layer array.  Storing outside the layer means
;;; the layer itself needs only one pointer for each child.  We pay
;;; once for the indirection when we reach the leaf, but we save on
;;; reallocation per-layer compared to storing both hashes and keys in
;;; the layer array.

(deftype hamt-layer () 'simple-vector)

(declaim (inline hamt-layer-ref))
(defun hamt-layer-ref (layer index)
  (declare (type hamt-layer layer))
  (aref layer index))

(declaim (inline hamt-layer-bitmap))
(defun hamt-layer-bitmap (layer)
  (declare (type hamt-layer layer))
  (the hamt-bitmap (aref layer 0)))

(declaim (inline hamt-layer-update))
(defun hamt-layer-update (layer index thing)
  (declare (type hamt-index index)
           (type hamt-layer layer))
  (let ((new-layer (make-array (length layer))))
    (replace new-layer layer :end2 index)
    (setf (aref new-layer index) thing)
    (let ((j (1+ index)))
      (replace new-layer layer :start1 j :start2 j))
    new-layer))

(declaim (inline hamt-layer-test-update))
(defun hamt-layer-test-update (layer index old-thing new-thing)
  (if (eq old-thing new-thing)
      layer
      (hamt-layer-update layer index new-thing)))

(declaim (inline hamt-layer-remove))
(defun hamt-layer-remove (layer bit index)
  (declare (type hamt-layer layer)
           (type hamt-bit bit)
           (type hamt-index index))
  (let* ((n (length layer)))
    (flet ((f ()
             (let* ((n1 (1- n))
                    (new-layer (make-array n1)))
               (setf (aref new-layer 0)
                     (logandc2 (hamt-layer-bitmap layer)
                               (ash 1 bit)))
               (replace new-layer layer :start1 1 :end1 index :start2 1)
               (replace new-layer layer :start1 index :start2 (1+ index))
               new-layer)))

      (cond
        ;; Removing the only element
        ((= n 2)
         nil)
        ((= n 3)
         ;; Downgrade layers with a single entry/bucket to just the
         ;; entry/bucket.  But keep layers with a single sublayer
         ;; as-is.
         (let ((thing (aref layer (- n index))))
           (typecase thing
             (hamt-layer
              (f))
             (t thing))))
        (t
         ;; general case
         (f))))))

(declaim (inline hamt-layer-open))
(defun hamt-layer-open (layer index)
  (declare (type fixnum index)
           (type hamt-layer layer))
  (let ((new-layer (make-array (length layer))))
    (replace new-layer layer :end2 index)
    (let ((j (1+ index)))
      (replace new-layer layer :start1 j :start2 j))
    new-layer))

(declaim (inline hamt-layer-nupdate))
(defun hamt-layer-nupdate (layer index thing)
  (declare (hamt-layer layer))
  (setf (aref layer index) thing)
  layer)

(declaim (inline hamt-layer-insert))
(defun hamt-layer-insert (layer bit index thing)
  (declare (type simple-vector layer)
           (type hamt-bit bit)
           (type positive-fixnum index))
  ;;(declare (optimize (speed 3) (safety 0)))
  (let* ((n (length layer))
         (new-layer (make-array (1+ n))))
    ;; update bitmap
    (setf (aref new-layer 0)
          (logior (the hamt-bitmap (aref layer 0))
                  (ash 1 bit)))
    (replace new-layer layer :start1 1 :start2 1 :end2 index)
    (setf (aref new-layer index) thing)
    (replace new-layer layer :start1 (1+ index) :start2 index)
    new-layer))

(defmacro if-hamt-present ((bit &optional index thing) (layer subhash)
                           present-case not-present-case)
  (with-gensyms (layer-var present bitmap)
    `(let* ((,layer-var ,layer)
            (,bit (hamt-bit ,subhash))
            (,bitmap (hamt-layer-bitmap ,layer-var))
            (,present (logbitp ,bit ,bitmap))
            ,@(when index `((,index (hamt-index ,bitmap ,bit)))))
       (if ,present
           (let* (,@(when thing `((,thing (aref ,layer-var ,index)))))
             ,present-case)
           ,not-present-case))))

(defmacro do-hamt-bits ((var bitmap &optional result) &body body)
  (with-gensyms (bitmap-var)
    `(do ((,bitmap-var ,bitmap))
         ((zerop ,bitmap-var) ,result)
       (let ((,var (hamt-bitmap-least ,bitmap-var)))
         (progn ,@body)
         (setq ,bitmap-var (logandc2 ,bitmap-var (ash 1,var)))))))

(declaim (inline hamt-layer-1))
(defun hamt-layer-1 (bitmap entry)
  (vector bitmap entry))

(declaim (inline hamt-layer-2))
(defun hamt-layer-2 (bitmap entry-1 entry-2)
  (vector bitmap entry-1 entry-2))

(declaim (inline hamt-layer-singleton))
(defun hamt-layer-singleton (entry subhash)
  (vector (ash 1 (hamt-bit subhash))
          entry))

(declaim (inline hamt-layer-finish))
(defun hamt-layer-finish (bitmap temp-layer)
  (declare (type hamt-bitmap bitmap)
           (type hamt-layer temp-layer))
  (let ((j (hamt-popcount bitmap)))
    (flet ((finish ()
             (let ((result (subseq temp-layer 0 (1+ j))))
               (setf (aref result 0) bitmap)
               result)))
      (declare (dynamic-extent (function finish)))
      (cond ((> j 1) (finish))
            ((= j 1)
             ;; Try to downgrade singleton layers to entries
             ;; or buckets.  However, if the singleton layer
             ;; contains a sublayer, keep the layer as-is.
             (let ((thing (aref temp-layer 1)))
               (etypecase thing
                 (hamt-layer (finish))
                 (hamt-set-entry thing)
                 (hamt-bucket thing))))
            (t nil)))))

;;; Buckets
;;;
;;; Handle hash collisions via chaining.
;;;
;;; Hash collisions ought to be infrequent, so buckets get to be a
;;; separate and indirected structure.
;;;
;;; Note: HAMTs only need to handle collisions when the underlying
;;; hash function collides because we use the full hash-code. A decent
;;; hash function will rarely collide, so HAMTs will rarely need to
;;; handle collisions.  In contrast, mutable hash tables use fewer
;;; bits of the hash (to index the table), so must more frequently
;;; handle collisions.  The reduced frequency of collision-handling in
;;; the HAMT may lead to improvided lookup times compared to mutable
;;; hash-tables.

(defstruct (hamt-bucket (:constructor hamt-bucket (hash-code list)))
  (hash-code 0 :type hamt-hash-code)
  (list nil :type list))

;;;;;;;;;;;;;;;;;;;;;;
;;; Set Operations ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Avoid storing separate values for sets.  Non-colliding entries just
;; need the hash-code and key.

;;; Seems that cons cells have lower overhead that structs.  We'll
;;; have many hamt-entries, so let's store those as cons's.
(deftype hamt-set-entry () 'cons)
(declaim (inline hamt-set-entry
                 hamt-set-entry-hash-code
                 hamt-set-entry-key))
(defun hamt-set-entry (hash-code key) (cons hash-code key))
(defun hamt-set-entry-hash-code (entry) (the fixnum (car entry)))
(defun hamt-set-entry-key (entry) (cdr entry))

(declaim (inline hamt-set-bucket*))
(defun hamt-set-bucket* (hash-code list)
  (declare (type hamt-hash-code hash-code)
           (type list list))
  (cond
    ((null list) nil)
    ((null (cdr list)) (hamt-set-entry hash-code (car list)))
    (t (hamt-bucket hash-code list))))

(declaim (inline hamt-set-entry-=))
(defun hamt-set-entry-= (e1 e2 test)
  (and (= (hamt-set-entry-hash-code e1)
          (hamt-set-entry-hash-code e2))
       (funcall test
                (hamt-set-entry-key e1)
                (hamt-set-entry-key e2))))

(declaim (inline hamt-bucket-member))
(defun hamt-set-bucket-member (bucket entry test)
  (and (= (hamt-bucket-hash-code bucket)
          (hamt-set-entry-hash-code entry))
       (member (hamt-set-entry-key entry)
               (hamt-bucket-list bucket)
               :test test)))

(defun hamt-set-count (hamt)
  (labels ((rec (thing)
             (etypecase thing
               (hamt-layer (rec-layer thing))
               (hamt-set-entry 1)
               (hamt-bucket (length (hamt-bucket-list thing)))))
           (rec-layer (layer)
             (loop for i from 1 below (the non-negative-fixnum (length layer))
                   summing (the fixnum (rec (aref layer i))))))
    (etypecase hamt
      (hamt-layer (rec-layer hamt))
      (null 0))))

(defun hamt-set-check (hamt)
  (labels ((rec (hamt bit-depth prefix)
             (declare (type hamt-bit-depth bit-depth))
             (let ((n (hamt-popcount (hamt-layer-bitmap hamt)))
                   (i 0)
                   (bitmap (hamt-layer-bitmap hamt)))
               (assert (= (1+ n) (length hamt)))
               (do-hamt-bits (k bitmap)
                 (let ((thing (aref hamt (incf i)))
                       (pp (logior (ash k bit-depth)
                                   prefix)))
                   (assert (= i (hamt-index bitmap k)))
                   (etypecase thing
                     (hamt-layer
                      (rec thing (+ +hamt-bits+ bit-depth) pp))
                     (hamt-set-entry
                      (f hamt k thing (hamt-set-entry-hash-code thing) bit-depth pp))
                     (hamt-bucket
                      (f hamt k thing (hamt-bucket-hash-code thing) bit-depth pp)))))))
           (f (hamt bit thing hash-code bit-depth prefix)
             (let ((subhash (hamt-bit-depth-subhash hash-code bit-depth))
                   (prehash (ldb (byte (+ bit-depth +hamt-bits+) 0)
                                 prefix)))
               ;; Check prefix
               (assert (= prehash prefix))
               ;; Check present
               (if-hamt-present
                   (bit-2 index thing-2) (hamt subhash)
                   (progn
                     (unless (and (= bit bit-2)
                                  (eq thing thing-2))
                       (break))
                     (assert (= bit bit-2)) ; ensure hash-code bit match
                     (assert (eq thing thing-2)))
                   (progn ; not present, something broke
                     (format *error-output* "~&Failed not-present: ~A" thing)
                     (format *error-output* "~&layer: ~A" hamt)
                     (format *error-output* "~&bit-depth: ~D" bit-depth)
                     (format *error-output* "~&bitmap: #b~b" (hamt-layer-bitmap hamt))
                     (format *error-output* "~&bit: ~D" bit)
                     (format *error-output* "~&bit-2: ~D" bit-2)
                     (format *error-output* "~&index: ~D" index)
                     (break)
                     (assert (and 'not-present nil)))))))
    (when hamt
      (rec hamt 0 0))))

;; Macro to enable debugging checks
(defmacro check-hamt-debug (form)
  (declare (ignore form))
  nil
  ;; uncomment for debugging checks
  ;; form
  )

(defmacro check-hamt-debug-set (hamt)
  `(check-hamt-debug (hamt-set-check ,hamt)))

(defmacro check-hamt-debug-member (hamt item hash-code test)
  `(check-hamt-debug (assert (hamt-set-member ,hamt ,item ,hash-code ,test))))

(defmacro check-hamt-debug-not-member (hamt item hash-code test)
  `(check-hamt-debug (assert (not (hamt-set-member ,hamt ,item ,hash-code ,test)))))


(defun hamt-set-find (hamt key hash-code test default)
  (declare (type hamt-hash-code hash-code)
           (function test))
  (labels ((find-list (list)
             (if list
                 (if (funcall test key (car list))
                     (values (car list) t)
                     (find-list (cdr list)))
                 (values default nil)))
           (rec (hamt subhash)
             (declare (type hamt-layer hamt))
             (if-hamt-present
                 (bit index thing) (hamt subhash)
                 ;; Got entry
                 (etypecase thing
                   (hamt-layer (rec thing (hamt-subhash (the hamt-hash-code
                                                             subhash))))
                   (hamt-set-entry
                    (if (and (= hash-code (hamt-set-entry-hash-code thing))
                             (funcall test key (hamt-set-entry-key thing)))
                        (values (hamt-set-entry-key thing) t)
                        (values default nil)))
                   (hamt-bucket
                    (if (= hash-code (hamt-bucket-hash-code thing))
                        (find-list (hamt-bucket-list thing))
                        (values nil nil))))
                 ;; No entry
                 (values default nil))))
    (declare (dynamic-extent (function find-list))
             (dynamic-extent (function rec)))
    (rec hamt hash-code)))


(defun hamt-set-member (hamt key hash-code test)
  (multiple-value-bind (v p) (hamt-set-find hamt key hash-code test nil)
    (declare (ignore v))
    p))

(defun hamt-set-layer-singleton (hash-code key)
  (hamt-layer-singleton (hamt-set-entry hash-code key)
                        hash-code))

(declaim (inline hamt-set-ninsert-2))
(defun hamt-set-ninsert-2 (hamt index
                           subhash-1 e1
                           subhash-2 e2)
  ;; Insert two distinct elements into a new layer
  (declare (type simple-vector hamt)
           (type hamt-hash-code subhash-1 subhash-2))
  (labels ((rec (hamt index subhash-1 subhash-2)
             (let* ((bit-1 (hamt-bit subhash-1))
                    (bit-2 (hamt-bit subhash-2))
                    (bitmap (ash 1 bit-1)))
               (cond
                 ((< bit-1 bit-2)
                  (hamt-layer-nupdate hamt index
                                      (hamt-layer-2 (logior bitmap (ash 1 bit-2))
                                                    e1 e2)))
                 ((< bit-2 bit-1)
                  (hamt-layer-nupdate hamt index
                                      (hamt-layer-2 (logior bitmap (ash 1 bit-2))
                                                    e2 e1)))
                 (t
                  (let ((new-hamt (hamt-layer-1 bitmap nil)))
                    (setf (aref hamt index) new-hamt)
                    (rec new-hamt 1
                         (hamt-subhash subhash-1)
                         (hamt-subhash subhash-2))))))))
    (declare (dynamic-extent (function rec)))
    (rec hamt index subhash-1 subhash-2)
    hamt))

(declaim (inline hamt-set-insert-2))
(defun hamt-set-insert-2 (hamt index
                          subhash-1 e1
                          subhash-2 e2)
  (hamt-set-ninsert-2 (hamt-layer-open hamt index) index
                      subhash-1 e1
                      subhash-2 e2))


(defun hamt-set-insert (hamt key hash-code test)
  (declare (type hamt-hash-code hash-code)
           (type function test))
  (labels ((rec (hamt hash-code bit-depth)
             (declare (type hamt-bit-depth bit-depth))
             ;;(assert (< depth 32))
             (if-hamt-present
                 (bit index thing) (hamt hash-code)
                 (etypecase thing
                   (hamt-layer (found-layer hamt index
                                            hash-code bit-depth thing))
                   (hamt-set-entry (found-entry hamt index
                                                hash-code bit-depth thing))
                   (hamt-bucket (found-bucket hamt index
                                              hash-code bit-depth thing)))
                 ;; No entry, make a new one
                 (new-entry hamt bit index)))
           (found-bucket (hamt index subhash bit-depth bucket)
             (let ((other-hash-code (hamt-bucket-hash-code bucket)))
               (if (= hash-code other-hash-code)
                   ;; hash collision: check / insert into this bucket
                   (if (member key (hamt-bucket-list bucket) :test test)
                       hamt
                       (hamt-layer-update hamt index
                                          (hamt-bucket hash-code
                                                       (cons key (hamt-bucket-list bucket)))))
                   ;; different hashes: create a new layer
                   (hamt-set-insert-2 hamt index
                                      (hamt-subhash subhash)
                                      (hamt-set-entry hash-code key)
                                      (hamt-bit-depth-subhash other-hash-code bit-depth)
                                      bucket))))
           (found-layer (hamt index hash-code bit-depth layer)
             (hamt-layer-test-update hamt index
                                     layer
                                     (rec layer (hamt-subhash hash-code)
                                          (+ +hamt-bits+ bit-depth))))
           (found-entry (hamt index subhash bit-depth entry)
             (let ((other-hash-code (hamt-set-entry-hash-code entry))
                   (other-key (hamt-set-entry-key entry)))
               (if (= other-hash-code hash-code)
                   ;; matching hash-code
                   (if (funcall test key other-key)
                       ;; already exists, return original hamt
                       hamt
                       ;; hash collision, create a bucket
                       (hamt-layer-update hamt index
                                          (hamt-bucket hash-code
                                                       (list key other-key))))
                   ;; different hash-codes, create a new layer
                   (hamt-set-insert-2 hamt index
                                      (hamt-subhash subhash)
                                      (hamt-set-entry hash-code key)
                                      (hamt-bit-depth-subhash other-hash-code bit-depth)
                                      entry))))
           (new-entry (layer bit index)
             (hamt-layer-insert layer bit index
                                (hamt-set-entry hash-code key))))
    (declare (dynamic-extent (function rec))
             (dynamic-extent (function found-bucket))
             (dynamic-extent (function found-layer))
             (dynamic-extent (function found-entry))
             (dynamic-extent (function new-entry)))
    (let ((result (rec hamt hash-code +hamt-bits+)))
      (check-hamt-debug-set result)
      (check-hamt-debug-member result key hash-code test)
      result)))

(defun hamt-set-replace (hamt key hash-code test)
  (declare (type hamt-hash-code hash-code)
           (type function test))
  (labels ((rec (hamt hash-code bit-depth)
             (declare (type hamt-bit-depth bit-depth))
             ;;(assert (< depth 32))
             (if-hamt-present
                 (bit index thing) (hamt hash-code)
                 (etypecase thing
                   (hamt-layer (found-layer hamt index
                                            hash-code bit-depth thing))
                   (hamt-set-entry (found-entry hamt index
                                                hash-code bit-depth thing))
                   (hamt-bucket (found-bucket hamt index
                                              hash-code bit-depth thing)))
                 ;; No entry, make a new one
                 (new-entry hamt bit index)))
           (found-bucket (hamt index subhash bit-depth bucket)
             (declare (type hamt-layer hamt)
                      (type hamt-bucket bucket))
             (let ((other-hash-code (hamt-bucket-hash-code bucket)))
               (if (= hash-code other-hash-code)
                   ;; Collision or already exists
                   (hamt-bucket hash-code
                                (cons key (remove key (hamt-bucket-list bucket) :test test)))
                   ;; different hashes: create a new layer
                   (hamt-set-insert-2 hamt index
                                      (hamt-subhash subhash)
                                      (hamt-set-entry hash-code key)
                                      (hamt-bit-depth-subhash other-hash-code bit-depth)
                                      bucket))))
           (found-layer (hamt index hash-code bit-depth layer)
             (declare (type hamt-layer hamt layer))
             (hamt-layer-update hamt index
                                (rec layer (hamt-subhash hash-code)
                                     (+ +hamt-bits+ bit-depth))))
           (found-entry (hamt index subhash bit-depth entry)
             (declare (type hamt-layer hamt)
                      (hamt-set-entry entry))
             (let ((other-hash-code (hamt-set-entry-hash-code entry))
                   (other-key (hamt-set-entry-key entry)))
               (if (= other-hash-code hash-code)
                   ;; matching hash-code
                   (hamt-layer-update hamt index
                                      (if (funcall test key other-key)
                                          ;; already exists, replace it
                                          (hamt-set-entry hash-code key)
                                          ;; hash collision, create a bucket
                                          (hamt-bucket hash-code
                                                       (list key other-key))))
                   ;; different hash-codes, create a new layer
                   (hamt-set-insert-2 hamt index
                                      (hamt-subhash subhash)
                                      (hamt-set-entry hash-code key)
                                      (hamt-bit-depth-subhash other-hash-code bit-depth)
                                      entry))))
           (new-entry (layer bit index)
             (hamt-layer-insert layer bit index
                                (hamt-set-entry hash-code key))))
    (declare (dynamic-extent (function rec))
             (dynamic-extent (function found-bucket))
             (dynamic-extent (function found-layer))
             (dynamic-extent (function found-entry))
             (dynamic-extent (function new-entry)))
    (let ((result (rec hamt hash-code +hamt-bits+)))
      (check-hamt-debug-set result)
      (check-hamt-debug-member result key hash-code test)
      result)))


(defun hamt-set-ninsert (hamt key hash-code test)
  (declare (type hamt-hash-code hash-code)
           (type function test)
           (type hamt-layer hamt))
  ;;(declare (optimize (speed 3) (safety 0)))
  (labels ((init (hamt subhash)
             (if-hamt-present
                 (bit index thing) (hamt subhash)
                 (progn (rec-present hamt subhash +hamt-bits+ index thing)
                        hamt)
                 (hamt-layer-insert hamt
                                    bit index
                                    (hamt-set-entry hash-code key))))
           (rec (parent pindex hamt subhash bit-depth)
             (declare (type hamt-bit-depth bit-depth))
             ;; Tail-recursive insertion: Pass along the cell of a
             ;; parent to overwrite.
             ;;
             ;; - Base case of not present: overwrite the parent cell by
             ;;   adding a new entry to the current layer.
             ;; - Base case of entry/bucket: ovewrite the current layer
             ;;   cell with new sublayer(s).  Or push to bucket if a
             ;;   hash collision.
             ;; - Recursive case of sublayer: tail-recurse on the
             ;;   sublayer, passing current layer and index as the new
             ;;   parent / parent index.
             (if-hamt-present
                 (bit index thing) (hamt subhash)
                 (rec-present hamt subhash bit-depth index thing)
                 (update parent pindex
                         (hamt-layer-insert hamt
                                            bit index
                                            (hamt-set-entry hash-code key)))))
           (rec-present (hamt subhash bit-depth index thing)
             (etypecase thing
               (hamt-layer ; tail-recursive layer insertion
                (rec hamt index
                     thing (hamt-subhash subhash) (+ +hamt-bits+ bit-depth)))
               (hamt-set-entry (found-entry hamt index
                                            subhash bit-depth thing))
               (hamt-bucket (found-bucket hamt index
                                          subhash bit-depth thing))))
           (found-bucket (hamt index subhash bit-depth bucket)
             (if (= hash-code (hamt-bucket-hash-code bucket))
                 ;; hash collision: check / insert into this bucket
                 (unless (member key (hamt-bucket-list bucket) :test test)
                   (push key (hamt-bucket-list bucket)))
                 ;; different hashe-codes: create a new layer
                 (hamt-set-ninsert-2 hamt index
                                     (hamt-subhash subhash)
                                     (hamt-set-entry hash-code key)
                                     (hamt-bit-depth-subhash (hamt-bucket-hash-code bucket)
                                                             bit-depth)
                                     bucket)))
           (found-entry (hamt index subhash bit-depth entry)
             (let ((other-hash-code (hamt-set-entry-hash-code entry))
                   (other-key (hamt-set-entry-key entry)))
               (if (= other-hash-code hash-code)
                   ;; matching hash-code
                   (unless (funcall test key other-key)
                     ;; hash collision, create a bucket
                     (update hamt index
                             (hamt-bucket hash-code (list key other-key))))
                   ;; different hash-codes, create new layer(s)
                   (hamt-set-ninsert-2 hamt index
                                       (hamt-subhash subhash)
                                       (hamt-set-entry hash-code key)
                                       (hamt-bit-depth-subhash other-hash-code bit-depth)
                                       entry))))
           (update (hamt index thing)
             (hamt-layer-nupdate hamt index thing)))
    (declare (dynamic-extent (function init))
             (dynamic-extent (function rec))
             (dynamic-extent (function rec-present))
             (dynamic-extent (function found-bucket))
             (dynamic-extent (function found-entry))
             (dynamic-extent (function update)))
    (let ((result (init hamt hash-code)))
      (check-hamt-debug-set result)
      (check-hamt-debug-member result key hash-code test)
      result)))

(defun hamt-set-nreplace (hamt key hash-code test)
  (declare (type hamt-hash-code hash-code)
           (type function test)
           (type hamt-layer hamt))
  ;;(declare (optimize (speed 3) (safety 0)))
  (labels ((init (hamt subhash)
             (if-hamt-present
                 (bit index thing) (hamt subhash)
                 (progn (rec-present hamt subhash +hamt-bits+ index thing)
                        hamt)
                 (hamt-layer-insert hamt
                                    bit index
                                    (hamt-set-entry hash-code key))))
           (rec (parent pindex hamt subhash bit-depth)
             (declare (type hamt-bit-depth bit-depth))
             ;; Tail-recursive insertion: Pass along the cell of a
             ;; parent to overwrite.
             ;;
             ;; - Base case of not present: overwrite the parent cell by
             ;;   adding a new entry to the current layer.
             ;; - Base case of entry/bucket: ovewrite the current layer
             ;;   cell with new sublayer(s).  Or push to bucket if a
             ;;   hash collision.
             ;; - Recursive case of sublayer: tail-recurse on the
             ;;   sublayer, passing current layer and index as the new
             ;;   parent / parent index.
             (if-hamt-present
                 (bit index thing) (hamt subhash)
                 (rec-present hamt subhash bit-depth index thing)
                 (update parent pindex
                         (hamt-layer-insert hamt
                                            bit index
                                            (hamt-set-entry hash-code key)))))
           (rec-present (hamt subhash bit-depth index thing)
             (etypecase thing
               (hamt-layer ; tail-recursive layer insertion
                (rec hamt index
                     thing (hamt-subhash subhash) (+ +hamt-bits+ bit-depth)))
               (hamt-set-entry (found-entry hamt index
                                            subhash bit-depth thing))
               (hamt-bucket (found-bucket hamt index
                                          subhash bit-depth thing))))
           (found-bucket (hamt index subhash bit-depth bucket)
             (if (= hash-code (hamt-bucket-hash-code bucket))
                 ;; hash collision: check / insert into this bucket
                 (progn
                   (setf (hamt-bucket-list bucket)
                         (cons key (remove key (hamt-bucket-list bucket) :test test)))
                   hamt)
                 ;; different hashe-codes: create a new layer
                 (hamt-set-ninsert-2 hamt index
                                     (hamt-subhash subhash)
                                     (hamt-set-entry hash-code key)
                                     (hamt-bit-depth-subhash (hamt-bucket-hash-code bucket)
                                                             bit-depth)
                                     bucket)))
           (found-entry (hamt index subhash bit-depth entry)
             (let ((other-hash-code (hamt-set-entry-hash-code entry))
                   (other-key (hamt-set-entry-key entry)))
               (if (= other-hash-code hash-code)
                   ;; matching hash-code
                   (update hamt index
                           (if (funcall test key other-key)
                               ;; Match, replace
                               (hamt-set-entry hash-code key)
                               ;; hash collision, create a bucket
                               (hamt-bucket hash-code (list key other-key))))
                   ;; different hash-codes, create new layer(s)
                   (hamt-set-ninsert-2 hamt index
                                       (hamt-subhash subhash)
                                       (hamt-set-entry hash-code key)
                                       (hamt-bit-depth-subhash other-hash-code bit-depth)
                                       entry))))
           (update (hamt index thing)
             (hamt-layer-nupdate hamt index thing)))
    (declare (dynamic-extent (function init))
             (dynamic-extent (function rec))
             (dynamic-extent (function rec-present))
             (dynamic-extent (function found-bucket))
             (dynamic-extent (function found-entry))
             (dynamic-extent (function update)))
    (let ((result (init hamt hash-code)))
      (check-hamt-debug-set result)
      (check-hamt-debug-member result key hash-code test)
      result)))

(defun hamt-set-remove (hamt key hash-code test)
  (declare (type function test)
           (type hamt-hash-code hash-code))
  (labels ((rec (hamt subhash)
             ;;(assert (< depth 32))
             (if-hamt-present
                 (bit index thing) (hamt subhash)
                 (etypecase thing
                   (hamt-layer (found-layer hamt bit index subhash thing))
                   (hamt-set-entry (found-entry hamt bit index thing))
                   (hamt-bucket (found-bucket hamt index thing)))
                 hamt))
           (found-layer (hamt bit index subhash layer)
             (let ((new-layer (rec layer (hamt-subhash subhash))))
               (cond
                 ((eq layer new-layer)
                  hamt)
                 (new-layer
                  (hamt-layer-update hamt index new-layer))
                 (t
                  (hamt-layer-remove hamt bit index)))))
           (found-entry (hamt bit index entry)
             (if (and (= hash-code (hamt-set-entry-hash-code entry))
                      (funcall test key (hamt-set-entry-key entry)))
                 ;; Matches, remove
                 (hamt-layer-remove hamt bit index)
                 ;; no match, return the original
                 hamt))
           (found-bucket (hamt index bucket)
             (if (= hash-code (hamt-bucket-hash-code bucket))
                 (let* ((list (hamt-bucket-list bucket))
                        (pos (position key list :test test)))
                   (if pos
                       (bucket-rm hamt index pos list)
                       hamt))
                 hamt))
           (bucket-rm (hamt index pos list)
             (hamt-layer-update hamt index
                                (if (null (cddr list))
                                    ;; downgrade to element
                                    (if (zerop pos)
                                        (hamt-set-entry hash-code (second list))
                                        (hamt-set-entry hash-code (first list)))
                                    ;; still a bucket
                                    (hamt-bucket hash-code
                                                 (rmn pos list)))))
           (rmn (n list)
             (if (zerop n)
                 (cdr list)
                 (cons (car list)
                       (rmn (1- n) (cdr list))))))
    (let* ((result-0 (rec hamt hash-code)))
      (when result-0
        (let ((result-1
                (etypecase result-0
                  (hamt-layer
                   result-0)
                  (hamt-set-entry
                   (hamt-layer-singleton result-0
                                         (hamt-set-entry-hash-code result-0)))
                  (hamt-bucket
                   (hamt-layer-singleton result-0
                                         (hamt-bucket-hash-code result-0))))))
          (check-hamt-debug-set result-1)
          (check-hamt-debug-not-member result-1 key hash-code test)
          result-1)))))


(defun hamt-set-union (set-1 set-2 test)
  (declare (type hamt-layer set-1 set-2)
           (type function test))
  (labels ((insert-2 (layer index bit-depth h1 e1 h2 e2)
             (hamt-set-insert-2 layer index
                                (hamt-bit-depth-subhash h1 bit-depth) e1
                                (hamt-bit-depth-subhash h2 bit-depth) e2))
           (ninsert-2 (layer index bit-depth h1 e1 h2 e2)
             (hamt-set-ninsert-2 layer index
                                 (hamt-bit-depth-subhash h1 bit-depth) e1
                                 (hamt-bit-depth-subhash h2 bit-depth) e2))
           (rec (nlayer index t1 t2 bit-depth)
             (declare (type hamt-bit-depth bit-depth))
             (etypecase t1
               (hamt-layer (etypecase t2
                             (hamt-layer
                              (hamt-layer-nupdate nlayer index
                                                  (f-l-l t1 t2 bit-depth)))
                             (hamt-set-entry
                              (hamt-layer-nupdate nlayer index
                                                  (f-l-e t1 t2 bit-depth)))
                             (hamt-bucket
                              (hamt-layer-nupdate nlayer index
                                                  (f-l-b t1 t2 bit-depth)))))
               (hamt-set-entry (etypecase t2
                                 (hamt-layer
                                  (hamt-layer-nupdate nlayer index
                                                      (f-l-e t2 t1 bit-depth)))
                                 (hamt-set-entry
                                  (f-e-e nlayer index t1 t2 bit-depth))
                                 (hamt-bucket
                                  (f-b-e nlayer index t2 t1 bit-depth))))
               (hamt-bucket (etypecase t2
                              (hamt-layer
                               (hamt-layer-nupdate nlayer index
                                                   (f-l-b t2 t1 bit-depth)))
                              (hamt-set-entry
                               (f-b-e nlayer index t1 t2 bit-depth))
                              (hamt-bucket
                               (f-b-b nlayer index t1 t2 bit-depth))))))
           (f-l-l (l1 l2 bit-depth) ; layer-layer
             (declare (type hamt-layer l1 l2)
                      (type hamt-bit-depth bit-depth))
             (let* ((b1 (hamt-layer-bitmap l1))
                    (b2 (hamt-layer-bitmap l2))
                    (bb (logior b1 b2))
                    (ll (make-array (1+ (hamt-popcount bb)))))
               (setf (aref ll 0) bb)
               ;; Entries in both
               (do-hamt-bits (k (logand b1 b2))
                 (rec ll (hamt-index bb k)
                      (aref l1 (hamt-index b1 k))
                      (aref l2 (hamt-index b2 k))
                      (+ +hamt-bits+ bit-depth)))
               ;; Entries in l1 but not l2
               (do-hamt-bits (k (logandc2 b1 b2))
                 (hamt-layer-nupdate ll (hamt-index bb k)
                                     (aref l1 (hamt-index b1 k))))
               ;; Entries in l2 but not l1
               (do-hamt-bits (k (logandc2 b2 b1))
                 (hamt-layer-nupdate ll (hamt-index bb k)
                                     (aref l2 (hamt-index b2 k))))
               ;; Result
               ll))
           (f-l-e (layer entry bit-depth) ; layer-entry
             (declare (type hamt-layer layer)
                      (type hamt-set-entry entry)
                      (type hamt-bit-depth bit-depth))
             (let* ((h (hamt-set-entry-hash-code entry))
                    (subhash (hamt-bit-depth-subhash h bit-depth)))
               (if-hamt-present
                   (bit index thing) (layer subhash)
                   ;; Present, combine
                   (etypecase thing
                     (hamt-layer
                      (hamt-layer-test-update layer index
                                              thing
                                              (f-l-e thing entry (+ +hamt-bits+ bit-depth))))
                     (hamt-set-entry
                      (f-l-e-e layer index
                               entry thing (+ +hamt-bits+ bit-depth)))
                     (hamt-bucket
                      (f-b-e (hamt-layer-open layer index) index
                             thing entry (+ +hamt-bits+ bit-depth))))
                   ;; Not Present, Insert
                   (hamt-layer-insert layer bit index entry))))
           (f-l-e-e (layer index e1 e2 bit-depth) ; entry-entry, no cons when equiv
             (declare (type hamt-set-entry e1 e2)
                      (type hamt-bit-depth bit-depth))
             (let ((h1 (hamt-set-entry-hash-code e1))
                   (h2 (hamt-set-entry-hash-code e2)))
               (if (= h1 h2)
                   (if (funcall test
                                (hamt-set-entry-key e1)
                                (hamt-set-entry-key e2))
                       layer
                       (hamt-layer-update layer index
                                          (hamt-bucket h1 (list (hamt-set-entry-key e1)
                                                                (hamt-set-entry-key e2)))))

                   (insert-2 layer index bit-depth
                             h1 e1 h2 e2))))
           (f-e-e (nlayer index e1 e2 bit-depth) ; entry-entry
             (declare (type hamt-set-entry e1 e2)
                      (type hamt-bit-depth bit-depth))
             (let ((h1 (hamt-set-entry-hash-code e1))
                   (h2 (hamt-set-entry-hash-code e2)))
               (if (= h1 h2)
                   (hamt-layer-nupdate nlayer index
                                       (if (funcall test
                                                    (hamt-set-entry-key e1)
                                                    (hamt-set-entry-key e2))
                                           e1
                                           (hamt-bucket h1 (list (hamt-set-entry-key e1)
                                                                 (hamt-set-entry-key e2)))))
                   (ninsert-2 nlayer index bit-depth
                              h1 e1
                              h2 e2))))
           (f-l-b (layer bucket bit-depth) ; layer-bucket
             (declare (type hamt-layer layer)
                      (type hamt-bucket bucket)
                      (type hamt-bit-depth bit-depth))
             (let* ((h (hamt-bucket-hash-code bucket))
                    (subhash (hamt-bit-depth-subhash h bit-depth)))
               (if-hamt-present
                   (bit index thing) (layer subhash)
                   ;; Present, Combine
                   (etypecase thing
                     (hamt-layer
                      (hamt-layer-update layer index
                                         (f-l-b thing bucket (+ +hamt-bits+ bit-depth))))
                     (hamt-set-entry
                      (f-b-e (hamt-layer-open layer index) index
                             bucket thing (+ +hamt-bits+ bit-depth)))
                     (hamt-bucket
                      (f-b-b (hamt-layer-open layer index) index
                             bucket thing (+ +hamt-bits+ bit-depth))))
                   ;; Not Present, Insert
                   (hamt-layer-insert layer bit index bucket))))
           (f-b-b (nlayer index b1 b2 bit-depth) ; bucket-bucket
             (declare (type hamt-bucket b1 b2)
                      (type hamt-bit-depth bit-depth))
             (let ((h1 (hamt-bucket-hash-code b1))
                   (h2 (hamt-bucket-hash-code b2)))
               (if (= h1 h2)
                   (hamt-layer-nupdate nlayer index
                                       (hamt-bucket h1 (union (hamt-bucket-list b1)
                                                              (hamt-bucket-list b2)
                                                              :test test)))
                   (ninsert-2 nlayer index bit-depth
                              h1 b1 h2 b2))))
           (f-b-e (nlayer index b e bit-depth) ; bucket-entry
             (declare (type hamt-bucket b)
                      (type hamt-set-entry e)
                      (type hamt-bit-depth bit-depth))
             (let ((hb (hamt-bucket-hash-code b))
                   (he (hamt-set-entry-hash-code e)))
               (if (= hb he)
                   ;; Hash collision or duplicate key
                   (hamt-layer-nupdate nlayer index
                                       (let ((e-k (hamt-set-entry-key e))
                                             (b-l (hamt-bucket-list b)))
                                         (if (member e-k b-l :test test)
                                             b
                                             (hamt-bucket hb (cons e-k b-l)))))
                   ;; Different hash-code
                   (ninsert-2 nlayer index bit-depth
                              hb b he e)))))
    (declare (dynamic-extent (function insert-2)
                             (function ninsert-2)
                             (function rec)
                             (function f-l-l)
                             (function f-l-e)
                             (function f-l-e-e)
                             (function f-e-e)
                             (function f-l-b)
                             (function f-b-b)
                             (function f-b-e)))
    (let ((result (f-l-l set-1 set-2 0)))
      ;; (hamt-set-check result) ; debugging check
      (check-hamt-debug-set result)
      result)))


(defun hamt-set-intersection (set-1 set-2 test)
  (declare (type hamt-layer set-1 set-2)
           (type function test))
  (labels ((rec (t1 t2 bit-depth)
             (etypecase t1
               (hamt-layer (etypecase t2
                             (hamt-layer (f-l-l t1 t2 (+ +hamt-bits+ bit-depth)))
                             (hamt-set-entry (f-l-e t1 t2 bit-depth))
                             (hamt-bucket (f-l-b t1 t2 bit-depth))))
               (hamt-set-entry (f-e-x t1 t2 bit-depth))
               (hamt-bucket (f-b-x t1 t2 bit-depth))))
           (f-e-x (entry other bit-depth)
             (etypecase other
               (hamt-layer (f-l-e other entry bit-depth))
               (hamt-set-entry (f-e-e entry other))
               (hamt-bucket (f-b-e other entry))))
           (f-b-x (bucket other bit-depth)
             (etypecase other
               (hamt-layer (f-l-b other bucket bit-depth))
               (hamt-set-entry (f-b-e bucket other))
               (hamt-bucket (f-b-b bucket other))))
           (f-l-l (l1 l2 bit-depth)
             (declare (type (hamt-layer) l1 l2)
                      (type (hamt-bit-depth) bit-depth))
             (let* ((b1 (hamt-layer-bitmap l1))
                    (b2 (hamt-layer-bitmap l2))
                    (bb (logand b1 b2))
                    (temp (make-array (1+ (hamt-popcount bb)))))
               (declare (dynamic-extent temp))
               ;; Find children
               (do-hamt-bits (k bb)
                 (if-let ((child (rec (aref l1 (hamt-index b1 k))
                                      (aref l2 (hamt-index b2 k))
                                      bit-depth)))
                   (setf (aref temp (hamt-index bb k)) child)
                   (setq bb (logandc2 bb (ash 1 k)))))
               ;; Fill in new array
               (hamt-layer-finish bb temp)))
           (f-l-e (layer entry bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-set-entry entry)
                      (type hamt-bit-depth bit-depth))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-set-entry-hash-code entry)
                                           bit-depth))
                 (f-e-x entry other (+ +hamt-bits+ bit-depth))
                 nil))
           (f-l-b (layer bucket bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-bucket bucket))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-bucket-hash-code bucket)
                                           bit-depth))
                 (f-b-x bucket other (+ +hamt-bits+ bit-depth))
                 nil))
           (f-e-e (e1 e2)
             (declare (type (hamt-set-entry) e1 e2))
             (when (hamt-set-entry-= e1 e2 test)
               e1))
           (f-b-b (b1 b2)
             (declare (type hamt-bucket b1 b2))
             (let ((h1 (hamt-bucket-hash-code b1)))
               (when (= h1 (hamt-bucket-hash-code b2))
                 (when-let ((list (intersection (hamt-bucket-list b1)
                                                (hamt-bucket-list b2)
                                                :test test)))
                   (if (null (cdr list))
                       (hamt-set-entry h1 (car list))
                       (hamt-bucket h1 list))))))
           (f-b-e (bucket entry)
             (declare (type hamt-bucket bucket)
                      (type hamt-set-entry entry))
             (when (hamt-set-bucket-member bucket entry test)
               entry)))
    (let ((result (f-l-l set-1 set-2 +hamt-bits+)))
      (etypecase result
        (hamt-layer
         ;; (hamt-set-check result) ; debugging check
         (check-hamt-debug-set result)
         result)
        (hamt-set-entry (hamt-layer-singleton result
                                              (hamt-set-entry-hash-code result)))
        (hamt-bucket (hamt-layer-singleton result
                                           (hamt-bucket-hash-code result)))
        (null nil)))))

(defun hamt-set-intersection-p (set-1 set-2 test)
  (declare (type hamt-layer set-1 set-2)
           (type function test))
  (labels ((rec (t1 t2 bit-depth)
             (etypecase t1
               (hamt-layer (etypecase t2
                             (hamt-layer (f-l-l t1 t2 (+ +hamt-bits+ bit-depth)))
                             (hamt-set-entry (f-l-e t1 t2 bit-depth))
                             (hamt-bucket (f-l-b t1 t2 bit-depth))))
               (hamt-set-entry (f-e-x t1 t2 bit-depth))
               (hamt-bucket (f-b-x t1 t2 bit-depth))))
           (f-e-x (entry other bit-depth)
             (etypecase other
               (hamt-layer (f-l-e other entry bit-depth))
               (hamt-set-entry (f-e-e entry other))
               (hamt-bucket (f-b-e other entry))))
           (f-b-x (bucket other bit-depth)
             (etypecase other
               (hamt-layer (f-l-b other bucket bit-depth))
               (hamt-set-entry (f-b-e bucket other))
               (hamt-bucket (f-b-b bucket other))))
           (f-l-l-bit (b1 b2 bb l1 l2 bit-depth)
             (declare (type simple-vector l1 l2)
                      (type hamt-bit-depth bit-depth))
             (unless (zerop bb)
               (let ((bit (hamt-bitmap-least bb)))
                 (or (and (logbitp bit bb)
                          (rec (aref l1 (hamt-index b1 bit))
                               (aref l2 (hamt-index b2 bit))
                               bit-depth))
                     (f-l-l-bit b1 b2
                                (logandc2 bb (ash 1 bit))
                                l1 l2 bit-depth)))))
           (f-l-l (l1 l2 bit-depth)
             (declare (type (hamt-layer) l1 l2)
                      (type (hamt-bit-depth) bit-depth))
             (let ((b1 (hamt-layer-bitmap l1))
                   (b2 (hamt-layer-bitmap l2)))
               (f-l-l-bit b1 b2 (logand b1 b2) l1 l2 bit-depth)))
           (f-l-e (layer entry bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-set-entry entry)
                      (type hamt-bit-depth bit-depth))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-set-entry-hash-code entry)
                                           bit-depth))
                 (f-e-x entry other (+ +hamt-bits+ bit-depth))
                 nil))
           (f-l-b (layer bucket bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-bucket bucket))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-bucket-hash-code bucket)
                                           bit-depth))
                 (f-b-x bucket other (+ +hamt-bits+ bit-depth))
                 nil))
           (f-e-e (e1 e2)
             (declare (type (hamt-set-entry) e1 e2))
             (hamt-set-entry-= e1 e2 test))
           (f-b-b (b1 b2)
             (declare (type hamt-bucket b1 b2))
             (and (= (hamt-bucket-hash-code b1)
                     (hamt-bucket-hash-code b2))
                  (intersection (hamt-bucket-list b1)
                                (hamt-bucket-list b2)
                                :test test)))
           (f-b-e (bucket entry)
             (declare (type hamt-bucket bucket)
                      (type hamt-set-entry entry))
             (hamt-set-bucket-member bucket entry test)))
    (when (f-l-l set-1 set-2 +hamt-bits+)
      t)))

(defun hamt-set-difference (set-1 set-2 test)
  (declare (type hamt-layer set-1 set-2)
           (type function test))
  (labels ((rec (t1 t2 bit-depth)
             (etypecase t1
               (hamt-layer (etypecase t2
                             (hamt-layer (f-l-l t1 t2 (+ +hamt-bits+ bit-depth)))
                             (hamt-set-entry (f-l-e t1 t2 bit-depth))
                             (hamt-bucket (f-l-b t1 t2 bit-depth))))
               (hamt-set-entry (f-e-x t1 t2 bit-depth))
               (hamt-bucket (f-b-x t1 t2 bit-depth))))
           (f-e-x (entry other bit-depth)
             (etypecase other
               (hamt-layer (f-e-l entry other bit-depth))
               (hamt-set-entry (f-e-e entry other))
               (hamt-bucket (f-e-b entry other))))
           (f-b-x (bucket other bit-depth)
             (etypecase other
               (hamt-layer (f-b-l bucket other bit-depth))
               (hamt-set-entry (f-b-e bucket other))
               (hamt-bucket (f-b-b bucket other))))
           (f-l-l (l1 l2 bit-depth)
             (declare (type (hamt-layer) l1 l2)
                      (type (hamt-bit-depth) bit-depth))
             (let* ((b1 (hamt-layer-bitmap l1))
                    (b2 (hamt-layer-bitmap l2))
                    (bb b1)
                    (temp (make-array (1+ (hamt-popcount bb)))))
               (declare (dynamic-extent temp))
               ;; Find children
               (do-hamt-bits (k b1)
                 (if (logbitp k b2)
                     ;; Set in layer-2, recurse
                     (if-let ((child (rec (aref l1 (hamt-index b1 k))
                                          (aref l2 (hamt-index b2 k))
                                          bit-depth)))
                       (setf (aref temp (hamt-index bb k)) child)
                       (setq bb (logandc2 bb (ash 1 k))))
                     ;; Not in layer-2, keep
                     (setf (aref temp (hamt-index bb k))
                           (aref l1 (hamt-index b1 k)))))
               ;; Fill in new array
               (hamt-layer-finish bb temp)))
           (update (layer bit index old-thing new-thing)
             (declare (type hamt-layer layer)
                      (type (or hamt-layer hamt-set-entry hamt-bucket null)
                            new-thing)
                      (type (or hamt-layer hamt-set-entry hamt-bucket)
                            old-thing)
                      (type hamt-bit bit)
                      (type hamt-index index))
             (if new-thing
                 (if (eq old-thing new-thing)
                     layer
                     (hamt-layer-update layer index new-thing))
                 (hamt-layer-remove layer bit index)))
           (f-l-e (layer entry bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-set-entry entry)
                      (type hamt-bit-depth bit-depth))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-set-entry-hash-code entry)
                                           bit-depth))
                 (update layer bit index other
                         (etypecase other
                           (hamt-layer (f-l-e other entry (+ +hamt-bits+ bit-depth)))
                           (hamt-set-entry (f-e-e other entry))
                           (hamt-bucket (f-b-e other entry))))
                 layer))
           (f-e-l (entry layer bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-set-entry entry)
                      (type hamt-bit-depth bit-depth))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-set-entry-hash-code entry)
                                           bit-depth))
                 (f-e-x entry other (+ +hamt-bits+ bit-depth))
                 entry))
           (f-e-e (e1 e2)
             (declare (type (hamt-set-entry) e1 e2))
             (unless (hamt-set-entry-= e1 e2 test)
               e1))
           (f-b-b (b1 b2)
             (declare (type hamt-bucket b1 b2))
             (let ((h1 (hamt-bucket-hash-code b1)))
               (if (= h1 (hamt-bucket-hash-code b2))
                   (hamt-set-bucket* h1
                                     (set-difference (hamt-bucket-list b1)
                                                     (hamt-bucket-list b2)
                                                     :test test))
                   b1)))
           (f-l-b (layer bucket bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-bucket bucket))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-bucket-hash-code bucket)
                                           bit-depth))
                 (update layer bit index other
                         (etypecase other
                           (hamt-layer (f-l-b other bucket (+ +hamt-bits+ bit-depth)))
                           (hamt-set-entry (f-e-b other bucket))
                           (hamt-bucket (f-b-b other bucket))))
                 layer))
           (f-b-l (bucket layer bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-bucket bucket))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-bucket-hash-code bucket)
                                           bit-depth))
                 (f-b-x bucket other (+ +hamt-bits+ bit-depth))
                 bucket))
           (f-e-b (entry bucket)
             (declare (type hamt-bucket bucket)
                      (type hamt-set-entry entry))
             (unless (hamt-set-bucket-member bucket entry test)
               entry))
           (f-b-e (bucket entry)
             (declare (type hamt-bucket bucket)
                      (type hamt-set-entry entry))
             (let ((h1 (hamt-bucket-hash-code bucket)))
               (if (= h1 (hamt-set-entry-hash-code entry))
                   (hamt-set-bucket* h1
                                     (remove (hamt-set-entry-key entry)
                                             (hamt-bucket-list bucket)
                                             :test test))
                   bucket))))
    (let ((result (f-l-l set-1 set-2 +hamt-bits+)))
      (etypecase result
        (hamt-layer
         ;; (hamt-set-check result) ; debugging check
         (check-hamt-debug-set result)
         result)
        (hamt-set-entry (hamt-layer-singleton result
                                              (hamt-set-entry-hash-code result)))
        (hamt-bucket (hamt-layer-singleton result
                                           (hamt-bucket-hash-code result)))
        (null nil)))))

(defun hamt-set-subset-p (set-1 set-2 test)
  (declare (type hamt-layer set-1 set-2)
           (type function test))
  (labels ((rec (t1 t2 bit-depth)
             (etypecase t1
               (hamt-layer (etypecase t2
                             (hamt-layer (f-l-l t1 t2 (+ +hamt-bits+ bit-depth)))
                             (hamt-set-entry (f-l-x t1 t2 bit-depth))
                             (hamt-bucket (f-l-x t1 t2 bit-depth))))
               (hamt-set-entry (f-e-x t1 t2 bit-depth))
               (hamt-bucket (f-b-x t1 t2 bit-depth))))
           (f-e-x (entry other bit-depth)
             (etypecase other
               (hamt-layer (f-e-l entry other bit-depth))
               (hamt-set-entry (f-e-e entry other))
               (hamt-bucket (f-e-b entry other))))
           (f-b-x (bucket other bit-depth)
             (etypecase other
               (hamt-layer (f-b-l bucket other bit-depth))
               (hamt-set-entry (f-b-e bucket other))
               (hamt-bucket (f-b-b bucket other))))
           (f-l-l-bit (b1 b2 bb l1 l2 bit-depth)
             (declare (type simple-vector l1 l2)
                      (type hamt-bit-depth bit-depth))
             (if (zerop bb)
                 t
                 (let ((bit (hamt-bitmap-least bb)))
                   (and (logbitp bit b2)
                        (rec (aref l1 (hamt-index b1 bit))
                             (aref l2 (hamt-index b2 bit))
                             bit-depth)
                        (f-l-l-bit b1 b2
                                   (logandc2 bb (ash 1 bit))
                                   l1 l2 bit-depth)))))
           (f-l-l (l1 l2 bit-depth)
             (declare (type (hamt-layer) l1 l2)
                      (type (hamt-bit-depth) bit-depth))
             (let ((b1 (hamt-layer-bitmap l1))
                   (b2 (hamt-layer-bitmap l2)))
               (when (zerop (logandc2 b1 b2)) ; does b1 have bits unset in b2?
                 (f-l-l-bit b1 b2 b1 l1 l2 bit-depth))))
           (f-e-l (entry layer bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-set-entry entry)
                      (type hamt-bit-depth bit-depth))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-set-entry-hash-code entry)
                                           bit-depth))
                 (f-e-x entry other (+ +hamt-bits+ bit-depth))
                 nil))
           (f-l-x (layer thing bit-depth)
             (declare (type hamt-layer layer))
             (case (length layer)
               ;; somehow empty layer
               (1 t)
               ;; singleton layer, recurse for paranoid check
               (2 (rec (aref layer 1) thing (+ +hamt-bits+ bit-depth)))
               ;; layer with two hash-codes cannot be subset of single hash-code thing
               (otherwise nil)))
           (f-b-l (bucket layer bit-depth)
             (declare (type hamt-layer layer)
                      (type hamt-bucket bucket))
             (if-hamt-present
                 (bit index other) (layer (hamt-bit-depth-subhash
                                           (hamt-bucket-hash-code bucket)
                                           bit-depth))
                 (f-b-x bucket other (+ +hamt-bits+ bit-depth))
                 nil))
           (f-e-e (e1 e2)
             (declare (type (hamt-set-entry) e1 e2))
             (hamt-set-entry-= e1 e2 test))
           (f-b-b (b1 b2)
             (declare (type hamt-bucket b1 b2))
             (and (= (hamt-bucket-hash-code b1)
                     (hamt-bucket-hash-code b2))
                  (subsetp (hamt-bucket-list b1)
                           (hamt-bucket-list b2)
                           :test test)))
           (f-e-b (entry bucket)
             (declare (type hamt-set-entry entry)
                      (type hamt-bucket bucket))
             (hamt-set-bucket-member bucket entry test))
           (f-b-e (bucket entry)
             ;; Paranoid check for empty and singleton buckets
             (let ((list (hamt-bucket-list bucket)))
               (or (null list)
                   (and (= (hamt-bucket-hash-code bucket)
                           (hamt-set-entry-hash-code entry))
                        (null (cdr list))
                        (funcall test (car list) entry))))))
    (when (f-l-l set-1 set-2 +hamt-bits+)
      t)))

;;; Higher-order functions

(declaim (inline %hamt-set-map-nil))
(defun %hamt-set-map-nil (function hamt)
  (declare (type function function))
  (labels ((rec (hamt)
             (loop for i from 1 below (length hamt)
                   for thing = (aref hamt i)
                   do (etypecase thing
                        (hamt-layer (rec thing))
                        (hamt-set-entry (funcall function (hamt-set-entry-key thing)))
                        (hamt-bucket
                         (map nil function (hamt-bucket-list thing)))))))
    (declare (dynamic-extent (function rec)))
    (etypecase hamt
      (hamt-layer
       (rec hamt)
       nil)
      (null nil))))

(defmacro hamt-set-do ((elt hamt &optional result) &body body)
  (with-gensyms (f)
    `(flet ((,f (,elt) ,@body))
       (declare (dynamic-extent (function ,f)))
       (%hamt-set-map-nil #',f ,hamt)
       ,result)))

(defun hamt-set-fold-left (function initial-value hamt)
  ;; function : (initial-value elt) -> initial-value
  (declare (type function function))
  ;; Process
  (hamt-set-do (x hamt initial-value)
    (setq initial-value
          (funcall function initial-value x))))

(defun hamt-set-fold-right (function hamt initial-value)
  ;; function : (elt initial-value) -> initial-value
  (declare (type function function))
  ;; Process
  (hamt-set-do (x hamt initial-value)
    (setq initial-value
          (funcall function x initial-value))))

(defun hamt-set-map-nil (function hamt)
  (%hamt-set-map-nil function hamt))

(defun hamt-set-map-list (function hamt)
  (declare (type function function))
  (let ((l nil))
    ;; Process
    (hamt-set-do (x hamt l)
      (push (funcall function x)
            l))))

(defun hamt-set-map-vector (function hamt)
  (declare (type function function))
  (let ((v (make-array (hamt-set-count hamt)))
        (k 0))
    (declare (fixnum k))
    ;; Process
    (hamt-set-do (x hamt v)
      (setf (aref v k)
            (funcall function x))
      (incf k))))

(defun hamt-set-map (result-type function hamt)
  (cond
    ((null result-type)
     (hamt-set-map-nil function hamt))
    ((eq result-type 'list)
     (hamt-set-map-list function hamt))
    ((eq result-type 'vector)
     (hamt-set-map-vector function hamt))))
