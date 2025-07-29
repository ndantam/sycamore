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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Bit Operations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +hamt-bits+
  ;; Try to keep the bitmap as a fixnum.  Do fixnums have 32 bits we
  ;; can use?
  (if (< (expt 2 32) most-positive-fixnum)
      5
      4))
(defconstant +hamt-mask+ (1- (expt 2 +hamt-bits+)))
(deftype hamt-bitmap ()
  `(unsigned-byte ,(expt 2 +hamt-bits+)))

(deftype hamt-bit ()
  `(unsigned-byte ,+hamt-bits+))

(declaim (inline hamt-bit))
(defun hamt-bit (hash-code) (the hamt-bit (ldb (byte +hamt-bits+ 0) hash-code)))

(declaim (inline hamt-index))
(defun hamt-index (bitmap bit) (logcount (ldb (byte bit 0)
                                              (the non-negative-fixnum bitmap))))
(declaim (inline hamt-subhash))
(defun hamt-subhash (hash-code) (ash (the non-negative-fixnum hash-code)
                                     (- +hamt-bits+)))

(declaim (inline hamt-subhash-depth))
(defun hamt-subhash-depth (hash-code depth)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum hash-code depth))
  (the non-negative-fixnum
       (ash hash-code
            (the fixnum (- (* depth +hamt-bits+))))))

;;;;;;;;;;;;;
;;; Types ;;;
;;;;;;;;;;;;;

;;; Layers
;;;
;;; Represent layers with simple vectors to avoid indirection of a
;;; separate table.

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
  (declare (type fixnum index)
           (type hamt-layer layer))
  (let ((new-layer (make-array (length layer))))
    (replace new-layer layer :end2 index)
    (setf (aref new-layer index) thing)
    (let ((j (1+ index)))
      (replace new-layer layer :start1 j :start2 j))
    new-layer))

(declaim (inline hamt-layer-open))
(defun hamt-layer-open (layer index)
  (declare (type fixnum index)
           (type simple-vector layer))
  (let ((new-vector (make-array (length layer))))
    (replace new-vector layer :end2 index)
    (replace new-vector layer :start1 (1+ index) :start2 (1+ index))
    new-vector))

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


(declaim (inline hamt-layer-bitop))
(defun hamt-layer-bitop (layer hash-code)
  (let* ((bit (hamt-bit hash-code))
         (bitmap (hamt-layer-bitmap layer))
         (index (hamt-index bitmap bit)))
    ;; 1+ the index so it refers to the layer's vector
    (values bit (1+ index) (logbitp bit bitmap))))

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
  (hash-code 0 :type non-negative-fixnum)
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

(defun hamt-set-find (hamt key hash-code test)
  (declare (type non-negative-fixnum hash-code)
           (function test))
  (labels ((find-list (list)
             (if list
                 (if (funcall test key (car list))
                     (values (car list) t)
                     (find-list (cdr list)))
                 (values nil nil)))
           (rec (hamt subhash)
             ;;(print (list 'find hamt subhash hash-code))
             (declare (type hamt-layer hamt))
             (multiple-value-bind (bit index present) (hamt-layer-bitop hamt hash-code)
               (declare (ignore bit))
               (if present
                   ;; Got entry
                   (let ((thing (hamt-layer-ref hamt index)))
                     (etypecase thing
                       (hamt-layer (rec thing (hamt-subhash (the non-negative-fixnum
                                                                 subhash))))
                       (hamt-set-entry
                        (if (and (= hash-code (hamt-set-entry-hash-code thing))
                                 (funcall test key (hamt-set-entry-key thing)))
                            (values (hamt-set-entry-key thing) t)
                            (values nil nil)))
                       (hamt-bucket
                        (if (= hash-code (hamt-bucket-hash-code thing))
                            (find-list (hamt-bucket-list thing))
                            (values nil nil)))))
                   ;; No entry
                   (values nil nil)))))
    (declare (dynamic-extent (function find-list))
             (dynamic-extent (function rec)))
    (rec hamt hash-code)))

(defun hamt-set-layer-singleton (hash-code key)
  (hamt-layer-singleton (hamt-set-entry hash-code key)
                        hash-code))

(declaim (inline hamt-set-insert-2))
(defun hamt-set-insert-2 (hamt index subhash hash-code key
                          other-subhash other-entry)
  ;; Insert two distinct elements into a new layer
  (declare (simple-vector hamt))
  (labels ((rec (hamt index subhash hash-code key
                 other-subhash other-entry)
             (let* ((bit (hamt-bit subhash))
                    (other-bit (hamt-bit other-subhash))
                    (bitmap (ash 1 bit)))
               (cond
                 ((< bit other-bit)
                  (hamt-layer-nupdate hamt index
                                      (hamt-layer-2 (logior bitmap (ash 1 other-bit))
                                                    (hamt-set-entry hash-code key)
                                                    other-entry)))
                 ((< other-bit bit)
                  (hamt-layer-nupdate hamt index
                                      (hamt-layer-2 (logior bitmap (ash 1 other-bit))
                                                    other-entry
                                                    (hamt-set-entry hash-code key))))
                 (t
                  (let ((new-hamt (hamt-layer-1 bitmap nil)))
                    (setf (aref hamt index) new-hamt)
                    (rec new-hamt 1
                         (hamt-subhash subhash)
                         hash-code key
                         (hamt-subhash other-subhash)
                         other-entry)))))))
    (declare (dynamic-extent (function rec)))
    (rec hamt index subhash hash-code key
         other-subhash other-entry)))

(defun hamt-set-insert (hamt key hash-code test)
  (declare (type non-negative-fixnum hash-code)
           (type function test)
           (type hamt-layer hamt))
  (labels ((rec (hamt hash-code depth)
             (declare (type fixnum depth))
             ;;(assert (< depth 32))
             (multiple-value-bind (bit index present) (hamt-layer-bitop hamt hash-code)
               (if present
                   ;; Got entry
                   (let ((thing (hamt-layer-ref hamt index)))
                     (etypecase thing
                       (hamt-layer (found-layer hamt index
                                                hash-code depth thing))
                       (hamt-set-entry (found-entry hamt index
                                                    hash-code depth thing))
                       (hamt-bucket (found-bucket hamt index
                                                  hash-code depth thing))))
                   ;; No entry, make a new one
                   (new-entry hamt bit index))))
           (found-bucket (hamt index subhash depth bucket)
             (if (= hash-code (hamt-bucket-hash-code bucket))
                 ;; hash collision: check / insert into this bucket
                 (if (member key (hamt-bucket-list bucket) :test test)
                     hamt
                     (update hamt index (hamt-bucket hash-code
                                                     (cons key (hamt-bucket-list bucket)))))
                 ;; different hashes: create a new layer
                 (hamt-set-insert-2 (hamt-layer-open hamt index) index
                                    (hamt-subhash subhash) hash-code key
                                    (hamt-subhash-depth (hamt-bucket-hash-code bucket) depth)
                                    bucket)))
           (found-layer (hamt index hash-code depth layer)
             (let ((new-layer (rec layer (hamt-subhash hash-code) (1+ depth))))
               (if (eq layer new-layer)
                   hamt
                   (update hamt index new-layer))))
           (found-entry (hamt index subhash depth entry)
             (let ((other-hash-code (hamt-set-entry-hash-code entry))
                   (other-key (hamt-set-entry-key entry)))
               (if (= other-hash-code hash-code)
                   ;; matching hash-code
                   (if (funcall test key other-key)
                       ;; already exists, return original hamt
                       hamt
                       ;; hash collision, create a bucket
                       (update hamt index
                               (hamt-bucket hash-code (list key other-key))))
                   ;; different hash-codes, create a new layer
                   (hamt-set-insert-2 (hamt-layer-open hamt index) index
                                      (hamt-subhash subhash) hash-code key
                                      (hamt-subhash-depth other-hash-code depth)
                                      entry))))

           (update (hamt index thing)
             (hamt-layer-update hamt index thing))
           (new-entry (layer bit index)
             (hamt-layer-insert (the hamt-layer layer)
                                bit index
                                (hamt-set-entry hash-code key))))
    (declare (dynamic-extent (function rec))
             (dynamic-extent (function found-bucket))
             (dynamic-extent (function found-layer))
             (dynamic-extent (function found-entry))
             (dynamic-extent (function update))
             (dynamic-extent (function new-entry)))
    (rec hamt hash-code 1)))


(defun hamt-set-ninsert (hamt key hash-code test)
  (declare (type non-negative-fixnum hash-code)
           (type function test)
           (type hamt-layer hamt))
  ;;(declare (optimize (speed 3) (safety 0)))
  (labels ((init (hamt subhash)
             (multiple-value-bind (bit index present) (hamt-layer-bitop hamt subhash)
               (if present
                   (progn (rec-present hamt subhash 1 index)
                          hamt)
                   (hamt-layer-insert hamt
                                      bit index
                                      (hamt-set-entry hash-code key)))))
           (rec (parent pindex hamt subhash depth)
             (declare (type fixnum depth))
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
             (multiple-value-bind (bit index present) (hamt-layer-bitop hamt subhash)
               (if present
                   (rec-present hamt subhash depth index)
                   (update parent pindex
                           (hamt-layer-insert hamt
                                              bit index
                                              (hamt-set-entry hash-code key))))))
           (rec-present (hamt subhash depth index)
             (let ((thing (hamt-layer-ref hamt index)))
               (etypecase thing
                 (hamt-layer ; tail-recursive layer insertion
                  (rec hamt index
                       thing (hamt-subhash subhash) (1+ depth)))
                 (hamt-set-entry (found-entry hamt index
                                              subhash depth thing))
                 (hamt-bucket (found-bucket hamt index
                                            subhash depth thing)))))
           (found-bucket (hamt index subhash depth bucket)
             (if (= hash-code (hamt-bucket-hash-code bucket))
                 ;; hash collision: check / insert into this bucket
                 (unless (member key (hamt-bucket-list bucket) :test test)
                   (push key (hamt-bucket-list bucket)))
                 ;; different hashe-codes: create a new layer
                 (hamt-set-insert-2 hamt index
                                    (hamt-subhash subhash) hash-code key
                                    (hamt-subhash-depth (hamt-bucket-hash-code bucket) depth)
                                    bucket)))
           (found-entry (hamt index subhash depth entry)
             (let ((other-hash-code (hamt-set-entry-hash-code entry))
                   (other-key (hamt-set-entry-key entry)))
               (if (= other-hash-code hash-code)
                   ;; matching hash-code
                   (unless (funcall test key other-key)
                     ;; hash collision, create a bucket
                     (update hamt index
                             (hamt-bucket hash-code (list key other-key))))
                   ;; different hash-codes, create new layer(s)
                   (hamt-set-insert-2 hamt index
                                      (hamt-subhash subhash) hash-code key
                                      (hamt-subhash-depth other-hash-code depth)
                                      entry))))
           (update (hamt index thing)
             (hamt-layer-nupdate hamt index thing)))
    (declare (dynamic-extent (function init))
             (dynamic-extent (function rec))
             (dynamic-extent (function rec-present))
             (dynamic-extent (function found-bucket))
             (dynamic-extent (function found-entry))
             (dynamic-extent (function update)))
    (init hamt hash-code)))


;; (defun hamt-set-remove (hamt key hash-code test)
;;   ;;(print (list 'insert hamt key value hash-code))
;;   (declare (type non-negative-fixnum hash-code)
;;            (type function test))
;;   (labels ((rec (hamt hash-code depth)
;;              (declare (type fixnum depth))
;;              ;;(assert (< depth 32))
;;              ;;(print (list 'rec hamt hash-code depth))
;;              (let* ((bit (hamt-bit hash-code))
;;                     (bitmap (hamt-layer-bitmap hamt))
;;                     (index (hamt-index bitmap bit))
;;                     (entries (hamt-layer-entries hamt)))
;;              t))

;;     (rec hamt hash-code t)))

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
