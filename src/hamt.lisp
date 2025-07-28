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

(declaim (inline hamt-bit))
(defun hamt-bit (hash-code) (ldb (byte +hamt-bits+ 0) hash-code))

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
  (the fixnum
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
  (the non-negative-fixnum (aref layer 0)))

(declaim (inline hamt-layer-update))
(defun hamt-layer-update (layer index thing)
  (array-tree-set layer thing index))

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
  (setf (aref layer index) thing)
  layer)

(declaim (inline hamt-layer-insert))
(defun hamt-layer-insert (layer bit index thing)
  (let ((layer (the simple-vector (array-tree-insert-at layer
                                                        thing
                                                        index))))
    (setf (aref layer 0)
          (logior (the non-negative-fixnum (aref layer 0))
                  (ash 1 bit)))
    layer))

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
;;; hash function collides because we can use the full hash-code. A
;;; decent hash function will rarely collide, so HAMTs will rarely
;;; need to handle collisions.  In contrast, mutable hash tables use
;;; fewer bits of the hash (to index the table), so must more
;;; frequently handle collisions.  The reduce frequency of
;;; collision-handling in the HAMT may lead to improvided lookup times
;;; compared to mutable hash-tables.

(defstruct (hamt-bucket (:constructor hamt-bucket (hash-code list)))
  (hash-code 0 :type non-negative-fixnum)
  (list nil :type list))


;;;;;;;;;;;;;;;;;;;;;;
;;; Set Operations ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Avoid storing separate values for the sets.  Non-colliding entries
;; just need the hash-code and key.

;;; Seems that cons cells have lower overhead that structs.  We'll
;;; have many hamt-entries, so let's store those as cons's.
(deftype hamt-set-entry () 'cons)
(declaim (inline hamt-set-entry
                 hamt-set-entry-hash-code
                 hamt-set-entry-key))
(defun hamt-set-entry (hash-code key) (cons hash-code key))
(defun hamt-set-entry-hash-code (entry) (the fixnum (car entry)))
(defun hamt-set-entry-key (entry) (cdr entry))

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
  ;;(declare (optimize (speed 3) (safety 0)))
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
  (declare (optimize (speed 3) (safety 0)))
  (labels ((init (hamt subhash)
             (multiple-value-bind (bit index present) (hamt-layer-bitop hamt subhash)
               (if present
                   (progn (rec-present hamt subhash 1 index)
                          hamt)
                   (new-entry hamt bit index))))
           (rec (parent pindex hamt subhash depth)
             (declare (type fixnum depth))
             ;; Tail-recursive insertion
             (multiple-value-bind (bit index present) (hamt-layer-bitop hamt subhash)
               (if present
                   (rec-present hamt subhash depth index)
                   (setf (aref parent pindex)
                         (new-entry hamt bit index)))))
           (rec-present (hamt subhash depth index)
             (let ((thing (hamt-layer-ref hamt index)))
                     (etypecase thing
                       (hamt-layer (rec hamt index
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
                 ;; different hashes: create a new layer
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
                   ;; different hash-codes, create a new layer
                   (hamt-set-insert-2 hamt index
                                      (hamt-subhash subhash) hash-code key
                                      (hamt-subhash-depth other-hash-code depth)
                                      entry))))
           (update (hamt index thing)
             (hamt-layer-nupdate hamt index thing))
           (new-entry (layer bit index)
             (hamt-layer-insert (the hamt-layer layer)
                                bit index
                                (hamt-set-entry hash-code key))))
    (declare (dynamic-extent (function init))
             (dynamic-extent (function rec))
             (dynamic-extent (function rec-present))
             (dynamic-extent (function found-bucket))
             (dynamic-extent (function found-entry))
             (dynamic-extent (function update))
             (dynamic-extent (function new-entry)))
    (init hamt hash-code)))


;; TODO: implement remove
;;
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

(defun hamt-set-fold (function initial-value hamt)
  ;; function : (initial-value elt) -> initial-value
  (declare (type function function))
  (loop for i from 1 below (length hamt)
        for thing = (aref hamt i)
        do (setq initial-value
                 (etypecase thing
                   (hamt-layer (hamt-set-fold function initial-value thing))
                   (hamt-set-entry (funcall function initial-value
                                            (hamt-set-entry-key thing)))
                   (hamt-bucket
                    (reduce function (hamt-bucket-list thing)
                            :initial-value initial-value)))))
  initial-value)
