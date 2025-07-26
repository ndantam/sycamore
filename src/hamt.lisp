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


(declaim (inline hamt-bit
                 hamt-index
                 hamt-subhash
                 hamt-subhash-depth))
(defun hamt-bit (hash-code) (ldb (byte +hamt-bits+ 0) hash-code))
(defun hamt-index (bitmap bit) (logcount (ldb (byte bit 0)
                                              (the non-negative-fixnum bitmap))))
(defun hamt-subhash (hash-code) (ash (the non-negative-fixnum hash-code)
                                     (- +hamt-bits+)))
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
  (array-tree-set layer thing  index))

(declaim (inline hamt-layer-insert))
(defun hamt-layer-insert (layer bit index thing)
  (let ((layer (array-tree-insert-at layer
                                     thing
                                     index)))
    (setf (aref layer 0)
          (logior (aref layer 0) (ash 1 bit)))
    layer))

(declaim (inline hamt-layer-bitop))
(defun hamt-layer-bitop (layer hash-code)
  (let* ((bit (hamt-bit hash-code))
         (bitmap (hamt-layer-bitmap layer))
         (index (hamt-index bitmap bit)))
    ;; 1+ the index so it refers to the layer's vector
    (values bit (1+ index) (logbitp bit bitmap))))


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
                            (find-list (hamt-bucket-list thing) )
                            (values nil nil)))))
                   ;; No entry
                   (values nil nil)))))
    (declare (dynamic-extent (function find-list))
             (dynamic-extent (function rec)))
    (rec hamt hash-code)))

(defun hamt-set-layer-singleton (key hash-code subhash)
  (hamt-layer-singleton (hamt-set-entry hash-code key)
                        subhash))

(defun hamt-set-insert (hamt key hash-code test)
  (declare (type non-negative-fixnum hash-code)
           (type function test))
  (labels ((rec (hamt hash-code depth)
             (declare (type fixnum depth))
             ;;(assert (< depth 32))
             (multiple-value-bind (bit index present) (hamt-layer-bitop hamt hash-code)
               (declare (type fixnum bit index)
                        (type boolean present))
               (if present
                   ;; Got entry
                   (let ((thing (hamt-layer-ref hamt index)))
                     (etypecase thing
                       (hamt-layer (found-layer hamt index
                                                hash-code depth thing) )
                       (hamt-set-entry (found-entry hamt index
                                                    hash-code depth thing))
                       (hamt-bucket (found-bucket hamt index
                                                  hash-code depth thing))))
                   ;; No entry, make a new one
                   (new-entry hamt bit index))))
           (found-bucket (hamt index subhash depth bucket)
             (if (= hash-code (hamt-bucket-hash-code bucket))
                 ;; hash collision: check / insert into this bucket
                 (if (member key (hamt-bucket-list bucket))
                     hamt
                     (update hamt index (hamt-bucket hash-code
                                                     (cons key (hamt-bucket-list bucket)))))
                 ;; different hashes: create a new layer
                 (let ((layer (hamt-layer-singleton
                               bucket
                               (hamt-subhash-depth (hamt-bucket-hash-code bucket) depth))))
                   (update hamt index
                           (rec layer (hamt-subhash subhash) (1+ depth))))))
           (found-layer (hamt index hash-code depth layer)
             (let ((new-layer (rec layer (hamt-subhash hash-code) (1+ depth))))
               (if (eq layer new-layer)
                   hamt
                   (update hamt index new-layer))))
           (found-entry (hamt index subhash depth entry)
             (if (= (hamt-set-entry-hash-code entry)
                    hash-code)
                 ;; matching hash-code
                 (if (funcall test key (hamt-set-entry-key entry))
                     ;; already exists, return original hamt
                     hamt
                     ;; hash collision, create a bucket
                     (update hamt index
                             (hamt-bucket hash-code
                                          (list key (hamt-set-entry-key entry)))) )
                 ;; different hash-codes, create a new layer
                 (let ((layer (hamt-set-layer-singleton (hamt-set-entry-key entry)
                                                        (hamt-set-entry-hash-code entry)
                                                        (hamt-subhash-depth
                                                         (hamt-set-entry-hash-code entry)
                                                         depth))))
                   (update hamt index
                           (rec layer (hamt-subhash subhash) (1+ depth))))))
           (update (hamt index thing)
             (hamt-layer-update hamt index thing))
           (new-entry (layer bit index)
             (hamt-layer-insert layer bit index
                                (hamt-set-entry hash-code key))))
    (declare (dynamic-extent (function rec))
             (dynamic-extent (function found-bucket))
             (dynamic-extent (function found-layer))
             (dynamic-extent (function found-entry))
             (dynamic-extent (function update))
             (dynamic-extent (function new-entry)))
    (rec hamt hash-code 1)))


;; TODO: non-consing insert


;; TODO: remove
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
