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

;;; Constants for bits

(defconstant +hamt-bits+
  ;; Try to keep the bitmap as a fixnum.
  (if (< (expt 2 32) most-positive-fixnum)
      5
      4))
(defconstant +hamt-mask+ (1- (expt 2 +hamt-bits+)))
(deftype hamt-bitmap ()
  `(unsigned-byte ,(expt 2 +hamt-bits+)))


;;; The structure

;; (defstruct (hamt-entry (:constructor hamt-entry (hash-code key)))
;;   (hash-code 0 :type non-negative-fixnum)
;;   key)

;;; Seems that cons cells have lower overhead that structs.  We'll
;;; have many hamt-entries, so let's store those as cons's.

(deftype hamt-entry ()
  'cons)

(declaim (inline hamt-entry
                 hamt-entry-hash-code
                 hamt-entry-key))
(defun hamt-entry (hash-code key) (cons hash-code key))
(defun hamt-entry-hash-code (entry) (the fixnum (car entry)))
(defun hamt-entry-key (entry) (cdr entry))

;; (defstruct (hamt-map-entry
;;             (:include hamt-entry)
;;             (:constructor hamt-map-entry (hash-code key value)))
;;   value)

(defstruct (hamt-bucket (:constructor hamt-bucket (hash-code list)))
  (hash-code 0 :type non-negative-fixnum)
  (list nil :type list))

(defstruct (hamt-layer (:constructor hamt-layer (bitmap entries)))
  (bitmap 0 :type (hamt-bitmap))
  (entries (make-array 0) :type simple-vector))


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
             (let ((bit (hamt-bit subhash))
                   (bitmap (hamt-layer-bitmap hamt)))
               (if (logbitp bit bitmap)
                   ;; Got entry
                   (let ((thing (aref (hamt-layer-entries hamt)
                                      (hamt-index bitmap bit))))
                     (etypecase thing
                       (hamt-layer (rec thing (hamt-subhash (the non-negative-fixnum
                                                                 subhash))))
                       (hamt-entry
                        (if (and (= hash-code (hamt-entry-hash-code thing))
                                 (funcall test key (hamt-entry-key thing)))
                            (values (hamt-entry-key thing) t)
                            (values nil nil)))
                       (hamt-bucket
                        (if (= hash-code (hamt-bucket-hash-code thing))
                            (find-list (hamt-bucket-list thing) )
                            (values nil nil)))))
                   ;; No entry
                   (values nil nil)))))
    (rec hamt hash-code)))


(defun hamt-entry-layer-singleton (entry subhash)
  (hamt-layer (ash 1 (hamt-bit subhash))
              (vector entry)))

(defun hamt-set-layer-singleton (key hash-code subhash)
  ;;(print (list 'singleton hash-code subhash))
  (hamt-entry-layer-singleton (hamt-entry hash-code key)
                               subhash))

(defun hamt-set-insert (hamt key hash-code test)
  ;;(print (list 'insert hamt key value hash-code))
  (declare (type non-negative-fixnum hash-code)
           (type function test))
  (labels ((rec (hamt hash-code depth)
             (declare (type fixnum depth))
             ;;(assert (< depth 32))
             ;;(print (list 'rec hamt hash-code depth))
             (let* ((bit (hamt-bit hash-code))
                    (bitmap (hamt-layer-bitmap hamt))
                    (index (hamt-index bitmap bit))
                    (entries (hamt-layer-entries hamt)))
               (if (logbitp bit bitmap)
                   ;; Got entry
                   (let ((thing (aref (hamt-layer-entries hamt)
                                      index)))
                     (etypecase thing
                       (hamt-layer (found-layer hamt entries bitmap index
                                                hash-code depth thing) )
                       (hamt-entry (found-entry hamt entries bitmap index
                                                hash-code depth thing))
                       (hamt-bucket (found-bucket hamt entries bitmap index
                                                  hash-code depth thing))))
                   ;; No entry, make a new one
                   (new-entry entries bitmap bit index))))
           (found-bucket (hamt entries bitmap index subhash depth bucket)
             (if (= hash-code (hamt-bucket-hash-code bucket))
                 ;; hash collision: check / insert into this bucket
                 (if (member key (hamt-bucket-list bucket))
                     hamt
                     (let ((bucket (hamt-bucket hash-code
                                                (cons key (hamt-bucket-list bucket)))))
                       (hamt-layer bitmap
                                   (array-tree-set entries
                                                   bucket
                                                   index))))
                 ;; different hashes: create a new layer
                 (let ((layer (hamt-entry-layer-singleton
                               bucket
                               (hamt-subhash-depth (hamt-bucket-hash-code bucket) depth))))
                    (hamt-layer bitmap (array-tree-set entries
                                                       (rec layer
                                                            (hamt-subhash subhash)
                                                            (1+ depth))
                                                       index)))))
           (found-layer (hamt entries bitmap index hash-code depth layer)
             (let ((new-layer (rec layer (hamt-subhash hash-code) (1+ depth))))
               (if (eq layer new-layer)
                   hamt
                   (hamt-layer bitmap (array-tree-set entries
                                                      new-layer
                                                      index)))))
           (found-entry (hamt entries bitmap index subhash depth entry)
             (if (= (hamt-entry-hash-code entry)
                    hash-code)
                 ;; matching hash-code
                 (if (funcall test key (hamt-entry-key entry))
                     ;; already exists, return original hamt
                     hamt
                     ;; hash collision, create a bucket
                     (let ((bucket (hamt-bucket hash-code
                                                (list key
                                                      (hamt-entry-key entry)))))
                       (hamt-layer bitmap
                                   (array-tree-set entries bucket index))))
                 ;; different hash-codes, create a new layer
                 (let ((layer (hamt-set-layer-singleton (hamt-entry-key entry)
                                                        (hamt-entry-hash-code entry)
                                                        (hamt-subhash-depth
                                                         (hamt-entry-hash-code entry)
                                                         depth))))
                   (hamt-layer bitmap (array-tree-set entries
                                                      (rec layer
                                                           (hamt-subhash subhash)
                                                           (1+ depth))
                                                      index)))))
           (new-entry (entries bitmap bit index)
             (hamt-layer (logior bitmap (ash 1 bit))
                         (array-tree-insert-at
                          entries
                          (hamt-entry hash-code key)
                          index))))
    (rec hamt hash-code 1)))


;;; Higher-order functions

(defun hamt-set-fold (function initial-value hamt)
  ;; function : (initial-value elt) -> initial-value
  (declare (type function function))
  (loop for thing across (hamt-layer-entries hamt)
        do (setq initial-value
                 (etypecase thing
                   (hamt-layer (hamt-set-fold function initial-value thing))
                   (hamt-entry (funcall function initial-value
                                        (hamt-entry-key thing)))
                   (hamt-bucket
                    (reduce function (hamt-bucket-list thing)
                            :initial-value initial-value)))))
  initial-value)

(defmethod print-object ((object hamt-layer) stream)
  (print-unreadable-object (object stream :type t :identity t)))
