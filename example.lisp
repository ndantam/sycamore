;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2015, Rice University
;;;; All rights reserved.
;;;;
;;;;   Redistribution and use in source and binary forms, with or
;;;;   without modification, are permitted provided that the following
;;;;   conditions are met:
;;;;
;;;;   * Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;   * Neither the name of copyright holder the names of its
;;;;     contributors may be used to endorse or promote products
;;;;     derived from this software without specific prior written
;;;;     permission.
;;;;
;;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;;   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;;   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;;   AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;;   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;;;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;;   POSSIBILITY OF SUCH DAMAGE.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EXAMPLE USAGE FOR SYCAMORE ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
;;; ROPES ;;;
;;;;;;;;;;;;;

;; Make a rope
(sycamore:rope "Hello" #\Space 'World!)

;; Also works on lists
(sycamore:rope (list "Hello" #\Space 'World!))

;; and arrays
(sycamore:rope (vector "Hello" #\Space 'World!))

;; Rope to string
(sycamore:rope-string (sycamore:rope "Hello" #\Space 'World!))

;; Write a rope
(sycamore:rope-write (sycamore:rope "Hello" #\Space 'World!)
                     :escape nil :stream *standard-output*)


;;;;;;;;;;;;;;;;;
;;; HASH SETS ;;;
;;;;;;;;;;;;;;;;;

;; Create a set with default EQL test
(sycamore:make-hash-set)

;; Create a set with default EQL test and initial elements from a list
(sycamore:list-hash-set '(1 2 -10 40))

;; Create a set with EQUALP test
(sycamore:list-hash-set '(1 1.0 2.0 2) :test #'equalp)

;; Insertion
(sycamore:hash-set-insert (sycamore:list-hash-set '(1 2))
                          0)

;; Lookup
(sycamore:hash-set-find (sycamore:list-hash-set '(0 1 2))
                        0)

;; Removal
(sycamore:hash-set-remove (sycamore:list-hash-set '(1 2 0))
                          0)

;; Union
(sycamore:hash-set-union (sycamore:list-hash-set '(1 2 0))
                         (sycamore:list-hash-set '(1 0 3)))

;; Intersection
(sycamore:hash-set-intersection (sycamore:list-hash-set '(1 2 0))
                                (sycamore:list-hash-set '(1 0 3)))

;; Difference
(sycamore:hash-set-difference (sycamore:list-hash-set '(1 2 0))
                              (sycamore:list-hash-set '(1 0 3)))

;; Map set
(sycamore:map-hash-set 'list #'1+
                       (sycamore:list-hash-set '(1 2 0)))

;; Fold set
(sycamore:fold-hash-set (lambda (list item) (cons item list))
                        nil
                        (sycamore:list-hash-set '(1 2 0)))

;;;;;;;;;;;;;;;;;
;;; TREE SETS ;;;
;;;;;;;;;;;;;;;;;

;; Define an ordering function
(defun compare (a b)
  (cond ((< a b) -1)
        ((> a b) 1)
        (t 0)))

;; Create a set for integers
(sycamore:tree-set #'compare 1 2 -10 40)

;; Insertion
(sycamore:tree-set-insert (sycamore:tree-set #'compare 1 2)
                          0)

;; Lookup
(sycamore:tree-set-find (sycamore:tree-set #'compare 1 2 0)
                        0)

;; Removal
(sycamore:tree-set-remove (sycamore:tree-set #'compare 1 2 0)
                          0)

;; Union operation
(sycamore:tree-set-union (sycamore:tree-set #'compare 1 2)
                         (sycamore:tree-set #'compare 1 0 3))

;; Intersection operation
(sycamore:tree-set-intersection (sycamore:tree-set #'compare 1 2)
                                (sycamore:tree-set #'compare 1 0 3))

;; Difference operation
(sycamore:tree-set-difference (sycamore:tree-set #'compare 1 2)
                              (sycamore:tree-set #'compare 1 0 3))

;; Map set
(sycamore:map-tree-set 'list #'1+
                        (sycamore:tree-set #'compare 1 0 10 2))

;; Fold set
(sycamore:fold-tree-set (lambda (list item) (cons item list))
                        nil
                        (sycamore:tree-set #'compare 1 0 10 2))
