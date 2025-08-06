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


;;;;;;;;;;;
;; QUEUE ;;
;;;;;;;;;;;
(defun queue-fuzz-generator ()
  (loop for i below (random (expt 2 10))
     collect
       (if (zerop (random 2))
           `(:enqueue ,(random (expt 2 10)))
           '(:dequeue))))

(defun queue-fuzz-tester (fuzz)
  (fuzz::do-operations
      ((queue list) (list (make-amortized-queue) nil))
      fuzz
    ((:enqueue arg)
     (list (amortized-enqueue queue arg)
           (append list (list arg))))
    ((:dequeue)
     (multiple-value-bind (q v) (amortized-dequeue queue)
       (and (equal v (car list))
            (list q (cdr list)))))))

(defun run-queue-tests (&key (count 1))
  (fuzz:run-tests #'queue-fuzz-generator
                  #'queue-fuzz-tester
                  :count count))

;;;;;;;;;;
;; Heap ;;
;;;;;;;;;;

;; (defun heap-fuzz-generator ()
;;   (loop for i below (random (expt 2 12))
;;      collect
;;        (case (random 3)
;;          (0 `(:insert ,(random (expt 2 12))))
;;          (1 '(:find-min))
;;          (2 `(:remove-min)))))

;; (defun heap-fuzz-tester (fuzz)
;;   (fuzz::do-operations
;;       ((list pairing-heap tree-heap) (list nil nil (make-tree-heap #'identity)))
;;       fuzz
;;     ((:insert arg)
;;      (list (cons arg list)
;;            (pairing-heap-insert pairing-heap arg #'-)
;;            (tree-heap-insert tree-heap arg)))
;;     ((:find-min)
;;      (if list
;;          (let ((new-list (sort list #'<)))
;;            (and (equal (car new-list)
;;                        (pairing-heap-find-min pairing-heap))
;;                 (equal (car new-list)
;;                        (tree-heap-find-min tree-heap))
;;                 (list new-list pairing-heap tree-heap)))
;;          (and (null pairing-heap)
;;               (tree-heap-empty-p tree-heap)
;;               (list list pairing-heap tree-heap))))
;;     ((:remove-min)
;;      (if list
;;          (multiple-value-bind (ph-1 pmin) (pairing-heap-remove-min pairing-heap #'-)
;;            (multiple-value-bind (th-1 tmin) (tree-heap-remove-min tree-heap)
;;              (destructuring-bind (lmin &rest lh-1) (sort list #'<)
;;                (and (equal lmin pmin)
;;                     (equal lmin tmin)
;;                     (list lh-1 ph-1 th-1)))))
;;          (and (null pairing-heap)
;;               (tree-heap-empty-p tree-heap)
;;               (list list pairing-heap tree-heap))))))

;; (defun run-heap-tests (&key (count 1))
;;   (fuzz:run-tests #'heap-fuzz-generator
;;                   #'heap-fuzz-tester
;;                   :count count))



;;;;;;;;;
;; BAG ;;
;;;;;;;;;

(defun bag-fuzz-generator ()
  (loop
     for i below (1+ (random (expt 2 10)))
     collect
       (random 32)))


(defun bag-fuzz-tester (fuzz)
  (let ((bag (fuzz:test-true 'produce-bag
                             (lambda ()
                               (fold #'tree-bag-insert (tree-bag #'fixnum-compare) fuzz))))
        (hash (fuzz:test-true 'produce-hash
                              (lambda ()
                                (fold (lambda (h x)
                                        (setf (gethash x h) (1+ (gethash x h 0)))
                                        h)
                                      (make-hash-table)
                                      fuzz)))))
    (loop for k being the hash-keys of hash
         do
         (fuzz:test= 'bag-count=
                     (lambda () (gethash k hash))
                     (lambda () (tree-bag-count bag k))))))

(defun run-bag-tests (&key (count 1))
  (fuzz:run-tests #'bag-fuzz-generator
                  #'bag-fuzz-tester
                  :formatter #'identity
                  :count count))

;;;;;;;;;;;;;;
;; TREE-SET ;;
;;;;;;;;;;;;;;

(defun make-tree-set-fuzz-generator (count max)
  (lambda ()
    (loop for i below 2
          collect (loop
                    for i below (1+ (random count))
                    collect
                    (random max)))))


(defun tree-set-fuzz-tester (fuzz)
  (let* ((list-1 (remove-duplicates (first fuzz)))
         (list-2 (remove-duplicates (second fuzz)))
         (compare #'fixnum-compare)
         (set-1 (fuzz:test-true 'build-wb-1
                                (lambda () (build-wb-tree compare nil list-1))))
         (set-2 (fuzz:test-true  'build-wb-2
                                 (lambda () (build-wb-tree compare nil list-2))))
         (set-p-1))
    (labels ((set-sort (x) (sort (copy-list x) #'<))
             (set-result (x)
               (setq set-p-1 x)
               (wb-tree-list x)))

      ;; constructed sets
      (fuzz:do-test ('wb-elements-1 :test #'equal)
        (set-sort list-1)
        (wb-tree-list set-1))
      (fuzz:do-test ('wb-elements-2 :test #'equal)
        (set-sort list-2)
        (wb-tree-list set-2))

      ;; balance
      (fuzz:test-true 'wb-balanced-1 (lambda () (wb-tree-balanced-p set-1)))
      (fuzz:test-true 'wb-balanced-2 (lambda () (wb-tree-balanced-p set-2)))
      (fuzz:test-true 'wb-balanced-sorted-1
                      (lambda () (wb-tree-balanced-p
                                  (build-wb-tree #'fixnum-compare nil (set-sort list-1)))))
      (fuzz:test-true 'wb-balanced-sorted-2
                      (lambda () (wb-tree-balanced-p
                                  (build-wb-tree #'fixnum-compare nil (set-sort list-2)))))

      ;; join balance
      (map-binary-tree :inorder nil
                       (lambda (x)
                         (with-wb-tree-split (l p r) set-1 x compare
                           (assert p)
                           (fuzz:test-true 'join-balanced
                                           (lambda () (wb-tree-balanced-p (join-wb-tree l x r
                                                                                          compare))))))
                       set-1)
      (map-binary-tree :inorder nil
                       (lambda (x)
                         (with-wb-tree-split (l p r) set-2 x compare
                           (assert p)
                           (fuzz:test-true 'join-balanced
                                           (lambda () (wb-tree-balanced-p (join-wb-tree l x r
                                                                                          compare))))))
                       set-2)


      ;; union
      (fuzz:do-test ('wb-union :test #'equal)
        (set-sort (union list-1 list-2))
        (set-result (wb-tree-union set-1 set-2 #'fixnum-compare)))
      (fuzz:test-true 'wb-union-balanced-1 (lambda () (wb-tree-balanced-p set-p-1)))

      ;; intersection
      (fuzz:do-test ('wb-intersection :test #'equal)
        (set-sort (intersection list-1 list-2))
        (set-result (wb-tree-intersection set-1 set-2 #'fixnum-compare)))
      (fuzz:test-true 'wb-intersection-balanced-1 (lambda () (wb-tree-balanced-p set-p-1)))

      ;; intersection-p
      (fuzz:test-eq 'wb-intersection-p
                    (lambda () (if (intersection list-1 list-2) t nil))
                    (lambda () (wb-tree-intersection-p set-1 set-2 #'fixnum-compare)))

      ;; subset-p
      (fuzz:test-eq 'wb-subset
                    (lambda () (if (subsetp list-1 list-2) t nil))
                    (lambda () (wb-tree-subset set-1 set-2 #'fixnum-compare)))

      ;; difference
      (fuzz:do-test ('wb-difference :test #'equal)
        (set-sort (set-difference list-1 list-2))
        (set-result (wb-tree-difference set-1 set-2 #'fixnum-compare)))
      (fuzz:test-true 'wb-difference-balanced-1 (lambda () (wb-tree-balanced-p set-p-1))))))

(defun run-tree-set-tests (&key (count 1))
  (fuzz:run-tests (make-tree-set-fuzz-generator (expt 2 12) (expt 2 12))
                  #'tree-set-fuzz-tester
                  :formatter #'identity
                  :count count)
  (fuzz:run-tests (make-tree-set-fuzz-generator (expt 2 12) (expt 2 4))
                  #'tree-set-fuzz-tester
                  :formatter #'identity
                  :count count)
  (fuzz:run-tests (make-tree-set-fuzz-generator (expt 2 10) (expt 2 20))
                  #'tree-set-fuzz-tester
                  :formatter #'identity
                  :count count)
  t)

;;;;;;;;;;;;;;
;; HASH-SET ;;
;;;;;;;;;;;;;;


(defun hamt-fuzz-generator ()
  (let ((vals '((12 12)
                (12 4)
                (10 20))))
    (append
     (loop with (count-pow max-pow) = (elt vals (random (length vals)))
           with count = (expt 2 count-pow)
           with max = (expt 2 max-pow)
           for i below 2
           collect (loop
                     for i below (1+ (random count))
                     collect
                     (random max)))
     (list
      (let* ((bits (integer-length most-positive-fixnum))
             (min-bits 8)
             (mask (1- (ash 1 (+ min-bits (random (- bits min-bits)))))))
        (lambda (x) (logand mask (sxhash x))))))))


(defun hamt-fuzz-tester (fuzz)
  (sb-ext:gc :full t)
  (labels ((set-sort (x) (sort (copy-list x) (lambda (x y)
                                               (declare (type fixnum x y))
                                               (< x y))))
           (hash-set-sort (h) (set-sort (hash-set-list h)))
           (hash-table-contains (h key)
             (multiple-value-bind (v p) (gethash key h)
               (declare (ignore v))
               p))
           (hash-table-intersection-p (h1 h2)
             (when (> (hash-table-count h1)
                      (hash-table-count h2))
               (rotatef h1 h2))
             (loop for k being the hash-keys of h1
                   when (hash-table-contains h2 k)
                     return t))
           (hash-table-intersection (h1 h2)
             (when (> (hash-table-count h1)
                      (hash-table-count h2))
               (rotatef h1 h2))
             (loop for k being the hash-keys of h1
                   when (gethash k h2)
                     collect k)))
    (let* ((h-set-insert (lambda (h x) (setf (gethash x h) x) h))
           (arg-list-1 (first fuzz))
           (arg-list-2 (second fuzz))
           (hash-1 (fold h-set-insert (make-hash-table) arg-list-1))
           (hash-2 (fold h-set-insert (make-hash-table) arg-list-2))
           (list-1 (set-sort (hash-table-keys hash-1)))
           (list-2 (set-sort (hash-table-keys hash-2)))
           (hash-function (third fuzz)))

      ;; Set Tests
      (let ((set-1 (list-hash-set arg-list-1 :hash-function hash-function))
            (set-2 (list-hash-set arg-list-2 :hash-function hash-function)))

        ;; Non-consing Construct
        (fuzz:do-test ('hash-elements-1 :test #'equal)
                      list-1
                      (set-sort (hash-set-list set-1)))
        (fuzz:do-test ('hash-elements-2 :test #'equal)
                      (set-sort list-2)
                      (set-sort (hash-set-list set-2)))

        (hamt-set-check (hash-set-root set-1))
        (hamt-set-check (hash-set-root set-2))

        ;; Construct
        (fuzz:do-test ('hash-insert-elements-1 :test #'equal)
                      list-1
                      (hash-set-sort
                       (reduce #'hash-set-insert list-1
                               :initial-value
                               (make-hash-set :hash-function hash-function))))
        (fuzz:do-test ('hash-insert-elements-2 :test #'equal)
                      list-2
                      (hash-set-sort (reduce #'hash-set-insert list-2
                                             :initial-value
                                             (make-hash-set :hash-function hash-function))))

        ;; Member
        (fuzz:do-test ('hash-member-1 :test #'equal)
                      (loop for x in list-1
                            collect (hash-table-contains hash-2 x))
                      (loop for x in list-1
                            collect (hash-set-member-p set-2 x)))

        ;; Insert
        (fuzz:do-test ('hash-insert :test #'equal)
                      (set-sort (let ((h (make-hash-table)))
                                  (dolist (x list-1) (setf (gethash x h) t))
                                  (dolist (x list-2) (setf (gethash x h) t))
                                  (hash-table-keys h)))
                      (hash-set-sort
                       (reduce #'hash-set-insert
                               list-2
                               :initial-value set-1)))

        ;; Remove
        (fuzz:do-test ('hash-remove :test #'equal)
                      (set-sort (let ((h (make-hash-table)))
                                  (dolist (x list-1) (setf (gethash x h) t))
                                  (dolist (x list-2) (remhash x h))
                                  (hash-table-keys h)))
                      (hash-set-sort
                       (reduce #'hash-set-remove
                               list-2
                               :initial-value set-1)))

        ;; Union
        (fuzz:do-test ('hash-union :test #'equal)
                      (set-sort (union list-1 list-2))
                      (set-sort (hash-set-list
                                 (hash-set-union set-1 set-2))))

        ;; Intersection
        (fuzz:do-test ('hash-intersection :test #'equal)
                      (set-sort (hash-table-intersection hash-1 hash-2))
                      (set-sort (hash-set-list
                                 (hash-set-intersection set-1 set-2))))

        ;; Difference
        (fuzz:do-test ('hash-difference :test #'equal)
                      (set-sort (loop for x in list-1
                                      unless (gethash x hash-2)
                                        collect x))
                      (hash-set-sort
                       (hash-set-difference set-1 set-2)))

        ;; intersection-p
        (fuzz:test-eq 'hash-intersection-p
                      (lambda () (hash-table-intersection-p hash-1 hash-2))
                      (lambda () (hash-set-intersection-p set-1 set-2)))

        ;; subset-p
        (fuzz:test-eq 'hash-subset-p
                      (lambda () (if (subsetp list-1 list-2) t nil))
                      (lambda () (hash-set-subset-p set-1 set-2)))

        )

      ;; Map Tests
      (let* ((alist-1 (loop for x in arg-list-1 collect (cons x (1+ x))))
             (alist-2 (loop for x in arg-list-2 collect (cons x (1+ x))))
             (hash-1 (alist-hash-table alist-1))
             (hash-2 (alist-hash-table alist-2))
             (map-1 (alist-hash-map alist-1))
             (map-2 (alist-hash-map alist-2))
             )
             ;;

        ;; Alist Construct
        (fuzz:do-test ('alist-hash-map :test #'equalp)
                      hash-1
                      (hash-map-hash-table map-1))
        (fuzz:do-test ('alist-hash-map :test #'equalp)
                      hash-2
                      (hash-map-hash-table map-2))

        ;; Hash Table Construct
        (fuzz:do-test ('hash-table-hash-map :test #'equalp)
                      hash-1
                      (hash-map-hash-table (hash-table-hash-map hash-1)))
        (fuzz:do-test ('hash-table-hash-map :test #'equalp)
                      hash-2
                      (hash-map-hash-table (hash-table-hash-map hash-2)))

        ;; To Alist
        (fuzz:do-test ('hash-map-alist :test #'equalp)
                      hash-1
                      (alist-hash-table (hash-map-alist map-1)))
        (fuzz:do-test ('hash-map-alist :test #'equalp)
                      hash-2
                      (alist-hash-table (hash-map-alist map-2)))

        ;; Find
        (fuzz:do-test ('hash-map-find :test #'equal)
                      (loop for x in list-1
                            collect (multiple-value-list (gethash x hash-2)))
                      (loop for x in list-1
                            collect (multiple-value-list (hash-map-find map-2 x))))

        ;; Contains
        (fuzz:do-test ('hash-map-contains :test #'equal)
                      (loop for x in list-1
                            collect (hash-table-contains hash-2 x))
                      (loop for x in list-1
                            collect (hash-map-contains map-2 x)))

        ;; Remove
        (fuzz:do-test ('hash-map-remove :test #'equalp)
                      (loop with h = (copy-hash-table hash-1)
                            for k being the hash-keys of hash-2
                            do (remhash k h)
                            finally (return h))
                      (hash-map-hash-table
                       (fold-hash-map-keys #'hash-map-remove map-1 map-2)))

        ;; Insert
        (fuzz:do-test ('hash-map-insert :test #'equalp)
                      (loop with h = (copy-hash-table hash-1)
                            for k being the hash-keys of hash-2
                              using (hash-value v)
                            do (setf (gethash k h) v)
                            finally (return h))
                      (hash-map-hash-table
                       (fold-hash-map #'hash-map-insert map-1 map-2)))


      ))))

(defun run-hamt-tests (&key (count 1))
  (fuzz:run-tests #'hamt-fuzz-generator
                  #'hamt-fuzz-tester
                  :formatter #'identity
                  :count count)
  t)
