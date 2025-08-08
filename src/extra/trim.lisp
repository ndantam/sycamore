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


(defun wb-tree-trim (tree lo hi compare)
  "Return subtree rooted between `lo' and `hi'."
  (declare (type function compare))
  ;(declare (optimize (speed 3) (safety 0)))
  (cond
    ((null tree) nil)
    ((< (the fixnum (funcall compare (binary-tree-value tree) lo)) 0)
     (wb-tree-trim (binary-tree-right tree) lo hi compare))
    ((< (the fixnum (funcall compare hi (binary-tree-value tree))) 0)
     (wb-tree-trim (binary-tree-left tree) lo hi compare))
    (t tree)))

;; root between lo and +infinity
(defun wb-tree-trim-lo (tree lo compare)
  (declare (type function compare))
  (cond
    ((null tree) nil)
    ((< (funcall compare lo (binary-tree-value tree)) 0)
     tree)
    (t (wb-tree-trim-lo (wb-tree-right tree) lo compare))))


;; root between -infinity and hi
(defun wb-tree-trim-hi (tree hi compare)
  (declare (type function compare))
  (cond
    ((null tree) nil)
    ((> (funcall compare hi (binary-tree-value tree)) 0)
     tree)
    (t (wb-tree-trim-hi (wb-tree-left tree) hi compare))))


(defun wb-tree-split-less (tree x compare)
  "Everything in tree before than x"
  (declare (type function compare))
  (cond-wb-tree-compare (x tree compare)
                         nil
                         (wb-tree-split-less (binary-tree-left tree) x compare)
                         (binary-tree-left tree)
                         (join-wb-tree (binary-tree-left tree)
                                        (binary-tree-value tree)
                                        (wb-tree-split-less (binary-tree-right tree) x compare)
                                        compare)))


(defun wb-tree-split-greater (tree x compare)
  "Everything in tree after than x"
  (declare (type function compare))
  ;(declare (optimize (speed 3) (safety 0)))
  (cond-wb-tree-compare (x tree compare)
                         nil
                         (join-wb-tree (wb-tree-split-greater (binary-tree-left tree) x compare)
                                        (binary-tree-value tree)
                                        (binary-tree-right tree)
                                        compare)
                         (binary-tree-right tree)
                         (wb-tree-split-greater (binary-tree-right tree) x compare)))


;; tree-2 rooted between lo and hi
(defun wb-tree-uni-bd (tree-1 tree-2 lo hi compare)
  (declare (type function compare))
  (let ((tree-2 (wb-tree-trim tree-2 lo hi compare)))
    (cond
      ((null tree-2) tree-1)
      ((null tree-1)
       (join-wb-tree (wb-tree-split-greater (wb-tree-left tree-2) lo compare)
                      (wb-tree-value tree-2)
                      (wb-tree-split-less (wb-tree-right tree-2) hi compare)
                      compare))
      (t (join-wb-tree (wb-tree-uni-bd (wb-tree-left tree-1)
                                         tree-2 lo (wb-tree-value tree-1) compare)
                        (wb-tree-value tree-1)
                        (wb-tree-uni-bd (wb-tree-right tree-1)
                                         tree-2 (wb-tree-value tree-1) hi compare)
                        compare)))))

;; tree-2 between -inf and hi
(defun wb-tree-uni-hi (tree-1 tree-2 hi compare)
  (let ((tree-2 (wb-tree-trim-hi tree-2 hi compare)))
    (cond
      ((null tree-2) tree-1)
      ((null tree-1) (wb-tree-split-less tree-2 hi compare))
      (t (join-wb-tree (wb-tree-uni-hi (wb-tree-left tree-1) tree-2 (wb-tree-value tree-1) compare)
                        (wb-tree-value tree-1)
                        (wb-tree-uni-bd (wb-tree-right tree-1) tree-2 (wb-tree-value tree-1) hi compare)
                        compare)))))

;; tree-2 between lo and +inf
(defun wb-tree-uni-lo (tree-1 tree-2 lo compare)
  (let ((tree-2 (wb-tree-trim-lo tree-2 lo compare)))
    (cond
      ((null tree-2) tree-1)
      ((null tree-1) (wb-tree-split-greater tree-2 lo compare))
      (t (join-wb-tree (wb-tree-uni-bd (wb-tree-left tree-1) tree-2 lo (wb-tree-value tree-1) compare)
                        (wb-tree-value tree-1)
                        (wb-tree-uni-lo (wb-tree-right tree-1) tree-2 (wb-tree-value tree-1) compare)
                        compare)))))

(defun wb-tree-hedge-union (tree-1 tree-2 compare)
  (declare (type function compare))
  (cond
    ((null tree-1) tree-2)
    ((null tree-2) tree-1)
    (t (with-wb-tree (l1 v1 r1) tree-1
         (join-wb-tree (wb-tree-uni-hi l1 tree-2 v1 compare)
                        v1
                        (wb-tree-uni-lo r1 tree-2 v1 compare)
                        compare)))))
