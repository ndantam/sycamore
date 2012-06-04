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

;;(declaim (optimize (speed 3) (safety 0)))


;;;;;;;;;;;;;;;
;; RED-BLACK ;;
;;;;;;;;;;;;;;;

;; Based on Chris Okasaki's Functional red-black trees
;; (defstruct (red-black
;;              (:include binary-tree)
;;              (:constructor make-red-black (red left value right)))
;;   (red nil :type boolean))

;; (defun red-black-redp (tree)
;;   (red-black-red tree))

;; (defun red-black-blackp (tree)
;;   (not (red-black-redp tree)))

;; (defun red-black-color (red tree)
;;   (make-red-black red
;;                   (red-black-left tree)
;;                   (red-black-value tree)
;;                   (red-black-right tree)))

;; (defun balance-red-black (red left value right)
;;   (labels ((when-red (tree)
;;              (when (red-black-p tree) (red-black-redp tree)))
;;            (balanced-tree (a x b y c z d)
;;              ;(declare (type red-black a b c d))
;;              (make-red-black t
;;                              (make-red-black nil a x b)
;;                              y
;;                              (make-red-black nil c z d))))
;;     (let* ((b (null red))
;;            (l (when-red left))
;;            (r (when-red right))
;;            (ll (when-red (when (red-black-p left)  (red-black-left left ))))
;;            (lr (when-red (when (red-black-p left)  (red-black-right left))))
;;            (rl (when-red (when (red-black-p right) (red-black-left right))))
;;            (rr (when-red (when (red-black-p right) (red-black-right right)))))
;;       (declare (type boolean b l r ll lr rl rr))
;;       (cond
;;         ((and b l ll)
;;          (balanced-tree (binary-tree-left-left left)
;;                         (binary-tree-value-left left)
;;                         (binary-tree-right-left left)
;;                         (binary-tree-value left)
;;                         (binary-tree-right left)
;;                         value
;;                         right))
;;         ((and b l lr)
;;          (balanced-tree (binary-tree-left left)
;;                         (binary-tree-value left)
;;                         (binary-tree-left-right left)
;;                         (binary-tree-value-right left)
;;                         (binary-tree-right-right left)
;;                         value
;;                         right ))
;;         ((and b r rl)
;;          (balanced-tree left
;;                         value
;;                         (binary-tree-left-left right)
;;                         (binary-tree-value-left right)
;;                         (binary-tree-right-left right)
;;                         (binary-tree-value right)
;;                         (binary-tree-right right)))
;;         ((and b r rr)
;;          (balanced-tree left
;;                         value
;;                         (binary-tree-left right)
;;                         (binary-tree-value right)
;;                         (binary-tree-left-right right)
;;                         (binary-tree-value-right right)
;;                         (binary-tree-right-right right)))
;;         (t
;;          (make-red-black red left value right))))))

;; (defun red-black-insert (value tree compare test)
;;   (labels ((ins (tree)
;;              (cond
;;                ((null tree) (make-red-black t nil value nil))
;;                ((funcall compare value (red-black-value tree))
;;                 (balance-red-black (red-black-red tree)
;;                                    (ins (red-black-left tree))
;;                                    (red-black-value tree)
;;                                    (red-black-right tree)))
;;                ((funcall test value (red-black-value tree))
;;                 tree)
;;                (t (balance-red-black (red-black-red tree)
;;                                      (red-black-left tree)
;;                                      (red-black-value tree)
;;                                      (ins (red-black-right tree)))))))
;;     (red-black-color nil (ins tree))))
