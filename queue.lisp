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



;; From Okasaki "Purely Functional Data Structures"

(defstruct (amortized-queue
             (:constructor %make-amortized-queue (forward reverse)))
  (forward nil :type list)
  (reverse nil :type list))

(defun make-amortized-queue ()
  (%make-amortized-queue nil nil))

(defun amortized-queue (&rest args)
  "Create an amortized queue of ARGS."
  (%make-amortized-queue args nil))

(defun amortized-queue-empty-p (queue)
  "Is the queue empty?"
  (not (or (amortized-queue-forward queue)
           (amortized-queue-reverse queue))))

(defun amortized-enqueue (queue element)
  "Add ELEMENT to QUEUE.
RETURNS: new-queue"
  (%make-amortized-queue (amortized-queue-forward queue)
                         (cons element (amortized-queue-reverse queue))))

(defun amortized-dequeue (queue)
  "Remove first element of QUEUE.
RETURNS: (VALUES new-queue element)"
  (let ((original-forward (amortized-queue-forward queue)))
    (multiple-value-bind (forward reverse)
        (if original-forward
            (values original-forward (amortized-queue-reverse queue))
            (values (reverse (amortized-queue-reverse queue)) nil))
      (values (%make-amortized-queue (cdr forward) reverse)
              (car forward)))))
