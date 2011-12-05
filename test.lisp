;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2011, Georgia Tech Research Corporation
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

(lisp-unit:define-test dfa-equal-basic
  (lisp-unit:assert-true (dfa-equal (make-fa '((0 a 1) (1 b 0)) 0 1)
                                    (make-fa '((x a y) (y b x)) 'x 'y)))
  (lisp-unit:assert-false (dfa-equal (make-fa '((0 a 1) (1 b 0)) 0 1)
                                     (make-fa '((x a y) (y b x)) 'y 'x))))



(lisp-unit:define-test dfa-minimize
  (lisp-unit:assert-true
   (dfa-equal (dfa-minimize-brzozowski (make-fa '((0 a 1) (1 b 2) (2 a 1)) 0 1))
              (make-fa '((0 a 1) (1 b 0)) 0 1)))
  (flet ((min-cmp (dfa)
           (dfa-equal (dfa-minimize-brzozowski dfa)
                      (dfa-minimize-hopcroft dfa))))
    (lisp-unit:assert-true
     (min-cmp (make-fa '((0 a 1) (1 b 2) (2 a 1)) 0 1)))))
