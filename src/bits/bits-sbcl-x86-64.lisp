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

;;; See: https://pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/

(sb-c:defknown %bit-popcnt ((unsigned-byte 64)) (integer 0 64)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-c:define-vop (%bit-popcnt)
  (:policy :fast)
  (:translate %bit-popcnt)
  (:args (x :scs (sb-vm::unsigned-reg) :target r))
  (:arg-types sb-vm::unsigned-num)
  (:results (r :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 3
    (unless (sb-vm::location= r x) ; only break the spurious dep. chain
      (sb-vm::inst xor r r))       ; if r isn't the same register as x.
    (sb-vm::inst popcnt r x)))

(sb-c:defknown %bit-ctz ((unsigned-byte 64)) (integer 0 64)
    (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

(sb-c:define-vop (%bit-ctz)
  (:policy :fast)
  (:translate %bit-ctz)
  (:args (x :scs (sb-vm::unsigned-reg) :target r))
  (:arg-types sb-vm::unsigned-num)
  (:results (r :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 3
    (sb-vm::inst bsf r x)))
