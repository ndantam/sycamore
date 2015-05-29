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

(in-package :sycamore)

(deftype rope ()
  `(or string rope-node nil))

(defstruct rope-node
  (count 0 :type non-negative-fixnum)
  (left nil :type rope)
  (right nil :type rope))


(defun rope-count (rope)
  (etypecase rope
    (rope-node (rope-node-count rope))
    (simple-string (length rope))
    (string (length rope))))

(defun %rope-cat (first second)
  (make-rope-node :count (+ (rope-count first)
                            (rope-count second))
                  :left first
                  :right second))

(defun rope-cat (sequence)
  "Concatenate all ropes in SEQUENCE."
  (reduce #'%rope-cat sequence))

(defun rope (&rest args)
  "Concatenate all ropes in ARGS."
  (declare (dynamic-extent args))
  (when args (rope-cat args)))


(defun rope-string (rope &key (element-type 'character))
  "Convert the rope to a string."
  (let ((string (make-string (rope-count rope)
                             :element-type element-type)))
    (labels ((visit-string (rope i)
               (replace string rope :start1 i)
               (+ i (length rope)))
             (visit (rope i)
               (etypecase rope
                 (simple-string
                  (visit-string rope i))
                 (string
                  (visit-string rope i))
                 (rope-node
                  (visit (rope-node-right rope)
                         (visit (rope-node-left rope) i)))
                 (null))))
      (declare (dynamic-extent #'visit-string #'visit))
      (visit rope 0))
    string))

(defun rope-subrope (rope start end)
  (labels ((helper-string (rope start end)
             (make-array (- end start)
                         :element-type (array-element-type rope)
                         :displaced-to rope
                         :displaced-index-offset start)))
    (etypecase rope
      (simple-string (helper-string rope start end))
      (string (helper-string rope start end)))))

(defun rope-ref (rope i)
  "Return the character at position I."
  (declare (type rope rope)
           (type non-negative-fixnum i))
  (etypecase rope
    (rope-node
     (let* ((left (rope-node-left rope))
            (left-count (rope-count left)))
       (if (< i left-count)
           (rope-ref left i)
           (rope-ref (rope-node-right rope)
                    (- i left-count)))))
    (simple-string (aref rope i))
    (string (aref rope i))))

(defun rope-write (rope &key
                          (escape *print-escape*)
                          (stream *standard-output*))
  "Write ROPE to STREAM."
  (labels ((write-1 (rope)
             (write rope
                    :stream stream
                    :escape nil))
           (rec (rope)
             (etypecase rope
               (rope-node (rec (rope-node-left rope))
                          (rec (rope-node-right rope)))
               (simple-string (write-1 rope))
               (string (write-1 rope))
               (sequence (map nil #'write-1 rope)))))
    (declare (dynamic-extent #'write-1 #'rec))
    (when escape (write-char #\" stream))
    (rec rope)
    (when escape (write-char #\" stream)))
  (values))

(defvar *print-rope-string* t
  "If true, ropes are printed as Lisp strings instead of structures.")

(defmethod print-object ((object rope-node) stream)
  (if *print-rope-string*
      (rope-write object :stream stream)
      (call-next-method object stream)))
