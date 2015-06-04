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

;;(declaim (optimize (speed 3) (safety 0)))

;;; TODO: optimizations
;;;   - collapse small ropes into simple-strings
;;;   - height-balance ropes
;;;   - use heap-based stack for very deep ropes

(deftype rope ()
  `(or string symbol rope-node character null))

(deftype rope-length-type () `non-negative-fixnum)
(deftype rope-height-type () `(integer 1 #.most-positive-fixnum))

(defstruct rope-node
  (length 0 :type rope-length-type)
  (height 1 :type rope-height-type)
  (left nil :type rope)
  (right nil :type rope))

(defun rope-length (rope)
  (etypecase rope
    (rope-node (rope-node-length rope))
    (simple-string (length rope))
    (string (length rope))
    (null 0)
    (symbol (length (symbol-name rope)))
    (character 1)))

(defun rope-height (rope)
  (etypecase rope
    ((or string symbol character) 0)
    (rope-node (rope-node-height rope))))

(declaim (inline %rope-helper))
(defun %rope-helper (rope)
  (labels ((rope-node-helper (rope)
             (values rope
                     (rope-node-length rope)
                     (rope-node-height rope))))
    (etypecase rope
      (rope-node (rope-node-helper rope))
      (simple-string
       (values rope (length rope) 0))
      (string
       (values rope (length rope) 0))
      (null
       (values nil 0 0))
      (symbol
       (values rope
               (length (symbol-name rope))
               0))
      (character (values rope 1 0))
      (list
       (rope-node-helper (rope-list-cat rope)))
      (array
       (rope-node-helper (rope-array-cat rope))))))

(defun %rope (first second)
  "Construct a rope from FIRST and SECOND.
FIRST: an object of rope or sequence type
SECOND: an object of rope or sequence type
RETURNS: a rope concatenating FIRST and SECOND"
  (multiple-value-bind (first length-1 height-1)
      (%rope-helper first)
    (if (= 0 length-1)
        second
        (multiple-value-bind (second length-2 height-2)
            (%rope-helper second)
          (if (zerop length-2)
              first
              (make-rope-node :length (+ length-1 length-2)
                              :height (1+ (max height-1 height-2))
                              :left first
                              :right second))))))

(defun rope-list-cat (list)
  (cond
    ((null list)
     nil)
    ((null (cdr list))
     (car list))
    ((null (cddr list))
     (%rope (first list) (second list)))
    (t
     (rope-list-cat (loop for rest = list then (cddr rest)
                       while rest
                       collect (%rope (first rest) (second rest)))))))

(defun rope-array-cat (array
                       &key
                         (start 0)
                         (end (length array)))
  (declare (type non-negative-fixnum start end)
           (type array array))
  (cond
    ((= (1+ start) end)
     (aref array start))
    ((>= start end)
     nil)
    (t
     (let ((midpoint (truncate (+ start end) 2)))
       (%rope (rope-array-cat array :start start :end midpoint)
              (rope-array-cat array :start midpoint :end end))))))

(declaim (inline rope))
(defun rope (&rest args)
  "Concatenate all ropes in ARGS.

Arguments of sequence type will be flattened and concatanted into the rope.

RETURNS: a rope"
  (declare (dynamic-extent args))
  (when args (rope-list-cat args)))

(defun rope-string (rope &key (element-type 'character))
  "Convert the rope to a string."
  (let ((string (make-string (rope-length rope)
                             :element-type element-type)))
    (labels ((visit-string (rope i)
               (replace string rope :start1 i)
               (+ i (length rope)))
             (visit (rope i)
               (etypecase rope
                 (rope-node
                  (visit (rope-node-right rope)
                         (visit (rope-node-left rope) i)))
                 (simple-string
                  (visit-string rope i))
                 (string
                  (visit-string rope i))
                 (null i)
                 (symbol (visit-string (symbol-name rope) i))
                 (character
                  (setf (schar string i) rope)
                  (1+ i)))))
      (declare (dynamic-extent #'visit-string #'visit))
      (visit rope 0))
    string))

(defun rope-subrope (rope start end)
  (declare (type fixnum start end))
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
            (left-length (rope-length left)))
       (if (< i left-length)
           (rope-ref left i)
           (rope-ref (rope-node-right rope)
                     (- i left-length)))))
    (simple-string (aref rope i))
    (string (aref rope i))
    (symbol (aref (symbol-name rope) i))))

(defun rope-write (rope &key
                          (escape *print-escape*)
                          (stream *standard-output*))
  "Write ROPE to STREAM."
  (labels ((rec (rope)
             (etypecase rope
               (rope-node (rec (rope-node-left rope))
                          (rec (rope-node-right rope)))
               (simple-string (write-sequence rope stream))
               (string (write-sequence rope stream))
               (null nil)
               (symbol (write-sequence (symbol-name rope) stream))
               (character (write-char rope stream))
               (sequence (map nil #'rec rope)))))
    (declare (dynamic-extent #'rec))
    (if escape
        (progn (write-char #\" stream)
               (rec rope)
               (write-char #\" stream))
        (rec rope)))
  (values))

(defvar *rope-print* :rope
  "How to print ropes, one of (or :rope :string :structure)")
(declaim (type (member :rope :string :structure) *rope-print*))

(defmethod print-object ((object rope-node) stream)
  (ecase *rope-print*
    (:rope (print-unreadable-object (object stream :type nil :identity nil)
             (princ "ROPE " stream)
             (rope-write object :stream stream :escape t)))
    (:string (rope-write object :stream stream))
    (:structure (call-next-method object stream))))

;;; Iteration ;;;
(defstruct rope-iterator
  (i 0 :type non-negative-fixnum)
  (stack nil :type list))

(defun rope-iterator-push (itr rope)
  (declare (type rope rope))
  (etypecase rope
    (null itr)
    (string
     (push rope (rope-iterator-stack itr))
     itr)
    (symbol
     (push (symbol-name rope) (rope-iterator-stack itr))
     itr)
    (rope-node
     (push rope (rope-iterator-stack itr))
     (rope-iterator-push itr (rope-node-left rope)))))

(defun rope-iterator-pop (itr)
  (let* ((popped (pop (rope-iterator-stack itr)))
         (top (car (rope-iterator-stack itr))))
    (when (stringp popped)
      (assert (= (length popped) (rope-iterator-i itr)))
      (setf (rope-iterator-i itr) 0))
    (if (null top)
        itr
        (let ((left (rope-node-left top))
              (right (rope-node-right top)))
          (cond
            ((eq popped left)
             (rope-iterator-push itr right))
            ((eq popped right)
             (rope-iterator-pop itr))
            (t (error "Popped node is orphaned.")))))))

(defun rope-iterator-next (itr)
  (let ((top (car (rope-iterator-stack itr)))
        (i (rope-iterator-i itr)))
    (declare (type (or null string) top))
    (cond
      ((null top) nil)
      ((= i (length top))
       (rope-iterator-next (rope-iterator-pop itr)))
      ((< i (length top))
       (prog1 (aref top i)
         (incf (rope-iterator-i itr))))
      (t (error "Invalid index during rope iteration.")))))

(defun rope-iterator (rope)
  (rope-iterator-push (make-rope-iterator) rope))

(defun rope-compare-lexographic (rope-1 rope-2)
  "Compare ropes lexographically."
  (let ((itr-1 (rope-iterator rope-1))
        (itr-2 (rope-iterator rope-2)))

    (loop
       for a = (rope-iterator-next itr-1)
       for b = (rope-iterator-next itr-2)
       while (and (and a b)
                  (eql a b))
       finally (return (if a
                           (if b
                               (- (char-code a)
                                  (char-code b))
                               1)
                           (if b -1 0))))))

(defun rope-compare-fast (rope-1 rope-2)
  "Compare ropes quickly.

The resulting order is not necessarily lexographic."
  (if (eq rope-1 rope-2)
      0
      (let ((n-1 (rope-length rope-1))
            (n-2 (rope-length rope-2)))
        (if (= n-1 n-2)
            ;; Compare equal length ropes lexigraphically
            (rope-compare-lexographic rope-1 rope-2)
            ;; Compare different length ropes by size
            (- n-1 n-2)))))


(defun sexp-rope (sexp &key
                         symbol-function)
  "Construct a rope representing S-Expression SEXP.

SYMBOL-FUNCTION: A function to transform symbols in the rope.
                 (lambda (symbol)) => rope
RETURNS: a rope"

  (declare (type list sexp)
           (type (or function null) symbol-function))
  (let ((rope '|(| ))
    (loop
       for rest on sexp
       for first = (car rest)
       do
         (setq rope
               (rope rope
                     (etypecase first
                       (cons (sexp-rope first :symbol-function symbol-function))
                       (symbol
                        (if symbol-function
                            (funcall symbol-function first)
                            first))
                       (string first)
                       (rope first)
                       (fixnum (format nil "~D" first)))
                     (if (cdr rest)
                         '| |
                         '|)|))))
       rope))

(defgeneric object-rope (object))

(defmethod object-rope ((object string))
  object)

(defmethod object-rope ((object rope-node))
  object)

(defmethod object-rope ((object character))
  object)

(defmethod object-rope ((object symbol))
  object)

(defun rope-map (function sequence
                 &key
                   (start 0)
                   end
                   separator)
"Apply FUNCTION to each element of SEQUENCE and collect results into a rope.

FUNCTION: (lambda (x)) => ROPE
SEQUENCE: a sequence
START: initial position in SEQUENCE
END: final position in SEQUENCE
SEQUENCE: a rope to splice between the items of SEQUENCE

RETURNS: a rope"
  (declare (type function function))
  (if (>= start (length sequence))
      ""
      (flet ((map-nosep (rope item)
               (rope rope (funcall function item)))
             (map-sep (rope item)
               (rope rope separator (funcall function item))))
        (declare (dynamic-extent #'map-nosep #'map-sep))
        (reduce (if separator
                    #'map-sep
                    #'map-nosep)
                sequence
                :start (1+ start)
                :initial-value (funcall function (elt sequence start))
                :end end))))
