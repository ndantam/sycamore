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

(defun ropep (object)
  (typep object 'rope))

(deftype rope-length-type () `non-negative-fixnum)
(deftype rope-height-type () `(integer 1 #.most-positive-fixnum))

(defstruct rope-node
  (length 0 :type rope-length-type)
  (height 1 :type rope-height-type)
  (left nil :type rope)
  (right nil :type rope))

(declaim (inline object-rope-check))
(defgeneric object-rope (object))

(defun object-rope-check (object)
  "Call (OBJECT-ROPE OBJECT) and check that result is a rope"
  (let ((result (object-rope object)))
    (check-type result rope)
    result))


(declaim (ftype (function (rope) rope-length-type) rope-length))
(defun rope-length (rope)
  "Return the number of characters in rope"
  (etypecase rope
    (rope-node (rope-node-length rope))
    (simple-string (length rope))
    (string (length rope))
    (null 0)
    (symbol (length (symbol-name rope)))
    (character 1)))

(declaim (ftype (function (rope) non-negative-fixnum) rope-height))
(defun rope-height (rope)
  "Return height of rope"
  (etypecase rope
    ((or string symbol character) 0)
    (rope-node (rope-node-height rope))))

(declaim (inline %rope-helper))
(defun %rope-helper (rope)
  (labels ((rope-node-helper (rope)
             (values rope
                     (rope-node-length rope)
                     (rope-node-height rope)))
           (rec (rope)
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
                (rec (rope-list-cat rope)))
               (array
                (rec (rope-array-cat rope)))
               (t (rec (object-rope-check rope))))))
    (rec rope)))

(declaim (ftype (function (t t) rope) %rope))

(defun %rope (first second)
  "Construct a rope from FIRST and SECOND.
FIRST: an object of rope or sequence type
SECOND: an object of rope or sequence type
RETURNS: a rope concatenating FIRST and SECOND"
  (multiple-value-bind (first length-1 height-1)
      (%rope-helper first)
    (multiple-value-bind (second length-2 height-2)
        (%rope-helper second)
      (cond ((zerop length-1)
             second)
            ((zerop length-2)
             first)
            (t
             (make-rope-node :length (+ length-1 length-2)
                             :height (1+ (max height-1 height-2))
                             :left first
                             :right second))))))

(declaim (ftype (function (list) rope) rope-list-cat))
(defun rope-list-cat (list)
  (if (null (cddr list))
      (%rope (first list) (second list))
      (rope-list-cat (loop for rest = list then (cddr rest)
                        while rest
                        collect (%rope (first rest) (second rest))))))

(declaim (ftype (function (array &key
                                 (:start fixnum)
                                 (:end fixnum))
                          rope)
                rope-array-cat))

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


(declaim (inline rope)
         (ftype (function * rope) rope))
(defun rope (&rest args)
  "Concatenate all ropes in ARGS.

Arguments of sequence type will be flattened and concatanted into the
rope.  Other non-rope arguments will be coerced to a rope type by
calling the OBJECT-ROPE generic function.


RETURNS: a rope"
  (declare (dynamic-extent args))
  (when args (rope-list-cat args)))

(declaim (ftype (function (t) rope) rope-1))
(defun rope-1 (rope)
  (etypecase rope
    (rope rope)
    (list
     (rope-list-cat rope))
    (array
     (rope-array-cat rope))
    (t (object-rope-check rope))))


(declaim (inline rope-2))
(defun rope-2 (a1 a2)
  (%rope a1 a2))

(defun rope-3 (a1 a2 a3)
  (%rope a1
         (%rope a2 a3)))

(defun rope-4 (a1 a2 a3 a4)
  (%rope (%rope a1 a2)
         (%rope a3 a4)))
(defun rope-5 (a1 a2 a3 a4 a5)
  (%rope (%rope a1 a2)
         (%rope a3
                (%rope a4 a5))))
(defun rope-6 (a1 a2 a3 a4 a5 a6)
  (%rope (%rope a1 a2)
         (%rope (%rope a3 a4)
                (%rope a5
                       a6))))
(defun rope-7 (a1 a2 a3 a4 a5 a6 a7)
  (%rope (%rope a1
                (%rope a2 a3))
         (%rope (%rope a4 a5)
                (%rope a6 a7))))
(defun rope-8 (a1 a2 a3 a4 a5 a6 a7 a8)
  (%rope (%rope (%rope a1 a2)
                (%rope a3 a4))
         (%rope (%rope a5 a6)
                (%rope a7 a8))))

;; A compiler macro to reduce dispatching when constructing ropes
(define-compiler-macro rope (&whole form &rest args)
  (case (length args)
    (0 nil)
    (1 `(rope-1 ,(car args)))
    (2 `(rope-2 ,@args))
    (3 `(rope-3 ,@args))
    (4 `(rope-4 ,@args))
    (5 `(rope-5 ,@args))
    (6 `(rope-6 ,@args))
    (7 `(rope-7 ,@args))
    (8 `(rope-8 ,@args))
    (otherwise form)))

(declaim (ftype (function (rope &key (:element-type symbol)) simple-string)
                rope-string))
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


(declaim (ftype (function ((or rope pathname)) pathname)
                rope-pathname))

(defun rope-pathname (rope)
  "Convert the rope to a pathname."
  (if (pathnamep rope)
      rope
      (pathname (rope-string rope))))

(defun subrope (rope &key
                       (start 0)
                       end
                       copy)
  "Return the subrope of ROPE,
beginning with element number START
and continuing up to element number END.

START: initial element number of the subrope
END: one past the final element number of the subrope
COPY: if true, copy leaf strings"
  (declare (type fixnum start)
           (type (or fixnum null) end))
  ;; (print (list rope start end))
  (let ((helper (if copy
                    #'subseq
                    (lambda (rope start end)
                      (make-array (- (or end (length rope)) start)
                                  :element-type (array-element-type rope)
                                  :displaced-to rope
                                  :displaced-index-offset start)))))
    (etypecase rope
      (simple-string (funcall helper rope start end))
      (string (funcall helper rope start end))
      (null (unless (and (zerop start) (zerop end))
              (error "Cannot find subrope of ~A" rope))
            rope)
      (symbol (subseq (symbol-name rope) start end))
      (rope-node
       (let* ((end (or end (rope-node-length rope)))
              (left (rope-node-left rope))
              (right (rope-node-right rope))
              (left-count (rope-length left)))
         (cond
           ((<= end left-count)
            (subrope left :start start :end end :copy copy))
           ((>= start left-count)
            (subrope right
                     :start (- start left-count)
                     :end (- end left-count)
                     :copy copy))
           (t (rope (subrope left :start start :end left-count :copy copy)
                    (subrope right :start 0 :end (- end left-count) :copy copy)))))))))



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

(defun output-rope (object place
                    &key
                      ;directory
                      if-exists)
  (let ((object (rope object)))
    (labels ((helper (place)
               (rope-write object :stream place :escape nil)
               (values))
             (path-helper (place)
               (ensure-directories-exist place)
               (with-open-file (s place :direction :output
                                  :if-exists if-exists
                                  :if-does-not-exist :create)
                 (helper s))))
      (cond
        ((streamp place)
         (helper place))
        ((eq place t)
         (helper *standard-output*))
        ((null place)
         object)
        ((ropep place)
         (path-helper (rope-string place)))
        ((pathnamep place)
         (path-helper place))
        (t (error "Unknown place type: ~A" place))))))

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

;;; Comparisons ;;;

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

(defun rope= (rope-1 rope-2)
  (zerop (rope-compare-fast rope-1 rope-2)))

(defun rope/= (rope-1 rope-2)
  (not (rope= rope-1 rope-2)))

(defun rope< (rope-1 rope-2)
  (< (rope-compare-lexographic rope-1 rope-2) 0))

(defun rope<= (rope-1 rope-2)
  (<= (rope-compare-lexographic rope-1 rope-2) 0))

(defun rope> (rope-1 rope-2)
  (> (rope-compare-lexographic rope-1 rope-2) 0))

(defun rope>= (rope-1 rope-2)
  (>= (rope-compare-lexographic rope-1 rope-2) 0))


(declaim (ftype (function (list &key (:symbol-function function)) rope)
                sexp-rope))
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


(defmethod object-rope ((object string))
  object)

(defmethod object-rope ((object rope-node))
  object)

(defmethod object-rope ((object character))
  object)

(defmethod object-rope ((object symbol))
  object)

(defmethod object-rope ((object list))
  (rope-list-cat object))

(defmethod object-rope ((object array))
  (rope-array-cat object))

(defmethod object-rope ((object float))
  (format nil "~F" object))

(defmethod object-rope ((object double-float))
  (format nil "~F" object))

(defmethod object-rope ((object pathname))
  (namestring object))

;; default to using the lisp printer to get a string
(defmethod object-rope ((object t))
  (princ-to-string object))

(defun rope-split (separator sequence
                   &key
                     (start 0)
                     (end (length sequence)))
  (if (<= end start)
      nil
      (flet ((f (a b)
               (rope a separator b)))
        (declare (dynamic-extent #'f))
        (reduce #'f sequence :start start :end end :from-end t))))


(defun rope-map (function sequence
                 &key
                   (start 0)
                   (end (length sequence))
                   separator)
"Apply FUNCTION to each element of SEQUENCE and collect results into a rope.

FUNCTION: (lambda (x)) => ROPE
SEQUENCE: a sequence
START: initial position in SEQUENCE
END: final position in SEQUENCE
SEPARATOR: a rope to splice between the items of SEQUENCE

RETURNS: a rope"
  (declare (type function function))
  (if (>= start end)
      ""
      (flet ((map-sep (item)
               (%rope separator (funcall function item))))
        (declare (dynamic-extent #'map-sep))
        (let ((sep-fun (if separator #'map-sep function)))
          (with-temp-array (tmp (- end start))
            (etypecase sequence
              (list (let ((begin (nthcdr start sequence)))
                      (setf (aref tmp 0)
                            (funcall function (first begin)))
                      (loop
                         for i from 1 below (length tmp)
                         for x in (cdr begin)
                         do (setf (aref tmp i)
                                  (funcall sep-fun x)))))
              (array (setf (aref tmp 0)
                           (funcall function (aref sequence start)))
                     (loop for i from 1
                        for j from (1+ start) below end
                        do (setf (aref tmp i)
                                 (funcall sep-fun (aref sequence j))))))
            (rope-array-cat tmp))))))

(defun rope-parenthesize (rope)
  "Return the parenthesized ROPE."
  (rope #\( rope #\)))
