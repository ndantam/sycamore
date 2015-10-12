;;;;  Copyright (c) 2015, Rice University
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

(in-package :sycamore-cgen)

(defparameter *cgen-indent-mark* #\Tab)
(defparameter *cgen-indent* "")
(defparameter *cgen-newline-indent* #\Newline)

(defmacro with-cgen-indent (&body body)
  `(let* ((*cgen-indent* (rope *cgen-indent-mark* *cgen-indent*))
          (*cgen-newline-indent* (rope #\Newline *cgen-indent*)))
     ,@body))

(defun cgen-stmt (stmt) (rope stmt #\;))


(defun cgen-return (value) (cgen-stmt (rope "return " value)))

;; TODO: escape
(defun cgen-string (value) (rope #\" value #\"))

(defstruct cgen-block
  header
  stmts)

(defmethod object-rope ((object cgen-block))
  (let ((body
         (rope #\{
               (with-cgen-indent
                 (rope (loop for s in (cgen-block-stmts object)
                          collect (rope *cgen-newline-indent* s))))
               *cgen-newline-indent*
               #\})))
    (if-let ((header (cgen-block-header object)))
      (rope *cgen-newline-indent* header " " *cgen-newline-indent* body)
      body)))

(defun cgen-defun (result name args body)
  (sycamore-cgen::make-cgen-block
   :header (rope result name (rope-parenthesize args))
   :stmts (flatten body)))

(defun cgen-block (&rest stmts)
  (make-cgen-block :stmts (flatten (ensure-list stmts))))

(defun cgen-while (test &rest body)
  (make-cgen-block :header (rope "while (" test ")")
                            :stmts (flatten (ensure-list body))))

(defun cgen-if (test &rest body)
  (make-cgen-block :header (rope "if (" test ")")
                            :stmts (flatten (ensure-list body))))

(defstruct cgen-binop
  op a b)

(defmethod object-rope ((object cgen-binop))
  (rope (rope-parenthesize (cgen-binop-a object))
        " " (cgen-binop-op object) " "
        (rope-parenthesize (cgen-binop-b object))))

(defun cgen-binop (op a b)
  (make-cgen-binop :op op :a a :b b))

(defun cgen-equal (a b)
  (cgen-binop :== a b))

(defun cgen-assign (a b)
  (cgen-binop := a b))

(defun cgen-include-local (thing)
    (rope "#include \"" thing "\"" #\Newline))

(defun cgen-include-system (thing)
  (rope "#include \<" thing "\>" #\Newline))

(defun cgen-define-constant (symbol &optional value)
  (rope "#define " symbol
        (if value
            (rope " " value)
            "")))

(defun cgen-comment (value)
  ;; TODO: escape
  (rope "/*" value "*/"))

(defun cgen-line-comment (value)
  (cgen-comment (rope " " value " ")))

(defun cgen-call-list (function args)
  (rope function "("
        (rope-split ", " args)
        ")"))

(defun cgen-call (function &rest args)
  (cgen-call-list function args))

(defun cgen-call-stmt (function &rest args)
  (cgen-stmt (cgen-call-list function args)))


(defun cgen-assign-stmt (a b)
  (cgen-stmt (cgen-assign a b)))

(defun cgen-declare (type name &optional initial-value)
  (cgen-stmt (rope type " " name
                   (if initial-value
                       (rope " = " initial-value)
                       ""))))

(defun cgen-array-initializer (values)
  (rope "{"
        (rope-split ", " values)
        "}"))

(defun cgen-double-float (value)
  (format nil "~Fd" value))

(defun cgen-single-float (value)
  (format nil "~Ff" value))

(defun cgen-declare-array (type name values-or-size)
  (cgen-stmt (rope type " " name
                   "["
                   (etypecase values-or-size
                     (number values-or-size)
                     (list (length values-or-size))
                     (array (length values-or-size)))

                   "]"
                   (when (or (listp values-or-size)
                             (arrayp values-or-size))
                     (rope " = " (cgen-array-initializer values-or-size))))))
