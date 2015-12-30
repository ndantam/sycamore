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
   :header (rope result #\Space name (rope-parenthesize args))
   :stmts (flatten body)))

(defun cgen-block (&rest stmts)
  (make-cgen-block :stmts (flatten (ensure-list stmts))))

(defun cgen-while (test &rest body)
  (make-cgen-block :header (rope "while (" test ")")
                            :stmts (flatten (ensure-list body))))

(defun cgen-if (test &rest body)
  (make-cgen-block :header (rope "if (" test ")")
                            :stmts (flatten (ensure-list body))))

(defmacro def-cgen-binop (symbol)
  (with-gensyms (a b)
    `(defun ,(intern (concatenate 'string "CGEN-" (string symbol)))
         (,a ,b)
       (cgen-binop ',symbol ,a ,b))))

(def-cgen-binop *)
(def-cgen-binop /)
(def-cgen-binop +)
(def-cgen-binop -)

(defun cgen-sizeof (arg)
  (cgen-call "sizeof" arg))

(let ((hash (alist-hash-table
             '((post-++ . 1)
               (post--- . 1)
               (|()| . 1)
               ([] . 1)
               (\. . 1)
               (-> . 1)

               (pre-++  . 2)
               (pre---  . 2)
               (!  . 2)
               (~  . 2)
               (unary-+  . 2)
               (unary--  . 2)
               (cast  . 2)
               (deref . 2)
               (addr  . 2)
               (align  . 2)

               (* . 3)
               (/ . 3)
               (% . 3)

               (+ . 4)
               (- . 4)

               (<< . 5)
               (>> . 5)

               (< . 6)
               (> . 6)
               (<= . 6)
               (>= . 6)

               (== . 7)
               (!= . 7)

               (&    .  8)
               (^    .  9)
               (\|   . 10)
               (&&   . 11)
               (\|\| . 12)

               (|?:| . 12)

               (= . 14)
               (+= . 14)
               (-= . 14)
               (*= . 14)
               (/= . 14)
               (%= . 14)
               (<<= . 14)
               (>>= . 14)
               (&= . 14)
               (^= . 14)
               (\|= . 14)

               (|,| . 15)

               ))))
  (defun op-precedence (op)
    (if-let ((p (gethash op hash)))
      p
      (error "Unrecognized operator '~A'" op))))

(defun precedence (e)
  (etypecase e
    (cgen-op (op-precedence (cgen-op-op e)))
    (number 0)
    (rope 0)))

(defun cgen-op-symbol (op)
  "Return the canonical symbol of op"
  (declare (type symbol op))
  (let ((op (case op
              ((or :or :lor) '\|\|)
              ((and :and :land) '&&)
              ((:bor) '\|)
              ((:band) '&)
              ((:= =) '=)
              ((:->) '->)
              ((:.) '|.|)
              ((:[]) '[])
              (otherwise op))))
    (assert (op-precedence op))
    op))


;; TODO: sanity check that operators are valid for the pre/post unary/binary type

(defstruct cgen-op
    op)

(defun cgen-op-rope (op)
  "Return the C rope of op"
  (cgen-op-symbol (cgen-op-op op)))

(defstruct (cgen-unop-pre (:include cgen-op)
                          (:constructor %cgen-unop-pre (op a)))
  a)

(defun cgen-unop-pre (op a)
  (%cgen-unop-pre (cgen-op-symbol op)
                  a))

(defstruct (cgen-unop-post (:include cgen-op)
                           (:constructor %cgen-unop-post (a op)))
  a)

(defun cgen-unop-post (a op)
  (%cgen-unop-post a
                   (cgen-op-symbol op)))

(defstruct (cgen-binop (:include cgen-op)
                       (:constructor %cgen-binop (op a b)))
  a b)

(defun cgen-binop (op a b)
  (%cgen-binop (cgen-op-symbol op)
               a b))


(defun cgen-parenthesize (parent child)
  ;(format t "~&parent: ~A:~D" parent (precedence parent))
  ;(format t "~&child: ~A:~D" child (precedence child))
  (if (> (precedence child) (precedence parent))
      (rope-parenthesize child)
      (rope child)))

(defmethod object-rope ((object cgen-unop-pre))
  ;; TODO: avoid redundant parenthesis
  (rope (cgen-op-rope object)
        (cgen-parenthesize object
                           (cgen-unop-pre-a object))))

(defmethod object-rope ((object cgen-unop-post))
  ;; TODO: avoid redundant parenthesis
  (rope (cgen-parenthesize object (cgen-unop-post-a object))
        (cgen-op-rope object)))




(defmethod object-rope ((object cgen-binop))
  ;; TODO: avoid redundant parenthesis
  (rope (cgen-parenthesize object (cgen-binop-a object))
        " " (cgen-op-rope object) " "
        (cgen-parenthesize object (cgen-binop-b object))))


(defun cgen-equal (a b)
  (cgen-binop '== a b))

(defun cgen-assign (a b)
  (cgen-binop '= a b))


(defstruct (cgen-subscript (:include cgen-binop)))
(defun cgen-subscript (array index)
  (make-cgen-subscript :op '[] :a array :b index))

(defmethod object-rope ((object cgen-subscript))
  (rope (cgen-subscript-a object)
        #\[ (cgen-subscript-b object) #\]))

;; (defun cgen-op (op args)
;;   (print (cons op args))


(defun cgen-identifier (rope &key case)
  (let ((string (rope-string rope)))
    (setq string (substitute #\_ #\- string))
    (case case
      (:upper (string-upcase string))
      (:lower (string-downcase string))
      (otherwise string))))




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


(defun cgen-exp (e)
  (labels ((rec-0 (nullary unary op args)
             (if (null args)
                 nullary
                 (rec-1 unary op (car args) (cdr args))))
           (rec-1 (unary op a args)
             (if (null args)
                 unary
                 (rec-2 op a (car args) (cdr args))))
           (rec-2 (op a b args)
             ;; right associative
             (let ((child (cgen-binop op (rec-e a) (rec-e b))))
               (if args
                   (rec-op op (cons child args))
                   child)))
           (rec-op (op args)
             (ecase (cgen-op-symbol op)
               (! (assert (null (cdr args)))
                  (cgen-unop-pre op (car args)))
               (+ (rec-0 0 (car args) op args))
               (* (rec-0 1 (car args) op args))
               (&& (rec-0 1 (car args) op args))
               (& (rec-0 1 (car args) op args))
               (\|\| (rec-0 0 (car args) op args))
               (\| (rec-0 0 (car args) op args))
               (== (print args)
                   (destructuring-bind (a b &rest args) args
                     (rec-2 op a b args)))
               ([]
                (assert (= 2 (length args)))
                (cgen-subscript (rec-e (first args))
                                (rec-e (second args))))
               ((= -> |.|)
                (assert (= 2 (length args)))
                (cgen-binop op
                            (rec-e (first args))
                            (rec-e (second args))))))
           (rec-e (e)
             (if (listp e)
                 (rec-op (car e) (cdr e))
                 e)))
    (rec-e e)))
