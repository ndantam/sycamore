(in-package :sycamore)

;; Single-bit hash trie test implementation.  Turns out HAMTs are
;; indeed much faster.

;; Entry: (cons fixnum key)
;; Layer: (cons (cons ff ff) (cons tt tt))
;; Bucket: (cons null fixnum k1 k2 ...)

(defun hct-set-insert (hct key hash-code test)
  ;;(declare (optimize (speed 3) (safety 0)))
  (declare (type non-negative-fixnum hash-code)
           (type function test))
  (labels ((rec (hct subhash)
             (declare (type fixnum subhash)
                      (type cons hct))
             (let ((a (car hct))
                   (d (cdr hct)))
               (etypecase a
                 ;; Layer
                 (cons
                  (if (not (logbitp 0 subhash))
                      (let ((r (rec a (ash subhash -1))))
                        (if (eq r a)
                            hct
                            (cons r d)))
                      (let ((r (rec d (ash subhash -1))))
                        (if (eq r d)
                            hct
                            (cons a r)))))
                 ;; Entry
                 (fixnum
                  (if (= hash-code a)
                      ;; same hash
                      (if (funcall test key d)
                          ;; same key
                          hct
                          ;; hash collision, make a bucket
                          (list nil hash-code key d))
                      ;; different hash
                      (let ((e (cons hash-code key)))
                        (if (not (logbitp 0 subhash))
                            (cons e hct)
                            (cons hct e)))))
                 ;; Bucket
                 (null
                  ;; same hash
                  (if (= hash-code (car d))
                      (if (member key (cdr d) :test test)
                          hct
                          (list* nil hash-code key (cdr d))))
                  ;; different hash
                  (let ((e (cons hash-code key)))
                    (if (not (logbitp 0 subhash))
                        (cons e hct)
                        (cons hct e))))))))
    (declare (dynamic-extent (function rec)))
    (etypecase hct
      (cons (rec hct hash-code))
      (null (cons hash-code key)))))

;; Higher order functions

(defun fold-right-hct (function hct initial-value)
  (labels ((rec (function thing initial-value)
             (etypecase (car thing)
               (cons
                (rec function (car thing)
                     (rec function (cdr thing) initial-value)))
               (fixnum
                (funcall function (cdr thing) initial-value))
               (null
                (dolist (x (cddr thing))
                  (setq initial-value (funcall function x initial-value)))))))
    (if hct
        (rec function hct initial-value)
        initial-value)))


;; Interface

(defstruct (hcmt-set (:constructor %make-hcmt-set (%functions root)))
  (%functions (cons nil nil) :type cons)
  root)

(defun make-hcmt-set (&key (test #'eql) (hash-function #'sxhash))
  (%make-hcmt-set (cons hash-function test) nil))

(defmacro %with-hcmt-set ((hash-function test) hash-set &body body)
  (with-gensyms (c)
    `(let ((,c (hcmt-set-%functions ,hash-set)))
       (let ((,hash-function (car ,c))
             (,test (cdr ,c)))
         ,@body))))

(defun hcmt-insert (set item)
  (%with-hcmt-set (hash-function test) set
    (%make-hcmt-set (hcmt-set-%functions set)
                    (hct-set-insert (hcmt-set-root set)
                                    item (funcall hash-function item) test))))



(defun list-hcmt-set (list &key (test #'eql) (hash-function #'sxhash))
  (reduce #'hcmt-insert
          list
          :initial-value (make-hcmt-set :test test :hash-function hash-function)))


(defun hcmt-set-list (set)
  (fold-right-hct #'cons (hcmt-set-root set) nil))






(defun time-hcmt ()
  (time-general (lambda (a) (list-hcmt-set a))
                :insert #'hcmt-insert
                ;; :find #'hash-set-find
                ;; :union #'hash-set-union
                ;; :intersection #'hash-set-intersection
                ;; :intersectionp #'hash-set-intersection-p
                ;; :subset #'hash-set-subset-p
                :name "SYCAMORE:HCMT"))
