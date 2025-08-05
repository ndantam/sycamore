;;;; -*- Lisp -*-
;;;;
;;;; Copyright (c) 2012, Georgia Tech Research Corporation
;;;; Copyright (c) 2025, Colorado School of Mines
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

(ql:quickload :fset)

(ql:quickload :cl-hamt)

(defparameter *bench-data-file-1*
  (make-pathname :directory '(:absolute "tmp") :name "sycamore-bench-1" :type "dat"))

(defparameter *bench-data-file-2*
  (make-pathname :directory '(:absolute "tmp") :name "sycamore-bench-2" :type "dat"))

(defun bench-generate-data (&key
                            (output-1 *bench-data-file-1*)
                            (output-2 *bench-data-file-2*)
                            (count-1 (expt 2 21))
                            (max-1 (* 2 count-1))
                            (count-2 count-1)
                            (max-2 max-1))
  (flet ((emit (count max output)
           (with-open-file (s output :direction :output :if-exists :supersede :if-does-not-exist :create)
             (format s "~{~&~D~}"
                     (loop for i below count
                        collect (random max))))))
    (emit count-1 max-1 output-1)
    (emit count-2 max-2 output-2)))

(defun bench-load (pathname)
  (with-open-file (s pathname :direction :input)
    (loop for i = (read s nil nil)
       while i
       collect i)))

(defun time-general (build
                     &key
                       (list-1 (bench-load *bench-data-file-1*))
                       (list-2 (bench-load *bench-data-file-2*))
                       insert remove union intersection intersectionp difference subset find
                       (output *standard-output*)
                       name)
  #+sbcl
  (sb-ext:gc :full t)
  (let ((*standard-output* output)
        (obj-1)
        (obj-2))
    (labels ((pre-test (test-name)
               (format output "~&~%: ~A: ~A  :" name test-name)
               #+sbcl
               (sb-ext:gc)))
    (if name
        (format t "~&: Benchmarks Results for ~A :" name)
        (format t "~&: Benchmarks Results :" ))
    ;; build
    (pre-test "build object 1")
    (setq obj-1 (time (funcall build list-1)))

    (pre-test "build object 2")
    (setq obj-2 (time (funcall build list-2)))

    ;; find
    (when find
      (pre-test "find 2 in 1")
      (time (loop for x in list-2
               do (funcall find obj-1 x)))

      (pre-test "find 1 in 1")
      (time (loop for x in list-1
               do (funcall find obj-1 x)))

      (pre-test "find 1 in 2")
      (time (loop for x in list-1
               do  (funcall find obj-2 x))))

    ;; insert
    (when insert
      (pre-test "insert 2 into 1")
      (time (loop for x in list-2
               for y =  (funcall insert obj-1 x) then
                 (funcall insert y x)))

      (pre-test "insert 1 into 2")
      (time (loop for x in list-1
               for y =  (funcall insert obj-2 x) then
                 (funcall insert y x))))
    ;; remove
    (when remove
      (pre-test "remove 2 from 1")
      (time (loop for x in list-2
               for y =  (funcall remove obj-1 x) then
                 (funcall insert y x)))

      (pre-test "remove 1 from 2")
      (time (loop for x in list-1
               for y =  (funcall remove obj-2 x) then
                 (funcall insert y x))))

    ;; union

    (when union
      (pre-test "union 1 2")
      (time (funcall union obj-1 obj-2))
      (pre-test "union 2 1")
      (time (funcall union obj-2 obj-1)))

    ;; intersection
    (when intersection
      (pre-test "intersection 1 2")
      (time (funcall intersection obj-1 obj-2))
      (pre-test "intersection 2 1")
      (time (funcall intersection obj-2 obj-1)))

    ;; intersectionp
    (when intersectionp
      (pre-test "intersectionp 1 2")
      (time (funcall intersectionp obj-1 obj-2))
      (pre-test "intersectionp 2 1")
      (time (funcall intersectionp obj-2 obj-1)))

    ;; subset
    (when subset
      (pre-test "subset 1 2")
      (time (funcall subset obj-1 obj-2))
      (pre-test "subset 2 1")
      (time (funcall subset obj-2 obj-1)))

    ;; difference
    (when difference
      (pre-test "difference 1 2")
      (time (funcall difference obj-1 obj-2))
      (pre-test "difference 2 1")
      (time (funcall difference obj-2 obj-1)))
    ))
  nil)



(defun time-wb ()
  ;; build
  (let ((compare (lambda (a b)
                   (declare (type fixnum a b))
                         (- a b))))
    (time-general (lambda (a) (build-wb-tree compare
                                              nil a))
                :insert (lambda (obj x)
                          (wb-tree-insert obj x compare))
                :remove (lambda (obj x)
                          (wb-tree-remove obj x compare))
                :find (lambda (obj x)
                          (binary-tree-find obj x compare))
                :union (lambda (x y)
                         (wb-tree-union x y compare))
                :intersection (lambda (x y)
                                (wb-tree-intersection x y compare))
                :intersectionp (lambda (x y)
                                (wb-tree-intersection-p x y compare))
                :difference (lambda (x y)
                              (wb-tree-difference x y compare))
                :subset (lambda (x y)
                              (wb-tree-subset x y compare))
                :name "SYCAMORE:WB")))


(defun time-fset ()
  (time-general (lambda (a) (fold #'fset:with (fset:empty-set) a))
                :insert (lambda (obj x)
                          (fset:with obj x))
                :find (lambda (obj x)
                          (fset:member? x obj))
                :remove (lambda (obj x)
                          (fset:less obj x))
                :union #'fset:union
                :intersection #'fset:intersection
                :subset #'fset:subset?
                :difference #'fset:set-difference-2
                :name "FSET"))


(defun time-hamt ()
  (time-general (lambda (a) (list-hash-set a))
                :insert #'hash-set-insert
                :find #'hash-set-find
                :union #'hash-set-union
                :intersection #'hash-set-intersection
                :intersectionp #'hash-set-intersection-p
                :difference #'hash-set-difference
                :subset #'hash-set-subset-p
                :name "SYCAMORE:HAMT"))

(defun time-cl-hamt ()
  (time-general (lambda (a) (reduce #'cl-hamt:set-insert  a
                                    :initial-value (cl-hamt:empty-set)))
                :insert (lambda (obj x) (cl-hamt:set-insert obj x))
                :find #'cl-hamt:set-lookup
                :name "CL-HAMT"))

(defun time-hash-table ()
  (time-general (lambda (a) (let ((hash (make-hash-table)))
                              (dolist (x a)
                                (setf (gethash x hash) t))
                              hash))
                ;:insert (lambda (obj x) (cl-hamt:set-insert obj x))
                :find (lambda (hash x)
                        (gethash x hash))
                :name "HASH-TABLE"))

(defun time-all (&key count (max count))
  (when (and count max)
    (bench-generate-data :count-1 count :max-1 max))
  (time-wb)
  (time-fset)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Benchmark Table Generation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-bytes-consed ()
  #+sbcl
  (sb-ext:get-bytes-consed)
  #-sbcl
  0)

(defun trial (thunk n iterations name op directory)
  (declare (type function thunk))
  #+sbcl
  (sb-ext:gc :full t)
  (let ((t0 (get-internal-run-time))
        (b0 (get-bytes-consed)))
    (prog1 (funcall thunk)
      (let* ((t1 (get-internal-run-time))
             (b1 (get-bytes-consed))
             (tt (/ (- t1 t0)
                    (* iterations internal-time-units-per-second)))
             (bb (/ (- b1 b0)
                    iterations)))
        (let ((directory (format nil "~A/~A/" directory op)))
          (ensure-directories-exist directory)
          (with-open-file (s (merge-pathnames (string name) directory)
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :append)
            (print (list :n n
                         :time (coerce tt 'double-float)
                         :bytes (coerce bb 'double-float))
                   s)))))))

(defparameter *plists*
  (list
   ;; HAMT
   (list :name 'sycamore-hamt
         :construct #'list-hash-set
         :insert #'hash-set-insert
         :remove #'hash-set-remove
         :find #'hash-set-find
         :union #'hash-set-union
         :intersection #'hash-set-intersection
         :difference #'hash-set-difference
         )
   ;; WB
   (let ((compare (lambda (a b)
                    (declare (type fixnum a b))
                    (- a b))))
     (list :name 'sycamore-wb-tree
           :construct (lambda (a) (fold #'tree-set-insert
                                        (make-tree-set compare)
                                        a))
           :insert #'tree-set-insert
           :remove #'tree-set-remove

           :find #'tree-set-find
           :union #'tree-set-union
           :intersection #'tree-set-intersection
           :difference #'tree-set-difference
           ))
   ;; ANSI HASH
   (list
    :name 'ANSI-HASH-TABLE
    :construct (lambda (a) (let ((hash (make-hash-table)))
                             (dolist (x a)
                               (setf (gethash x hash) t))
                             hash))
    :find (lambda (hash x)
            (gethash x hash)))
   ;; FSET
   (list :name 'fset
         :construct (lambda (a) (fold #'fset:with (fset:empty-set) a))
         :insert (lambda (obj x)
                   (fset:with obj x))
         :find (lambda (obj x)
                 (fset:member? x obj))
         :remove (lambda (obj x)
                   (fset:less obj x))
         :union #'fset:union
         :intersection #'fset:intersection
         :difference #'fset:set-difference-2)
   ;; CL-HAMT
   (list :name 'cl-hamt
         :construct (lambda (a) (fold #'cl-hamt:set-insert
                                      (cl-hamt:empty-set)
                                      a))
         :insert #'cl-hamt:set-insert
         :find #'cl-hamt:set-lookup
         :remove #'cl-hamt:set-remove
         )
   ))

(defmacro trial-thunk (iterations &body body)
  (with-gensyms (k itr)
    `(let ((,itr ,iterations))
       (if (= ,itr 1)
           (lambda () ,@body)
           (lambda ()
             (dotimes (,k (1- ,itr))
               ,@body)
             ,@body)))))

(defmacro when-thunk ((var symbol plist) &body body)
  `(let ((,var (getf ,plist ,symbol)))
     (format t "~&~A" ,symbol)
     (when ,var
       (let ((,var (the function ,var)))
         ,@body))))


(defun sample-set (k plist n iterations list-1 list-2 directory)
  (format t "~&IMPL: ~A, N=~D, s=~D" (getf plist :name) n k)
  (let* ((name (getf plist :name))
         (construct (the function (getf plist :construct)))
         (set-1 (trial (trial-thunk iterations
                         (funcall construct list-1))
                       n iterations name "construct" directory))
         (set-2 (trial (trial-thunk iterations
                         (funcall construct list-2))
                       n iterations name "construct" directory)))
    ;; Insert
    (when-thunk (f :insert plist)
      (trial (trial-thunk iterations
               (dolist (elt list-2)
                 (funcall f set-1 elt)))
             n (* n iterations) name "insert" directory)
      (trial (trial-thunk iterations
               (dolist (elt list-1)
                 (funcall f set-2 elt)))
             n (* n iterations) name "insert" directory))

    ;; Find
    (when-thunk (f :find plist)
      (trial (trial-thunk iterations
               (dolist (elt list-2)
                 (funcall f set-1 elt)))
             n (* n iterations) name "find" directory)
      (trial (trial-thunk iterations
               (dolist (elt list-1)
                 (funcall f set-2 elt)))
             n (* n iterations) name "find" directory))
    ;; Remove
    (when-thunk (f :remove plist)
      (trial (trial-thunk iterations
               (dolist (elt list-2)
                 (funcall f set-1 elt)))
             n (* n iterations) name "remove" directory)
      (trial (trial-thunk iterations
               (dolist (elt list-1)
                 (funcall f set-2 elt)))
             n (* n iterations) name "remove" directory))
    ;; Union
    (when-thunk (f :union plist)
      (trial (trial-thunk iterations
               (funcall f set-1 set-2))
             n iterations name "union" directory))

    ;; Intersection
    (when-thunk (f :intersection plist)
      (trial (trial-thunk iterations
               (funcall f set-1 set-2))
             n iterations name "intersection" directory))

    ;; Difference
    (when-thunk (f :difference plist)
      (trial (trial-thunk iterations
               (funcall f set-1 set-2))
             n iterations name "difference" directory))))

(defun bench-size (power)
  (let ((n (expt 2 power)))
    (cons n
          (max 1
               (floor (/ (expt 2 10) n))))))

(defparameter *bench-sizes*
  (loop for i from 5 upto 20
        collect (bench-size i)))

(defparameter *bench-samples* 30)

(defun gen-list (size)
  (let ((h (make-hash-table :size size))
        (max (* 4 size)))
    (loop until (= (hash-table-count h)
                   size)
          for x = (random max)
          do (setf (gethash x h) x))
    (hash-table-values h)))

(defun run-bench (&optional (directory "/tmp/sycamore-bench/"))
  ;; Check Directory
  (when (probe-file directory)
    (error "Directory `~A' already exists, refusing to overwrite." directory))
  (ensure-directories-exist directory)
  ;; Run tests
  (loop for (n . iterations) in *bench-sizes*
        do (loop for k below *bench-samples*
                 for list-1 = (gen-list n)
                 for list-2 = (gen-list n)
                 do (loop for plist in *plists*
                          do (sample-set k plist n iterations list-1 list-2 directory)))))

(defun bench-line (pathname)
  (let ((h-time (make-hash-table))
        (h-bytes (make-hash-table)))
    ;; Read data
    (with-open-file (s pathname :direction :input)
      (loop for line = (read s nil nil)
            while line
            do (destructuring-bind (&key n time bytes) line
                 (assert (and n time bytes))
                 (push time (gethash n h-time))
                 (push bytes (gethash n h-bytes)))))
    ;; States
    (loop for key in (sort (copy-list (hash-table-keys h-time)) #'<=)
          for time-list = (gethash key h-time)
          collect (mean time-list))))

(defun bench-header (&optional (stream t))
  (let ((list (cons "Implementation"
                    (sort (map 'list #'car *bench-sizes*) #'<=))))
    (format stream "~&~{| ~A ~}|~%" list)
    (dotimes (i (length list))
      (format stream "|----"))
    (format stream "|")))

(defun bench-section (heading &optional (stream t))
  (format stream "~&~A~%~A~%~%"
          heading
          (make-string (length heading) :initial-element #\=)))

(defun bench-subsection (heading &optional (stream t))
  (format stream "~&~A~%~A"
          heading
          (make-string (length heading) :initial-element #\-)))

(defun bench-line-0 (name line &optional (stream t))
  (format stream "~&| ~A |" name)
  (loop for mean in line
        for (factor . unit) = (cond ((< mean 1e-3)
                                     (cons 1e6 "us"))
                                    ((< mean 1)
                                     (cons 1e3 "ms"))
                                    (t (cons 1 "s")))
        do (format stream " ~,2F ~A |"
                   (* factor mean)
                   unit)))


(defun bench-line-rel (name line-0 line &optional (stream t))
  (format stream "~&| ~A |" name)
  (loop for mean-0 in line-0
        for mean in line
        do (format stream " ~,2Fx |"
                   (/ mean mean-0))))

(defun bench-table (directory &optional (stream t))
  (bench-section (string-upcase (car (last (pathname-directory directory))))
                 stream)
  (bench-header stream)
  (let* ((name-0 (string (getf (car *plists*) :name)))
         (line-0 (bench-line (concatenate 'string (namestring directory)
                                          name-0))))
    (bench-line-0 name-0 line-0 stream)
    (loop for pp in (cdr *plists*)
          for name = (string (getf pp :name))
          for pathname = (concatenate 'string (namestring directory) name)
          when (probe-file pathname)
            do (bench-line-rel name line-0 (bench-line pathname)
                               stream))))


(defun bench-cpu ()
  #-sbcl
  "Unknown"
  #+sbcl
  (with-output-to-string (s)
    (sb-ext:run-program
     "/bin/sh"
     '("-c"
       "cat /proc/cpuinfo | grep 'model name' | head -n 1 | sed -e 's/model name\\s*:\\s*//'")
     :output s)))
(defun bench-uname ()
  #-sbcl
  "Unknown"
  #+sbcl
  (with-output-to-string (s)
    (sb-ext:run-program
     "/bin/sh"
     '("-c"
       "uname -a"
       )
     :output s)))

(defun bench-text (&optional (directory "/tmp/sycamore-bench/") (stream t))

  (bench-section "Sycamore Persistent Set Benchmarks" stream)
  (format stream "~&This document contains timing benchmarks of the persistent set implementations in Sycamore, FSET, and CL-HAMT, along with a baseline of (mutable) ANSI CL Hash-Tables. The performance ranking is as follows and is generally consistent across the tested operations:~%~%")
  (format stream "~&1. ANSI CL Hash-Tables (fastest construction and find, but mutable only)")
  (format stream "~&2. Sycamore HAMTs (fastest persistent set)")
  (format stream "~&3. Sycamore WB-Trees")
  (format stream "~&4. FSet")
  (format stream "~&5. CL-HAMT")

  (format stream "~&~%")
  (bench-section "Test Setup" stream)

  (bench-subsection "Data" stream)
  (format stream "~&- Operations performed for random sets of the stated sizes")
  (format stream "~&- ~D samples (random sets) per size" *bench-samples*)
  (format stream "~&- Results for ~A are shown as mean run time"
          (getf (car *plists*) :name))
  (format stream "~&- Results for others are shown as relative run time (/ TIME-OTHER TIME-~A), equivalent to the speedup of ~A over OTHER"
          (getf (car *plists*) :name) (getf (car *plists*) :name))

  (format stream "~&~%")
  (bench-subsection "System" stream)
  (multiple-value-bind
        (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore second day-of-week dst-p tz))
    (format stream "~&- Date: ~D-~2,'0D-~2,'0D, ~2,'0D:~2,'0D"
            year month day minute hour))
  (format stream "~&- Uname: ~A" (bench-uname))
  (format stream "~&- CPU: ~A" (bench-cpu))
  (format stream "~&- Lisp Implementation: ~A ~A"
          (lisp-implementation-type) (lisp-implementation-version))


  (loop for op in '(construct insert find remove union intersection difference)
        for subdir = (concatenate 'string (namestring directory)
                                  (string-downcase (string op))
                                  "/")
        do (format stream "~%~%")
           (bench-table subdir stream)))

(defun bench-file (filespec &optional (directory "/tmp/sycamore-bench/"))
  (with-open-file (stream filespec
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (bench-text directory stream)))
