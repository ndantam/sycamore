(in-package :sycamore)


(defstruct trie
  prefix
  suffixes
  present
  value
  )

(defun trie-value-node (prefix present value suffixes)
  (make-trie :prefix prefix
             :present present
             :value value
             :suffixes suffixes))


(defun make-trie-compare (compare-element)
  (lambda (trie-a trie-b)
    (labels ((get-element (trie)
               (etypecase trie
                 (list (car trie))
                 (trie (car (trie-prefix trie))))))
      (funcall compare-element (get-element trie-a) (get-element trie-b)))))

(defun prefix-split (a b &optional (test #'eql))
  "Split the common prefixes and disjoint suffixes in A and B."
  (declare (type function test)
           (type list a b))
  (let ((a-suffix nil)
        (b-suffix nil))
    (let ((new-prefix
           (loop
              for a-rest on a
              for b-rest on b
              for aa = (car a-rest)
              for bb = (car b-rest)
              for test-res = (funcall test aa bb)
              while test-res
              collect aa
              finally (if test-res
                          (setq a-suffix (cdr a-rest)
                                b-suffix (cdr b-rest))
                          (setq a-suffix a-rest
                                b-suffix b-rest)))))
      (values new-prefix a-suffix b-suffix))))

(defun trie-insert (trie key &key value compare (test #'eql))
  (labels ((rec-cons (trie key)
             (multiple-value-bind (new-prefix trie-suffix key-suffix)
                 (prefix-split trie key test)
               (trie-value-node new-prefix
                                (or (null trie-suffix) (null key-suffix))
                                nil
                                (cond ((null trie-suffix) key-suffix)
                                      ((null key-suffix) trie-suffix)
                                      (t (cond-compare ((car trie-suffix) (car key-suffix) compare)
                                                       (vector trie-suffix key-suffix)
                                                       (error "Invalid comparison result")
                                                       (vector key-suffix trie-suffix)))))))
           (rec-trie (trie key value)
             (multiple-value-bind (new-prefix trie-suffix key-suffix)
                 (prefix-split (trie-prefix trie) key test)
               (cond
                 ((null key-suffix)
                  ;; insert key into this node
                  (trie-value-node new-prefix
                                   t
                                   value
                                   (rec (trie-suffixes trie) trie-suffix (trie-value trie))))
                 ;; This node retains its key and value
                 ((null trie-suffix)
                  (trie-value-node new-prefix
                                   (trie-present trie)
                                   (trie-value trie)
                                   (rec (trie-suffixes trie) key-suffix value)))
                 ;; push this key and value down to suffixes
                 (t
                  (rec (rec (trie-suffixes trie) key-suffix (trie-value trie))
                       key
                       value)))))
           (rec-array (trie key value)
             (wb-tree-modify-vector trie key (make-trie-compare compare)
                                     (lambda (trie)
                                       (values (rec trie key value)
                                               t))
                                     nil))
           (rec-tree (trie key value)
             (wb-tree-modify trie value (make-trie-compare compare)
                              (lambda (trie)
                                (values (rec trie key value)
                                        t))))
           (rec (trie key value)
             (if key
                 (etypecase trie
                   (null (if value
                             (trie-value-node key t value nil)
                             key))
                   (cons
                    (assert (null value) () "Cannot insert value into list trie")
                    (rec-cons trie key))
                   (trie
                    (rec-trie trie key value))
                   (simple-vector
                    (rec-array trie key value))
                   (wb-tree
                    (rec-tree trie key value))))))

    ;; null key
    (rec trie key value)))
