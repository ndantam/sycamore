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

(in-package :motion-grammar)


(defmacro assert-finite-set-equal (a b)
  `(lisp-unit:assert-true (finite-set-equal ,a ,b)))

(lisp-unit:define-test dfa-equal-basic
  (lisp-unit:assert-true (dfa-equal (make-fa '((0 a 1) (1 b 0)) 0 (finite-set 1))
                                    (make-fa '((x a y) (y b x)) 'x (finite-set 'y))))
  (lisp-unit:assert-false (dfa-equal (make-fa '((0 a 1) (1 b 0)) 0 (finite-set 1))
                                     (make-fa '((x a y) (y b x)) 'y (finite-set 'x)))))



(lisp-unit:define-test dfa-minimize
  (lisp-unit:assert-true
   (dfa-equal (fa-minimize-brzozowski (make-fa '((0 a 1) (1 b 2) (2 a 1)) 0 (finite-set 1)))
              (make-fa '((0 a 1) (1 b 0)) 0 (finite-set 1))))
  (flet ((min-cmp (dfa)
           (dfa-equal (fa-minimize-brzozowski dfa)
                      (fa-minimize-hopcroft dfa))))
    (lisp-unit:assert-true
     (min-cmp (make-fa '((0 a 1) (1 b 2) (2 a 1)) 0 (finite-set 1))))

    (lisp-unit:assert-true
     (make-fa '((0 a 1) (1 b 0)
                (0 e 2) (1 e 2))
              0 (finite-set 2)))
    (lisp-unit:assert-true
     (min-cmp (make-fa '((0 a 1) (1 b 0) (1 c 2) (2 a 1)
                         (0 e 3) (1 e 3) (2 e 3))
                       0 (finite-set 3))
              ))))

;; examples from the dragon book
(lisp-unit:define-test fa-dragon
  (let* ((fig-3-56  '(:concatenation (:closure (:union a b)) a b b))
         (fig-3-63 (make-fa '((0 b 0) (0 a 1) (1 a 1) (1 b 2) (2 a 1) (2 b 3)
                              (3 a 1) (3 b 0))
                            0 (finite-set 3)))
         (my-fig-3-36 (regex->nfa fig-3-56))
         (my-fig-3-63 (nfa->dfa my-fig-3-36)))

    (lisp-unit:assert-true (dfa-equal fig-3-63
                                      (fa-minimize-hopcroft my-fig-3-63)))
    (lisp-unit:assert-true (dfa-equal fig-3-63
                                      (fa-minimize-hopcroft my-fig-3-63)))))

;; examples from Hopcroft '79
(lisp-unit:define-test fa-hopcroft-79
  (let ((fig-2-15-a (make-fa '((q7 :epsilon q5) (q7 :epsilon q8)
                               (q5 1 q6)
                               (q6 :epsilon q5) (q6 :epsilon q8))
                             'q7 (finite-set 'q8)))
        (fig-2-15-b (make-fa '((q3 0 q4)
                               (q4 :epsilon q7)
                               (q7 :epsilon q5) (q7 :epsilon q8)
                               (q5 1 q6)
                               (q6 :epsilon q5) (q6 :epsilon q8))
                             'q3 (finite-set 'q8)))
        (fig-2-15-c (make-fa '((q9 :epsilon q1) (q9 :epsilon q3)
                               (q1 1 q2)
                               (q2 :epsilon q10)
                               (q3 0 q4)
                               (q4 :epsilon q7)
                               (q7 :epsilon q8) (q7 :epsilon q5)
                               (q5 1 q6)
                               (q6 :epsilon q5) (q6 :epsilon q8)
                               (q8 :epsilon q10))
                             'q9 (finite-set 'q10)))
        (regex-2-15-a '(:closure 1))
        (regex-2-15-b '(:concatenation 0 (:closure 1)))
        (regex-2-15-c '(:union (:concatenation 0 (:closure 1)) 1)))
    (lisp-unit:assert-true
     (dfa-equal (fa-minimize-hopcroft fig-2-15-a)
                (fa-minimize-hopcroft (regex->nfa regex-2-15-a))))
    (lisp-unit:assert-true
     (dfa-equal (fa-minimize-brzozowski fig-2-15-a)
                (fa-minimize-hopcroft (regex->nfa regex-2-15-a))))

    (lisp-unit:assert-true
     (dfa-equal (fa-minimize-hopcroft fig-2-15-b)
                (fa-minimize-hopcroft (regex->nfa regex-2-15-b))))
    (lisp-unit:assert-true
     (dfa-equal (fa-minimize-brzozowski fig-2-15-b)
                (fa-minimize-hopcroft (regex->nfa regex-2-15-b))))

    (lisp-unit:assert-true
     (dfa-equal (fa-minimize-hopcroft fig-2-15-c)
                (fa-minimize-hopcroft (regex->nfa regex-2-15-c))))
    (lisp-unit:assert-true
     (dfa-equal (fa-minimize-brzozowski fig-2-15-c)
                (fa-minimize-hopcroft (regex->nfa regex-2-15-c)))))



  (let ((ex-3-7 '(:concatenation (:closure 0) 1 (:closure 0)))
        (fig-3-2 (make-fa '((a 0 b) (a 1 c)
                            (b 0 a) (b 1 d)
                            (c 0 e) (c 1 f)
                            (d 0 e) (d 1 f)
                            (e 0 e) (e 1 f)
                            (f 0 f) (f 1 f))
                          'a '(c d e)))
        (fig-3-4 (make-fa '((e 0 e) (e 1 1)
                            (1 0 1) (1 1 11)
                            (11 0 11) (11 1 11))
                          'e (finite-set 1)))
        (fig-3-4-min (make-fa '((e 0 e) (e 1 1)
                                (1 0 1))
                              'e (finite-set 1))))
    ;; mostly-minimal
    (lisp-unit:assert-true (dfa-equal fig-3-4-min
                                      (fa-minimize-hopcroft fig-3-4)))
    (lisp-unit:assert-true (dfa-equal fig-3-4-min
                                      (fa-minimize-brzozowski fig-3-4)))

    ;; bigger dfa
    (lisp-unit:assert-true (dfa-equal fig-3-4-min
                                      (fa-minimize-hopcroft fig-3-2)))
    (lisp-unit:assert-true (dfa-equal fig-3-4-min
                                      (fa-minimize-brzozowski fig-3-2)))
                                        ; regex
    (lisp-unit:assert-true
     (dfa-equal fig-3-4-min
                (fa-minimize-hopcroft (regex->nfa ex-3-7))))
    (lisp-unit:assert-true
     (dfa-equal fig-3-4-min
                (fa-minimize-brzozowski (nfa->dfa (regex->nfa ex-3-7))))))

  (let ((fig-3-5 (make-fa '((a 0 b) (a 1 f)
                            (b 0 g) (b 1 c)
                            (c 0 a) (c 1 c)
                            (d 0 c) (d 1 g)
                            (e 0 h) (e 1 f)
                            (f 0 c) (f 1 g)
                            (g 0 g) (g 1 e)
                            (h 0 g) (h 1 c))
                          'a (finite-set 'c)))
        (fig-3-7 (make-fa '((a-e 0 b-h) (a-e 1 d-f)
                            (b-h 0 g)   (b-h 1 c)
                            (g 0 g)     (g 1 a-e)
                            (c 0 a-e)   (c 1 c)
                            (d-f 0 c)   (d-f 1 g))
                          'a-e (finite-set 'c))))
    (lisp-unit:assert-true
     (dfa-equal fig-3-7
                (fa-minimize-hopcroft fig-3-5)))

    (lisp-unit:assert-true
     (dfa-equal fig-3-7
                (fa-minimize-brzozowski fig-3-5))))
  t)



;; examples from Sipser
(lisp-unit:define-test fa-sipser
  (let ((fig-1-42 (make-fa '((1 :epsilon 3) (1 b 2)
                             (2 a 2) (2 a 3) (2 b 3)
                             (3 a 1))
                           1 (finite-set 1)))
        (fig-1-44 (make-fa '((2 a 23) (2 b 3)
                             (3 a 13)
                             (13 a 13) (13 b 2)
                             (23 b 3) (23 a 123)
                             (123 a 123) (123 b 23))
                           13 (list 13 123))))
    (lisp-unit:assert-true (dfa-equal fig-1-44
                                      (fa-minimize-hopcroft fig-1-44)))
    (lisp-unit:assert-true (dfa-equal fig-1-44
                                      (fa-minimize-brzozowski fig-1-44)))
    (lisp-unit:assert-true (dfa-equal (fa-minimize-hopcroft fig-1-42)
                                      fig-1-44))))


(lisp-unit:define-test regex-dfa-matcher
  (labels ((equiv-hop-brz (regex)
             (let* ((nfa (regex->nfa regex))
                    (dfa (nfa->dfa nfa))
                    (hop (fa-minimize-hopcroft dfa)))
               (and (dfa-equal (fa-minimize-brzozowski dfa) hop)
                    (dfa-equal (fa-minimize-brzozowski nfa) hop))))
           (match-dfa (regex string)
             (funcall (chain regex
                             #'regex->nfa
                             #'nfa->dfa
                             #'dfa->string-matcher)
                      string))
           (match-hop (regex string)
             (funcall (chain regex
                             #'regex->nfa
                             #'fa-minimize-hopcroft
                             #'dfa->string-matcher)
                      string))
           (match-brz (regex string)
             (funcall (chain regex
                             #'regex->nfa
                             #'fa-minimize-brzozowski
                             #'dfa->string-matcher)
                      string)))
    (macrolet ((test (regex positive negative)
                 `(progn
                    (lisp-unit:assert-true (equiv-hop-brz ',regex))
                    ,@(mapcan
                       (lambda (p)
                         `((lisp-unit:assert-true (match-dfa ',regex ,p))
                           (lisp-unit:assert-true (match-hop  ',regex,p))
                           (lisp-unit:assert-true (match-brz  ',regex,p))))
                       positive)
                    ,@(mapcan
                       (lambda (n)
                         `((lisp-unit:assert-false (match-dfa ',regex ,n))
                           (lisp-unit:assert-false (match-hop ',regex ,n))
                           (lisp-unit:assert-false (match-brz ',regex ,n))))
                       negative))))
      (test (:closure #\a)
            ("" "a" "aa" "aaa" "aaa")
            ("b" "ba" "aab" "baaa" "aba"))
      (test (:concatenation (:closure #\a) #\b)
            ("b" "ab" "aab" "aaab" "aaaaaab")
            ("" "a" "aba" "aaa" "baaa"))
      ;; Sipser p. 65, some "interesting" regexes
      (test (:concatenation (:closure #\a) #\b (:closure #\a))
            ("b" "ab" "aab" "baa" "aba" "aaba" "aaaaaab")
            ("" "a" "abba" "baaab" "bbaaa"))
      (test (:concatenation (:closure (:union #\a #\b #\c))
                            #\a #\a #\b
                            (:closure (:union #\a #\b #\c)))
            ("aab" "aaab" "aaba" "aabaa" "aabaaba")
            ("" "a" "abba" "baaa" "bbaaa"))
      (test (:closure (:concatenation (:union #\a #\b)
                                      (:union #\a #\b)))
            ("" "aa" "bb" "ab" "ba" "aaaa" "aaab" "aaba" "bbaa")
            ("a" "a" "b" "aaa" "aba" "baa" "bbb"))
      (test (:closure (:concatenation (:union #\a #\b)
                                      (:union #\a #\b)
                                      (:union #\a #\b)))
            ("" "aaa" "bbb" "aab" "aba" "aaaaaa" "bbbbbb" "aaabbb")
            ("a" "a" "b" "aa" "ab" "ba" "babb" "bbaaa"))
      (test (:union (:concatenation #\a (:closure (:union #\a #\b)) #\a)
                    (:concatenation #\b (:closure (:union #\a #\b)) #\b)
                    #\a #\b)
            ("a" "b" "aa" "bb" "aba" "bab" "abaa" "bababab")
            ("ab" "ba" "abb" "bba" "ababab"))
      t)))



(lisp-unit:define-test fa-convert
  (let ((state-fa (make-fa '((eat hungry sleep)
                             (eat thirsty drink)
                             (sleep hungry eat)
                             (sleep thirsty drink)
                             (drink hungry eat)
                             (drink tired sleep))
                           'sleep '(sleep)))
        (edge-fa (make-fa '((eat-1 hungry sleep-0)
                            (eat-1 thirsty drink-0)
                            (sleep-1 hungry eat-0)
                            (sleep-1 thirsty drink-0)
                            (drink-1 hungry eat-0)
                            (drink-1 tired sleep-0)
                            (eat-0 eat eat-1)
                            (sleep-0 sleep sleep-1)
                            (drink-0 drink drink-1))
                          'sleep-0 '(sleep-1))))
    (lisp-unit:assert-true (fa-equiv (fa-minimize-brzozowski (fa-state->edge state-fa))
                                     (fa-minimize-hopcroft edge-fa)))))


(lisp-unit:define-test grammar-basic
  (let ((g '((a b c) (b e f))))
    ;; map
    (lisp-unit:assert-true
     (equal (grammar-map 'list (lambda (l r) (list :a l r))
                         g)
            '((:a a (b c))
              (:a b (e f)))))
    ;; nonterminals
    (let ((n-r '(a b))
          (n (grammar-nonterminals g)))
      (lisp-unit:assert-true
       (finite-set-equal n n-r)))

    ;; substitute list
    (lisp-unit:assert-true
     '((a k b c) (c k b f))
     (grammar-substitute-terminal-list '((a b c) (c b f))
                                       'b '(k b)))

    ;; remove non-sentential
    (lisp-unit:assert-true
     (equal (grammar-remove-nonsentential '((s a b) (s 1) (a 1)) '(1))
            '((s 1)
              (a 1))))

    ;; Remove unreachables
    (lisp-unit:assert-true
     (equal (grammar-remove-unreachable '((a b) (b 1) (b 3) (c 2)))
            '((a b) (b 1) (b 3))))

    (lisp-unit:assert-true
     (equal (grammar-remove-unreachable '((a 1 c) (b 1) (b 3) (c 2)))
            '((a 1 c) (c 2))))

    ;; Remove useless
    (lisp-unit:assert-true
     (equal (grammar-remove-useless '((s a b) (s 1) (a 1)) '(1))
            '((s 1))))

    ;; first/follow function
    ;; Dragon p222
    (let* ((grammar '((E T E-p)
                      (E-p + T E-p)
                      (E-p :epsilon)
                      (T F T-p)
                      (T-p * F T-p)
                      (T-p :epsilon)
                      (F |(| E |)|)
                      (F id)))
           (first (grammar-first-function grammar))
           (follow (grammar-follow-function grammar))
           (table (make-predictive-table grammar)))
      ;; first
      (assert-finite-set-equal (funcall first '+)
                               (finite-set '+))
      (assert-finite-set-equal (funcall first '*)
                               (finite-set '*))
      (assert-finite-set-equal (funcall first '|)|)
                               (finite-set '|)|))
      (assert-finite-set-equal (funcall first '|(|)
                               (finite-set '|(|))
      (assert-finite-set-equal (funcall first 'id)
                               (finite-set 'id))

      (assert-finite-set-equal (funcall first 'F)
                               (finite-set '|(| 'id))
      (assert-finite-set-equal (funcall first 'T)
                               (finite-set '|(| 'id))
      (assert-finite-set-equal (funcall first 'E)
                               (finite-set '|(| 'id))

      (assert-finite-set-equal (funcall first 'E-p)
                               (finite-set '+ :epsilon))
      (assert-finite-set-equal (funcall first 'T-p)
                               (finite-set '* :epsilon))
      ;; follow
      (assert-finite-set-equal (funcall follow 'E)
                               (finite-set '|)| :$))
      (assert-finite-set-equal (funcall follow 'E-p)
                               (finite-set '|)| :$))

      (assert-finite-set-equal (funcall follow 'T)
                               (finite-set '+ '|)| :$))
      (assert-finite-set-equal (funcall follow 'T-p)
                               (finite-set '+ '|)| :$))

      (assert-finite-set-equal (funcall follow 'F)
                               (finite-set '+ '* '|)| :$))
      ;; predictive table
      (lisp-unit:assert-equal '(E T E-p) (funcall table 'E 'id))
      (lisp-unit:assert-equal nil (funcall table 'E-p 'id))
      (lisp-unit:assert-equal '(T F T-p) (funcall table 'T 'id))
      (lisp-unit:assert-equal nil (funcall table 'T-p 'id))
      (lisp-unit:assert-equal '(F id) (funcall table 'F 'id))

      (lisp-unit:assert-equal nil (funcall table 'E '+))
      (lisp-unit:assert-equal '(E-p + T E-p) (funcall table 'E-p '+))
      (lisp-unit:assert-equal nil (funcall table 'T '+))
      (lisp-unit:assert-equal '(T-p :epsilon) (funcall table 'T-p '+))
      (lisp-unit:assert-equal nil (funcall table 'F '+))

      (lisp-unit:assert-equal nil (funcall table 'E '*))
      (lisp-unit:assert-equal nil (funcall table 'E-p '*))
      (lisp-unit:assert-equal nil (funcall table 'T '*))
      (lisp-unit:assert-equal '(T-p * F T-p) (funcall table 'T-p '*))
      (lisp-unit:assert-equal nil (funcall table 'F '*))

      (lisp-unit:assert-equal nil (funcall table 'E '*))
      (lisp-unit:assert-equal nil (funcall table 'E-p '*))
      (lisp-unit:assert-equal nil (funcall table 'T '*))
      (lisp-unit:assert-equal '(T-p * F T-p) (funcall table 'T-p '*))
      (lisp-unit:assert-equal nil (funcall table 'F '*))

      (lisp-unit:assert-equal '(E T E-p) (funcall table 'E '|(|))
      (lisp-unit:assert-equal nil (funcall table 'E-p '|(|))
      (lisp-unit:assert-equal '(T F T-p) (funcall table 'T '|(|))
      (lisp-unit:assert-equal nil (funcall table 'E-p '|(|))
      (lisp-unit:assert-equal '(F |(| E |)|) (funcall table 'F '|(|))

      (lisp-unit:assert-equal nil (funcall table 'E '|)|))
      (lisp-unit:assert-equal '(E-p :epsilon) (funcall table 'E-p '|)|))
      (lisp-unit:assert-equal nil (funcall table 'T '|)|))
      (lisp-unit:assert-equal '(T-p :epsilon) (funcall table 'T-p '|)|))
      (lisp-unit:assert-equal nil (funcall table 'F '|)|))

      (lisp-unit:assert-equal nil (funcall table 'E :$))
      (lisp-unit:assert-equal '(E-p :epsilon) (funcall table 'E-p :$))
      (lisp-unit:assert-equal nil (funcall table 'T :$))
      (lisp-unit:assert-equal '(T-p :epsilon) (funcall table 'T-p :$))
      (lisp-unit:assert-equal nil (funcall table 'F :$)))

    ;; predictive table
    ;; Dragon p225, example 4.33
    (let* ((grammar  '((S |i| E |t| S S-p)
                       (S |a|)
                       (S-p |e| S)
                       (S-p :epsilon)
                       (E |b|)))
           (table (make-predictive-table grammar :duplicate-error-p nil)))
      (assert-finite-set-equal (finite-set '(S |a|))
                               (funcall table 's '|a|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 's-p '|a|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e '|a|))

      (assert-finite-set-equal (finite-set)
                               (funcall table 's '|b|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 's-p '|b|))
      (assert-finite-set-equal (finite-set '(E |b|))
                               (funcall table 'e '|b|))

      (assert-finite-set-equal (finite-set)
                               (funcall table 's '|e|))
      (assert-finite-set-equal (finite-set '(S-p :epsilon) '(S-p |e| S))
                               (funcall table 's-p '|e|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e '|e|))

      (assert-finite-set-equal (finite-set '(S |i| E |t| S S-p))
                               (funcall table 's '|i|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 's-p '|i|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e '|i|))

      (assert-finite-set-equal (finite-set)
                               (funcall table 's '|t|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 's-p '|t|))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e '|t|))

      (assert-finite-set-equal (finite-set)
                               (funcall table 's :$))
      (assert-finite-set-equal (finite-set '(S-p :epsilon))
                               (funcall table 's-p :$))
      (assert-finite-set-equal (finite-set)
                               (funcall table 'e :$)))


    ;; chain rule
    (lisp-unit:assert-true (grammar-chain-rule-p '(1 2 3) '(a b c) '(a b)))
    (lisp-unit:assert-true (grammar-chain-rule-p '(1 2 3) '(a b c) '(c a)))
    (lisp-unit:assert-false (grammar-chain-rule-p '(1 2 3) '(a b c) '(a 1)))
    (lisp-unit:assert-false (grammar-chain-rule-p '(1 2 3) '(a b c) '(a)))
    (lisp-unit:assert-false (grammar-chain-rule-p '(1 2 3) '(a b c) '(a b c)))
    (lisp-unit:assert-false (grammar-chain-rule-p '(1 2 3) '(a b c) '(a 1 c)))))

(lisp-unit:define-test grammar-regular
  (let ((fa (make-fa '((0 x 1) (1 y 0) (1 z 2)) 0 (finite-set 2)))
        (gram-1 '((a x b) (b y a)  (b z)))
        (gram-2 '((a x b) (b y a) (b y x b) (b z))))
    (lisp-unit:assert-true
     (fa-equiv fa (grammar->fa  gram-1)))
    (lisp-unit:assert-true
     (fa-equiv fa (nfa->dfa (grammar->fa gram-2))))))


(lisp-unit:define-test grammar-norm
  (let ((sipser-2-10 '((s a s a) (s x b) (a b) (a s) (b y) (b)))
        (no-epsilon '((s a s a) (s x b) (s x) (s s a) (s a s) (s s) (a b) (a s) (b y)))
        (no-epsilon-unit '((s a s a) (s x b) (s x) (s s a) (s a s)
                           (a y) (a a s a) (a x b) (a x) (a s a) (a a s)
                           (b y))))
    (lisp-unit:assert-true
     (finite-set-equal no-epsilon
                       (grammar-remove-epsilon sipser-2-10)))
    (lisp-unit:assert-true
     (finite-set-equal no-epsilon-unit
                       (grammar-remove-unit (grammar-remove-epsilon sipser-2-10)))))
  ;; Blum-Koch
  (lisp-unit:assert-true
   (finite-set-equal (blum-koch-subgrammar-productions 'b '((b a 2) (a 1)))
                     (finite-set (list (gsymbol-gen 'b 'start) 1 (gsymbol-gen 'a 'b))
                                 (list (gsymbol-gen 'a 'b) 2))))

  (lisp-unit:assert-true
   (finite-set-equal (blum-koch-subgrammar-productions  'b
                                                        '((b d1 a1)
                                                          (d1 d2 a2)
                                                          (d2 d3 a3)
                                                          (d3 d4 a4)
                                                          (d4 a g)))
                     (finite-set (list (gsymbol-gen 'b 'start) 'a 'g (gsymbol-gen 'd4 'b))
                                 (list (gsymbol-gen 'd4 'b) 'a4 (gsymbol-gen 'd3 'b))
                                 (list (gsymbol-gen 'd3 'b) 'a3 (gsymbol-gen 'd2 'b))
                                 (list (gsymbol-gen 'd2 'b) 'a2 (gsymbol-gen 'd1 'b))
                                 (list (gsymbol-gen 'd1 'b) 'a1))))


  )


(lisp-unit:define-test tree

  ;; equal
  (let ((a (binary-tree-from-list '(2 (1) (3)))))
    (lisp-unit:assert-true (binary-tree-equal a
                                              (binary-tree-from-list '(1 nil (3 (2))))
                                              #'-))
    (lisp-unit:assert-true (binary-tree-equal a
                                              (binary-tree-from-list '(1 nil (2 nil (3))))
                                              #'-))
    (lisp-unit:assert-true (binary-tree-equal a
                                              (binary-tree-from-list '(3 (2 (1))))
                                              #'-))

    (lisp-unit:assert-false (binary-tree-equal a
                                              (binary-tree-from-list '(1 (0) (3 (2))))
                                              #'-))
    (lisp-unit:assert-false (binary-tree-equal a
                                              (binary-tree-from-list '(1 nil (2 (1.5) (3))))
                                              #'-))
    (lisp-unit:assert-false (binary-tree-equal a
                                              (binary-tree-from-list '(3 (2 (1 (0)))))
                                              #'-))
   )

  ;; avl-tree

  (let ((a (make-avl-tree nil 1 nil))
        (b (make-avl-tree nil 3 nil))
        (c (make-avl-tree nil 5 nil))
        (d (make-avl-tree nil 7 nil)))
    (let ((bal (make-avl-tree (make-avl-tree a 2 b) 4 (make-avl-tree c 6 d)))
          (right-right (make-avl-tree a 2 (make-avl-tree b 4 (make-avl-tree c 6 d))))
          (right-left (make-avl-tree a 2 (make-avl-tree (make-avl-tree b 4 c) 6 d)))
          (left-left (make-avl-tree (make-avl-tree (make-avl-tree a 2 b) 4 c) 6 d))
          (left-right (make-avl-tree (make-avl-tree a 2 (make-avl-tree b 4 c)) 6 d)) )
      (let ((bal-right-right (left-avl-tree (binary-tree-left right-right)
                                            (binary-tree-value right-right)
                                            (binary-tree-right right-right)))
            (bal-right-left (left-right-avl-tree (binary-tree-left right-left)
                                                 (binary-tree-value right-left)
                                                 (binary-tree-right right-left)))
            (bal-left-left (right-avl-tree (binary-tree-left left-left)
                                           (binary-tree-value left-left)
                                           (binary-tree-right left-left)))
            (bal-left-right (right-left-avl-tree (binary-tree-left left-right)
                                                 (binary-tree-value left-right)
                                                 (binary-tree-right left-right))))
        (lisp-unit:assert-equalp bal bal-right-right)
        (lisp-unit:assert-equalp bal bal-left-right)
        (lisp-unit:assert-equalp bal bal-right-left)
        (lisp-unit:assert-equalp bal bal-left-left))))

  (dotimes (i 100)
    (labels ((avl-tree-list (tree)
               (map-binary-tree :inorder 'list #'identity tree)))
      (let* ((list-1 (loop for i below 50 collect (random 100)))
             (list-2 (loop for i below 100 collect (+ 110 (random 100))))
             (sort-1 (remove-duplicates (sort (copy-list list-1) #'<)))
             (sort-2 (remove-duplicates (sort (copy-list list-2) #'<)))
             (avl-tree-1 (fold (lambda (a x) (avl-tree-insert a x #'-))
                               (make-avl-tree nil (car list-1) nil) (cdr list-1)))
             (avl-tree-2 (fold (lambda (a x) (avl-tree-insert a x #'-))
                               (make-avl-tree nil (car list-2) nil) (cdr list-2)))
             (avl-tree-12 (fold (lambda (a x) (avl-tree-insert a x #'-)) avl-tree-1 list-2))
             (avl-tree-cat (avl-tree-concatenate avl-tree-1 avl-tree-2)))

        ;; construction
        (lisp-unit:assert-equal (avl-tree-list avl-tree-1) sort-1)
        (lisp-unit:assert-equal (avl-tree-list avl-tree-2) sort-2)

        ;; concatenate
        (lisp-unit:assert-equal (avl-tree-list avl-tree-cat)
                                (append sort-1 sort-2))
        (lisp-unit:assert-equal (avl-tree-list avl-tree-cat)
                                (avl-tree-list avl-tree-12))

        ;; equal
        (lisp-unit:assert-true (binary-tree-equal avl-tree-cat avl-tree-12 #'-))

        (lisp-unit:assert-true (not (binary-tree-equal avl-tree-1 avl-tree-2 #'-)))

        ;; subset
        (lisp-unit:assert-true (binary-tree-subset avl-tree-1 avl-tree-12 #'-))
        (lisp-unit:assert-true (binary-tree-subset avl-tree-2 avl-tree-12 #'-))
        (lisp-unit:assert-true (binary-tree-subset avl-tree-cat avl-tree-12 #'-))

        (lisp-unit:assert-true (not (binary-tree-subset avl-tree-12 avl-tree-1 #'-)))
        (lisp-unit:assert-true (not (binary-tree-subset avl-tree-12 avl-tree-2 #'-)))


        ;; min
        (lisp-unit:assert-equal (binary-tree-min avl-tree-1)
                                (car sort-1))
        (lisp-unit:assert-equal (binary-tree-min avl-tree-2)
                                (car sort-2))

        ;; remove-min
        (lisp-unit:assert-equal (avl-tree-list (avl-tree-remove-min avl-tree-1))
                                (cdr sort-1))
        (lisp-unit:assert-equal (avl-tree-list (avl-tree-remove-min avl-tree-2))
                                (cdr sort-2))

        ;; remove-max
        (lisp-unit:assert-equal (avl-tree-list (avl-tree-remove-max avl-tree-1))
                                (subseq sort-1 0 (1- (length sort-1))))
        (lisp-unit:assert-equal (avl-tree-list (avl-tree-remove-max avl-tree-2))
                                (subseq sort-2 0 (1- (length sort-2))))

        ;; remove
        (let ((list (append sort-1 sort-2)))
          (dotimes (i 10)
            (let ((i (random (length list))))
              (lisp-unit:assert-equal (avl-tree-list (avl-tree-remove avl-tree-cat (elt list i) #'-))
                                      (append (subseq list 0 i)
                                              (subseq list (1+ i)))))))


        ;; split
        (multiple-value-bind (left present right)
            (avl-tree-split avl-tree-12 101 #'-)
          (lisp-unit:assert-equal (avl-tree-list left) sort-1)
          (lisp-unit:assert-equal (avl-tree-list right) sort-2)
          (lisp-unit:assert-false present)
          )
        )))


  (dotimes (i 100)
    (let* ((list-1 (loop for i below 100 collect (random 100)))
           (list-2 (loop for i below 100 collect (random 100)))
           (set-1 (apply #'tree-set #'- list-1))
           (set-2 (apply #'tree-set #'- list-2)))
      ;; union
      (lisp-unit:assert-equal (remove-duplicates (sort (copy-list (union list-1 list-2)) #'<))
                              (map-tree-set 'list #'identity (tree-set-union set-1 set-2)))
      ;; intersection
      (lisp-unit:assert-equal (remove-duplicates (sort (copy-list (intersection list-1 list-2)) #'<))
                              (map-tree-set 'list #'identity (tree-set-intersection set-1 set-2)))
      ;; difference
      (lisp-unit:assert-equal (remove-duplicates (sort (copy-list (set-difference list-1 list-2)) #'<))
                              (map-tree-set 'list #'identity (tree-set-difference set-1 set-2)))
      ;; member
      (dolist (x list-1)
        (lisp-unit:assert-true (tree-set-member-p set-1 x)))
      (dolist (x list-2)
        (lisp-unit:assert-true (tree-set-member-p set-2 x)))
      (let ((set-i (tree-set-difference set-1 set-2)))
        (dolist (x list-2)
          (lisp-unit:assert-false (tree-set-member-p set-i x))))
      ;; remove
      (lisp-unit:assert-equal (remove-duplicates (sort (copy-list (set-difference list-1 list-2)) #'<))
                              (map-tree-set 'list #'identity (fold #'tree-set-remove set-1 list-2)))

      ;; subset
      (lisp-unit::assert-true (tree-set-subset set-1 (tree-set-union set-1 set-2)))
      (lisp-unit::assert-true (tree-set-subset set-2 (tree-set-union set-1 set-2)))

      (lisp-unit::assert-true (or (subsetp list-1 list-2)
                                  (not (tree-set-subset set-1 set-2))))
      (lisp-unit::assert-true (or (subsetp list-2 list-1)
                                  (not (tree-set-subset set-2 set-1))))
      ))
  )

;; (lisp-unit:define-test red-black
;;   ;; red-black
;;   (let ((bal (make-red-black t
;;                              (make-red-black nil 'a 'x 'b)
;;                              'y
;;                              (make-red-black nil 'c 'z 'd))))

;;     (lisp-unit:assert-equalp bal
;;                              (balance-red-black nil (make-red-black t 'a
;;                                                                     'x
;;                                                                     (make-red-black t 'b 'y 'c))
;;                                                 'z 'd))
;;     (lisp-unit:assert-equalp bal
;;                              (balance-red-black t
;;                                                 (make-red-black nil 'a 'x 'b)
;;                                                 'y
;;                                                 (make-red-black nil 'c 'z 'd)))

;;     (lisp-unit:assert-equalp bal
;;                              (balance-red-black nil
;;                                                 (make-red-black t (make-red-black t 'a 'x 'b)
;;                                                                 'y
;;                                                                 'c)
;;                                                 'z
;;                                                 'd))
;;     (lisp-unit:assert-equalp bal
;;                              (balance-red-black nil
;;                                                 'a
;;                                                 'x
;;                                                 (make-red-black t
;;                                                                 'b
;;                                                                 'y
;;                                                                 (make-red-black t 'c 'z 'd))))
;;     (lisp-unit:assert-equalp bal
;;                              (balance-red-black nil 'a 'x
;;                                                 (make-red-black t (make-red-black t 'b 'y 'c)
;;                                                                 'z 'd)))))
