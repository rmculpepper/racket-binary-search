;; Copyright 2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(provide binary-search
         binary-search-all)

;; Global assumptions:
;; - start <= end; search space consists of [start,end)
;; - forall i,j: if (start <= i <= j < end), then (f i) <= (f j)

;; (Data X) = (U (Vectorof X) (Integer -> X))
;; (Cmp X) = (X X -> (U '= '< '>))  or  #f if X is Real

;; binary-search : (Data X) X Integer Integer [(Cmp X) Symbol] -> (U Integer #f)
(define (binary-search f goal
                       [start 0]
                       [end (and (vector? f) (vector-length f))]
                       #:compare [cmp #f]
                       #:mode [mode 'any/=])
  (define who 'binary-search)
  (check-args who f goal start end cmp)
  (binary-search* f goal start end cmp mode who))

;; binary-search-all : ... -> (values Integer Integer)
(define (binary-search-all f goal
                           [start 0]
                           [end (and (vector? f) (vector-length f))]
                           #:compare [cmp #f])
  (define who 'binary-search-all)
  (check-args who f goal start end cmp)
  (binary-search-all* f goal start end cmp 'ab who))

;; ----------------------------------------

(define (check-args who f goal start end cmp)
  (unless (or (vector? f) (and (procedure? f) (procedure-arity-includes? f 1)))
    (raise-argument-error who "(or/c vector? (procedure-arity-includes/c 1))" f))
  (unless (exact-integer? start)
    (raise-argument-error who "exact-integer?" start))
  (unless (exact-integer? end)
    (raise-argument-error who "exact-integer?" end))
  (unless (or (eq? cmp #f) (and (procedure? cmp) (procedure-arity-includes? cmp 2)))
    (raise-argument-error who "(or/c #f (procedure-arity-includes/c 2))" cmp))
  (cond [(vector? f)
         (unless (and (<= 0 start) (< start (vector-length f)))
           (raise-range-error who "vector" "start " start f 0 (sub1 (vector-length f))))
         (unless (<= start end (vector-length f))
           (raise-range-error who "vector" "end " end f start (sub1 (vector-length f))))]
        [else
         (unless (<= start end)
           (raise-arguments-error who "bad start and end indexes" "start" start "end" end))]))

(define (binary-search* f goal start end cmp mode who)
  (case mode
    [(any/=)
     (define-values (a b) (binary-search-all* f goal start end cmp 'any who))
     (and (< a b) a)]
    [(least/= greatest/=)
     (define-values (a b) (binary-search-all* f goal start end cmp 'ab who))
     (case mode
       [(least/=) (and (< a b) a)]
       [(greatest/=) (and (< a b) (sub1 b))])]
    [(least/>= greatest/<)
     (define-values (a _b) (binary-search-all* f goal start end cmp 'a who))
     (case mode
       [(least/>=) (and (< a end) a)]
       [(greatest/<) (and (< start a) (sub1 a))])]
    [(least/> greatest/<=)
     (define-values (_a b) (binary-search-all* f goal start end cmp 'b who))
     (case mode
       [(least/>) (and (< b end) b)]
       [(greatest/<=) (and (< start b) (sub1 b))])]
    [else (error who "bad mode: ~e" mode)]))

(define (binary-search-all* f goal start end cmp mode who)
  (define (binary-search-loop a b mode)
    ;; INV(a,ab): forall i : if (start <= i < a), then ref(i) < goal
    ;; INV(b,ab): forall j : if (b <= j < end), then ref(j) > goal
    (cond [(= a b)
           (values a b)]
          [else
           (define mid (quotient (+ a b) 2))
           (define fmid (if (vector? f) (vector-ref f mid) (f mid)))
           (define cmp-result (compare cmp fmid goal))
           (case cmp-result
             [(=) (case mode
                    [(ab)
                     (define-values (loa lob) (binary-search-loop a mid 'a))
                     (define-values (hia hib) (binary-search-loop (add1 mid) b 'b))
                     (values loa hib)]
                    [(a)
                     (binary-search-loop a mid 'a)]
                    [(b)
                     (binary-search-loop (add1 mid) b 'b)]
                    [(any)
                     (values mid (add1 mid))])]
             [(<) (binary-search-loop (add1 mid) b mode)]
             [(>) (binary-search-loop a mid mode)]
             [else (compare-error who cmp cmp-result fmid goal)])]))
  (binary-search-loop start end mode))

(define (compare cmp x y)
  (cond [cmp (cmp x y)]
        [(= x y) '=]
        [(< x y) '<]
        [(> x y) '>]
        [else #f]))

(define (compare-error who cmp result x y)
  (if cmp
      (error who "bad result from compare function\n  result: ~e\n  arguments: ~e, ~e" result x y)
      (error who "incomparable: ~e, ~e" x y)))

;; ----------------------------------------

(module+ unchecked
  (provide binary-search
           binary-search-all)
  (define (binary-search f goal
                         [start 0]
                         [end (and (vector? f) (vector-length f))]
                         #:compare [cmp #f]
                         #:mode [mode 'any/=])
    (binary-search* f goal start end cmp mode 'binary-search))
  (define (binary-search-all f goal
                             [start 0]
                             [end (and (vector? f) (vector-length f))]
                             #:compare [cmp #f])
    (define who 'unchecked-binary-search-all)
    (binary-search-all* f goal start end cmp 'ab 'binary-search-all)))
