;; Copyright 2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/vector
         rackunit
         binary-search)

(define (linear-search v goal
                       [start 0]
                       [end (vector-length v)]
                       #:mode [mode 'least/=])
  (define (accept? y)
    (case mode
      [(least/= greatest/=) (= y goal)]
      [(least/>) (> y goal)]
      [(least/>=) (>= y goal)]
      [(greatest/<) (< y goal)]
      [(greatest/<=) (<= y goal)]
      [else #f]))
  (case mode
    [(least/= least/> least/>=)
     (for/first ([x (in-range start end)]
                 #:when (let ([y (vector-ref v x)])
                          (accept? y)))
       x)]
    [(greatest/= greatest/< greatest/<=)
     (for/first ([x (in-range (sub1 end) (sub1 start) -1)]
                 #:when (let ([y (vector-ref v x)])
                          (accept? y)))
       x)]))

(define (random-sorted-vector len maxn)
  (define v (for/vector ([i (in-range len)])
              (add1 (random maxn))))
  (vector-sort! v <)
  v)

(define det-modes
  '(least/= greatest/= least/> least/>= greatest/< greatest/<=))

(define (test-goal v goal)
  (let ([k (binary-search v goal #:mode 'any/=)])
    ;; any/=
    (cond [k (check-equal? (vector-ref v k) goal)]
          [else (check-equal? k (linear-search v goal #:mode 'least/=))]))
  ;; deterministic modes
  (for ([mode (in-list det-modes)])
    (check-equal? (binary-search v goal #:mode mode)
                  (linear-search v goal #:mode mode)
                  (format "mode = ~s, goal = ~s" mode goal))))

(define (test len maxn)
  (random-seed 77)
  (test-case (format "test with length ~s, max ~s" len maxn)
    (define v (random-sorted-vector len maxn))
    #;(printf "v = ~s\n" v)
    (for ([goal (in-range 0 (+ maxn 2))])
      (test-goal v goal))))

(test 10 10)
(for ([iter 5])
  (test 100 100))
(test 1000 100)
(test 100 1000)
(test 1000 1000)

;; TODO:
;; - test cmp
