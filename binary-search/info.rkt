;; Copyright 2025 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang info

;; ========================================
;; pkg info

(define collection "binary-search")
(define deps
  '("base"
    "rackunit-lib"
    "binary-search-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
(define implies '("binary-search-lib"))
(define pkg-authors '(ryanc))

;; ========================================
;; collect info

(define name "binary-search")
(define scribblings
  '(["binary-search.scrbl" ()]))
