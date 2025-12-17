#lang scribble/manual
@(require scribble/examples
          (for-label racket/base racket/contract/base binary-search))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/vector binary-search))

@title[#:tag "binary-search"]{Binary Search}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[binary-search]

@defproc[(binary-search [data (or/c (-> exact-integer? any/c) vector?)]
                        [goal any/c]
                        [start exact-integer? 0]
                        [end (or/c exact-integer? #f) #f]
                        [#:compare compare (or/c #f (-> any/c any/c (or/c '= '< '>))) #f]
                        [#:mode mode symbol? 'any/=])
         (or/c exact-integer? #f)]{

Performs a binary search for @racket[goal] in @racket[data] and returns the
@racket[index] in the range @racket[start] (inclusive) to @racket[end]
(exclusive) that satisfies the criteria specified by @racket[mode]. If no such
index exists, @racket[#f] is returned.

The input, @racket[data] from @racket[start] to @racket[end], must be sorted in
ascending order according to @racket[compare]. If @racket[compare] is
@racket[#f], then standard numeric order is used.

If @racket[end] is @racket[#f] and @racket[data] is a vector, then the end index
is @racket[(vector-length data)]. If @racket[end] is @racket[#f] and
@racket[data] is a procedure, then the end index is found by repeatedly doubling
the search range until it contains the correct result. If the doubling phase
does not succeed within a certain number of steps (currently 100), an error is
raised.

If the result @racket[_index] is not @racket[#f], then it is an exact integer
satisfying @racket[(<= start _index)] and @racket[(< _index end)]. The result
@racket[_index] is determined by the @racket[_mode] as follows:
@itemlist[

@item{@racket['any/=] -- Returns any @racket[_index] in the given range such
that @racket[(data _index)] is equal to @racket[goal] according to
@racket[compare]. The result is not necessarily the least or greatest such
index, but rather the first such index found during the binary search.}

@item{@racket['least/=], @racket['greatest/=] --- Returns the least or greatest
@racket[_index], respectively, such that @racket[(data _index)] is equal to
@racket[goal].}

@item{@racket['greatest/<] --- Returns the greatest @racket[_index] such that
@racket[(data _index)] is strictly greater than @racket[goal].}

@item{@racket['greatest/<=] --- Returns the greatest @racket[_index] such that
@racket[(data _index)] is greater than or equal to @racket[goal].}

@item{@racket['least/>] --- Returns the least @racket[_index] such that
@racket[(data _index)] is strictly less than @racket[goal].}

@item{@racket['least/>=] --- Returns the least @racket[_index] such that
@racket[(data _index)] is less than or equal to @racket[goal].}

]

@examples[#:eval the-eval
(define data (vector 1 3 4 4 4 9))
(define (show-split v k)
  (printf "~s => ~s ~s ~s\n" k
          (vector-copy v 0 k)
          (vector-ref v k)
          (vector-copy v (add1 k))))
(show-split data (binary-search data 4 #:mode 'any/=))
(show-split data (binary-search data 4 #:mode 'least/=))
(show-split data (binary-search data 4 #:mode 'greatest/=))
(show-split data (binary-search data 4 #:mode 'greatest/<))
(show-split data (binary-search data 4 #:mode 'greatest/<=))
(show-split data (binary-search data 4 #:mode 'least/>))
(show-split data (binary-search data 4 #:mode 'least/>=))
(binary-search data 2 #:mode 'least/=)
(show-split data (binary-search data 2 #:mode 'least/>))
(define (floor-div4 n) (floor (/ n 4)))
(binary-search floor-div4 5 #:mode 'least/=)
(binary-search floor-div4 5 #:mode 'greatest/=)
(binary-search floor-div4 5 #:mode 'least/>)
]}


@defproc[(binary-search-all [data (or/c (-> exact-integer? any/c) vector?)]
                            [goal any/c]
                            [start exact-integer? 0]
                            [end exact-integer? (and (vector? data) (vector-length data))]
                            [#:compare compare (or/c #f (-> any/c any/c (or/c '= '< '>))) #f])
         (values exact-integer? exact-integer?)]{

Returns the range of indexes of @racket[data] within @racket[start] (inclusive)
to @racket[end] (exclusive) whose values are equal to @racket[goal].

The input, @racket[data] from @racket[start] to @racket[end], must be sorted in
ascending order according to @racket[compare].  If @racket[compare] is
@racket[#f], then standard numeric order is used.

The function returns @racket[(values _starteq _endeq)] such that
the following properties hold:
@itemlist[

@item{@racket[(<= start _starteq _endeq end)]}

@item{For every index @racket[_k] such that @racket[(<= start _k)] and
@racket[(< _k _starteq)], @racket[(data _k)] is strictly less than
@racket[goal] according to @racket[compare].}

@item{For every index @racket[_k] such that @racket[(<= _starteq _k)] and
@racket[(< _k _endeq)], @racket[(data _k)] is equal to @racket[goal] according
to @racket[compare].}

@item{For every index @racket[_k] such that @racket[(<= _endeq _k)] and
@racket[(< _k end)], @racket[(data _k)] is strictly greater than @racket[goal]
according to @racket[compare].}

]
Given the partition of the input into less-than, equal, and greater-than ranges,
it is straightforward to implement all of the modes supported by
@racket[binary-search]. Note that any of the ranges may be empty. In particular,
if the input does not contain any values equal to @racket[goal], then
@racket[_starteq] and @racket[_endeq] are equal and indicate the index where
inserting @racket[goal] would preserve ordering. (Beware, the insertion point is
limited to the range from @racket[start] to @racket[end], and it would preserve
ordering for that section of @racket[data], but it may not preserve ordering on
@racket[data]'s full domain.)

@examples[#:eval the-eval
(define data (vector 1 3 4 4 4 9))
(define-syntax-rule (show-split2 v e)
  (let-values ([(starteq endeq) e])
    (printf "~s ~s => ~s ~s ~s\n"
            starteq endeq
            (vector-copy v 0 starteq)
            (vector-copy v starteq endeq)
            (vector-copy v endeq))))
(show-split2 data (binary-search-all data 4))
(show-split2 data (binary-search-all data 5))
(show-split2 data (binary-search-all data 10))
(show-split2 data (binary-search-all data 10 0 4))
(define (floor-div4 n) (floor (/ n 4)))
(binary-search-all floor-div4 5)
]}

@; ----------------------------------------
@(close-eval the-eval)
