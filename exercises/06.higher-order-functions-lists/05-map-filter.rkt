#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "01-map.rkt")
(require "02-filter.rkt")
(require "03-fold-left.rkt")
(require "04-fold-right.rkt")

; Дефинирайте filter въз основа на fold-right
(define (filter p? xs)
  (fold-right p? 1 xs)
)

; Дефинирайте map въз основа на fold-left
(define (map f xs)
  (fold-left f '() xs)
)

(display "ACTUAL TESTS-----------------------------------------------------------\n")
(run-tests map-tests 'verbose)
(run-tests filter-tests 'verbose)