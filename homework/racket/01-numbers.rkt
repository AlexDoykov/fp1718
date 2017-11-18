#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да дефинираме следните имена

; one, two, three, four, five, six, seven, eight, nine, ten
; plus, minus, times, div

; Така че тестовете долу да минават.

(define (one . x)
  (if (null? x)
    1
   ((car (car x)) 1 (cadr (car  x)))
  ))

(define (plus other)
  (list + other)
  )

(define (two . x)
  (if (null? x)
    2
   ((car (car x)) 2 (cadr (car  x)))
  ))

(define (minus other)
  (list - other)
  )

(define (three . x)
  (if (null? x)
    3
   ((car (car x)) 3 (cadr (car  x)))
  ))

(define (times other)
  (list * other)
  )

(define (four . x)
  (if (null? x)
    4
   ((car (car x)) 4 (cadr (car  x)))
  ))

(define (div other)
  (list quotient other)
  )


(define (five . x)
  (if (null? x)
    5
   ((car (car x)) 5 (cadr (car  x)))
  ))


(define (six . x)
  (if (null? x)
    6
   ((car (car x)) 6 (cadr (car  x)))
  ))

(define (seven . x)
  (if (null? x)
    7
   ((car (car x)) 7 (cadr (car  x)))
  ))

(define (eight . x)
  (if (null? x)
    8
   ((car (car x)) 8 (cadr (car  x)))
  ))

(define (nine . x)
  (if (null? x)
    9
   ((car (car x)) 9 (cadr (car  x)))
  ))

(define tests
  (test-suite "Numbers tests"
    (check-equal? (one (plus (one))) 2)
    (check-equal? (three (times (five))) 15)
    (check-equal? (eight (div (two))) 4)
    (check-equal? (seven (times (six))) 42)
    (check-equal? (nine (minus (three))) 6)
    ; Добави и за изваждане
  )
)

(run-tests tests 'verbose)
