#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да дефинираме следните имена

; one, two, three, four, five, six, seven, eight, nine, ten
; plus, minus, times, div

; Така че тестовете долу да минават.

(define (one . x)
  (cond
    ((null? x) 1)
    ((not (list? (car x))) x)
    ((and (list? (car x)) (not (null? (car x)))) ((car (car x)) (one (cdr x))))
    (else 4)
  )
  )

(define (plus other)
  (list + other)
  )

(define tests
  (test-suite "Numbers tests"
    (check-equal? (one (plus (one))) 2)
;    (check-equal? (three (times (five))) 15)
;    (check-equal? (eight (div (two))) 4)
;    (check-equal? (seven (times (six))) 42)
;    (check-equal? (nine (minus (three))) 6)
    ; Добави и за изваждане
  )
)

(run-tests tests 'verbose)
