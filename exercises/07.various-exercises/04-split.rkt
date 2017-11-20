#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да разделим списъка xs на две.
; Дадена е дължината, която искаме да има първия - n.

(define (split n xs)
  (define (helper n xs result)
  (cond
    ((= n 0) (cons result (cons xs '())))
    (else (helper (- n 1) (cdr xs) (append result (list (car xs)))))
  ))
  (helper n xs '())
  )

(define tests
  (test-suite "Split"
    (check-equal? (split 3 '(1 2 3 4 5 6 7)) '((1 2 3) (4 5 6 7)))
    (check-equal? (split 0 '(1 2 3 4 5 6 7)) '(() (1 2 3 4 5 6 7)))
    (check-equal? (split 7 '(1 2 3 4 5 6 7)) '((1 2 3 4 5 6 7) ()))
  )
)
(run-tests tests 'verbose)
