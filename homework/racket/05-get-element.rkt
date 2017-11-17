#lang racket
(require rackunit)
(require rackunit/text-ui)
(require (only-in "./02-make-matrix.rkt" make-matrix))
 (require "./03-get-row.rkt")
; Искаме да можем да взимаме елемента на ред i и колона j в дадена матрица

; Използвай взимане на i-ти ред или j-та колона.
; Написал си ги и ще ти спестят няколко реда в get-element.
; (provide get-column) (или get-row) в съответния файл
; + (require "./04-get-column.rkt") тук ти вършат работа.

;(define (get-element matrix i j)
;  (cond
;    ((null? matrix) '())
;    ((> i 0) (get-element (cdr matrix) (- i 1) j))
;    ((list? (car matrix)) (get-element (car matrix) i j))
;    ((= j 0) (car matrix))
;    (else (get-element (cdr matrix) i (- j 1)))
;    )
;)

(define (get-element matrix i j)
  (cond ((> i 0) (get-element (get-row matrix i) 0 j)) ;тук може ли взимането на реда да се напише по някакъв по хубав начин
        ((> j 0) (get-element (cdr matrix) i (- j 1)))
        (else (car matrix)))
  )

(define tests
  (test-suite "Get element tests"
      (check-equal? (get-element (make-matrix (range 1 7) 2 3) 1 2) 6)
      (check-equal? (get-element (make-matrix (range 1 101) 4 5) 2 2) 13)
  )
)

(run-tests tests 'verbose)