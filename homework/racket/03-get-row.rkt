#lang racket
(require rackunit)
(require rackunit/text-ui)
(require (only-in "./02-make-matrix.rkt" make-matrix))
(provide get-row)

; Искаме да можем да взимаме к-тия ред на дадена матрица.

; Когато обхождаш матрица (car matrix) винаги е списък (текущия ред).
; В някакъв момент (ред 16) извикваш get-row с нещо, което не е матрица, а просто списък (текущия ред).
; След това cons-ваш наново целия ред в нов списък. Защо?
(define (get-row matrix k)
  (if(null? matrix)
     '()
      (if (= k 0)
           (if(list? (car matrix))
              (get-row (car matrix) k)
              (cons (car matrix) (get-row (cdr matrix) k)))
           (get-row (cdr matrix) (- k 1)))
       )
)

(define tests
  (test-suite "Get row tests"
    (check-equal? (get-row (make-matrix (range 1 7) 2 3) 1) '(4 5 6))
    (check-equal? (get-row (make-matrix (range 1 101) 5 10) 3) (range 31 41))
  )
)

(run-tests tests 'verbose)
