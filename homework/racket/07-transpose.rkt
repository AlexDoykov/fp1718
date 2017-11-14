#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "02-make-matrix.rkt")

; Търсим транспонираната матрица на дадена такава

(define (get-column matrix k)
  (define (helper matrix k cur-k)
    (if (null? matrix)
        '()
        (if (list? (car matrix))
            (cons (helper (car matrix) k 0) (helper (cdr matrix) k 0))
            (if (= cur-k k)
                (car matrix)
                (helper (cdr matrix) k (+ cur-k 1))
                )
        )
    )
    )
  (helper matrix k 0)
  )

; cool. Опитай да я направиш с някоя от трите нови функции за списъци, които правихме.
(define (transpose m)
  (define (helper m index)
    (if (= index (length (car m)))
        '()
        (cons (get-column m index) (helper m (+ index 1)))
        )
    )
  (helper m 0)
  )

(define tests
  (test-suite "Transpose tests"
    (check-equal? (transpose (make-matrix (range 1 7) 2 3)) (make-matrix '(1 4 2 5 3 6) 3 2))
    (check-equal? (transpose (make-matrix (range 7 13) 3 2)) (make-matrix '(7 9 11 8 10 12) 2 3))
    (check-equal? (transpose (make-matrix (range 1 5) 2 2)) (make-matrix '(1 3 2 4) 2 2))
  )
)

(run-tests tests 'verbose)
