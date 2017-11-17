#lang racket
(require rackunit)
(require rackunit/text-ui)
(require (only-in "02-make-matrix.rkt" make-matrix))
(provide get-column)
; Искаме да можем да взимаме к-тата колона на дадена матрица
; Същото като get-row.
; Изглежда сложно.
; Наскоро учихме едни полезни функции за работа със списъци.

(define (nth x n)
  (if (= n 0)
      (car x)
      (nth (cdr x) (- n 1))
     )
  )

(define (get-column matrix k)
  (map (lambda(x) (nth x k)) matrix)
  )

(define tests
  (test-suite "Get row tests"
    ;Примерно използване на weird-range:
    ; (weird-range 1 10 3) -> (1 11 21)
    ; (weird-range 5 11 2) -> (5 16 27)
    (let ((weird-range
           (lambda (value increment n)
             (letrec ((helper
                    (lambda (counter)
                      (if (= counter n)
                          '()
                          (cons (+ value (* increment counter)) (helper (+ counter 1)))))))
               (helper 0)))))
                         
      (check-equal? (get-column (make-matrix (range 1 7) 2 3) 1) '(2 5))
      (check-equal? (get-column (make-matrix (range 1 101) 5 10) 3) (weird-range 4 10 5))
    )
  )
)

(run-tests tests 'verbose)