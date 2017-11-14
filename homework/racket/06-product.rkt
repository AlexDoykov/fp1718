#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "02-make-matrix.rkt")
; Търсим произведението на две матрици

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

; Само вмятам: на това му казват dot product
(define (pow-row-column row column)
  (if (or (null? column) (null? row))
      0
      (+ (* (car row) (car column)) (pow-row-column (cdr row) (cdr column)))
      )
  )

(define (cal-list row matrix)
  (define (helper row matrix index)
    (if (= index (- (length row) 1))
        '()
        (cons (pow-row-column row (get-column matrix index)) (helper row matrix (+ index 1))))
    )
  (helper row matrix 0)
  )


(define (matrix* m1 m2)
  (if (null? m1)
      '()
      (cons (cal-list (car m1) m2) (matrix* (cdr m1) m2))
))

(define tests
  (test-suite "Multiplication tests"
    (let ((first-matrix (make-matrix (range 1 7) 2 3))
          (second-matrix (make-matrix (range 7 13) 3 2))
          (expected-result (make-matrix '(58 64 139 154) 2 2)))
      (check-equal? (matrix* first-matrix second-matrix) expected-result))
  )
)

(run-tests tests 'verbose)


  ;(1 2 3)
  ;(4 5 6)

  ;(7 8)
  ;(9 10)
  ;(11 12)