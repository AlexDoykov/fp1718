#lang racket
(require rackunit)
(require rackunit/text-ui)

; Искаме да вмъкнем х в списъка хs на съответния index.
; Индексираме от 0.

(define (insert x index xs)
  (define (helper x index xs result)
    (cond
      ((null? xs) (append result (list x)))
      ((= index 0) (append (append result (list x))  xs))
      (else (helper x (- index 1) (cdr xs) (append result (list (car xs)))))
    ))
  (helper x index xs '())
  )

(define tests
 (test-suite "random-tests"
   (check-equal? (insert 2 1 '(1 3 4 5)) '(1 2 3 4 5))
   (check-equal? (insert 8 0 '(1 7 5)) '(8 1 7 5))
   (check-equal? (insert 99 3 '()) '(99))
   (check-equal? (insert 99 88 '(1 2 3)) '(1 2 3 99))
 )
) 



(run-tests tests 'verbose)
