#lang racket
(require rackunit)
(require rackunit/text-ui)

 (define (take-while pred? lst)
   (define (helper lst result)
   (cond
     ((null? lst) '())
     ((= (length lst) 1) (list (car lst)))
     ((pred? (car lst) (car (cdr lst))) (append result (list (car lst))))
     (else (helper (cdr lst) (append result (list (car lst)))))
   ))
   (helper lst '())
   )

 (define (drop-while pred? list)
   (cond
     ( (or (null? list) (= (length list) 1)) '())
     ((pred? (car list) (car (cdr list))) (cdr list))
     (else (drop-while (cdr list)))
   ))



; Искаме да сортираме списък от числа по метода quicksort
(define (quicksort xs)
  (if (null? xs)
      '()
      (append (quicksort (filter (lambda (x) (>= (car xs) x)) (cdr xs) )) (list (car xs)) (quicksort (filter (lambda (x) (< (car xs) x)) (cdr xs) )) )
      ))

(define tests
  (test-suite "Generic sort tests"
    (letrec (
             (original-list '(32 39213 2813 8321 921 23))
             (sorted-list (quicksort original-list))
             (same-lengths? (lambda (xs ys) (= (length xs) (length ys))))
             (same-elements?
              (lambda (xs ys)
                (cond ((null? xs) #t)
                      ((not (member (car xs) ys)) #f)
                      (else (same-elements? (cdr xs) ys)))))
             (increasing?
              (lambda (xs)
                (cond ((null? (cdr xs)) #t)
                      ((< (car xs) (cadr xs)) (increasing? (cdr xs)))
                      (else #f))))
            )
                        
      (check-true (same-lengths? original-list sorted-list))
      (check-true (same-elements? original-list sorted-list))
      (check-true (increasing? sorted-list)))
  )
) 

(run-tests tests 'verbose)
