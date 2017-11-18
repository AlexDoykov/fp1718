#lang racket

(define (find-other op? f g a b)
  (cond
    ((> a b) #f)
    ((op? (f a) (g a)) #t)
    (else (find-other op? f g (+ a 1) b))
  ))
  

(define (mixed? f g a b)
  (cond
    ((> a b) #f)
    ((< (f a) (g a)) (if (find-other > f g (+ a 1) b) #t #f))
    ((> (f a) (g a)) (if (find-other < f g (+ a 1) b) #t #f))
    (else (mixed? f g (+ a 1) b))
  )
  )





(define (member? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (member? x (cdr xs)))
    )
  )

(define (unique-in-sub-list xs)
  (cond ((null? xs) '())
        ((not (member? (car xs) (cdr xs))) (cons (car xs) (unique-in-sub-list (cdr xs))))
        (else (unique-in-sub-list (filter (lambda(x) (not (= x (car xs)))) xs)))
        )
  )


(define (maxUnique ll)
  (define (helper ll max-element)
    (cond ((null? ll) max-element)
          ((null? (unique-in-sub-list (car ll))) (helper (cdr ll) max-element))
          ((equal? max-element #f) (helper (cdr ll) (apply max (unique-in-sub-list (car ll)))))
          ((< max-element (apply max (unique-in-sub-list (car ll)))) (helper (cdr ll) (apply max (unique-in-sub-list (car ll)))))
          (else (helper (cdr ll) max-element))
          )
  )
  (helper ll #f)
  )


(define (checkMatrix? m k)
  (cond ((null? m) #t)
        ((null? (filter (lambda(x) (if (= (remainder k x) 0) #f #t)) (car m))) #f)
        (else (checkMatrix? (cdr m) k))
        ))

(define  (longestAscending­ l)
  (define (helper result l)
    (take (lambda (x) (< x (car l)) #t) l))
  (helper (car l) (cdr l))
  )












































