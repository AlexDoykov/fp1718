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


;=========================================


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

 (define (take-while lst)
   (define (helper lst result)
   (cond
     ((null? lst) '())
     ((= (length lst) 1) (list (car lst)))
     ((> (car lst) (car (cdr lst))) (append result (list (car lst))))
     (else (helper (cdr lst) (append result (list (car lst)))))
   ))
   (helper lst '())
   )

 (define (drop-while list)
   (cond
     ( (or (null? list) (= (length list) 1)) '())
     ((> (car list) (car (cdr list))) (cdr list))
     (else (drop-while (cdr list)))
   ))

(define (longest-ascending­ xs)
  (define (helper result xs)
  (cond
    ((null? xs) result)
    ((<= (length result) (length (take-while xs))) (helper (take-while xs) (drop-while xs)))
    (else (helper result (drop-while xs)))
    ))
  (helper (list (car xs)) (cdr xs))
  )






































