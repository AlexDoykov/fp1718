#lang racket


(define (cnt elem xs)
  (cond
    ((null? xs) 0)
    ((not (= (car xs) elem)) 0) 
    ((= (car xs) elem) (+ 1 (cnt elem (cdr xs))))
    (else (cnt elem (cdr xs)))
    )
  )

(define (make-assoc-list xs)
  (if (null? xs)
      '()
      (cons (cons (car xs) (cnt (car xs) xs)) (make-assoc-list (drop xs (cnt (car xs) xs))))
  )
)


(define (member? x xs)
  (cond
    ((null? xs) #f)
    ((equal? x (car xs)) #t)
    (else (member? x (cdr xs)))
    )
  )

(define graph '((a b c) (b c) (c) (d c)))

(define (get-edges graph)
  (map car graph)
)

(define (get-neighbours graph edge)
 (cdr (assv edge graph))
)

(define (count-exit graph edge)
  (length (cdr (assv edge graph)))
)

(define (count-enter graph edge)
  (length (filter (lambda (x) (member? edge (cdr x))) graph)) 
)