(define head (lambda (x . y) x))
(define tail (lambda (x . y) y))

(assert-equal? 1 (head 1 2 3))
(assert-equal? (list 2 3) (tail 1 2 3))
