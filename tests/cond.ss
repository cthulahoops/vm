(define (c x)
  (cond [(= x 7) 1]
	  [(> x 7) (define y 5) y]
	  [else 2]))

(assert-equal? 1 (c 7))
(assert-equal? 5 (c 8))
(assert-equal? 2 (c 0))
