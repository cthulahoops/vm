(assert-equal? (* (call/cc (lambda (k) (k 5))) 4) 20)
(assert-equal? (* 4 (call/cc (lambda (k) (k 5)))) 20)
