(define head (lambda (x . y) x))
(define tail (lambda (x . y) y))

(write (head 1 2 3))
(newline)
(write (tail 1 2 3))
(newline)
