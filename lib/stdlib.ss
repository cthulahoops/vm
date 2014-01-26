(define list (lambda x x))
(define map (lambda (f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs))))))
