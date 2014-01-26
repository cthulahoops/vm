(define list (lambda x x))
(define not  (lambda (x) (if x #f #t)))
(define map  (lambda (f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs))))))

(define (length lst) (if (null? lst) 0 (+ 1 (length (cdr lst)))))
