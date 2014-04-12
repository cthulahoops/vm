(define vm?  ($vm-op 1 type?))
(define error ($vm-op 1 error))
(define str  ($vm-op 1 show))
(define car  ($vm-op 1 decons flip drop))
(define cdr  ($vm-op 1 decons drop))
(define cons ($vm-op 2 flip cons))
(define vm+ ($vm-op 2 +))
(define vm* ($vm-op 2 *))
(define vm- ($vm-op 2 -))
(define <   ($vm-op 2 <))
(define >   ($vm-op 2 >))
(define <=  ($vm-op 2 <=))
(define >=  ($vm-op 2 >=))

(define eq?   ($vm-op 2 =))
(define (= x y) (and (number? x) (number? y) (eq? x y)))
(define eqv?  eq?)

(define (equal? x y)
    (or (eq? x y)
        (and (pair? x) (pair? y) (equal? (car x) (car y)) (equal? (cdr x) (cdr y)))))

(define error ($vm-op 1 error))

(define (null? x) (eq? '() x))

(define $vm-port ($vm-op 1 port))
(define (current-output-port) ($vm-port 'stdout))
(define (current-input-port) ($vm-port 'stdin))
(define (current-error-port) ($vm-port 'stderr))

(define $vm-write ($vm-op 2 write))
(define (newline) ($vm-write (current-output-port) "\n"))
(define (write x) ($vm-write (current-output-port) (value->string x)))

(define $vm-read ($vm-op 1 read))

(define list (lambda x x))
(define not  (lambda (x) (if x #f #t)))
(define map  (lambda (f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs))))))

(define (procedure? x) (and
                         (eqv? 'pair (vm? x))
                         (eqv? 'procedure (vm? (car x)))))
(define (pair? x)   (and
                      (eqv? 'pair (vm? x))
                      (not (procedure? x))))
(define (number? x) (eqv? 'number (vm? x)))
(define (symbol? x) (eqv? 'symbol (vm? x)))
(define (string? x) (eqv? 'string (vm? x)))

(define (length lst) (if (null? lst) 0 (+ 1 (length (cdr lst)))))

(define (foldl f a xs) (if (null? xs) a (foldl f (f a (car xs)) (cdr xs))))

(define + (lambda xs (foldl vm+ 0 xs)))
(define * (lambda xs (foldl vm* 1 xs)))
(define (- x . xs) (foldl vm- x xs))

(define string-append (lambda xs (foldl vm+ "" xs)))

(define (value->string x)
  (cond [(pair? x) (string-append "(" (value->string (car x)) (tail->string (cdr x)))]
	  [(null? x) "()"]
	  [else (str x)]))

(define (tail->string x)
  (cond [(pair? x) (string-append " " (value->string (car x)) (tail->string (cdr x)))]
	  [(null? x) ")"]
	  [else (string-append " . " (str x) ")")]))

(define (assert x msg) (if x #t (error msg)))
(define (assert-equal? x y)  (assert (equal? x y) (value->string (list 'assertion-failed x y)))) 
