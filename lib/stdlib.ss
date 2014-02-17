(define null? ($vm-op 1 nil =))
(define vm?  ($vm-op 1 ?))
(define str  ($vm-op 1 show))
(define car  ($vm-op 1 ` flip drop))
(define cdr  ($vm-op 1 ` drop))
(define cons ($vm-op 2 flip ,))
(define vm+ ($vm-op 2 +))
(define vm* ($vm-op 2 *))
(define -   ($vm-op 2 -))
(define <   ($vm-op 2 <))
(define >   ($vm-op 2 >))
(define <=  ($vm-op 2 <=))
(define >=  ($vm-op 2 >=))
(define =   ($vm-op 2 =))

(define $vm-port ($vm-op 1 port))
(define (current-output-port) ($vm-port 'stdout))
(define (current-input-port) ($vm-port 'stdin))
(define (current-error-port) ($vm-port 'stderr))

(define $vm-write ($vm-op 2 w))
(define (newline) ($vm-write (current-output-port) "\n"))
(define (write x) ($vm-write (current-output-port) (value->string x)))

(define $vm-read ($vm-op 1 r))

(define list (lambda x x))
(define not  (lambda (x) (if x #f #t)))
(define map  (lambda (f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs))))))

(define eqv? =)

(define (pair? x) (eqv? 'pair (vm? x)))
(define (number? x) (eqv? 'number (vm? x)))
(define (symbol? x) (eqv? 'symbol (vm? x)))
(define (string? x) (eqv? 'string (vm? x)))

(define (length lst) (if (null? lst) 0 (+ 1 (length (cdr lst)))))

(define (foldl f a xs) (if (null? xs) a (foldl f (f a (car xs)) (cdr xs))))

(define + (lambda xs (foldl vm+ 0 xs)))
(define * (lambda xs (foldl vm* 1 xs)))
(define string-append (lambda xs (foldl vm+ "" xs)))

(define (value->string x)
  (cond [(pair? x) (string-append "(" (value->string (car x)) (tail->string (cdr x)))]
	  [(null? x) "()"]
	  [else (str x)]))

(define (tail->string x)
  (cond [(pair? x) (string-append " " (value->string (car x)) (tail->string (cdr x)))]
	  [(null? x) ")"]
	  [else (string-append " . " (str x) ")")]))
