This is an experimental virtual machine and compiler that I've been writing for
the sole purpose of learning about virtual machines and compilers. The virtual
machine is a simple stack based machine, with two stacks, key-value memory
frames and a very slow mark and sweep garbage collector.

The compiler compiles Scheme for this machine.

The whole lot is written Haskell, with an increasing amount of scheme library
and test code.

You can build using make and start a simple Repl with

$ ./Repl
>>> (define (square x) (* x x))
()
>>> (square -9)
81
>>> (let [(x 3)] (+ (square x) (* 3 x) 17))
35
>>> (define (fac n) (if (= 1 n) 1 (* n (fac (- n 1)))))
()
>>> (fac 10)
3628800
>>> (fac 100)
933262154439441526816992388562667004907159682643816214685929638952175999\
932299156089414639761565182862536979208272237582511852109168640000000000\
00000000000000

The compiler is LispC. (Using tail to remove all the library functions.)

$ echo "(write (* 8 9)) (newline)" | ./LispC | tail -2
& nil & nil 9 , 8 , :* ! ` jmp flip $ , :write ! ` jmp flip $
& nil :newline ! ` jmp flip $

And you can use Run to run the resulting code on the vm.

$ ./Run /tmp/example 
72