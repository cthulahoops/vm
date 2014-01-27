PROGS=Run LispC Repl

all: ${PROGS}

clean:
	-rm *.o *.hi ${PROGS}

LispC: LispC.hs Assemble.hs Compile.hs Vm.hs Memory.hs
	ghc --make -o LispC LispC.hs

Run: Run.hs Assemble.hs Vm.hs Memory.hs
	ghc --make -o Run Run.hs

Repl: Repl.hs Assemble.hs Compile.hs Vm.hs Memory.hs
	ghc --make -o Repl Repl.hs
