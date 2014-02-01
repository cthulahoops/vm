PROGS=Run LispC Repl
VMMODS=Assemble.hs Vm.hs Memory.hs Stack.hs Marks.hs

all: ${PROGS}

clean:
	-rm *.o *.hi ${PROGS}

LispC: LispC.hs ${VMMODS}
	ghc --make -o LispC LispC.hs

Run: Run.hs ${VMMODS}
	ghc --make -o Run Run.hs

Repl: Repl.hs ${VMMODS}
	ghc --make -o Repl Repl.hs
