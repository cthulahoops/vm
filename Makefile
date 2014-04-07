PROGS=Run LispC Repl
VMMODS=Vm.hs Memory.hs Stack.hs Marks.hs VmCode.hs VmTypes.hs
CCMODS=Compile.hs Parse.hs SExprs.hs

all: ${PROGS}

test: LispC Run Repl
	./run_tests.sh

clean:
	-rm *.o *.hi ${PROGS}

LispC: LispC.hs ${CCMODS}
	ghc --make -O2 -o LispC LispC.hs

Run: Run.hs ${VMMODS}
	ghc --make -O2 -o Run Run.hs

Repl: Repl.hs ${VMMODS} ${CCMODS}
	ghc --make -O2 -o Repl Repl.hs
