PROGS=Run LispC Repl
VMMODS=Vm.hs Memory.hs Stack.hs Marks.hs VmCode.hs VmTypes.hs
CCMODS=Compile.hs Parse.hs SExprs.hs

all: ${PROGS}

clean:
	-rm *.o *.hi ${PROGS}

LispC: LispC.hs ${VMMODS} ${CCMODS}
	ghc --make -o LispC LispC.hs

Run: Run.hs ${VMMODS}
	ghc --make -o Run Run.hs

Repl: Repl.hs ${VMMODS} ${CCMODS}
	ghc --make -o Repl Repl.hs
