PROGS=Run LispC

all: ${PROGS}

clean:
	-rm *.o *.hi ${PROGS}

LispC: LispC.hs Assemble.hs Compile.hs Vm.hs
	ghc --make -o LispC LispC.hs

Run: Run.hs Assemble.hs Vm.hs
	ghc --make -o Run Run.hs
