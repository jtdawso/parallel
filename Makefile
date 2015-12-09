all: SerialMandelbrot.hs ForkIOMandelbrot.hs
	ghc  -O2 -threaded SerialMandelbrot
	ghc  -O2 -threaded ForkIOMandelbrot 
	ghc -O2 -threaded ParMandelbrot -rtsopts -eventlog
	ghc -O2 -threaded RepaMandelbrot -rtsopts -eventlog

SerialMandelbrot.o: all

run: SerialMandelbrot.o
	./SerialMandelbrot 


.PHONY:
clean: 
	rm SerialMandelbrot ForkIOMandelbrot ParMandelbrot RepaMandelbrot *.o *.hi
