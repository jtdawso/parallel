all: SerialMandelbrot.hs ForkIOMandelbrot.hs
	ghc -O2 -threaded SerialMandelbrot
	ghc -O2 -threaded ForkIOMandelbrot 
#	ghc -threaded ParMandelbrot -rtsopts -eventlog

SerialMandelbrot.o: all

run: SerialMandelbrot.o
	./SerialMandelbrot

.PHONY:
clean: 
	rm SerialMandelbrot ForkIOMandelbrot ParMandelbrot *.o *.hi
