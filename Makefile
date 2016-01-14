# file: Makefile
# author: Justin Dawson (JDawson@ku.edu)

all: SerialMandelbrot.hs ForkIOMandelbrot.hs ParMandelbrot.hs RepaMandelbrot.hs RepaSerialMandelbrot.hs CudaMandelbrot.hs
	ghc  -O2 -threaded SerialMandelbrot
	ghc  -O2 -threaded ForkIOMandelbrot 
	ghc -O2 -threaded ParMandelbrot -rtsopts -eventlog
	ghc -O2 -threaded RepaMandelbrot -rtsopts -eventlog
	ghc -O2 -threaded RepaSerialMandelbrot -rtsopts -eventlog
	ghc -O2 -threaded CudaMandelbrot 



.PHONY:
clean: 
	rm -f SerialMandelbrot ForkIOMandelbrot ParMandelbrot RepaMandelbrot RepaSerialMandelbrot CudaMandelbrot *.o *.hi
