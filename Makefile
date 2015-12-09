# file: Makefile
# author: Justin Dawson (JDawson@ku.edu)

all: SerialMandelbrot.hs ForkIOMandelbrot.hs
	ghc  -O2 -threaded SerialMandelbrot
	ghc  -O2 -threaded ForkIOMandelbrot 
	ghc -O2 -threaded ParMandelbrot -rtsopts -eventlog
	ghc -O2 -threaded RepaMandelbrot -rtsopts -eventlog
	ghc -O2 -threaded RepaSerialMandelbrot -rtsopts -eventlog



.PHONY:
clean: 
	rm SerialMandelbrot ForkIOMandelbrot ParMandelbrot RepaMandelbrot RepaSerialMandelbrot *.o *.hi
