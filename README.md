#Mandelbrot Set

This repo contains different implementations of the Mandelbrot Set:

* Serial Implementation- SerialMandelbrot
* ForkIO Implementation: work is manually split in 4 and sent to 4 threads - ForkIOMandelbrot
* ParMonad Implementation (Control.Parallel.Strategies) - ParMandelbrot
* Repa Array Serial Implementation- RepaMandelbrot
* Repa Array Parallel Implementation- RepaSerialMandelbrot



Each of these implementations use BlankCanvas running on port 3000.
The ParMonad and Repa examples can be run with runtime options to specify
the number of cores and if you would like an eventlog.

After running the make command, you can run the executable then open a web
browser to localhost:3000


some example runtimes:

* Serial- 15.9 s
* ForkIO- 14.5 s
* ParMonad 2 cores -16.5 s
* Repa Serial - 15.5 s
* Repa Parallel 4 cores - 14.9 s


