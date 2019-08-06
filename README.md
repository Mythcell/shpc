# shpc

This repository contains a selection of useful code I wrote for the two postgraduate units SHPC4001 and SHPC4002 that I studied at
The University of Western Australia in 2018.  The code is mostly taken from the assignments and projects I did across the two units.
As of 2019, SHPC4001 is no longer taught in Fortran (replaced with Python) and SHPC4002 is also undergoing changes.

### The Code

**halo** contains code from my semester-long computational astronomy project in SHPC4002.  The idea is to approximate the
local density of a simulated NFW black hole (based on output from a GADGET simulation) through a nearest-neighbour calculation
using a cubic spline kernel.  The main aims were to parallelise the code using a mix of OpenMP/MPI and see how the running time
scales with the simulation resolution (or rather the number of particles).

**numerics** contains code from the first couple of workshops in SHPC4001 that largely revolved around using the Euler method (and others)
to solve differential equations, eventually moving onto methods for root finding.  Strangely, the course omitted higher order methods
like RK4.

**quantum** followed the numerics workshops and contains code for several Fortran assignments in which we had to, one way or another,
solve the Schrödinger Equation for a given potential V(x) and plot the corresponding wavefunctions.

**optimisation** saw a brief foray into more traditional methods of mathematical optimisation.  We were given the freedom to choose
which language to use; I went with Java mainly since I needed some OOP in my life.

**graphs** contains my code for a week-long assignment on graph theory, or complex networks, that involved constructing 3 main graph
models (Erdos-Renyi, Barabasi-Albert, Watts-Strogatz) and comparing metrics (degree distribution, shortest paths, etc).

**misc** contains some test code for OpenMP and MPI. It also contains code for Conway's Game of Life that was one of
the assignments set in SHPC4002's sister unit CITS3402.
