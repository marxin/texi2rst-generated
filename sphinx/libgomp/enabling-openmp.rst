.. _enabling-openmp:

Enabling OpenMP
---------------

To activate the OpenMP extensions for C/C++ and Fortran, the compile-time 
flag :command:`-fopenmp` must be specified.  This enables the OpenMP directive
``#pragma omp`` in C/C++ and ``!$omp`` directives in free form, 
``c$omp``, ``*$omp`` and ``!$omp`` directives in fixed form, 
``!$`` conditional compilation sentinels in free form and ``c$``,
``*$`` and ``!$`` sentinels in fixed form, for Fortran.  The flag also
arranges for automatic linking of the OpenMP runtime library 
(Runtime Library Routines).

A complete description of all OpenMP directives accepted may be found in 
the https://www.openmp.orgOpenMP Application Program Interface manual,
version 4.5.

.. -
   OpenMP Runtime Library Routines
   -

