..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_get_wtime:

omp_get_wtime -- Elapsed wall clock time
****************************************

Description:
  Elapsed wall clock time in seconds.  The time is measured per thread, no
  guarantee can be made that two distinct threads measure the same time.
  Time is measured from some "time in the past", which is an arbitrary time
  guaranteed not to change during the execution of the program.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``double omp_get_wtime(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``double precision function omp_get_wtime()``

See also:
  :ref:`omp_get_wtick`

Reference:
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.4.1.
