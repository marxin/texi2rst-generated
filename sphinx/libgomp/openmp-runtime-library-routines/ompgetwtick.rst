..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_get_wtick:

omp_get_wtick -- Get timer precision
************************************

:samp:`{Description}:`

  Gets the timer precision, i.e., the number of seconds between two 
  successive clock ticks.

:samp:`{C/C++}:`

  .. list-table::

     * - *Prototype*:
       - ``double omp_get_wtick(void);``

:samp:`{Fortran}:`

  .. list-table::

     * - *Interface*:
       - ``double precision function omp_get_wtick()``

:samp:`{See also}:`

  :ref:`omp_get_wtime`

:samp:`{Reference}:`

  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.4.2.
