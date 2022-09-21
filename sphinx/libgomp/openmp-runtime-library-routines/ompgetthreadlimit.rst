..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

  .. _omp_get_thread_limit:

omp_get_thread_limit -- Maximum number of threads
*************************************************

:samp:`{Description}:`
  Return the maximum number of threads of the program.

:samp:`{C/C++}:`

  .. list-table::

     * - *Prototype*:
       - ``int omp_get_thread_limit(void);``

:samp:`{Fortran}:`

  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_thread_limit()``

:samp:`{See also}:`
  :ref:`omp_get_max_threads`, :ref:`OMP_THREAD_LIMIT`

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.14.

