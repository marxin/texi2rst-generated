..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_team_size:

omp_get_team_size -- Number of threads in a team
************************************************

:samp:`{Description}:`
  This function returns the number of threads in a thread team to which
  either the current thread or its ancestor belongs.  For values of :samp:`{level}`
  outside zero to ``omp_get_level``, -1 is returned; if :samp:`{level}` is zero,
  1 is returned, and for ``omp_get_level``, the result is identical
  to ``omp_get_num_threads``.

:samp:`{C/C++}:`

  .. list-table::

     * - *Prototype*:
       - ``int omp_get_team_size(int level);``

:samp:`{Fortran}:`

  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_team_size(level)``
     * -
       - ``integer level``

:samp:`{See also}:`
  :ref:`omp_get_num_threads`, :ref:`omp_get_level`, :ref:`omp_get_ancestor_thread_num`

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.19.

