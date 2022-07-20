..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_max_active_levels:

omp_get_max_active_levels -- Current maximum number of active regions
*********************************************************************

:samp:`{Description}:`
  This function obtains the maximum allowed number of nested, active parallel regions.

C/C++

  .. list-table::

     * - *Prototype*:
       - ``int omp_get_max_active_levels(void);``

:samp:`{Fortran}:`

  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_max_active_levels()``

:samp:`{See also}:`
  :ref:`omp_set_max_active_levels`, :ref:`omp_get_active_level`

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.16.

