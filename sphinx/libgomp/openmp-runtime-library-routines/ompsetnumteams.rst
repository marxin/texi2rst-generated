..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_set_num_teams:

omp_set_num_teams -- Set upper teams limit for teams construct
**************************************************************

:samp:`{Description}:`
  Specifies the upper bound for number of teams created by the teams construct
  which does not specify a ``num_teams`` clause.  The
  argument of ``omp_set_num_teams`` shall be a positive integer.

:samp:`{C/C++}:`

  ============  ==========================================
  *Prototype*:  ``void omp_set_num_teams(int num_teams);``
  ============  ==========================================

:samp:`{Fortran}:`

  ============  ===========================================
  *Interface*:  ``subroutine omp_set_num_teams(num_teams)``
                ``integer, intent(in) :: num_teams``
  ============  ===========================================

:samp:`{See also}:`
  :ref:`OMP_NUM_TEAMS`, :ref:`omp_get_num_teams`, :ref:`omp_get_max_teams`

:samp:`{Reference}:`
  `OpenMP specification v5.1 <https://www.openmp.org>`_, Section 3.4.3.

