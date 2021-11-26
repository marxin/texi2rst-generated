..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_num_teams:

OMP_NUM_TEAMS -- Specifies the number of teams to use by teams region
*********************************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Specifies the upper bound for number of teams to use in teams regions
  without explicit ``num_teams`` clause.  The value of this variable shall
  be a positive integer.  If undefined it defaults to 0 which means
  implementation defined upper bound.

:samp:`{See also}:`
  :ref:`omp_set_num_teams`

:samp:`{Reference}:`
  `OpenMP specification v5.1 <https://www.openmp.org>`_, Section 6.23
