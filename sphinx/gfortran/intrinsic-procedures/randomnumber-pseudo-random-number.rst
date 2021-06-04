  .. _random_number:

RANDOM_NUMBER --- Pseudo-random number
**************************************

.. index:: RANDOM_NUMBER

.. index:: random number generation

:samp:`{Description}:`
  Returns a single pseudorandom number or an array of pseudorandom numbers
  from the uniform distribution over the range 0 \leq x < 1.

  The runtime-library implements the xoshiro256** pseudorandom number
  generator (PRNG). This generator has a period of 2^{256} - 1,
  and when using multiple threads up to 2^{128} threads can each
  generate 2^{128} random numbers before any aliasing occurs.

  Note that in a multi-threaded program (e.g. using OpenMP directives),
  each thread will have its own random number state. For details of the
  seeding procedure, see the documentation for the ``RANDOM_SEED``
  intrinsic.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL RANDOM_NUMBER(HARVEST)``

:samp:`{Arguments}:`
  =================  ===============================================
  :samp:`{HARVEST}`  Shall be a scalar or an array of type ``REAL``.
  =================  ===============================================
  =================  ===============================================

:samp:`{Example}:`

  .. code-block:: fortran

    program test_random_number
      REAL :: r(5,5)
      CALL RANDOM_NUMBER(r)
    end program

:samp:`{See also}:`
  RANDOM_SEED, 
  RANDOM_INIT

