  .. _random_seed:

RANDOM_SEED --- Initialize a pseudo-random number sequence
**********************************************************

.. index:: RANDOM_SEED

.. index:: random number generation, seeding

.. index:: seeding a random number generator

:samp:`{Description}:`
  Restarts or queries the state of the pseudorandom number generator used by 
  ``RANDOM_NUMBER``.

  If ``RANDOM_SEED`` is called without arguments, it is seeded with
  random data retrieved from the operating system.

  As an extension to the Fortran standard, the GFortran
  ``RANDOM_NUMBER`` supports multiple threads. Each thread in a
  multi-threaded program has its own seed.  When ``RANDOM_SEED`` is
  called either without arguments or with the :samp:`{PUT}` argument, the
  given seed is copied into a master seed as well as the seed of the
  current thread. When a new thread uses ``RANDOM_NUMBER`` for the
  first time, the seed is copied from the master seed, and forwarded
  N * 2^{128} steps to guarantee that the random stream does not
  alias any other stream in the system, where :samp:`{N}` is the number of
  threads that have used ``RANDOM_NUMBER`` so far during the program
  execution.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL RANDOM_SEED([SIZE, PUT, GET])``

:samp:`{Arguments}:`
  ==============  ======================================================================
  :samp:`{SIZE}`  (Optional) Shall be a scalar and of type default 
                  ``INTEGER``, with ``INTENT(OUT)``. It specifies the minimum size 
                  of the arrays used with the :samp:`{PUT}` and :samp:`{GET}` arguments.
  ==============  ======================================================================
  :samp:`{PUT}`   (Optional) Shall be an array of type default 
                  ``INTEGER`` and rank one. It is ``INTENT(IN)`` and the size of 
                  the array must be larger than or equal to the number returned by the 
                  :samp:`{SIZE}` argument.
  :samp:`{GET}`   (Optional) Shall be an array of type default 
                  ``INTEGER`` and rank one. It is ``INTENT(OUT)`` and the size 
                  of the array must be larger than or equal to the number returned by 
                  the :samp:`{SIZE}` argument.
  ==============  ======================================================================

:samp:`{Example}:`

  .. code-block:: fortran

    program test_random_seed
      implicit none
      integer, allocatable :: seed(:)
      integer :: n

      call random_seed(size = n)
      allocate(seed(n))
      call random_seed(get=seed)
      write (*, *) seed
    end program test_random_seed

:samp:`{See also}:`
  RANDOM_NUMBER, 
  RANDOM_INIT

