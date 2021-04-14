  .. _rand:

``RAND`` - Real pseudo-random number
************************************

.. index:: RAND

.. index:: random number generation

:samp:`{Description}:`
  ``RAND(FLAG)`` returns a pseudo-random number from a uniform
  distribution between 0 and 1. If :samp:`{FLAG}` is 0, the next number
  in the current sequence is returned; if :samp:`{FLAG}` is 1, the generator
  is restarted by ``CALL SRAND(0)``; if :samp:`{FLAG}` has any other value,
  it is used as a new seed with ``SRAND``.

  This intrinsic routine is provided for backwards compatibility with
  GNU Fortran 77. It implements a simple modulo generator as provided 
  by :command:`g77`. For new code, one should consider the use of 
  RANDOM_NUMBER as it implements a superior algorithm.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = RAND(I)``

:samp:`{Arguments}:`
  ===========  ========================================
  :samp:`{I}`  Shall be a scalar ``INTEGER`` of kind 4.
  ===========  ========================================
  ===========  ========================================

:samp:`{Return value}:`
  The return value is of ``REAL`` type and the default kind.

:samp:`{Example}:`

  .. code-block:: c++

    program test_rand
      integer,parameter :: seed = 86456

      call srand(seed)
      print *, rand(), rand(), rand(), rand()
      print *, rand(seed), rand(), rand(), rand()
    end program test_rand

:samp:`{See also}:`
  SRAND, 
  RANDOM_NUMBER

