  .. _irand:

IRAND --- Integer pseudo-random number
**************************************

.. index:: IRAND

.. index:: random number generation

:samp:`{Description}:`
  ``IRAND(FLAG)`` returns a pseudo-random number from a uniform
  distribution between 0 and a system-dependent limit (which is in most
  cases 2147483647). If :samp:`{FLAG}` is 0, the next number
  in the current sequence is returned; if :samp:`{FLAG}` is 1, the generator
  is restarted by ``CALL SRAND(0)`` ; if :samp:`{FLAG}` has any other value,
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
  ``RESULT = IRAND(I)``

:samp:`{Arguments}:`
  ===========  ========================================
  :samp:`{I}`  Shall be a scalar ``INTEGER`` of kind 4.
  ===========  ========================================
  ===========  ========================================

:samp:`{Return value}:`
  The return value is of ``INTEGER(kind=4)`` type.

:samp:`{Example}:`

  .. code-block:: c++

    program test_irand
      integer,parameter :: seed = 86456

      call srand(seed)
      print *, irand(), irand(), irand(), irand()
      print *, irand(seed), irand(), irand(), irand()
    end program test_irand

