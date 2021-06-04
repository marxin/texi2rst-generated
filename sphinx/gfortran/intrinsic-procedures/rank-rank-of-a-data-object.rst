  .. _rank:

RANK --- Rank of a data object
******************************

.. index:: RANK

.. index:: rank

:samp:`{Description}:`
  ``RANK(A)`` returns the rank of a scalar or array data object.

:samp:`{Standard}:`
  Technical Specification (TS) 29113

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = RANK(A)``

:samp:`{Arguments}:`
  ===========  ==================
  :samp:`{A}`  can be of any type
  ===========  ==================
  ===========  ==================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the default integer
  kind. For arrays, their rank is returned; for scalars zero is returned.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_rank
      integer :: a
      real, allocatable :: b(:,:)

      print *, rank(a), rank(b) ! Prints:  0  2
    end program test_rank

