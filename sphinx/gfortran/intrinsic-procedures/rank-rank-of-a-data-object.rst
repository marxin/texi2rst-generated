.. _rank:

RANK --- Rank of a data object
******************************

.. index:: RANK

.. index:: rank

.. function:: RANK(A)

  ``RANK(A)`` returns the rank of a scalar or array data object.

  :param A:
    can be of any type

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind. For arrays, their rank is returned; for scalars zero is returned.

  :samp:`{Standard}:`
    Technical Specification (TS) 29113

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = RANK(A)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_rank
        integer :: a
        real, allocatable :: b(:,:)

        print *, rank(a), rank(b) ! Prints:  0  2
      end program test_rank

