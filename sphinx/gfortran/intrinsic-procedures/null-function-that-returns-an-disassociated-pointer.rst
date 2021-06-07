.. _null:

NULL --- Function that returns an disassociated pointer
*******************************************************

.. index:: NULL

.. index:: pointer, status

.. index:: pointer, disassociated

.. function:: NULL

  Returns a disassociated pointer.

  :param MOLD:
    (Optional) shall be a pointer of any association
    status and of any type.

  :return:
    A disassociated pointer.

  :samp:`{Standard}:`
    Fortran 95 and later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`
    ``PTR => NULL([MOLD])``

  :samp:`{Example}:`

    .. code-block:: fortran

      REAL, POINTER, DIMENSION(:) :: VEC => NULL ()

  :samp:`{See also}:`
    ASSOCIATED

