.. _maskl:

MASKL --- Left justified mask
*****************************

.. index:: MASKL

.. index:: mask, left justified

.. function:: MASKL(I[, KIND])

  ``MASKL(I[, KIND])`` has its leftmost :samp:`{I}` bits set to 1, and the
  remaining bits set to 0.

  :param I:
    Shall be of type ``INTEGER``.

  :param KIND:
    Shall be a scalar constant expression of type
    ``INTEGER``.

  :return:
    The return value is of type ``INTEGER``. If :samp:`{KIND}` is present, it
    specifies the kind value of the return type; otherwise, it is of the
    default integer kind.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`
    ``RESULT = MASKL(I[, KIND])``

  :samp:`{See also}:`
    MASKR

