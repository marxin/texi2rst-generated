.. _same_type_as:

SAME_TYPE_AS ---  Query dynamic types for equality
**************************************************

.. index:: SAME_TYPE_AS

.. function:: SAME_TYPE_AS

  Query dynamic types for equality.

  :param A:
    Shall be an object of extensible declared type or
    unlimited polymorphic.

  :param B:
    Shall be an object of extensible declared type or
    unlimited polymorphic.

  :return:
    The return value is a scalar of type default logical. It is true if and
    only if the dynamic type of A is the same as the dynamic type of B.

  :samp:`{Standard}:`
    Fortran 2003 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`
    ``RESULT = SAME_TYPE_AS(A, B)``

  :samp:`{See also}:`
    EXTENDS_TYPE_OF

