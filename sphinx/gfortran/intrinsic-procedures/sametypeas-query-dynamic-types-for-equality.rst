  .. _same_type_as:

SAME_TYPE_AS ---  Query dynamic types for equality
**************************************************

.. index:: SAME_TYPE_AS

:samp:`{Description}:`
  Query dynamic types for equality.

:samp:`{Standard}:`
  Fortran 2003 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = SAME_TYPE_AS(A, B)``

:samp:`{Arguments}:`
  ===========  =================================================
  :samp:`{A}`  Shall be an object of extensible declared type or
               unlimited polymorphic.
  ===========  =================================================
  :samp:`{B}`  Shall be an object of extensible declared type or
               unlimited polymorphic.
  ===========  =================================================

:samp:`{Return value}:`
  The return value is a scalar of type default logical. It is true if and
  only if the dynamic type of A is the same as the dynamic type of B.

:samp:`{See also}:`
  EXTENDS_TYPE_OF

