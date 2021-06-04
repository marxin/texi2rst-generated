  .. _extends_type_of:

EXTENDS_TYPE_OF ---  Query dynamic type for extension
*****************************************************

.. index:: EXTENDS_TYPE_OF

:samp:`{Description}:`
  Query dynamic type for extension.

:samp:`{Standard}:`
  Fortran 2003 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = EXTENDS_TYPE_OF(A, MOLD)``

:samp:`{Arguments}:`
  ==============  =================================================
  :samp:`{A}`     Shall be an object of extensible declared type or
                  unlimited polymorphic.
  :samp:`{MOLD}`  Shall be an object of extensible declared type or
                  unlimited polymorphic.
  ==============  =================================================

:samp:`{Return value}:`
  The return value is a scalar of type default logical. It is true if and only if
  the dynamic type of A is an extension type of the dynamic type of MOLD.

:samp:`{See also}:`
  SAME_TYPE_AS

