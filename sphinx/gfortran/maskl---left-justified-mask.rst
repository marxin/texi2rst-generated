  .. _maskl:

MASKL - Left justified mask
***************************

.. index:: MASKL

.. index:: mask, left justified

:samp:`{Description}:`
  ``MASKL(I[, KIND])`` has its leftmost :samp:`{I}` bits set to 1, and the
  remaining bits set to 0.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = MASKL(I[, KIND])``

:samp:`{Arguments}:`
  ==============  =============================================
  :samp:`{I}`     Shall be of type ``INTEGER``.
  ==============  =============================================
  :samp:`{KIND}`  Shall be a scalar constant expression of type
                  ``INTEGER``.
  ==============  =============================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER``. If :samp:`{KIND}` is present, it
  specifies the kind value of the return type; otherwise, it is of the
  default integer kind.

:samp:`{See also}:`
  MASKR

