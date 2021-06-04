  .. _int8:

INT8 --- Convert to 64-bit integer type
***************************************

.. index:: INT8

.. index:: conversion, to integer

:samp:`{Description}:`
  Convert to a ``KIND=8`` integer type. This is equivalent to the
  standard ``INT`` intrinsic with an optional argument of
  ``KIND=8``, and is only included for backwards compatibility.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = INT8(A)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{A}`  Shall be of type ``INTEGER``,
               ``REAL``, or ``COMPLEX``.
  ===========  =============================

:samp:`{Return value}:`
  The return value is a ``INTEGER(8)`` variable.

:samp:`{See also}:`
  INT, 
  INT2, 
  LONG

