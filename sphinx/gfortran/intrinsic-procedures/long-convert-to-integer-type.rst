  .. _long:

LONG - Convert to integer type
******************************

.. index:: LONG

.. index:: conversion, to integer

:samp:`{Description}:`
  Convert to a ``KIND=4`` integer type, which is the same size as a C
  ``long`` integer.  This is equivalent to the standard ``INT``
  intrinsic with an optional argument of ``KIND=4``, and is only
  included for backwards compatibility.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = LONG(A)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{A}`  Shall be of type ``INTEGER``,
               ``REAL``, or ``COMPLEX``.
  ===========  =============================
  ===========  =============================

:samp:`{Return value}:`
  The return value is a ``INTEGER(4)`` variable.

:samp:`{See also}:`
  INT, 
  INT2, 
  INT8

