  .. _int2:

``INT2`` - Convert to 16-bit integer type
*****************************************

.. index:: INT2

.. index:: SHORT

.. index:: conversion, to integer

:samp:`{Description}:`
  Convert to a ``KIND=2`` integer type. This is equivalent to the
  standard ``INT`` intrinsic with an optional argument of
  ``KIND=2``, and is only included for backwards compatibility.

  The ``SHORT`` intrinsic is equivalent to ``INT2``.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = INT2(A)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{A}`  Shall be of type ``INTEGER``,
               ``REAL``, or ``COMPLEX``.
  ===========  =============================
  ===========  =============================

:samp:`{Return value}:`
  The return value is a ``INTEGER(2)`` variable.

:samp:`{See also}:`
  INT, 
  INT8, 
  LONG

