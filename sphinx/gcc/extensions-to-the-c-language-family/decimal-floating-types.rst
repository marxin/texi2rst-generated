.. _decimal-float:

Decimal Floating Types
**********************

.. index:: decimal floating types

.. index:: _Decimal32 data type

.. index:: _Decimal64 data type

.. index:: _Decimal128 data type

.. index:: df integer suffix

.. index:: dd integer suffix

.. index:: dl integer suffix

.. index:: DF integer suffix

.. index:: DD integer suffix

.. index:: DL integer suffix

As an extension, GNU C supports decimal floating types as
defined in the N1312 draft of ISO/IEC WDTR24732.  Support for decimal
floating types in GCC will evolve as the draft technical report changes.
Calling conventions for any target might also change.  Not all targets
support decimal floating types.

The decimal floating types are ``_Decimal32``, ``_Decimal64``, and
``_Decimal128``.  They use a radix of ten, unlike the floating types
``float``, ``double``, and ``long double`` whose radix is not
specified by the C standard but is usually two.

Support for decimal floating types includes the arithmetic operators
add, subtract, multiply, divide; unary arithmetic operators;
relational operators; equality operators; and conversions to and from
integer and other floating types.  Use a suffix :samp:`df` or
:samp:`DF` in a literal constant of type ``_Decimal32``, :samp:`dd`
or :samp:`DD` for ``_Decimal64``, and :samp:`dl` or :samp:`DL` for
``_Decimal128``.

GCC support of decimal float as specified by the draft technical report
is incomplete:

* When the value of a decimal floating type cannot be represented in the
  integer type to which it is being converted, the result is undefined
  rather than the result value specified by the draft technical report.

* GCC does not provide the C library functionality associated with
  math.h, fenv.h, stdio.h, stdlib.h, and
  wchar.h, which must come from a separate C library implementation.
  Because of this the GNU C compiler does not define macro
  ``__STDC_DEC_FP__`` to indicate that the implementation conforms to
  the technical report.

Types ``_Decimal32``, ``_Decimal64``, and ``_Decimal128``
are supported by the DWARF debug information format.
