.. _decimal-float-library-routines:

Routines for decimal floating point emulation
*********************************************

.. index:: decimal float library

.. index:: IEEE 754-2008

The software decimal floating point library implements IEEE 754-2008
decimal floating point arithmetic and is only activated on selected
targets.

The software decimal floating point library supports either DPD
(Densely Packed Decimal) or BID (Binary Integer Decimal) encoding
as selected at configure time.

Arithmetic functions
^^^^^^^^^^^^^^^^^^^^

.. function:: _Decimal32 __dpd_addsd3(_Decimal32 a,_Decimal32 b)

  These functions return the sum of :samp:`{a}` and :samp:`{b}`.

.. function:: _Decimal32 __dpd_subsd3(_Decimal32 a,_Decimal32 b)

  These functions return the difference between :samp:`{b}` and :samp:`{a}` ;
  that is, :samp:`{a}` - :samp:`{b}`.

.. function:: _Decimal32 __dpd_mulsd3(_Decimal32 a,_Decimal32 b)

  These functions return the product of :samp:`{a}` and :samp:`{b}`.

.. function:: _Decimal32 __dpd_divsd3(_Decimal32 a,_Decimal32 b)

  These functions return the quotient of :samp:`{a}` and :samp:`{b}` ; that is,
  :samp:`{a}` / :samp:`{b}`.

.. function:: _Decimal32 __dpd_negsd2(_Decimal32 a)

  These functions return the negation of :samp:`{a}`.  They simply flip the
  sign bit, so they can produce negative zero and negative NaN.

Conversion functions
^^^^^^^^^^^^^^^^^^^^

.. function:: _Decimal64 __dpd_extendsddd2(_Decimal32 a)

  These functions convert the value :samp:`{a}` from one decimal floating type
  to another.

.. function:: _Decimal64 __dpd_extendsfdd(float a)

  These functions convert the value of :samp:`{a}` from a binary floating type
  to a decimal floating type of a different size.

.. function:: float __dpd_truncddsf(_Decimal64 a)

  These functions convert the value of :samp:`{a}` from a decimal floating type
  to a binary floating type of a different size.

.. function:: _Decimal32 __dpd_extendsfsd(float a)

  These functions convert the value of :samp:`{a}` between decimal and
  binary floating types of the same size.

.. function:: int __dpd_fixsdsi(_Decimal32 a)

  These functions convert :samp:`{a}` to a signed integer.

.. function:: long __dpd_fixsddi(_Decimal32 a)

  These functions convert :samp:`{a}` to a signed long.

.. function:: unsigned int __dpd_fixunssdsi(_Decimal32 a)

  These functions convert :samp:`{a}` to an unsigned integer.  Negative values all become zero.

.. function:: unsigned long __dpd_fixunssddi(_Decimal32 a)

  These functions convert :samp:`{a}` to an unsigned long.  Negative values
  all become zero.

.. function:: _Decimal32 __dpd_floatsisd(int i)

  These functions convert :samp:`{i}` , a signed integer, to decimal floating point.

.. function:: _Decimal32 __dpd_floatdisd(long i)

  These functions convert :samp:`{i}` , a signed long, to decimal floating point.

.. function:: _Decimal32 __dpd_floatunssisd(unsigned inti)

  These functions convert :samp:`{i}` , an unsigned integer, to decimal floating point.

.. function:: _Decimal32 __dpd_floatunsdisd(unsigned longi)

  These functions convert :samp:`{i}` , an unsigned long, to decimal floating point.

Comparison functions
^^^^^^^^^^^^^^^^^^^^

.. function:: int __dpd_unordsd2(_Decimal32 a,_Decimal32 b)

  These functions return a nonzero value if either argument is NaN, otherwise 0.

There is also a complete group of higher level functions which
correspond directly to comparison operators.  They implement the ISO C
semantics for floating-point comparisons, taking NaN into account.
Pay careful attention to the return values defined for each set.
Under the hood, all of these routines are implemented as

.. code-block:: c++

    if (__bid_unordXd2 (a, b))
      return E;
    return __bid_cmpXd2 (a, b);

where :samp:`{E}` is a constant chosen to give the proper behavior for
NaN.  Thus, the meaning of the return value is different for each set.
Do not rely on this implementation; only the semantics documented
below are guaranteed.

.. function:: int __dpd_eqsd2(_Decimal32 a,_Decimal32 b)

  These functions return zero if neither argument is NaN, and :samp:`{a}` and
  :samp:`{b}` are equal.

.. function:: int __dpd_nesd2(_Decimal32 a,_Decimal32 b)

  These functions return a nonzero value if either argument is NaN, or
  if :samp:`{a}` and :samp:`{b}` are unequal.

.. function:: int __dpd_gesd2(_Decimal32 a,_Decimal32 b)

  These functions return a value greater than or equal to zero if
  neither argument is NaN, and :samp:`{a}` is greater than or equal to
  :samp:`{b}`.

.. function:: int __dpd_ltsd2(_Decimal32 a,_Decimal32 b)

  These functions return a value less than zero if neither argument is
  NaN, and :samp:`{a}` is strictly less than :samp:`{b}`.

.. function:: int __dpd_lesd2(_Decimal32 a,_Decimal32 b)

  These functions return a value less than or equal to zero if neither
  argument is NaN, and :samp:`{a}` is less than or equal to :samp:`{b}`.

.. function:: int __dpd_gtsd2(_Decimal32 a,_Decimal32 b)

  These functions return a value greater than zero if neither argument
  is NaN, and :samp:`{a}` is strictly greater than :samp:`{b}`.

