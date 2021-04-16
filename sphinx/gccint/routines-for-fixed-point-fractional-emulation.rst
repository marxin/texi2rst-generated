.. _fixed-point-fractional-library-routines:

Routines for fixed-point fractional emulation
*********************************************

.. index:: fixed-point fractional library

.. index:: fractional types

.. index:: Embedded C

The software fixed-point library implements fixed-point fractional
arithmetic, and is only activated on selected targets.

For ease of comprehension ``fract`` is an alias for the
``_Fract`` type, ``accum`` an alias for ``_Accum``, and
``sat`` an alias for ``_Sat``.

For illustrative purposes, in this section the fixed-point fractional type
``short fract`` is assumed to correspond to machine mode ``QQmode``;
``unsigned short fract`` to ``UQQmode``;
``fract`` to ``HQmode``;
``unsigned fract`` to ``UHQmode``;
``long fract`` to ``SQmode``;
``unsigned long fract`` to ``USQmode``;
``long long fract`` to ``DQmode``;
and ``unsigned long long fract`` to ``UDQmode``.
Similarly the fixed-point accumulator type
``short accum`` corresponds to ``HAmode``;
``unsigned short accum`` to ``UHAmode``;
``accum`` to ``SAmode``;
``unsigned accum`` to ``USAmode``;
``long accum`` to ``DAmode``;
``unsigned long accum`` to ``UDAmode``;
``long long accum`` to ``TAmode``;
and ``unsigned long long accum`` to ``UTAmode``.

Arithmetic functions
^^^^^^^^^^^^^^^^^^^^

.. function:: short fract __addqq3(short fracta,short fractb)

  These functions return the sum of :samp:`{a}` and :samp:`{b}`.

.. function:: short fract __ssaddqq3(short fracta,short fractb)

  These functions return the sum of :samp:`{a}` and :samp:`{b}` with signed saturation.

.. function:: unsigned short fract __usadduqq3(unsigned shortfract a,unsigned shortfract b)

  These functions return the sum of :samp:`{a}` and :samp:`{b}` with unsigned saturation.

.. function:: short fract __subqq3(short fracta,short fractb)

  These functions return the difference of :samp:`{a}` and :samp:`{b}` ;
  that is, ``a - b``.

.. function:: short fract __sssubqq3(short fracta,short fractb)

  These functions return the difference of :samp:`{a}` and :samp:`{b}` with signed
  saturation;  that is, ``a - b``.

.. function:: unsigned short fract __ussubuqq3(unsigned shortfract a,unsigned shortfract b)

  These functions return the difference of :samp:`{a}` and :samp:`{b}` with unsigned
  saturation;  that is, ``a - b``.

.. function:: short fract __mulqq3(short fracta,short fractb)

  These functions return the product of :samp:`{a}` and :samp:`{b}`.

.. function:: short fract __ssmulqq3(short fracta,short fractb)

  These functions return the product of :samp:`{a}` and :samp:`{b}` with signed
  saturation.

.. function:: unsigned short fract __usmuluqq3(unsigned shortfract a,unsigned shortfract b)

  These functions return the product of :samp:`{a}` and :samp:`{b}` with unsigned
  saturation.

.. function:: short fract __divqq3(short fracta,short fractb)

  These functions return the quotient of the signed division of :samp:`{a}`
  and :samp:`{b}`.

.. function:: unsigned short fract __udivuqq3(unsigned shortfract a,unsigned shortfract b)

  These functions return the quotient of the unsigned division of :samp:`{a}`
  and :samp:`{b}`.

.. function:: short fract __ssdivqq3(short fracta,short fractb)

  These functions return the quotient of the signed division of :samp:`{a}`
  and :samp:`{b}` with signed saturation.

.. function:: unsigned short fract __usdivuqq3(unsigned shortfract a,unsigned shortfract b)

  These functions return the quotient of the unsigned division of :samp:`{a}`
  and :samp:`{b}` with unsigned saturation.

.. function:: short fract __negqq2(short fracta)

  These functions return the negation of :samp:`{a}`.

.. function:: short fract __ssnegqq2(short fracta)

  These functions return the negation of :samp:`{a}` with signed saturation.

.. function:: unsigned short fract __usneguqq2(unsigned shortfract a)

  These functions return the negation of :samp:`{a}` with unsigned saturation.

.. function:: short fract __ashlqq3(short fracta,int b)

  These functions return the result of shifting :samp:`{a}` left by :samp:`{b}` bits.

.. function:: short fract __ashrqq3(short fracta,int b)

  These functions return the result of arithmetically shifting :samp:`{a}` right
  by :samp:`{b}` bits.

.. function:: unsigned short fract __lshruqq3(unsigned shortfract a,int b)

  These functions return the result of logically shifting :samp:`{a}` right
  by :samp:`{b}` bits.

.. function:: fract __ssashlhq3(fract a,int b)

  These functions return the result of shifting :samp:`{a}` left by :samp:`{b}` bits
  with signed saturation.

.. function:: unsigned short fract __usashluqq3(unsigned shortfract a,int b)

  These functions return the result of shifting :samp:`{a}` left by :samp:`{b}` bits
  with unsigned saturation.

Comparison functions
^^^^^^^^^^^^^^^^^^^^

The following functions implement fixed-point comparisons.  These functions
implement a low-level compare, upon which the higher level comparison
operators (such as less than and greater than or equal to) can be
constructed.  The returned values lie in the range zero to two, to allow
the high-level operators to be implemented by testing the returned
result using either signed or unsigned comparison.

.. function:: int __cmpqq2(short fracta,short fractb)

  These functions perform a signed or unsigned comparison of :samp:`{a}` and
  :samp:`{b}` (depending on the selected machine mode).  If :samp:`{a}` is less
  than :samp:`{b}` , they return 0; if :samp:`{a}` is greater than :samp:`{b}` , they
  return 2; and if :samp:`{a}` and :samp:`{b}` are equal they return 1.

Conversion functions
^^^^^^^^^^^^^^^^^^^^

.. function:: fract __fractqqhq2(short fracta)

  These functions convert from fractional and signed non-fractionals to
  fractionals and signed non-fractionals, without saturation.

.. function:: fract __satfractqqhq2(short fracta)

  The functions convert from fractional and signed non-fractionals to
  fractionals, with saturation.

.. function:: unsigned char __fractunsqqqi(short fracta)

  These functions convert from fractionals to unsigned non-fractionals;
  and from unsigned non-fractionals to fractionals, without saturation.

.. function:: short fract __satfractunsqiqq(unsigned chara)

  These functions convert from unsigned non-fractionals to fractionals,
  with saturation.

