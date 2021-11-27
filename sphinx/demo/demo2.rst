Multiple
====

Arithmetic functions
^^^^^^^^^^^^^^^^^^^^

.. function:: _Decimal32 __dpd_addsd3 (_Decimal32 a, _Decimal32 b)
.. function:: _Decimal32 __bid_addsd3 (_Decimal32 a, _Decimal32 b)
.. function:: _Decimal64 __dpd_adddd3 (_Decimal64 a, _Decimal64 b)
.. function:: _Decimal64 __bid_adddd3 (_Decimal64 a, _Decimal64 b)
.. function:: _Decimal128 __dpd_addtd3 (_Decimal128 a, _Decimal128 b)
.. function:: _Decimal128 __bid_addtd3 (_Decimal128 a, _Decimal128 b)

  These functions return the sum of :samp:`{a}` and :samp:`{b}`.

Tools/packages necessary for building GCC
=========================================

autoconf version 2.69
GNU m4 version 1.4.6 (or later)
  Necessary when modifying :samp:`configure.ac`, :samp:`aclocal.m4`, etc.
  to regenerate :samp:`configure` and :samp:`config.in` files.

git (any version)
SSH (any version)
Foobar (all versions)

  Necessary to access the source repository.  Public releases and weekly
  snapshots of the development sources are also available via HTTPS.

.. option:: -mmmx
.. option:: -msse
.. option:: -msse2

  These switches enable the use of instructions in the MMX, SSE,
  SSE2, ... 
