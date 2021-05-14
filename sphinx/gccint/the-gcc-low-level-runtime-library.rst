.. _libgcc:

The GCC low-level runtime library
---------------------------------

GCC provides a low-level runtime library, libgcc.a or
libgcc_s.so.1 on some platforms.  GCC generates calls to
routines in this library automatically, whenever it needs to perform
some operation that is too complicated to emit inline code for.

Most of the routines in ``libgcc`` handle arithmetic operations
that the target processor cannot perform directly.  This includes
integer multiply and divide on some machines, and all floating-point
and fixed-point operations on other machines.  ``libgcc`` also includes
routines for exception handling, and a handful of miscellaneous operations.

Some of these routines can be defined in mostly machine-independent C.
Others must be hand-written in assembly language for each processor
that needs them.

GCC will also generate calls to C library routines, such as
``memcpy`` and ``memset``, in some cases.  The set of routines
that GCC may possibly use is documented in Other
BuiltinsgccUsing the GNU Compiler Collection (GCC).

These routines take arguments and return values of a specific machine
mode, not a specific C type.  See :ref:`machine-modes`, for an explanation
of this concept.  For illustrative purposes, in this chapter the
floating point type ``float`` is assumed to correspond to ``SFmode`` ;
``double`` to ``DFmode`` ; and ``long double`` to both
``TFmode`` and ``XFmode``.  Similarly, the integer types ``int``
and ``unsigned int`` correspond to ``SImode`` ; ``long`` and
``unsigned long`` to ``DImode`` ; and ``long long`` and
``unsigned long long`` to ``TImode``.

.. toctree::

  integer-library-routines
  soft-float-library-routines
  decimal-float-library-routines
  fixed-point-fractional-library-routines
  exception-handling-routines
  miscellaneous-routines
  routines-for-integer-arithmetic
  routines-for-floating-point-emulation
  routines-for-decimal-floating-point-emulation
  routines-for-fixed-point-fractional-emulation
  language-independent-routines-for-exception-handling
  miscellaneous-runtime-library-routines

