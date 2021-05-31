.. _max-and-min-intrinsics-with-real-nan-arguments:

MAX and MIN intrinsics with REAL NaN arguments
**********************************************

.. index:: MAX, MIN, NaN

The Fortran standard does not specify what the result of the
``MAX`` and ``MIN`` intrinsics are if one of the arguments is a
``NaN``.  Accordingly, the GNU Fortran compiler does not specify
that either, as this allows for faster and more compact code to be
generated.  If the programmer wishes to take some specific action in
case one of the arguments is a ``NaN``, it is necessary to
explicitly test the arguments before calling ``MAX`` or ``MIN``,
e.g. with the ``IEEE_IS_NAN`` function from the intrinsic module
``IEEE_ARITHMETIC``.
