  .. _log10:

``LOG10`` - Base 10 logarithm function
**************************************

.. index:: LOG10

.. index:: ALOG10

.. index:: DLOG10

.. index:: exponential function, inverse

.. index:: logarithm function with base 10

.. index:: base 10 logarithm function

:samp:`{Description}:`
  ``LOG10(X)`` computes the base 10 logarithm of :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 77 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = LOG10(X)``

:samp:`{Arguments}:`
  ===========  ===========================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  ===========================
  ===========  ===========================

:samp:`{Return value}:`
  The return value is of type ``REAL`` or ``COMPLEX``.
  The kind type parameter is the same as :samp:`{X}`.

:samp:`{Example}:`

  .. code-block:: c++

    program test_log10
      real(8) :: x = 10.0_8
      x = log10(x)
    end program test_log10

:samp:`{Specific names}:`
  =============  =============  ===========  ====================
  Name           Argument       Return type  Standard
  =============  =============  ===========  ====================
  ``ALOG10(X)``  ``REAL(4) X``  ``REAL(4)``  Fortran 77 and later
  ``DLOG10(X)``  ``REAL(8) X``  ``REAL(8)``  Fortran 77 and later
  =============  =============  ===========  ====================
