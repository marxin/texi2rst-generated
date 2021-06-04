  .. _exp:

EXP --- Exponential function 
*****************************

.. index:: EXP

.. index:: DEXP

.. index:: CEXP

.. index:: ZEXP

.. index:: CDEXP

.. index:: exponential function

.. index:: logarithm function, inverse

:samp:`{Description}:`
  ``EXP(X)`` computes the base e exponential of :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 77 and later, has overloads that are GNU extensions

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = EXP(X)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{X}`  The type shall be ``REAL`` or
               ``COMPLEX``.
  ===========  =============================

:samp:`{Return value}:`
  The return value has same type and kind as :samp:`{X}`.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_exp
      real :: x = 1.0
      x = exp(x)
    end program test_exp

:samp:`{Specific names}:`
  ============  ================  ==============  ====================
  Name          Argument          Return type     Standard
  ``EXP(X)``    ``REAL(4) X``     ``REAL(4)``     Fortran 77 and later
  ``DEXP(X)``   ``REAL(8) X``     ``REAL(8)``     Fortran 77 and later
  ``CEXP(X)``   ``COMPLEX(4) X``  ``COMPLEX(4)``  Fortran 77 and later
  ``ZEXP(X)``   ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
  ``CDEXP(X)``  ``COMPLEX(8) X``  ``COMPLEX(8)``  GNU extension
  ============  ================  ==============  ====================
