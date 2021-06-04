  .. _maxexponent:

MAXEXPONENT --- Maximum exponent of a real kind
***********************************************

.. index:: MAXEXPONENT

.. index:: model representation, maximum exponent

:samp:`{Description}:`
  ``MAXEXPONENT(X)`` returns the maximum exponent in the model of the
  type of ``X``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = MAXEXPONENT(X)``

:samp:`{Arguments}:`
  ===========  ==========================
  :samp:`{X}`  Shall be of type ``REAL``.
  ===========  ==========================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the default integer
  kind.

:samp:`{Example}:`

  .. code-block:: fortran

    program exponents
      real(kind=4) :: x
      real(kind=8) :: y

      print *, minexponent(x), maxexponent(x)
      print *, minexponent(y), maxexponent(y)
    end program exponents

