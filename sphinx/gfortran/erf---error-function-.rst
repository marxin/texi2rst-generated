  .. _erf:

``ERF`` - Error function 
*************************

.. index:: ERF

.. index:: error function

:samp:`{Description}:`
  ``ERF(X)`` computes the error function of :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = ERF(X)``

:samp:`{Arguments}:`
  ===========  ===========================
  :samp:`{X}`  The type shall be ``REAL``.
  ===========  ===========================
  ===========  ===========================

:samp:`{Return value}:`
  The return value is of type ``REAL``, of the same kind as
  :samp:`{X}` and lies in the range -1 \leq erf (x) \leq 1 .

:samp:`{Example}:`

  .. code-block:: c++

    program test_erf
      real(8) :: x = 0.17_8
      x = erf(x)
    end program test_erf

:samp:`{Specific names}:`
  ===========  =============  ===========  =============
  Name         Argument       Return type  Standard
  ===========  =============  ===========  =============
  ``DERF(X)``  ``REAL(8) X``  ``REAL(8)``  GNU extension
  ===========  =============  ===========  =============
