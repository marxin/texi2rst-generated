  .. _aimag:

``AIMAG`` - Imaginary part of complex number  
**********************************************

.. index:: AIMAG

.. index:: DIMAG

.. index:: IMAG

.. index:: IMAGPART

.. index:: complex numbers, imaginary part

:samp:`{Description}:`
  ``AIMAG(Z)`` yields the imaginary part of complex argument ``Z``.
  The ``IMAG(Z)`` and ``IMAGPART(Z)`` intrinsic functions are provided
  for compatibility with :command:`g77`, and their use in new code is 
  strongly discouraged.

:samp:`{Standard}:`
  Fortran 77 and later, has overloads that are GNU extensions

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = AIMAG(Z)``

:samp:`{Arguments}:`
  ===========  ==============================================
  :samp:`{Z}`  The type of the argument shall be ``COMPLEX``.
  ===========  ==============================================
  ===========  ==============================================

:samp:`{Return value}:`
  The return value is of type ``REAL`` with the
  kind type parameter of the argument.

:samp:`{Example}:`

  .. code-block:: c++

    program test_aimag
      complex(4) z4
      complex(8) z8
      z4 = cmplx(1.e0_4, 0.e0_4)
      z8 = cmplx(0.e0_8, 1.e0_8)
      print *, aimag(z4), dimag(z8)
    end program test_aimag

:samp:`{Specific names}:`
  ===============  ================  ===========  ====================
  Name             Argument          Return type  Standard
  ===============  ================  ===========  ====================
  ``AIMAG(Z)``     ``COMPLEX Z``     ``REAL``     Fortran 77 and later
  ``DIMAG(Z)``     ``COMPLEX(8) Z``  ``REAL(8)``  GNU extension
  ``IMAG(Z)``      ``COMPLEX Z``     ``REAL``     GNU extension
  ``IMAGPART(Z)``  ``COMPLEX Z``     ``REAL``     GNU extension
  ===============  ================  ===========  ====================
