  .. _real:

REAL - Convert to real type 
****************************

.. index:: REAL

.. index:: REALPART

.. index:: FLOAT

.. index:: DFLOAT

.. index:: FLOATI

.. index:: FLOATJ

.. index:: FLOATK

.. index:: SNGL

.. index:: conversion, to real

.. index:: complex numbers, real part

:samp:`{Description}:`
  ``REAL(A [, KIND])`` converts its argument :samp:`{A}` to a real type.  The
  ``REALPART`` function is provided for compatibility with :command:`g77`,
  and its use is strongly discouraged.

:samp:`{Standard}:`
  Fortran 77 and later, with :samp:`{KIND}` argument Fortran 90 and later, has GNU extensions

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  =============================
  ``RESULT = REAL(A [, KIND])``
  =============================
  ``RESULT = REALPART(Z)``
  =============================

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{A}`     Shall be ``INTEGER``, ``REAL``, or
                  ``COMPLEX``.
  ==============  =======================================================
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  These functions return a ``REAL`` variable or array under
  the following rules: 

  (A)
    ``REAL(A)`` is converted to a default real type if :samp:`{A}` is an 
    integer or real variable.

  (B)
    ``REAL(A)`` is converted to a real type with the kind type parameter
    of :samp:`{A}` if :samp:`{A}` is a complex variable.

  (C)
    ``REAL(A, KIND)`` is converted to a real type with kind type
    parameter :samp:`{KIND}` if :samp:`{A}` is a complex, integer, or real
    variable.

:samp:`{Example}:`

  .. code-block:: c++

    program test_real
      complex :: x = (1.0, 2.0)
      print *, real(x), real(x,8), realpart(x)
    end program test_real

:samp:`{Specific names}:`
  =============  ==============  ===========  =============
  Name           Argument        Return type  Standard
  =============  ==============  ===========  =============
  ``FLOAT(A)``   ``INTEGER(4)``  ``REAL(4)``  GNU extension
  ``DFLOAT(A)``  ``INTEGER(4)``  ``REAL(8)``  GNU extension
  ``FLOATI(A)``  ``INTEGER(2)``  ``REAL(4)``  GNU extension
  ``FLOATJ(A)``  ``INTEGER(4)``  ``REAL(4)``  GNU extension
  ``FLOATK(A)``  ``INTEGER(8)``  ``REAL(4)``  GNU extension
  ``SNGL(A)``    ``INTEGER(8)``  ``REAL(4)``  GNU extension
  =============  ==============  ===========  =============

:samp:`{See also}:`
  DBLE

