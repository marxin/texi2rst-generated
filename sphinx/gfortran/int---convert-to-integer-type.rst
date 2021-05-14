  .. _int:

INT - Convert to integer type
*****************************

.. index:: INT

.. index:: IFIX

.. index:: IDINT

.. index:: conversion, to integer

:samp:`{Description}:`
  Convert to integer type

:samp:`{Standard}:`
  Fortran 77 and later, with boz-literal-constant Fortran 2008 and later.

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = INT(A [, KIND))``

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{A}`     Shall be of type ``INTEGER``,
                  ``REAL``, or ``COMPLEX`` or a boz-literal-constant.
  ==============  =======================================================
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  These functions return a ``INTEGER`` variable or array under 
  the following rules: 

  (A)
    If :samp:`{A}` is of type ``INTEGER``, ``INT(A) = A`` 

  (B)
    If :samp:`{A}` is of type ``REAL`` and |A| < 1, ``INT(A)``
    equals ``0``. If |A| \geq 1, then ``INT(A)`` is the integer
    whose magnitude is the largest integer that does not exceed the magnitude
    of :samp:`{A}` and whose sign is the same as the sign of :samp:`{A}`.

  (C)
    If :samp:`{A}` is of type ``COMPLEX``, rule B is applied to the real part of :samp:`{A}`.

:samp:`{Example}:`

  .. code-block:: c++

    program test_int
      integer :: i = 42
      complex :: z = (-3.7, 1.0)
      print *, int(i)
      print *, int(z), int(z,8)
    end program

:samp:`{Specific names}:`
  ============  =============  ===========  ====================
  Name          Argument       Return type  Standard
  ============  =============  ===========  ====================
  ``INT(A)``    ``REAL(4) A``  ``INTEGER``  Fortran 77 and later
  ``IFIX(A)``   ``REAL(4) A``  ``INTEGER``  Fortran 77 and later
  ``IDINT(A)``  ``REAL(8) A``  ``INTEGER``  Fortran 77 and later
  ============  =============  ===========  ====================
