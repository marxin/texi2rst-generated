..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _bessel_jn:

BESSEL_JN --- Bessel function of the first kind
***********************************************

.. index:: BESSEL_JN

.. index:: BESJN

.. index:: DBESJN

.. index:: Bessel function, first kind

.. function:: BESSEL_JN(N, X)

  ``BESSEL_JN(N, X)`` computes the Bessel function of the first kind of
  order :samp:`{N}` of :samp:`{X}`. This function is available under the name
  ``BESJN`` as a GNU extension.  If :samp:`{N}` and :samp:`{X}` are arrays,
  their ranks and shapes shall conform.  

  :param N:
    Shall be a scalar or an array of type  ``INTEGER``.

  :param N1:
    Shall be a non-negative scalar of type  ``INTEGER``.

  :param N2:
    Shall be a non-negative scalar of type  ``INTEGER``.

  :param X:
    Shall be a scalar or an array of type  ``REAL`` ;
    for ``BESSEL_JN(N1, N2, X)`` it shall be scalar.

  :return:
    The return value is a scalar of type ``REAL``. It has the same
    kind as :samp:`{X}`.

  :samp:`{Standard}:`
    Fortran 2008 and later, negative :samp:`{N}` is allowed as GNU extension

  :samp:`{Class}:`
    Elemental function, except for the transformational function
    ``BESSEL_JN(N1, N2, X)``

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = BESSEL_JN(N, X)
    RESULT = BESSEL_JN(N1, N2, X)

  :samp:`{Note}:`
    The transformational function uses a recurrence algorithm which might,
    for some values of :samp:`{X}`, lead to different results than calls to
    the elemental function.

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_besjn
        real(8) :: x = 1.0_8
        x = bessel_jn(5,x)
      end program test_besjn

  :samp:`{Specific names}:`
    ================  =============  ===========  =============
    Name              Argument       Return type  Standard
    ================  =============  ===========  =============
    ``DBESJN(N, X)``  ``INTEGER N``  ``REAL(8)``  GNU extension
                      ``REAL(8) X``
    ================  =============  ===========  =============
