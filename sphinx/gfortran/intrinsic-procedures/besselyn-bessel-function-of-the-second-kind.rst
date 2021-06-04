  .. _bessel_yn:

BESSEL_YN --- Bessel function of the second kind
************************************************

.. index:: BESSEL_YN

.. index:: BESYN

.. index:: DBESYN

.. index:: Bessel function, second kind

:samp:`{Description}:`
  ``BESSEL_YN(N, X)`` computes the Bessel function of the second kind of
  order :samp:`{N}` of :samp:`{X}`. This function is available under the name
  ``BESYN`` as a GNU extension.  If :samp:`{N}` and :samp:`{X}` are arrays,
  their ranks and shapes shall conform.  

  ``BESSEL_YN(N1, N2, X)`` returns an array with the Bessel functions
  of the first kind of the orders :samp:`{N1}` to :samp:`{N2}`.

:samp:`{Standard}:`
  Fortran 2008 and later, negative :samp:`{N}` is allowed as GNU extension

:samp:`{Class}:`
  Elemental function, except for the transformational function
  ``BESSEL_YN(N1, N2, X)``

:samp:`{Syntax}:`
  =================================
  ``RESULT = BESSEL_YN(N, X)``
  =================================
  ``RESULT = BESSEL_YN(N1, N2, X)``
  =================================

:samp:`{Arguments}:`
  ============  ====================================================
  :samp:`{N}`   Shall be a scalar or an array of type  ``INTEGER`` .
  ============  ====================================================
  :samp:`{N1}`  Shall be a non-negative scalar of type  ``INTEGER``.
  :samp:`{N2}`  Shall be a non-negative scalar of type  ``INTEGER``.
  :samp:`{X}`   Shall be a scalar or an array of type  ``REAL`` ;
                for ``BESSEL_YN(N1, N2, X)`` it shall be scalar.
  ============  ====================================================

:samp:`{Return value}:`
  The return value is a scalar of type ``REAL``. It has the same
  kind as :samp:`{X}`.

:samp:`{Note}:`
  The transformational function uses a recurrence algorithm which might,
  for some values of :samp:`{X}`, lead to different results than calls to
  the elemental function.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_besyn
      real(8) :: x = 1.0_8
      x = bessel_yn(5,x)
    end program test_besyn

:samp:`{Specific names}:`
  ===============  =============  ===========  =============
  Name             Argument       Return type  Standard
  ===============  =============  ===========  =============
  ``DBESYN(N,X)``  ``INTEGER N``  ``REAL(8)``  GNU extension
                   ``REAL(8) X``
  ===============  =============  ===========  =============
