  .. _nearest:

``NEAREST`` - Nearest representable number
******************************************

.. index:: NEAREST

.. index:: real number, nearest different

.. index:: floating point, nearest different

:samp:`{Description}:`
  ``NEAREST(X, S)`` returns the processor-representable number nearest
  to ``X`` in the direction indicated by the sign of ``S``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = NEAREST(X, S)``

:samp:`{Arguments}:`
  ===========  =============================
  :samp:`{X}`  Shall be of type ``REAL``.
  ===========  =============================
  :samp:`{S}`  Shall be of type ``REAL`` and
               not equal to zero.
  ===========  =============================

:samp:`{Return value}:`
  The return value is of the same type as ``X``. If ``S`` is
  positive, ``NEAREST`` returns the processor-representable number
  greater than ``X`` and nearest to it. If ``S`` is negative,
  ``NEAREST`` returns the processor-representable number smaller than
  ``X`` and nearest to it.

:samp:`{Example}:`

  .. code-block:: c++

    program test_nearest
      real :: x, y
      x = nearest(42.0, 1.0)
      y = nearest(42.0, -1.0)
      write (*,"(3(G20.15))") x, y, x - y
    end program test_nearest

