  .. _huge:

HUGE - Largest number of a kind
*******************************

.. index:: HUGE

.. index:: limits, largest number

.. index:: model representation, largest number

:samp:`{Description}:`
  ``HUGE(X)`` returns the largest number that is not an infinity in
  the model of the type of ``X``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = HUGE(X)``

:samp:`{Arguments}:`
  ===========  =========================================
  :samp:`{X}`  Shall be of type ``REAL`` or ``INTEGER``.
  ===========  =========================================
  ===========  =========================================

:samp:`{Return value}:`
  The return value is of the same type and kind as :samp:`{X}`

:samp:`{Example}:`

  .. code-block:: c++

    program test_huge_tiny
      print *, huge(0), huge(0.0), huge(0.0d0)
      print *, tiny(0.0), tiny(0.0d0)
    end program test_huge_tiny

