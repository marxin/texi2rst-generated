  .. _radix:

RADIX --- Base of a model number
********************************

.. index:: RADIX

.. index:: model representation, base

.. index:: model representation, radix

:samp:`{Description}:`
  ``RADIX(X)`` returns the base of the model representing the entity :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = RADIX(X)``

:samp:`{Arguments}:`
  ===========  ========================================
  :samp:`{X}`  Shall be of type ``INTEGER`` or ``REAL``
  ===========  ========================================
  ===========  ========================================

:samp:`{Return value}:`
  The return value is a scalar of type ``INTEGER`` and of the default
  integer kind.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_radix
      print *, "The radix for the default integer kind is", radix(0)
      print *, "The radix for the default real kind is", radix(0.0)
    end program test_radix

:samp:`{See also}:`
  SELECTED_REAL_KIND

