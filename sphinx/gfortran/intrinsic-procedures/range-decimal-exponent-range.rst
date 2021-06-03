  .. _range:

RANGE --- Decimal exponent range
********************************

.. index:: RANGE

.. index:: model representation, range

:samp:`{Description}:`
  ``RANGE(X)`` returns the decimal exponent range in the model of the
  type of ``X``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = RANGE(X)``

:samp:`{Arguments}:`
  ===========  ======================================
  :samp:`{X}`  Shall be of type ``INTEGER``, ``REAL``
               or ``COMPLEX``.
  ===========  ======================================
  ===========  ======================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of the default integer
  kind.

:samp:`{Example}:`
  See ``PRECISION`` for an example.

:samp:`{See also}:`
  SELECTED_REAL_KIND, 
  PRECISION

