.. _range:

RANGE --- Decimal exponent range
********************************

.. index:: RANGE

.. index:: model representation, range

.. function:: RANGE(X)

  ``RANGE(X)`` returns the decimal exponent range in the model of the
  type of ``X``.

  :param X:
    Shall be of type ``INTEGER``, ``REAL``
    or ``COMPLEX``.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`
    ``RESULT = RANGE(X)``

  :samp:`{Example}:`
    See ``PRECISION`` for an example.

  :samp:`{See also}:`
    SELECTED_REAL_KIND, 
    PRECISION

