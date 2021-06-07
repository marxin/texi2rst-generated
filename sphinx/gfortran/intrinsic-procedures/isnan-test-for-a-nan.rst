.. _isnan:

ISNAN --- Test for a NaN
************************

.. index:: ISNAN

.. index:: IEEE, ISNAN

.. function:: ISNAN

  ``ISNAN`` tests whether a floating-point value is an IEEE
  Not-a-Number (NaN).

  :param X:
    Variable of the type ``REAL``.

  :return:
    Returns a default-kind ``LOGICAL``. The returned value is ``TRUE``
    if :samp:`{X}` is a NaN and ``FALSE`` otherwise.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`
    ``ISNAN(X)``

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_nan
        implicit none
        real :: x
        x = -1.0
        x = sqrt(x)
        if (isnan(x)) stop '"x" is a NaN'
      end program test_nan

