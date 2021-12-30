..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _time8:

TIME8 --- Time function (64-bit)
********************************

.. index:: TIME8

.. index:: time, current

.. index:: current time

.. function:: TIME8()

  Returns the current time encoded as an integer (in the manner of the
  function ``time(3)`` in the C standard library). This value is
  suitable for passing to :ref:`CTIME`, :ref:`GMTIME`, and :ref:`LTIME`.

  :return:
    The return value is a scalar of type ``INTEGER(8)``.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = TIME8()

  :samp:`{See also}:`
    :ref:`DATE_AND_TIME`, 
    :ref:`CTIME`, 
    :ref:`GMTIME`, 
    :ref:`LTIME`, 
    :ref:`MCLOCK8`, 
    :ref:`TIME`

