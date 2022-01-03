..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _time:

TIME --- Time function
**********************

.. index:: TIME

.. index:: time, current

.. index:: current time

.. function:: TIME()

  Returns the current time encoded as an integer (in the manner of the
  function ``time(3)`` in the C standard library). This value is
  suitable for passing to :ref:`CTIME`, :ref:`GMTIME`, and :ref:`LTIME`.

  :return:
    The return value is a scalar of type ``INTEGER(4)``.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = TIME()

  :samp:`{See also}:`
    :ref:`DATE_AND_TIME`, 
    :ref:`CTIME`, 
    :ref:`GMTIME`, 
    :ref:`LTIME`, 
    :ref:`MCLOCK`, 
    :ref:`TIME8`

