.. _mclock:

MCLOCK --- Time function
************************

.. index:: MCLOCK

.. index:: time, clock ticks

.. index:: clock ticks

.. function:: MCLOCK

  Returns the number of clock ticks since the start of the process, based
  on the function ``clock(3)`` in the C standard library.

  :return:
    The return value is a scalar of type ``INTEGER(4)``, equal to the
    number of clock ticks since the start of the process, or ``-1`` if
    the system does not support ``clock(3)``.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`
    ``RESULT = MCLOCK()``

  :samp:`{See also}:`
    CTIME, 
    GMTIME, 
    LTIME, 
    MCLOCK, 
    TIME

