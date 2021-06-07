.. _second:

SECOND --- CPU time function
****************************

.. index:: SECOND

.. index:: time, elapsed

.. index:: elapsed time

.. function:: SECOND

  Returns a ``REAL(4)`` value representing the elapsed CPU time in
  seconds.  This provides the same functionality as the standard
  ``CPU_TIME`` intrinsic, and is only included for backwards
  compatibility.

  :param TIME:
    Shall be of type ``REAL(4)``.

  :return:
    In either syntax, :samp:`{TIME}` is set to the process's current runtime in
    seconds.

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Subroutine, function

  :samp:`{Syntax}:`
    =====================
    ``CALL SECOND(TIME)``
    ``TIME = SECOND()``
    =====================

  :samp:`{See also}:`
    CPU_TIME

