  .. _second:

SECOND - CPU time function
**************************

.. index:: SECOND

.. index:: time, elapsed

.. index:: elapsed time

:samp:`{Description}:`
  Returns a ``REAL(4)`` value representing the elapsed CPU time in
  seconds.  This provides the same functionality as the standard
  ``CPU_TIME`` intrinsic, and is only included for backwards
  compatibility.

  This intrinsic is provided in both subroutine and function forms;
  however, only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  =====================
  ``CALL SECOND(TIME)``
  =====================
  ``TIME = SECOND()``
  =====================

:samp:`{Arguments}:`
  ==============  =============================
  :samp:`{TIME}`  Shall be of type ``REAL(4)``.
  ==============  =============================
  ==============  =============================

:samp:`{Return value}:`
  In either syntax, :samp:`{TIME}` is set to the process's current runtime in
  seconds.

:samp:`{See also}:`
  CPU_TIME

