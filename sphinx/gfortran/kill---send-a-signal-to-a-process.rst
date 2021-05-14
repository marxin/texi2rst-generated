  .. _kill:

KILL - Send a signal to a process
*********************************

.. index:: KILL

:samp:`{Description}:`
  Sends the signal specified by :samp:`{SIG}` to the process :samp:`{PID}`.
  See ``kill(2)``.

  This intrinsic is provided in both subroutine and function forms;
  however, only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ==================================
  ``CALL KILL(PID, SIG [, STATUS])``
  ==================================
  ``STATUS = KILL(PID, SIG)``
  ==================================

:samp:`{Arguments}:`
  ================  =========================================================================
  :samp:`{PID}`     Shall be a scalar ``INTEGER`` with ``INTENT(IN)``.
  ================  =========================================================================
  :samp:`{SIG}`     Shall be a scalar ``INTEGER`` with ``INTENT(IN)``.
  :samp:`{STATUS}`  [Subroutine](Optional)
                    Shall be a scalar ``INTEGER``.
                    Returns 0 on success; otherwise a system-specific error code is returned.
  :samp:`{STATUS}`  [Function] The kind type parameter is that of
                    ``pid``.
                    Returns 0 on success; otherwise a system-specific error code is returned.
  ================  =========================================================================

:samp:`{See also}:`
  ABORT, 
  EXIT

