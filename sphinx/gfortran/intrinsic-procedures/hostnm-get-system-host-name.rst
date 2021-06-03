  .. _hostnm:

HOSTNM --- Get system host name
*******************************

.. index:: HOSTNM

.. index:: system, host name

:samp:`{Description}:`
  Retrieves the host name of the system on which the program is running.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  =============================
  ``CALL HOSTNM(C [, STATUS])``
  =============================
  ``STATUS = HOSTNM(NAME)``
  =============================

:samp:`{Arguments}:`
  ================  ================================================================
  :samp:`{C}`       Shall of type ``CHARACTER`` and of default kind.
  ================  ================================================================
  :samp:`{STATUS}`  (Optional) status flag of type ``INTEGER``.
                    Returns 0 on success, or a system specific error code otherwise.
  ================  ================================================================

:samp:`{Return value}:`
  In either syntax, :samp:`{NAME}` is set to the current hostname if it can
  be obtained, or to a blank string otherwise.

