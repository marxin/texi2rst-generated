  .. _system:

SYSTEM --- Execute a shell command
**********************************

.. index:: SYSTEM

.. index:: system, system call

:samp:`{Description}:`
  Passes the command :samp:`{COMMAND}` to a shell (see ``system(3)`` ). If
  argument :samp:`{STATUS}` is present, it contains the value returned by
  ``system(3)``, which is presumably 0 if the shell command succeeded.
  Note that which shell is used to invoke the command is system-dependent
  and environment-dependent.

  This intrinsic is provided in both subroutine and function forms;
  however, only one form can be used in any given program unit.

  Note that the ``system`` function need not be thread-safe. It is
  the responsibility of the user to ensure that ``system`` is not
  called concurrently.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ===================================
  ``CALL SYSTEM(COMMAND [, STATUS])``
  ===================================
  ``STATUS = SYSTEM(COMMAND)``
  ===================================

:samp:`{Arguments}:`
  =================  ================================================
  :samp:`{COMMAND}`  Shall be of default ``CHARACTER`` type.
  =================  ================================================
  :samp:`{STATUS}`   (Optional) Shall be of default ``INTEGER`` type.
  =================  ================================================

:samp:`{See also}:`
  EXECUTE_COMMAND_LINE, which is part of the Fortran 2008 standard
  and should considered in new code for future portability.

