  .. _execute_command_line:

EXECUTE_COMMAND_LINE - Execute a shell command
**********************************************

.. index:: EXECUTE_COMMAND_LINE

.. index:: system, system call

.. index:: command line

:samp:`{Description}:`
  ``EXECUTE_COMMAND_LINE`` runs a shell command, synchronously or
  asynchronously.

  The ``COMMAND`` argument is passed to the shell and executed (The
  shell is ``sh`` on Unix systems, and ``cmd.exe`` on Windows.).
  If ``WAIT`` is present and has the value false, the execution of
  the command is asynchronous if the system supports it; otherwise, the
  command is executed synchronously using the C library's ``system``
  call.

  The three last arguments allow the user to get status information.  After
  synchronous execution, ``EXITSTAT`` contains the integer exit code of
  the command, as returned by ``system``.  ``CMDSTAT`` is set to zero
  if the command line was executed (whatever its exit status was).
  ``CMDMSG`` is assigned an error message if an error has occurred.

  Note that the ``system`` function need not be thread-safe. It is
  the responsibility of the user to ensure that ``system`` is not
  called concurrently.

  For asynchronous execution on supported targets, the POSIX
  ``posix_spawn`` or ``fork`` functions are used.  Also, a signal
  handler for the ``SIGCHLD`` signal is installed.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL EXECUTE_COMMAND_LINE(COMMAND [, WAIT, EXITSTAT, CMDSTAT, CMDMSG ])``

:samp:`{Arguments}:`
  ==================  ==================================================
  :samp:`{COMMAND}`   Shall be a default ``CHARACTER`` scalar.
  ==================  ==================================================
  :samp:`{WAIT}`      (Optional) Shall be a default ``LOGICAL`` scalar.
  :samp:`{EXITSTAT}`  (Optional) Shall be an ``INTEGER`` of the
                      default kind.
  :samp:`{CMDSTAT}`   (Optional) Shall be an ``INTEGER`` of the
                      default kind.
  :samp:`{CMDMSG}`    (Optional) Shall be an ``CHARACTER`` scalar of the
                      default kind.
  ==================  ==================================================

:samp:`{Example}:`

  .. code-block:: c++

    program test_exec
      integer :: i

      call execute_command_line ("external_prog.exe", exitstat=i)
      print *, "Exit status of external_prog.exe was ", i

      call execute_command_line ("reindex_files.exe", wait=.false.)
      print *, "Now reindexing files in the background"

    end program test_exec

:samp:`{Note}:`
  Because this intrinsic is implemented in terms of the ``system``
  function call, its behavior with respect to signaling is processor
  dependent. In particular, on POSIX-compliant systems, the SIGINT and
  SIGQUIT signals will be ignored, and the SIGCHLD will be blocked. As
  such, if the parent process is terminated, the child process might not be
  terminated alongside.

:samp:`{See also}:`
  SYSTEM

