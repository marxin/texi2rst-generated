  .. _getpid:

GETPID --- Process ID function
******************************

.. index:: GETPID

.. index:: system, process ID

.. index:: process ID

:samp:`{Description}:`
  Returns the numerical process identifier of the current process.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Function

:samp:`{Syntax}:`
  ``RESULT = GETPID()``

:samp:`{Return value}:`
  The return value of ``GETPID`` is an ``INTEGER`` of the default
  kind.

:samp:`{Example}:`

  .. code-block:: fortran

    program info
      print *, "The current process ID is ", getpid()
      print *, "Your numerical user ID is ", getuid()
      print *, "Your numerical group ID is ", getgid()
    end program info

:samp:`{See also}:`
  GETGID, 
  GETUID

