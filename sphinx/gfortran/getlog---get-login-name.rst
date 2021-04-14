  .. _getlog:

``GETLOG`` - Get login name
***************************

.. index:: GETLOG

.. index:: system, login name

.. index:: login name

:samp:`{Description}:`
  Gets the username under which the program is running.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL GETLOG(C)``

:samp:`{Arguments}:`
  ===========  ===================================================
  :samp:`{C}`  Shall be of type ``CHARACTER`` and of default kind.
  ===========  ===================================================
  ===========  ===================================================

:samp:`{Return value}:`
  Stores the current user name in :samp:`{LOGIN}`.  (On systems where POSIX
  functions ``geteuid`` and ``getpwuid`` are not available, and 
  the ``getlogin`` function is not implemented either, this will
  return a blank string.)

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM TEST_GETLOG
      CHARACTER(32) :: login
      CALL GETLOG(login)
      WRITE(*,*) login
    END PROGRAM

:samp:`{See also}:`
  GETUID

