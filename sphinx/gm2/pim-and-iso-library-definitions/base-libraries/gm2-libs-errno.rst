.. _gm2-libs-errno:

gm2-libs/errno
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE errno ;

  CONST
      EINTR  =  4 ;   (* system call interrupted *)
      ERANGE = 34 ;   (* result is too large     *)
      EAGAIN = 11 ;   (* retry the system call   *)

  geterrno
  PROCEDURE geterrno () : INTEGER ;

  END errno.

