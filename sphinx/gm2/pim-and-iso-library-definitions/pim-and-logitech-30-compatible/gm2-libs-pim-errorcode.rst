.. _gm2-libs-pim-errorcode:

gm2-libs-pim/ErrorCode
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ErrorCode ;

  EXPORT QUALIFIED SetErrorCode, GetErrorCode, ExitToOS ;

  (*
     SetErrorCode - sets the exit value which will be used if
                    the application terminates normally.
  *)

  SetErrorCode
  PROCEDURE SetErrorCode (value: INTEGER) ;

  (*
     GetErrorCode - returns the current value to be used upon
                    application termination.
  *)

  GetErrorCode
  PROCEDURE GetErrorCode (VAR value: INTEGER) ;

  (*
     ExitToOS - terminate the application and exit returning
                the last value set by SetErrorCode to the OS.
  *)

  ExitToOS
  PROCEDURE ExitToOS ;

  END ErrorCode.

