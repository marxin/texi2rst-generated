.. _gm2-libs-sysexceptions:

gm2-libs/SysExceptions
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SysExceptions ;

  (* Provides a mechanism for the underlying libraries to
     configure the exception routines.  This mechanism
     is used by both the ISO and PIM libraries.
     It is written to be ISO compliant and this also
     allows for mixed dialect projects.  *)

  FROM SYSTEM IMPORT ADDRESS ;

  TYPE
  PROCEXCEPTION (type)
     PROCEXCEPTION = PROCEDURE (ADDRESS) ;

  InitExceptionHandlers
  PROCEDURE InitExceptionHandlers (indexf, range, casef, invalidloc,
                                   function, wholevalue, wholediv,
                                   realvalue, realdiv, complexvalue,
                                   complexdiv, protection, systemf,
                                   coroutine, exception: PROCEXCEPTION) ;

  END SysExceptions.

