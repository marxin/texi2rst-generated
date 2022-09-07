.. _gm2-libs-m2link:

gm2-libs/M2LINK
^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FOR "C" M2LINK ;

  TYPE
  PtrToChar (type)
     PtrToChar = POINTER TO CHAR ;

  (* These variables are set by the compiler in the program module
     according to linking command line options.  *)

  VAR
  ForcedModuleInitOrder (var)
     ForcedModuleInitOrder: PtrToChar ;
  StaticInitialization  (var)
     StaticInitialization : BOOLEAN ;

  END M2LINK. (var)
  END M2LINK.

