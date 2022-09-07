.. _gm2-libs-iso-rtentity:

gm2-libs-iso/RTentity
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RTentity ;

  (*
      Description: provides a set of routines for maintaining an
                   efficient mechanism to group opaque (or pointer)
                   data structures together.  Internally the
                   entities are grouped together using a binary
                   tree.  It does not use Storage - and instead
                   uses malloc, free from libc as Storage uses the
                   module to detect erroneous deallocations.
  *)

  IMPORT SYSTEM ;

  TYPE
  Group (type)
     Group ;

  InitGroup
  PROCEDURE InitGroup () : Group ;
  KillGroup
  PROCEDURE KillGroup (g: Group) : Group ;
  GetKey
  PROCEDURE GetKey (g: Group; a: SYSTEM.ADDRESS) : CARDINAL ;
  PutKey
  PROCEDURE PutKey (g: Group; a: SYSTEM.ADDRESS; key: CARDINAL) ;
  DelKey
  PROCEDURE DelKey (g: Group; a: SYSTEM.ADDRESS) ;
  IsIn
  PROCEDURE IsIn (g: Group; a: SYSTEM.ADDRESS) : BOOLEAN ;

  END RTentity.

