.. _gm2-libs-sysstorage:

gm2-libs/SysStorage
^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SysStorage ;

  (*  Provides dynamic allocation for the system components.
      This allows the application to use the traditional Storage module
      which can be handled differently.  *)

  FROM SYSTEM IMPORT ADDRESS ;
  EXPORT QUALIFIED ALLOCATE, DEALLOCATE, REALLOCATE, Available, Init ;

  (*
     ALLOCATE - attempt to allocate memory from the heap.
                NIL is returned in, a, if ALLOCATE fails.
  *)

  ALLOCATE
  PROCEDURE ALLOCATE (VAR a: ADDRESS ; size: CARDINAL) ;

  (*
     DEALLOCATE - return, size, bytes to the heap.
                  The variable, a, is set to NIL.
  *)

  DEALLOCATE
  PROCEDURE DEALLOCATE (VAR a: ADDRESS ; size: CARDINAL) ;

  (*
     REALLOCATE - attempts to reallocate storage. The address,
                  a, should either be NIL in which case ALLOCATE
                  is called, or alternatively it should have already
                  been initialized by ALLOCATE. The allocated storage
                  is resized accordingly.
  *)

  REALLOCATE
  PROCEDURE REALLOCATE (VAR a: ADDRESS; size: CARDINAL) ;

  (*
     Available - returns TRUE if, size, bytes can be allocated.
  *)

  Available
  PROCEDURE Available (size: CARDINAL) : BOOLEAN;

  (*
     Init - initializes the heap.
            This does nothing on a GNU/Linux system.
            But it remains here since it might be used in an
            embedded system.
  *)

  Init
  PROCEDURE Init ;

  END SysStorage.

