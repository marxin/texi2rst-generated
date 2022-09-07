.. _gm2-libs-storage:

gm2-libs/Storage
^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Storage ;

  FROM SYSTEM IMPORT ADDRESS ;

  EXPORT QUALIFIED ALLOCATE, DEALLOCATE, REALLOCATE, Available ;

  (*
     ALLOCATE - attempt to allocate memory from the heap.
                NIL is returned in, a, if ALLOCATE fails.
  *)

  ALLOCATE
  PROCEDURE ALLOCATE (VAR a: ADDRESS ; Size: CARDINAL) ;

  (*
     DEALLOCATE - return, Size, bytes to the heap.
                  The variable, a, is set to NIL.
  *)

  DEALLOCATE
  PROCEDURE DEALLOCATE (VAR a: ADDRESS ; Size: CARDINAL) ;

  (*
     REALLOCATE - attempts to reallocate storage. The address,
                  a, should either be NIL in which case ALLOCATE
                  is called, or alternatively it should have already
                  been initialized by ALLOCATE. The allocated storage
                  is resized accordingly.
  *)

  REALLOCATE
  PROCEDURE REALLOCATE (VAR a: ADDRESS; Size: CARDINAL) ;

  (*
     Available - returns TRUE if, Size, bytes can be allocated.
  *)

  Available
  PROCEDURE Available (Size: CARDINAL) : BOOLEAN ;

  END Storage.

