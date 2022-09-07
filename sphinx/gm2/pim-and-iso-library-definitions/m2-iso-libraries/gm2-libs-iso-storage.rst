.. _gm2-libs-iso-storage:

gm2-libs-iso/Storage
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Storage;

    (* Facilities for dynamically allocating and deallocating storage *)

  IMPORT SYSTEM;

  ALLOCATE
  PROCEDURE ALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL);
    (* Allocates storage for a variable of size amount and assigns
       the address of this variable to addr. If there is insufficient
       unallocated storage to do this, the value NIL is assigned to addr.
    *)

  DEALLOCATE
  PROCEDURE DEALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL);
    (* Deallocates amount locations allocated by ALLOCATE for
       the storage of the variable addressed by addr and assigns
       the value NIL to addr.
    *)

  REALLOCATE
  PROCEDURE REALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL);
    (* Attempts to reallocate, amount of storage.  Effectively it
       calls ALLOCATE, copies the amount of data pointed to by
       addr into the new space and DEALLOCATES the addr.
       This procedure is a GNU extension.
    *)

  TYPE
  StorageExceptions (type)
    StorageExceptions = (
      nilDeallocation,             (* first argument to DEALLOCATE is NIL *)
      pointerToUnallocatedStorage, (* storage to deallocate not allocated by ALLOCATE *)
      wrongStorageToUnallocate     (* amount to deallocate is not amount allocated *)
    );

  IsStorageException
  PROCEDURE IsStorageException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional
       execution state because of the raising of an exception from
       StorageExceptions; otherwise returns FALSE.
    *)

  StorageException
  PROCEDURE StorageException (): StorageExceptions;
    (* If the current coroutine is in the exceptional execution
       state because of the raising of an exception from
       StorageExceptions, returns the corresponding
       enumeration value, and otherwise raises an exception.
    *)

  END Storage.

