.. _gm2-libs-iso-semaphores:

gm2-libs-iso/Semaphores
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Semaphores;

    (* Provides mutual exclusion facilities for use by processes. *)

  TYPE
    SEMAPHORE;

  Create
  PROCEDURE Create (VAR s: SEMAPHORE; initialCount: CARDINAL );
    (* Creates and returns s as the identity of a new semaphore that
       has its associated count initialized to initialCount, and has
       no processes yet waiting on it.
    *)

  Destroy
  PROCEDURE Destroy (VAR s: SEMAPHORE);
    (* Recovers the resources used to implement the semaphore s,
       provided that no process is waiting for s to become free.
    *)

  Claim
  PROCEDURE Claim (s: SEMAPHORE);
    (* If the count associated with the semaphore s is non-zero,
       decrements this count and allows the calling process to
       continue; otherwise suspends the calling process until
       s is released.
    *)

  Release
  PROCEDURE Release (s: SEMAPHORE);
    (* If there are any processes waiting on the semaphore s,
       allows one of them to enter the ready state; otherwise
       increments the count associated with s.
    *)

  CondClaim
  PROCEDURE CondClaim (s: SEMAPHORE): BOOLEAN;
    (* Returns FALSE if the call Claim(s) would cause the calling
       process to be suspended; in this case the count associated
       with s is not changed. Otherwise returns TRUE and the
       associated count is decremented.
    *)

  END Semaphores.

