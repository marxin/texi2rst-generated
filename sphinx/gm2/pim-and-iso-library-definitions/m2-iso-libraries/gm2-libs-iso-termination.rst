.. _gm2-libs-iso-termination:

gm2-libs-iso/TERMINATION
^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE TERMINATION;

  (* Provides facilities for enquiries concerning the occurrence of termination events. *)

  IsTerminating
  PROCEDURE IsTerminating (): BOOLEAN ;
    (* Returns true if any coroutine has started  program termination and false otherwise. *)

  HasHalted
  PROCEDURE HasHalted (): BOOLEAN ;
    (* Returns true if a call to HALT has been made and false otherwise. *)

  END TERMINATION.

