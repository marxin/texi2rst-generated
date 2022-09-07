.. _gm2-libs-iso-srawio:

gm2-libs-iso/SRawIO
^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SRawIO;

    (* Reading and writing data over default channels using raw operations, that is, with no
       conversion or interpretation. The read result is of the type IOConsts.ReadResults.
    *)

  IMPORT SYSTEM;

  Read
  PROCEDURE Read (VAR to: ARRAY OF SYSTEM.LOC);
    (* Reads storage units from the default input channel, and assigns them to successive
       components of to.  The read result is set to the value allRight, wrongFormat, or
       endOfInput.
    *)

  Write
  PROCEDURE Write (from: ARRAY OF SYSTEM.LOC);
    (* Writes storage units to the default output channel from successive components of from.
    *)

  END SRawIO.

