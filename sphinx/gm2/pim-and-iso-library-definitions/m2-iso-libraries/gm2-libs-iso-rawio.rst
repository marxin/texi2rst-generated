.. _gm2-libs-iso-rawio:

gm2-libs-iso/RawIO
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RawIO;

    (* Reading and writing data over specified channels using raw
       operations, that is, with no conversion or interpretation.
       The read result is of the type IOConsts.ReadResults.
    *)

  IMPORT IOChan, SYSTEM;

  Read
  PROCEDURE Read (cid: IOChan.ChanId; VAR to: ARRAY OF SYSTEM.LOC);
    (* Reads storage units from cid, and assigns them to
       successive components of to. The read result is set
       to the value allRight, wrongFormat, or endOfInput.
    *)

  Write
  PROCEDURE Write (cid: IOChan.ChanId; from: ARRAY OF SYSTEM.LOC);
    (* Writes storage units to cid from successive components
       of from. *)

  END RawIO.

