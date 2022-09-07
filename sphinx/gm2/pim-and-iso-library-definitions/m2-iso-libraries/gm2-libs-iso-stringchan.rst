.. _gm2-libs-iso-stringchan:

gm2-libs-iso/StringChan
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE StringChan ;

  (*
      Description: provides a set of Channel and String
                   input and output procedures.
  *)

  FROM DynamicStrings IMPORT String ;
  IMPORT IOChan;

  (*
     writeString - writes a string, s, to ChanId, cid.
                   The string, s, is not destroyed.
  *)

  writeString
  PROCEDURE writeString (cid: IOChan.ChanId; s: String) ;

  (*
     writeFieldWidth - writes a string, s, to ChanId, cid.
                       The string, s, is not destroyed and it
                       is prefixed by spaces so that at least,
                       width, characters are written.  If the
                       string, s, is longer than width then
                       no spaces are prefixed to the output
                       and the entire string is written.
  *)

  writeFieldWidth
  PROCEDURE writeFieldWidth (cid: IOChan.ChanId;
                             s: String; width: CARDINAL) ;

  END StringChan.

