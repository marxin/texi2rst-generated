.. _gm2-libs-iso-simplecipher:

gm2-libs-iso/SimpleCipher
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SimpleCipher ;

  (*
      Description: provides a simple Caesar cipher layer which
                   can be attached to any channel device.  This,
                   pedagogical, module is designed to show how
                   it is possible to add further layers underneath
                   the channel devices.
  *)

  FROM IOChan IMPORT ChanId ;

  (*
     InsertCipherLayer - inserts a caesar cipher below channel, cid.
                         The encryption, key, is specified.
  *)

  InsertCipherLayer
  PROCEDURE InsertCipherLayer (cid: ChanId; key: INTEGER) ;

  (*
     RemoveCipherLayer - removes a Caesar cipher below channel, cid.
  *)

  RemoveCipherLayer
  PROCEDURE RemoveCipherLayer (cid: ChanId) ;

  END SimpleCipher.

