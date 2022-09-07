.. _gm2-libs-iso-rtio:

gm2-libs-iso/RTio
^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RTio ;

  (*
      Description: provides low level routines for creating and destroying
                   ChanIds.  This is necessary to allow multiple modules
                   to create, ChanId values, where ChanId is an opaque
                   type.
  *)

  IMPORT FIO, IOLink ;

  TYPE
  ChanId (type)
     ChanId ;

  (*
     InitChanId - return a new ChanId.
  *)

  InitChanId
  PROCEDURE InitChanId () : ChanId ;

  (*
     KillChanId - deallocate a ChanId.
  *)

  KillChanId
  PROCEDURE KillChanId (c: ChanId) : ChanId ;

  (*
     NilChanId - return a NIL pointer.
  *)

  NilChanId
  PROCEDURE NilChanId () : ChanId ;

  (*
     GetDeviceId - returns the device id, from, c.
  *)

  GetDeviceId
  PROCEDURE GetDeviceId (c: ChanId) : IOLink.DeviceId ;

  (*
     SetDeviceId - sets the device id in, c.
  *)

  SetDeviceId
  PROCEDURE SetDeviceId (c: ChanId; d: IOLink.DeviceId) ;

  (*
     GetDevicePtr - returns the device table ptr, from, c.
  *)

  GetDevicePtr
  PROCEDURE GetDevicePtr (c: ChanId) : IOLink.DeviceTablePtr ;

  (*
     SetDevicePtr - sets the device table ptr in, c.
  *)

  SetDevicePtr
  PROCEDURE SetDevicePtr (c: ChanId; p: IOLink.DeviceTablePtr) ;

  (*
     GetFile - returns the file field from, c.
  *)

  GetFile
  PROCEDURE GetFile (c: ChanId) : FIO.File ;

  (*
     SetFile - sets the file field in, c.
  *)

  SetFile
  PROCEDURE SetFile (c: ChanId; f: FIO.File) ;

  END RTio.

