.. _gm2-libs-iso-iolink:

gm2-libs-iso/IOLink
^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE IOLink;

  (* Types and procedures for the standard implementation of channels *)

  IMPORT IOChan, IOConsts, ChanConsts, SYSTEM;

  TYPE
    DeviceId;
      (* Values of this type are used to identify new device modules,
         and are normally obtained by them during their initialization.
      *)

  AllocateDeviceId
  PROCEDURE AllocateDeviceId (VAR did: DeviceId);
    (* Allocates a unique value of type DeviceId, and assigns this
       value to did. *)

  MakeChan
  PROCEDURE MakeChan (did: DeviceId; VAR cid: IOChan.ChanId);
    (* Attempts to make a new channel for the device module identified
       by did. If no more channels can be made, the identity of
       the invalid channel is assigned to cid.  Otherwise, the identity
       of a new channel is assigned to cid.
    *)

  UnMakeChan
  PROCEDURE UnMakeChan (did: DeviceId; VAR cid: IOChan.ChanId);
    (* If the device module identified by did is not the module that
       made the channel identified by cid, the exception wrongDevice is
       raised; otherwise the channel is deallocated, and the value
       identifying the invalid channel is assigned to cid.
    *)

  TYPE
  DeviceTablePtr (type)
    DeviceTablePtr = POINTER TO DeviceTable;
      (* Values of this type are used to refer to device tables *)

  TYPE
  LookProc (type)
    LookProc      = PROCEDURE (DeviceTablePtr, VAR CHAR, VAR IOConsts.ReadResults) ;
  SkipProc (type)
    SkipProc      = PROCEDURE (DeviceTablePtr) ;
  SkipLookProc (type)
    SkipLookProc  = PROCEDURE (DeviceTablePtr, VAR CHAR, VAR IOConsts.ReadResults) ;
  WriteLnProc (type)
    WriteLnProc   = PROCEDURE (DeviceTablePtr) ;
  TextReadProc (type)
    TextReadProc  = PROCEDURE (DeviceTablePtr, SYSTEM.ADDRESS, CARDINAL, VAR CARDINAL) ;
  TextWriteProc (type)
    TextWriteProc = PROCEDURE (DeviceTablePtr, SYSTEM.ADDRESS, CARDINAL) ;
  RawReadProc (type)
    RawReadProc   = PROCEDURE (DeviceTablePtr, SYSTEM.ADDRESS, CARDINAL, VAR CARDINAL) ;
  RawWriteProc (type)
    RawWriteProc  = PROCEDURE (DeviceTablePtr, SYSTEM.ADDRESS, CARDINAL) ;
  GetNameProc (type)
    GetNameProc   = PROCEDURE (DeviceTablePtr, VAR ARRAY OF CHAR) ;
  ResetProc (type)
    ResetProc     = PROCEDURE (DeviceTablePtr) ;
  FlushProc (type)
    FlushProc     = PROCEDURE (DeviceTablePtr) ;
  FreeProc (type)
    FreeProc      = PROCEDURE (DeviceTablePtr) ;
       (* Carry out the operations involved in closing the corresponding
          channel, including flushing buffers, but do not unmake the
          channel.
       *)

  TYPE
  DeviceData (type)
    DeviceData = SYSTEM.ADDRESS;

  DeviceTable (type)
    DeviceTable =
      RECORD                         (* Initialized by MakeChan to: *)
        cd: DeviceData;              (* the value NIL *)
        did: DeviceId;               (* the value given in the call of MakeChan *)
        cid: IOChan.ChanId;          (* the identity of the channel *)
        result: IOConsts.ReadResults;(* the value notKnown *)
        errNum: IOChan.DeviceErrNum; (* undefined *)
        flags: ChanConsts.FlagSet;   (* ChanConsts.FlagSet{} *)
        doLook: LookProc;            (* raise exception notAvailable *)
        doSkip: SkipProc;            (* raise exception notAvailable *)
        doSkipLook: SkipLookProc;    (* raise exception notAvailable *)
        doLnWrite: WriteLnProc;      (* raise exception notAvailable *)
        doTextRead: TextReadProc;    (* raise exception notAvailable *)
        doTextWrite: TextWriteProc;  (* raise exception notAvailable *)
        doRawRead: RawReadProc;      (* raise exception notAvailable *)
        doRawWrite: RawWriteProc;    (* raise exception notAvailable *)
        doGetName: GetNameProc;      (* return the empty string *)
        doReset: ResetProc;          (* do nothing *)
        doFlush: FlushProc;          (* do nothing *)
        doFree: FreeProc;            (* do nothing *)
      END;

    (* The pointer to the device table for a channel is obtained using the
       following procedure: *)

  (*
     If the device module identified by did is not the module that made
     the channel identified by cid, the exception wrongDevice is raised.
  *)

  DeviceTablePtrValue
  PROCEDURE DeviceTablePtrValue (cid: IOChan.ChanId; did: DeviceId): DeviceTablePtr;

  (*
     Tests if the device module identified by did is the module
     that made the channel identified by cid.
  *)

  IsDevice
  PROCEDURE IsDevice (cid: IOChan.ChanId; did: DeviceId) : BOOLEAN;

  TYPE
  DevExceptionRange (type)
    DevExceptionRange = IOChan.ChanExceptions;

  (*
    ISO standard states defines

    DevExceptionRange = [IOChan.notAvailable ..  IOChan.textParseError];

    however this must be a bug as other modules need to raise
    IOChan.wrongDevice exceptions.
  *)

  RAISEdevException
  PROCEDURE RAISEdevException (cid: IOChan.ChanId; did: DeviceId;
                               x: DevExceptionRange; s: ARRAY OF CHAR);

    (* If the device module identified by did is not the module that made the channel
       identified by cid, the exception wrongDevice is raised; otherwise the given exception
       is raised, and the string value in s is included in the exception message.
    *)

  IsIOException
  PROCEDURE IsIOException () : BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional execution state
       because of the raising af an exception from ChanExceptions;
       otherwise FALSE.
    *)

  IOException
  PROCEDURE IOException () : IOChan.ChanExceptions;
    (* If the current coroutine is in the exceptional execution state because of the
       raising af an exception from ChanExceptions, returns the corresponding
       enumeration value, and otherwise raises an exception.
    *)

  END IOLink.

