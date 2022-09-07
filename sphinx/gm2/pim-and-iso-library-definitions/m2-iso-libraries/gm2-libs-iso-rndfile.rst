.. _gm2-libs-iso-rndfile:

gm2-libs-iso/RndFile
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RndFile;

    (* Random access files *)

  IMPORT IOChan, ChanConsts, SYSTEM;

  TYPE
  ChanId (type)
     ChanId = IOChan.ChanId;
  FlagSet (type)
     FlagSet = ChanConsts.FlagSet;
  OpenResults (type)
     OpenResults = ChanConsts.OpenResults;

     (* Accepted singleton values of FlagSet *)

  CONST
     (* input operations are requested/available *)
  read  (const)
     read = FlagSet{ChanConsts.readFlag};
     (* output operations are requested/available *)
  write  (const)
     write = FlagSet{ChanConsts.writeFlag};
     (* a file may/must/did exist before the channel is opened *)
  old  (const)
     old = FlagSet{ChanConsts.oldFlag};
     (* text operations are requested/available *)
  text  (const)
     text = FlagSet{ChanConsts.textFlag};
     (* raw operations are requested/available *)
  raw  (const)
     raw = FlagSet{ChanConsts.rawFlag};

  OpenOld
  PROCEDURE OpenOld (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                     VAR res: OpenResults);
    (* Attempts to obtain and open a channel connected to a stored random
       access file of the given name.
       The old flag is implied; without the write flag, read is implied;
       without the text flag, raw is implied.
       If successful, assigns to cid the identity of the opened channel,
       assigns the value opened to res, and sets the read/write position
       to the start of the file.
       If a channel cannot be opened as required, the value of res indicates
       the reason, and cid identifies the invalid channel.
    *)

  OpenClean
  PROCEDURE OpenClean (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                       VAR res: OpenResults);
    (* Attempts to obtain and open a channel connected to a stored random
       access file of the given name.
       The write flag is implied; without the text flag, raw is implied.
       If successful, assigns to cid the identity of the opened channel,
       assigns the value opened to res, and truncates the file to zero length.
       If a channel cannot be opened as required, the value of res indicates
       the reason, and cid identifies the invalid channel.
    *)

  IsRndFile
  PROCEDURE IsRndFile (cid: ChanId): BOOLEAN;
    (* Tests if the channel identified by cid is open to a random access file. *)

  IsRndFileException
  PROCEDURE IsRndFileException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional execution
       state because of the raising of a RndFile exception; otherwise returns
       FALSE.
    *)

  CONST
  FilePosSize  (const)
     FilePosSize = SIZE(LONGINT) ;
     (* <implementation-defined whole number greater than zero>; *)

  TYPE
  FilePos (type)
     FilePos = LONGINT ;  (* ARRAY [1 .. FilePosSize] OF SYSTEM.LOC; *)

  StartPos
  PROCEDURE StartPos (cid: ChanId): FilePos;
    (* If the channel identified by cid is not open to a random access file,
       the exception wrongDevice is raised; otherwise returns the position of
       the start of the file.
    *)

  CurrentPos
  PROCEDURE CurrentPos (cid: ChanId): FilePos;
    (* If the channel identified by cid is not open to a random access file,
       the exception wrongDevice is raised; otherwise returns the position
       of the current read/write position.
    *)

  EndPos
  PROCEDURE EndPos (cid: ChanId): FilePos;
    (* If the channel identified by cid is not open to a random access file,
       the exception wrongDevice is raised; otherwise returns the first
       position after which there have been no writes.
    *)

  NewPos
  PROCEDURE NewPos (cid: ChanId; chunks: INTEGER; chunkSize: CARDINAL;
                    from: FilePos): FilePos;
    (* If the channel identified by cid is not open to a random access file,
       the exception wrongDevice is raised; otherwise returns the position
       (chunks * chunkSize) relative to the position given by from, or
       raises the exception posRange if the required position cannot be
       represented as a value of type FilePos.
    *)

  SetPos
  PROCEDURE SetPos (cid: ChanId; pos: FilePos);
    (* If the channel identified by cid is not open to a random access file,
       the exception wrongDevice is raised; otherwise sets the read/write
       position to the value given by pos.
    *)

  Close
  PROCEDURE Close (VAR cid: ChanId);
    (* If the channel identified by cid is not open to a random access file,
       the exception wrongDevice is raised; otherwise closes the channel,
       and assigns the value identifying the invalid channel to cid.
    *)

  END RndFile.

