.. _gm2-libs-iso-seqfile:

gm2-libs-iso/SeqFile
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SeqFile;

    (* Rewindable sequential files *)

  IMPORT IOChan, ChanConsts;

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

  OpenWrite
  PROCEDURE OpenWrite (VAR cid: ChanId; name: ARRAY OF CHAR;
                       flags: FlagSet; VAR res: OpenResults);
    (*
       Attempts to obtain and open a channel connected to a stored
       rewindable file of the given name.
       The write flag is implied; without the raw flag, text is
       implied.  If successful, assigns to cid the identity of
       the opened channel, assigns the value opened to res, and
       selects output mode, with the write position at the start
       of the file (i.e. the file is of zero length).
       If a channel cannot be opened as required, the value of
       res indicates the reason, and cid identifies the invalid
       channel.
    *)

  OpenAppend
  PROCEDURE OpenAppend (VAR cid: ChanId; name: ARRAY OF CHAR;
                        flags: FlagSet; VAR res: OpenResults);
    (*
       Attempts to obtain and open a channel connected to a stored
       rewindable file of the given name.  The write and old flags
       are implied; without the raw flag, text is implied.  If
       successful, assigns to cid the identity of the opened channel,
       assigns the value opened to res, and selects output mode,
       with the write position corresponding to the length of the
       file.  If a channel cannot be opened as required, the value
       of res indicates the reason, and cid identifies the invalid
       channel.
    *)

  OpenRead
  PROCEDURE OpenRead (VAR cid: ChanId; name: ARRAY OF CHAR;
                      flags: FlagSet; VAR res: OpenResults);
    (* Attempts to obtain and open a channel connected to a stored
       rewindable file of the given name.
       The read and old flags are implied; without the raw flag,
       text is implied.  If successful, assigns to cid the
       identity of the opened channel, assigns the value opened to
       res, and selects input mode, with the read position
       corresponding to the start of the file.
       If a channel cannot be opened as required, the value of
       res indicates the reason, and cid identifies the invalid
       channel.
    *)

  IsSeqFile
  PROCEDURE IsSeqFile (cid: ChanId): BOOLEAN;
    (* Tests if the channel identified by cid is open to a
       rewindable sequential file. *)

  Reread
  PROCEDURE Reread (cid: ChanId);
    (* If the channel identified by cid is not open to a rewindable
       sequential file, the exception wrongDevice is raised;
       otherwise attempts to set the read position to the
       start of the file, and to select input mode.
       If the operation cannot be performed (perhaps because of
       insufficient permissions) neither input mode nor output
       mode is selected.
    *)

  Rewrite
  PROCEDURE Rewrite (cid: ChanId);
    (* If the channel identified by cid is not open to a
       rewindable sequential file, the exception wrongDevice is
       raised; otherwise, attempts to truncate the file to zero
       length, and to select output mode.  If the operation
       cannot be performed (perhaps because of insufficient
       permissions) neither input mode nor output mode is selected.
    *)

  Close
  PROCEDURE Close (VAR cid: ChanId);
    (* If the channel identified by cid is not open to a rewindable
       sequential file, the exception wrongDevice is raised;
       otherwise closes the channel, and assigns the value
       identifying the invalid channel to cid.
    *)

  END SeqFile.

