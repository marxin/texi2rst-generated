.. _gm2-libs-iso-streamfile:

gm2-libs-iso/StreamFile
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE StreamFile;

    (* Independent sequential data streams *)

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
  read  (const)
    read = FlagSet{ChanConsts.readFlag};   (* input operations are requested/available *)
  write  (const)
    write = FlagSet{ChanConsts.writeFlag}; (* output operations are requested/available *)
  old  (const)
    old = FlagSet{ChanConsts.oldFlag};     (* a file may/must/did exist before the channel is
                                              opened *)
  text  (const)
    text = FlagSet{ChanConsts.textFlag};   (* text operations are requested/available *)
  raw  (const)
    raw = FlagSet{ChanConsts.rawFlag};     (* raw operations are requested/available *)

  Open
  PROCEDURE Open (VAR cid: ChanId; name: ARRAY OF CHAR;
                  flags: FlagSet; VAR res: OpenResults);
    (* Attempts to obtain and open a channel connected to a
       sequential stream of the given name.
       The read flag implies old; without the raw flag, text is
       implied.  If successful, assigns to cid the identity of
       the opened channel, and assigns the value opened to res.
       If a channel cannot be opened as required, the value of
       res indicates the reason, and cid identifies the invalid
       channel.
    *)

  IsStreamFile
  PROCEDURE IsStreamFile (cid: ChanId): BOOLEAN;
    (* Tests if the channel identified by cid is open to a sequential stream. *)

  Close
  PROCEDURE Close (VAR cid: ChanId);
    (* If the channel identified by cid is not open to a sequential stream, the exception
       wrongDevice is raised; otherwise closes the channel, and assigns the value identifying
       the invalid channel to cid.
    *)

  END StreamFile.

