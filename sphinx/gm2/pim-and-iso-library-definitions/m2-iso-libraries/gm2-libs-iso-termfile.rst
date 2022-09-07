.. _gm2-libs-iso-termfile:

gm2-libs-iso/TermFile
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE TermFile;

    (* Access to the terminal device *)

    (* Channels opened by this module are connected to a single
       terminal device; typed characters are distributed between
       channels according to the sequence of read requests.
    *)

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
    read = FlagSet{ChanConsts.readFlag};
    (* input operations are requested/available *)
  write  (const)
    write = FlagSet{ChanConsts.writeFlag};
    (* output operations are requested/available *)
  text  (const)
    text = FlagSet{ChanConsts.textFlag};
    (* text operations are requested/available *)
  raw  (const)
    raw = FlagSet{ChanConsts.rawFlag};
    (* raw operations are requested/available *)
  echo  (const)
    echo = FlagSet{ChanConsts.echoFlag};
    (* echoing by interactive device on reading of
       characters from input stream requested/applies
    *)

  Open
  PROCEDURE Open (VAR cid: ChanId; flagset: FlagSet; VAR res: OpenResults);
    (* Attempts to obtain and open a channel connected to
       the terminal.  Without the raw flag, text is implied.
       Without the echo flag, line mode is requested,
       otherwise single character mode is requested.
       If successful, assigns to cid the identity of
       the opened channel, and assigns the value opened to res.
       If a channel cannot be opened as required, the value of
       res indicates the reason, and cid identifies the
       invalid channel.
    *)

  IsTermFile
  PROCEDURE IsTermFile (cid: ChanId): BOOLEAN;
    (* Tests if the channel identified by cid is open to
       the terminal. *)

  Close
  PROCEDURE Close (VAR cid: ChanId);
    (* If the channel identified by cid is not open to the terminal,
       the exception wrongDevice is raised; otherwise closes the
       channel and assigns the value identifying the invalid channel
       to cid.
    *)

  END TermFile.

