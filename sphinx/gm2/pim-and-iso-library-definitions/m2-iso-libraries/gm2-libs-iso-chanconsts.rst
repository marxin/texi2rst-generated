.. _gm2-libs-iso-chanconsts:

gm2-libs-iso/ChanConsts
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ChanConsts;

    (* Common types and values for channel open requests and results *)

  TYPE
  ChanFlags (type)
    ChanFlags =        (* Request flags possibly given when a channel is opened *)
    ( readFlag,        (* input operations are requested/available *)
      writeFlag,       (* output operations are requested/available *)
      oldFlag,         (* a file may/must/did exist before the channel is opened *)
      textFlag,        (* text operations are requested/available *)
      rawFlag,         (* raw operations are requested/available *)
      interactiveFlag, (* interactive use is requested/applies *)
      echoFlag         (* echoing by interactive device on removal of characters from input
                          stream requested/applies *)
    );

  FlagSet (type)
    FlagSet = SET OF ChanFlags;

    (* Singleton values of FlagSet, to allow for example, read + write *)

  CONST
  read  (const)
    read = FlagSet{readFlag};   (* input operations are requested/available *)
  write  (const)
    write = FlagSet{writeFlag}; (* output operations are requested/available *)
  old  (const)
    old = FlagSet{oldFlag};     (* a file may/must/did exist before the channel is opened *)
  text  (const)
    text = FlagSet{textFlag};   (* text operations are requested/available *)
  raw  (const)
    raw = FlagSet{rawFlag};     (* raw operations are requested/available *)
  interactive  (const)
    interactive = FlagSet{interactiveFlag}; (* interactive use is requested/applies *)
  echo  (const)
    echo = FlagSet{echoFlag};   (* echoing by interactive device on removal of characters from
                                   input stream requested/applies *)

  TYPE
  OpenResults (type)
    OpenResults =        (* Possible results of open requests *)
      (opened,           (* the open succeeded as requested *)
       wrongNameFormat,  (* given name is in the wrong format for the implementation *)
       wrongFlags,       (* given flags include a value that does not apply to the device *)
       tooManyOpen,      (* this device cannot support any more open channels *)
       outOfChans,       (* no more channels can be allocated *)
       wrongPermissions, (* file or directory permissions do not allow request *)
       noRoomOnDevice,   (* storage limits on the device prevent the open *)
       noSuchFile,       (* a needed file does not exist *)
       fileExists,       (* a file of the given name already exists when a new one is required *)
       wrongFileType,    (* the file is of the wrong type to support the required operations *)
       noTextOperations, (* text operations have been requested, but are not supported *)
       noRawOperations,  (* raw operations have been requested, but are not supported *)
       noMixedOperations,(* text and raw operations have been requested, but they
                            are not supported in combination *)
       alreadyOpen,      (* the source/destination is already open for operations not supported
                            in combination with the requested operations *)
       otherProblem      (* open failed for some other reason *)
      );

  END ChanConsts.

