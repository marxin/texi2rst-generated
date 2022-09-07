.. _gm2-libs-iso-memstream:

gm2-libs-iso/MemStream
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE MemStream ;

  (*
      Description: provides an ISO module which can write to a memory
                   buffer or read from a memory buffer.
  *)

  FROM IOChan IMPORT ChanId ;
  FROM ChanConsts IMPORT FlagSet, OpenResults ;
  FROM SYSTEM IMPORT ADDRESS, LOC ;

  (*
     Attempts to obtain and open a channel connected to a contigeous
     buffer in memory.  The write flag is implied; without the raw
     flag, text is implied.  If successful, assigns to cid the identity of
     the opened channel, assigns the value opened to res.
     If a channel cannot be opened as required,
     the value of res indicates the reason, and cid identifies the
     invalid channel.

     The parameters, buffer, length and used maybe updated as
     data is written.  The buffer maybe reallocated
     and its address might alter, however the parameters will
     always reflect the current active buffer.  When this
     channel is closed the buffer is deallocated and
     buffer will be set to NIL, length and used will be set to
     zero.
  *)

  OpenWrite
  PROCEDURE OpenWrite (VAR cid: ChanId; flags: FlagSet;
                       VAR res: OpenResults;
                       VAR buffer: ADDRESS;
                       VAR length: CARDINAL;
                       VAR used: CARDINAL;
                       deallocOnClose: BOOLEAN) ;

  (*
     Attempts to obtain and open a channel connected to a contigeous
     buffer in memory.  The read and old flags are implied; without
     the raw flag, text is implied.  If successful, assigns to cid the
     identity of the opened channel, assigns the value opened to res, and
     selects input mode, with the read position corresponding to the start
     of the buffer.  If a channel cannot be opened as required, the value of
     res indicates the reason, and cid identifies the invalid channel.
  *)

  OpenRead
  PROCEDURE OpenRead (VAR cid: ChanId; flags: FlagSet;
                      VAR res: OpenResults;
                      buffer: ADDRESS; length: CARDINAL;
                      deallocOnClose: BOOLEAN) ;

  (*
     Close - if the channel identified by cid is not open to
             a memory stream, the exception wrongDevice is
             raised; otherwise closes the channel, and assigns
             the value identifying the invalid channel to cid.
  *)

  Close
  PROCEDURE Close (VAR cid: ChanId) ;

  (*
     Rewrite - assigns the buffer index to zero.  Subsequent
               writes will overwrite the previous buffer contents.
  *)

  Rewrite
  PROCEDURE Rewrite (cid: ChanId) ;

  (*
     Reread - assigns the buffer index to zero.  Subsequent
              reads will read the previous buffer contents.
  *)

  Reread
  PROCEDURE Reread (cid: ChanId) ;

  (*
     IsMem - tests if the channel identified by cid is open as
             a memory stream.
  *)

  IsMem
  PROCEDURE IsMem (cid: ChanId) : BOOLEAN ;

  END MemStream.

