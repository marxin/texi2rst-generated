.. _gm2-libs-iso-stdchans:

gm2-libs-iso/StdChans
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE StdChans;

    (* Access to standard and default channels *)

  IMPORT IOChan;

  TYPE
  ChanId (type)
    ChanId = IOChan.ChanId;
      (* Values of this type are used to identify channels *)

    (* The following functions return the standard channel values.
       These channels cannot be closed.
    *)

  StdInChan
  PROCEDURE StdInChan (): ChanId;
    (* Returns the identity of the implementation-defined standard source for
  program
       input.
    *)

  StdOutChan
  PROCEDURE StdOutChan (): ChanId;
    (* Returns the identity of the implementation-defined standard source for program
       output.
    *)

  StdErrChan
  PROCEDURE StdErrChan (): ChanId;
    (* Returns the identity of the implementation-defined standard destination for program
       error messages.
    *)

  NullChan
  PROCEDURE NullChan (): ChanId;
    (* Returns the identity of a channel open to the null device. *)

    (* The following functions return the default channel values *)

  InChan
  PROCEDURE InChan (): ChanId;
    (* Returns the identity of the current default input channel. *)

  OutChan
  PROCEDURE OutChan (): ChanId;
    (* Returns the identity of the current default output channel. *)

  ErrChan
  PROCEDURE ErrChan (): ChanId;
    (* Returns the identity of the current default error message channel. *)

    (* The following procedures allow for redirection of the default channels *)

  SetInChan
  PROCEDURE SetInChan (cid: ChanId);
    (* Sets the current default input channel to that identified by cid. *)

  SetOutChan
  PROCEDURE SetOutChan (cid: ChanId);
    (* Sets the current default output channel to that identified by cid. *)

  SetErrChan
  PROCEDURE SetErrChan (cid: ChanId);
    (* Sets the current default error channel to that identified by cid. *)

  END StdChans.

