.. _gm2-libs-iso-serversocket:

gm2-libs-iso/ServerSocket
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ServerSocket ;

  (*
      Description: provides a mechanism to open a server socket
                   as an ISO Modula-2 channel.
  *)

  FROM IOChan IMPORT ChanId ;
  FROM ChanConsts IMPORT FlagSet, OpenResults ;

  (*
     OpenSocketBindListen - opens a TCP server socket.  The socket
                            is bound to, port, and will allow, listen,
                            pending connections.  The result of these
                            combined operations is returned in, res.
  *)

  OpenSocketBindListen
  PROCEDURE OpenSocketBindListen (VAR socketid: ChanId;
                                  port: CARDINAL; listen: CARDINAL;
                                  VAR res: OpenResults) ;

  (*
     OpenAccept - attempts to open a new channel whose
                  input/output capability is determined by,
                  flags.  The result of this attempt is returned
                  in res.
  *)

  OpenAccept
  PROCEDURE OpenAccept (VAR cid: ChanId; socketid: ChanId;
                        flags: FlagSet; VAR res: OpenResults) ;

  (*
     Close - if the channel identified by cid was not opened as
             a server socket stream, the exception wrongDevice is
             raised; otherwise closes the channel, and assigns
             the value identifying the invalid channel to cid.
  *)

  Close
  PROCEDURE Close (VAR cid: ChanId) ;

  (*
     IsSocket - tests if the channel identified by cid is open as
                a server socket stream.
  *)

  IsSocket
  PROCEDURE IsSocket (cid: ChanId) : BOOLEAN ;

  END ServerSocket.

