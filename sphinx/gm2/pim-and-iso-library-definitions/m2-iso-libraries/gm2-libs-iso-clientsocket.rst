.. _gm2-libs-iso-clientsocket:

gm2-libs-iso/ClientSocket
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ClientSocket ;

  FROM IOChan IMPORT ChanId ;
  FROM ChanConsts IMPORT FlagSet, OpenResults ;

  (*
     OpenSocket - opens a TCP client connection to host:port.
  *)

  OpenSocket
  PROCEDURE OpenSocket (VAR cid: ChanId;
                        host: ARRAY OF CHAR; port: CARDINAL;
                        f: FlagSet; VAR res: OpenResults) ;

  (*
     Close - if the channel identified by cid is not open to
             a socket stream, the exception wrongDevice is
             raised; otherwise closes the channel, and assigns
             the value identifying the invalid channel to cid.
  *)

  Close
  PROCEDURE Close (VAR cid: ChanId) ;

  (*
     IsSocket - tests if the channel identified by cid is open as
                a client socket stream.
  *)

  IsSocket
  PROCEDURE IsSocket (cid: ChanId) : BOOLEAN ;

  END ClientSocket.

