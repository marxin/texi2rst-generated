.. _gm2-libs-sckt:

gm2-libs/sckt
^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE sckt ;

  FROM SYSTEM IMPORT ADDRESS ;
  EXPORT UNQUALIFIED tcpServerState,
                     tcpServerEstablish, tcpServerEstablishPort,
                     tcpServerAccept, getLocalIP,
                     tcpServerPortNo, tcpServerIP, tcpServerSocketFd,
                     tcpServerClientIP, tcpServerClientPortNo,
                     tcpClientState,
                     tcpClientSocket, tcpClientSocketIP, tcpClientConnect,
                     tcpClientPortNo, tcpClientIP, tcpClientSocketFd ;

  TYPE
  tcpServerState (type)
     tcpServerState = ADDRESS ;
  tcpClientState (type)
     tcpClientState = ADDRESS ;

  (*
     tcpServerEstablish - returns a tcpState containing the relevant
                          information about a socket declared to receive
                          tcp connections.
  *)

  tcpServerEstablish
  PROCEDURE tcpServerEstablish () : tcpServerState ;

  (*
     tcpServerEstablishPort - returns a tcpState containing the relevant
                              information about a socket declared to receive
                              tcp connections.  This method attempts to use
                              the port specified by the parameter.
  *)

  tcpServerEstablishPort
  PROCEDURE tcpServerEstablishPort (port: CARDINAL) : tcpServerState ;

  (*
     tcpServerAccept - returns a file descriptor once a client has connected and
                       been accepted.
  *)

  tcpServerAccept
  PROCEDURE tcpServerAccept (s: tcpServerState) : INTEGER ;

  (*
     tcpServerPortNo - returns the portNo from structure, s.
  *)

  tcpServerPortNo
  PROCEDURE tcpServerPortNo (s: tcpServerState) : CARDINAL ;

  (*
     tcpSocketFd - returns the sockFd from structure, s.
  *)

  tcpServerSocketFd
  PROCEDURE tcpServerSocketFd (s: tcpServerState) : INTEGER ;

  (*
     getLocalIP - returns the IP address of this machine.
  *)

  getLocalIP
  PROCEDURE getLocalIP (s: tcpServerState) : CARDINAL ;

  (*
     tcpServerIP - returns the IP address from structure, s.
  *)

  tcpServerIP
  PROCEDURE tcpServerIP (s: tcpServerState) : CARDINAL ;

  (*
     tcpServerClientIP - returns the IP address of the client who
                         has connected to server, s.
  *)

  tcpServerClientIP
  PROCEDURE tcpServerClientIP (s: tcpServerState) : CARDINAL ;

  (*
     tcpServerClientPortNo - returns the port number of the client who
                             has connected to server, s.
  *)

  tcpServerClientPortNo
  PROCEDURE tcpServerClientPortNo (s: tcpServerState) : CARDINAL ;

  (*
     tcpClientSocket - returns a file descriptor (socket) which has
                       connected to, serverName:portNo.
  *)

  tcpClientSocket
  PROCEDURE tcpClientSocket (serverName: ADDRESS; portNo: CARDINAL) : tcpClientState ;

  (*
     tcpClientSocketIP - returns a file descriptor (socket) which has
                         connected to, ip:portNo.
  *)

  tcpClientSocketIP
  PROCEDURE tcpClientSocketIP (ip: CARDINAL; portNo: CARDINAL) : tcpClientState ;

  (*
     tcpClientConnect - returns the file descriptor associated with, s,
                        once a connect has been performed.
  *)

  tcpClientConnect
  PROCEDURE tcpClientConnect (s: tcpClientState) : INTEGER ;

  (*
     tcpClientPortNo - returns the portNo from structure, s.
  *)

  tcpClientPortNo
  PROCEDURE tcpClientPortNo (s: tcpClientState) : INTEGER ;

  (*
     tcpClientSocketFd - returns the sockFd from structure, s.
  *)

  tcpClientSocketFd
  PROCEDURE tcpClientSocketFd (s: tcpClientState) : INTEGER ;

  (*
     tcpClientIP - returns the IP address from structure, s.
  *)

  tcpClientIP
  PROCEDURE tcpClientIP (s: tcpClientState) : CARDINAL ;

  END sckt.

