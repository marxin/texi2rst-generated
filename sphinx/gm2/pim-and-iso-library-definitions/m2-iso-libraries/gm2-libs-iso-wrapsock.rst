.. _gm2-libs-iso-wrapsock:

gm2-libs-iso/wrapsock
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE wrapsock ;

  (*
      Description: provides a set of wrappers to some client side
                   tcp socket primatives.
  *)

  FROM SYSTEM IMPORT ADDRESS ;
  FROM ChanConsts IMPORT OpenResults ;

  TYPE
  clientInfo (type)
     clientInfo = ADDRESS ;

  (*
     clientOpen - returns an ISO Modula-2 OpenResult.
                  It attempts to connect to:  hostname:portNo.
                  If successful then the data structure, c,
                  will have its fields initialized.
  *)

  clientOpen
  PROCEDURE clientOpen (c: clientInfo;
                        hostname: ADDRESS;
                        length: CARDINAL;
                        portNo: CARDINAL) : OpenResults ;

  (*
     clientOpenIP - returns an ISO Modula-2 OpenResult.
                    It attempts to connect to:  ipaddress:portNo.
                    If successful then the data structure, c,
                    will have its fields initialized.
  *)

  clientOpenIP
  PROCEDURE clientOpenIP (c: clientInfo;
                          ip: CARDINAL;
                          portNo: CARDINAL) : OpenResults ;

  (*
     getClientPortNo - returns the portNo from structure, c.
  *)

  getClientPortNo
  PROCEDURE getClientPortNo (c: clientInfo) : CARDINAL ;

  (*
     getClientHostname - fills in the hostname of the server
                         the to which the client is connecting.
  *)

  getClientHostname
  PROCEDURE getClientHostname (c: clientInfo;
                               hostname: ADDRESS; high: CARDINAL) ;

  (*
     getClientSocketFd - returns the sockFd from structure, c.
  *)

  getClientSocketFd
  PROCEDURE getClientSocketFd (c: clientInfo) : INTEGER ;

  (*
     getClientIP - returns the sockFd from structure, s.
  *)

  getClientIP
  PROCEDURE getClientIP (c: clientInfo) : CARDINAL ;

  (*
     getPushBackChar - returns TRUE if a pushed back character
                       is available.
  *)

  getPushBackChar
  PROCEDURE getPushBackChar (c: clientInfo; VAR ch: CHAR) : BOOLEAN ;

  (*
     setPushBackChar - returns TRUE if it is able to push back a
                       character.
  *)

  setPushBackChar
  PROCEDURE setPushBackChar (c: clientInfo; ch: CHAR) : BOOLEAN ;

  (*
     getSizeOfClientInfo - returns the sizeof (opaque data type).
  *)

  getSizeOfClientInfo
  PROCEDURE getSizeOfClientInfo () : CARDINAL ;

  END wrapsock.

