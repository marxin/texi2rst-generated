.. _gm2-libs-iso-rtgenif:

gm2-libs-iso/RTgenif
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RTgenif ;

  (*
      Description: provides a generic interface mechanism used
                   by RTgen.  This is not an ISO module but rather
                   a runtime support module.
  *)

  FROM SYSTEM IMPORT ADDRESS ;
  FROM IOLink IMPORT DeviceId, DeviceTablePtr ;

  TYPE
  GenDevIF (type)
     GenDevIF ;
  readchar (type)
     readchar   = PROCEDURE (GenDevIF, DeviceTablePtr) : CHAR ;
  unreadchar (type)
     unreadchar = PROCEDURE (GenDevIF, DeviceTablePtr, CHAR) : CHAR ;
  geterrno (type)
     geterrno   = PROCEDURE (GenDevIF, DeviceTablePtr) : INTEGER ;
  readbytes (type)
     readbytes  = PROCEDURE (GenDevIF, DeviceTablePtr, ADDRESS, CARDINAL, VAR CARDINAL) : BOOLEAN ;
  writebytes (type)
     writebytes = PROCEDURE (GenDevIF, DeviceTablePtr, ADDRESS, CARDINAL, VAR CARDINAL) : BOOLEAN ;
  writeln (type)
     writeln    = PROCEDURE (GenDevIF, DeviceTablePtr) : BOOLEAN ;
  iseof (type)
     iseof      = PROCEDURE (GenDevIF, DeviceTablePtr) : BOOLEAN ;
  iseoln (type)
     iseoln     = PROCEDURE (GenDevIF, DeviceTablePtr) : BOOLEAN ;
  iserror (type)
     iserror    = PROCEDURE (GenDevIF, DeviceTablePtr) : BOOLEAN ;

  (*
     InitGenDevIF - initializes a generic device.
  *)

  InitGenDevIF
  PROCEDURE InitGenDevIF (d     : DeviceId;
                          rc    : readchar;
                          urc   : unreadchar;
                          geterr: geterrno;
                          rbytes: readbytes;
                          wbytes: writebytes;
                          wl    : writeln;
                          eof   : iseof;
                          eoln  : iseoln;
                          iserr : iserror) : GenDevIF ;

  (*
     getDID - returns the device id this generic interface.
  *)

  getDID
  PROCEDURE getDID (g: GenDevIF) : DeviceId ;

  (*
     doReadChar - returns the next character from the generic
                  device.
  *)

  doReadChar
  PROCEDURE doReadChar (g: GenDevIF; d: DeviceTablePtr) : CHAR ;

  (*
     doUnReadChar - pushes back a character to the generic device.
  *)

  doUnReadChar
  PROCEDURE doUnReadChar (g: GenDevIF; d: DeviceTablePtr; ch: CHAR) : CHAR ;

  (*
     doGetErrno - returns the errno relating to the generic device.
  *)

  doGetErrno
  PROCEDURE doGetErrno (g: GenDevIF; d: DeviceTablePtr) : INTEGER ;

  (*
     doRBytes - attempts to read, n, bytes from the generic device.
                It set the actual amount read and returns a boolean
                to determine whether an error occurred.
  *)

  doRBytes
  PROCEDURE doRBytes (g: GenDevIF; d: DeviceTablePtr;
                      to: ADDRESS; max: CARDINAL;
                      VAR actual: CARDINAL) : BOOLEAN ;

  (*
     doWBytes - attempts to write, n, bytes to the generic device.
                It sets the actual amount written and returns a
                boolean to determine whether an error occurred.
  *)

  doWBytes
  PROCEDURE doWBytes (g: GenDevIF; d: DeviceTablePtr;
                      from: ADDRESS; max: CARDINAL;
                      VAR actual: CARDINAL) : BOOLEAN ;

  (*
     doWrLn - writes an end of line marker and returns
              TRUE if successful.
  *)

  doWrLn
  PROCEDURE doWrLn (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;

  (*
     isEOF - returns true if the end of file was reached.
  *)

  isEOF
  PROCEDURE isEOF (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;

  (*
     isEOLN - returns true if the end of line was reached.
  *)

  isEOLN
  PROCEDURE isEOLN (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;

  (*
     isError - returns true if an error was seen in the device.
  *)

  isError
  PROCEDURE isError (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;

  (*
     KillGenDevIF - deallocates a generic device.
  *)

  KillGenDevIF
  PROCEDURE KillGenDevIF (g: GenDevIF) : GenDevIF ;

  END RTgenif.

