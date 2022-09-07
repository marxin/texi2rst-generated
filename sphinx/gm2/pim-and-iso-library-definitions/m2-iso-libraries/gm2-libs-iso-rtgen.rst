.. _gm2-libs-iso-rtgen:

gm2-libs-iso/RTgen
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RTgen ;

  (*
      Description: provides a generic device interface between
                   ISO channels and the underlying PIM style
                   FIO procedure calls.
  *)

  FROM RTgenif IMPORT GenDevIF ;
  FROM IOLink IMPORT DeviceId, DeviceTablePtr;
  FROM IOConsts IMPORT ReadResults ;
  FROM SYSTEM IMPORT ADDRESS ;

  TYPE
  ChanDev (type)
     ChanDev ;
  DeviceType (type)
     DeviceType = (seqfile, streamfile, programargs, stdchans, term, socket, rndfile) ;

  (*
     InitChanDev - initialize and return a ChanDev.
  *)

  InitChanDev
  PROCEDURE InitChanDev (t: DeviceType; d: DeviceId; g: GenDevIF) : ChanDev ;

  (*
     KillChanDev - deallocates, g.
  *)

  KillChanDev
  PROCEDURE KillChanDev (g: GenDevIF) : GenDevIF ;

  (*
     RaiseEOFinLook - returns TRUE if the Look procedure
                      should raise an exception if it
                      sees end of file.
  *)

  RaiseEOFinLook
  PROCEDURE RaiseEOFinLook (g: ChanDev) : BOOLEAN ;

  (*
     RaiseEOFinSkip - returns TRUE if the Skip procedure
                      should raise an exception if it
                      sees end of file.
  *)

  RaiseEOFinSkip
  PROCEDURE RaiseEOFinSkip (g: ChanDev) : BOOLEAN ;

  doLook
  PROCEDURE doLook (g: ChanDev;
                    d: DeviceTablePtr;
                    VAR ch: CHAR;
                    VAR r: ReadResults) ;

  doSkip
  PROCEDURE doSkip (g: ChanDev;
                    d: DeviceTablePtr) ;

  doSkipLook
  PROCEDURE doSkipLook (g: ChanDev;
                        d: DeviceTablePtr;
                        VAR ch: CHAR;
                        VAR r: ReadResults) ;

  doWriteLn
  PROCEDURE doWriteLn (g: ChanDev;
                       d: DeviceTablePtr) ;

  doReadText
  PROCEDURE doReadText (g: ChanDev;
                        d: DeviceTablePtr;
                        to: ADDRESS;
                        maxChars: CARDINAL;
                        VAR charsRead: CARDINAL) ;

  doWriteText
  PROCEDURE doWriteText (g: ChanDev;
                         d: DeviceTablePtr;
                         from: ADDRESS;
                         charsToWrite: CARDINAL) ;

  doReadLocs
  PROCEDURE doReadLocs (g: ChanDev;
                        d: DeviceTablePtr;
                        to: ADDRESS;
                        maxLocs: CARDINAL;
                        VAR locsRead: CARDINAL) ;

  doWriteLocs
  PROCEDURE doWriteLocs (g: ChanDev;
                         d: DeviceTablePtr;
                         from: ADDRESS;
                         locsToWrite: CARDINAL) ;

  (*
     checkErrno - checks a number of errno conditions and raises
                  appropriate ISO exceptions if they occur.
  *)

  checkErrno
  PROCEDURE checkErrno (g: ChanDev; d: DeviceTablePtr) ;

  END RTgen.

