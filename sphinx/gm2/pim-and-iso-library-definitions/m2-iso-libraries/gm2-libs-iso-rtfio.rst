.. _gm2-libs-iso-rtfio:

gm2-libs-iso/RTfio
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RTfio ;

  (*
      Description: provides default FIO based methods for the RTgenif
                   procedures.  These will be used by StreamFile,
                   SeqFile, StdChans, TermFile and RndFile.
  *)

  FROM SYSTEM IMPORT ADDRESS ;
  FROM IOLink IMPORT DeviceTablePtr;
  FROM RTgenif IMPORT GenDevIF ;

  (*
     doreadchar - returns a CHAR from the file associated with, g.
  *)

  doreadchar
  PROCEDURE doreadchar (g: GenDevIF; d: DeviceTablePtr) : CHAR ;

  (*
     dounreadchar - pushes a CHAR back onto the file associated
                    with, g.
  *)

  dounreadchar
  PROCEDURE dounreadchar (g: GenDevIF; d: DeviceTablePtr; ch: CHAR) : CHAR ;

  (*
     dogeterrno - returns the errno relating to the generic device.
  *)

  dogeterrno
  PROCEDURE dogeterrno (g: GenDevIF; d: DeviceTablePtr) : INTEGER ;

  (*
     dorbytes - reads upto, max, bytes setting, actual, and
                returning FALSE if an error (not due to eof)
                occurred.
  *)

  dorbytes
  PROCEDURE dorbytes (g: GenDevIF;
                      d: DeviceTablePtr;
                      to: ADDRESS;
                      max: CARDINAL;
                      VAR actual: CARDINAL) : BOOLEAN ;

  (*
     dowbytes - writes up to, nBytes.  It returns FALSE
                if an error occurred and it sets actual
                to the amount of data written.
  *)

  dowbytes
  PROCEDURE dowbytes (g: GenDevIF;
                      d: DeviceTablePtr;
                      from: ADDRESS;
                      nBytes: CARDINAL;
                      VAR actual: CARDINAL) : BOOLEAN ;

  (*
     dowriteln - attempt to write an end of line marker to the
                 file and returns TRUE if successful.
  *)

  dowriteln
  PROCEDURE dowriteln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;

  (*
     iseof - returns TRUE if end of file has been seen.
  *)

  iseof
  PROCEDURE iseof (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;

  (*
     iseoln - returns TRUE if end of line has been seen.
  *)

  iseoln
  PROCEDURE iseoln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;

  (*
     iserror - returns TRUE if an error was seen on the device.
               Note that reaching EOF is not classified as an
               error.
  *)

  iserror
  PROCEDURE iserror (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;

  END RTfio.

