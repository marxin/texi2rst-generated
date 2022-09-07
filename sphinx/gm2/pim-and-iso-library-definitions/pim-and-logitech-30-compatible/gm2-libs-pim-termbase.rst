.. _gm2-libs-pim-termbase:

gm2-libs-pim/Termbase
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Termbase ;

  (*
     Initially the read routines from Keyboard and the
     write routine from Display is assigned to the Read,
     KeyPressed and Write procedures.
  *)

  EXPORT QUALIFIED ReadProcedure, StatusProcedure, WriteProcedure,
                   AssignRead, AssignWrite, UnAssignRead, UnAssignWrite,
                   Read, KeyPressed, Write ;

  TYPE
  ReadProcedure (type)
     ReadProcedure = PROCEDURE (VAR CHAR) ;
  WriteProcedure (type)
     WriteProcedure = PROCEDURE (CHAR) ;
  StatusProcedure (type)
     StatusProcedure = PROCEDURE () : BOOLEAN ;

  (*
     AssignRead - assigns a read procedure and status procedure for terminal
                  input. Done is set to TRUE if successful. Subsequent
                  Read and KeyPressed calls are mapped onto the user supplied
                  procedures. The previous read and status procedures are
                  uncovered and reused after UnAssignRead is called.
  *)

  AssignRead
  PROCEDURE AssignRead (rp: ReadProcedure; sp: StatusProcedure;
                        VAR Done: BOOLEAN) ;

  (*
     UnAssignRead - undo the last call to AssignRead and set Done to TRUE
                    on success.
  *)

  UnAssignRead
  PROCEDURE UnAssignRead (VAR Done: BOOLEAN) ;

  (*
     Read - reads a single character using the currently active read
            procedure.
  *)

  Read
  PROCEDURE Read (VAR ch: CHAR) ;

  (*
     KeyPressed - returns TRUE if a character is available to be read.
  *)

  KeyPressed
  PROCEDURE KeyPressed () : BOOLEAN ;

  (*
     AssignWrite - assigns a write procedure for terminal output.
                   Done is set to TRUE if successful. Subsequent
                   Write calls are mapped onto the user supplied
                   procedure. The previous write procedure is
                   uncovered and reused after UnAssignWrite is called.
  *)

  AssignWrite
  PROCEDURE AssignWrite (wp: WriteProcedure; VAR Done: BOOLEAN) ;

  (*
     UnAssignWrite - undo the last call to AssignWrite and set Done to TRUE
                     on success.
  *)

  UnAssignWrite
  PROCEDURE UnAssignWrite (VAR Done: BOOLEAN) ;

  (*
     Write - writes a single character using the currently active write
             procedure.
  *)

  Write
  PROCEDURE Write (VAR ch: CHAR) ;

  END Termbase.

