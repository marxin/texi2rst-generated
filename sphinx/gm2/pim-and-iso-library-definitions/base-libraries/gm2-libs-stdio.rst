.. _gm2-libs-stdio:

gm2-libs/StdIO
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE StdIO ;

  EXPORT QUALIFIED ProcRead, ProcWrite,
                   Read, Write,
                   PushOutput, PopOutput, GetCurrentOutput,
                   PushInput, PopInput, GetCurrentInput ;

  TYPE
  ProcWrite (type)
     ProcWrite = PROCEDURE (CHAR) ;
  ProcRead (type)
     ProcRead  = PROCEDURE (VAR CHAR) ;

  (*
     Read - is the generic procedure that all higher application layers
            should use to receive a character.
  *)

  Read
  PROCEDURE Read (VAR ch: CHAR) ;

  (*
     Write - is the generic procedure that all higher application layers
             should use to emit a character.
  *)

  Write
  PROCEDURE Write (ch: CHAR) ;

  (*
     PushOutput - pushes the current Write procedure onto a stack,
                  any future references to Write will actually invoke
                  procedure, p.
  *)

  PushOutput
  PROCEDURE PushOutput (p: ProcWrite) ;

  (*
     PopOutput - restores Write to use the previous output procedure.
  *)

  PopOutput
  PROCEDURE PopOutput ;

  (*
     GetCurrentOutput - returns the current output procedure.
  *)

  GetCurrentOutput
  PROCEDURE GetCurrentOutput () : ProcWrite ;

  (*
     PushInput - pushes the current Read procedure onto a stack,
                 any future references to Read will actually invoke
                 procedure, p.
  *)

  PushInput
  PROCEDURE PushInput (p: ProcRead) ;

  (*
     PopInput - restores Write to use the previous output procedure.
  *)

  PopInput
  PROCEDURE PopInput ;

  (*
     GetCurrentInput - returns the current input procedure.
  *)

  GetCurrentInput
  PROCEDURE GetCurrentInput () : ProcRead ;

  END StdIO.

