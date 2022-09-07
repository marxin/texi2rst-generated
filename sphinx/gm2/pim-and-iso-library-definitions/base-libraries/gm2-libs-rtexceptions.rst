.. _gm2-libs-rtexceptions:

gm2-libs/RTExceptions
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RTExceptions ;

  (* Runtime exception handler routines.  This should
     be considered as a system module for GNU Modula-2
     and allow the compiler to interface with exception
     handling.  *)

  FROM SYSTEM IMPORT ADDRESS ;
  EXPORT QUALIFIED EHBlock,
                   Raise, SetExceptionBlock, GetExceptionBlock,
                   GetTextBuffer, GetTextBufferSize, GetNumber,
                   InitExceptionBlock, KillExceptionBlock,
                   PushHandler, PopHandler,
                   BaseExceptionsThrow, DefaultErrorCatch,
                   IsInExceptionState, SetExceptionState,
                   SwitchExceptionState, GetBaseExceptionBlock,
                   SetExceptionSource, GetExceptionSource ;

  TYPE
  EHBlock (type)
     EHBlock ;
  ProcedureHandler (type)
     ProcedureHandler = PROCEDURE ;

  (*
     Raise - invoke the exception handler associated with, number,
             in the active EHBlock.  It keeps a record of the number
             and message in the EHBlock for later use.
  *)

  Raise
  PROCEDURE Raise (number: CARDINAL;
                   file: ADDRESS; line: CARDINAL;
                   column: CARDINAL; function: ADDRESS;
                   message: ADDRESS) ;

  (*
     SetExceptionBlock - sets, source, as the active EHB.
  *)

  SetExceptionBlock
  PROCEDURE SetExceptionBlock (source: EHBlock) ;

  (*
     GetExceptionBlock - returns the active EHB.
  *)

  GetExceptionBlock
  PROCEDURE GetExceptionBlock () : EHBlock ;

  (*
     GetTextBuffer - returns the address of the EHB buffer.
  *)

  GetTextBuffer
  PROCEDURE GetTextBuffer (e: EHBlock) : ADDRESS ;

  (*
     GetTextBufferSize - return the size of the EHB text buffer.
  *)

  GetTextBufferSize
  PROCEDURE GetTextBufferSize (e: EHBlock) : CARDINAL ;

  (*
     GetNumber - return the exception number associated with,
                 source.
  *)

  GetNumber
  PROCEDURE GetNumber (source: EHBlock) : CARDINAL ;

  (*
     InitExceptionBlock - creates and returns a new exception block.
  *)

  InitExceptionBlock
  PROCEDURE InitExceptionBlock () : EHBlock ;

  (*
     KillExceptionBlock - destroys the EHB, e, and all its handlers.
  *)

  KillExceptionBlock
  PROCEDURE KillExceptionBlock (e: EHBlock) : EHBlock ;

  (*
     PushHandler - install a handler in EHB, e.
  *)

  PushHandler
  PROCEDURE PushHandler (e: EHBlock; number: CARDINAL; p: ProcedureHandler) ;

  (*
     PopHandler - removes the handler associated with, number, from
                  EHB, e.
  *)

  PopHandler
  PROCEDURE PopHandler (e: EHBlock; number: CARDINAL) ;

  (*
     DefaultErrorCatch - displays the current error message in
                         the current exception block and then
                         calls HALT.
  *)

  DefaultErrorCatch
  PROCEDURE DefaultErrorCatch ;

  (*
     BaseExceptionsThrow - configures the Modula-2 exceptions to call
                           THROW which in turn can be caught by an
                           exception block.  If this is not called then
                           a Modula-2 exception will simply call an
                           error message routine and then HALT.
  *)

  BaseExceptionsThrow
  PROCEDURE BaseExceptionsThrow ;

  (*
     IsInExceptionState - returns TRUE if the program is currently
                          in the exception state.
  *)

  IsInExceptionState
  PROCEDURE IsInExceptionState () : BOOLEAN ;

  (*
     SetExceptionState - returns the current exception state and
                         then sets the current exception state to,
                         to.
  *)

  SetExceptionState
  PROCEDURE SetExceptionState (to: BOOLEAN) : BOOLEAN ;

  (*
     SwitchExceptionState - assigns, from, with the current exception
                            state and then assigns the current exception
                            to, to.
  *)

  SwitchExceptionState
  PROCEDURE SwitchExceptionState (VAR from: BOOLEAN; to: BOOLEAN) ;

  (*
     GetBaseExceptionBlock - returns the initial language exception block
                             created.
  *)

  GetBaseExceptionBlock
  PROCEDURE GetBaseExceptionBlock () : EHBlock ;

  (*
     SetExceptionSource - sets the current exception source to, source.
  *)

  SetExceptionSource
  PROCEDURE SetExceptionSource (source: ADDRESS) ;

  (*
     GetExceptionSource - returns the current exception source.
  *)

  GetExceptionSource
  PROCEDURE GetExceptionSource () : ADDRESS ;

  END RTExceptions.

