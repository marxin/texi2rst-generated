.. _gm2-libs-pushbackinput:

gm2-libs/PushBackInput
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE PushBackInput ;

  FROM FIO IMPORT File ;
  FROM DynamicStrings IMPORT String ;

  EXPORT QUALIFIED Open, PutCh, GetCh, Error, WarnError, WarnString,
                   Close, SetDebug, GetExitStatus, PutStr,
                   PutString, GetColumnPosition, GetCurrentLine ;

  (*
     Open - opens a file for reading.
  *)

  Open
  PROCEDURE Open (a: ARRAY OF CHAR) : File ;

  (*
     GetCh - gets a character from either the push back stack or
             from file, f.
  *)

  GetCh
  PROCEDURE GetCh (f: File) : CHAR ;

  (*
     PutCh - pushes a character onto the push back stack, it also
             returns the character which has been pushed.
  *)

  PutCh
  PROCEDURE PutCh (ch: CHAR) : CHAR ;

  (*
     PutString - pushes a string onto the push back stack.
  *)

  PutString
  PROCEDURE PutString (a: ARRAY OF CHAR) ;

  (*
     PutStr - pushes a dynamic string onto the push back stack.
              The string, s, is not deallocated.
  *)

  PutStr
  PROCEDURE PutStr (s: String) ;

  (*
     Error - emits an error message with the appropriate file, line combination.
  *)

  Error
  PROCEDURE Error (a: ARRAY OF CHAR) ;

  (*
     WarnError - emits an error message with the appropriate file, line combination.
                 It does not terminate but when the program finishes an exit status of
                 1 will be issued.
  *)

  WarnError
  PROCEDURE WarnError (a: ARRAY OF CHAR) ;

  (*
     WarnString - emits an error message with the appropriate file, line combination.
                  It does not terminate but when the program finishes an exit status of
                  1 will be issued.
  *)

  WarnString
  PROCEDURE WarnString (s: String) ;

  (*
     Close - closes the opened file.
  *)

  Close
  PROCEDURE Close (f: File) ;

  (*
     GetExitStatus - returns the exit status which will be 1 if any warnings were issued.
  *)

  GetExitStatus
  PROCEDURE GetExitStatus () : CARDINAL ;

  (*
     SetDebug - sets the debug flag on or off.
  *)

  SetDebug
  PROCEDURE SetDebug (d: BOOLEAN) ;

  (*
     GetColumnPosition - returns the column position of the current character.
  *)

  GetColumnPosition
  PROCEDURE GetColumnPosition () : CARDINAL ;

  (*
     GetCurrentLine - returns the current line number.
  *)

  GetCurrentLine
  PROCEDURE GetCurrentLine () : CARDINAL ;

  END PushBackInput.

