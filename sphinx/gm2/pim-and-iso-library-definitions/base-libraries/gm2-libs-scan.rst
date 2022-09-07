.. _gm2-libs-scan:

gm2-libs/Scan
^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Scan ;

  (* Provides a primitive symbol fetching from input.
     Symbols are delimited by spaces and tabs.
     Limitation only allows one source file at
     a time to deliver symbols.  *)

  EXPORT QUALIFIED GetNextSymbol, WriteError,
                   OpenSource, CloseSource,
                   TerminateOnError, DefineComments ;

  (* OpenSource - opens a source file for reading.                  *)

  OpenSource
  PROCEDURE OpenSource (a: ARRAY OF CHAR) : BOOLEAN ;

  (* CloseSource - closes the current source file from reading.     *)

  CloseSource
  PROCEDURE CloseSource ;

  (* GetNextSymbol gets the next source symbol and returns it in a. *)

  GetNextSymbol
  PROCEDURE GetNextSymbol (VAR a: ARRAY OF CHAR) ;

  (* WriteError writes a message, a, under the source line, which   *)
  (* attempts to pinpoint the Symbol at fault.                      *)

  WriteError
  PROCEDURE WriteError (a: ARRAY OF CHAR) ;

  (*
     TerminateOnError - exits with status 1 if we call WriteError.
  *)

  TerminateOnError
  PROCEDURE TerminateOnError ;

  (*
     DefineComments - defines the start of comments within the source
                      file.

                      The characters in Start define the comment start
                      and characters in End define the end.
                      The BOOLEAN eoln determine whether the comment
                      is terminated by end of line. If eoln is TRUE
                      then End is ignored.

                      If this procedure is never called then no comments
                      are allowed.
  *)

  DefineComments
  PROCEDURE DefineComments (Start, End: ARRAY OF CHAR; eoln: BOOLEAN) ;

  END Scan.

