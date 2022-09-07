.. _gm2-libs-debug:

gm2-libs/Debug
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Debug ;

  (*
      Description: provides some simple debugging routines.
  *)

  EXPORT QUALIFIED Halt, DebugString ;

  (*
     Halt - writes a message in the format:
            Module:Line:Message

            It then terminates by calling HALT.
  *)

  Halt
  PROCEDURE Halt (Message: ARRAY OF CHAR;
                  LineNo: CARDINAL;
                  Module: ARRAY OF CHAR) ;

  (*
     DebugString - writes a string to the debugging device (Scn.Write).
                   It interprets \n as carriage return, linefeed.
  *)

  DebugString
  PROCEDURE DebugString (a: ARRAY OF CHAR) ;

  END Debug.

