.. _gm2-libs-pim-inout:

gm2-libs-pim/InOut
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE InOut ;

  IMPORT ASCII ;
  FROM DynamicStrings IMPORT String ;
  EXPORT QUALIFIED EOL, Done, termCH, OpenInput, OpenOutput,
                   CloseInput, CloseOutput,
                   Read, ReadString, ReadInt, ReadCard,
                   Write, WriteLn, WriteString, WriteInt, WriteCard,
                   WriteOct, WriteHex,
                   ReadS, WriteS ;

  CONST
  EOL  (const)
     EOL = ASCII.EOL ;

  VAR
  Done   (var)
     Done  : BOOLEAN ;
  termCH (var)
     termCH: CHAR ;

  (*
     OpenInput - reads a string from stdin as the filename for reading.
                 If the filename ends with `.' then it appends the defext
                 extension. The global variable Done is set if all
                 was successful.
  *)

  OpenInput
  PROCEDURE OpenInput (defext: ARRAY OF CHAR) ;

  (*
     CloseInput - closes an opened input file and returns input back to
                  StdIn.
  *)

  CloseInput
  PROCEDURE CloseInput ;

  (*
     OpenOutput - reads a string from stdin as the filename for writing.
                  If the filename ends with `.' then it appends the defext
                  extension. The global variable Done is set if all
                  was successful.
  *)

  OpenOutput
  PROCEDURE OpenOutput (defext: ARRAY OF CHAR) ;

  (*
     CloseOutput - closes an opened output file and returns output back to
                   StdOut.
  *)

  CloseOutput
  PROCEDURE CloseOutput ;

  (*
     Read - reads a single character from the current input file.
            Done is set to FALSE if end of file is reached or an
            error occurs.
  *)

  Read
  PROCEDURE Read (VAR ch: CHAR) ;

  (*
     ReadString - reads a sequence of characters. Leading white space
                  is ignored and the string is terminated with a character
                  <= ' '
  *)

  ReadString
  PROCEDURE ReadString (VAR s: ARRAY OF CHAR) ;

  (*
     WriteString - writes a string to the output file.
  *)

  WriteString
  PROCEDURE WriteString (s: ARRAY OF CHAR) ;

  (*
     Write - writes out a single character, ch, to the current output file.
  *)

  Write
  PROCEDURE Write (ch: CHAR) ;

  (*
     WriteLn - writes a newline to the output file.
  *)

  WriteLn
  PROCEDURE WriteLn ;

  (*
     ReadInt - reads a string and converts it into an INTEGER, x.
               Done is set if an INTEGER is read.
  *)

  ReadInt
  PROCEDURE ReadInt (VAR x: INTEGER) ;

  (*
     ReadInt - reads a string and converts it into an INTEGER, x.
               Done is set if an INTEGER is read.
  *)

  ReadCard
  PROCEDURE ReadCard (VAR x: CARDINAL) ;

  (*
     WriteCard - writes the CARDINAL, x, to the output file. It ensures
                 that the number occupies, n, characters. Leading spaces
                 are added if required.
  *)

  WriteCard
  PROCEDURE WriteCard (x, n: CARDINAL) ;

  (*
     WriteInt - writes the INTEGER, x, to the output file. It ensures
                that the number occupies, n, characters. Leading spaces
                are added if required.
  *)

  WriteInt
  PROCEDURE WriteInt (x: INTEGER; n: CARDINAL) ;

  (*
     WriteOct - writes the CARDINAL, x, to the output file in octal.
                It ensures that the number occupies, n, characters.
                Leading spaces are added if required.
  *)

  WriteOct
  PROCEDURE WriteOct (x, n: CARDINAL) ;

  (*
     WriteHex - writes the CARDINAL, x, to the output file in hexadecimal.
                It ensures that the number occupies, n, characters.
                Leading spaces are added if required.
  *)

  WriteHex
  PROCEDURE WriteHex (x, n: CARDINAL) ;

  (*
     ReadS - returns a string which has is a sequence of characters.
             Leading white space is ignored and string is terminated
             with a character <= ' '.
  *)

  ReadS
  PROCEDURE ReadS () : String ;

  (*
     WriteS - writes a String to the output device.
              It returns the string, s.
  *)

  WriteS
  PROCEDURE WriteS (s: String) : String ;

  END InOut.

