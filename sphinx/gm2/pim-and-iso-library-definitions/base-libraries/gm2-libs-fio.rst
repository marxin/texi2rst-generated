.. _gm2-libs-fio:

gm2-libs/FIO
^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FIO ;

  (* Provides a simple buffered file input/output library.  *)

  FROM SYSTEM IMPORT ADDRESS, BYTE ;

  EXPORT QUALIFIED (* types *)
                   File,
                   (* procedures *)
                   OpenToRead, OpenToWrite, OpenForRandom, Close,
                   EOF, EOLN, WasEOLN, IsNoError, Exists, IsActive,
                   exists, openToRead, openToWrite, openForRandom,
                   SetPositionFromBeginning,
                   SetPositionFromEnd,
                   FindPosition,
                   ReadChar, ReadString,
                   WriteChar, WriteString, WriteLine,
                   WriteCardinal, ReadCardinal,
                   UnReadChar,
                   WriteNBytes, ReadNBytes,
                   FlushBuffer,
                   GetUnixFileDescriptor,
                   GetFileName, getFileName, getFileNameLength,
                   FlushOutErr,
                   (* variables *)
                   StdIn, StdOut, StdErr ;

  TYPE
  File (type)
     File = CARDINAL ;

  (* the following variables are initialized to their UNIX equivalents *)
  VAR
  StdIn (var)
  StdOut (var)
  StdErr (var)
     StdIn, StdOut, StdErr: File ;

  (*
     IsNoError - returns a TRUE if no error has occured on file, f.
  *)

  IsNoError
  PROCEDURE IsNoError (f: File) : BOOLEAN ;

  (*
     IsActive - returns TRUE if the file, f, is still active.
  *)

  IsActive
  PROCEDURE IsActive (f: File) : BOOLEAN ;

  (*
     Exists - returns TRUE if a file named, fname exists for reading.
  *)

  Exists
  PROCEDURE Exists (fname: ARRAY OF CHAR) : BOOLEAN ;

  (*
     OpenToRead - attempts to open a file, fname, for reading and
                  it returns this file.
                  The success of this operation can be checked by
                  calling IsNoError.
  *)

  OpenToRead
  PROCEDURE OpenToRead (fname: ARRAY OF CHAR) : File ;

  (*
     OpenToWrite - attempts to open a file, fname, for write and
                   it returns this file.
                   The success of this operation can be checked by
                   calling IsNoError.
  *)

  OpenToWrite
  PROCEDURE OpenToWrite (fname: ARRAY OF CHAR) : File ;

  (*
     OpenForRandom - attempts to open a file, fname, for random access
                     read or write and it returns this file.
                     The success of this operation can be checked by
                     calling IsNoError.
                     towrite, determines whether the file should be
                     opened for writing or reading.
                     newfile, determines whether a file should be
                     created if towrite is TRUE or whether the
                     previous file should be left alone,
                     allowing this descriptor to seek
                     and modify an existing file.
  *)

  OpenForRandom
  PROCEDURE OpenForRandom (fname: ARRAY OF CHAR;
                           towrite, newfile: BOOLEAN) : File ;

  (*
     Close - close a file which has been previously opened using:
             OpenToRead, OpenToWrite, OpenForRandom.
             It is correct to close a file which has an error status.
  *)

  Close
  PROCEDURE Close (f: File) ;

  (* the following functions are functionally equivalent to the above
     except they allow C style names.
  *)

  exists
  PROCEDURE exists        (fname: ADDRESS; flength: CARDINAL) : BOOLEAN ;
  openToRead
  PROCEDURE openToRead    (fname: ADDRESS; flength: CARDINAL) : File ;
  openToWrite
  PROCEDURE openToWrite   (fname: ADDRESS; flength: CARDINAL) : File ;
  openForRandom
  PROCEDURE openForRandom (fname: ADDRESS; flength: CARDINAL;
                           towrite, newfile: BOOLEAN) : File ;

  (*
     FlushBuffer - flush contents of the FIO file, f, to libc.
  *)

  FlushBuffer
  PROCEDURE FlushBuffer (f: File) ;

  (*
     ReadNBytes - reads nBytes of a file into memory area, dest, returning
                  the number of bytes actually read.
                  This function will consume from the buffer and then
                  perform direct libc reads. It is ideal for large reads.
  *)

  ReadNBytes
  PROCEDURE ReadNBytes (f: File; nBytes: CARDINAL;
                        dest: ADDRESS) : CARDINAL ;

  (*
     ReadAny - reads HIGH(a) bytes into, a. All input
               is fully buffered, unlike ReadNBytes and thus is more
               suited to small reads.
  *)

  ReadAny
  PROCEDURE ReadAny (f: File; VAR a: ARRAY OF BYTE) ;

  (*
     WriteNBytes - writes nBytes from memory area src to a file
                   returning the number of bytes actually written.
                   This function will flush the buffer and then
                   write the nBytes using a direct write from libc.
                   It is ideal for large writes.
  *)

  WriteNBytes
  PROCEDURE WriteNBytes (f: File; nBytes: CARDINAL;
                         src: ADDRESS) : CARDINAL ;

  (*
     WriteAny - writes HIGH(a) bytes onto, file, f. All output
                is fully buffered, unlike WriteNBytes and thus is more
                suited to small writes.
  *)

  WriteAny
  PROCEDURE WriteAny (f: File; VAR a: ARRAY OF BYTE) ;

  (*
     WriteChar - writes a single character to file, f.
  *)

  WriteChar
  PROCEDURE WriteChar (f: File; ch: CHAR) ;

  (*
     EOF - tests to see whether a file, f, has reached end of file.
  *)

  EOF
  PROCEDURE EOF (f: File) : BOOLEAN ;

  (*
     EOLN - tests to see whether a file, f, is about to read a newline.
            It does NOT consume the newline.  It reads the next character
            and then immediately unreads the character.
  *)

  EOLN
  PROCEDURE EOLN (f: File) : BOOLEAN ;

  (*
     WasEOLN - tests to see whether a file, f, has just read a newline
               character.
  *)

  WasEOLN
  PROCEDURE WasEOLN (f: File) : BOOLEAN ;

  (*
     ReadChar - returns a character read from file, f.
                Sensible to check with IsNoError or EOF after calling
                this function.
  *)

  ReadChar
  PROCEDURE ReadChar (f: File) : CHAR ;

  (*
     UnReadChar - replaces a character, ch, back into file, f.
                  This character must have been read by ReadChar
                  and it does not allow successive calls.  It may
                  only be called if the previous read was successful,
                  end of file or end of line seen.
  *)

  UnReadChar
  PROCEDURE UnReadChar (f: File ; ch: CHAR) ;

  (*
     WriteLine - writes out a linefeed to file, f.
  *)

  WriteLine
  PROCEDURE WriteLine (f: File) ;

  (*
     WriteString - writes a string to file, f.
  *)

  WriteString
  PROCEDURE WriteString (f: File; a: ARRAY OF CHAR) ;

  (*
     ReadString - reads a string from file, f, into string, a.
                  It terminates the string if HIGH is reached or
                  if a newline is seen or an error occurs.
  *)

  ReadString
  PROCEDURE ReadString (f: File; VAR a: ARRAY OF CHAR) ;

  (*
     WriteCardinal - writes a CARDINAL to file, f.
                     It writes the binary image of the CARDINAL.
                     to file, f.
  *)

  WriteCardinal
  PROCEDURE WriteCardinal (f: File; c: CARDINAL) ;

  (*
     ReadCardinal - reads a CARDINAL from file, f.
                    It reads a bit image of a CARDINAL
                    from file, f.
  *)

  ReadCardinal
  PROCEDURE ReadCardinal (f: File) : CARDINAL ;

  (*
     GetUnixFileDescriptor - returns the UNIX file descriptor of a file.
                             Useful when combining FIO.mod with select
                             (in Selective.def - but note the comments in
                              Selective about using read/write primatives)
  *)

  GetUnixFileDescriptor
  PROCEDURE GetUnixFileDescriptor (f: File) : INTEGER ;

  (*
     SetPositionFromBeginning - sets the position from the beginning
                                of the file.
  *)

  SetPositionFromBeginning
  PROCEDURE SetPositionFromBeginning (f: File; pos: LONGINT) ;

  (*
     SetPositionFromEnd - sets the position from the end of the file.
  *)

  SetPositionFromEnd
  PROCEDURE SetPositionFromEnd (f: File; pos: LONGINT) ;

  (*
     FindPosition - returns the current absolute position in file, f.
  *)

  FindPosition
  PROCEDURE FindPosition (f: File) : LONGINT ;

  (*
     GetFileName - assigns, a, with the filename associated with, f.
  *)

  GetFileName
  PROCEDURE GetFileName (f: File; VAR a: ARRAY OF CHAR) ;

  (*
     getFileName - returns the address of the filename associated with, f.
  *)

  getFileName
  PROCEDURE getFileName (f: File) : ADDRESS ;

  (*
     getFileNameLength - returns the number of characters associated with
                         filename, f.
  *)

  getFileNameLength
  PROCEDURE getFileNameLength (f: File) : CARDINAL ;

  (*
     FlushOutErr - flushes, StdOut, and, StdErr.
  *)

  FlushOutErr
  PROCEDURE FlushOutErr ;

  END FIO.

