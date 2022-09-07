.. _gm2-libs-sfio:

gm2-libs/SFIO
^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SFIO ;

  FROM DynamicStrings IMPORT String ;
  FROM FIO IMPORT File ;

  EXPORT QUALIFIED OpenToRead, OpenToWrite, OpenForRandom, Exists, WriteS, ReadS ;

  (*
     Exists - returns TRUE if a file named, fname exists for reading.
  *)

  Exists
  PROCEDURE Exists (fname: String) : BOOLEAN ;

  (*
     OpenToRead - attempts to open a file, fname, for reading and
                  it returns this file.
                  The success of this operation can be checked by
                  calling IsNoError.
  *)

  OpenToRead
  PROCEDURE OpenToRead (fname: String) : File ;

  (*
     OpenToWrite - attempts to open a file, fname, for write and
                   it returns this file.
                   The success of this operation can be checked by
                   calling IsNoError.
  *)

  OpenToWrite
  PROCEDURE OpenToWrite (fname: String) : File ;

  (*
     OpenForRandom - attempts to open a file, fname, for random access
                     read or write and it returns this file.
                     The success of this operation can be checked by
                     calling IsNoError.
                     towrite, determines whether the file should be
                     opened for writing or reading.
                     if towrite is TRUE or whether the previous file should
                     be left alone, allowing this descriptor to seek
                     and modify an existing file.
  *)

  OpenForRandom
  PROCEDURE OpenForRandom (fname: String; towrite, newfile: BOOLEAN) : File ;

  (*
     WriteS - writes a string, s, to, file. It returns the String, s.
  *)

  WriteS
  PROCEDURE WriteS (file: File; s: String) : String ;

  (*
     ReadS - reads a string, s, from, file. It returns the String, s.
             It stops reading the string at the end of line or end of file.
             It consumes the newline at the end of line but does not place
             this into the returned string.
  *)

  ReadS
  PROCEDURE ReadS (file: File) : String ;

  END SFIO.

