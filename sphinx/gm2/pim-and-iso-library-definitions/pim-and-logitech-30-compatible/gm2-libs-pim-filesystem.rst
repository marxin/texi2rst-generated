.. _gm2-libs-pim-filesystem:

gm2-libs-pim/FileSystem
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FileSystem ;

  (*  Use this module sparingly, FIO or the ISO file modules have a
      much cleaner interface.  *)

  FROM SYSTEM IMPORT WORD, BYTE, ADDRESS ;
  IMPORT FIO ;
  FROM DynamicStrings IMPORT String ;

  EXPORT QUALIFIED File, Response, Flag, FlagSet,

                   Create, Close, Lookup, Rename, Delete,
                   SetRead, SetWrite, SetModify, SetOpen,
                   Doio, SetPos, GetPos, Length, Reset,

                   ReadWord, ReadChar, ReadByte, ReadNBytes,
                   WriteWord, WriteChar, WriteByte, WriteNBytes ;

  TYPE
  File (type)
     File = RECORD
               res     : Response ;
               flags   : FlagSet ;
               eof     : BOOLEAN ;
               lastWord: WORD ;
               lastByte: BYTE ;
               fio     : FIO.File ;
               highpos,
               lowpos  : CARDINAL ;
               name    : String ;
  END (type)
            END ;

  Flag (type)
     Flag = (
             read,        (* read access mode *)
             write,       (* write access mode *)
             modify,
             truncate,    (* truncate file when closed *)
             again,       (* reread the last character *)
             temporary,   (* file is temporary *)
             opened       (* file has been opened *)
            );

  FlagSet (type)
     FlagSet = SET OF Flag;

  Response (type)
     Response = (done, notdone, notsupported, callerror,
                 unknownfile, paramerror, toomanyfiles,
  userdeverror) (type)
                 userdeverror) ;

  Command (type)
     Command = (create, close, lookup, rename, delete,
                setread, setwrite, setmodify, setopen,
                doio, setpos, getpos, length) ;

  (*
     Create - creates a temporary file. To make the file perminant
              the file must be renamed.
  *)

  Create
  PROCEDURE Create (VAR f: File) ;

  (*
     Close - closes an open file.
  *)

  Close
  PROCEDURE Close (f: File) ;

  (*
     Lookup - looks for a file, filename. If the file is found
              then, f, is opened. If it is not found and, newFile,
              is TRUE then a new file is created and attached to, f.
              If, newFile, is FALSE and no file was found then f.res
              is set to notdone.
  *)

  Lookup
  PROCEDURE Lookup (VAR f: File; filename: ARRAY OF CHAR; newFile: BOOLEAN) ;

  (*
     Rename - rename a file and change a temporary file to a permanent
              file. f.res is set appropriately.
  *)

  Rename
  PROCEDURE Rename (VAR f: File; newname: ARRAY OF CHAR) ;

  (*
     Delete - deletes a file, name, and sets the f.res field.
              f.res is set appropriately.
  *)

  Delete
  PROCEDURE Delete (name: ARRAY OF CHAR; VAR f: File) ;

  (*
     ReadWord - reads a WORD, w, from file, f.
                f.res is set appropriately.
  *)

  ReadWord
  PROCEDURE ReadWord (VAR f: File; VAR w: WORD) ;

  (*
     WriteWord - writes one word to a file, f.
                 f.res is set appropriately.
  *)

  WriteWord
  PROCEDURE WriteWord (VAR f: File; w: WORD) ;

  (*
     ReadChar - reads one character from a file, f.
  *)

  ReadChar
  PROCEDURE ReadChar (VAR f: File; VAR ch: CHAR) ;

  (*
     WriteChar - writes a character, ch, to a file, f.
                 f.res is set appropriately.
  *)

  WriteChar
  PROCEDURE WriteChar (VAR f: File; ch: CHAR) ;

  (*
     ReadByte - reads a BYTE, b, from file, f.
                f.res is set appropriately.
  *)

  ReadByte
  PROCEDURE ReadByte (VAR f: File; VAR b: BYTE) ;

  (*
     WriteByte - writes one BYTE, b, to a file, f.
                 f.res is set appropriately.
  *)

  WriteByte
  PROCEDURE WriteByte (VAR f: File; b: BYTE) ;

  (*
     ReadNBytes - reads a sequence of bytes from a file, f.
  *)

  ReadNBytes
  PROCEDURE ReadNBytes (VAR f: File; a: ADDRESS; amount: CARDINAL;
                        VAR actuallyRead: CARDINAL) ;

  (*
     WriteNBytes - writes a sequence of bytes to file, f.
  *)

  WriteNBytes
  PROCEDURE WriteNBytes (VAR f: File; a: ADDRESS; amount: CARDINAL;
                         VAR actuallyWritten: CARDINAL) ;

  (*
     Again - returns the last character read to the internal buffer
             so that it can be read again.
  *)

  Again
  PROCEDURE Again (VAR f: File) ;

  (*
     SetRead - puts the file, f, into the read state.
               The file position is unchanged.
  *)

  SetRead
  PROCEDURE SetRead (VAR f: File) ;

  (*
     SetWrite - puts the file, f, into the write state.
                The file position is unchanged.
  *)

  SetWrite
  PROCEDURE SetWrite (VAR f: File) ;

  (*
     SetModify - puts the file, f, into the modify state.
                 The file position is unchanged but the file can be
                 read and written.
  *)

  SetModify
  PROCEDURE SetModify (VAR f: File) ;

  (*
     SetOpen - places a file, f, into the open state. The file may
               have been in the read/write/modify state before and
               in which case the previous buffer contents are flushed
               and the file state is reset to open. The position is
               unaltered.
  *)

  SetOpen
  PROCEDURE SetOpen (VAR f: File) ;

  (*
     Reset - places a file, f, into the open state and reset the
             position to the start of the file.
  *)

  Reset
  PROCEDURE Reset (VAR f: File) ;

  (*
     SetPos - lseek to a position within a file.
  *)

  SetPos
  PROCEDURE SetPos (VAR f: File; high, low: CARDINAL) ;

  (*
     GetPos - return the position within a file.
  *)

  GetPos
  PROCEDURE GetPos (VAR f: File; VAR high, low: CARDINAL) ;

  (*
     Length - returns the length of file, in, high, and, low.
  *)

  Length
  PROCEDURE Length (VAR f: File; VAR high, low: CARDINAL) ;

  (*
     Doio - effectively flushes a file in write mode, rereads the
            current buffer from disk if in read mode and writes
            and rereads the buffer if in modify mode.
  *)

  Doio
  PROCEDURE Doio (VAR f: File) ;

  (*
     FileNameChar - checks to see whether the character, ch, is
                    legal in a filename. nul is returned if the
                    character was illegal.
  *)

  FileNameChar
  PROCEDURE FileNameChar (ch: CHAR) ;

  END FileSystem.

