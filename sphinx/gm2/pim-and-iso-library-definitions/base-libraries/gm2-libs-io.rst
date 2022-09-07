.. _gm2-libs-io:

gm2-libs/IO
^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE IO ;

  (*
     Description: provides Read, Write, Errors procedures that map onto UNIX
                  file descriptors 0, 1 and 2. This is achieved by using
                  FIO if we are in buffered mode and using libc.write
                  if not.
  *)

  EXPORT QUALIFIED Read, Write, Error,
                   UnBufferedMode, BufferedMode,
                   EchoOn, EchoOff ;

  Read
  PROCEDURE Read (VAR ch: CHAR) ;
  Write
  PROCEDURE Write (ch: CHAR) ;
  Error
  PROCEDURE Error (ch: CHAR) ;

  (*
     UnBufferedMode - places file descriptor, fd, into an unbuffered mode.
  *)

  UnBufferedMode
  PROCEDURE UnBufferedMode (fd: INTEGER; input: BOOLEAN) ;

  (*
     BufferedMode - places file descriptor, fd, into a buffered mode.
  *)

  BufferedMode
  PROCEDURE BufferedMode (fd: INTEGER; input: BOOLEAN) ;

  (*
     EchoOn - turns on echoing for file descriptor, fd.  This
              only really makes sence for a file descriptor opened
              for terminal input or maybe some specific file descriptor
              which is attached to a particular piece of hardware.
  *)

  EchoOn
  PROCEDURE EchoOn (fd: INTEGER; input: BOOLEAN) ;

  (*
     EchoOff - turns off echoing for file descriptor, fd.  This
               only really makes sence for a file descriptor opened
               for terminal input or maybe some specific file descriptor
               which is attached to a particular piece of hardware.
  *)

  EchoOff
  PROCEDURE EchoOff (fd: INTEGER; input: BOOLEAN) ;

  END IO.

