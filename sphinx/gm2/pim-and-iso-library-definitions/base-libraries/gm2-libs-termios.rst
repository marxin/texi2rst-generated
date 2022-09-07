.. _gm2-libs-termios:

gm2-libs/termios
^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE termios ;

  FROM SYSTEM IMPORT ADDRESS ;

  TYPE
  TERMIOS (type)
     TERMIOS = ADDRESS ;

  ControlChar (type)
     ControlChar = (vintr, vquit, verase, vkill, veof, vtime, vmin,
                    vswtc, vstart, vstop, vsusp, veol, vreprint, vdiscard,
                    vwerase, vlnext, veol2) ;

  Flag (type)
     Flag = (
             (* input flag bits *)
             ignbrk, ibrkint, ignpar, iparmrk, inpck, istrip, inlcr,
             igncr, icrnl, iuclc, ixon, ixany, ixoff, imaxbel,
             (* output flag bits *)
             opost, olcuc, onlcr, ocrnl, onocr, onlret, ofill, ofdel,
             onl0, onl1, ocr0, ocr1, ocr2, ocr3,
             otab0, otab1, otab2, otab3, obs0, obs1, off0, off1, ovt0, ovt1,
             (* baud rate *)
             b0, b50, b75, b110, b135, b150, b200, b300, b600, b1200,
             b1800, b2400, b4800, b9600, b19200, b38400,
             b57600, b115200, b240400, b460800, b500000, b576000,
             b921600, b1000000, b1152000, b1500000, b2000000, b2500000,
             b3000000, b3500000, b4000000, maxbaud, crtscts,
             (* character size *)
             cs5, cs6, cs7, cs8, cstopb, cread, parenb, parodd, hupcl, clocal,
             (* local flags *)
             lisig, licanon, lxcase, lecho, lechoe, lechok, lechonl, lnoflsh,
             ltopstop, lechoctl, lechoprt, lechoke, lflusho, lpendin, liexten) ;

  (*
     InitTermios - new data structure.
  *)

  InitTermios
  PROCEDURE InitTermios () : TERMIOS ;

  (*
     KillTermios - delete data structure.
  *)

  KillTermios
  PROCEDURE KillTermios (t: TERMIOS) : TERMIOS ;

  (*
     cfgetospeed - return output baud rate.
  *)

  cfgetospeed
  PROCEDURE cfgetospeed (t: TERMIOS) : INTEGER ;

  (*
     cfgetispeed - return input baud rate.
  *)

  cfgetispeed
  PROCEDURE cfgetispeed (t: TERMIOS) : INTEGER ;

  (*
     cfsetospeed - set output baud rate.
  *)

  cfsetospeed
  PROCEDURE cfsetospeed (t: TERMIOS; b: CARDINAL) : INTEGER ;

  (*
     cfsetispeed - set input baud rate.
  *)

  cfsetispeed
  PROCEDURE cfsetispeed (t: TERMIOS; b: CARDINAL) : INTEGER ;

  (*
     cfsetspeed - set input and output baud rate.
  *)

  cfsetspeed
  PROCEDURE cfsetspeed (t: TERMIOS; b: CARDINAL) : INTEGER ;

  (*
     tcgetattr - get state of, fd, into, t.
  *)

  tcgetattr
  PROCEDURE tcgetattr (fd: INTEGER; t: TERMIOS) : INTEGER ;

  (*
     The following three functions return the different option values.
  *)

  tcsnow
  PROCEDURE tcsnow () : INTEGER ;   (* alter fd now *)
  tcsdrain
  PROCEDURE tcsdrain () : INTEGER ; (* alter when all output has been sent *)
  tcsflush
  PROCEDURE tcsflush () : INTEGER ; (* like drain, except discard any pending input *)

  (*
     tcsetattr - set state of, fd, to, t, using option.
  *)

  tcsetattr
  PROCEDURE tcsetattr (fd: INTEGER; option: INTEGER; t: TERMIOS) : INTEGER ;

  (*
     cfmakeraw - sets, t, to raw mode.
  *)

  cfmakeraw
  PROCEDURE cfmakeraw (t: TERMIOS) ;

  (*
     tcsendbreak - send zero bits for duration.
  *)

  tcsendbreak
  PROCEDURE tcsendbreak (fd: INTEGER; duration: INTEGER) : INTEGER ;

  (*
     tcdrain - waits for pending output to be written on, fd.
  *)

  tcdrain
  PROCEDURE tcdrain (fd: INTEGER) : INTEGER ;

  (*
     tcflushi - flush input.
  *)

  tcflushi
  PROCEDURE tcflushi (fd: INTEGER) : INTEGER ;

  (*
     tcflusho - flush output.
  *)

  tcflusho
  PROCEDURE tcflusho (fd: INTEGER) : INTEGER ;

  (*
     tcflushio - flush input and output.
  *)

  tcflushio
  PROCEDURE tcflushio (fd: INTEGER) : INTEGER ;

  (*
     tcflowoni - restart input on, fd.
  *)

  tcflowoni
  PROCEDURE tcflowoni (fd: INTEGER) : INTEGER ;

  (*
     tcflowoffi - stop input on, fd.
  *)

  tcflowoffi
  PROCEDURE tcflowoffi (fd: INTEGER) : INTEGER ;

  (*
     tcflowono - restart output on, fd.
  *)

  tcflowono
  PROCEDURE tcflowono (fd: INTEGER) : INTEGER ;

  (*
     tcflowoffo - stop output on, fd.
  *)

  tcflowoffo
  PROCEDURE tcflowoffo (fd: INTEGER) : INTEGER ;

  (*
     GetFlag - sets a flag value from, t, in, b, and returns TRUE
               if, t, supports, f.
  *)

  GetFlag
  PROCEDURE GetFlag (t: TERMIOS; f: Flag; VAR b: BOOLEAN) : BOOLEAN ;

  (*
     SetFlag - sets a flag value in, t, to, b, and returns TRUE if
               this flag value is supported.
  *)

  SetFlag
  PROCEDURE SetFlag (t: TERMIOS; f: Flag; b: BOOLEAN) : BOOLEAN ;

  (*
     GetChar - sets a CHAR, ch, value from, t, and returns TRUE if
               this value is supported.
  *)

  GetChar
  PROCEDURE GetChar (t: TERMIOS; c: ControlChar; VAR ch: CHAR) : BOOLEAN ;

  (*
     SetChar - sets a CHAR value in, t, and returns TRUE if, c,
               is supported.
  *)

  SetChar
  PROCEDURE SetChar (t: TERMIOS; c: ControlChar; ch: CHAR) : BOOLEAN ;

  END termios.

