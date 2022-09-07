.. _gm2-libs-wrapc:

gm2-libs/wrapc
^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE wrapc ;

  FROM SYSTEM IMPORT ADDRESS ;

  EXPORT QUALIFIED strtime, filesize, fileinode,
                   getrand, getusername, filemtime,
                   getnameuidgid, signbit, signbitf, signbitl,
  		 isfinite, isfinitel, isfinitef ;

  (*
     strtime - returns the C string for the equivalent C asctime
               function.
  *)

  strtime
  PROCEDURE strtime () : ADDRESS ;

  (*
     filesize - assigns the size of a file, f, into low, high and
                returns zero if successful.
  *)

  filesize
  PROCEDURE filesize (f: INTEGER; VAR low, high: CARDINAL) : INTEGER ;

  (*
     fileinode - return the inode associated with file, f.
  *)

  fileinode
  PROCEDURE fileinode (f: INTEGER; VAR low, high: CARDINAL) : INTEGER ;

  (*
     filemtime - returns the mtime of a file, f.
  *)

  filemtime
  PROCEDURE filemtime (f: INTEGER) : INTEGER ;

  (*
     getrand - returns a random number between 0..n-1
  *)

  getrand
  PROCEDURE getrand (n: INTEGER) : INTEGER ;

  (*
     getusername - returns a C string describing the current user.
  *)

  getusername
  PROCEDURE getusername () : ADDRESS ;

  (*
     getnameuidgid - fills in the, uid, and, gid, which represents
                     user, name.
  *)

  getnameuidgid
  PROCEDURE getnameuidgid (name: ADDRESS; VAR uid, gid: INTEGER) ;

  (*
     in C these procedure functions are really macros, so we provide
     real C functions and let gm2 call these if the builtins
     are unavailable.
  *)

  signbit
  PROCEDURE signbit (r: REAL) : INTEGER ;
  signbitf
  PROCEDURE signbitf (s: SHORTREAL) : INTEGER ;
  signbitl
  PROCEDURE signbitl (l: LONGREAL) : INTEGER ;

  (*
     isfinite - provide non builtin alternative to the gcc builtin isfinite.
                Returns 1 if x is finite and 0 if it is not.
  *)

  isfinite
  PROCEDURE isfinite (x: REAL) : INTEGER ;

  (*
     isfinitef - provide non builtin alternative to the gcc builtin isfinite.
                 Returns 1 if x is finite and 0 if it is not.
  *)

  isfinitef
  PROCEDURE isfinitef (x: SHORTREAL) : INTEGER ;

  (*
     isfinitel - provide non builtin alternative to the gcc builtin isfinite.
                 Returns 1 if x is finite and 0 if it is not.
  *)

  isfinitel
  PROCEDURE isfinitel (x: LONGREAL) : INTEGER ;

  END wrapc.

.. -

