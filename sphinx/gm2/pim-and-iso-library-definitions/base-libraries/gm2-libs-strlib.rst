.. _gm2-libs-strlib:

gm2-libs/StrLib
^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE StrLib ;

  EXPORT QUALIFIED StrConCat, StrLen, StrCopy, StrEqual, StrLess,
        	       	 IsSubString, StrRemoveWhitePrefix ;

  (*
     StrConCat - combines a and b into c.
  *)

  StrConCat
  PROCEDURE StrConCat (a, b: ARRAY OF CHAR; VAR c: ARRAY OF CHAR) ;

  (*
     StrLess - returns TRUE if string, a, alphabetically occurs before
               string, b.
  *)

  StrLess
  PROCEDURE StrLess (a, b: ARRAY OF CHAR) : BOOLEAN ;

  (*
     StrEqual - performs a = b on two strings.
  *)

  StrEqual
  PROCEDURE StrEqual (a, b: ARRAY OF CHAR) : BOOLEAN ;

  (*
     StrLen - returns the length of string, a.
  *)

  StrLen
  PROCEDURE StrLen (a: ARRAY OF CHAR) : CARDINAL ;

  (*
     StrCopy - copy string src into string dest providing dest is large enough.
               If dest is smaller than a then src then the string is truncated when
               dest is full.  Add a nul character if there is room in dest.
  *)

  StrCopy
  PROCEDURE StrCopy (src: ARRAY OF CHAR ; VAR dest: ARRAY OF CHAR) ;

  (*
     IsSubString - returns true if b is a subcomponent of a.
  *)

  IsSubString
  PROCEDURE IsSubString (a, b: ARRAY OF CHAR) : BOOLEAN ;

  (*
     StrRemoveWhitePrefix - copies string, into string, b, excluding any white
                            space infront of a.
  *)

  StrRemoveWhitePrefix
  PROCEDURE StrRemoveWhitePrefix (a: ARRAY OF CHAR; VAR b: ARRAY OF CHAR) ;

  END StrLib.

