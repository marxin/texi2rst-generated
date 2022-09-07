.. _gm2-libs-optlib:

gm2-libs/OptLib
^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE OptLib ;

  FROM SYSTEM IMPORT ADDRESS ;
  FROM DynamicStrings IMPORT String ;

  TYPE
  Option (type)
     Option ;

  (*
     InitOption - constructor for Option.
  *)

  InitOption
  PROCEDURE InitOption (argc: INTEGER; argv: ADDRESS) : Option ;

  (*
     KillOption - deconstructor for Option.
  *)

  KillOption
  PROCEDURE KillOption (o: Option) : Option ;

  (*
     Dup - duplicate the option array inside, o.
           Notice that this does not duplicate all the contents
           (strings) of argv.
           Shallow copy of the top level indices.
  *)

  Dup
  PROCEDURE Dup (o: Option) : Option ;

  (*
     Slice - return a new option which has elements [low:high] from the
             options, o.
  *)

  Slice
  PROCEDURE Slice (o: Option; low, high: INTEGER) : Option ;

  (*
     IndexStrCmp - returns the index in the argv array which matches
                   string, s.  -1 is returned if the string is not found.
  *)

  IndexStrCmp
  PROCEDURE IndexStrCmp (o: Option; s: String) : INTEGER ;

  (*
     IndexStrNCmp - returns the index in the argv array where the first
                    characters are matched by string, s.
                    -1 is returned if the string is not found.
  *)

  IndexStrNCmp
  PROCEDURE IndexStrNCmp (o: Option; s: String) : INTEGER ;

  (*
     ConCat - returns the concatenation of a and b.
  *)

  ConCat
  PROCEDURE ConCat (a, b: Option) : Option ;

  (*
     GetArgv - return the argv component of option.
  *)

  GetArgv
  PROCEDURE GetArgv (o: Option) : ADDRESS ;

  (*
     GetArgc - return the argc component of option.
  *)

  GetArgc
  PROCEDURE GetArgc (o: Option) : INTEGER ;

  END OptLib.

