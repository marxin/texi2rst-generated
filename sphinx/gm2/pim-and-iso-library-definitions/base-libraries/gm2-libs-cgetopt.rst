.. _gm2-libs-cgetopt:

gm2-libs/cgetopt
^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE cgetopt ;

  FROM SYSTEM IMPORT ADDRESS ;

  TYPE
  Options (type)
     Options = ADDRESS ;

  VAR
  optarg                 (var)
     optarg                : ADDRESS ;
  optind (var)
  opterr (var)
  optopt (var)
     optind, opterr, optopt: INTEGER ;

  (*
     getopt - the getopt() function parses the command-line arguments.
              Its arguments argc and argv are the argument count and array as
              passed to the main() function on program invocation.  An element of
              argv that starts with '-' (and is not exactly "-" or "--") is an
              option element.  The characters of this element (aside from the
              initial '-') are option characters.  If getopt() is called
              repeatedly, it returns successively each of the option characters
              from each of the option elements.
  *)

  getopt
  PROCEDURE getopt (argc: INTEGER; argv: ADDRESS; optstring: ADDRESS) : CHAR ;

  (*
     getopt_long - works like getopt() except that it also accepts long options,
                   started with two dashes.  (If the program accepts only long
                   options, then optstring should be specified as an empty string (""),
                   not NULL.)  Long option names may be abbreviated if the abbreviation
                   is unique or is an exact match for some defined option.  A
                   long option may take a parameter, of the form --arg=param or
                   --arg param.
  *)

  getopt_long
  PROCEDURE getopt_long (argc: INTEGER; argv: ADDRESS; optstring: ADDRESS;
                         longopts: ADDRESS; VAR longindex: INTEGER) : INTEGER ;

  (*
     getopt_long_only - a wrapper for the C getopt_long_only.
  *)

  getopt_long_only
  PROCEDURE getopt_long_only (argc: INTEGER; argv: ADDRESS; optstring: ADDRESS;
                              longopts: ADDRESS; VAR longindex: INTEGER) : INTEGER ;

  (*
     InitOptions - constructor for empty Options.
  *)

  InitOptions
  PROCEDURE InitOptions () : Options ;

  (*
     KillOptions - deconstructor for empty Options.
  *)

  KillOptions
  PROCEDURE KillOptions (o: Options) : Options ;

  (*
     SetOption - set option[index] with {name, has_arg, flag, val}.
  *)

  SetOption
  PROCEDURE SetOption (o: Options; index: CARDINAL;
                       name: ADDRESS; has_arg: BOOLEAN;
                       VAR flag: INTEGER; val: INTEGER) ;

  (*
     GetLongOptionArray - return a pointer to the C array containing all
                          long options.
  *)

  GetLongOptionArray
  PROCEDURE GetLongOptionArray (o: Options) : ADDRESS ;

  END cgetopt.

