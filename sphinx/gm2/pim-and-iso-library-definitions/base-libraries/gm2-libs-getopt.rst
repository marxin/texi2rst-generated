.. _gm2-libs-getopt:

gm2-libs/GetOpt
^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE GetOpt ;

  FROM SYSTEM IMPORT ADDRESS ;
  FROM DynamicStrings IMPORT String ;

  CONST
  no_argument  (const)
     no_argument = 0 ;
  required_argument  (const)
     required_argument = 1 ;
  optional_argument  (const)
     optional_argument = 2 ;

  TYPE
  LongOptions (type)
     LongOptions ;
  PtrToInteger (type)
     PtrToInteger = POINTER TO INTEGER ;

  (*
     GetOpt - call C getopt and fill in the parameters:
              optarg, optind, opterr and optop.
  *)

  GetOpt
  PROCEDURE GetOpt (argc: INTEGER; argv: ADDRESS; optstring: String;
                    VAR optarg: String;
                    VAR optind, opterr, optopt: INTEGER) : CHAR ;

  (*
     InitLongOptions - creates and returns a LongOptions empty array.
  *)

  InitLongOptions
  PROCEDURE InitLongOptions () : LongOptions ;

  (*
     AddLongOption - appends long option {name, has_arg, flag, val} to the
                     array of options and new long options array is
                     returned.
                     The old array, lo, should no longer be used.

     (from man 3 getopt)
         The meanings of the different fields are:

         name   is the name of the long option.

         has_arg
                is: no_argument (or 0) if the option does not take an
                argument; required_argument (or  1) if the option
                requires an argument; or optional_argument (or 2) if
                the option takes an optional argument.

         flag   specifies how results are returned for a long option.
                If flag is NULL, then getopt_long() returns val.
                (For example, the calling program may set val to the
                equivalent short option character).  Otherwise,
                getopt_long() returns 0, and flag points to a
                variable which is set to val if the option is found,
                but left unchanged if the option is not found.

         val    is the value to return, or to load into the variable
                pointed to by flag.

         The last element of the array has to be filled with zeros.
  *)

  AddLongOption
  PROCEDURE AddLongOption (lo: LongOptions;
                           name: String; has_arg: INTEGER;
                           flag: PtrToInteger;
                           val: INTEGER) : LongOptions ;

  (*
     KillLongOptions - returns NIL and also frees up memory
                       associated with, lo.
  *)

  KillLongOptions
  PROCEDURE KillLongOptions (lo: LongOptions) : LongOptions ;

  (*
     GetOptLong - works like GetOpt but will accept long options (using
                  two dashes).  If the program only accepts long options
                  then optstring should be an empty string, not NIL.
  *)

  GetOptLong
  PROCEDURE GetOptLong (argc: INTEGER; argv: ADDRESS; optstring: String;
                        longopts: LongOptions;
                        VAR longindex: INTEGER) : INTEGER ;

  (*
     GetOptLongOnly - works like GetOptLong except that a single dash
                      can be used for a long option.
  *)

  GetOptLongOnly
  PROCEDURE GetOptLongOnly (argc: INTEGER; argv: ADDRESS;
                            optstring: String; longopts: LongOptions;
                            VAR longindex: INTEGER) : INTEGER ;

  END GetOpt.

