.. _gm2-libs-dynamicstrings:

gm2-libs/DynamicStrings
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE DynamicStrings ;

  FROM SYSTEM IMPORT ADDRESS ;
  EXPORT QUALIFIED String,
                   InitString, KillString, Fin, InitStringCharStar,
                   InitStringChar, Index, RIndex,
                   Mark, Length, ConCat, ConCatChar, Assign, Dup, Add,
                   Equal, EqualCharStar, EqualArray, ToUpper, ToLower,
                   CopyOut, Mult, Slice,
                   RemoveWhitePrefix, RemoveWhitePostfix, RemoveComment,
                   char, string,
                   InitStringDB, InitStringCharStarDB, InitStringCharDB,
                   MultDB, DupDB, SliceDB,
                   PushAllocation, PopAllocation, PopAllocationExemption ;

  TYPE
  String (type)
     String ;

  (*
     InitString - creates and returns a String type object.
                  Initial contents are, a.
  *)

  InitString
  PROCEDURE InitString (a: ARRAY OF CHAR) : String ;

  (*
     KillString - frees String, s, and its contents.
                  NIL is returned.
  *)

  KillString
  PROCEDURE KillString (s: String) : String ;

  (*
     Fin - finishes with a string, it calls KillString with, s.
           The purpose of the procedure is to provide a short cut
           to calling KillString and then testing the return result.
  *)

  Fin
  PROCEDURE Fin (s: String) ;

  (*
     InitStringCharStar - initializes and returns a String to contain
                          the C string.
  *)

  InitStringCharStar
  PROCEDURE InitStringCharStar (a: ADDRESS) : String ;

  (*
     InitStringChar - initializes and returns a String to contain the
                      single character, ch.
  *)

  InitStringChar
  PROCEDURE InitStringChar (ch: CHAR) : String ;

  (*
     Mark - marks String, s, ready for garbage collection.
  *)

  Mark
  PROCEDURE Mark (s: String) : String ;

  (*
     Length - returns the length of the String, s.
  *)

  Length
  PROCEDURE Length (s: String) : CARDINAL ;

  (*
     ConCat - returns String, a, after the contents of, b,
              have been appended.
  *)

  ConCat
  PROCEDURE ConCat (a, b: String) : String ;

  (*
     ConCatChar - returns String, a, after character, ch,
                  has been appended.
  *)

  ConCatChar
  PROCEDURE ConCatChar (a: String; ch: CHAR) : String ;

  (*
     Assign - assigns the contents of, b, into, a.
              String, a, is returned.
  *)

  Assign
  PROCEDURE Assign (a, b: String) : String ;

  (*
     Dup - duplicate a String, s, returning the copy of s.
  *)

  Dup
  PROCEDURE Dup (s: String) : String ;

  (*
     Add - returns a new String which contains the contents of a and b.
  *)

  Add
  PROCEDURE Add (a, b: String) : String ;

  (*
     Equal - returns TRUE if String, a, and, b, are equal.
  *)

  Equal
  PROCEDURE Equal (a, b: String) : BOOLEAN ;

  (*
     EqualCharStar - returns TRUE if contents of String, s, is
                     the same as the string, a.
  *)

  EqualCharStar
  PROCEDURE EqualCharStar (s: String; a: ADDRESS) : BOOLEAN ;

  (*
     EqualArray - returns TRUE if contents of String, s, is the
                  same as the string, a.
  *)

  EqualArray
  PROCEDURE EqualArray (s: String; a: ARRAY OF CHAR) : BOOLEAN ;

  (*
     Mult - returns a new string which is n concatenations of String, s.
            If n<=0 then an empty string is returned.
  *)

  Mult
  PROCEDURE Mult (s: String; n: CARDINAL) : String ;

  (*
     Slice - returns a new string which contains the elements
             low..high-1

             strings start at element 0
             Slice(s, 0, 2)  will return elements 0, 1 but not 2
             Slice(s, 1, 3)  will return elements 1, 2 but not 3
             Slice(s, 2, 0)  will return elements 2..max
             Slice(s, 3, -1) will return elements 3..max-1
             Slice(s, 4, -2) will return elements 4..max-2
  *)

  Slice
  PROCEDURE Slice (s: String; low, high: INTEGER) : String ;

  (*
     Index - returns the indice of the first occurance of, ch, in
             String, s. -1 is returned if, ch, does not exist.
             The search starts at position, o.
  *)

  Index
  PROCEDURE Index (s: String; ch: CHAR; o: CARDINAL) : INTEGER ;

  (*
     RIndex - returns the indice of the last occurance of, ch,
              in String, s. The search starts at position, o.
              -1 is returned if, ch, is not found.
  *)

  RIndex
  PROCEDURE RIndex (s: String; ch: CHAR; o: CARDINAL) : INTEGER ;

  (*
     RemoveComment - assuming that, comment, is a comment delimiter
                     which indicates anything to its right is a comment
                     then strip off the comment and also any white space
                     on the remaining right hand side.
                     It leaves any white space on the left hand side
                     alone.
  *)

  RemoveComment
  PROCEDURE RemoveComment (s: String; comment: CHAR) : String ;

  (*
     RemoveWhitePrefix - removes any leading white space from String, s.
                         A new string is returned.
  *)

  RemoveWhitePrefix
  PROCEDURE RemoveWhitePrefix (s: String) : String ;

  (*
     RemoveWhitePostfix - removes any leading white space from String, s.
                          A new string is returned.
  *)

  RemoveWhitePostfix
  PROCEDURE RemoveWhitePostfix (s: String) : String ;

  (*
     ToUpper - returns string, s, after it has had its lower case
               characters replaced by upper case characters.
               The string, s, is not duplicated.
  *)

  ToUpper
  PROCEDURE ToUpper (s: String) : String ;

  (*
     ToLower - returns string, s, after it has had its upper case
               characters replaced by lower case characters.
               The string, s, is not duplicated.
  *)

  ToLower
  PROCEDURE ToLower (s: String) : String ;

  (*
     CopyOut - copies string, s, to a.
  *)

  CopyOut
  PROCEDURE CopyOut (VAR a: ARRAY OF CHAR; s: String) ;

  (*
     char - returns the character, ch, at position, i, in String, s.
            As Slice the index can be negative so:

            char(s, 0) will return the first character
            char(s, 1) will return the second character
            char(s, -1) will return the last character
            char(s, -2) will return the penultimate character

            a nul character is returned if the index is out of range.
  *)

  char
  PROCEDURE char (s: String; i: INTEGER) : CHAR ;

  (*
     string - returns the C style char * of String, s.
  *)

  string
  PROCEDURE string (s: String) : ADDRESS ;

  (*
     to easily debug an application using this library one could use
     use the following macro processing defines:

     #define InitString(X) InitStringDB(X, __FILE__, __LINE__)
     #define InitStringCharStar(X) InitStringCharStarDB(X, \
       __FILE__, __LINE__)
     #define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
     #define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
     #define Dup(X) DupDB(X, __FILE__, __LINE__)
     #define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)

     and then invoke gm2 with the -fcpp flag.
  *)

  (*
     InitStringDB - the debug version of InitString.
  *)

  InitStringDB
  PROCEDURE InitStringDB (a: ARRAY OF CHAR;
                          file: ARRAY OF CHAR; line: CARDINAL) : String ;

  (*
     InitStringCharStarDB - the debug version of InitStringCharStar.
  *)

  InitStringCharStarDB
  PROCEDURE InitStringCharStarDB (a: ADDRESS;
                                  file: ARRAY OF CHAR;
                                  line: CARDINAL) : String ;

  (*
     InitStringCharDB - the debug version of InitStringChar.
  *)

  InitStringCharDB
  PROCEDURE InitStringCharDB (ch: CHAR;
                              file: ARRAY OF CHAR;
                              line: CARDINAL) : String ;

  (*
     MultDB - the debug version of MultDB.
  *)

  MultDB
  PROCEDURE MultDB (s: String; n: CARDINAL;
                    file: ARRAY OF CHAR; line: CARDINAL) : String ;

  (*
     DupDB - the debug version of Dup.
  *)

  DupDB
  PROCEDURE DupDB (s: String;
                   file: ARRAY OF CHAR; line: CARDINAL) : String ;

  (*
     SliceDB - debug version of Slice.
  *)

  SliceDB
  PROCEDURE SliceDB (s: String; low, high: INTEGER;
                     file: ARRAY OF CHAR; line: CARDINAL) : String ;

  (*
     PushAllocation - pushes the current allocation/deallocation lists.
  *)

  PushAllocation
  PROCEDURE PushAllocation ;

  (*
     PopAllocation - test to see that all strings are deallocated since
                     the last push.  Then it pops to the previous
                     allocation/deallocation lists.

                     If halt is true then the application terminates
                     with an exit code of 1.
  *)

  PopAllocation
  PROCEDURE PopAllocation (halt: BOOLEAN) ;

  (*
     PopAllocationExemption - test to see that all strings are
                              deallocated, except string, e, since
                              the last push.
                              Then it pops to the previous
                              allocation/deallocation lists.

                              If halt is true then the application
                              terminates with an exit code of 1.

                              The string, e, is returned unmodified,
  *)

  PopAllocationExemption
  PROCEDURE PopAllocationExemption (halt: BOOLEAN; e: String) : String ;

  END DynamicStrings.

