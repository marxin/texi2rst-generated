.. _extensions:

GNU Modula-2 language extensions
********************************

This section introduces the GNU Modula-2 language extensions.
The GNU Modula-2 compiler allows abstract data types to be any type,
not just restricted to a pointer type providing the
:samp:`-fextended-opaque` option is supplied
See :ref:`compiler-options`.

Declarations can be made in any order, whether they are
types, constants, procedures, nested modules or variables.

.. (@xref{Passes, , ,}.)

GNU Modula-2 also allows programmers to interface to ``C`` and
assembly language.

GNU Modula-2 provides support for the special tokens ``__LINE__``,
``__FILE__``, ``__FUNCTION__`` and ``__DATE__``.  Support for
these tokens will occur even if the :samp:`-fcpp` option is not
supplied. A table of these identifiers and their data type and values
is given below:

.. code-block:: modula2

  Scope       GNU Modula-2 token      Data type and example value

  anywhere    __LINE__                Constant Literal compatible
                                      with CARDINAL, INTEGER and WORD.
                                      Example 1234

  anywhere    __FILE__                Constant string compatible
                                      with parameter ARRAY OF CHAR or
                                      an ARRAY whose SIZE is >= string
                                      length. Example
                                      "hello.mod"

  procedure   __FUNCTION__            Constant string compatible
                                      with parameter ARRAY OF CHAR or
                                      an ARRAY whose SIZE is >= string
                                      length. Example
                                      "calc"

  module      __FUNCTION__            Example
                                      "module hello initialization"

  anywhere    __DATE__                Constant string compatible
                                      with parameter ARRAY OF CHAR or
                                      an ARRAY whose SIZE is >= string
                                      length. Example
                                      "Thu Apr 29 10:07:16 BST 2004"

  anywhere   __COLUMN__               Gives a contant literal number
                                      determining the left hand column
                                      where the first _ appears in
                                      __COLUMN__.  The left most column
                                      is 1.

The preprocessor :samp:`cpp` can be invoked via the :samp:`-fcpp`
command line option. This in turn invokes :samp:`cpp` with the
following arguments :samp:`-traditional -lang-asm`.  These options
preserve comments and all quotations.  :samp:`gm2` treats a :samp:`#`
character in the first column as a preprocessor directive.

For example here is a module which calls ``FatalError``
via the macro ``ERROR``.

.. code-block:: modula2

  MODULE cpp ;

  FROM SYSTEM IMPORT ADR, SIZE ;
  FROM libc IMPORT exit, printf, malloc ;

  PROCEDURE FatalError (a, file: ARRAY OF CHAR;
                           line: CARDINAL;
                           func: ARRAY OF CHAR) ;
  BEGIN
     printf("%s:%d:fatal error, %s, in %s\n",
             ADR(file), line, ADR(a), ADR(func)) ;
     exit(1)
  END FatalError ;

  #define ERROR(X)  FatalError(X, __FILE__, __LINE__, __FUNCTION__)

  VAR
     pc: POINTER TO CARDINAL;
  BEGIN
     pc := malloc(SIZE(CARDINAL)) ;
     IF pc=NIL
     THEN
        ERROR('out of memory')
     END
  END cpp.

Another use for the C preprocessor in Modula-2 might be to turn on
debugging code.  For example the library module
:samp:`FormatStrings.mod` uses procedures from :samp:`DynamicStrings.mod`
and to track down memory leaks it was useful to track the source file
and line where each string was created.  Here is a section of
:samp:`FormatStrings.mod` which shows how the debugging code was
enabled and disabled by adding ``-fcpp`` to the command line.

.. code-block:: modula2

  FROM DynamicStrings IMPORT String, InitString, InitStringChar, Mark,
                             ConCat, Slice, Index, char,
                             Assign, Length, Mult, Dup, ConCatChar,
                             PushAllocation, PopAllocationExemption,
                             InitStringDB, InitStringCharStarDB,
                             InitStringCharDB, MultDB, DupDB, SliceDB ;

  (*
  #define InitString(X) InitStringDB(X, __FILE__, __LINE__)
  #define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, \
                                                     __LINE__)
  #define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
  #define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
  #define Dup(X) DupDB(X, __FILE__, __LINE__)
  #define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
  *)

  PROCEDURE doDSdbEnter ;
  BEGIN
     PushAllocation
  END doDSdbEnter ;

  PROCEDURE doDSdbExit (s: String) ;
  BEGIN
     s := PopAllocationExemption(TRUE, s)
  END doDSdbExit ;

  PROCEDURE DSdbEnter ;
  BEGIN
  END DSdbEnter ;

  PROCEDURE DSdbExit (s: String) ;
  BEGIN
  END DSdbExit ;

  (*
  #define DBsbEnter doDBsbEnter
  #define DBsbExit  doDBsbExit
  *)

  PROCEDURE Sprintf1 (s: String; w: ARRAY OF BYTE) : String ;
  BEGIN
     DSdbEnter ;
     s := FormatString(HandleEscape(s), w) ;
     DSdbExit(s) ;
     RETURN( s )
  END Sprintf1 ;

It is worth noting that the overhead of this code once ``-fcpp`` is
not present and -O2 is used will be zero since the local empty
procedures ``DSdbEnter`` and ``DSdbExit`` will be thrown away by
the optimization passes of the GCC backend.

.. toctree::
  :maxdepth: 2

  optional-procedure-parameter

