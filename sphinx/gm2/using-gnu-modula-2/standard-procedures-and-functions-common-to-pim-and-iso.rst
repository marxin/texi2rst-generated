Standard procedures and functions common to PIM and ISO
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following procedures are implemented and conform with Programming
in Modula-2 and ISO Modula-2: ``NEW``, ``DISPOSE``, ``INC``,
``DEC``, ``INCL``, ``EXCL`` and ``HALT``.  The standard
functions are: ``ABS``, ``CAP``, ``CHR``, ``FLOAT``,
``HIGH``, ``LFLOAT``, ``LTRUNC``, ``MIN``, ``MAX``,
``ODD``, ``SFLOAT``, ``STRUNC`` ``TRUNC`` and
``VAL``. All these functions and procedures (except ``HALT``,
``NEW``, ``DISPOSE`` and, under non constant conditions,
``LENGTH``) generate in-line code for efficiency.

.. code-block:: modula2

  (*
     ABS - returns the positive value of i.
  *)

  ABS
  PROCEDURE ABS (i: <any signed type>) : <any signed type> ;

.. code-block:: modula2

  (*
     CAP - returns the capital of character ch providing
           ch lies within the range 'a'..'z'. Otherwise ch
           is returned unaltered.
  *)

  CAP
  PROCEDURE CAP (ch: CHAR) : CHAR ;

.. code-block:: modula2

  (*
     CHR - converts a value of a <whole number type> into a CHAR.
           CHR(x) is shorthand for VAL(CHAR, x).
  *)

  CHR
  PROCEDURE CHR (x: <whole number type>) : CHAR ;

.. code-block:: modula2

  (*
     DISPOSE - the procedure DISPOSE is replaced by:
               DEALLOCATE(p, TSIZE(p^)) ;
               The user is expected to import the procedure DEALLOCATE
               (normally found in the module, Storage.)

               In:  a variable p: of any pointer type which has been
                    initialized by a call to NEW.
               Out: the area of memory
                    holding p^ is returned to the system.
                    Note that the underlying procedure DEALLOCATE
                    procedure in module Storage will assign p to NIL.
  *)

  DISPOSE
  PROCEDURE DISPOSE (VAR p:<any pointer type>) ;

.. code-block:: modula2

  (*
     DEC - can either take one or two parameters.  If supplied
           with one parameter then on the completion of the call to
           DEC, v will have its predecessor value.  If two
           parameters are supplied then the value v will have its
           n'th predecessor.  For these reasons the value of n
           must be >=0.
  *)

  DEC
  PROCEDURE DEC (VAR v: <any base type>; [n: <any base type> = 1]) ;

.. code-block:: modula2

  (*
     EXCL - excludes bit element e from a set type s.
  *)

  EXCL
  PROCEDURE EXCL (VAR s: <any set type>; e: <element of set type s>) ;

.. code-block:: modula2

  (*
     FLOAT - will return a REAL number whose value is the same as o.
  *)

  FLOAT
  PROCEDURE FLOAT (o: <any whole number type>) : REAL ;

.. code-block:: modula2

  (*
     FLOATS - will return a SHORTREAL number whose value is the same as o.
  *)

  FLOATS
  PROCEDURE FLOATS (o: <any whole number type>) : REAL ;

.. code-block:: modula2

  (*
     FLOATL - will return a LONGREAL number whose value is the same as o.
  *)

  FLOATL
  PROCEDURE FLOATL (o: <any whole number type>) : REAL ;

.. code-block:: modula2

  (*
     HALT - will call the HALT procedure inside the module M2RTS.
            Users can replace M2RTS.
  *)

  HALT
  PROCEDURE HALT ;

.. code-block:: modula2

  (*
     HIGH - returns the last accessible index of an parameter declared as
            ARRAY OF CHAR. Thus

            PROCEDURE foo (a: ARRAY OF CHAR) ;
            VAR
               c: CARDINAL ;
            BEGIN
               c := HIGH(a)
            END foo ;

            BEGIN
               foo('hello')
            END

            will cause the local variable c to contain the value 4
  *)

  HIGH
  PROCEDURE HIGH (a: ARRAY OF CHAR) : CARDINAL ;

.. code-block:: modula2

  (*
     INC - can either take one or two parameters.  If supplied
           with one parameter then on the completion of the call to
           INC, v will have its successor value.  If two
           parameters are supplied then the value v will have its
           n'th successor.  For these reasons the value of n
           must be >=0.
  *)

  INC
  PROCEDURE INC (VAR v: <any base type>; [n: <any base type> = 1]) ;

.. code-block:: modula2

  (*
     INCL - includes bit element e to a set type s.
  *)

  INCL
  PROCEDURE INCL (VAR s: <any set type>; e: <element of set type s>) ;

.. code-block:: modula2

  (*
     LFLOAT - will return a LONGREAL number whose value is the same as o.
  *)

  LFLOAT
  PROCEDURE LFLOAT (o: <any whole number type>) : LONGREAL ;

.. code-block:: modula2

  (*
     LTRUNC - will return a LONG<type> number whose value is the
              same as o.  PIM2, PIM3 and ISO Modula-2 will return
              a LONGCARD whereas PIM4 returns LONGINT.
  *)

  LTRUNC
  PROCEDURE LTRUNC (o: <any floating point type>) : LONG<type> ;

.. code-block:: modula2

  (*
     MIN - returns the lowest legal value of an ordinal type.
  *)

  MIN
  PROCEDURE MIN (t: <ordinal type>) : <ordinal type> ;

.. code-block:: modula2

  (*
     MAX - returns the largest legal value of an ordinal type.
  *)

  MAX
  PROCEDURE MAX (t: <ordinal type>) : <ordinal type> ;

.. code-block:: modula2

  (*
     NEW - the procedure NEW is replaced by:
           ALLOCATE(p, TSIZE(p^)) ;
           The user is expected to import the procedure ALLOCATE
           (normally found in the module, Storage.)

           In:  a variable p: of any pointer type.
           Out: variable p is set to some allocated memory
                which is large enough to hold all the contents of p^.
  *)

  NEW
  PROCEDURE NEW (VAR p:<any pointer type>) ;

.. code-block:: modula2

  (*
     ODD - returns TRUE if the value is not divisible by 2.
  *)

  ODD
  PROCEDURE ODD (x: <whole number type>) : BOOLEAN ;

.. code-block:: modula2

  (*
     SFLOAT - will return a SHORTREAL number whose value is the same
              as o.
  *)

  SFLOAT
  PROCEDURE SFLOAT (o: <any whole number type>) : SHORTREAL ;

.. code-block:: modula2

  (*
     STRUNC - will return a SHORT<type> number whose value is the same
              as o.  PIM2, PIM3 and ISO Modula-2 will return a
              SHORTCARD whereas PIM4 returns SHORTINT.
  *)

  STRUNC
  PROCEDURE STRUNC (o: <any floating point type>) : SHORT<type> ;

.. code-block:: modula2

  (*
     TRUNC - will return a <type> number whose value is the same as o.
             PIM2, PIM3 and ISO Modula-2 will return a CARDINAL
             whereas PIM4 returns INTEGER.
  *)

  TRUNC
  PROCEDURE TRUNC (o: <any floating point type>) : <type> ;

.. code-block:: modula2

  (*
     TRUNCS - will return a <type> number whose value is the same
              as o.  PIM2, PIM3 and ISO Modula-2 will return a
              SHORTCARD whereas PIM4 returns SHORTINT.
  *)

  TRUNCS
  PROCEDURE TRUNCS (o: <any floating point type>) : <type> ;

.. code-block:: modula2

  (*
     TRUNCL - will return a <type> number whose value is the same
              as o.  PIM2, PIM3 and ISO Modula-2 will return a
              LONGCARD whereas PIM4 returns LONGINT.
  *)

  TRUNCL
  PROCEDURE TRUNCL (o: <any floating point type>) : <type> ;

.. code-block:: modula2

  (*
     VAL - converts data i of <any simple data type 2> to
           <any simple data type 1> and returns this value.
           No range checking is performed during this conversion.
  *)

  VAL
  PROCEDURE VAL (<any simple data type 1>,
                 i: <any simple data type 2>) : <any simple data type 1> ;

