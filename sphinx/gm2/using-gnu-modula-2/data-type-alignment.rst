.. _alignment:

Data type alignment
*******************

GNU Modula-2 allows you to specify alignment for types and variables.
The syntax for alignment is to use the ISO pragma directives ``<*``
``bytealignment (`` expression ``)`` and ``*>``.  These directives
can be used after type and variable declarations.

The ebnf of the alignment production is:

.. code-block:: modula2

  Alignment := [ ByteAlignment ] =:
  ByteAlignment := '<*' AttributeExpression '*>' =:
  AlignmentExpression := "(" ConstExpression ")" =:

The ``Alignment`` ebnf statement may be used during contruction of
types, records, record fields, arrays, pointers and variables.  Below
is an example of aligning a type so that the variable ``bar`` is
aligned on a 1024 address.

.. code-block:: modula2

  MODULE align ;

  TYPE
     foo = INTEGER <* bytealignment(1024) *> ;

  VAR
     z  : INTEGER ;
     bar: foo ;
  BEGIN
  END align.

The next example aligns a variable on a 1024 byte boundary.

.. code-block:: modula2

  MODULE align2 ;

  VAR
     x  : CHAR ;
     z  : ARRAY [0..255] OF INTEGER <* bytealignment(1024) *> ;
  BEGIN
  END align2.

Here the example aligns a pointer on a 1024 byte boundary.

.. code-block:: modula2

  MODULE align4 ;

  FROM SYSTEM IMPORT ADR ;
  FROM libc IMPORT exit ;

  VAR
     x  : CHAR ;
     z  : POINTER TO INTEGER <* bytealignment(1024) *> ;
  BEGIN
     IF ADR(z) MOD 1024=0
     THEN
        exit(0)
     ELSE
        exit(1)
     END
  END align4.

In example ``align5`` record field ``y`` is aligned on a 1024
byte boundary.

.. code-block:: modula2

  MODULE align5 ;

  FROM SYSTEM IMPORT ADR ;
  FROM libc IMPORT exit ;

  TYPE
     rec = RECORD
              x: CHAR ;
              y: CHAR <* bytealignment(1024) *> ;
           END ;
  VAR
     r: rec ;
  BEGIN
     IF ADR(r.y) MOD 1024=0
     THEN
        exit(0)
     ELSE
        exit(1)
     END
  END align5.

In the example below module ``align6`` declares ``foo`` as an
array of 256 ``INTEGER`` s.  The array ``foo`` is aligned on a
1024 byte boundary.

.. code-block:: modula2

  MODULE align6 ;

  FROM SYSTEM IMPORT ADR ;
  FROM libc IMPORT exit ;

  TYPE
     foo = ARRAY [0..255] OF INTEGER <* bytealignment(1024) *> ;

  VAR
     x  : CHAR ;
     z  : foo ;
  BEGIN
     IF ADR(z) MOD 1024=0
     THEN
        exit(0)
     ELSE
        exit(1)
     END
  END align6.

