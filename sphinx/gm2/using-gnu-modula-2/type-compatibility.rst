.. _type-compatibility:

Type compatibility
******************

This section discuss the issues surrounding assignment, expression
and parameter compatibility, their effect of the additional
fixed sized datatypes and also their effect of runtime checking.
The data types supported by the compiler are:

.. code-block:: modula2

  GNU Modula-2              scope      switches
  =============================================
  INTEGER                   pervasive
  LONGINT                   pervasive
  SHORTINT                  pervasive
  CARDINAL                  pervasive
  LONGCARD                  pervasive
  SHORTCARD                 pervasive
  BOOLEAN                   pervasive
  BITSET                    pervasive
  REAL                      pervasive
  LONGREAL                  pervasive
  SHORTREAL                 pervasive
  CHAR                      pervasive
  SHORTCOMPLEX              pervasive
  COMPLEX                   pervasive
  LONGCOMPLEX               pervasive

  LOC                       SYSTEM     -fiso
  BYTE                      SYSTEM
  WORD                      SYSTEM
  ADDRESS                   SYSTEM

  The following extensions are supported for
  most architectures (please check SYSTEM.def).
  =============================================
  INTEGER8                  SYSTEM
  INTEGER16                 SYSTEM
  INTEGER32                 SYSTEM
  INTEGER64                 SYSTEM
  CARDINAL8                 SYSTEM
  CARDINAL16                SYSTEM
  CARDINAL32                SYSTEM
  CARDINAL64                SYSTEM
  BITSET8                   SYSTEM
  BITSET16                  SYSTEM
  BITSET32                  SYSTEM
  WORD16                    SYSTEM
  WORD32                    SYSTEM
  WORD64                    SYSTEM
  REAL32                    SYSTEM
  REAL64                    SYSTEM
  REAL96                    SYSTEM
  REAL128                   SYSTEM
  COMPLEX32                 SYSTEM
  COMPLEX64                 SYSTEM
  COMPLEX96                 SYSTEM
  COMPLEX128                SYSTEM

The Modula-2 language categorises compatibility between entities of
possibly differing types into three subcomponents: expressions,
assignments, and parameters.  Parameter compatibility is further
divided into two sections for pass by reference and pass by value
compatibility.

For more detail on the Modula-2 type compatibility see the Modula-2
ISO standard BS ISO/IEC 10514-1:1996 page 121-125.  For detail on the
PIM type compatibility see Programming in Modula-2 Edition 4 page 29,
(Elementary Data Types).

.. toctree::
  :maxdepth: 2

  expression-compatibility
  assignment-compatibility
  parameter-compatibility

