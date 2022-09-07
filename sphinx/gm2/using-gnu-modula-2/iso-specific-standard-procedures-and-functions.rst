ISO specific standard procedures and functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The standard function ``LENGTH`` is specific to ISO Modula-2 and
is defined as:

.. code-block:: modula2

  (*
     IM - returns the imaginary component of a complex type.
          The return value will the same type as the imaginary field
          within the complex type.
  *)

  IM
  PROCEDURE IM (c: <any complex type>) : <floating point type> ;

.. code-block:: modula2

  (*
     INT - returns an INTEGER value which has the same value as v.
           This function is equivalent to: VAL(INTEGER, v).
  *)

  INT
  PROCEDURE INT (v: <any ordinal type>) : INTEGER ;

.. code-block:: modula2

  (*
     LENGTH - returns the length of string a.
  *)

  LENGTH
  PROCEDURE LENGTH (a: ARRAY OF CHAR) : CARDINAL ;

This function is evaluated at compile time, providing that string
``a`` is a constant. If ``a`` cannot be evaluated then a call is
made to ``M2RTS.Length``.

.. code-block:: modula2

  (*
     ODD - returns a BOOLEAN indicating whether the whole number
           value, v, is odd.
  *)

  ODD
  PROCEDURE ODD (v: <any whole number type>) : BOOLEAN ;

.. code-block:: modula2

  (*
     RE - returns the real component of a complex type.
          The return value will the same type as the real field
          within the complex type.
  *)

  RE
  PROCEDURE RE (c: <any complex type>) : <floating point type> ;

