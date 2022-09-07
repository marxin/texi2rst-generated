.. _gm2-libs-pim-floatingutilities:

gm2-libs-pim/FloatingUtilities
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FloatingUtilities ;

  EXPORT QUALIFIED Frac, Round, Float, Trunc,
                   Fracl, Roundl, Floatl, Truncl ;

  (*
     Frac - returns the fractional component of, r.
  *)

  Frac
  PROCEDURE Frac (r: REAL) : REAL ;

  (*
     Int - returns the integer part of r. It rounds the value towards zero.
  *)

  Int
  PROCEDURE Int (r: REAL) : INTEGER ;

  (*
     Round - returns the number rounded to the nearest integer.
  *)

  Round
  PROCEDURE Round (r: REAL) : INTEGER ;

  (*
     Float - returns a REAL value corresponding to, i.
  *)

  Float
  PROCEDURE Float (i: INTEGER) : REAL ;

  (*
     Trunc - round to the nearest integer not larger in absolute
             value.
  *)

  Trunc
  PROCEDURE Trunc (r: REAL) : INTEGER ;

  (*
     Fracl - returns the fractional component of, r.
  *)

  Fracl
  PROCEDURE Fracl (r: LONGREAL) : LONGREAL ;

  (*
     Intl - returns the integer part of r. It rounds the value towards zero.
  *)

  Intl
  PROCEDURE Intl (r: LONGREAL) : LONGINT ;

  (*
     Roundl - returns the number rounded to the nearest integer.
  *)

  Roundl
  PROCEDURE Roundl (r: LONGREAL) : LONGINT ;

  (*
     Floatl - returns a REAL value corresponding to, i.
  *)

  Floatl
  PROCEDURE Floatl (i: INTEGER) : LONGREAL ;

  (*
     Truncl - round to the nearest integer not larger in absolute
              value.
  *)

  Truncl
  PROCEDURE Truncl (r: LONGREAL) : LONGINT ;

  END FloatingUtilities.

