.. _gm2-libs-iso-longcomplexmath:

gm2-libs-iso/LongComplexMath
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE LongComplexMath;

    (* Mathematical functions for the type LONGCOMPLEX *)

  CONST
  i  (const)
    i =    CMPLX (0.0, 1.0);
  one  (const)
    one =  CMPLX (1.0, 0.0);
  zero  (const)
    zero = CMPLX (0.0, 0.0);

  abs
  PROCEDURE abs (z: LONGCOMPLEX): LONGREAL;
    (* Returns the length of z *)

  arg
  PROCEDURE arg (z: LONGCOMPLEX): LONGREAL;
    (* Returns the angle that z subtends to the positive real axis *)

  conj
  PROCEDURE conj (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the complex conjugate of z *)

  power
  PROCEDURE power (base: LONGCOMPLEX; exponent: LONGREAL): LONGCOMPLEX;
    (* Returns the value of the number base raised to the power exponent *)

  sqrt
  PROCEDURE sqrt (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the principal square root of z *)

  exp
  PROCEDURE exp (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the complex exponential of z *)

  ln
  PROCEDURE ln (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the principal value of the natural logarithm of z *)

  sin
  PROCEDURE sin (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the sine of z *)

  cos
  PROCEDURE cos (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the cosine of z *)

  tan
  PROCEDURE tan (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the tangent of z *)

  arcsin
  PROCEDURE arcsin (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the arcsine of z *)

  arccos
  PROCEDURE arccos (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the arccosine of z *)

  arctan
  PROCEDURE arctan (z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the arctangent of z *)

  polarToComplex
  PROCEDURE polarToComplex (abs, arg: LONGREAL): LONGCOMPLEX;
    (* Returns the complex number with the specified polar coordinates *)

  scalarMult
  PROCEDURE scalarMult (scalar: LONGREAL; z: LONGCOMPLEX): LONGCOMPLEX;
    (* Returns the scalar product of scalar with z *)

  IsCMathException
  PROCEDURE IsCMathException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional execution state
       because of the raising of an exception in a routine from this module; otherwise
       returns FALSE.
    *)

  END LongComplexMath.

