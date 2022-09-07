.. _gm2-libs-iso-shortcomplexmath:

gm2-libs-iso/ShortComplexMath
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ShortComplexMath;

    (* Mathematical functions for the type SHORTCOMPLEX *)

  CONST
  i  (const)
    i =    CMPLX (0.0, 1.0);
  one  (const)
    one =  CMPLX (1.0, 0.0);
  zero  (const)
    zero = CMPLX (0.0, 0.0);

  abs
  PROCEDURE abs (z: SHORTCOMPLEX): SHORTREAL;
    (* Returns the length of z *)

  arg
  PROCEDURE arg (z: SHORTCOMPLEX): SHORTREAL;
    (* Returns the angle that z subtends to the positive real axis *)

  conj
  PROCEDURE conj (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the complex conjugate of z *)

  power
  PROCEDURE power (base: SHORTCOMPLEX; exponent: SHORTREAL): SHORTCOMPLEX;
    (* Returns the value of the number base raised to the power exponent *)

  sqrt
  PROCEDURE sqrt (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the principal square root of z *)

  exp
  PROCEDURE exp (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the complex exponential of z *)

  ln
  PROCEDURE ln (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the principal value of the natural logarithm of z *)

  sin
  PROCEDURE sin (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the sine of z *)

  cos
  PROCEDURE cos (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the cosine of z *)

  tan
  PROCEDURE tan (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the tangent of z *)

  arcsin
  PROCEDURE arcsin (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the arcsine of z *)

  arccos
  PROCEDURE arccos (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the arccosine of z *)

  arctan
  PROCEDURE arctan (z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the arctangent of z *)

  polarToComplex
  PROCEDURE polarToComplex (abs, arg: SHORTREAL): SHORTCOMPLEX;
    (* Returns the complex number with the specified polar coordinates *)

  scalarMult
  PROCEDURE scalarMult (scalar: SHORTREAL; z: SHORTCOMPLEX): SHORTCOMPLEX;
    (* Returns the scalar product of scalar with z *)

  IsCMathException
  PROCEDURE IsCMathException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional execution state
       because of the raising of an exception in a routine from this module; otherwise
       returns FALSE.
    *)

  END ShortComplexMath.

