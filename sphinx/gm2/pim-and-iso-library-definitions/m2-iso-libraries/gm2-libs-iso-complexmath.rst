.. _gm2-libs-iso-complexmath:

gm2-libs-iso/ComplexMath
^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ComplexMath;

    (* Mathematical functions for the type COMPLEX *)

  CONST
  i  (const)
    i =    CMPLX (0.0, 1.0);
  one  (const)
    one =  CMPLX (1.0, 0.0);
  zero  (const)
    zero = CMPLX (0.0, 0.0);

  abs
  PROCEDURE __BUILTIN__ abs (z: COMPLEX): REAL;
    (* Returns the length of z *)

  arg
  PROCEDURE __BUILTIN__ arg (z: COMPLEX): REAL;
    (* Returns the angle that z subtends to the positive real axis *)

  conj
  PROCEDURE __BUILTIN__ conj (z: COMPLEX): COMPLEX;
    (* Returns the complex conjugate of z *)

  power
  PROCEDURE __BUILTIN__ power (base: COMPLEX; exponent: REAL): COMPLEX;
    (* Returns the value of the number base raised to the power exponent *)

  sqrt
  PROCEDURE __BUILTIN__ sqrt (z: COMPLEX): COMPLEX;
    (* Returns the principal square root of z *)

  exp
  PROCEDURE __BUILTIN__ exp (z: COMPLEX): COMPLEX;
    (* Returns the complex exponential of z *)

  ln
  PROCEDURE __BUILTIN__ ln (z: COMPLEX): COMPLEX;
    (* Returns the principal value of the natural logarithm of z *)

  sin
  PROCEDURE __BUILTIN__ sin (z: COMPLEX): COMPLEX;
    (* Returns the sine of z *)

  cos
  PROCEDURE __BUILTIN__ cos (z: COMPLEX): COMPLEX;
    (* Returns the cosine of z *)

  tan
  PROCEDURE __BUILTIN__ tan (z: COMPLEX): COMPLEX;
    (* Returns the tangent of z *)

  arcsin
  PROCEDURE __BUILTIN__ arcsin (z: COMPLEX): COMPLEX;
    (* Returns the arcsine of z *)

  arccos
  PROCEDURE __BUILTIN__ arccos (z: COMPLEX): COMPLEX;
    (* Returns the arccosine of z *)

  arctan
  PROCEDURE __BUILTIN__ arctan (z: COMPLEX): COMPLEX;
    (* Returns the arctangent of z *)

  polarToComplex
  PROCEDURE polarToComplex (abs, arg: REAL): COMPLEX;
    (* Returns the complex number with the specified polar coordinates *)

  scalarMult
  PROCEDURE scalarMult (scalar: REAL; z: COMPLEX): COMPLEX;
    (* Returns the scalar product of scalar with z *)

  IsCMathException
  PROCEDURE IsCMathException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional
       execution state because of the raising of an exception in a
       routine from this module; otherwise returns FALSE.
    *)

  END ComplexMath.

