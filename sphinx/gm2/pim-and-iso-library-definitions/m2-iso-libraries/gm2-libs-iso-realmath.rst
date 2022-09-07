.. _gm2-libs-iso-realmath:

gm2-libs-iso/RealMath
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RealMath;

    (* Mathematical functions for the type REAL *)

  CONST
  pi    (const)
    pi   = 3.1415926535897932384626433832795028841972;
  exp1  (const)
    exp1 = 2.7182818284590452353602874713526624977572;

  sqrt
  PROCEDURE __BUILTIN__ sqrt (x: REAL): REAL;
    (* Returns the positive square root of x *)

  exp
  PROCEDURE __BUILTIN__ exp (x: REAL): REAL;
    (* Returns the exponential of x *)

  ln
  PROCEDURE __BUILTIN__ ln (x: REAL): REAL;
    (* Returns the natural logarithm of x *)

    (* The angle in all trigonometric functions is measured in radians *)

  sin
  PROCEDURE __BUILTIN__ sin (x: REAL): REAL;
    (* Returns the sine of x *)

  cos
  PROCEDURE __BUILTIN__ cos (x: REAL): REAL;
    (* Returns the cosine of x *)

  tan
  PROCEDURE tan (x: REAL): REAL;
    (* Returns the tangent of x *)

  arcsin
  PROCEDURE arcsin (x: REAL): REAL;
    (* Returns the arcsine of x *)

  arccos
  PROCEDURE arccos (x: REAL): REAL;
    (* Returns the arccosine of x *)

  arctan
  PROCEDURE arctan (x: REAL): REAL;
    (* Returns the arctangent of x *)

  power
  PROCEDURE power (base, exponent: REAL) : REAL;
    (* Returns the value of the number base raised to the power exponent *)

  round
  PROCEDURE round (x: REAL) : INTEGER;
    (* Returns the value of x rounded to the nearest integer *)

  IsRMathException
  PROCEDURE IsRMathException () : BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional execution state
       because of the raising of an exception in a routine from this module; otherwise
       returns FALSE.
    *)

  END RealMath.

