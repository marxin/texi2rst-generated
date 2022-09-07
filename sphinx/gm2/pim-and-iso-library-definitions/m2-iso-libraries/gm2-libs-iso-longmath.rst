.. _gm2-libs-iso-longmath:

gm2-libs-iso/LongMath
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE LongMath;

    (* Mathematical functions for the type LONGREAL *)

  CONST
  pi    (const)
    pi   = 3.1415926535897932384626433832795028841972;
  exp1  (const)
    exp1 = 2.7182818284590452353602874713526624977572;

  sqrt
  PROCEDURE __BUILTIN__ sqrt (x: LONGREAL): LONGREAL;
    (* Returns the positive square root of x *)

  exp
  PROCEDURE __BUILTIN__ exp (x: LONGREAL): LONGREAL;
    (* Returns the exponential of x *)

  ln
  PROCEDURE __BUILTIN__ ln (x: LONGREAL): LONGREAL;
    (* Returns the natural logarithm of x *)

    (* The angle in all trigonometric functions is measured in radians *)

  sin
  PROCEDURE __BUILTIN__ sin (x: LONGREAL): LONGREAL;
    (* Returns the sine of x *)

  cos
  PROCEDURE __BUILTIN__ cos (x: LONGREAL): LONGREAL;
    (* Returns the cosine of x *)

  tan
  PROCEDURE tan (x: LONGREAL): LONGREAL;
    (* Returns the tangent of x *)

  arcsin
  PROCEDURE arcsin (x: LONGREAL): LONGREAL;
    (* Returns the arcsine of x *)

  arccos
  PROCEDURE arccos (x: LONGREAL): LONGREAL;
    (* Returns the arccosine of x *)

  arctan
  PROCEDURE arctan (x: LONGREAL): LONGREAL;
    (* Returns the arctangent of x *)

  power
  PROCEDURE power (base, exponent: LONGREAL): LONGREAL;
    (* Returns the value of the number base raised to the power exponent *)

  round
  PROCEDURE round (x: LONGREAL): INTEGER;
    (* Returns the value of x rounded to the nearest integer *)

  IsRMathException
  PROCEDURE IsRMathException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional
       execution state because of the raising of an exception in a
       routine from this module; otherwise returns FALSE.
    *)

  END LongMath.

