.. _gm2-libs-libm:

gm2-libs/libm
^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FOR "C" libm ;

  (* Users are strongly advised to use MathLib0 or RealMath as calls
     to functions within these modules will generate inline code.
     This module is used by MathLib0 and RealMath when inline code cannot
     be generated.  *)

  EXPORT UNQUALIFIED sin, sinl, sinf,
                     cos, cosl, cosf,
                     tan, tanl, tanf,
                     sqrt, sqrtl, sqrtf,
                     asin, asinl, asinf,
                     acos, acosl, acosf,
                     atan, atanl, atanf,
                     atan2, atan2l, atan2f,
                     exp, expl, expf,
                     log, logl, logf,
                     exp10, exp10l, exp10f,
                     pow, powl, powf,
                     floor, floorl, floorf,
                     ceil, ceill, ceilf ;

  sin
  PROCEDURE sin (x: REAL) : REAL ;
  sinl
  PROCEDURE sinl (x: LONGREAL) : LONGREAL ;
  sinf
  PROCEDURE sinf (x: SHORTREAL) : SHORTREAL ;
  cos
  PROCEDURE cos (x: REAL) : REAL ;
  cosl
  PROCEDURE cosl (x: LONGREAL) : LONGREAL ;
  cosf
  PROCEDURE cosf (x: SHORTREAL) : SHORTREAL ;
  tan
  PROCEDURE tan (x: REAL) : REAL ;
  tanl
  PROCEDURE tanl (x: LONGREAL) : LONGREAL ;
  tanf
  PROCEDURE tanf (x: SHORTREAL) : SHORTREAL ;
  sqrt
  PROCEDURE sqrt (x: REAL) : REAL ;
  sqrtl
  PROCEDURE sqrtl (x: LONGREAL) : LONGREAL ;
  sqrtf
  PROCEDURE sqrtf (x: SHORTREAL) : SHORTREAL ;
  asin
  PROCEDURE asin (x: REAL) : REAL ;
  asinl
  PROCEDURE asinl (x: LONGREAL) : LONGREAL ;
  asinf
  PROCEDURE asinf (x: SHORTREAL) : SHORTREAL ;
  acos
  PROCEDURE acos (x: REAL) : REAL ;
  acosl
  PROCEDURE acosl (x: LONGREAL) : LONGREAL ;
  acosf
  PROCEDURE acosf (x: SHORTREAL) : SHORTREAL ;
  atan
  PROCEDURE atan (x: REAL) : REAL ;
  atanl
  PROCEDURE atanl (x: LONGREAL) : LONGREAL ;
  atanf
  PROCEDURE atanf (x: SHORTREAL) : SHORTREAL ;
  atan2
  PROCEDURE atan2 (x, y: REAL) : REAL ;
  atan2l
  PROCEDURE atan2l (x, y: LONGREAL) : LONGREAL ;
  atan2f
  PROCEDURE atan2f (x, y: SHORTREAL) : SHORTREAL ;
  exp
  PROCEDURE exp (x: REAL) : REAL ;
  expl
  PROCEDURE expl (x: LONGREAL) : LONGREAL ;
  expf
  PROCEDURE expf (x: SHORTREAL) : SHORTREAL ;
  log
  PROCEDURE log (x: REAL) : REAL ;
  logl
  PROCEDURE logl (x: LONGREAL) : LONGREAL ;
  logf
  PROCEDURE logf (x: SHORTREAL) : SHORTREAL ;
  exp10
  PROCEDURE exp10 (x: REAL) : REAL ;
  exp10l
  PROCEDURE exp10l (x: LONGREAL) : LONGREAL ;
  exp10f
  PROCEDURE exp10f (x: SHORTREAL) : SHORTREAL ;
  pow
  PROCEDURE pow (x, y: REAL) : REAL ;
  powl
  PROCEDURE powl (x, y: LONGREAL) : LONGREAL ;
  powf
  PROCEDURE powf (x, y: SHORTREAL) : SHORTREAL ;
  floor
  PROCEDURE floor (x: REAL) : REAL ;
  floorl
  PROCEDURE floorl (x: LONGREAL) : LONGREAL ;
  floorf
  PROCEDURE floorf (x: SHORTREAL) : SHORTREAL ;
  ceil
  PROCEDURE ceil (x: REAL) : REAL ;
  ceill
  PROCEDURE ceill (x: LONGREAL) : LONGREAL ;
  ceilf
  PROCEDURE ceilf (x: SHORTREAL) : SHORTREAL ;

  END libm.

