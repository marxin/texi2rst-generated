.. _gm2-libs-mathlib0:

gm2-libs/MathLib0
^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE MathLib0 ;

  CONST
     pi   = 3.1415926535897932384626433832795028841972;
     exp1 = 2.7182818284590452353602874713526624977572;

  sqrt
  PROCEDURE __BUILTIN__ sqrt (x: REAL) : REAL ;
  exp
  PROCEDURE exp (x: REAL) : REAL ;
  ln
  PROCEDURE ln (x: REAL) : REAL ;
  sin
  PROCEDURE __BUILTIN__ sin (x: REAL) : REAL ;
  cos
  PROCEDURE __BUILTIN__ cos (x: REAL) : REAL ;
  tan
  PROCEDURE tan (x: REAL) : REAL ;
  arctan
  PROCEDURE arctan (x: REAL) : REAL ;
  entier
  PROCEDURE entier (x: REAL) : INTEGER ;

  END MathLib0.

