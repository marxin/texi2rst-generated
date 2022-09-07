.. _gm2-libs-lmathlib0:

gm2-libs/LMathLib0
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE LMathLib0 ;

  CONST
     pi   = 3.1415926535897932384626433832795028841972;
     exp1 = 2.7182818284590452353602874713526624977572;

  sqrt
  PROCEDURE __BUILTIN__ sqrt (x: LONGREAL) : LONGREAL ;
  exp
  PROCEDURE exp (x: LONGREAL) : LONGREAL ;
  ln
  PROCEDURE ln (x: LONGREAL) : LONGREAL ;
  sin
  PROCEDURE __BUILTIN__ sin (x: LONGREAL) : LONGREAL ;
  cos
  PROCEDURE __BUILTIN__ cos (x: LONGREAL) : LONGREAL ;
  tan
  PROCEDURE tan (x: LONGREAL) : LONGREAL ;
  arctan
  PROCEDURE arctan (x: LONGREAL) : LONGREAL ;
  entier
  PROCEDURE entier (x: LONGREAL) : INTEGER ;

  END LMathLib0.

