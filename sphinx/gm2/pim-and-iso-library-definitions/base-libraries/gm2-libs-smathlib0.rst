.. _gm2-libs-smathlib0:

gm2-libs/SMathLib0
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE SMathLib0 ;

  CONST
     pi   = 3.1415926535897932384626433832795028841972;
     exp1 = 2.7182818284590452353602874713526624977572;

  sqrt
  PROCEDURE __BUILTIN__ sqrt (x: SHORTREAL) : SHORTREAL ;
  exp
  PROCEDURE exp (x: SHORTREAL) : SHORTREAL ;
  ln
  PROCEDURE ln (x: SHORTREAL) : SHORTREAL ;
  sin
  PROCEDURE __BUILTIN__ sin (x: SHORTREAL) : SHORTREAL ;
  cos
  PROCEDURE __BUILTIN__ cos (x: SHORTREAL) : SHORTREAL ;
  tan
  PROCEDURE tan (x: SHORTREAL) : SHORTREAL ;
  arctan
  PROCEDURE arctan (x: SHORTREAL) : SHORTREAL ;
  entier
  PROCEDURE entier (x: SHORTREAL) : INTEGER ;

  END SMathLib0.

