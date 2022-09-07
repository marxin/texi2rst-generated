.. _gm2-libs-pim-random:

gm2-libs-pim/Random
^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Random ;

  FROM SYSTEM IMPORT BYTE ;
  EXPORT QUALIFIED Randomize, RandomInit, RandomBytes, RandomCard, RandomInt, RandomReal, RandomLongReal ;

  (*
     Randomize - initialize the random number generator with a seed
                 based on the microseconds.
  *)

  Randomize
  PROCEDURE Randomize ;

  (*
     RandomInit - initialize the random number generator with value, seed.
  *)

  RandomInit
  PROCEDURE RandomInit (seed: CARDINAL) ;

  (*
     RandomBytes - fills in an array with random values.
  *)

  RandomBytes
  PROCEDURE RandomBytes (VAR a: ARRAY OF BYTE) ;

  (*
     RandomInt - return an INTEGER in the range 0..bound-1
  *)

  RandomInt
  PROCEDURE RandomInt (bound: INTEGER) : INTEGER ;

  (*
     RandomCard - return a CARDINAL in the range 0..bound-1
  *)

  RandomCard
  PROCEDURE RandomCard (bound: CARDINAL) : CARDINAL ;

  (*
     RandomReal - return a REAL number in the range 0.0..1.0
  *)

  RandomReal
  PROCEDURE RandomReal () : REAL ;

  (*
     RandomLongReal - return a LONGREAL number in the range 0.0..1.0
  *)

  RandomLongReal
  PROCEDURE RandomLongReal () : LONGREAL ;

  END Random.

