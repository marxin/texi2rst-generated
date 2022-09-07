.. _gm2-libs-iso-randomnumber:

gm2-libs-iso/RandomNumber
^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RandomNumber ;

  (*
      Description: provides primitives for obtaining random numbers on
                   pervasive data types.
  *)

  FROM SYSTEM IMPORT BYTE ;
  EXPORT QUALIFIED Randomize, RandomInit, RandomBytes,
                   RandomCard, RandomShortCard, RandomLongCard,
                   RandomInt, RandomShortInt, RandomLongInt,
                   RandomReal, RandomLongReal, RandomShortReal ;

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
     RandomInt - return an INTEGER in the range [low .. high].
  *)

  RandomInt
  PROCEDURE RandomInt (low, high: INTEGER) : INTEGER ;

  (*
     RandomShortInt - return an SHORTINT in the range [low..high].
  *)

  RandomShortInt
  PROCEDURE RandomShortInt (low, high: SHORTINT) : SHORTINT ;

  (*
     RandomLongInt - return an LONGINT in the range [low..high].
  *)

  RandomLongInt
  PROCEDURE RandomLongInt (low, high: LONGINT) : LONGINT ;

  (*
     RandomShortCard - return a SHORTCARD in the range [low..high].
  *)

  RandomShortCard
  PROCEDURE RandomShortCard (low, high: CARDINAL) : CARDINAL ;

  (*
     RandomCard - return a CARDINAL in the range [low..high].
  *)

  RandomCard
  PROCEDURE RandomCard (low, high: CARDINAL) : CARDINAL ;

  (*
     RandomLongCard - return an LONGCARD in the range [low..high].
  *)

  RandomLongCard
  PROCEDURE RandomLongCard (low, high: LONGCARD) : LONGCARD ;

  (*
     RandomReal - return a REAL number in the range 0.0..1.0
  *)

  RandomReal
  PROCEDURE RandomReal () : REAL ;

  (*
     RandomShortReal - return a SHORTREAL number in the range 0.0..1.0
  *)

  RandomShortReal
  PROCEDURE RandomShortReal () : SHORTREAL ;

  (*
     RandomLongReal - return a LONGREAL number in the range 0.0..1.0
  *)

  RandomLongReal
  PROCEDURE RandomLongReal () : LONGREAL ;

  END RandomNumber.

