.. _gm2-libs-pim-delay:

gm2-libs-pim/Delay
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Delay ;

  EXPORT QUALIFIED Delay ;

  (*
     milliSec - delays the program by approximately, milliSec, milliseconds.
  *)

  Delay
  PROCEDURE Delay (milliSec: INTEGER) ;

  END Delay.

