.. _gm2-libs-pim-keyboard:

gm2-libs-pim/Keyboard
^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Keyboard ;

  EXPORT QUALIFIED Read, KeyPressed ;

  (*
     Read - reads a character from StdIn. If necessary it will wait
            for a key to become present on StdIn.
  *)

  Read
  PROCEDURE Read (VAR ch: CHAR) ;

  (*
     KeyPressed - returns TRUE if a character can be read from StdIn
                  without blocking the caller.
  *)

  KeyPressed
  PROCEDURE KeyPressed () : BOOLEAN ;

  END Keyboard.

