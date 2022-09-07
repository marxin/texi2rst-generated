.. _gm2-libs-pim-display:

gm2-libs-pim/Display
^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Display ;

  EXPORT QUALIFIED Write ;

  (*
     Write - display a character to the stdout.
             ASCII.EOL moves to the beginning of the next line.
             ASCII.del erases the character to the left of the cursor.
  *)

  Write
  PROCEDURE Write (ch: CHAR) ;

  END Display.

