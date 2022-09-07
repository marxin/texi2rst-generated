.. _gm2-libs-assertion:

gm2-libs/Assertion
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Assertion ;

  EXPORT QUALIFIED Assert ;

  (*
     Assert - tests the boolean Condition, if it fails then HALT
              is called.
  *)

  Assert
  PROCEDURE Assert (Condition: BOOLEAN) ;

  END Assertion.

