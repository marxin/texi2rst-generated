.. _gm2-libs-coroutines:

gm2-libs/COROUTINES
^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FOR "C" COROUTINES ;

  CONST
     UnassignedPriority = 0 ;

  TYPE
  INTERRUPTSOURCE (type)
     INTERRUPTSOURCE = CARDINAL ;
  PROTECTION (type)
     PROTECTION = [UnassignedPriority..7] ;

  END COROUTINES.

