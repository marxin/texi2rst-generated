.. _gm2-libs-iso-preemptive:

gm2-libs-iso/Preemptive
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Preemptive ;

  (*
     initPreemptive - if microsecs > 0 then turn on preemptive scheduling.
                      if microsecs = 0 then preemptive scheduling is turned off.
  *)

  initPreemptive
  PROCEDURE initPreemptive (seconds, microsecs: CARDINAL) ;

  END Preemptive.

