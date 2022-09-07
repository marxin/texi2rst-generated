.. _gm2-libs-pim-break:

gm2-libs-pim/Break
^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE Break ;

  EXPORT QUALIFIED EnableBreak, DisableBreak, InstallBreak, UnInstallBreak ;

  (*
     EnableBreak - enable the current break handler.
  *)

  EnableBreak
  PROCEDURE EnableBreak ;

  (*
     DisableBreak - disable the current break handler (and all
                    installed handlers).
  *)

  DisableBreak
  PROCEDURE DisableBreak ;

  (*
     InstallBreak - installs a procedure, p, to be invoked when
                    a ctrl-c is caught. Any number of these
                    procedures may be stacked. Only the top
                    procedure is run when ctrl-c is caught.
  *)

  InstallBreak
  PROCEDURE InstallBreak (p: PROC) ;

  (*
     UnInstallBreak - pops the break handler stack.
  *)

  UnInstallBreak
  PROCEDURE UnInstallBreak ;

  END Break.

