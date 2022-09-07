.. _gm2-libs-iso-programargs:

gm2-libs-iso/ProgramArgs
^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ProgramArgs;

    (* Access to program arguments *)

  IMPORT IOChan;

  TYPE
  ChanId (type)
    ChanId = IOChan.ChanId;

  ArgChan
  PROCEDURE ArgChan (): ChanId;
    (* Returns a value that identifies a channel for reading
       program arguments *)

  IsArgPresent
  PROCEDURE IsArgPresent (): BOOLEAN;
    (* Tests if there is a current argument to read from.  If not,
       read <= IOChan.CurrentFlags() will be FALSE, and attempting
       to read from the argument channel will raise the exception
       notAvailable.
    *)

  NextArg
  PROCEDURE NextArg ();
    (* If there is another argument, causes subsequent input from the
       argument device to come from the start of the next argument.
       Otherwise there is no argument to read from, and a call of
       IsArgPresent will return FALSE.
    *)

  END ProgramArgs.

