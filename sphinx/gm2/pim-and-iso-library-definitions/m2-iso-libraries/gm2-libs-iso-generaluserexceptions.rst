.. _gm2-libs-iso-generaluserexceptions:

gm2-libs-iso/GeneralUserExceptions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE GeneralUserExceptions;

  (* Provides facilities for general user-defined exceptions *)

  TYPE
  GeneralExceptions (type)
    GeneralExceptions = (problem, disaster);

  RaiseGeneralException
  PROCEDURE RaiseGeneralException (exception: GeneralExceptions;
                                   text: ARRAY OF CHAR);
    (* Raises exception using text as the associated message *)

  IsGeneralException
  PROCEDURE IsGeneralException (): BOOLEAN;
    (* Returns TRUE if the current coroutine is in the exceptional
       execution state because of the raising of an exception from
       GeneralExceptions; otherwise returns FALSE.
    *)

  GeneralException
  PROCEDURE GeneralException(): GeneralExceptions;
    (* If the current coroutine is in the exceptional execution
       state because of the raising of an exception from
       GeneralExceptions, returns the corresponding enumeration value,
       and otherwise raises an exception.
    *)

  END GeneralUserExceptions.

