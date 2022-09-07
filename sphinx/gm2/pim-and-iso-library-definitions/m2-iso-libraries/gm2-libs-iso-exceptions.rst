.. _gm2-libs-iso-exceptions:

gm2-libs-iso/EXCEPTIONS
^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE EXCEPTIONS;

  (* Provides facilities for raising user exceptions
     and for making enquiries concerning the current execution state.
  *)

  TYPE
    ExceptionSource;   (* values of this type are used within library
                          modules to identify the source of raised
                          exceptions *)
  ExceptionNumber (type)
    ExceptionNumber = CARDINAL;

  AllocateSource
  PROCEDURE AllocateSource(VAR newSource: ExceptionSource);
    (* Allocates a unique value of type ExceptionSource *)

  RAISE
  PROCEDURE RAISE (source: ExceptionSource;
                   number: ExceptionNumber; message: ARRAY OF CHAR);
    (* Associates the given values of source, number and message with
       the current context and raises an exception.
    *)

  CurrentNumber
  PROCEDURE CurrentNumber (source: ExceptionSource): ExceptionNumber;
    (* If the current coroutine is in the exceptional execution state
       because of the raising of an exception from source, returns
       the corresponding number, and otherwise raises an exception.
    *)

  GetMessage
  PROCEDURE GetMessage (VAR text: ARRAY OF CHAR);
    (* If the current coroutine is in the exceptional execution state,
       returns the possibly truncated string associated with the
       current context.  Otherwise, in normal execution state,
       returns the empty string.
    *)

  IsCurrentSource
  PROCEDURE IsCurrentSource (source: ExceptionSource): BOOLEAN;
    (* If the current coroutine is in the exceptional execution state
       because of the raising of an exception from source, returns
       TRUE, and otherwise returns FALSE.
    *)

  IsExceptionalExecution
  PROCEDURE IsExceptionalExecution (): BOOLEAN;
    (* If the current coroutine is in the exceptional execution state
       because of the raising of an exception, returns TRUE, and
       otherwise returns FALSE.
    *)

  END EXCEPTIONS.

