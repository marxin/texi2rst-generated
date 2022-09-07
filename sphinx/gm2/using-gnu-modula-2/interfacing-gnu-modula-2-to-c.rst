.. _interface-to-c:

Interfacing GNU Modula-2 to C
*****************************

The GNU Modula-2 compiler tries to use the C calling convention
wherever possible however some parameters have no C equivalent and
thus a language specific method is used.  For example unbounded arrays
are passed as a ``struct {void *address, unsigned int high}`` and
the contents of these arrays are copied by callee functions when they
are declared as non ``VAR`` parameters.  The ``VAR`` equivalent
unbounded array parameters need no copy, but still use the
``struct`` representation.

The recommended method of interfacing GNU Modula-2 to C is by telling
the definition module that the implementation is in the C language.
This is achieved by using the tokens ``DEFINITION MODULE FOR "C"``.
Here is an example :samp:`libprintf.def`.

.. code-block:: modula2

  DEFINITION MODULE FOR "C" libprintf ;

  EXPORT UNQUALIFIED printf ;

  PROCEDURE printf (a: ARRAY OF CHAR; ...) : [ INTEGER ] ;

  END libprintf.

the ``UNQUALIFIED`` keyword in the definition module informs
GNU Modula-2 not to prefix the module name to exported references
in the object file.

The ``printf`` declaration states that the first parameter
semantically matches ``ARRAY OF CHAR`` but since the module is for
the C language it will be mapped onto ``char *``. The token
``...`` indicates a variable number of arguments (varargs) and all
parameters passed here are mapped onto their C equivalents. Arrays and
constant strings are passed as pointers.  Lastly ``[ INTEGER ]``
states that the caller can ignore the function return result if desired.

The hello world program can be rewritten as:

.. code-block:: modula2

  MODULE hello ;

  FROM libprintf IMPORT printf ;

  BEGIN
     printf("hello world\n")
  END hello.

and it can be compiled by:

:samp:`gm2 -g -I. hello.mod -lc`

In reality the :samp:`-lc` is redundant as libc is always included in the
linking process. It is shown here to emphasize that the C library or
object file containing ``printf`` must be present.

If a procedure function is declared using varargs then some parameter
values are converted.  The table below summarises the default conversions
and default types used.

.. code-block:: modula2

  Actual Parameter       |  Default conversion  |   Type of actual
                         |                      |   value passed
  ===============================================================
  123                    |  none                |   long long int
  "hello world"          |  none                |   const char *
  a: ARRAY OF CHAR       |  ADR(a)              |   char *
  a: ARRAY [0..5] OF CHAR|  ADR(a)              |   char *
  3.14                   |  none                |   long double

If you wish to pass ``int`` values then you should explicitly
convert the constants using one of the conversion mechanisms.
For example:  ``INTEGER(10)`` or ``VAL(INTEGER, 10)`` or
``CAST(INTEGER, 10)``.

