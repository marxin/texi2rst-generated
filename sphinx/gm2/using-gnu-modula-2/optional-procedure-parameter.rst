Optional procedure parameter
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNU Modula-2 allows the last parameter to a procedure or function
parameter to be optional. For example in the ISO library
:samp:`COROUTINES.def` the procedure ``NEWCOROUTINE`` is defined as
having an optional fifth argument (``initProtection``) which, if
absent, is automatically replaced by ``NIL``.

.. code-block:: modula2

  NEWCOROUTINE
  PROCEDURE NEWCOROUTINE (procBody: PROC; workspace: SYSTEM.ADDRESS;
                          size: CARDINAL; VAR cr: COROUTINE;
                          [initProtection: PROTECTION = NIL]);

    (* Creates a new coroutine whose body is given by procBody,
       and returns the identity of the coroutine in cr.
       workspace is a pointer to the work space allocated to
       the coroutine; size specifies the size of this workspace
       in terms of SYSTEM.LOC.

       The optional fifth argument may contain a single parameter
       which specifies the initial protection level of the coroutine.
    *)

The implementation module :samp:`COROUTINES.mod` implements this
procedure using the following syntax:

.. code-block:: modula2

  PROCEDURE NEWCOROUTINE (procBody: PROC; workspace: SYSTEM.ADDRESS;
                          size: CARDINAL; VAR cr: COROUTINE;
                          [initProtection: PROTECTION]);
  BEGIN

  END NEWCOROUTINE ;

Note that it is illegal for this declaration to contain an initialiser
value for ``initProtection``. However it is necessary to surround
this parameter with the brackets ``[`` and ``]``. This serves to
remind the programmer that the last parameter was declared as optional
in the definition module.

Local procedures can be declared to have an optional final parameter
in which case the initializer is mandatory in the implementation or
program module.

GNU Modula-2 also provides additional fixed sized data types which
are all exported from the ``SYSTEM`` module.
See :ref:`gm2:the-pim-system-module`.
See :ref:`gm2:the-iso-system-module`.

