.. _gm2-libs-cxxabi:

gm2-libs/cxxabi
^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE FOR "C" cxxabi ;

  (* This should only be used by the compiler and it matches the
      g++ implementation.  *)

  FROM SYSTEM IMPORT ADDRESS ;
  EXPORT UNQUALIFIED __cxa_begin_catch, __cxa_end_catch, __cxa_rethrow ;

  __cxa_begin_catch
  PROCEDURE __cxa_begin_catch (a: ADDRESS) : ADDRESS ;
  __cxa_end_catch
  PROCEDURE __cxa_end_catch ;
  __cxa_rethrow
  PROCEDURE __cxa_rethrow ;

  END cxxabi.

