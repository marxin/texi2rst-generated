.. _semantic-checking:

How to detect runtime problems at compile time
**********************************************

Consider the following program:

.. code-block:: modula2

  MODULE assignvalue ;  (*!m2iso+gm2*)

  PROCEDURE bad () : INTEGER ;
  VAR
     i: INTEGER ;
  BEGIN
     i := -1 ;
     RETURN i
  END bad ;

  VAR
     foo: CARDINAL ;
  BEGIN
     (* the m2rte plugin will detect this as an error, post
        optimization.  *)
     foo := bad ()
  END assignvalue.

here we see that the programmer has overlooked that the return value
from :samp:`bad` will cause an overflow to :samp:`foo`.  If we compile
the code with the following options:

.. code-block:: modula2

  $ gm2 -g -fsoft-check-all -O2 -c assignvalue.mod
  assignvalue.mod:16:0:inevitable that this error will occur at runtime,
  assignment will result in an overflow

The gm2 semantic plugin is automatically run and will generate a
warning message for every exception call which is known as reachable.
It is highly advised to run the optimizer (:samp:`-O2` or :samp:`-O3`)
with :samp:`-fsoft-check-all` so that the compiler is able to run the
optimizer and perform variable and flow analysis before the semantic
plugin is invoked.

