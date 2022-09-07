.. _assembly-language:

Interface to assembly language
******************************

The interface for GNU Modula-2 to assembly language is almost
identical to GNU C.  The only alterations are that the keywords
``asm`` and ``volatile`` are in capitals, following the Modula-2
convention.

A simple, but highly non optimal, example is given below. Here we want
to add the two ``CARDINAL`` s ``foo`` and ``bar`` together and
return the result.  The target processor is assumed to be executing
the x86_64 instruction set.

.. code-block:: modula2

  PROCEDURE Example (foo, bar: CARDINAL) : CARDINAL ;
  VAR
     myout: CARDINAL ;
  BEGIN
     ASM VOLATILE ("movq %1,%%rax; addq %2,%%rax; movq %%rax,%0"
        : "=rm" (myout)            (* outputs *)
        : "rm" (foo), "rm" (bar)   (* inputs  *)
        : "rax") ;                 (* we trash *)
     RETURN( myout )
  END Example ;

For a full description of this interface we refer the reader to the GNU C manual.

See :ref:`gcc:extended-asm`.

The same example can be written using the newer extensions of naming
the operands rather than using numbered arguments.

.. code-block:: modula2

  PROCEDURE Example (foo, bar: CARDINAL) : CARDINAL ;
  VAR
     myout: CARDINAL ;
  BEGIN
     ASM VOLATILE (
      "movq %[left],%%rax; addq %[right],%%rax; movq %%rax,%[output]"
        : [output] "=rm" (myout)                  (* outputs *)
        : [left] "rm" (foo), [right] "rm" (bar)   (* inputs  *)
        : "rax") ;                                (* we trash *)
     RETURN( myout )
  END Example ;

Both examples generate exactly the same code.  It is worth noting that
the specifier 'rm' indicates that the operand can be either a
register or memory.  Of course you must choose an instruction which
can take either, but this allows the compiler to take make more
efficient choices depending upon the optimization level given to the
compiler.

