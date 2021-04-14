.. _jump-patterns:

Defining Jump Instruction Patterns
**********************************

.. index:: jump instruction patterns

.. index:: defining jump instruction patterns

GCC does not assume anything about how the machine realizes jumps.
The machine description should define a single pattern, usually
a ``define_expand``, which expands to all the required insns.

Usually, this would be a comparison insn to set the condition code
and a separate branch insn testing the condition code and branching
or not according to its value.  For many machines, however,
separating compares and branches is limiting, which is why the
more flexible approach with one ``define_expand`` is used in GCC.
The machine description becomes clearer for architectures that
have compare-and-branch instructions but no condition code.  It also
works better when different sets of comparison operators are supported
by different kinds of conditional branches (e.g. integer vs.
floating-point), or by conditional branches with respect to conditional stores.

Two separate insns are always used if the machine description represents
a condition code register using the legacy RTL expression ``(cc0)``,
and on most machines that use a separate condition code register
(see :ref:`condition-code`).  For machines that use ``(cc0)``, in
fact, the set and use of the condition code must be separate and
adjacent``note`` insns can separate them, though.

, thus
allowing flags in ``cc_status`` to be used (see :ref:`condition-code`) and
so that the comparison and branch insns could be located from each other
by using the functions ``prev_cc0_setter`` and ``next_cc0_user``.

Even in this case having a single entry point for conditional branches
is advantageous, because it handles equally well the case where a single
comparison instruction records the results of both signed and unsigned
comparison of the given operands (with the branch insns coming in distinct
signed and unsigned flavors) as in the x86 or SPARC, and the case where
there are distinct signed and unsigned compare instructions and only
one set of conditional branch instructions as in the PowerPC.

