..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _machine-desc:

Machine Descriptions
--------------------

.. index:: machine descriptions

A machine description has two parts: a file of instruction patterns
(.md file) and a C header file of macro definitions.

The .md file for a target machine contains a pattern for each
instruction that the target machine supports (or at least each instruction
that is worth telling the compiler about).  It may also contain comments.
A semicolon causes the rest of the line to be a comment, unless the semicolon
is inside a quoted string.

See the next chapter for information on the C header file.

.. toctree::
  :maxdepth: 2

  machine-descriptions/patterns
  machine-descriptions/example
  machine-descriptions/rtl-template
  machine-descriptions/output-template
  machine-descriptions/output-statement
  machine-descriptions/predicates
  machine-descriptions/constraints
  machine-descriptions/standard-names
  machine-descriptions/pattern-ordering
  machine-descriptions/dependent-patterns
  machine-descriptions/jump-patterns
  machine-descriptions/looping-patterns
  machine-descriptions/insn-canonicalizations
  machine-descriptions/expander-definitions
  machine-descriptions/insn-splitting
  machine-descriptions/including-patterns
  machine-descriptions/peephole-definitions
  machine-descriptions/insn-attributes
  machine-descriptions/conditional-execution
  machine-descriptions/define-subst
  machine-descriptions/constant-definitions
  machine-descriptions/iterators
  machine-descriptions/overview-of-how-the-machine-description-is-used
  machine-descriptions/everything-about-instruction-patterns
  machine-descriptions/example-of-defineinsn
  machine-descriptions/output-templates-and-operand-substitution
  machine-descriptions/c-statements-for-assembler-output
  machine-descriptions/operand-constraints
  machine-descriptions/standard-pattern-names-for-generation
  machine-descriptions/when-the-order-of-patterns-matters
  machine-descriptions/interdependence-of-patterns
  machine-descriptions/defining-jump-instruction-patterns
  machine-descriptions/defining-looping-instruction-patterns
  machine-descriptions/canonicalization-of-instructions
  machine-descriptions/defining-rtl-sequences-for-code-generation
  machine-descriptions/defining-how-to-split-instructions
  machine-descriptions/including-patterns-in-machine-descriptions
  machine-descriptions/machine-specific-peephole-optimizers
  machine-descriptions/instruction-attributes
  machine-descriptions/rtl-templates-transformations

