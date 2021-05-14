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

  overview
  patterns
  example
  rtl-template
  output-template
  output-statement
  predicates
  constraints
  standard-names
  pattern-ordering
  dependent-patterns
  jump-patterns
  looping-patterns
  insn-canonicalizations
  expander-definitions
  insn-splitting
  including-patterns
  peephole-definitions
  insn-attributes
  conditional-execution
  define-subst
  constant-definitions
  iterators
  overview-of-how-the-machine-description-is-used
  everything-about-instruction-patterns
  example-of-defineinsn
  output-templates-and-operand-substitution
  c-statements-for-assembler-output
  operand-constraints
  standard-pattern-names-for-generation
  when-the-order-of-patterns-matters
  interdependence-of-patterns
  defining-jump-instruction-patterns
  defining-looping-instruction-patterns
  canonicalization-of-instructions
  defining-rtl-sequences-for-code-generation
  defining-how-to-split-instructions
  including-patterns-in-machine-descriptions
  machine-specific-peephole-optimizers
  instruction-attributes
  rtl-templates-transformations

