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

  How the machine description is used. <overview>
  How to write instruction patterns. <patterns>
  An explained example of a ``define_insn`` pattern. <example>
  The RTL template defines what insns match a pattern. <rtl-template>
  The output template says how to make assembler code
                          from such an insn. <output-template>
  For more generality, write C code to output
                          the assembler code. <output-statement>
  Controlling what kinds of operands can be used
                          for an insn. <predicates>
  Fine-tuning operand selection. <constraints>
  Names mark patterns to use for code generation. <standard-names>
  When the order of patterns makes a difference. <pattern-ordering>
  Having one pattern may make you need another. <dependent-patterns>
  Special considerations for patterns for jump insns. <jump-patterns>
  How to define patterns for special looping insns. <looping-patterns>
  Canonicalization of Instructions <insn-canonicalizations>
  Generating a sequence of several RTL insns
                          for a standard operation. <expander-definitions>
  Splitting Instructions into Multiple Instructions. <insn-splitting>
  Including Patterns in Machine Descriptions. <including-patterns>
  Defining machine-specific peephole optimizations. <peephole-definitions>
  Specifying the value of attributes for generated insns. <insn-attributes>
  Generating ``define_insn`` patterns for
                           predication. <conditional-execution>
  Generating ``define_insn`` and ``define_expand``
  			patterns from other patterns. <define-subst>
  Defining symbolic constants that can be used in the
                          md file. <constant-definitions>
  Using iterators to generate patterns from a template. <iterators>

.. toctree::

  overview-of-how-the-machine-description-is-used
  everything-about-instruction-patterns
  example-of-defineinsn
  rtl-template
  output-templates-and-operand-substitution
  c-statements-for-assembler-output
  predicates
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
  conditional-execution
  rtl-templates-transformations
  constant-definitions
  iterators

