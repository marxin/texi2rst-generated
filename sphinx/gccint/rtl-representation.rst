.. _rtl:

RTL Representation
------------------

.. index:: RTL representation

.. index:: representation of RTL

.. index:: Register Transfer Language (RTL)

The last part of the compiler work is done on a low-level intermediate
representation called Register Transfer Language.  In this language, the
instructions to be output are described, pretty much one by one, in an
algebraic form that describes what the instruction does.

RTL is inspired by Lisp lists.  It has both an internal form, made up of
structures that point at other structures, and a textual form that is used
in the machine description and in printed debugging dumps.  The textual
form uses nested parentheses to indicate the pointers in the internal form.

.. toctree::
  :maxdepth: 2

  rtl-objects
  rtl-classes
  accessors
  special-accessors
  flags
  machine-modes
  constants
  regs-and-memory
  arithmetic
  comparisons
  bit-fields
  vector-operations
  conversions
  rtl-declarations
  side-effects
  incdec
  assembler
  debug-information
  insns
  calls
  rtl-ssa
  sharing
  reading-rtl
  rtl-object-types
  rtl-classes-and-formats
  access-to-operands
  access-to-special-operands
  flags-in-an-rtl-expression
  constant-expression-types
  registers-and-memory
  rtl-expressions-for-arithmetic
  comparison-operations
  side-effect-expressions
  embedded-side-effects-on-addresses
  assembler-instructions-as-expressions
  variable-location-debug-information-in-rtl
  rtl-representation-of-function-call-insns
  on-the-side-ssa-form-for-rtl
  structure-sharing-assumptions

