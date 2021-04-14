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

  Expressions vs vectors vs strings vs integers. <rtl-objects>
  Categories of RTL expression objects, and their structure. <rtl-classes>
  Macros to access expression operands or vector elts. <accessors>
  Macros to access specific annotations on RTL. <special-accessors>
  Other flags in an RTL expression. <flags>
  Describing the size and format of a datum. <machine-modes>
  Expressions with constant values. <constants>
  Expressions representing register contents or memory. <regs-and-memory>
  Expressions representing arithmetic on other expressions. <arithmetic>
  Expressions representing comparison of expressions. <comparisons>
  Expressions representing bit-fields in memory or reg. <bit-fields>
  Expressions involving vector datatypes. <vector-operations>
  Extending, truncating, floating or fixing. <conversions>
  Declaring volatility, constancy, etc. <rtl-declarations>
  Expressions for storing in registers, etc. <side-effects>
  Embedded side-effects for autoincrement addressing. <incdec>
  Representing ``asm`` with operands. <assembler>
  Expressions representing debugging information. <debug-information>
  Expression types for entire insns. <insns>
  RTL representation of function call insns. <calls>
  An on-the-side SSA form for RTL <rtl-ssa>
  Some expressions are unique; others *must* be copied. <sharing>
  Reading textual RTL from a file. <reading-rtl>

.. toctree::

  rtl-object-types
  rtl-classes-and-formats
  access-to-operands
  access-to-special-operands
  flags-in-an-rtl-expression
  machine-modes
  constant-expression-types
  registers-and-memory
  rtl-expressions-for-arithmetic
  comparison-operations
  bit-fields
  vector-operations
  conversions
  declarations
  side-effect-expressions
  embedded-side-effects-on-addresses
  assembler-instructions-as-expressions
  variable-location-debug-information-in-rtl
  insns
  rtl-representation-of-function-call-insns
  on-the-side-ssa-form-for-rtl
  structure-sharing-assumptions
  reading-rtl

