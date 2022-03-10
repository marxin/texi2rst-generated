..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _rtl:

.. index:: RTL representation

.. index:: representation of RTL

.. index:: Register Transfer Language (RTL)

RTL Representation
------------------

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

  rtl-representation/rtl-objects
  rtl-representation/rtl-classes
  rtl-representation/accessors
  rtl-representation/special-accessors
  rtl-representation/flags
  rtl-representation/machine-modes
  rtl-representation/constants
  rtl-representation/regs-and-memory
  rtl-representation/arithmetic
  rtl-representation/comparisons
  rtl-representation/bit-fields
  rtl-representation/vector-operations
  rtl-representation/conversions
  rtl-representation/rtl-declarations
  rtl-representation/side-effects
  rtl-representation/incdec
  rtl-representation/assembler
  rtl-representation/debug-information
  rtl-representation/insns
  rtl-representation/calls
  rtl-representation/rtl-ssa
  rtl-representation/sharing
  rtl-representation/reading-rtl
  rtl-representation/rtl-object-types
  rtl-representation/rtl-classes-and-formats
  rtl-representation/access-to-operands
  rtl-representation/access-to-special-operands
  rtl-representation/flags-in-an-rtl-expression
  rtl-representation/constant-expression-types
  rtl-representation/registers-and-memory
  rtl-representation/rtl-expressions-for-arithmetic
  rtl-representation/comparison-operations
  rtl-representation/side-effect-expressions
  rtl-representation/embedded-side-effects-on-addresses
  rtl-representation/assembler-instructions-as-expressions
  rtl-representation/variable-location-debug-information-in-rtl
  rtl-representation/rtl-representation-of-function-call-insns
  rtl-representation/on-the-side-ssa-form-for-rtl
  rtl-representation/structure-sharing-assumptions

