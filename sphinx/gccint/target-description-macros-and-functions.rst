.. _target-macros:

Target Description Macros and Functions
---------------------------------------

.. index:: machine description macros

.. index:: target description macros

.. index:: macros, target description

.. index:: tm.h macros

In addition to the file :samp:`{machine}`.md, a machine description
includes a C header file conventionally given the name
:samp:`{machine}`.h and a C source file named :samp:`{machine}`.c.
The header file defines numerous macros that convey the information
about the target machine that does not fit into the scheme of the
.md file.  The file tm.h should be a link to
:samp:`{machine}`.h.  The header file config.h includes
tm.h and most compiler source files include config.h.  The
source file defines a variable ``targetm``, which is a structure
containing pointers to functions and data relating to the target
machine.  :samp:`{machine}`.c should also contain their definitions,
if they are not defined elsewhere in GCC, and other functions called
through the macros defined in the .h file.

.. toctree::

  target-structure
  driver
  run-time-target
  per-function-data
  storage-layout
  type-layout
  registers
  register-classes
  stack-and-calling
  varargs
  trampolines
  library-calls
  addressing-modes
  anchored-addresses
  condition-code
  costs
  scheduling
  sections
  pic
  assembler-format
  debugging-info
  floating-point
  mode-switching
  target-attributes
  emulated-tls
  mips-coprocessors
  pch-target
  c++-abi
  d-language-and-abi
  named-address-spaces
  misc
  the-global-targetm-variable
  controlling-the-compilation-driver-gcc
  run-time-target-specification
  defining-data-structures-for-per-function-information
  layout-of-source-language-data-types
  register-usage
  stack-layout-and-calling-conventions
  implementing-the-varargs-macros
  support-for-nested-functions
  implicit-calls-to-library-routines
  condition-code-status
  describing-relative-costs-of-operations
  adjusting-the-instruction-scheduler
  dividing-the-output-into-sections-texts-data-
  position-independent-code
  defining-the-output-assembler-language
  controlling-debugging-information-format
  cross-compilation-and-floating-point
  mode-switching-instructions
  defining-target-specific-uses-of-attribute
  emulating-tls
  defining-coprocessor-specifics-for-mips-targets
  parameters-for-precompiled-header-validity-checking
  c++-abi-parameters
  d-abi-parameters
  adding-support-for-named-address-spaces
  miscellaneous-parameters

