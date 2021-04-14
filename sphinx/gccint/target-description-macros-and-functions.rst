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

  The ``targetm`` variable. <target-structure>
  Controlling how the driver runs the compilation passes. <driver>
  Defining :samp:`-m` options like :option:`-m68000` and :option:`-m68020`. <run-time-target>
  Defining data structures for per-function information. <per-function-data>
  Defining sizes and alignments of data. <storage-layout>
  Defining sizes and properties of basic user data types. <type-layout>
  Naming and describing the hardware registers. <registers>
  Defining the classes of hardware registers. <register-classes>
  Defining which way the stack grows and by how much. <stack-and-calling>
  Defining the varargs macros. <varargs>
  Code set up at run time to enter a nested function. <trampolines>
  Controlling how library routines are implicitly called. <library-calls>
  Defining addressing modes valid for memory operands. <addressing-modes>
  Defining how :option:`-fsection-anchors` should work. <anchored-addresses>
  Defining how insns update the condition code. <condition-code>
  Defining relative costs of different operations. <costs>
  Adjusting the behavior of the instruction scheduler. <scheduling>
  Dividing storage into text, data, and other sections. <sections>
  Macros for position independent code. <pic>
  Defining how to write insns and pseudo-ops to output. <assembler-format>
  Defining the format of debugging output. <debugging-info>
  Handling floating point for cross-compilers. <floating-point>
  Insertion of mode-switching instructions. <mode-switching>
  Defining target-specific uses of ``__attribute__``. <target-attributes>
  Emulated TLS support. <emulated-tls>
  MIPS coprocessor support and how to customize it. <mips-coprocessors>
  Validity checking for precompiled headers. <pch-target>
  Controlling C++ ABI changes. <c++-abi>
  Controlling D ABI changes. <d-language-and-abi>
  Adding support for named address spaces <named-address-spaces>
  Everything else. <misc>

.. toctree::

  the-global-targetm-variable
  controlling-the-compilation-driver-gcc
  run-time-target-specification
  defining-data-structures-for-per-function-information
  storage-layout
  layout-of-source-language-data-types
  register-usage
  register-classes
  stack-layout-and-calling-conventions
  implementing-the-varargs-macros
  support-for-nested-functions
  implicit-calls-to-library-routines
  addressing-modes
  anchored-addresses
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

