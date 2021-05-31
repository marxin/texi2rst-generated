.. _c-extensions:

Extensions to the C Language Family
-----------------------------------

.. index:: extensions, C language

.. index:: C language extensions

.. index:: pedantic

GNU C provides several language features not found in ISO standard C.
(The :option:`-pedantic` option directs GCC to print a warning message if
any of these features is used.)  To test for the availability of these
features in conditional compilation, check for a predefined macro
``__GNUC__``, which is always defined under GCC.

These extensions are available in C and Objective-C.  Most of them are
also available in C++.  See :ref:`Extensions to the
C++ Language <c++-extensions>`, for extensions that apply *only* to C++.

Some features that are in ISO C99 but not C90 or C++ are also, as
extensions, accepted by GCC in C90 mode and in C++.

.. toctree::
  :maxdepth: 2

  statement-exprs
  local-labels
  labels-as-values
  nested-functions
  nonlocal-gotos
  constructing-calls
  typeof
  conditionals
  __int128
  long-long
  complex
  floating-types
  half-precision
  decimal-float
  hex-floats
  fixed-point
  named-address-spaces
  zero-length
  empty-structures
  variable-length
  variadic-macros
  escaped-newlines
  subscripting
  pointer-arith
  variadic-pointer-args
  pointers-to-arrays
  initializers
  compound-literals
  designated-inits
  case-ranges
  cast-to-union
  mixed-labels-and-declarations
  function-attributes
  variable-attributes
  type-attributes
  label-attributes
  enumerator-attributes
  statement-attributes
  attribute-syntax
  function-prototypes
  c++-comments
  dollar-signs
  character-escapes
  alignment
  inline
  volatiles
  using-assembly-language-with-c
  alternate-keywords
  incomplete-enums
  function-names
  return-address
  vector-extensions
  offsetof
  __sync-builtins
  __atomic-builtins
  integer-overflow-builtins
  x86-specific-memory-model-extensions-for-transactional-memory
  object-size-checking
  other-builtins
  target-builtins
  target-format-checks
  pragmas
  unnamed-fields
  thread-local
  binary-constants
  statements-and-declarations-in-expressions
  locally-declared-labels
  constructing-function-calls
  referring-to-a-type-with-typeof
  conditionals-with-omitted-operands
  128-bit-integers
  double-word-integers
  complex-numbers
  additional-floating-types
  half-precision-floating-point
  decimal-floating-types
  fixed-point-types
  arrays-of-length-zero
  structures-with-no-members
  arrays-of-variable-length
  macros-with-a-variable-number-of-arguments
  slightly-looser-rules-for-escaped-newlines
  non-lvalue-arrays-may-have-subscripts
  arithmetic-on-void-and-function-pointers
  pointer-arguments-in-variadic-functions
  pointers-to-arrays-with-qualifiers-work-as-expected
  non-constant-initializers
  designated-initializers
  cast-to-a-union-type
  mixed-declarations-labels-and-code
  declaring-attributes-of-functions
  specifying-attributes-of-variables
  specifying-attributes-of-types
  prototypes-and-old-style-function-definitions
  c++-style-comments
  dollar-signs-in-identifier-names
  the-character-esc-in-constants
  determining-the-alignment-of-functions-types-or-variables
  an-inline-function-is-as-fast-as-a-macro
  when-is-a-volatile-object-accessed
  how-to-use-inline-assembly-language-in-c-code
  incomplete-enum-types
  function-names-as-strings
  getting-the-return-or-frame-address-of-a-function
  using-vector-instructions-through-built-in-functions
  support-for-offsetof
  legacy-sync-built-in-functions-for-atomic-memory-access
  built-in-functions-for-memory-model-aware-atomic-operations
  built-in-functions-to-perform-arithmetic-with-overflow-checking
  object-size-checking-built-in-functions
  other-built-in-functions-provided-by-gcc
  built-in-functions-specific-to-particular-target-machines
  format-checks-specific-to-particular-target-machines
  pragmas-accepted-by-gcc
  unnamed-structure-and-union-fields
  thread-local-storage
  binary-constants-using-the-0b-prefix

