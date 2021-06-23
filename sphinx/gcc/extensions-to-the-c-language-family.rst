..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

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

  extensions-to-the-c-language-family/statement-exprs
  extensions-to-the-c-language-family/local-labels
  extensions-to-the-c-language-family/labels-as-values
  extensions-to-the-c-language-family/nested-functions
  extensions-to-the-c-language-family/nonlocal-gotos
  extensions-to-the-c-language-family/constructing-calls
  extensions-to-the-c-language-family/typeof
  extensions-to-the-c-language-family/conditionals
  extensions-to-the-c-language-family/int128
  extensions-to-the-c-language-family/long-long
  extensions-to-the-c-language-family/complex
  extensions-to-the-c-language-family/floating-types
  extensions-to-the-c-language-family/half-precision
  extensions-to-the-c-language-family/decimal-float
  extensions-to-the-c-language-family/hex-floats
  extensions-to-the-c-language-family/fixed-point
  extensions-to-the-c-language-family/named-address-spaces
  extensions-to-the-c-language-family/zero-length
  extensions-to-the-c-language-family/empty-structures
  extensions-to-the-c-language-family/variable-length
  extensions-to-the-c-language-family/variadic-macros
  extensions-to-the-c-language-family/escaped-newlines
  extensions-to-the-c-language-family/subscripting
  extensions-to-the-c-language-family/pointer-arith
  extensions-to-the-c-language-family/variadic-pointer-args
  extensions-to-the-c-language-family/pointers-to-arrays
  extensions-to-the-c-language-family/initializers
  extensions-to-the-c-language-family/compound-literals
  extensions-to-the-c-language-family/designated-inits
  extensions-to-the-c-language-family/case-ranges
  extensions-to-the-c-language-family/cast-to-union
  extensions-to-the-c-language-family/mixed-labels-and-declarations
  extensions-to-the-c-language-family/function-attributes
  extensions-to-the-c-language-family/variable-attributes
  extensions-to-the-c-language-family/type-attributes
  extensions-to-the-c-language-family/label-attributes
  extensions-to-the-c-language-family/enumerator-attributes
  extensions-to-the-c-language-family/statement-attributes
  extensions-to-the-c-language-family/attribute-syntax
  extensions-to-the-c-language-family/function-prototypes
  extensions-to-the-c-language-family/c++-comments
  extensions-to-the-c-language-family/dollar-signs
  extensions-to-the-c-language-family/character-escapes
  extensions-to-the-c-language-family/alignment
  extensions-to-the-c-language-family/inline
  extensions-to-the-c-language-family/volatiles
  extensions-to-the-c-language-family/using-assembly-language-with-c
  extensions-to-the-c-language-family/alternate-keywords
  extensions-to-the-c-language-family/incomplete-enums
  extensions-to-the-c-language-family/function-names
  extensions-to-the-c-language-family/return-address
  extensions-to-the-c-language-family/vector-extensions
  extensions-to-the-c-language-family/offsetof
  extensions-to-the-c-language-family/sync-builtins
  extensions-to-the-c-language-family/atomic-builtins
  extensions-to-the-c-language-family/integer-overflow-builtins
  extensions-to-the-c-language-family/x86-specific-memory-model-extensions-for-transactional-memory
  extensions-to-the-c-language-family/object-size-checking
  extensions-to-the-c-language-family/other-builtins
  extensions-to-the-c-language-family/target-builtins
  extensions-to-the-c-language-family/target-format-checks
  extensions-to-the-c-language-family/pragmas
  extensions-to-the-c-language-family/unnamed-fields
  extensions-to-the-c-language-family/thread-local
  extensions-to-the-c-language-family/binary-constants
  extensions-to-the-c-language-family/statements-and-declarations-in-expressions
  extensions-to-the-c-language-family/locally-declared-labels
  extensions-to-the-c-language-family/constructing-function-calls
  extensions-to-the-c-language-family/referring-to-a-type-with-typeof
  extensions-to-the-c-language-family/conditionals-with-omitted-operands
  extensions-to-the-c-language-family/128-bit-integers
  extensions-to-the-c-language-family/double-word-integers
  extensions-to-the-c-language-family/complex-numbers
  extensions-to-the-c-language-family/additional-floating-types
  extensions-to-the-c-language-family/half-precision-floating-point
  extensions-to-the-c-language-family/decimal-floating-types
  extensions-to-the-c-language-family/fixed-point-types
  extensions-to-the-c-language-family/arrays-of-length-zero
  extensions-to-the-c-language-family/structures-with-no-members
  extensions-to-the-c-language-family/arrays-of-variable-length
  extensions-to-the-c-language-family/macros-with-a-variable-number-of-arguments
  extensions-to-the-c-language-family/slightly-looser-rules-for-escaped-newlines
  extensions-to-the-c-language-family/non-lvalue-arrays-may-have-subscripts
  extensions-to-the-c-language-family/arithmetic-on-void-and-function-pointers
  extensions-to-the-c-language-family/pointer-arguments-in-variadic-functions
  extensions-to-the-c-language-family/pointers-to-arrays-with-qualifiers-work-as-expected
  extensions-to-the-c-language-family/non-constant-initializers
  extensions-to-the-c-language-family/designated-initializers
  extensions-to-the-c-language-family/cast-to-a-union-type
  extensions-to-the-c-language-family/mixed-declarations-labels-and-code
  extensions-to-the-c-language-family/declaring-attributes-of-functions
  extensions-to-the-c-language-family/specifying-attributes-of-variables
  extensions-to-the-c-language-family/specifying-attributes-of-types
  extensions-to-the-c-language-family/prototypes-and-old-style-function-definitions
  extensions-to-the-c-language-family/c++-style-comments
  extensions-to-the-c-language-family/dollar-signs-in-identifier-names
  extensions-to-the-c-language-family/the-character-esc-in-constants
  extensions-to-the-c-language-family/determining-the-alignment-of-functions-types-or-variables
  extensions-to-the-c-language-family/an-inline-function-is-as-fast-as-a-macro
  extensions-to-the-c-language-family/when-is-a-volatile-object-accessed
  extensions-to-the-c-language-family/how-to-use-inline-assembly-language-in-c-code
  extensions-to-the-c-language-family/incomplete-enum-types
  extensions-to-the-c-language-family/function-names-as-strings
  extensions-to-the-c-language-family/getting-the-return-or-frame-address-of-a-function
  extensions-to-the-c-language-family/using-vector-instructions-through-built-in-functions
  extensions-to-the-c-language-family/support-for-offsetof
  extensions-to-the-c-language-family/legacy-sync-built-in-functions-for-atomic-memory-access
  extensions-to-the-c-language-family/built-in-functions-for-memory-model-aware-atomic-operations
  extensions-to-the-c-language-family/built-in-functions-to-perform-arithmetic-with-overflow-checking
  extensions-to-the-c-language-family/object-size-checking-built-in-functions
  extensions-to-the-c-language-family/other-built-in-functions-provided-by-gcc
  extensions-to-the-c-language-family/format-checks-specific-to-particular-target-machines
  extensions-to-the-c-language-family/pragmas-accepted-by-gcc
  extensions-to-the-c-language-family/unnamed-structure-and-union-fields
  extensions-to-the-c-language-family/thread-local-storage
  extensions-to-the-c-language-family/binary-constants-using-the-0b-prefix

