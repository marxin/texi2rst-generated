C++ Standards Support in GCC
============================

GCC supports different dialects of C++, corresponding to the multiple
published ISO standards. Which standard it implements can be selected
using the ``-std=`` command-line option.

-  `C++98 <#cxx98>`__
-  `C++11 <#cxx11>`__
-  `C++14 <#cxx14>`__
-  `C++17 <#cxx17>`__
-  `C++20 <#cxx20>`__
-  `C++23 <#cxx23>`__
-  `Technical Specifications <#tses>`__

For information about the status of C++ defect reports, please see `this
page <https://gcc.gnu.org/projects/cxx-dr-status.html>`__.

For information about the status of the library implementation, please
see the `Implementation
Status <https://gcc.gnu.org/onlinedocs/libstdc++/manual/status.html>`__
section of the Libstdc++ manual.

.. _cxx23:

C++23 Support in GCC
--------------------

GCC has experimental support for the next revision of the C++ standard,
which is expected to be published in 2023.

C++23 features are available since GCC 11. To enable C++23 support, add
the command-line parameter ``-std=c++2b`` to your ``g++`` command line.
Or, to enable GNU extensions in addition to C++23 features, add
``-std=gnu++2b``.

**Important**: Because the ISO C++23 standard is still evolving, GCC's
support is **experimental**. No attempt will be made to maintain
backward compatibility with implementations of C++23 features that do
not reflect the final standard.

C++23 Language Features
-----------------------

.. list-table::
   :widths: 18 18 18 18
   :header-rows: 1

   - 

      - Language Feature
      - Proposal
      - Available in GCC?
      - SD-6 Feature Test
   - 

      - Literal Suffix for (signed) size_t
      - `P0330R8 <https://wg21.link/p0330r8>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - \__cpp_size_t_suffix >= 202006L
   - 

      - Make () more optional for lambdas
      - `P1102R2 <https://wg21.link/p1102r2>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - DR: Declarations and where to find them
      - `P1787R6 <https://wg21.link/p1787r6>`__
      - `No <https://gcc.gnu.org/PR98939>`__
      - 
   - 

      - ``if consteval``
      - `P1938R3 <https://wg21.link/p1938r3>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - \__cpp_if_consteval >= 202106L
   - 

      - C++ Identifier Syntax using Unicode Standard Annex 31
      - `P1949R7 <https://wg21.link/p1949r7>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - 
   - 

      - Allow Duplicate Attributes
      - `P2156R1 <https://wg21.link/p2156r1>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - Narrowing contextual conversions to bool
      - `P1401R5 <https://wg21.link/p1401r5>`__
      - 9
      - 
   - 

      - Trimming whitespaces before line splicing
      - `P2223R2 <https://wg21.link/p2223r2>`__
      - Yes
      - 
   - 

      - Mixed string literal concatenation
      - `P2201R1 <https://wg21.link/p2201r1>`__
      - Yes
      - 
   - 

      - Make declaration order layout mandated
      - `P1847R4 <https://wg21.link/p1847r4>`__
      - Yes
      - 
   - 

      - Removing Garbage Collection Support
      - `P2186R2 <https://wg21.link/p2186r2>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - 
   - 

      - Simpler implicit move
      - `P2266R3 <https://wg21.link/p2266r3>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - \__cpp_implicit_move >= 202207L
   - 

      - Deducing this
      - `P0847R7 <https://wg21.link/p0847r7>`__
      - `No <https://gcc.gnu.org/PR102609>`__
      - \__cpp_explicit_this_parameter >= 202110L
   - 

      - 
      - `CWG2586 <https://wg21.link/cwg2586>`__
      - 
      - 
   - 

      - Change scope of lambda trailing-return-type
      - `P2036R3 <https://wg21.link/p2036r3>`__
      - `No <https://gcc.gnu.org/PR102610>`__
      - 
   - 

      - 
      - `P2579R0 <https://wg21.link/p2579r0>`__
      - 
      - 
   - 

      - Multidimensional subscript operator
      - `P2128R6 <https://wg21.link/p2128r6>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - \__cpp_multidimensional_subscript >= 202110L
   - 

      - 
      - `CWG2507 <https://wg21.link/cwg2507>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - 
   - 

      - Non-literal variables (and labels and gotos) in constexpr
         functions
      - `P2242R3 <https://wg21.link/p2242r3>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - \__cpp_constexpr >= 202110L
   - 

      - Character encoding of diagnostic text
      - `P2246R1 <https://wg21.link/p2246r1>`__
      - `No <https://gcc.gnu.org/PR102613>`__
      - 
   - 

      - Character sets and encodings
      - `P2314R4 <https://wg21.link/p2314r4>`__
      - `No <https://gcc.gnu.org/PR102614>`__
      - 
   - 

      - Consistent character literal encoding
      - `P2316R2 <https://wg21.link/p2316r2>`__
      - Yes
      - 
   - 

      - Add support for preprocessing directives elifdef and elifndef
      - `P2334R1 <https://wg21.link/p2334r1>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - 
   - 

      - Extend init-statement to allow alias-declaration
      - `P2360R0 <https://wg21.link/p2360r0>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - 
   - 

      - auto(x): decay-copy in the language
      - `P0849R8 <https://wg21.link/p0849r8>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - 
   - 

      - Labels at the end of compound statements
      - `P2324R1 <https://wg21.link/p2324r1>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - 
   - 

      - CWG 2397: auto specifier for pointers and references to arrays
      - `CWG2397 <https://wg21.link/cwg2397>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - 
   - 

      - CWG 2481: Cv-qualification of temporary to which a reference is
         bound
      - `CWG2481 <https://wg21.link/cwg2481>`__
      - Yes
      - 
   - 

      - Attributes on lambda-expressions
      - `P2173R1 <https://wg21.link/p2173r1>`__
      - 9
      - 
   - 

      - A type trait to detect reference binding to temporary
      - `P2255R2 <https://wg21.link/p2255r2>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - 
   - 

      - The Equality Operator You Are Looking For
      - `P2468R2 <https://wg21.link/p2468r2>`__
      - `No <https://gcc.gnu.org/PR106644>`__
      - 
   - 

      - De-deprecating volatile compound operations
      - `P2327R1 <https://wg21.link/p2327r1>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - 
   - 

      - Support for ``#warning``
      - `P2437R1 <https://wg21.link/p2437r1>`__
      - Yes (extension)
         `13 <../gcc-13/changes.html#cxx>`__ (P2437R1)
      - 
   - 

      - Remove non-encodable wide character literals and multicharacter
         wide character literals
      - `P2362R3 <https://wg21.link/p2362r3>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - 
   - 

      - Delimited escape sequences
      - `P2290R3 <https://wg21.link/p2290r3>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - 
   - 

      - Named universal character escapes
      - `P2071R2 <https://wg21.link/p2071r2>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - \__cpp_named_character_escapes >= 202207L
   - 

      - Relaxing some constexpr restrictions
      - `P2448R2 <https://wg21.link/p2448r2>`__
      - `No <https://gcc.gnu.org/PR106649>`__
      - \__cpp_constexpr >= 202207L
   - 

      - Using unknown references in constant expressions
      - `P2280R4 <https://wg21.link/p2280r4>`__
      - `No <https://gcc.gnu.org/PR106650>`__
      - 
   - 

      - static ``operator()``
      - `P1169R4 <https://wg21.link/p1169r4>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - \__cpp_static_call_operator >= 202207L
   - 

      - Extended floating-point types and standard names
      - `P1467R9 <https://wg21.link/p1467r9>`__
      - `No <https://gcc.gnu.org/PR106652>`__
      - 
   - 

      - Class template argument deduction from inherited constructors
      - `P2582R1 <https://wg21.link/p2582r1>`__
      - `No <https://gcc.gnu.org/PR106653>`__
      - 
   - 

      - Portable assumptions
      - `P1774R8 <https://wg21.link/p1774r8>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - 
   - 

      - Support for UTF-8 as a portable source file encoding
      - `P2295R6 <https://wg21.link/p2295r6>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - 
   - 

      - ``char8_t`` Compatibility and Portability Fix
      - `P2513R3 <https://wg21.link/p2513r3>`__
      - `13 <../gcc-13/changes.html#cxx>`__
      - \__cpp_char8_t >= 202207L
   - 

      - Relax requirements on ``wchar_t`` to match existing practices
      - `P2460R2 <https://wg21.link/p2460r2>`__
      - Yes
      - 
   - 

      - Explicit lifetime management
      - `P2590R2 <https://wg21.link/p2590r2>`__
      - `No <https://gcc.gnu.org/PR106658>`__
      - 

.. _cxx20:

C++20 Support in GCC
--------------------

GCC has experimental support for the latest revision of the C++
standard, which was published in 2020.

C++20 features are available since GCC 8. To enable C++20 support, add
the command-line parameter ``-std=c++20`` (use ``-std=c++2a`` in GCC 9
and earlier) to your ``g++`` command line. Or, to enable GNU extensions
in addition to C++20 features, add ``-std=gnu++20``.

**Important**: Because the ISO C++20 standard is very recent, GCC's
support is **experimental**.

C++20 Language Features
-----------------------

The following table lists new language features that have been accepted
into the C++20 working draft. The "Proposal" column provides a link to
the ISO C++ committee proposal that describes the feature, while the
"Available in GCC?" column indicates the first version of GCC that
contains an implementation of this feature (if it has been implemented).

.. list-table::
   :widths: 18 18 18 18
   :header-rows: 1

   - 

      - Language Feature
      - Proposal
      - Available in GCC?
      - SD-6 Feature Test
   - 

      - Default member initializers for bit-fields
      - `P0683R1 <https://wg21.link/p0683r1>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - 
   - 

      - Fixing const-qualified pointers to members
      - `P0704R1 <https://wg21.link/p0704r1>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - 
   - 

      - Allow lambda capture ``[=, this]``
      - `P0409R2 <https://wg21.link/p0409r2>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - 
   - 

      - ``__VA_OPT__`` for preprocessor comma elision
      - `P0306R4 <https://wg21.link/p0306r4>`__
         `P1042R1 <https://wg21.link/p1042r1>`__
      - `8 <../gcc-8/changes.html#cxx>`__ (partial, no ``#__VA_OPT__``
         support)
         `10 <../gcc-10/changes.html#cxx>`__ (partial, no placemarker
         token handling changes)
         `12 <../gcc-12/changes.html#cxx>`__
      - 
   - 

      - Designated initializers
      - `P0329R4 <https://wg21.link/p0329r4>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - \__cpp_designated_initializers >= 201707
   - 

      - Familiar template syntax for generic lambdas
      - `P0428R2 <https://wg21.link/p0428r2>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - \__cpp_generic_lambdas >= 201707
   - 

      - List deduction of vector
      - `P0702R1 <https://wg21.link/p0702r1>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - 
   - 

      - Concepts
      - `P0734R0 <https://wg21.link/p0734r0>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - \__cpp_concepts >= 201907
   - 

      - 
      - `P0857R0 <https://wg21.link/p0857r0>`__
      - 
      - 
   - 

      - 
      - `P1084R2 <https://wg21.link/p1084r2>`__
      - 
      - 
   - 

      - 
      - `P1141R2 <https://wg21.link/p1141r2>`__
      - 
      - 
   - 

      - 
      - `P0848R3 <https://wg21.link/p0848r3>`__
      - 
      - \__cpp_concepts >= 202002
   - 

      - 
      - `P1616R1 <https://wg21.link/p1616r1>`__
      - 
      - \__cpp_concepts >= 201907
   - 

      - 
      - `P1452R2 <https://wg21.link/p1452r2>`__
      - 
      - 
   - 

      - 
      - `P1972R0 <https://wg21.link/p1972r0>`__
      - 
      - 
   - 

      - 
      - `P1980R0 <https://wg21.link/p1980r0>`__
      - 
      - 
   - 

      - 
      - `P2092R0 <https://wg21.link/p2092r0>`__
      - 
      - 
   - 

      - 
      - `P2103R0 <https://wg21.link/p2103r0>`__
      - 
      - 
   - 

      - 
      - `P2113R0 <https://wg21.link/p2113r0>`__
      - `10.2 <../gcc-10/changes.html#cxx>`__ (no reversed operator
         handling)
      - 
   - 

      - Range-based for statements with initializer
      - `P0614R1 <https://wg21.link/p0614r1>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Simplifying implicit lambda capture
      - `P0588R1 <https://wg21.link/p0588r1>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - 
   - 

      - ADL and function templates that are not visible
      - `P0846R0 <https://wg21.link/p0846r0>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - ``const`` mismatch with defaulted copy constructor
      - `P0641R2 <https://wg21.link/p0641r2>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Less eager instantiation of ``constexpr`` functions
      - `P0859R0 <https://wg21.link/p0859r0>`__
      - 5.2 (mostly)
         `9 <../gcc-9/changes.html#cxx>`__ (P0859R0)
      - \__cpp_constexpr_in_decltype >= 201711
   - 

      - Consistent comparison (``operator<=>``)
      - `P0515R3 <https://wg21.link/p0515r3>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - \__cpp_impl_three_way_comparison >= 201711
   - 

      - 
      - `P0905R1 <https://wg21.link/p0905r1>`__
      - 
      - 
   - 

      - 
      - `P1120R0 <https://wg21.link/p1120r0>`__
      - 
      - 
   - 

      - 
      - `P1185R2 <https://wg21.link/p1185r2>`__
      - 
      - 
   - 

      - 
      - `P1186R3 <https://wg21.link/p1186r3>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1630R1 <https://wg21.link/p1630r1>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1946R0 <https://wg21.link/p1946r0>`__
      - 
      - 
   - 

      - 
      - `P1959R0 <https://wg21.link/p1959r0>`__
      - 
      - 
   - 

      - 
      - `P2002R1 <https://wg21.link/p2002r1>`__
      - `10.2 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - 
      - `P2085R0 <https://wg21.link/p2085r0>`__
      - 
      - 
   - 

      - Access checking on specializations
      - `P0692R1 <https://wg21.link/p0692r1>`__
      - Yes
      - 
   - 

      - Default constructible and assignable stateless lambdas
      - `P0624R2 <https://wg21.link/p0624r2>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Lambdas in unevaluated contexts
      - `P0315R4 <https://wg21.link/p0315r4>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Language support for empty objects
      - `P0840R2 <https://wg21.link/p0840r2>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Relaxing the range-for loop customization point finding rules
      - `P0962R1 <https://wg21.link/p0962r1>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - 
   - 

      - Allow structured bindings to accessible members
      - `P0969R0 <https://wg21.link/p0969r0>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - 
   - 

      - Relaxing the structured bindings customization point finding
         rules
      - `P0961R1 <https://wg21.link/p0961r1>`__
      - `8 <../gcc-8/changes.html#cxx>`__
      - 
   - 

      - Down with typename!
      - `P0634R3 <https://wg21.link/p0634r3>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Allow pack expansion in lambda init-capture
      - `P0780R2 <https://wg21.link/p0780r2>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - \__cpp_init_captures >= 201803
   - 

      - 
      - `P2095R0 <https://wg21.link/p2095r0>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - Proposed wording for likely and unlikely attributes
      - `P0479R5 <https://wg21.link/p0479r5>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Deprecate implicit capture of this via [=]
      - `P0806R2 <https://wg21.link/p0806r2>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Class Types in Non-Type Template Parameters
      - `P0732R2 <https://wg21.link/p0732r2>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - \__cpp_nontype_template_parameter_class >= 201806
   - 

      - Inconsistencies with non-type template parameters
      - `P1907R1 <https://wg21.link/p1907r1>`__
      - `10 <../gcc-10/changes.html#cxx>`__ (no floating point, union,
         or subobject template args)
         `11 <../gcc-11/changes.html#cxx>`__ (complete)
      - \__cpp_nontype_template_args >= 201911
   - 

      - Atomic Compare-and-Exchange with Padding Bits
      - `P0528R3 <https://wg21.link/p0528r3>`__
      - `11 <../gcc-11/changes.html#cxx>`__ (compiler side only)
         `13 <../gcc-13/changes.html#cxx>`__ (full)
      - 
   - 

      - Efficient sized delete for variable sized classes
      - `P0722R3 <https://wg21.link/p0722r3>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - \__cpp_impl_destroying_delete >= 201806
   - 

      - Allowing Virtual Function Calls in Constant Expressions
      - `P1064R0 <https://wg21.link/p1064r0>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Prohibit aggregates with user-declared constructors
      - `P1008R1 <https://wg21.link/p1008r1>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - explicit(bool)
      - `P0892R2 <https://wg21.link/p0892r2>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - \__cpp_conditional_explicit >= 201806
   - 

      - Signed integers are two's complement
      - `P1236R1 <https://wg21.link/p1236r1>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - char8_t
      - `P0482R6 <https://wg21.link/p0482r6>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - \__cpp_char8_t >= 201811
   - 

      - Immediate functions (consteval)
      - `P1073R3 <https://wg21.link/p1073r3>`__
      - `10 <../gcc-10/changes.html#cxx>`__ (no ``consteval virtual``)
         `11 <../gcc-11/changes.html#cxx>`__ (full)
      - \__cpp_consteval >= 201811
   - 

      - 
      - `P1937R2 <https://wg21.link/p1937r2>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - std::is_constant_evaluated
      - `P0595R2 <https://wg21.link/p0595r2>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Nested inline namespaces
      - `P1094R2 <https://wg21.link/p1094r2>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - Relaxations of constexpr restrictions
      - `P1002R1 <https://wg21.link/p1002r1>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1327R1 <https://wg21.link/p1327r1>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1330R0 <https://wg21.link/p1330r0>`__
      - `9 <../gcc-9/changes.html#cxx>`__
      - \__cpp_constexpr >= 202002
   - 

      - 
      - `P1331R2 <https://wg21.link/p1331r2>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - \__cpp_constexpr >= 201907
   - 

      - 
      - `P1668R1 <https://wg21.link/p1668r1>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - 
      - `P0784R7 <https://wg21.link/p0784r7>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - \__cpp_constexpr_dynamic_alloc >= 201907
   - 

      - Feature test macros
      - `P0941R2 <https://wg21.link/p0941r2>`__
      - `4.9 <../gcc-4.9/changes.html#cxx>`__ (``__cpp_`` macros)
         `5 <../gcc-5/changes.html#cxx>`__ (``__has_cpp_attribute``)
      - 
   - 

      - Modules
      - `P1103R3 <https://wg21.link/p1103r3>`__
      - `11 <../gcc-11/changes.html#cxx>`__ (requires ``-fmodules-ts``)
         (No Private Module Fragment,
         Parser-level Global Module Entity Merging,
         Global Module Implications of ``extern "C/C++"``,
         or Partition-specific Definition Visibility)
      - ``__cpp_modules >= 201810L``
         (Date of p1103r3)
   - 

      - 
      - `P1766R1 <https://wg21.link/p1766r1>`__
      - No
      - 
   - 

      - 
      - `P1811R0 <https://wg21.link/p1811r0>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1703R1 <https://wg21.link/p1703r1>`__ (superceded by p1857)
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1874R1 <https://wg21.link/p1874r1>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1979R0 <https://wg21.link/p1979r0>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1779R3 <https://wg21.link/p1779r3>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1857R3 <https://wg21.link/p1857r3>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - 
      - `P2115R0 <https://wg21.link/p2115r0>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - 
      - `P1815R2 <https://wg21.link/p1815r2>`__
      - No
      - 
   - 

      - Coroutines
      - `P0912R5 <https://wg21.link/p0912r5>`__ as applied to
         `n4861 <https://wg21.link/n4861>`__
      - `10 <../gcc-10/changes.html#cxx>`__ (requires -fcoroutines)
      - \__cpp_impl_coroutine >= 201902
   - 

      - Parenthesized initialization of aggregates
      - `P0960R3 <https://wg21.link/p0960r3>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - \__cpp_aggregate_paren_init >= 201902
   - 

      - 
      - `P1975R0 <https://wg21.link/p1975r0>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - DR: array size deduction in *new-expression*
      - `P1009R2 <https://wg21.link/p1009r2>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - DR: Converting from ``T*`` to ``bool`` should be considered
         narrowing
      - `P1957R2 <https://wg21.link/p1957r2>`__
      - `10 <../gcc-10/changes.html#cxx>`__ (C++20 mode only),
         `11 <../gcc-11/changes.html#cxx>`__ (all modes)
      - 
   - 

      - Stronger Unicode requirements
      - `P1041R4 <https://wg21.link/p1041r4>`__
         `P1139R2 <https://wg21.link/p1139r2>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - Structured binding extensions
      - `P1091R3 <https://wg21.link/p1091r3>`__
         `P1381R1 <https://wg21.link/p1381r1>`__
      - `10 <../gcc-10/changes.html#cxx>`__
         `8 <../gcc-8/changes.html#cxx>`__
      - 
   - 

      - Deprecate ``a[b,c]``
      - `P1161R3 <https://wg21.link/p1161r3>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - Deprecating some uses of ``volatile``
      - `P1152R4 <https://wg21.link/p1152r4>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - ``[[nodiscard("with reason")]]``
      - `P1301R4 <https://wg21.link/p1301r4>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - ``using enum``
      - `P1099R5 <https://wg21.link/p1099r5>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - Class template argument deduction for aggregates
      - `P1816R0 <https://wg21.link/p1816r0>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - \__cpp_deduction_guides >= 201907L
   - 

      - 
      - `P2082R1 <https://wg21.link/p2082r1>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - Class template argument deduction for alias templates
      - `P1814R0 <https://wg21.link/p1814r0>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - Permit conversions to arrays of unknown bound
      - `P0388R4 <https://wg21.link/p0388r4>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - 
   - 

      - ``constinit``
      - `P1143R2 <https://wg21.link/p1143r2>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - \__cpp_constinit >= 201907
   - 

      - Layout-compatibility and Pointer-interconvertibility Traits
      - `P0466R5 <https://wg21.link/p0466r5>`__
      - `12 <../gcc-12/changes.html#cxx>`__
      - (in library)
   - 

      - DR: Checking for abstract class types
      - `P0929R2 <https://wg21.link/p0929r2>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 
   - 

      - DR: More implicit moves (merge P0527R1 and P1155R3)
      - `P1825R0 <https://wg21.link/p1825r0>`__
      - `11 <../gcc-11/changes.html#cxx>`__ (C++20 mode)
      - 
   - 

      - DR: Pseudo-destructors end object lifetimes
      - `P0593R6 <https://wg21.link/p0593r6>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - 

.. _cxx17:

C++17 Support in GCC
--------------------

GCC has almost full support for the previous revision of the C++
standard, which was published in 2017. Some library features are missing
or incomplete, as described in `the library
documentation <https://gcc.gnu.org/onlinedocs/libstdc++/manual/status.html#status.iso.2017>`__.

C++17 features are available since GCC 5. This mode is the default in
GCC 11; it can be explicitly selected with the ``-std=c++17``
command-line flag, or ``-std=gnu++17`` to enable GNU extensions as well.

C++17 Language Features
-----------------------

The following table lists new language features that have been accepted
into the C++17 working draft. The "Proposal" column provides a link to
the ISO C++ committee proposal that describes the feature, while the
"Available in GCC?" column indicates the first version of GCC that
contains an implementation of this feature (if it has been implemented).

.. list-table::
   :widths: 18 18 18 18
   :header-rows: 1

   - 

      - Language Feature
      - Proposal
      - Available in GCC?
      - SD-6 Feature Test
   - 

      - Removing trigraphs
      - `N4086 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4086.html>`__
      - `5 <../gcc-5/changes.html#cxx>`__
      - 
   - 

      - ``u8`` character literals
      - `N4267 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4267.html>`__
      - `6 <../gcc-6/changes.html#cxx>`__
      - \__cpp_unicode_characters >= 201411
   - 

      - Folding expressions
      - `N4295 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4295.html>`__
      - `6 <../gcc-6/changes.html#cxx>`__
      - \__cpp_fold_expressions >= 201411
   - 

      - Attributes for namespaces and enumerators
      - `N4266 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4266.html>`__
      - `4.9 <../gcc-4.9/changes.html#cxx>`__ (namespaces)
         `6 <../gcc-6/changes.html#cxx>`__ (enumerators)
      - \__cpp_namespace_attributes >= 201411
         \__cpp_enumerator_attributes >= 201411
   - 

      - Nested namespace definitions
      - `N4230 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4230.html>`__
      - `6 <../gcc-6/changes.html#cxx>`__
      - \__cpp_nested_namespace_definitions >= 201411
   - 

      - Allow constant evaluation for all non-type template arguments
      - `N4268 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4268.html>`__
      - `6 <../gcc-6/changes.html#cxx>`__
      - \__cpp_nontype_template_args >= 201411
   - 

      - Extending ``static_assert``
      - `N3928 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n3928.pdf>`__
      - `6 <../gcc-6/changes.html#cxx>`__
      - \__cpp_static_assert >= 201411
   - 

      - New Rules for auto deduction from braced-init-list
      - `N3922 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n3922.html>`__
      - `5 <../gcc-5/changes.html#cxx>`__
      - 
   - 

      - Allow typename in a template template parameter
      - `N4051 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4051.html>`__
      - `5 <../gcc-5/changes.html#cxx>`__
      - 
   - 

      - ``[[fallthrough]]`` attribute
      - `P0188R1 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0188r1.pdf>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__has_cpp_attribute(fallthrough)
   - 

      - ``[[nodiscard]]`` attribute
      - `P0189R1 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0189r1.pdf>`__
      - `4.8 <../gcc-4.8/changes.html#cxx>`__
         (``[[gnu::warn_unused_result]]``)
         `7 <../gcc-7/changes.html#cxx>`__ (P0189R1)
      - \__has_cpp_attribute(nodiscard)
   - 

      - ``[[maybe_unused]]`` attribute
      - `P0212R1 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0212r1.pdf>`__
      - `4.8 <../gcc-4.8/changes.html#cxx>`__ (``[[gnu::unused]]``)
         `7 <../gcc-7/changes.html#cxx>`__ (P0212R1)
      - \__has_cpp_attribute(maybe_unused)
   - 

      - Extension to aggregate initialization
      - `P0017R1 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/p0017r1.html>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_aggregate_bases >= 201603
   - 

      - Wording for ``constexpr`` lambda
      - `P0170R1 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0170r1.pdf>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_constexpr >= 201603
   - 

      - Unary Folds and Empty Parameter Packs
      - `P0036R0 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/p0036r0.pdf>`__
      - `6 <../gcc-6/changes.html#cxx>`__
      - \__cpp_fold_expressions >= 201603
   - 

      - Generalizing the Range-Based For Loop
      - `P0184R0 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0184r0.html>`__
      - `6 <../gcc-6/changes.html#cxx>`__
      - \__cpp_range_based_for >= 201603
   - 

      - Lambda capture of ``*this`` by Value
      - `P0018R3 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0018r3.html>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_capture_star_this >= 201603
   - 

      - Construction Rules for ``enum class`` variables
      - `P0138R2 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0138r2.pdf>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - 
   - 

      - Hexadecimal floating literals for C++
      - `P0245R1 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0245r1.html>`__
      - 3.0
      - \__cpp_hex_float >= 201603
   - 

      - Dynamic memory allocation for over-aligned data
      - `P0035R4 <https://wg21.link/p0035>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_aligned_new >= 201606
   - 

      - Guaranteed copy elision
      - `P0135R1 <https://wg21.link/p0135>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_guaranteed_copy_elision >= 201606
   - 

      - Refining Expression Evaluation Order for Idiomatic C++
      - `P0145R3 <https://wg21.link/p0145>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - 
   - 

      - ``constexpr`` if
      - `P0292R2 <https://wg21.link/p0292>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_if_constexpr >= 201606
   - 

      - Selection statements with initializer
      - `P0305R1 <https://wg21.link/p0305>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - 
   - 

      - Template argument deduction for class templates
      - `P0091R3 <https://wg21.link/p0091>`__
         `P0512R0 <https://wg21.link/p0512r0>`__
      - `7 <../gcc-7/changes.html#cxx>`__
         `8 <../gcc-8/changes.html#cxx>`__
      - \__cpp_deduction_guides >= 201606
         \__cpp_deduction_guides >= 201611
   - 

      - Declaring non-type template parameters with auto
      - `P0127R2 <https://wg21.link/p0127>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_template_auto >= 201606
         \__cpp_nontype_template_parameter_auto >= 201606
   - 

      - Using attribute namespaces without repetition
      - `P0028R4 <https://wg21.link/p0028>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - 
   - 

      - Ignoring unsupported non-standard attributes
      - `P0283R2 <https://wg21.link/p0283>`__
      - Yes
      - 
   - 

      - Structured bindings
      - `P0217R3 <https://wg21.link/p0217>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_structured_bindings >= 201606
   - 

      - Remove Deprecated Use of the ``register`` Keyword
      - `P0001R1 <https://wg21.link/p0001>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - 
   - 

      - Remove Deprecated ``operator++(bool)``
      - `P0002R1 <https://wg21.link/p0002>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - 
   - 

      - Make exception specifications be part of the type system
      - `P0012R1 <https://wg21.link/p0012>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_noexcept_function_type >= 201510
   - 

      - ``__has_include`` for C++17
      - `P0061R1 <https://wg21.link/p0061>`__
      - `5 <../gcc-5/changes.html#cxx>`__
      - 
   - 

      - Rewording inheriting constructors (core issue 1941 et al)
      - `P0136R1 <https://wg21.link/p0136>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_inheriting_constructors >= 201511
   - 

      - Inline variables
      - `P0386R2 <https://wg21.link/p0386r2>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_inline_variables >= 201606
   - 

      - DR 150, Matching of template template arguments
      - `P0522R0 <https://wg21.link/p0522r0>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_template_template_args >= 201611
   - 

      - Removing dynamic exception specifications
      - `P0003R5 <https://wg21.link/p0003r5>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - 
   - 

      - Pack expansions in *using-declarations*
      - `P0195R2 <https://wg21.link/p0195r2>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - \__cpp_variadic_using >= 201611
   - 

      - A ``byte`` type definition
      - `P0298R0 <https://wg21.link/p0298r0>`__
      - `7 <../gcc-7/changes.html#cxx>`__
      - 

.. _tses:

Technical Specifications
------------------------

GCC also implements experimental support for some language Technical
Specifications published by the C++ committee.

**Important**: Because these Technical Specifications are still evolving
toward future inclusion in a C++ standard, GCC's support is
**experimental**. No attempt will be made to maintain backward
compatibility with implementations of features that do not reflect the
final standard.

.. list-table::
   :header-rows: 1

   - 

      - Technical Specification
      - Document
      - Available in GCC?
      - Compiler Option
      - SD-6 Feature Test
   - 

      - Concepts
      - `N4377 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4377.pdf>`__
      - `6 <../gcc-6/changes.html#cxx>`__
      - -fconcepts
      - \__cpp_concepts >= 201507
   - 

      - Transactional Memory
      - `N4514 <https://www.open-std.org/Jtc1/sc22/wg21/docs/papers/2015/n4514.pdf>`__
      - `6 <../gcc-6/changes.html#cxx>`__ (no atomic_cancel)
      - -fgnu-tm
      - \__cpp_transactional_memory >= 201505
   - 

      - Coroutines
      - `N4649 <https://wg21.link/n4649>`__
      - `10 <../gcc-10/changes.html#cxx>`__
      - -fcoroutines
      - \__cpp_impl_coroutine >= 201902L
   - 

      - Modules
      - `N4720 <https://wg21.link/n4720>`__
      - `11 <../gcc-11/changes.html#cxx>`__
      - -fmodules-ts
      - \__cpp_modules >= 201810L

.. _cxx14:

C++14 Support in GCC
--------------------

GCC has full support for the of the 2014 C++ standard.

This mode is the default in GCC 6.1 up until GCC 10 (including); it can
be explicitly selected with the ``-std=c++14`` command-line flag, or
``-std=gnu++14`` to enable GNU extensions as well.

C++14 Language Features
-----------------------

The following table lists new language features that are part of the
C++14 standard. The "Proposal" column provides a link to the ISO C++
committee proposal that describes the feature, while the "Available in
GCC?" column indicates the first version of GCC that contains an
implementation of this feature.

.. list-table::
   :widths: 18 18 18 18
   :header-rows: 1

   - 

      - Language Feature
      - Proposal
      - Available in GCC?
      - SD-6 Feature Test
   - 

      - Tweak to certain C++ contextual conversions
      - `N3323 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3323.pdf>`__
      - `4.9 <../gcc-4.9/changes.html#cxx>`__
      - 
   - 

      - Binary literals
      - `N3472 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3472.pdf>`__
      - `4.3 <../gcc-4.3/changes.html#cxx>`__ (GNU)
         `4.9 <../gcc-4.9/changes.html#cxx>`__ (N3472)
      - \__cpp_binary_literals >= 201304
   - 

      - Return type deduction for normal functions
      - `N3638 <https://isocpp.org/files/papers/N3638.html>`__
      - `4.8 <../gcc-4.8/changes.html#cxx>`__ (N3386)
         `4.9 <../gcc-4.9/changes.html#cxx>`__ (N3638)
      - \__cpp_decltype_auto >= 201304
   - 

      - Generalized lambda capture (init-capture)
      - `N3648 <https://isocpp.org/files/papers/N3648.html>`__
      - `4.5 <../gcc-4.5/changes.html#cplusplus>`__ (partial)
         `4.9 <../gcc-4.9/changes.html#cxx>`__ (N3648)
      - \__cpp_init_captures >= 201304
   - 

      - Generic (polymorphic) lambda expressions
      - `N3649 <https://isocpp.org/files/papers/N3649.html>`__
      - `4.9 <../gcc-4.9/changes.html#cxx>`__
      - \__cpp_generic_lambdas >= 201304
   - 

      - Variable templates
      - `N3651 <https://isocpp.org/files/papers/N3651.pdf>`__
      - `5 <../gcc-5/changes.html#cxx>`__
      - \__cpp_variable_templates >= 201304
   - 

      - Relaxing requirements on constexpr functions
      - `N3652 <https://isocpp.org/files/papers/N3652.html>`__
      - `5 <../gcc-5/changes.html#cxx>`__
      - \__cpp_constexpr >= 201304
   - 

      - Member initializers and aggregates
      - `N3653 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3653.html>`__
      - `5 <../gcc-5/changes.html#cxx>`__
      - \__cpp_aggregate_nsdmi >= 201304
   - 

      - Clarifying memory allocation
      - `N3664 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3664.html>`__
      - N/A
      - 
   - 

      - Sized deallocation
      - `N3778 <https://isocpp.org/files/papers/n3778.html>`__
      - `5 <../gcc-5/changes.html#cxx>`__
      - \__cpp_sized_deallocation >= 201309
   - 

      - [[deprecated]] attribute
      - `N3760 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3760.html>`__
      - `4.9 <../gcc-4.9/changes.html#cxx>`__ (N3797)
      - \__has_cpp_attribute(deprecated) >= 201309
   - 

      - Single-quotation-mark as a digit separator
      - `N3781 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3781.pdf>`__
      - `4.9 <../gcc-4.9/changes.html#cxx>`__ (N3797)
      - \__cpp_digit_separator >= 201309

This feature was briefly part of the C++14 working paper, but was not
part of the published standard; as a result, it has been removed from
the compiler.

.. list-table::
   :widths: 18 18 18 18
   :header-rows: 1

   - 

      - Language Feature
      - Proposal
      - Available in GCC?
      - SD-6 Feature Test
   - 

      - Runtime-sized arrays with automatic storage duration
         (Removed from the standard)
      - `N3639 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3639.html>`__
      - ?.? (GNU VLAs)
         `4.9 <../gcc-4.9/changes.html#cxx>`__ (N3639)
         `5 <../gcc-5/changes.html#cxx>`__ (GNU VLAs)
      - \__cpp_runtime_arrays >= 198712

.. _cxx11:

C++11 Support in GCC
--------------------

GCC 4.8.1 was the first feature-complete implementation of the 2011 C++
standard, previously known as C++0x.

This mode can be selected with the ``-std=c++11`` command-line flag, or
``-std=gnu++11`` to enable GNU extensions as well.

For information about C++11 support in a specific version of GCC, please
see:

-  `GCC 4.3 C++0x Status <../gcc-4.3/cxx0x_status.html>`__
-  `GCC 4.4 C++0x Status <../gcc-4.4/cxx0x_status.html>`__
-  `GCC 4.5 C++0x Status <../gcc-4.5/cxx0x_status.html>`__
-  `GCC 4.6 C++0x Status <../gcc-4.6/cxx0x_status.html>`__
-  `GCC 4.7 C++11 Status <../gcc-4.7/cxx0x_status.html>`__
-  `GCC 4.8 C++11 Status <../gcc-4.8/cxx0x_status.html>`__

.. list-table::
   :widths: 18 18 18 18
   :header-rows: 1

   - 

      - Language Feature
      - Proposal
      - Available in GCC?
      - SD-6 Feature Test
   - 

      - Rvalue references
      - `N2118 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2006/n2118.html>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - \__cpp_rvalue_references >= 200610
   - 

      -     Rvalue references for ``*this``
      - `N2439 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2439.htm>`__
      - `GCC 4.8.1 <../gcc-4.8/changes.html>`__
      - \__cpp_ref_qualifiers >= 200710
   - 

      - Initialization of class objects by rvalues
      - `N1610 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2004/n1610.html>`__
      - Yes
      - 
   - 

      - Non-static data member initializers
      - `N2756 <https://www.open-std.org/JTC1/SC22/WG21/docs/papers/2008/n2756.htm>`__
      - `GCC 4.7 <../gcc-4.7/changes.html>`__
      - \__cpp_nsdmi >= 200809
   - 

      - Variadic templates
      - `N2242 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2242.pdf>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - \__cpp_variadic_templates >= 200704
   - 

      -     Extending variadic template template parameters
      - `N2555 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2555.pdf>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      - Initializer lists
      - `N2672 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2672.htm>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - \__cpp_initializer_lists >= 200806
   - 

      - Static assertions
      - `N1720 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2004/n1720.html>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - \__cpp_static_assert >= 200410
   - 

      - ``auto``-typed variables
      - `N1984 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2006/n1984.pdf>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      -     Multi-declarator ``auto``
      - `N1737 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2004/n1737.pdf>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      -     Removal of auto as a storage-class specifier
      - `N2546 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2546.htm>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      -     New function declarator syntax
      - `N2541 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2541.htm>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      - New wording for C++0x lambdas
      - `N2927 <https://www.open-std.org/JTC1/SC22/WG21/docs/papers/2009/n2927.pdf>`__
      - `GCC 4.5 <../gcc-4.5/changes.html>`__
      - \__cpp_lambdas >= 200907
   - 

      - Declared type of an expression
      - `N2343 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2343.pdf>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - \__cpp_decltype >= 200707
   - 

      -     decltype and call expressions
      - `N3276 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2011/n3276.pdf>`__
      - `GCC 4.8.1 <../gcc-4.8/changes.html>`__
      - 
   - 

      - Right angle brackets
      - `N1757 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2005/n1757.html>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - 
   - 

      - Default template arguments for function templates
      - `DR226 <https://www.open-std.org/jtc1/sc22/wg21/docs/cwg_defects.html#226>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - 
   - 

      - Solving the SFINAE problem for expressions
      - `DR339 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2634.html>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      - Template aliases
      - `N2258 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2258.pdf>`__
      - `GCC 4.7 <../gcc-4.7/changes.html>`__
      - \__cpp_alias_templates >= 200704
   - 

      - Extern templates
      - `N1987 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2006/n1987.htm>`__
      - Yes
      - 
   - 

      - Null pointer constant
      - `N2431 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2431.pdf>`__
      - `GCC 4.6 <../gcc-4.6/changes.html>`__
      - 
   - 

      - Strongly-typed enums
      - `N2347 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2347.pdf>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      - Forward declarations for enums
      - `N2764 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2764.pdf>`__
      - `GCC 4.6 <../gcc-4.6/changes.html>`__
      - 
   - 

      - Generalized attributes
      - `N2761 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2761.pdf>`__
      - `GCC 4.8 <../gcc-4.8/changes.html>`__
      - \__cpp_attributes >= 200809;
         \__has_cpp_attribute(noreturn) >= 200809;
         \__has_cpp_attribute(carries_dependency) == 0 (not implemented)
   - 

      - Generalized constant expressions
      - `N2235 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2235.pdf>`__
      - `GCC 4.6 <../gcc-4.6/changes.html>`__
      - \__cpp_constexpr >= 200704
   - 

      - Alignment support
      - `N2341 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2341.pdf>`__
      - `GCC 4.8 <../gcc-4.8/changes.html>`__
      - 
   - 

      - Delegating constructors
      - `N1986 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2006/n1986.pdf>`__
      - `GCC 4.7 <../gcc-4.7/changes.html>`__
      - \__cpp_delegating_constructors >= 200604
   - 

      - Inheriting constructors
      - `N2540 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2540.htm>`__
      - `GCC 4.8 <../gcc-4.8/changes.html>`__
      - \__cpp_inheriting_constructors >= 200802
   - 

      - Explicit conversion operators
      - `N2437 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2437.pdf>`__
      - `GCC 4.5 <../gcc-4.5/changes.html>`__
      - 
   - 

      - New character types
      - `N2249 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2249.html>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - \__cpp_unicode_characters >= 200704
   - 

      - Unicode string literals
      - `N2442 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2442.htm>`__
      - `GCC 4.5 <../gcc-4.5/changes.html>`__
      - \__cpp_unicode_literals >= 200710
   - 

      - Raw string literals
      - `N2442 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2442.htm>`__
      - `GCC 4.5 <../gcc-4.5/changes.html>`__
      - \__cpp_raw_strings >= 200710
   - 

      - Universal character name literals
      - `N2170 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2170.html>`__
      - `GCC 4.5 <../gcc-4.5/changes.html>`__
      - 
   - 

      - User-defined literals
      - `N2765 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2765.pdf>`__
      - `GCC 4.7 <../gcc-4.7/changes.html>`__
      - \__cpp_user_defined_literals >= 200809
   - 

      - Standard Layout Types
      - `N2342 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2342.htm>`__
      - `GCC 4.5 <../gcc-4.5/changes.html>`__
      - 
   - 

      - Defaulted and deleted functions
      - `N2346 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2346.htm>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      - Extended friend declarations
      - `N1791 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2005/n1791.pdf>`__
      - `GCC 4.7 <../gcc-4.7/changes.html>`__
      - 
   - 

      - Extending ``sizeof``
      - `N2253 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2253.html>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      - Inline namespaces
      - `N2535 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2535.htm>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      - Unrestricted unions
      - `N2544 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2544.pdf>`__
      - `GCC 4.6 <../gcc-4.6/changes.html>`__
      - 
   - 

      - Local and unnamed types as template arguments
      - `N2657 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2657.htm>`__
      - `GCC 4.5 <../gcc-4.5/changes.html>`__
      - 
   - 

      - Range-based for
      - `N2930 <https://www.open-std.org/JTC1/SC22/WG21/docs/papers/2009/n2930.html>`__
      - `GCC 4.6 <../gcc-4.6/changes.html>`__
      - \__cpp_range_based_for >= 200907
   - 

      - Explicit virtual overrides
      - `N2928 <https://www.open-std.org/JTC1/SC22/WG21/docs/papers/2009/n2928.htm>`__
         `N3206 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2010/n3206.htm>`__
         `N3272 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2011/n3272.htm>`__
      - `GCC 4.7 <../gcc-4.7/changes.html>`__
      - 
   - 

      - Minimal support for garbage collection and reachability-based
         leak detection
      - `N2670 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2670.htm>`__
      - No
      - 
   - 

      - Allowing move constructors to throw [noexcept]
      - `N3050 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2010/n3050.html>`__
      - `GCC 4.6 <../gcc-4.6/changes.html>`__
      - 
   - 

      - Defining move special member functions
      - `N3053 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2010/n3053.html>`__
      - `GCC 4.6 <../gcc-4.6/changes.html>`__
      - 
   - 

      - Concurrency
      - 
      - 
      - 
   - 

      - Sequence points
      - `N2239 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2239.html>`__
      - Yes
      - 
   - 

      - Atomic operations
      - `N2427 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2427.html>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      - Strong Compare and Exchange
      - `N2748 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2748.html>`__
      - `GCC 4.5 <../gcc-4.5/changes.html>`__
      - 
   - 

      - Bidirectional Fences
      - `N2752 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2752.htm>`__
      - `GCC 4.8 <../gcc-4.8/changes.html>`__
      - 
   - 

      - Memory model
      - `N2429 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2429.htm>`__
      - `GCC 4.8 <../gcc-4.8/changes.html>`__
      - 
   - 

      - Data-dependency ordering: atomics and memory model
      - `N2664 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2664.htm>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
         (memory_order_consume)
      - 
   - 

      - Propagating exceptions
      - `N2179 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2179.html>`__
      - `GCC 4.4 <../gcc-4.4/changes.html>`__
      - 
   - 

      - Abandoning a process and at_quick_exit
      - `N2440 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2440.htm>`__
      - `GCC 4.8 <../gcc-4.8/changes.html>`__
      - 
   - 

      - Allow atomics use in signal handlers
      - `N2547 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2547.htm>`__
      - Yes
      - 
   - 

      - Thread-local storage
      - `N2659 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2659.htm>`__
      - `GCC 4.8 <../gcc-4.8/changes.html>`__
      - 
   - 

      - Dynamic initialization and destruction with concurrency
      - `N2660 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2008/n2660.htm>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - \__cpp_threadsafe_static_init >= 200806
   - 

      - C99 Features in C++11
      - 
      - 
      - 
   - 

      - ``__func__`` predefined identifier
      - `N2340 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2340.htm>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - 
   - 

      - C99 preprocessor
      - `N1653 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2004/n1653.htm>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - 
   - 

      - ``long long``
      - `N1811 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2005/n1811.pdf>`__
      - `GCC 4.3 <../gcc-4.3/changes.html>`__
      - 
   - 

      - Extended integral types
      - `N1988 <https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2006/n1988.pdf>`__
      - Yes
      - 

.. _cxx98:

C++98 Support in GCC
--------------------

GCC has full support for the 1998 C++ standard as modified by the 2003
technical corrigendum and some later defect reports, excluding the
``export`` feature which was later removed from the language.

This mode is the default in GCC versions prior to 6.1; it can be
explicitly selected with the ``-std=c++98`` command-line flag, or
``-std=gnu++98`` to enable GNU extensions as well.

.. container:: copyright

   For questions related to the use of GCC, please consult these web
   pages and the `GCC manuals <https://gcc.gnu.org/onlinedocs/>`__. If
   that fails, the gcc-help@gcc.gnu.org mailing list might help.
   Comments on these web pages and the development of GCC are welcome on
   our developer list at gcc@gcc.gnu.org. All of `our
   lists <https://gcc.gnu.org/lists.html>`__ have public archives.
   Copyright (C) `Free Software Foundation,
   Inc. <https://www.fsf.org>`__ Verbatim copying and distribution of
   this entire article is permitted in any medium, provided this notice
   is preserved.

   These pages are `maintained by the GCC
   team <https://gcc.gnu.org/about.html>`__. Last modified
   2022-10-21\ `. <http://validator.w3.org/check/referer>`__
