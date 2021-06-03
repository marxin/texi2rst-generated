.. _c++-dialect-options:

Options Controlling C++ Dialect
*******************************

.. index:: compiler options, C++

.. index:: C++ options, command-line

.. index:: options, C++

This section describes the command-line options that are only meaningful
for C++ programs.  You can also use most of the GNU compiler options
regardless of what language your program is in.  For example, you
might compile a file firstClass.C like this:

.. code-block:: c++

  g++ -g -fstrict-enums -O -c firstClass.C

In this example, only :option:`-fstrict-enums` is an option meant
only for C++ programs; you can use the other options with any
language supported by GCC.

Some options for compiling C programs, such as :option:`-std`, are also
relevant for C++ programs.
See :ref:`Options Controlling C Dialect <c-dialect-options>`.

Here is a list of options that are *only* for compiling C++ programs:

.. option:: -fabi-version=n

  Use version :samp:`{n}` of the C++ ABI.  The default is version 0.

  Version 0 refers to the version conforming most closely to
  the C++ ABI specification.  Therefore, the ABI obtained using version 0
  will change in different versions of G++ as ABI bugs are fixed.

  Version 1 is the version of the C++ ABI that first appeared in G++ 3.2.

  Version 2 is the version of the C++ ABI that first appeared in G++
  3.4, and was the default through G++ 4.9.

  Version 3 corrects an error in mangling a constant address as a
  template argument.

  Version 4, which first appeared in G++ 4.5, implements a standard
  mangling for vector types.

  Version 5, which first appeared in G++ 4.6, corrects the mangling of
  attribute const/volatile on function pointer types, decltype of a
  plain decl, and use of a function parameter in the declaration of
  another parameter.

  Version 6, which first appeared in G++ 4.7, corrects the promotion
  behavior of C++11 scoped enums and the mangling of template argument
  packs, const/static_cast, prefix ++ and -, and a class scope function
  used as a template argument.

  Version 7, which first appeared in G++ 4.8, that treats nullptr_t as a
  builtin type and corrects the mangling of lambdas in default argument
  scope.

  Version 8, which first appeared in G++ 4.9, corrects the substitution
  behavior of function types with function-cv-qualifiers.

  Version 9, which first appeared in G++ 5.2, corrects the alignment of
  ``nullptr_t``.

  Version 10, which first appeared in G++ 6.1, adds mangling of
  attributes that affect type identity, such as ia32 calling convention
  attributes (e.g. :samp:`stdcall`).

  Version 11, which first appeared in G++ 7, corrects the mangling of
  sizeof... expressions and operator names.  For multiple entities with
  the same name within a function, that are declared in different scopes,
  the mangling now changes starting with the twelfth occurrence.  It also
  implies :option:`-fnew-inheriting-ctors`.

  Version 12, which first appeared in G++ 8, corrects the calling
  conventions for empty classes on the x86_64 target and for classes
  with only deleted copy/move constructors.  It accidentally changes the
  calling convention for classes with a deleted copy constructor and a
  trivial move constructor.

  Version 13, which first appeared in G++ 8.2, fixes the accidental
  change in version 12.

  Version 14, which first appeared in G++ 10, corrects the mangling of
  the nullptr expression.

  Version 15, which first appeared in G++ 11, changes the mangling of
  ``__alignof__`` to be distinct from that of ``alignof``, and
  dependent operator names.

  See also :option:`-Wabi`.

.. option:: -fabi-compat-version=n

  On targets that support strong aliases, G++
  works around mangling changes by creating an alias with the correct
  mangled name when defining a symbol with an incorrect mangled name.
  This switch specifies which ABI version to use for the alias.

  With :option:`-fabi-version`:samp:`=0` (the default), this defaults to 11 (GCC 7
  compatibility).  If another ABI version is explicitly selected, this
  defaults to 0.  For compatibility with GCC versions 3.2 through 4.9,
  use :option:`-fabi-compat-version`:samp:`=2`.

  If this option is not provided but :option:`-Wabi`:samp:`={n}` is, that
  version is used for compatibility aliases.  If this option is provided
  along with :option:`-Wabi` (without the version), the version from this
  option is used for the warning.

.. option:: -fno-access-control, -faccess-control

  Turn off all access checking.  This switch is mainly useful for working
  around bugs in the access control code.

.. option:: -faligned-new

  Enable support for C++17 ``new`` of types that require more
  alignment than ``void* ::operator new(std::size_t)`` provides.  A
  numeric argument such as ``-faligned-new=32`` can be used to
  specify how much alignment (in bytes) is provided by that function,
  but few users will need to override the default of
  ``alignof(std::max_align_t)``.

  This flag is enabled by default for :option:`-std`:samp:`=c++17`.

.. option:: -fchar8_t, -fno-char8_t

  Enable support for ``char8_t`` as adopted for C++20.  This includes
  the addition of a new ``char8_t`` fundamental type, changes to the
  types of UTF-8 string and character literals, new signatures for
  user-defined literals, associated standard library updates, and new
  ``__cpp_char8_t`` and ``__cpp_lib_char8_t`` feature test macros.

  This option enables functions to be overloaded for ordinary and UTF-8
  strings:

  .. code-block:: c++

    int f(const char *);    // #1
    int f(const char8_t *); // #2
    int v1 = f("text");     // Calls #1
    int v2 = f(u8"text");   // Calls #2

  and introduces new signatures for user-defined literals:

  .. code-block:: c++

    int operator""_udl1(char8_t);
    int v3 = u8'x'_udl1;
    int operator""_udl2(const char8_t*, std::size_t);
    int v4 = u8"text"_udl2;
    template<typename T, T...> int operator""_udl3();
    int v5 = u8"text"_udl3;

  The change to the types of UTF-8 string and character literals introduces
  incompatibilities with ISO C++11 and later standards.  For example, the
  following code is well-formed under ISO C++11, but is ill-formed when
  :option:`-fchar8_t` is specified.

  .. code-block:: c++

    char ca[] = u8"xx";     // error: char-array initialized from wide
                            //        string
    const char *cp = u8"xx";// error: invalid conversion from
                            //        `const char8_t*' to `const char*'
    int f(const char*);
    auto v = f(u8"xx");     // error: invalid conversion from
                            //        `const char8_t*' to `const char*'
    std::string s{u8"xx"};  // error: no matching function for call to
                            //        `std::basic_string<char>::basic_string()'
    using namespace std::literals;
    s = u8"xx"s;            // error: conversion from
                            //        `basic_string<char8_t>' to non-scalar
                            //        type `basic_string<char>' requested

.. option:: -fcheck-new

  Check that the pointer returned by ``operator new`` is non-null
  before attempting to modify the storage allocated.  This check is
  normally unnecessary because the C++ standard specifies that
  ``operator new`` only returns ``0`` if it is declared
  ``throw()``, in which case the compiler always checks the
  return value even without this option.  In all other cases, when
  ``operator new`` has a non-empty exception specification, memory
  exhaustion is signalled by throwing ``std::bad_alloc``.  See also
  :samp:`new (nothrow)`.

.. option:: -fconcepts, -fconcepts-ts

  Below :option:`-std`:samp:`=c++20`, :option:`-fconcepts` enables support for the
  C++ Extensions for Concepts Technical Specification, ISO 19217 (2015).

  With :option:`-std`:samp:`=c++20` and above, Concepts are part of the language
  standard, so :option:`-fconcepts` defaults to on.  But the standard
  specification of Concepts differs significantly from the TS, so some
  constructs that were allowed in the TS but didn't make it into the
  standard can still be enabled by :option:`-fconcepts-ts`.

.. option:: -fconstexpr-depth=n

  Set the maximum nested evaluation depth for C++11 constexpr functions
  to :samp:`{n}`.  A limit is needed to detect endless recursion during
  constant expression evaluation.  The minimum specified by the standard
  is 512.

.. option:: -fconstexpr-cache-depth=n

  Set the maximum level of nested evaluation depth for C++11 constexpr
  functions that will be cached to :samp:`{n}`.  This is a heuristic that
  trades off compilation speed (when the cache avoids repeated
  calculations) against memory consumption (when the cache grows very
  large from highly recursive evaluations).  The default is 8.  Very few
  users are likely to want to adjust it, but if your code does heavy
  constexpr calculations you might want to experiment to find which
  value works best for you.

.. option:: -fconstexpr-loop-limit=n

  Set the maximum number of iterations for a loop in C++14 constexpr functions
  to :samp:`{n}`.  A limit is needed to detect infinite loops during
  constant expression evaluation.  The default is 262144 (1<<18).

.. option:: -fconstexpr-ops-limit=n

  Set the maximum number of operations during a single constexpr evaluation.
  Even when number of iterations of a single loop is limited with the above limit,
  if there are several nested loops and each of them has many iterations but still
  smaller than the above limit, or if in a body of some loop or even outside
  of a loop too many expressions need to be evaluated, the resulting constexpr
  evaluation might take too long.
  The default is 33554432 (1<<25).

.. option:: -fcoroutines

  Enable support for the C++ coroutines extension (experimental).

.. option:: -fno-elide-constructors, -felide-constructors

  The C++ standard allows an implementation to omit creating a temporary
  that is only used to initialize another object of the same type.
  Specifying this option disables that optimization, and forces G++ to
  call the copy constructor in all cases.  This option also causes G++
  to call trivial member functions which otherwise would be expanded inline.

  In C++17, the compiler is required to omit these temporaries, but this
  option still affects trivial member functions.

.. option:: -fno-enforce-eh-specs, -fenforce-eh-specs

  Don't generate code to check for violation of exception specifications
  at run time.  This option violates the C++ standard, but may be useful
  for reducing code size in production builds, much like defining
  ``NDEBUG``.  This does not give user code permission to throw
  exceptions in violation of the exception specifications; the compiler
  still optimizes based on the specifications, so throwing an
  unexpected exception results in undefined behavior at run time.

.. option:: -fextern-tls-init, -fno-extern-tls-init

  The C++11 and OpenMP standards allow ``thread_local`` and
  ``threadprivate`` variables to have dynamic (runtime)
  initialization.  To support this, any use of such a variable goes
  through a wrapper function that performs any necessary initialization.
  When the use and definition of the variable are in the same
  translation unit, this overhead can be optimized away, but when the
  use is in a different translation unit there is significant overhead
  even if the variable doesn't actually need dynamic initialization.  If
  the programmer can be sure that no use of the variable in a
  non-defining TU needs to trigger dynamic initialization (either
  because the variable is statically initialized, or a use of the
  variable in the defining TU will be executed before any uses in
  another TU), they can avoid this overhead with the
  :option:`-fno-extern-tls-init` option.

  On targets that support symbol aliases, the default is
  :option:`-fextern-tls-init`.  On targets that do not support symbol
  aliases, the default is :option:`-fno-extern-tls-init`.

.. option:: -fno-gnu-keywords, -fgnu-keywords

  Do not recognize ``typeof`` as a keyword, so that code can use this
  word as an identifier.  You can use the keyword ``__typeof__`` instead.
  This option is implied by the strict ISO C++ dialects: :option:`-ansi`,
  :option:`-std`:samp:`=c++98`, :option:`-std`:samp:`=c++11`, etc.

.. option:: -fno-implicit-templates, -fimplicit-templates

  Never emit code for non-inline templates that are instantiated
  implicitly (i.e. by use); only emit code for explicit instantiations.
  If you use this option, you must take care to structure your code to
  include all the necessary explicit instantiations to avoid getting
  undefined symbols at link time.
  See :ref:`template-instantiation`, for more information.

.. option:: -fno-implicit-inline-templates, -fimplicit-inline-templates

  Don't emit code for implicit instantiations of inline templates, either.
  The default is to handle inlines differently so that compiles with and
  without optimization need the same set of explicit instantiations.

.. option:: -fno-implement-inlines, -fimplement-inlines

  To save space, do not emit out-of-line copies of inline functions
  controlled by ``#pragma implementation``.  This causes linker
  errors if these functions are not inlined everywhere they are called.

.. option:: -fmodules-ts, -fno-modules-ts

  Enable support for C++20 modules (See :ref:`c++-modules`).  The
  :option:`-fno-modules-ts` is usually not needed, as that is the
  default.  Even though this is a C++20 feature, it is not currently
  implicitly enabled by selecting that standard version.

.. option:: -fmodule-header, -fmodule-header=user, -fmodule-header=system

  Compile a header file to create an importable header unit.

.. option:: -fmodule-implicit-inline

  Member functions defined in their class definitions are not implicitly
  inline for modular code.  This is different to traditional C++
  behavior, for good reasons.  However, it may result in a difficulty
  during code porting.  This option makes such function definitions
  implicitly inline.  It does however generate an ABI incompatibility,
  so you must use it everywhere or nowhere.  (Such definitions outside
  of a named module remain implicitly inline, regardless.)

.. option:: -fno-module-lazy, -fmodule-lazy

  Disable lazy module importing and module mapper creation.

.. option:: -fmodule-mapper=[hostname]:port[?ident]

  .. index:: CXX_MODULE_MAPPER environment variable

  An oracle to query for module name to filename mappings.  If
  unspecified the :envvar:`CXX_MODULE_MAPPER` environment variable is used,
  and if that is unset, an in-process default is provided.

.. option:: -fmodule-only

  Only emit the Compiled Module Interface, inhibiting any object file.

.. option:: -fms-extensions

  Disable Wpedantic warnings about constructs used in MFC, such as implicit
  int and getting a pointer to member function via non-standard syntax.

.. option:: -fnew-inheriting-ctors

  Enable the P0136 adjustment to the semantics of C++11 constructor
  inheritance.  This is part of C++17 but also considered to be a Defect
  Report against C++11 and C++14.  This flag is enabled by default
  unless :option:`-fabi-version`:samp:`=10` or lower is specified.

.. option:: -fnew-ttp-matching

  Enable the P0522 resolution to Core issue 150, template template
  parameters and default arguments: this allows a template with default
  template arguments as an argument for a template template parameter
  with fewer template parameters.  This flag is enabled by default for
  :option:`-std`:samp:`=c++17`.

.. option:: -fno-nonansi-builtins, -fnonansi-builtins

  Disable built-in declarations of functions that are not mandated by
  ANSI/ISO C.  These include ``ffs``, ``alloca``, ``_exit``,
  ``index``, ``bzero``, ``conjf``, and other related functions.

.. option:: -fnothrow-opt

  Treat a ``throw()`` exception specification as if it were a
  ``noexcept`` specification to reduce or eliminate the text size
  overhead relative to a function with no exception specification.  If
  the function has local variables of types with non-trivial
  destructors, the exception specification actually makes the
  function smaller because the EH cleanups for those variables can be
  optimized away.  The semantic effect is that an exception thrown out of
  a function with such an exception specification results in a call
  to ``terminate`` rather than ``unexpected``.

.. option:: -fno-operator-names, -foperator-names

  Do not treat the operator name keywords ``and``, ``bitand``,
  ``bitor``, ``compl``, ``not``, ``or`` and ``xor`` as
  synonyms as keywords.

.. option:: -fno-optional-diags, -foptional-diags

  Disable diagnostics that the standard says a compiler does not need to
  issue.  Currently, the only such diagnostic issued by G++ is the one for
  a name having multiple meanings within a class.

.. option:: -fpermissive

  Downgrade some diagnostics about nonconformant code from errors to
  warnings.  Thus, using :option:`-fpermissive` allows some
  nonconforming code to compile.

.. option:: -fno-pretty-templates, -fpretty-templates

  When an error message refers to a specialization of a function
  template, the compiler normally prints the signature of the
  template followed by the template arguments and any typedefs or
  typenames in the signature (e.g. ``void f(T) [with T = int]``
  rather than ``void f(int)`` ) so that it's clear which template is
  involved.  When an error message refers to a specialization of a class
  template, the compiler omits any template arguments that match
  the default template arguments for that template.  If either of these
  behaviors make it harder to understand the error message rather than
  easier, you can use :option:`-fno-pretty-templates` to disable them.

.. option:: -fno-rtti, -frtti

  Disable generation of information about every class with virtual
  functions for use by the C++ run-time type identification features
  ( ``dynamic_cast`` and ``typeid`` ).  If you don't use those parts
  of the language, you can save some space by using this flag.  Note that
  exception handling uses the same information, but G++ generates it as
  needed. The ``dynamic_cast`` operator can still be used for casts that
  do not require run-time type information, i.e. casts to ``void *`` or to
  unambiguous base classes.

  Mixing code compiled with :option:`-frtti` with that compiled with
  :option:`-fno-rtti` may not work.  For example, programs may
  fail to link if a class compiled with :option:`-fno-rtti` is used as a base 
  for a class compiled with :option:`-frtti`.  

.. option:: -fsized-deallocation

  Enable the built-in global declarations

  .. code-block:: c++

    void operator delete (void *, std::size_t) noexcept;
    void operator delete[] (void *, std::size_t) noexcept;

  as introduced in C++14.  This is useful for user-defined replacement
  deallocation functions that, for example, use the size of the object
  to make deallocation faster.  Enabled by default under
  :option:`-std`:samp:`=c++14` and above.  The flag :option:`-Wsized-deallocation`
  warns about places that might want to add a definition.

.. option:: -fstrict-enums

  Allow the compiler to optimize using the assumption that a value of
  enumerated type can only be one of the values of the enumeration (as
  defined in the C++ standard; basically, a value that can be
  represented in the minimum number of bits needed to represent all the
  enumerators).  This assumption may not be valid if the program uses a
  cast to convert an arbitrary integer value to the enumerated type.

.. option:: -fstrong-eval-order

  Evaluate member access, array subscripting, and shift expressions in
  left-to-right order, and evaluate assignment in right-to-left order,
  as adopted for C++17.  Enabled by default with :option:`-std`:samp:`=c++17`.
  :option:`-fstrong-eval-order`:samp:`=some` enables just the ordering of member
  access and shift expressions, and is the default without
  :option:`-std`:samp:`=c++17`.

.. option:: -ftemplate-backtrace-limit=n

  Set the maximum number of template instantiation notes for a single
  warning or error to :samp:`{n}`.  The default value is 10.

.. option:: -ftemplate-depth=n

  Set the maximum instantiation depth for template classes to :samp:`{n}`.
  A limit on the template instantiation depth is needed to detect
  endless recursions during template class instantiation.  ANSI/ISO C++
  conforming programs must not rely on a maximum depth greater than 17
  (changed to 1024 in C++11).  The default value is 900, as the compiler
  can run out of stack space before hitting 1024 in some situations.

.. option:: -fno-threadsafe-statics, -fthreadsafe-statics

  Do not emit the extra code to use the routines specified in the C++
  ABI for thread-safe initialization of local statics.  You can use this
  option to reduce code size slightly in code that doesn't need to be
  thread-safe.

.. option:: -fuse-cxa-atexit

  Register destructors for objects with static storage duration with the
  ``__cxa_atexit`` function rather than the ``atexit`` function.
  This option is required for fully standards-compliant handling of static
  destructors, but only works if your C library supports
  ``__cxa_atexit``.

.. option:: -fno-use-cxa-get-exception-ptr, -fuse-cxa-get-exception-ptr

  Don't use the ``__cxa_get_exception_ptr`` runtime routine.  This
  causes ``std::uncaught_exception`` to be incorrect, but is necessary
  if the runtime routine is not available.

.. option:: -fvisibility-inlines-hidden

  This switch declares that the user does not attempt to compare
  pointers to inline functions or methods where the addresses of the two functions
  are taken in different shared objects.

  The effect of this is that GCC may, effectively, mark inline methods with
  ``__attribute__ ((visibility ("hidden")))`` so that they do not
  appear in the export table of a DSO and do not require a PLT indirection
  when used within the DSO.  Enabling this option can have a dramatic effect
  on load and link times of a DSO as it massively reduces the size of the
  dynamic export table when the library makes heavy use of templates.

  The behavior of this switch is not quite the same as marking the
  methods as hidden directly, because it does not affect static variables
  local to the function or cause the compiler to deduce that
  the function is defined in only one shared object.

  You may mark a method as having a visibility explicitly to negate the
  effect of the switch for that method.  For example, if you do want to
  compare pointers to a particular inline method, you might mark it as
  having default visibility.  Marking the enclosing class with explicit
  visibility has no effect.

  Explicitly instantiated inline methods are unaffected by this option
  as their linkage might otherwise cross a shared library boundary.
  See :ref:`template-instantiation`.

.. option:: -fvisibility-ms-compat

  This flag attempts to use visibility settings to make GCC's C++
  linkage model compatible with that of Microsoft Visual Studio.

  The flag makes these changes to GCC's linkage model:

  * It sets the default visibility to ``hidden``, like
    :option:`-fvisibility`:samp:`=hidden`.

  * Types, but not their members, are not hidden by default.

  * The One Definition Rule is relaxed for types without explicit
    visibility specifications that are defined in more than one
    shared object: those declarations are permitted if they are
    permitted when this option is not used.

  In new code it is better to use :option:`-fvisibility`:samp:`=hidden` and
  export those classes that are intended to be externally visible.
  Unfortunately it is possible for code to rely, perhaps accidentally,
  on the Visual Studio behavior.

  Among the consequences of these changes are that static data members
  of the same type with the same name but defined in different shared
  objects are different, so changing one does not change the other;
  and that pointers to function members defined in different shared
  objects may not compare equal.  When this flag is given, it is a
  violation of the ODR to define types with the same name differently.

.. option:: -fno-weak, -fweak

  Do not use weak symbol support, even if it is provided by the linker.
  By default, G++ uses weak symbols if they are available.  This
  option exists only for testing, and should not be used by end-users;
  it results in inferior code and has no benefits.  This option may
  be removed in a future release of G++.

.. option:: -fext-numeric-literals, -fno-ext-numeric-literals

  .. note::

    C++ and Objective-C++ only

  Accept imaginary, fixed-point, or machine-defined
  literal number suffixes as GNU extensions.
  When this option is turned off these suffixes are treated
  as C++11 user-defined literal numeric suffixes.
  This is on by default for all pre-C++11 dialects and all GNU dialects:
  :option:`-std`:samp:`=c++98`, :option:`-std`:samp:`=gnu++98`, :option:`-std`:samp:`=gnu++11`,
  :option:`-std`:samp:`=gnu++14`.
  This option is off by default
  for ISO C++11 onwards ( :option:`-std`:samp:`=c++11`, ...).

.. option:: -nostdinc++

  Do not search for header files in the standard directories specific to
  C++, but do still search the other standard directories.  (This option
  is used when building the C++ library.)

.. option:: -flang-info-include-translate, -flang-info-include-translate-not, -flang-info-include-translate=header

  Inform of include translation events.  The first will note accepted
  include translations, the second will note declined include
  translations.  The :samp:`{header}` form will inform of include
  translations relating to that specific header.  If :samp:`{header}` is of
  the form ``"user"`` or ``<system>`` it will be resolved to a
  specific user or system header using the include path.

.. option:: -flang-info-module-cmi, -flang-info-module-cmi=module

  Inform of Compiled Module Interface pathnames.  The first will note
  all read CMI pathnames.  The :samp:`{module}` form will not reading a
  specific module's CMI.  :samp:`{module}` may be a named module or a
  header-unit (the latter indicated by either being a pathname containing
  directory separators or enclosed in ``<>`` or ``""`` ).

.. option:: -stdlib=libstdc++,libc++

  When G++ is configured to support this option, it allows specification of
  alternate C++ runtime libraries.  Two options are available: :samp:`{libstdc++}`
  (the default, native C++ runtime for G++) and :samp:`{libc++}` which is the
  C++ runtime installed on some operating systems (e.g. Darwin versions from
  Darwin11 onwards).  The option switches G++ to use the headers from the
  specified library and to emit ``-lstdc++`` or ``-lc++`` respectively,
  when a C++ runtime is required for linking.

In addition, these warning options have meanings only for C++ programs:

.. option:: -Wabi-tag

  .. note::

    C++ and Objective-C++ only

  Warn when a type with an ABI tag is used in a context that does not
  have that ABI tag.  See C++ Attributes for more information
  about ABI tags.

.. option:: -Wcomma-subscript, -Wno-comma-subscript

  .. note::

    C++ and Objective-C++ only

  Warn about uses of a comma expression within a subscripting expression.
  This usage was deprecated in C++20.  However, a comma expression wrapped
  in ``( )`` is not deprecated.  Example:

  .. code-block:: c++

    void f(int *a, int b, int c) {
        a[b,c];     // deprecated
        a[(b,c)];   // OK
    }

  Enabled by default with :option:`-std`:samp:`=c++20`.

.. option:: -Wctad-maybe-unsupported, -Wno-ctad-maybe-unsupported

  .. note::

    C++ and Objective-C++ only

  Warn when performing class template argument deduction (CTAD) on a type with
  no explicitly written deduction guides.  This warning will point out cases
  where CTAD succeeded only because the compiler synthesized the implicit
  deduction guides, which might not be what the programmer intended.  Certain
  style guides allow CTAD only on types that specifically "opt-in"; i.e., on
  types that are designed to support CTAD.  This warning can be suppressed with
  the following pattern:

  .. code-block:: c++

    struct allow_ctad_t; // any name works
    template <typename T> struct S {
      S(T) { }
    };
    S(allow_ctad_t) -> S<void>; // guide with incomplete parameter type will never be considered

.. option:: -Wctor-dtor-privacy, -Wno-ctor-dtor-privacy

  .. note::

    C++ and Objective-C++ only

  Warn when a class seems unusable because all the constructors or
  destructors in that class are private, and it has neither friends nor
  public static member functions.  Also warn if there are no non-private
  methods, and there's at least one private member function that isn't
  a constructor or destructor.

.. option:: -Wdelete-non-virtual-dtor, -Wno-delete-non-virtual-dtor

  .. note::

    C++ and Objective-C++ only

  Warn when ``delete`` is used to destroy an instance of a class that
  has virtual functions and non-virtual destructor. It is unsafe to delete
  an instance of a derived class through a pointer to a base class if the
  base class does not have a virtual destructor.  This warning is enabled
  by :option:`-Wall`.

.. option:: -Wdeprecated-copy, -Wno-deprecated-copy

  .. note::

    C++ and Objective-C++ only

  Warn that the implicit declaration of a copy constructor or copy
  assignment operator is deprecated if the class has a user-provided
  copy constructor or copy assignment operator, in C++11 and up.  This
  warning is enabled by :option:`-Wextra`.  With
  :option:`-Wdeprecated-copy-dtor`, also deprecate if the class has a
  user-provided destructor.

.. option:: -Wno-deprecated-enum-enum-conversion, -Wdeprecated-enum-enum-conversion

  .. note::

    C++ and Objective-C++ only

  Disable the warning about the case when the usual arithmetic conversions
  are applied on operands where one is of enumeration type and the other is
  of a different enumeration type.  This conversion was deprecated in C++20.
  For example:

  .. code-block:: c++

    enum E1 { e };
    enum E2 { f };
    int k = f - e;

  :option:`-Wdeprecated-enum-enum-conversion` is enabled by default with
  :option:`-std`:samp:`=c++20`.  In pre-C++20 dialects, this warning can be enabled
  by :option:`-Wenum-conversion`.

.. option:: -Wno-deprecated-enum-float-conversion, -Wdeprecated-enum-float-conversion

  .. note::

    C++ and Objective-C++ only

  Disable the warning about the case when the usual arithmetic conversions
  are applied on operands where one is of enumeration type and the other is
  of a floating-point type.  This conversion was deprecated in C++20.  For
  example:

  .. code-block:: c++

    enum E1 { e };
    enum E2 { f };
    bool b = e <= 3.7;

  :option:`-Wdeprecated-enum-float-conversion` is enabled by default with
  :option:`-std`:samp:`=c++20`.  In pre-C++20 dialects, this warning can be enabled
  by :option:`-Wenum-conversion`.

.. option:: -Wno-init-list-lifetime, -Winit-list-lifetime

  .. note::

    C++ and Objective-C++ only

  Do not warn about uses of ``std::initializer_list`` that are likely
  to result in dangling pointers.  Since the underlying array for an
  ``initializer_list`` is handled like a normal C++ temporary object,
  it is easy to inadvertently keep a pointer to the array past the end
  of the array's lifetime.  For example:

  * If a function returns a temporary ``initializer_list``, or a local
    ``initializer_list`` variable, the array's lifetime ends at the end
    of the return statement, so the value returned has a dangling pointer.

  * If a new-expression creates an ``initializer_list``, the array only
    lives until the end of the enclosing full-expression, so the
    ``initializer_list`` in the heap has a dangling pointer.

  * When an ``initializer_list`` variable is assigned from a
    brace-enclosed initializer list, the temporary array created for the
    right side of the assignment only lives until the end of the
    full-expression, so at the next statement the ``initializer_list``
    variable has a dangling pointer.

    .. code-block:: c++

      // li's initial underlying array lives as long as li
      std::initializer_list<int> li = { 1,2,3 };
      // assignment changes li to point to a temporary array
      li = { 4, 5 };
      // now the temporary is gone and li has a dangling pointer
      int i = li.begin()[0] // undefined behavior

  * When a list constructor stores the ``begin`` pointer from the
    ``initializer_list`` argument, this doesn't extend the lifetime of
    the array, so if a class variable is constructed from a temporary
    ``initializer_list``, the pointer is left dangling by the end of
    the variable declaration statement.

.. option:: -Winvalid-imported-macros, -Wno-invalid-imported-macros

  Verify all imported macro definitions are valid at the end of
  compilation.  This is not enabled by default, as it requires
  additional processing to determine.  It may be useful when preparing
  sets of header-units to ensure consistent macros.

.. option:: -Wno-literal-suffix, -Wliteral-suffix

  .. note::

    C++ and Objective-C++ only

  Do not warn when a string or character literal is followed by a
  ud-suffix which does not begin with an underscore.  As a conforming
  extension, GCC treats such suffixes as separate preprocessing tokens
  in order to maintain backwards compatibility with code that uses
  formatting macros from ``<inttypes.h>``.  For example:

  .. code-block:: c++

    #define __STDC_FORMAT_MACROS
    #include <inttypes.h>
    #include <stdio.h>

    int main() {
      int64_t i64 = 123;
      printf("My int64: %" PRId64"\n", i64);
    }

  In this case, ``PRId64`` is treated as a separate preprocessing token.

  This option also controls warnings when a user-defined literal
  operator is declared with a literal suffix identifier that doesn't
  begin with an underscore. Literal suffix identifiers that don't begin
  with an underscore are reserved for future standardization.

  These warnings are enabled by default.

.. option:: -Wno-narrowing, -Wnarrowing

  .. note::

    C++ and Objective-C++ only

  For C++11 and later standards, narrowing conversions are diagnosed by default,
  as required by the standard.  A narrowing conversion from a constant produces
  an error, and a narrowing conversion from a non-constant produces a warning,
  but :option:`-Wno-narrowing` suppresses the diagnostic.
  Note that this does not affect the meaning of well-formed code;
  narrowing conversions are still considered ill-formed in SFINAE contexts.

  With :option:`-Wnarrowing` in C++98, warn when a narrowing
  conversion prohibited by C++11 occurs within
  :samp:`{ }`, e.g.

  .. code-block:: c++

    int i = { 2.2 }; // error: narrowing from double to int

  This flag is included in :option:`-Wall` and :option:`-Wc++11-compat`.

.. option:: -Wnoexcept, -Wno-noexcept

  .. note::

    C++ and Objective-C++ only

  Warn when a noexcept-expression evaluates to false because of a call
  to a function that does not have a non-throwing exception
  specification (i.e. ``throw()`` or ``noexcept`` ) but is known by
  the compiler to never throw an exception.

.. option:: -Wnoexcept-type, -Wno-noexcept-type

  .. note::

    C++ and Objective-C++ only

  Warn if the C++17 feature making ``noexcept`` part of a function
  type changes the mangled name of a symbol relative to C++14.  Enabled
  by :option:`-Wabi` and :option:`-Wc++17-compat`.

  As an example:

  .. code-block:: c++

    template <class T> void f(T t) { t(); };
    void g() noexcept;
    void h() { f(g); } 

  In C++14, ``f`` calls ``f<void(*)()>``, but in
  C++17 it calls ``f<void(*)()noexcept>``.

.. option:: -Wclass-memaccess, -Wno-class-memaccess

  .. note::

    C++ and Objective-C++ only

  Warn when the destination of a call to a raw memory function such as
  ``memset`` or ``memcpy`` is an object of class type, and when writing
  into such an object might bypass the class non-trivial or deleted constructor
  or copy assignment, violate const-correctness or encapsulation, or corrupt
  virtual table pointers.  Modifying the representation of such objects may
  violate invariants maintained by member functions of the class.  For example,
  the call to ``memset`` below is undefined because it modifies a non-trivial
  class object and is, therefore, diagnosed.  The safe way to either initialize
  or clear the storage of objects of such types is by using the appropriate
  constructor or assignment operator, if one is available.

  .. code-block:: c++

    std::string str = "abc";
    memset (&str, 0, sizeof str);

  The :option:`-Wclass-memaccess` option is enabled by :option:`-Wall`.
  Explicitly casting the pointer to the class object to ``void *`` or
  to a type that can be safely accessed by the raw memory function suppresses
  the warning.

.. option:: -Wnon-virtual-dtor, -Wno-non-virtual-dtor

  .. note::

    C++ and Objective-C++ only

  Warn when a class has virtual functions and an accessible non-virtual
  destructor itself or in an accessible polymorphic base class, in which
  case it is possible but unsafe to delete an instance of a derived
  class through a pointer to the class itself or base class.  This
  warning is automatically enabled if :option:`-Weffc++` is specified.

.. option:: -Wregister, -Wno-register

  .. note::

    C++ and Objective-C++ only

  Warn on uses of the ``register`` storage class specifier, except
  when it is part of the GNU Explicit Register Variables extension.
  The use of the ``register`` keyword as storage class specifier has
  been deprecated in C++11 and removed in C++17.
  Enabled by default with :option:`-std`:samp:`=c++17`.

.. option:: -Wreorder, -Wno-reorder

  .. note::

    C++ and Objective-C++ only

  .. index:: reordering, warning

  .. index:: warning for reordering of member initializers

  Warn when the order of member initializers given in the code does not
  match the order in which they must be executed.  For instance:

  .. code-block:: c++

    struct A {
      int i;
      int j;
      A(): j (0), i (1) { }
    };

  The compiler rearranges the member initializers for ``i``
  and ``j`` to match the declaration order of the members, emitting
  a warning to that effect.  This warning is enabled by :option:`-Wall`.

.. option:: -Wno-pessimizing-move, -Wpessimizing-move

  .. note::

    C++ and Objective-C++ only

  This warning warns when a call to ``std::move`` prevents copy
  elision.  A typical scenario when copy elision can occur is when returning in
  a function with a class return type, when the expression being returned is the
  name of a non-volatile automatic object, and is not a function parameter, and
  has the same type as the function return type.

  .. code-block:: c++

    struct T {
    ...
    };
    T fn()
    {
      T t;
      ...
      return std::move (t);
    }

  But in this example, the ``std::move`` call prevents copy elision.

  This warning is enabled by :option:`-Wall`.

.. option:: -Wno-redundant-move, -Wredundant-move

  .. note::

    C++ and Objective-C++ only

  This warning warns about redundant calls to ``std::move`` ; that is, when
  a move operation would have been performed even without the ``std::move``
  call.  This happens because the compiler is forced to treat the object as if
  it were an rvalue in certain situations such as returning a local variable,
  where copy elision isn't applicable.  Consider:

  .. code-block:: c++

    struct T {
    ...
    };
    T fn(T t)
    {
      ...
      return std::move (t);
    }

  Here, the ``std::move`` call is redundant.  Because G++ implements Core
  Issue 1579, another example is:

  .. code-block:: c++

    struct T { // convertible to U
    ...
    };
    struct U {
    ...
    };
    U fn()
    {
      T t;
      ...
      return std::move (t);
    }

  In this example, copy elision isn't applicable because the type of the
  expression being returned and the function return type differ, yet G++
  treats the return value as if it were designated by an rvalue.

  This warning is enabled by :option:`-Wextra`.

.. option:: -Wrange-loop-construct, -Wno-range-loop-construct

  .. note::

    C++ and Objective-C++ only

  This warning warns when a C++ range-based for-loop is creating an unnecessary
  copy.  This can happen when the range declaration is not a reference, but
  probably should be.  For example:

  .. code-block:: c++

    struct S { char arr[128]; };
    void fn () {
      S arr[5];
      for (const auto x : arr) { ... }
    }

  It does not warn when the type being copied is a trivially-copyable type whose
  size is less than 64 bytes.

  This warning also warns when a loop variable in a range-based for-loop is
  initialized with a value of a different type resulting in a copy.  For example:

  .. code-block:: c++

    void fn() {
      int arr[10];
      for (const double &x : arr) { ... }
    }

  In the example above, in every iteration of the loop a temporary value of
  type ``double`` is created and destroyed, to which the reference
  ``const double &`` is bound.

  This warning is enabled by :option:`-Wall`.

.. option:: -Wredundant-tags, -Wno-redundant-tags

  .. note::

    C++ and Objective-C++ only

  Warn about redundant class-key and enum-key in references to class types
  and enumerated types in contexts where the key can be eliminated without
  causing an ambiguity.  For example:

  .. code-block:: c++

    struct foo;
    struct foo *p;   // warn that keyword struct can be eliminated

  On the other hand, in this example there is no warning:

  .. code-block:: c++

    struct foo;
    void foo ();   // "hides" struct foo
    void bar (struct foo&);  // no warning, keyword struct is necessary

.. option:: -Wno-subobject-linkage, -Wsubobject-linkage

  .. note::

    C++ and Objective-C++ only

  Do not warn
  if a class type has a base or a field whose type uses the anonymous
  namespace or depends on a type with no linkage.  If a type A depends on
  a type B with no or internal linkage, defining it in multiple
  translation units would be an ODR violation because the meaning of B
  is different in each translation unit.  If A only appears in a single
  translation unit, the best way to silence the warning is to give it
  internal linkage by putting it in an anonymous namespace as well.  The
  compiler doesn't give this warning for types defined in the main .C
  file, as those are unlikely to have multiple definitions.
  :option:`-Wsubobject-linkage` is enabled by default.

.. option:: -Weffc++, -Wno-effc++

  .. note::

    C++ and Objective-C++ only

  Warn about violations of the following style guidelines from Scott Meyers'
  Effective C++ series of books:

  * Define a copy constructor and an assignment operator for classes
    with dynamically-allocated memory.

  * Prefer initialization to assignment in constructors.

  * Have ``operator=`` return a reference to ``*this``.

  * Don't try to return a reference when you must return an object.

  * Distinguish between prefix and postfix forms of increment and
    decrement operators.

  * Never overload ``&&``, ``||``, or ``,``.

  This option also enables :option:`-Wnon-virtual-dtor`, which is also
  one of the effective C++ recommendations.  However, the check is
  extended to warn about the lack of virtual destructor in accessible
  non-polymorphic bases classes too.

  When selecting this option, be aware that the standard library
  headers do not obey all of these guidelines; use :samp:`grep -v`
  to filter out those warnings.

.. option:: -Wno-exceptions, -Wexceptions

  .. note::

    C++ and Objective-C++ only

  Disable the warning about the case when an exception handler is shadowed by
  another handler, which can point out a wrong ordering of exception handlers.

.. option:: -Wstrict-null-sentinel, -Wno-strict-null-sentinel

  .. note::

    C++ and Objective-C++ only

  Warn about the use of an uncasted ``NULL`` as sentinel.  When
  compiling only with GCC this is a valid sentinel, as ``NULL`` is defined
  to ``__null``.  Although it is a null pointer constant rather than a
  null pointer, it is guaranteed to be of the same size as a pointer.
  But this use is not portable across different compilers.

.. option:: -Wno-non-template-friend, -Wnon-template-friend

  .. note::

    C++ and Objective-C++ only

  Disable warnings when non-template friend functions are declared
  within a template.  In very old versions of GCC that predate implementation
  of the ISO standard, declarations such as 
  :samp:`friend int foo(int)`, where the name of the friend is an unqualified-id,
  could be interpreted as a particular specialization of a template
  function; the warning exists to diagnose compatibility problems, 
  and is enabled by default.

.. option:: -Wold-style-cast, -Wno-old-style-cast

  .. note::

    C++ and Objective-C++ only

  Warn if an old-style (C-style) cast to a non-void type is used within
  a C++ program.  The new-style casts ( ``dynamic_cast``,
  ``static_cast``, ``reinterpret_cast``, and ``const_cast`` ) are
  less vulnerable to unintended effects and much easier to search for.

.. option:: -Woverloaded-virtual, -Wno-overloaded-virtual

  .. note::

    C++ and Objective-C++ only

  .. index:: overloaded virtual function, warning

  .. index:: warning for overloaded virtual function

  Warn when a function declaration hides virtual functions from a
  base class.  For example, in:

  .. code-block:: c++

    struct A {
      virtual void f();
    };

    struct B: public A {
      void f(int);
    };

  the ``A`` class version of ``f`` is hidden in ``B``, and code
  like:

  .. code-block:: c++

    B* b;
    b->f();

  fails to compile.

.. option:: -Wno-pmf-conversions, -Wpmf-conversions

  .. note::

    C++ and Objective-C++ only

  Disable the diagnostic for converting a bound pointer to member function
  to a plain pointer.

.. option:: -Wsign-promo, -Wno-sign-promo

  .. note::

    C++ and Objective-C++ only

  Warn when overload resolution chooses a promotion from unsigned or
  enumerated type to a signed type, over a conversion to an unsigned type of
  the same size.  Previous versions of G++ tried to preserve
  unsignedness, but the standard mandates the current behavior.

.. option:: -Wtemplates, -Wno-templates

  .. note::

    C++ and Objective-C++ only

  Warn when a primary template declaration is encountered.  Some coding
  rules disallow templates, and this may be used to enforce that rule.
  The warning is inactive inside a system header file, such as the STL, so
  one can still use the STL.  One may also instantiate or specialize
  templates.

.. option:: -Wno-mismatched-new-delete, -Wmismatched-new-delete

  .. note::

    C++ and Objective-C++ only

  Warn for mismatches between calls to ``operator new`` or ``operator
  delete`` and the corresponding call to the allocation or deallocation function.
  This includes invocations of C++ ``operator delete`` with pointers
  returned from either mismatched forms of ``operator new``, or from other
  functions that allocate objects for which the ``operator delete`` isn't
  a suitable deallocator, as well as calls to other deallocation functions
  with pointers returned from ``operator new`` for which the deallocation
  function isn't suitable.

  For example, the ``delete`` expression in the function below is diagnosed
  because it doesn't match the array form of the ``new`` expression
  the pointer argument was returned from.  Similarly, the call to ``free``
  is also diagnosed.

  .. code-block:: c++

    void f ()
    {
      int *a = new int[n];
      delete a;   // warning: mismatch in array forms of expressions

      char *p = new char[n];
      free (p);   // warning: mismatch between new and free
    }

  The related option :option:`-Wmismatched-dealloc` diagnoses mismatches
  involving allocation and deallocation functions other than ``operator
  new`` and ``operator delete``.

  :option:`-Wmismatched-new-delete` is enabled by default.

.. option:: -Wmismatched-tags, -Wno-mismatched-tags

  .. note::

    C++ and Objective-C++ only

  Warn for declarations of structs, classes, and class templates and their
  specializations with a class-key that does not match either the definition
  or the first declaration if no definition is provided.

  For example, the declaration of ``struct Object`` in the argument list
  of ``draw`` triggers the warning.  To avoid it, either remove the redundant
  class-key ``struct`` or replace it with ``class`` to match its definition.

  .. code-block:: c++

    class Object {
    public:
      virtual ~Object () = 0;
    };
    void draw (struct Object*);

  It is not wrong to declare a class with the class-key ``struct`` as
  the example above shows.  The :option:`-Wmismatched-tags` option is intended
  to help achieve a consistent style of class declarations.  In code that is
  intended to be portable to Windows-based compilers the warning helps prevent
  unresolved references due to the difference in the mangling of symbols
  declared with different class-keys.  The option can be used either on its
  own or in conjunction with :option:`-Wredundant-tags`.

.. option:: -Wmultiple-inheritance, -Wno-multiple-inheritance

  .. note::

    C++ and Objective-C++ only

  Warn when a class is defined with multiple direct base classes.  Some
  coding rules disallow multiple inheritance, and this may be used to
  enforce that rule.  The warning is inactive inside a system header file,
  such as the STL, so one can still use the STL.  One may also define
  classes that indirectly use multiple inheritance.

.. option:: -Wvirtual-inheritance, -Wno-virtual-inheritance

  Warn when a class is defined with a virtual direct base class.  Some
  coding rules disallow multiple inheritance, and this may be used to
  enforce that rule.  The warning is inactive inside a system header file,
  such as the STL, so one can still use the STL.  One may also define
  classes that indirectly use virtual inheritance.

.. option:: -Wno-virtual-move-assign, -Wvirtual-move-assign

  Suppress warnings about inheriting from a virtual base with a
  non-trivial C++11 move assignment operator.  This is dangerous because
  if the virtual base is reachable along more than one path, it is
  moved multiple times, which can mean both objects end up in the
  moved-from state.  If the move assignment operator is written to avoid
  moving from a moved-from object, this warning can be disabled.

.. option:: -Wnamespaces, -Wno-namespaces

  Warn when a namespace definition is opened.  Some coding rules disallow
  namespaces, and this may be used to enforce that rule.  The warning is
  inactive inside a system header file, such as the STL, so one can still
  use the STL.  One may also use using directives and qualified names.

.. option:: -Wno-terminate, -Wterminate

  .. note::

    C++ and Objective-C++ only

  Disable the warning about a throw-expression that will immediately
  result in a call to ``terminate``.

.. option:: -Wno-vexing-parse, -Wvexing-parse

  .. note::

    C++ and Objective-C++ only

  Warn about the most vexing parse syntactic ambiguity.  This warns about
  the cases when a declaration looks like a variable definition, but the
  C++ language requires it to be interpreted as a function declaration.
  For instance:

  .. code-block:: c++

    void f(double a) {
      int i();        // extern int i (void);
      int n(int(a));  // extern int n (int);
    }

  Another example:

  .. code-block:: c++

    struct S { S(int); };
    void f(double a) {
      S x(int(a));   // extern struct S x (int);
      S y(int());    // extern struct S y (int (*) (void));
      S z();         // extern struct S z (void);
    }

  The warning will suggest options how to deal with such an ambiguity; e.g.,
  it can suggest removing the parentheses or using braces instead.

  This warning is enabled by default.

.. option:: -Wno-class-conversion, -Wclass-conversion

  .. note::

    C++ and Objective-C++ only

  Do not warn when a conversion function converts an
  object to the same type, to a base class of that type, or to void; such
  a conversion function will never be called.

.. option:: -Wvolatile, -Wno-volatile

  .. note::

    C++ and Objective-C++ only

  Warn about deprecated uses of the ``volatile`` qualifier.  This includes
  postfix and prefix ``++`` and ``--`` expressions of
  ``volatile`` -qualified types, using simple assignments where the left
  operand is a ``volatile`` -qualified non-class type for their value,
  compound assignments where the left operand is a ``volatile`` -qualified
  non-class type, ``volatile`` -qualified function return type,
  ``volatile`` -qualified parameter type, and structured bindings of a
  ``volatile`` -qualified type.  This usage was deprecated in C++20.

  Enabled by default with :option:`-std`:samp:`=c++20`.

.. option:: -Wzero-as-null-pointer-constant, -Wno-zero-as-null-pointer-constant

  .. note::

    C++ and Objective-C++ only

  Warn when a literal :samp:`0` is used as null pointer constant.  This can
  be useful to facilitate the conversion to ``nullptr`` in C++11.

.. option:: -Waligned-new, -Wno-aligned-new

  Warn about a new-expression of a type that requires greater alignment
  than the ``alignof(std::max_align_t)`` but uses an allocation
  function without an explicit alignment parameter. This option is
  enabled by :option:`-Wall`.

  Normally this only warns about global allocation functions, but
  :option:`-Waligned-new`:samp:`=all` also warns about class member allocation
  functions.

.. option:: -Wno-placement-new, -Wplacement-new=n, -Wplacement-new

  Warn about placement new expressions with undefined behavior, such as
  constructing an object in a buffer that is smaller than the type of
  the object.  For example, the placement new expression below is diagnosed
  because it attempts to construct an array of 64 integers in a buffer only
  64 bytes large.

  .. code-block:: c++

    char buf [64];
    new (buf) int[64];

  This warning is enabled by default.

  ``-Wplacement-new=1``
    This is the default warning level of :option:`-Wplacement-new`.  At this
    level the warning is not issued for some strictly undefined constructs that
    GCC allows as extensions for compatibility with legacy code.  For example,
    the following ``new`` expression is not diagnosed at this level even
    though it has undefined behavior according to the C++ standard because
    it writes past the end of the one-element array.

    .. code-block:: c++

      struct S { int n, a[1]; };
      S *s = (S *)malloc (sizeof *s + 31 * sizeof s->a[0]);
      new (s->a)int [32]();

  ``-Wplacement-new=2``
    At this level, in addition to diagnosing all the same constructs as at level
    1, a diagnostic is also issued for placement new expressions that construct
    an object in the last member of structure whose type is an array of a single
    element and whose size is less than the size of the object being constructed.
    While the previous example would be diagnosed, the following construct makes
    use of the flexible member array extension to avoid the warning at level 2.

    .. code-block:: c++

      struct S { int n, a[]; };
      S *s = (S *)malloc (sizeof *s + 32 * sizeof s->a[0]);
      new (s->a)int [32]();

.. option:: -Wcatch-value, -Wcatch-value=n(C++ and Objective-C++ only), -Wno-catch-value

  Warn about catch handlers that do not catch via reference.
  With :option:`-Wcatch-value`:samp:`=1` (or :option:`-Wcatch-value` for short)
  warn about polymorphic class types that are caught by value.
  With :option:`-Wcatch-value`:samp:`=2` warn about all class types that are caught
  by value. With :option:`-Wcatch-value`:samp:`=3` warn about all types that are
  not caught by reference. :option:`-Wcatch-value` is enabled by :option:`-Wall`.

.. option:: -Wconditionally-supported, -Wno-conditionally-supported

  .. note::

    C++ and Objective-C++ only

  Warn for conditionally-supported (C++11 [intro.defs]) constructs.

.. option:: -Wno-delete-incomplete, -Wdelete-incomplete

  .. note::

    C++ and Objective-C++ only

  Do not warn when deleting a pointer to incomplete type, which may cause
  undefined behavior at runtime.  This warning is enabled by default.

.. option:: -Wextra-semi, -Wno-extra-semi

  .. note::

    C++, Objective-C++ only

  Warn about redundant semicolons after in-class function definitions.

.. option:: -Wno-inaccessible-base, -Winaccessible-base

  .. note::

    C++, Objective-C++ only

  This option controls warnings
  when a base class is inaccessible in a class derived from it due to
  ambiguity.  The warning is enabled by default.
  Note that the warning for ambiguous virtual
  bases is enabled by the :option:`-Wextra` option.

  .. code-block:: c++

    struct A { int a; };

    struct B : A { };

    struct C : B, A { };

.. option:: -Wno-inherited-variadic-ctor, -Winherited-variadic-ctor

  Suppress warnings about use of C++11 inheriting constructors when the
  base class inherited from has a C variadic constructor; the warning is
  on by default because the ellipsis is not inherited.

.. option:: -Wno-invalid-offsetof, -Winvalid-offsetof

  .. note::

    C++ and Objective-C++ only

  Suppress warnings from applying the ``offsetof`` macro to a non-POD
  type.  According to the 2014 ISO C++ standard, applying ``offsetof``
  to a non-standard-layout type is undefined.  In existing C++ implementations,
  however, ``offsetof`` typically gives meaningful results.
  This flag is for users who are aware that they are
  writing nonportable code and who have deliberately chosen to ignore the
  warning about it.

  The restrictions on ``offsetof`` may be relaxed in a future version
  of the C++ standard.

.. option:: -Wsized-deallocation, -Wno-sized-deallocation

  .. note::

    C++ and Objective-C++ only

  Warn about a definition of an unsized deallocation function

  .. code-block:: c++

    void operator delete (void *) noexcept;
    void operator delete[] (void *) noexcept;

  without a definition of the corresponding sized deallocation function

  .. code-block:: c++

    void operator delete (void *, std::size_t) noexcept;
    void operator delete[] (void *, std::size_t) noexcept;

  or vice versa.  Enabled by :option:`-Wextra` along with
  :option:`-fsized-deallocation`.

.. option:: -Wsuggest-final-types, -Wno-suggest-final-types

  Warn about types with virtual methods where code quality would be improved
  if the type were declared with the C++11 ``final`` specifier,
  or, if possible,
  declared in an anonymous namespace. This allows GCC to more aggressively
  devirtualize the polymorphic calls. This warning is more effective with
  link-time optimization,
  where the information about the class hierarchy graph is
  more complete.

.. option:: -Wsuggest-final-methods, -Wno-suggest-final-methods

  Warn about virtual methods where code quality would be improved if the method
  were declared with the C++11 ``final`` specifier,
  or, if possible, its type were
  declared in an anonymous namespace or with the ``final`` specifier.
  This warning is
  more effective with link-time optimization, where the information about the
  class hierarchy graph is more complete. It is recommended to first consider
  suggestions of :option:`-Wsuggest-final-types` and then rebuild with new
  annotations.

.. option:: -Wsuggest-override, -Wno-suggest-override

  Warn about overriding virtual functions that are not marked with the
  ``override`` keyword.

.. option:: -Wuseless-cast, -Wno-useless-cast

  .. note::

    C++ and Objective-C++ only

  Warn when an expression is casted to its own type.

.. option:: -Wno-conversion-null, -Wconversion-null

  .. note::

    C++ and Objective-C++ only

  Do not warn for conversions between ``NULL`` and non-pointer
  types. :option:`-Wconversion-null` is enabled by default.

