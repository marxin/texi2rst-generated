.. _function-attributes:

Declaring Attributes of Functions
*********************************

.. index:: function attributes

.. index:: declaring attributes of functions

.. index:: volatile applied to function

.. index:: const applied to function

In GNU C and C++, you can use function attributes to specify certain
function properties that may help the compiler optimize calls or
check code more carefully for correctness.  For example, you
can use attributes to specify that a function never returns
( ``noreturn`` ), returns a value depending only on the values of
its arguments ( ``const`` ), or has ``printf`` -style arguments
( ``format`` ).

You can also use attributes to control memory placement, code
generation options or call/return conventions within the function
being annotated.  Many of these attributes are target-specific.  For
example, many targets support attributes for defining interrupt
handler functions, which typically must follow special register usage
and return conventions.  Such attributes are described in the subsection
for each target.  However, a considerable number of attributes are
supported by most, if not all targets.  Those are described in
the Common Function Attributes section.

Function attributes are introduced by the ``__attribute__`` keyword
in the declaration of a function, followed by an attribute specification
enclosed in double parentheses.  You can specify multiple attributes in
a declaration by separating them by commas within the double parentheses
or by immediately following one attribute specification with another.
See :ref:`attribute-syntax`, for the exact rules on attribute syntax and
placement.  Compatible attribute specifications on distinct declarations
of the same function are merged.  An attribute specification that is not
compatible with attributes already applied to a declaration of the same
function is ignored with a warning.

Some function attributes take one or more arguments that refer to
the function's parameters by their positions within the function parameter
list.  Such attribute arguments are referred to as :dfn:`positional arguments`.
Unless specified otherwise, positional arguments that specify properties
of parameters with pointer types can also specify the same properties of
the implicit C++ ``this`` argument in non-static member functions, and
of parameters of reference to a pointer type.  For ordinary functions,
position one refers to the first parameter on the list.  In C++ non-static
member functions, position one refers to the implicit ``this`` pointer.
The same restrictions and effects apply to function attributes used with
ordinary functions or C++ member functions.

GCC also supports attributes on
variable declarations (see :ref:`variable-attributes`),
labels (see :ref:`label-attributes`),
enumerators (see :ref:`enumerator-attributes`),
statements (see :ref:`statement-attributes`),
and types (see :ref:`type-attributes`).

There is some overlap between the purposes of attributes and pragmas
(see :ref:`Pragmas Accepted by GCC <pragmas>`).  It has been
found convenient to use ``__attribute__`` to achieve a natural
attachment of attributes to their corresponding declarations, whereas
``#pragma`` is of use for compatibility with other compilers
or constructs that do not naturally form part of the grammar.

In addition to the attributes documented here,
GCC plugins may provide their own attributes.

.. toctree::
  :maxdepth: 2

  common-function-attributes
  aarch64-function-attributes
  amd-gcn-function-attributes
  arc-function-attributes
  arm-function-attributes
  avr-function-attributes
  blackfin-function-attributes
  bpf-function-attributes
  cr16-function-attributes
  c-sky-function-attributes
  epiphany-function-attributes
  h8-300-function-attributes
  ia-64-function-attributes
  m32c-function-attributes
  m32r-d-function-attributes
  m68k-function-attributes
  mcore-function-attributes
  mep-function-attributes
  microblaze-function-attributes
  microsoft-windows-function-attributes
  mips-function-attributes
  msp430-function-attributes
  nds32-function-attributes
  nios-ii-function-attributes
  nvidia-ptx-function-attributes
  powerpc-function-attributes
  risc-v-function-attributes
  rl78-function-attributes
  rx-function-attributes
  s-390-function-attributes
  sh-function-attributes
  symbian-os-function-attributes
  v850-function-attributes
  visium-function-attributes
  x86-function-attributes
  xstormy16-function-attributes

.. _common-function-attributes:

Common Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

The following attributes are supported on most targets.

.. Keep this table alphabetized by attribute name.  Treat _ as space.

.. option:: access

  The ``access`` attribute enables the detection of invalid or unsafe
  accesses by functions to which they apply or their callers, as well as
  write-only accesses to objects that are never read from.  Such accesses
  may be diagnosed by warnings such as :option:`-Wstringop-overflow`,
  :option:`-Wuninitialized`, :option:`-Wunused`, and others.

  The ``access`` attribute specifies that a function to whose by-reference
  arguments the attribute applies accesses the referenced object according to
  :samp:`{access-mode}`.  The :samp:`{access-mode}` argument is required and must be
  one of four names: ``read_only``, ``read_write``, ``write_only``,
  or ``none``.  The remaining two are positional arguments.

  The required :samp:`{ref-index}` positional argument  denotes a function
  argument of pointer (or in C++, reference) type that is subject to
  the access.  The same pointer argument can be referenced by at most one
  distinct ``access`` attribute.

  The optional :samp:`{size-index}` positional argument denotes a function
  argument of integer type that specifies the maximum size of the access.
  The size is the number of elements of the type referenced by :samp:`{ref-index}`,
  or the number of bytes when the pointer type is ``void*``.  When no
  :samp:`{size-index}` argument is specified, the pointer argument must be either
  null or point to a space that is suitably aligned and large for at least one
  object of the referenced type (this implies that a past-the-end pointer is
  not a valid argument).  The actual size of the access may be less but it
  must not be more.

  The ``read_only`` access mode specifies that the pointer to which it
  applies is used to read the referenced object but not write to it.  Unless
  the argument specifying the size of the access denoted by :samp:`{size-index}`
  is zero, the referenced object must be initialized.  The mode implies
  a stronger guarantee than the ``const`` qualifier which, when cast away
  from a pointer, does not prevent the pointed-to object from being modified.
  Examples of the use of the ``read_only`` access mode is the argument to
  the ``puts`` function, or the second and third arguments to
  the ``memcpy`` function.

  .. code-block:: c++

    __attribute__ ((access (read_only, 1))) int puts (const char*);
    __attribute__ ((access (read_only, 2, 3))) void* memcpy (void*, const void*, size_t);

  The ``read_write`` access mode applies to arguments of pointer types
  without the ``const`` qualifier.  It specifies that the pointer to which
  it applies is used to both read and write the referenced object.  Unless
  the argument specifying the size of the access denoted by :samp:`{size-index}`
  is zero, the object referenced by the pointer must be initialized.  An example
  of the use of the ``read_write`` access mode is the first argument to
  the ``strcat`` function.

  .. code-block:: c++

    __attribute__ ((access (read_write, 1), access (read_only, 2))) char* strcat (char*, const char*);

  The ``write_only`` access mode applies to arguments of pointer types
  without the ``const`` qualifier.  It specifies that the pointer to which
  it applies is used to write to the referenced object but not read from it.
  The object referenced by the pointer need not be initialized.  An example
  of the use of the ``write_only`` access mode is the first argument to
  the ``strcpy`` function, or the first two arguments to the ``fgets``
  function.

  .. code-block:: c++

    __attribute__ ((access (write_only, 1), access (read_only, 2))) char* strcpy (char*, const char*);
    __attribute__ ((access (write_only, 1, 2), access (read_write, 3))) int fgets (char*, int, FILE*);

  The access mode ``none`` specifies that the pointer to which it applies
  is not used to access the referenced object at all.  Unless the pointer is
  null the pointed-to object must exist and have at least the size as denoted
  by the :samp:`{size-index}` argument.  The object need not be initialized.
  The mode is intended to be used as a means to help validate the expected
  object size, for example in functions that call ``__builtin_object_size``.
  See :ref:`object-size-checking`.

.. option:: alias ("target")

  .. index:: alias function attribute

  The ``alias`` attribute causes the declaration to be emitted as an alias
  for another symbol, which must have been previously declared with the same
  type, and for variables, also the same size and alignment.  Declaring an alias
  with a different type than the target is undefined and may be diagnosed.  As
  an example, the following declarations:

  .. code-block:: c++

    void __f () { /* Do something. */; }
    void f () __attribute__ ((weak, alias ("__f")));

  define :samp:`f` to be a weak alias for :samp:`__f`.  In C++, the mangled name
  for the target must be used.  It is an error if :samp:`__f` is not defined in
  the same translation unit.

  This attribute requires assembler and object file support,
  and may not be available on all targets.

.. option:: aligned

  .. index:: aligned function attribute

  The ``aligned`` attribute specifies a minimum alignment for
  the first instruction of the function, measured in bytes.  When specified,
  :samp:`{alignment}` must be an integer constant power of 2.  Specifying no
  :samp:`{alignment}` argument implies the ideal alignment for the target.
  The ``__alignof__`` operator can be used to determine what that is
  (see :ref:`alignment`).  The attribute has no effect when a definition for
  the function is not provided in the same translation unit.

  The attribute cannot be used to decrease the alignment of a function
  previously declared with a more restrictive alignment; only to increase
  it.  Attempts to do otherwise are diagnosed.  Some targets specify
  a minimum default alignment for functions that is greater than 1.  On
  such targets, specifying a less restrictive alignment is silently ignored.
  Using the attribute overrides the effect of the :option:`-falign-functions`
  (see :ref:`optimize-options`) option for this function.

  Note that the effectiveness of ``aligned`` attributes may be
  limited by inherent limitations in the system linker 
  and/or object file format.  On some systems, the
  linker is only able to arrange for functions to be aligned up to a
  certain maximum alignment.  (For some linkers, the maximum supported
  alignment may be very very small.)  See your linker documentation for
  further information.

  The ``aligned`` attribute can also be used for variables and fields
  (see :ref:`variable-attributes`.)

.. option:: alloc_align (position)

  .. index:: alloc_align function attribute

  The ``alloc_align`` attribute may be applied to a function that
  returns a pointer and takes at least one argument of an integer or
  enumerated type.
  It indicates that the returned pointer is aligned on a boundary given
  by the function argument at :samp:`{position}`.  Meaningful alignments are
  powers of 2 greater than one.  GCC uses this information to improve
  pointer alignment analysis.

  The function parameter denoting the allocated alignment is specified by
  one constant integer argument whose number is the argument of the attribute.
  Argument numbering starts at one.

  For instance,

  .. code-block:: c++

    void* my_memalign (size_t, size_t) __attribute__ ((alloc_align (1)));

  declares that ``my_memalign`` returns memory with minimum alignment
  given by parameter 1.

.. option:: alloc_size (position)

  .. index:: alloc_size function attribute

  The ``alloc_size`` attribute may be applied to a function that
  returns a pointer and takes at least one argument of an integer or
  enumerated type.
  It indicates that the returned pointer points to memory whose size is
  given by the function argument at :samp:`{position-1}`, or by the product
  of the arguments at :samp:`{position-1}` and :samp:`{position-2}`.  Meaningful
  sizes are positive values less than ``PTRDIFF_MAX``.  GCC uses this
  information to improve the results of ``__builtin_object_size``.

  The function parameter(s) denoting the allocated size are specified by
  one or two integer arguments supplied to the attribute.  The allocated size
  is either the value of the single function argument specified or the product
  of the two function arguments specified.  Argument numbering starts at
  one for ordinary functions, and at two for C++ non-static member functions.

  For instance,

  .. code-block:: c++

    void* my_calloc (size_t, size_t) __attribute__ ((alloc_size (1, 2)));
    void* my_realloc (void*, size_t) __attribute__ ((alloc_size (2)));

  declares that ``my_calloc`` returns memory of the size given by
  the product of parameter 1 and 2 and that ``my_realloc`` returns memory
  of the size given by parameter 2.

.. option:: always_inline

  .. index:: always_inline function attribute

  Generally, functions are not inlined unless optimization is specified.
  For functions declared inline, this attribute inlines the function
  independent of any restrictions that otherwise apply to inlining.
  Failure to inline such a function is diagnosed as an error.
  Note that if such a function is called indirectly the compiler may
  or may not inline it depending on optimization level and a failure
  to inline an indirect call may or may not be diagnosed.

.. option:: artificial

  .. index:: artificial function attribute

  This attribute is useful for small inline wrappers that if possible
  should appear during debugging as a unit.  Depending on the debug
  info format it either means marking the function as artificial
  or using the caller location for all instructions within the inlined
  body.

.. option:: assume_aligned (alignment)

  .. index:: assume_aligned function attribute

  The ``assume_aligned`` attribute may be applied to a function that
  returns a pointer.  It indicates that the returned pointer is aligned
  on a boundary given by :samp:`{alignment}`.  If the attribute has two
  arguments, the second argument is misalignment :samp:`{offset}`.  Meaningful
  values of :samp:`{alignment}` are powers of 2 greater than one.  Meaningful
  values of :samp:`{offset}` are greater than zero and less than :samp:`{alignment}`.

  For instance

  .. code-block:: c++

    void* my_alloc1 (size_t) __attribute__((assume_aligned (16)));
    void* my_alloc2 (size_t) __attribute__((assume_aligned (32, 8)));

  declares that ``my_alloc1`` returns 16-byte aligned pointers and
  that ``my_alloc2`` returns a pointer whose value modulo 32 is equal
  to 8.

.. option:: cold

  .. index:: cold function attribute

  The ``cold`` attribute on functions is used to inform the compiler that
  the function is unlikely to be executed.  The function is optimized for
  size rather than speed and on many targets it is placed into a special
  subsection of the text section so all cold functions appear close together,
  improving code locality of non-cold parts of program.  The paths leading
  to calls of cold functions within code are marked as unlikely by the branch
  prediction mechanism.  It is thus useful to mark functions used to handle
  unlikely conditions, such as ``perror``, as cold to improve optimization
  of hot functions that do call marked functions in rare occasions.

  When profile feedback is available, via :option:`-fprofile-use`, cold functions
  are automatically detected and this attribute is ignored.

.. option:: const

  .. index:: const function attribute

  .. index:: functions that have no side effects

  Calls to functions whose return value is not affected by changes to
  the observable state of the program and that have no observable effects
  on such state other than to return a value may lend themselves to
  optimizations such as common subexpression elimination.  Declaring such
  functions with the ``const`` attribute allows GCC to avoid emitting
  some calls in repeated invocations of the function with the same argument
  values.

  For example,

  .. code-block:: c++

    int square (int) __attribute__ ((const));

  tells GCC that subsequent calls to function ``square`` with the same
  argument value can be replaced by the result of the first call regardless
  of the statements in between.

  The ``const`` attribute prohibits a function from reading objects
  that affect its return value between successive invocations.  However,
  functions declared with the attribute can safely read objects that do
  not change their return value, such as non-volatile constants.

  The ``const`` attribute imposes greater restrictions on a function's
  definition than the similar ``pure`` attribute.  Declaring the same
  function with both the ``const`` and the ``pure`` attribute is
  diagnosed.  Because a const function cannot have any observable side
  effects it does not make sense for it to return ``void``.  Declaring
  such a function is diagnosed.

  .. index:: pointer arguments

  Note that a function that has pointer arguments and examines the data
  pointed to must *not* be declared ``const`` if the pointed-to
  data might change between successive invocations of the function.  In
  general, since a function cannot distinguish data that might change
  from data that cannot, const functions should never take pointer or,
  in C++, reference arguments. Likewise, a function that calls a non-const
  function usually must not be const itself.

.. option:: constructor

  .. index:: constructor function attribute

  .. index:: destructor function attribute

  The ``constructor`` attribute causes the function to be called
  automatically before execution enters ``main ()``.  Similarly, the
  ``destructor`` attribute causes the function to be called
  automatically after ``main ()`` completes or ``exit ()`` is
  called.  Functions with these attributes are useful for
  initializing data that is used implicitly during the execution of
  the program.

  On some targets the attributes also accept an integer argument to
  specify a priority to control the order in which constructor and
  destructor functions are run.  A constructor
  with a smaller priority number runs before a constructor with a larger
  priority number; the opposite relationship holds for destructors.  So,
  if you have a constructor that allocates a resource and a destructor
  that deallocates the same resource, both functions typically have the
  same priority.  The priorities for constructor and destructor
  functions are the same as those specified for namespace-scope C++
  objects (see :ref:`c++-attributes`).  However, at present, the order in which
  constructors for C++ objects with static storage duration and functions
  decorated with attribute ``constructor`` are invoked is unspecified.
  In mixed declarations, attribute ``init_priority`` can be used to
  impose a specific ordering.

  Using the argument forms of the ``constructor`` and ``destructor``
  attributes on targets where the feature is not supported is rejected with
  an error.

.. option:: copy

  .. index:: copy function attribute

  The ``copy`` attribute applies the set of attributes with which
  :samp:`{function}` has been declared to the declaration of the function
  to which the attribute is applied.  The attribute is designed for
  libraries that define aliases or function resolvers that are expected
  to specify the same set of attributes as their targets.  The ``copy``
  attribute can be used with functions, variables, or types.  However,
  the kind of symbol to which the attribute is applied (either function
  or variable) must match the kind of symbol to which the argument refers.
  The ``copy`` attribute copies only syntactic and semantic attributes
  but not attributes that affect a symbol's linkage or visibility such as
  ``alias``, ``visibility``, or ``weak``.  The ``deprecated``
  and ``target_clones`` attribute are also not copied.
  See :ref:`common-type-attributes`.
  See :ref:`common-variable-attributes`.

  For example, the :samp:`{StrongAlias}` macro below makes use of the ``alias``
  and ``copy`` attributes to define an alias named :samp:`{alloc}` for function
  :samp:`{allocate}` declared with attributes :samp:`{alloc_size}`, :samp:`{malloc}`, and
  :samp:`{nothrow}`.  Thanks to the ``__typeof__`` operator the alias has
  the same type as the target function.  As a result of the ``copy``
  attribute the alias also shares the same attributes as the target.

  .. code-block:: c++

    #define StrongAlias(TargetFunc, AliasDecl)  \
      extern __typeof__ (TargetFunc) AliasDecl  \
        __attribute__ ((alias (#TargetFunc), copy (TargetFunc)));

    extern __attribute__ ((alloc_size (1), malloc, nothrow))
      void* allocate (size_t);
    StrongAlias (allocate, alloc);

.. option:: deprecated

  .. index:: deprecated function attribute

  The ``deprecated`` attribute results in a warning if the function
  is used anywhere in the source file.  This is useful when identifying
  functions that are expected to be removed in a future version of a
  program.  The warning also includes the location of the declaration
  of the deprecated function, to enable users to easily find further
  information about why the function is deprecated, or what they should
  do instead.  Note that the warnings only occurs for uses:

  .. code-block:: c++

    int old_fn () __attribute__ ((deprecated));
    int old_fn ();
    int (*fn_ptr)() = old_fn;

  results in a warning on line 3 but not line 2.  The optional :samp:`{msg}`
  argument, which must be a string, is printed in the warning if
  present.

  The ``deprecated`` attribute can also be used for variables and
  types (see :ref:`variable-attributes`, see :ref:`type-attributes`.)

  The message attached to the attribute is affected by the setting of
  the :option:`-fmessage-length` option.

.. option:: error ("message")

  .. index:: error function attribute

  .. index:: warning function attribute

  If the ``error`` or ``warning`` attribute 
  is used on a function declaration and a call to such a function
  is not eliminated through dead code elimination or other optimizations, 
  an error or warning (respectively) that includes :samp:`{message}` is diagnosed.  
  This is useful
  for compile-time checking, especially together with ``__builtin_constant_p``
  and inline functions where checking the inline function arguments is not
  possible through ``extern char [(condition) ? 1 : -1];`` tricks.

  While it is possible to leave the function undefined and thus invoke
  a link failure (to define the function with
  a message in ``.gnu.warning*`` section),
  when using these attributes the problem is diagnosed
  earlier and with exact location of the call even in presence of inline
  functions or when not emitting debugging information.

.. option:: externally_visible

  .. index:: externally_visible function attribute

  This attribute, attached to a global variable or function, nullifies
  the effect of the :option:`-fwhole-program` command-line option, so the
  object remains visible outside the current compilation unit.

  If :option:`-fwhole-program` is used together with :option:`-flto` and 
  :command:`gold` is used as the linker plugin, 
  ``externally_visible`` attributes are automatically added to functions 
  (not variable yet due to a current :command:`gold` issue) 
  that are accessed outside of LTO objects according to resolution file
  produced by :command:`gold`.
  For other linkers that cannot generate resolution file,
  explicit ``externally_visible`` attributes are still necessary.

.. option:: flatten

  .. index:: flatten function attribute

  Generally, inlining into a function is limited.  For a function marked with
  this attribute, every call inside this function is inlined, if possible.
  Functions declared with attribute ``noinline`` and similar are not
  inlined.  Whether the function itself is considered for inlining depends
  on its size and the current inlining parameters.

.. option:: format (archetype, string-index, first-to-check), -Wformat, -ffreestanding, -fno-builtin

  .. index:: format function attribute

  .. index:: functions with printf, scanf, strftime or strfmon style arguments

  The ``format`` attribute specifies that a function takes ``printf``,
  ``scanf``, ``strftime`` or ``strfmon`` style arguments that
  should be type-checked against a format string.  For example, the
  declaration:

  .. code-block:: c++

    extern int
    my_printf (void *my_object, const char *my_format, ...)
          __attribute__ ((format (printf, 2, 3)));

  causes the compiler to check the arguments in calls to ``my_printf``
  for consistency with the ``printf`` style format string argument
  ``my_format``.

  The parameter :samp:`{archetype}` determines how the format string is
  interpreted, and should be ``printf``, ``scanf``, ``strftime``,
  ``gnu_printf``, ``gnu_scanf``, ``gnu_strftime`` or
  ``strfmon``.  (You can also use ``__printf__``,
  ``__scanf__``, ``__strftime__`` or ``__strfmon__``.)  On
  MinGW targets, ``ms_printf``, ``ms_scanf``, and
  ``ms_strftime`` are also present.
  :samp:`{archetype}` values such as ``printf`` refer to the formats accepted
  by the system's C runtime library,
  while values prefixed with :samp:`gnu_` always refer
  to the formats accepted by the GNU C Library.  On Microsoft Windows
  targets, values prefixed with :samp:`ms_` refer to the formats accepted by the
  msvcrt.dll library.
  The parameter :samp:`{string-index}`
  specifies which argument is the format string argument (starting
  from 1), while :samp:`{first-to-check}` is the number of the first
  argument to check against the format string.  For functions
  where the arguments are not available to be checked (such as
  ``vprintf`` ), specify the third parameter as zero.  In this case the
  compiler only checks the format string for consistency.  For
  ``strftime`` formats, the third parameter is required to be zero.
  Since non-static C++ methods have an implicit ``this`` argument, the
  arguments of such methods should be counted from two, not one, when
  giving values for :samp:`{string-index}` and :samp:`{first-to-check}`.

  In the example above, the format string ( ``my_format`` ) is the second
  argument of the function ``my_print``, and the arguments to check
  start with the third argument, so the correct parameters for the format
  attribute are 2 and 3.

  The ``format`` attribute allows you to identify your own functions
  that take format strings as arguments, so that GCC can check the
  calls to these functions for errors.  The compiler always (unless
  :option:`-ffreestanding` or :option:`-fno-builtin` is used) checks formats
  for the standard library functions ``printf``, ``fprintf``,
  ``sprintf``, ``scanf``, ``fscanf``, ``sscanf``, ``strftime``,
  ``vprintf``, ``vfprintf`` and ``vsprintf`` whenever such
  warnings are requested (using :option:`-Wformat` ), so there is no need to
  modify the header file stdio.h.  In C99 mode, the functions
  ``snprintf``, ``vsnprintf``, ``vscanf``, ``vfscanf`` and
  ``vsscanf`` are also checked.  Except in strictly conforming C
  standard modes, the X/Open function ``strfmon`` is also checked as
  are ``printf_unlocked`` and ``fprintf_unlocked``.
  See :ref:`Options Controlling C Dialect <c-dialect-options>`.

  For Objective-C dialects, ``NSString`` (or ``__NSString__`` ) is
  recognized in the same context.  Declarations including these format attributes
  are parsed for correct syntax, however the result of checking of such format
  strings is not yet defined, and is not carried out by this version of the
  compiler.

  The target may also provide additional types of format checks.
  See :ref:`Format Checks Specific to Particular
  Target Machines <target-format-checks>`.

.. option:: format_arg (string-index), -Wformat-nonliteral

  .. index:: format_arg function attribute

  The ``format_arg`` attribute specifies that a function takes one or
  more format strings for a ``printf``, ``scanf``, ``strftime`` or
  ``strfmon`` style function and modifies it (for example, to translate
  it into another language), so the result can be passed to a
  ``printf``, ``scanf``, ``strftime`` or ``strfmon`` style
  function (with the remaining arguments to the format function the same
  as they would have been for the unmodified string).  Multiple
  ``format_arg`` attributes may be applied to the same function, each
  designating a distinct parameter as a format string.  For example, the
  declaration:

  .. code-block:: c++

    extern char *
    my_dgettext (char *my_domain, const char *my_format)
          __attribute__ ((format_arg (2)));

  causes the compiler to check the arguments in calls to a ``printf``,
  ``scanf``, ``strftime`` or ``strfmon`` type function, whose
  format string argument is a call to the ``my_dgettext`` function, for
  consistency with the format string argument ``my_format``.  If the
  ``format_arg`` attribute had not been specified, all the compiler
  could tell in such calls to format functions would be that the format
  string argument is not constant; this would generate a warning when
  :option:`-Wformat-nonliteral` is used, but the calls could not be checked
  without the attribute.

  In calls to a function declared with more than one ``format_arg``
  attribute, each with a distinct argument value, the corresponding
  actual function arguments are checked against all format strings
  designated by the attributes.  This capability is designed to support
  the GNU ``ngettext`` family of functions.

  The parameter :samp:`{string-index}` specifies which argument is the format
  string argument (starting from one).  Since non-static C++ methods have
  an implicit ``this`` argument, the arguments of such methods should
  be counted from two.

  The ``format_arg`` attribute allows you to identify your own
  functions that modify format strings, so that GCC can check the
  calls to ``printf``, ``scanf``, ``strftime`` or ``strfmon``
  type function whose operands are a call to one of your own function.
  The compiler always treats ``gettext``, ``dgettext``, and
  ``dcgettext`` in this manner except when strict ISO C support is
  requested by :option:`-ansi` or an appropriate :option:`-std` option, or
  :option:`-ffreestanding` or :option:`-fno-builtin`
  is used.  See :ref:`Options
  Controlling C Dialect <c-dialect-options>`.

  For Objective-C dialects, the ``format-arg`` attribute may refer to an
  ``NSString`` reference for compatibility with the ``format`` attribute
  above.

  The target may also allow additional types in ``format-arg`` attributes.
  See :ref:`Format Checks Specific to Particular
  Target Machines <target-format-checks>`.

.. option:: gnu_inline

  .. index:: gnu_inline function attribute

  This attribute should be used with a function that is also declared
  with the ``inline`` keyword.  It directs GCC to treat the function
  as if it were defined in gnu90 mode even when compiling in C99 or
  gnu99 mode.

  If the function is declared ``extern``, then this definition of the
  function is used only for inlining.  In no case is the function
  compiled as a standalone function, not even if you take its address
  explicitly.  Such an address becomes an external reference, as if you
  had only declared the function, and had not defined it.  This has
  almost the effect of a macro.  The way to use this is to put a
  function definition in a header file with this attribute, and put
  another copy of the function, without ``extern``, in a library
  file.  The definition in the header file causes most calls to the
  function to be inlined.  If any uses of the function remain, they
  refer to the single copy in the library.  Note that the two
  definitions of the functions need not be precisely the same, although
  if they do not have the same effect your program may behave oddly.

  In C, if the function is neither ``extern`` nor ``static``, then
  the function is compiled as a standalone function, as well as being
  inlined where possible.

  This is how GCC traditionally handled functions declared
  ``inline``.  Since ISO C99 specifies a different semantics for
  ``inline``, this function attribute is provided as a transition
  measure and as a useful feature in its own right.  This attribute is
  available in GCC 4.1.3 and later.  It is available if either of the
  preprocessor macros ``__GNUC_GNU_INLINE__`` or
  ``__GNUC_STDC_INLINE__`` are defined.  See :ref:`An Inline
  Function is As Fast As a Macro <inline>`.

  In C++, this attribute does not depend on ``extern`` in any way,
  but it still requires the ``inline`` keyword to enable its special
  behavior.

.. option:: hot

  .. index:: hot function attribute

  The ``hot`` attribute on a function is used to inform the compiler that
  the function is a hot spot of the compiled program.  The function is
  optimized more aggressively and on many targets it is placed into a special
  subsection of the text section so all hot functions appear close together,
  improving locality.

  When profile feedback is available, via :option:`-fprofile-use`, hot functions
  are automatically detected and this attribute is ignored.

.. option:: ifunc ("resolver")

  .. index:: ifunc function attribute

  .. index:: indirect functions

  .. index:: functions that are dynamically resolved

  The ``ifunc`` attribute is used to mark a function as an indirect
  function using the STT_GNU_IFUNC symbol type extension to the ELF
  standard.  This allows the resolution of the symbol value to be
  determined dynamically at load time, and an optimized version of the
  routine to be selected for the particular processor or other system
  characteristics determined then.  To use this attribute, first define
  the implementation functions available, and a resolver function that
  returns a pointer to the selected implementation function.  The
  implementation functions' declarations must match the API of the
  function being implemented.  The resolver should be declared to
  be a function taking no arguments and returning a pointer to
  a function of the same type as the implementation.  For example:

  .. code-block:: c++

    void *my_memcpy (void *dst, const void *src, size_t len)
    {
      ...
      return dst;
    }

    static void * (*resolve_memcpy (void))(void *, const void *, size_t)
    {
      return my_memcpy; // we will just always select this routine
    }

  The exported header file declaring the function the user calls would
  contain:

  .. code-block:: c++

    extern void *memcpy (void *, const void *, size_t);

  allowing the user to call ``memcpy`` as a regular function, unaware of
  the actual implementation.  Finally, the indirect function needs to be
  defined in the same translation unit as the resolver function:

  .. code-block:: c++

    void *memcpy (void *, const void *, size_t)
         __attribute__ ((ifunc ("resolve_memcpy")));

  In C++, the ``ifunc`` attribute takes a string that is the mangled name
  of the resolver function.  A C++ resolver for a non-static member function
  of class ``C`` should be declared to return a pointer to a non-member
  function taking pointer to ``C`` as the first argument, followed by
  the same arguments as of the implementation function.  G++ checks
  the signatures of the two functions and issues
  a :option:`-Wattribute-alias` warning for mismatches.  To suppress a warning
  for the necessary cast from a pointer to the implementation member function
  to the type of the corresponding non-member function use
  the :option:`-Wno-pmf-conversions` option.  For example:

  .. code-block:: c++

    class S
    {
    private:
      int debug_impl (int);
      int optimized_impl (int);

      typedef int Func (S*, int);

      static Func* resolver ();
    public:

      int interface (int);
    };

    int S::debug_impl (int) { /* ... */ }
    int S::optimized_impl (int) { /* ... */ }

    S::Func* S::resolver ()
    {
      int (S::*pimpl) (int)
        = getenv ("DEBUG") ? &S::debug_impl : &S::optimized_impl;

      // Cast triggers -Wno-pmf-conversions.
      return reinterpret_cast<Func*>(pimpl);
    }

    int S::interface (int) __attribute__ ((ifunc ("_ZN1S8resolverEv")));

  Indirect functions cannot be weak.  Binutils version 2.20.1 or higher
  and GNU C Library version 2.11.1 are required to use this feature.

.. option:: interrupt

  Many GCC back ends support attributes to indicate that a function is
  an interrupt handler, which tells the compiler to generate function
  entry and exit sequences that differ from those from regular
  functions.  The exact syntax and behavior are target-specific;
  refer to the following subsections for details.

.. option:: leaf

  .. index:: leaf function attribute

  Calls to external functions with this attribute must return to the
  current compilation unit only by return or by exception handling.  In
  particular, a leaf function is not allowed to invoke callback functions
  passed to it from the current compilation unit, directly call functions
  exported by the unit, or ``longjmp`` into the unit.  Leaf functions
  might still call functions from other compilation units and thus they
  are not necessarily leaf in the sense that they contain no function
  calls at all.

  The attribute is intended for library functions to improve dataflow
  analysis.  The compiler takes the hint that any data not escaping the
  current compilation unit cannot be used or modified by the leaf
  function.  For example, the ``sin`` function is a leaf function, but
  ``qsort`` is not.

  Note that leaf functions might indirectly run a signal handler defined
  in the current compilation unit that uses static variables.  Similarly,
  when lazy symbol resolution is in effect, leaf functions might invoke
  indirect functions whose resolver function or implementation function is
  defined in the current compilation unit and uses static variables.  There
  is no standard-compliant way to write such a signal handler, resolver
  function, or implementation function, and the best that you can do is to
  remove the ``leaf`` attribute or mark all such static variables
  ``volatile``.  Lastly, for ELF-based systems that support symbol
  interposition, care should be taken that functions defined in the
  current compilation unit do not unexpectedly interpose other symbols
  based on the defined standards mode and defined feature test macros;
  otherwise an inadvertent callback would be added.

  The attribute has no effect on functions defined within the current
  compilation unit.  This is to allow easy merging of multiple compilation
  units into one, for example, by using the link-time optimization.  For
  this reason the attribute is not allowed on types to annotate indirect
  calls.

``malloc``:samp:`malloc ({deallocator})`
.. option:: malloc (deallocator, ptr-index)

  .. index:: malloc function attribute

  .. index:: functions that behave like malloc

  Attribute ``malloc`` indicates that a function is ``malloc`` -like,
  i.e., that the pointer :samp:`{P}` returned by the function cannot alias any
  other pointer valid when the function returns, and moreover no
  pointers to valid objects occur in any storage addressed by :samp:`{P}`. In
  addition, the GCC predicts that a function with the attribute returns
  non-null in most cases.

  Independently, the form of the attribute with one or two arguments
  associates ``deallocator`` as a suitable deallocation function for
  pointers returned from the ``malloc`` -like function.  :samp:`{ptr-index}`
  denotes the positional argument to which when the pointer is passed in
  calls to ``deallocator`` has the effect of deallocating it.

  Using the attribute with no arguments is designed to improve optimization
  by relying on the aliasing property it implies.  Functions like ``malloc``
  and ``calloc`` have this property because they return a pointer to
  uninitialized or zeroed-out, newly obtained storage.  However, functions
  like ``realloc`` do not have this property, as they may return pointers
  to storage containing pointers to existing objects.  Additionally, since
  all such functions are assumed to return null only infrequently, callers
  can be optimized based on that assumption.

  Associating a function with a :samp:`{deallocator}` helps detect calls to
  mismatched allocation and deallocation functions and diagnose them under
  the control of options such as :option:`-Wmismatched-dealloc`.  It also
  makes it possible to diagnose attempts to deallocate objects that were not
  allocated dynamically, by :option:`-Wfree-nonheap-object`.  To indicate
  that an allocation function both satisifies the nonaliasing property and
  has a deallocator associated with it, both the plain form of the attribute
  and the one with the :samp:`{deallocator}` argument must be used.  The same
  function can be both an allocator and a deallocator.  Since inlining one
  of the associated functions but not the other could result in apparent
  mismatches, this form of attribute ``malloc`` is not accepted on inline
  functions.  For the same reason, using the attribute prevents both
  the allocation and deallocation functions from being expanded inline.

  For example, besides stating that the functions return pointers that do
  not alias any others, the following declarations make ``fclose``
  a suitable deallocator for pointers returned from all functions except
  ``popen``, and ``pclose`` as the only suitable deallocator for
  pointers returned from ``popen``.  The deallocator functions must
  be declared before they can be referenced in the attribute.

  .. code-block:: c++

    int fclose (FILE*);
    int pclose (FILE*);

    __attribute__ ((malloc, malloc (fclose, 1)))
      FILE* fdopen (int, const char*);
    __attribute__ ((malloc, malloc (fclose, 1)))
      FILE* fopen (const char*, const char*);
    __attribute__ ((malloc, malloc (fclose, 1)))
      FILE* fmemopen(void *, size_t, const char *);
    __attribute__ ((malloc, malloc (pclose, 1)))
      FILE* popen (const char*, const char*);
    __attribute__ ((malloc, malloc (fclose, 1)))
      FILE* tmpfile (void);

  The warnings guarded by :option:`-fanalyzer` respect allocation and
  deallocation pairs marked with the ``malloc``.  In particular:

  * The analyzer will emit a :option:`-Wanalyzer-mismatching-deallocation`
    diagnostic if there is an execution path in which the result of an
    allocation call is passed to a different deallocator.

  * The analyzer will emit a :option:`-Wanalyzer-double-free`
    diagnostic if there is an execution path in which a value is passed
    more than once to a deallocation call.

  * The analyzer will consider the possibility that an allocation function
    could fail and return NULL.  It will emit
    :option:`-Wanalyzer-possible-null-dereference` and
    :option:`-Wanalyzer-possible-null-argument` diagnostics if there are
    execution paths in which an unchecked result of an allocation call is
    dereferenced or passed to a function requiring a non-null argument.
    If the allocator always returns non-null, use
    ``__attribute__ ((returns_nonnull))`` to suppress these warnings.
    For example:

    .. code-block:: c++

      char *xstrdup (const char *)
        __attribute__((malloc (free), returns_nonnull));

  * The analyzer will emit a :option:`-Wanalyzer-use-after-free`
    diagnostic if there is an execution path in which the memory passed
    by pointer to a deallocation call is used after the deallocation.

  * The analyzer will emit a :option:`-Wanalyzer-malloc-leak` diagnostic if
    there is an execution path in which the result of an allocation call
    is leaked (without being passed to the deallocation function).

  * The analyzer will emit a :option:`-Wanalyzer-free-of-non-heap` diagnostic
    if a deallocation function is used on a global or on-stack variable.

  The analyzer assumes that deallocators can gracefully handle the ``NULL``
  pointer.  If this is not the case, the deallocator can be marked with
  ``__attribute__((nonnull))`` so that :option:`-fanalyzer` can emit
  a :option:`-Wanalyzer-possible-null-argument` diagnostic for code paths
  in which the deallocator is called with NULL.

.. option:: no_icf

  .. index:: no_icf function attribute

  This function attribute prevents a functions from being merged with another
  semantically equivalent function.

.. option:: no_instrument_function, -finstrument-functions, -p, -pg

  .. index:: no_instrument_function function attribute

  If any of :option:`-finstrument-functions`, :option:`-p`, or :option:`-pg` are 
  given, profiling function calls are
  generated at entry and exit of most user-compiled functions.
  Functions with this attribute are not so instrumented.

.. option:: no_profile_instrument_function

  .. index:: no_profile_instrument_function function attribute

  The ``no_profile_instrument_function`` attribute on functions is used
  to inform the compiler that it should not process any profile feedback based
  optimization code instrumentation.

.. option:: no_reorder

  .. index:: no_reorder function attribute

  Do not reorder functions or variables marked ``no_reorder``
  against each other or top level assembler statements the executable.
  The actual order in the program will depend on the linker command
  line. Static variables marked like this are also not removed.
  This has a similar effect
  as the :option:`-fno-toplevel-reorder` option, but only applies to the
  marked symbols.

.. option:: no_sanitize ("sanitize_option")

  .. index:: no_sanitize function attribute

  The ``no_sanitize`` attribute on functions is used
  to inform the compiler that it should not do sanitization of any option
  mentioned in :samp:`{sanitize_option}`.  A list of values acceptable by
  the :option:`-fsanitize` option can be provided.

  .. code-block:: c++

    void __attribute__ ((no_sanitize ("alignment", "object-size")))
    f () { /* Do something. */; }
    void __attribute__ ((no_sanitize ("alignment,object-size")))
    g () { /* Do something. */; }

.. option:: no_sanitize_address

  .. index:: no_sanitize_address function attribute

  The ``no_sanitize_address`` attribute on functions is used
  to inform the compiler that it should not instrument memory accesses
  in the function when compiling with the :option:`-fsanitize`:samp:`=address` option.
  The ``no_address_safety_analysis`` is a deprecated alias of the
  ``no_sanitize_address`` attribute, new code should use
  ``no_sanitize_address``.

.. option:: no_sanitize_thread

  .. index:: no_sanitize_thread function attribute

  The ``no_sanitize_thread`` attribute on functions is used
  to inform the compiler that it should not instrument memory accesses
  in the function when compiling with the :option:`-fsanitize`:samp:`=thread` option.

.. option:: no_sanitize_undefined

  .. index:: no_sanitize_undefined function attribute

  The ``no_sanitize_undefined`` attribute on functions is used
  to inform the compiler that it should not check for undefined behavior
  in the function when compiling with the :option:`-fsanitize`:samp:`=undefined` option.

.. option:: no_sanitize_coverage

  .. index:: no_sanitize_coverage function attribute

  The ``no_sanitize_coverage`` attribute on functions is used
  to inform the compiler that it should not do coverage-guided
  fuzzing code instrumentation ( :option:`-fsanitize-coverage` ).

.. option:: no_split_stack, -fsplit-stack

  .. index:: no_split_stack function attribute

  If :option:`-fsplit-stack` is given, functions have a small
  prologue which decides whether to split the stack.  Functions with the
  ``no_split_stack`` attribute do not have that prologue, and thus
  may run with only a small amount of stack space available.

.. option:: no_stack_limit

  .. index:: no_stack_limit function attribute

  This attribute locally overrides the :option:`-fstack-limit-register`
  and :option:`-fstack-limit-symbol` command-line options; it has the effect
  of disabling stack limit checking in the function it applies to.

.. option:: noclone

  .. index:: noclone function attribute

  This function attribute prevents a function from being considered for
  cloning-a mechanism that produces specialized copies of functions
  and which is (currently) performed by interprocedural constant
  propagation.

.. option:: noinline

  .. index:: noinline function attribute

  This function attribute prevents a function from being considered for
  inlining.

  .. Don't enumerate the optimizations by name here; we try to be

  .. future-compatible with this mechanism.

  If the function does not have side effects, there are optimizations
  other than inlining that cause function calls to be optimized away,
  although the function call is live.  To keep such calls from being
  optimized away, put

  .. code-block:: c++

    asm ("");

  (see :ref:`extended-asm`) in the called function, to serve as a special
  side effect.

.. option:: noipa

  .. index:: noipa function attribute

  Disable interprocedural optimizations between the function with this
  attribute and its callers, as if the body of the function is not available
  when optimizing callers and the callers are unavailable when optimizing
  the body.  This attribute implies ``noinline``, ``noclone`` and
  ``no_icf`` attributes.    However, this attribute is not equivalent
  to a combination of other attributes, because its purpose is to suppress
  existing and future optimizations employing interprocedural analysis,
  including those that do not have an attribute suitable for disabling
  them individually.  This attribute is supported mainly for the purpose
  of testing the compiler.

.. option:: nonnull

  .. index:: nonnull function attribute

  .. index:: functions with non-null pointer arguments

  The ``nonnull`` attribute may be applied to a function that takes at
  least one argument of a pointer type.  It indicates that the referenced
  arguments must be non-null pointers.  For instance, the declaration:

  .. code-block:: c++

    extern void *
    my_memcpy (void *dest, const void *src, size_t len)
            __attribute__((nonnull (1, 2)));

  causes the compiler to check that, in calls to ``my_memcpy``,
  arguments :samp:`{dest}` and :samp:`{src}` are non-null.  If the compiler
  determines that a null pointer is passed in an argument slot marked
  as non-null, and the :option:`-Wnonnull` option is enabled, a warning
  is issued.  See :ref:`warning-options`.  Unless disabled by
  the :option:`-fno-delete-null-pointer-checks` option the compiler may
  also perform optimizations based on the knowledge that certain function
  arguments cannot be null. In addition,
  the :option:`-fisolate-erroneous-paths-attribute` option can be specified
  to have GCC transform calls with null arguments to non-null functions
  into traps. See :ref:`optimize-options`.

  If no :samp:`{arg-index}` is given to the ``nonnull`` attribute,
  all pointer arguments are marked as non-null.  To illustrate, the
  following declaration is equivalent to the previous example:

  .. code-block:: c++

    extern void *
    my_memcpy (void *dest, const void *src, size_t len)
            __attribute__((nonnull));

.. option:: noplt

  .. index:: noplt function attribute

  The ``noplt`` attribute is the counterpart to option :option:`-fno-plt`.
  Calls to functions marked with this attribute in position-independent code
  do not use the PLT.

  .. code-block:: c++

    /* Externally defined function foo.  */
    int foo () __attribute__ ((noplt));

    int
    main (/* ... */)
    {
      /* ... */
      foo ();
      /* ... */
    }

  The ``noplt`` attribute on function ``foo``
  tells the compiler to assume that
  the function ``foo`` is externally defined and that the call to
  ``foo`` must avoid the PLT
  in position-independent code.

  In position-dependent code, a few targets also convert calls to
  functions that are marked to not use the PLT to use the GOT instead.

.. option:: noreturn

  .. index:: noreturn function attribute

  .. index:: functions that never return

  A few standard library functions, such as ``abort`` and ``exit``,
  cannot return.  GCC knows this automatically.  Some programs define
  their own functions that never return.  You can declare them
  ``noreturn`` to tell the compiler this fact.  For example,

  .. code-block:: c++

    void fatal () __attribute__ ((noreturn));

    void
    fatal (/* ... */)
    {
      /* ... */ /* Print error message. */ /* ... */
      exit (1);
    }

  The ``noreturn`` keyword tells the compiler to assume that
  ``fatal`` cannot return.  It can then optimize without regard to what
  would happen if ``fatal`` ever did return.  This makes slightly
  better code.  More importantly, it helps avoid spurious warnings of
  uninitialized variables.

  The ``noreturn`` keyword does not affect the exceptional path when that
  applies: a ``noreturn`` -marked function may still return to the caller
  by throwing an exception or calling ``longjmp``.

  In order to preserve backtraces, GCC will never turn calls to
  ``noreturn`` functions into tail calls.

  Do not assume that registers saved by the calling function are
  restored before calling the ``noreturn`` function.

  It does not make sense for a ``noreturn`` function to have a return
  type other than ``void``.

.. option:: nothrow

  .. index:: nothrow function attribute

  The ``nothrow`` attribute is used to inform the compiler that a
  function cannot throw an exception.  For example, most functions in
  the standard C library can be guaranteed not to throw an exception
  with the notable exceptions of ``qsort`` and ``bsearch`` that
  take function pointer arguments.

:samp:`optimize ({level}, ...)`
.. option:: optimize (string, ...)

  .. index:: optimize function attribute

  The ``optimize`` attribute is used to specify that a function is to
  be compiled with different optimization options than specified on the
  command line.  Valid arguments are constant non-negative integers and
  strings.  Each numeric argument specifies an optimization :samp:`{level}`.
  Each :samp:`{string}` argument consists of one or more comma-separated
  substrings.  Each substring that begins with the letter ``O`` refers
  to an optimization option such as :option:`-O0` or :option:`-Os`.  Other
  substrings are taken as suffixes to the ``-f`` prefix jointly
  forming the name of an optimization option.  See :ref:`optimize-options`.

  :samp:`#pragma GCC optimize` can be used to set optimization options
  for more than one function.  See :ref:`function-specific-option-pragmas`,
  for details about the pragma.

  Providing multiple strings as arguments separated by commas to specify
  multiple options is equivalent to separating the option suffixes with
  a comma (:samp:`,`) within a single string.  Spaces are not permitted
  within the strings.

  Not every optimization option that starts with the :samp:`{-f}` prefix
  specified by the attribute necessarily has an effect on the function.
  The ``optimize`` attribute should be used for debugging purposes only.
  It is not suitable in production code.

.. option:: patchable_function_entry

  .. index:: patchable_function_entry function attribute

  .. index:: extra NOP instructions at the function entry point

  In case the target's text segment can be made writable at run time by
  any means, padding the function entry with a number of NOPs can be
  used to provide a universal tool for instrumentation.

  The ``patchable_function_entry`` function attribute can be used to
  change the number of NOPs to any desired value.  The two-value syntax
  is the same as for the command-line switch
  :option:`-fpatchable-function-entry`:samp:`=N,M`, generating :samp:`{N}` NOPs, with
  the function entry point before the :samp:`{M}` th NOP instruction.
  :samp:`{M}` defaults to 0 if omitted e.g. function entry point is before
  the first NOP.

  If patchable function entries are enabled globally using the command-line
  option :option:`-fpatchable-function-entry`:samp:`=N,M`, then you must disable
  instrumentation on all functions that are part of the instrumentation
  framework with the attribute ``patchable_function_entry (0)``
  to prevent recursion.

.. option:: pure

  .. index:: pure function attribute

  .. index:: functions that have no side effects

  Calls to functions that have no observable effects on the state of
  the program other than to return a value may lend themselves to optimizations
  such as common subexpression elimination.  Declaring such functions with
  the ``pure`` attribute allows GCC to avoid emitting some calls in repeated
  invocations of the function with the same argument values.

  The ``pure`` attribute prohibits a function from modifying the state
  of the program that is observable by means other than inspecting
  the function's return value.  However, functions declared with the ``pure``
  attribute can safely read any non-volatile objects, and modify the value of
  objects in a way that does not affect their return value or the observable
  state of the program.

  For example,

  .. code-block:: c++

    int hash (char *) __attribute__ ((pure));

  tells GCC that subsequent calls to the function ``hash`` with the same
  string can be replaced by the result of the first call provided the state
  of the program observable by ``hash``, including the contents of the array
  itself, does not change in between.  Even though ``hash`` takes a non-const
  pointer argument it must not modify the array it points to, or any other object
  whose value the rest of the program may depend on.  However, the caller may
  safely change the contents of the array between successive calls to
  the function (doing so disables the optimization).  The restriction also
  applies to member objects referenced by the ``this`` pointer in C++
  non-static member functions.

  Some common examples of pure functions are ``strlen`` or ``memcmp``.
  Interesting non-pure functions are functions with infinite loops or those
  depending on volatile memory or other system resource, that may change between
  consecutive calls (such as the standard C ``feof`` function in
  a multithreading environment).

  The ``pure`` attribute imposes similar but looser restrictions on
  a function's definition than the ``const`` attribute: ``pure``
  allows the function to read any non-volatile memory, even if it changes
  in between successive invocations of the function.  Declaring the same
  function with both the ``pure`` and the ``const`` attribute is
  diagnosed.  Because a pure function cannot have any observable side
  effects it does not make sense for such a function to return ``void``.
  Declaring such a function is diagnosed.

.. option:: returns_nonnull

  .. index:: returns_nonnull function attribute

  The ``returns_nonnull`` attribute specifies that the function
  return value should be a non-null pointer.  For instance, the declaration:

  .. code-block:: c++

    extern void *
    mymalloc (size_t len) __attribute__((returns_nonnull));

  lets the compiler optimize callers based on the knowledge
  that the return value will never be null.

.. option:: returns_twice

  .. index:: returns_twice function attribute

  .. index:: functions that return more than once

  The ``returns_twice`` attribute tells the compiler that a function may
  return more than one time.  The compiler ensures that all registers
  are dead before calling such a function and emits a warning about
  the variables that may be clobbered after the second return from the
  function.  Examples of such functions are ``setjmp`` and ``vfork``.
  The ``longjmp`` -like counterpart of such function, if any, might need
  to be marked with the ``noreturn`` attribute.

.. option:: section ("section-name")

  .. index:: section function attribute

  .. index:: functions in arbitrary sections

  Normally, the compiler places the code it generates in the ``text`` section.
  Sometimes, however, you need additional sections, or you need certain
  particular functions to appear in special sections.  The ``section``
  attribute specifies that a function lives in a particular section.
  For example, the declaration:

  .. code-block:: c++

    extern void foobar (void) __attribute__ ((section ("bar")));

  puts the function ``foobar`` in the ``bar`` section.

  Some file formats do not support arbitrary sections so the ``section``
  attribute is not available on all platforms.
  If you need to map the entire contents of a module to a particular
  section, consider using the facilities of the linker instead.

.. option:: sentinel

  .. index:: sentinel function attribute

  This function attribute indicates that an argument in a call to the function
  is expected to be an explicit ``NULL``.  The attribute is only valid on
  variadic functions.  By default, the sentinel is expected to be the last
  argument of the function call.  If the optional :samp:`{position}` argument
  is specified to the attribute, the sentinel must be located at
  :samp:`{position}` counting backwards from the end of the argument list.

  .. code-block:: c++

    __attribute__ ((sentinel))
    is equivalent to
    __attribute__ ((sentinel(0)))

  The attribute is automatically set with a position of 0 for the built-in
  functions ``execl`` and ``execlp``.  The built-in function
  ``execle`` has the attribute set with a position of 1.

  A valid ``NULL`` in this context is defined as zero with any object
  pointer type.  If your system defines the ``NULL`` macro with
  an integer type then you need to add an explicit cast.  During
  installation GCC replaces the system ``<stddef.h>`` header with
  a copy that redefines NULL appropriately.

  The warnings for missing or incorrect sentinels are enabled with
  :option:`-Wformat`.

.. option:: simd

  .. index:: simd function attribute

  This attribute enables creation of one or more function versions that
  can process multiple arguments using SIMD instructions from a
  single invocation.  Specifying this attribute allows compiler to
  assume that such versions are available at link time (provided
  in the same or another translation unit).  Generated versions are
  target-dependent and described in the corresponding Vector ABI document.  For
  x86_64 target this document can be found
  `here <https://sourceware.org/glibc/wiki/libmvec?action=AttachFile&do=view&target=VectorABI.txt>`_.

  The optional argument :samp:`{mask}` may have the value
  ``notinbranch`` or ``inbranch``,
  and instructs the compiler to generate non-masked or masked
  clones correspondingly. By default, all clones are generated.

  If the attribute is specified and ``#pragma omp declare simd`` is
  present on a declaration and the :option:`-fopenmp` or :option:`-fopenmp-simd`
  switch is specified, then the attribute is ignored.

.. option:: stack_protect

  .. index:: stack_protect function attribute

  This attribute adds stack protection code to the function if 
  flags :option:`-fstack-protector`, :option:`-fstack-protector-strong`
  or :option:`-fstack-protector-explicit` are set.

.. option:: no_stack_protector

  .. index:: no_stack_protector function attribute

  This attribute prevents stack protection code for the function.

.. option:: target (string, ...)

  .. index:: target function attribute

  Multiple target back ends implement the ``target`` attribute
  to specify that a function is to
  be compiled with different target options than specified on the
  command line.  One or more strings can be provided as arguments.
  Each string consists of one or more comma-separated suffixes to
  the ``-m`` prefix jointly forming the name of a machine-dependent
  option.  See :ref:`Machine-Dependent Options <submodel-options>`.

  The ``target`` attribute can be used for instance to have a function
  compiled with a different ISA (instruction set architecture) than the
  default.  :samp:`#pragma GCC target` can be used to specify target-specific
  options for more than one function.  See :ref:`function-specific-option-pragmas`,
  for details about the pragma.

  For instance, on an x86, you could declare one function with the
  ``target("sse4.1,arch=core2")`` attribute and another with
  ``target("sse4a,arch=amdfam10")``.  This is equivalent to
  compiling the first function with :option:`-msse4.1` and
  :option:`-march`:samp:`=core2` options, and the second function with
  :option:`-msse4a` and :option:`-march`:samp:`=amdfam10` options.  It is up to you
  to make sure that a function is only invoked on a machine that
  supports the particular ISA it is compiled for (for example by using
  ``cpuid`` on x86 to determine what feature bits and architecture
  family are used).

  .. code-block:: c++

    int core2_func (void) __attribute__ ((__target__ ("arch=core2")));
    int sse3_func (void) __attribute__ ((__target__ ("sse3")));

  Providing multiple strings as arguments separated by commas to specify
  multiple options is equivalent to separating the option suffixes with
  a comma (:samp:`,`) within a single string.  Spaces are not permitted
  within the strings.

  The options supported are specific to each target; refer to x86
  Function Attributes, PowerPC Function Attributes,
  ARM Function Attributes, AArch64 Function Attributes,
  Nios II Function Attributes, and S/390 Function Attributes
  for details.

.. option:: symver ("name2@nodename")

  .. index:: symver function attribute

  On ELF targets this attribute creates a symbol version.  The :samp:`{name2}` part
  of the parameter is the actual name of the symbol by which it will be
  externally referenced.  The ``nodename`` portion should be the name of a
  node specified in the version script supplied to the linker when building a
  shared library.  Versioned symbol must be defined and must be exported with
  default visibility.

  .. code-block:: c++

    __attribute__ ((__symver__ ("foo@VERS_1"))) int
    foo_v1 (void)
    {
    }

  Will produce a ``.symver foo_v1, foo@VERS_1`` directive in the assembler
  output. 

  One can also define multiple version for a given symbol
  (starting from binutils 2.35).

  .. code-block:: c++

    __attribute__ ((__symver__ ("foo@VERS_2"), __symver__ ("foo@VERS_3")))
    int symver_foo_v1 (void)
    {
    }

  This example creates a symbol name ``symver_foo_v1``
  which will be version ``VERS_2`` and ``VERS_3`` of ``foo``.

  If you have an older release of binutils, then symbol alias needs to
  be used:

  .. code-block:: c++

    __attribute__ ((__symver__ ("foo@VERS_2")))
    int foo_v1 (void)
    {
      return 0;
    }

    __attribute__ ((__symver__ ("foo@VERS_3")))
    __attribute__ ((alias ("foo_v1")))
    int symver_foo_v1 (void);

  Finally if the parameter is ``"name2@@nodename"`` then in
  addition to creating a symbol version (as if
  ``"name2@nodename"`` was used) the version will be also used
  to resolve :samp:`{name2}` by the linker.

.. option:: target_clones (options)

  .. index:: target_clones function attribute

  The ``target_clones`` attribute is used to specify that a function
  be cloned into multiple versions compiled with different target options
  than specified on the command line.  The supported options and restrictions
  are the same as for ``target`` attribute.

  For instance, on an x86, you could compile a function with
  ``target_clones("sse4.1,avx")``.  GCC creates two function clones,
  one compiled with :option:`-msse4.1` and another with :option:`-mavx`.

  On a PowerPC, you can compile a function with
  ``target_clones("cpu=power9,default")``.  GCC will create two
  function clones, one compiled with :option:`-mcpu`:samp:`=power9` and another
  with the default options.  GCC must be configured to use GLIBC 2.23 or
  newer in order to use the ``target_clones`` attribute.

  It also creates a resolver function (see
  the ``ifunc`` attribute above) that dynamically selects a clone
  suitable for current architecture.  The resolver is created only if there
  is a usage of a function with ``target_clones`` attribute.

  Note that any subsequent call of a function without ``target_clone``
  from a ``target_clone`` caller will not lead to copying
  (target clone) of the called function.
  If you want to enforce such behaviour,
  we recommend declaring the calling function with the ``flatten`` attribute?

.. option:: unused

  .. index:: unused function attribute

  This attribute, attached to a function, means that the function is meant
  to be possibly unused.  GCC does not produce a warning for this
  function.

.. option:: used

  .. index:: used function attribute

  This attribute, attached to a function, means that code must be emitted
  for the function even if it appears that the function is not referenced.
  This is useful, for example, when the function is referenced only in
  inline assembly.

  When applied to a member function of a C++ class template, the
  attribute also means that the function is instantiated if the
  class itself is instantiated.

.. option:: retain

  .. index:: retain function attribute

  For ELF targets that support the GNU or FreeBSD OSABIs, this attribute
  will save the function from linker garbage collection.  To support
  this behavior, functions that have not been placed in specific sections
  (e.g. by the ``section`` attribute, or the ``-ffunction-sections``
  option), will be placed in new, unique sections.

  This additional functionality requires Binutils version 2.36 or later.

.. option:: visibility ("visibility_type")

  .. index:: visibility function attribute

  This attribute affects the linkage of the declaration to which it is attached.
  It can be applied to variables (see :ref:`common-variable-attributes`) and types
  (see :ref:`common-type-attributes`) as well as functions.

  There are four supported :samp:`{visibility_type}` values: default,
  hidden, protected or internal visibility.

  .. code-block:: c++

    void __attribute__ ((visibility ("protected")))
    f () { /* Do something. */; }
    int i __attribute__ ((visibility ("hidden")));

  The possible values of :samp:`{visibility_type}` correspond to the
  visibility settings in the ELF gABI.

  .. keep this list of visibilities in alphabetical order.

  ``default``
    Default visibility is the normal case for the object file format.
    This value is available for the visibility attribute to override other
    options that may change the assumed visibility of entities.

    On ELF, default visibility means that the declaration is visible to other
    modules and, in shared libraries, means that the declared entity may be
    overridden.

    On Darwin, default visibility means that the declaration is visible to
    other modules.

    Default visibility corresponds to 'external linkage' in the language.

  ``hidden``
    Hidden visibility indicates that the entity declared has a new
    form of linkage, which we call 'hidden linkage'.  Two
    declarations of an object with hidden linkage refer to the same object
    if they are in the same shared object.

  ``internal``
    Internal visibility is like hidden visibility, but with additional
    processor specific semantics.  Unless otherwise specified by the
    psABI, GCC defines internal visibility to mean that a function is
    *never* called from another module.  Compare this with hidden
    functions which, while they cannot be referenced directly by other
    modules, can be referenced indirectly via function pointers.  By
    indicating that a function cannot be called from outside the module,
    GCC may for instance omit the load of a PIC register since it is known
    that the calling function loaded the correct value.

  ``protected``
    Protected visibility is like default visibility except that it
    indicates that references within the defining module bind to the
    definition in that module.  That is, the declared entity cannot be
    overridden by another module.

    All visibilities are supported on many, but not all, ELF targets
  (supported when the assembler supports the :samp:`.visibility`
  pseudo-op).  Default visibility is supported everywhere.  Hidden
  visibility is supported on Darwin targets.

  The visibility attribute should be applied only to declarations that
  would otherwise have external linkage.  The attribute should be applied
  consistently, so that the same entity should not be declared with
  different settings of the attribute.

  In C++, the visibility attribute applies to types as well as functions
  and objects, because in C++ types have linkage.  A class must not have
  greater visibility than its non-static data member types and bases,
  and class members default to the visibility of their class.  Also, a
  declaration without explicit visibility is limited to the visibility
  of its type.

  In C++, you can mark member functions and static member variables of a
  class with the visibility attribute.  This is useful if you know a
  particular method or static member variable should only be used from
  one shared object; then you can mark it hidden while the rest of the
  class has default visibility.  Care must be taken to avoid breaking
  the One Definition Rule; for example, it is usually not useful to mark
  an inline method as hidden without marking the whole class as hidden.

  A C++ namespace declaration can also have the visibility attribute.

  .. code-block:: c++

    namespace nspace1 __attribute__ ((visibility ("protected")))
    { /* Do something. */; }

  This attribute applies only to the particular namespace body, not to
  other definitions of the same namespace; it is equivalent to using
  :samp:`#pragma GCC visibility` before and after the namespace
  definition (see :ref:`visibility-pragmas`).

  In C++, if a template argument has limited visibility, this
  restriction is implicitly propagated to the template instantiation.
  Otherwise, template instantiations and specializations default to the
  visibility of their template.

  If both the template and enclosing class have explicit visibility, the
  visibility from the template is used.

.. option:: warn_unused_result

  .. index:: warn_unused_result function attribute

  The ``warn_unused_result`` attribute causes a warning to be emitted
  if a caller of the function with this attribute does not use its
  return value.  This is useful for functions where not checking
  the result is either a security problem or always a bug, such as
  ``realloc``.

  .. code-block:: c++

    int fn () __attribute__ ((warn_unused_result));
    int foo ()
    {
      if (fn () < 0) return -1;
      fn ();
      return 0;
    }

  results in warning on line 5.

.. option:: weak

  .. index:: weak function attribute

  The ``weak`` attribute causes a declaration of an external symbol
  to be emitted as a weak symbol rather than a global.  This is primarily
  useful in defining library functions that can be overridden in user code,
  though it can also be used with non-function declarations.  The overriding
  symbol must have the same type as the weak symbol.  In addition, if it
  designates a variable it must also have the same size and alignment as
  the weak symbol.  Weak symbols are supported for ELF targets, and also
  for a.out targets when using the GNU assembler and linker.

.. option:: weakref

  .. index:: weakref function attribute

  The ``weakref`` attribute marks a declaration as a weak reference.
  Without arguments, it should be accompanied by an ``alias`` attribute
  naming the target symbol.  Alternatively, :samp:`{target}` may be given as
  an argument to ``weakref`` itself, naming the target definition of
  the alias.  The :samp:`{target}` must have the same type as the declaration.
  In addition, if it designates a variable it must also have the same size
  and alignment as the declaration.  In either form of the declaration
  ``weakref`` implicitly marks the declared symbol as ``weak``.  Without
  a :samp:`{target}` given as an argument to ``weakref`` or to ``alias``,
  ``weakref`` is equivalent to ``weak`` (in that case the declaration
  may be ``extern`` ).

  .. code-block:: c++

    /* Given the declaration: */
    extern int y (void);

    /* the following... */
    static int x (void) __attribute__ ((weakref ("y")));

    /* is equivalent to... */
    static int x (void) __attribute__ ((weakref, alias ("y")));

    /* or, alternatively, to... */
    static int x (void) __attribute__ ((weakref));
    static int x (void) __attribute__ ((alias ("y")));

  A weak reference is an alias that does not by itself require a
  definition to be given for the target symbol.  If the target symbol is
  only referenced through weak references, then it becomes a ``weak``
  undefined symbol.  If it is directly referenced, however, then such
  strong references prevail, and a definition is required for the
  symbol, not necessarily in the same translation unit.

  The effect is equivalent to moving all references to the alias to a
  separate translation unit, renaming the alias to the aliased symbol,
  declaring it as weak, compiling the two separate translation units and
  performing a link with relocatable output (i.e. ``ld -r`` ) on them.

  A declaration to which ``weakref`` is attached and that is associated
  with a named ``target`` must be ``static``.

.. option:: zero_call_used_regs ("choice")

  .. index:: zero_call_used_regs function attribute

  The ``zero_call_used_regs`` attribute causes the compiler to zero
  a subset of all call-used registersA 'call-used' register
  is a register whose contents can be changed by a function call;
  therefore, a caller cannot assume that the register has the same contents
  on return from the function as it had before calling the function.  Such
  registers are also called 'call-clobbered', 'caller-saved', or
  'volatile'.

   at function return.
  This is used to increase program security by either mitigating
  Return-Oriented Programming (ROP) attacks or preventing information leakage
  through registers.

  In order to satisfy users with different security needs and control the
  run-time overhead at the same time, the :samp:`{choice}` parameter provides a
  flexible way to choose the subset of the call-used registers to be zeroed.
  The three basic values of :samp:`{choice}` are:

  * :samp:`skip` doesn't zero any call-used registers.

  * :samp:`used` only zeros call-used registers that are used in the function.
    A 'used' register is one whose content has been set or referenced in
    the function.

  * :samp:`all` zeros all call-used registers.

  In addition to these three basic choices, it is possible to modify
  :samp:`used` or :samp:`all` as follows:

  * Adding :samp:`-gpr` restricts the zeroing to general-purpose registers.

  * Adding :samp:`-arg` restricts the zeroing to registers that can sometimes
    be used to pass function arguments.  This includes all argument registers
    defined by the platform's calling conversion, regardless of whether the
    function uses those registers for function arguments or not.

  The modifiers can be used individually or together.  If they are used
  together, they must appear in the order above.

  The full list of :samp:`{choice}` s is therefore:

  ``skip``
    doesn't zero any call-used register.

  ``used``
    only zeros call-used registers that are used in the function.

  ``used-gpr``
    only zeros call-used general purpose registers that are used in the function.

  ``used-arg``
    only zeros call-used registers that are used in the function and pass arguments.

  ``used-gpr-arg``
    only zeros call-used general purpose registers that are used in the function
    and pass arguments.

  ``all``
    zeros all call-used registers.

  ``all-gpr``
    zeros all call-used general purpose registers.

  ``all-arg``
    zeros all call-used registers that pass arguments.

  ``all-gpr-arg``
    zeros all call-used general purpose registers that pass
    arguments.

    Of this list, :samp:`used-arg`, :samp:`used-gpr-arg`, :samp:`all-arg`,
  and :samp:`all-gpr-arg` are mainly used for ROP mitigation.

  The default for the attribute is controlled by :option:`-fzero-call-used-regs`.

.. This is the end of the target-independent attribute table

.. _aarch64-function-attributes:

AArch64 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following target-specific function attributes are available for the
AArch64 target.  For the most part, these options mirror the behavior of
similar command-line options (see :ref:`aarch64-options`), but on a
per-function basis.

.. option:: general-regs-only

  .. index:: general-regs-only function attribute, AArch64

  Indicates that no floating-point or Advanced SIMD registers should be
  used when generating code for this function.  If the function explicitly
  uses floating-point code, then the compiler gives an error.  This is
  the same behavior as that of the command-line option
  :option:`-mgeneral-regs-only`.

.. option:: fix-cortex-a53-835769

  .. index:: fix-cortex-a53-835769 function attribute, AArch64

  Indicates that the workaround for the Cortex-A53 erratum 835769 should be
  applied to this function.  To explicitly disable the workaround for this
  function specify the negated form: ``no-fix-cortex-a53-835769``.
  This corresponds to the behavior of the command line options
  :option:`-mfix-cortex-a53-835769` and :option:`-mno-fix-cortex-a53-835769`.

.. option:: cmodel=

  .. index:: cmodel= function attribute, AArch64

  Indicates that code should be generated for a particular code model for
  this function.  The behavior and permissible arguments are the same as
  for the command line option :option:`-mcmodel` =.

.. option:: strict-align

  .. index:: strict-align function attribute, AArch64

  ``strict-align`` indicates that the compiler should not assume that unaligned
  memory references are handled by the system.  To allow the compiler to assume
  that aligned memory references are handled by the system, the inverse attribute
  ``no-strict-align`` can be specified.  The behavior is same as for the
  command-line option :option:`-mstrict-align` and :option:`-mno-strict-align`.

.. option:: omit-leaf-frame-pointer

  .. index:: omit-leaf-frame-pointer function attribute, AArch64

  Indicates that the frame pointer should be omitted for a leaf function call.
  To keep the frame pointer, the inverse attribute
  ``no-omit-leaf-frame-pointer`` can be specified.  These attributes have
  the same behavior as the command-line options :option:`-momit-leaf-frame-pointer`
  and :option:`-mno-omit-leaf-frame-pointer`.

.. option:: tls-dialect=

  .. index:: tls-dialect= function attribute, AArch64

  Specifies the TLS dialect to use for this function.  The behavior and
  permissible arguments are the same as for the command-line option
  :option:`-mtls-dialect` =.

.. option:: arch=

  .. index:: arch= function attribute, AArch64

  Specifies the architecture version and architectural extensions to use
  for this function.  The behavior and permissible arguments are the same as
  for the :option:`-march` = command-line option.

.. option:: tune=

  .. index:: tune= function attribute, AArch64

  Specifies the core for which to tune the performance of this function.
  The behavior and permissible arguments are the same as for the :option:`-mtune` =
  command-line option.

.. option:: cpu=

  .. index:: cpu= function attribute, AArch64

  Specifies the core for which to tune the performance of this function and also
  whose architectural features to use.  The behavior and valid arguments are the
  same as for the :option:`-mcpu` = command-line option.

.. option:: sign-return-address

  .. index:: sign-return-address function attribute, AArch64

  Select the function scope on which return address signing will be applied.  The
  behavior and permissible arguments are the same as for the command-line option
  :option:`-msign-return-address` =.  The default value is ``none``.  This
  attribute is deprecated.  The ``branch-protection`` attribute should
  be used instead.

.. option:: branch-protection

  .. index:: branch-protection function attribute, AArch64

  Select the function scope on which branch protection will be applied.  The
  behavior and permissible arguments are the same as for the command-line option
  :option:`-mbranch-protection` =.  The default value is ``none``.

.. option:: outline-atomics

  .. index:: outline-atomics function attribute, AArch64

  Enable or disable calls to out-of-line helpers to implement atomic operations.
  This corresponds to the behavior of the command line options
  :option:`-moutline-atomics` and :option:`-mno-outline-atomics`.

The above target attributes can be specified as follows:

.. code-block:: c++

  __attribute__((target("attr-string")))
  int
  f (int a)
  {
    return a + 5;
  }

where ``attr-string`` is one of the attribute strings specified above.

Additionally, the architectural extension string may be specified on its
own.  This can be used to turn on and off particular architectural extensions
without having to specify a particular architecture version or core.  Example:

.. code-block:: c++

  __attribute__((target("+crc+nocrypto")))
  int
  foo (int a)
  {
    return a + 5;
  }

In this example ``target("+crc+nocrypto")`` enables the ``crc``
extension and disables the ``crypto`` extension for the function ``foo``
without modifying an existing :option:`-march` = or :option:`-mcpu` option.

Multiple target function attributes can be specified by separating them with
a comma.  For example:

.. code-block:: c++

  __attribute__((target("arch=armv8-a+crc+crypto,tune=cortex-a53")))
  int
  foo (int a)
  {
    return a + 5;
  }

is valid and compiles function ``foo`` for ARMv8-A with ``crc``
and ``crypto`` extensions and tunes it for ``cortex-a53``.

Inlining rules
~~~~~~~~~~~~~~

Specifying target attributes on individual functions or performing link-time
optimization across translation units compiled with different target options
can affect function inlining rules:

In particular, a caller function can inline a callee function only if the
architectural features available to the callee are a subset of the features
available to the caller.
For example: A function ``foo`` compiled with :option:`-march`:samp:`=armv8-a+crc`,
or tagged with the equivalent ``arch=armv8-a+crc`` attribute,
can inline a function ``bar`` compiled with :option:`-march`:samp:`=armv8-a+nocrc`
because the all the architectural features that function ``bar`` requires
are available to function ``foo``.  Conversely, function ``bar`` cannot
inline function ``foo``.

Additionally inlining a function compiled with :option:`-mstrict-align` into a
function compiled without ``-mstrict-align`` is not allowed.
However, inlining a function compiled without :option:`-mstrict-align` into a
function compiled with :option:`-mstrict-align` is allowed.

Note that CPU tuning options and attributes such as the :option:`-mcpu` =,
:option:`-mtune` = do not inhibit inlining unless the CPU specified by the
:option:`-mcpu` = option or the ``cpu=`` attribute conflicts with the
architectural feature rules specified above.

.. _amd-gcn-function-attributes:

AMD GCN Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the AMD GCN back end:

.. option:: amdgpu_hsa_kernel

  .. index:: amdgpu_hsa_kernel function attribute, AMD GCN

  This attribute indicates that the corresponding function should be compiled as
  a kernel function, that is an entry point that can be invoked from the host
  via the HSA runtime library.  By default functions are only callable only from
  other GCN functions.

  This attribute is implicitly applied to any function named ``main``, using
  default parameters.

  Kernel functions may return an integer value, which will be written to a
  conventional place within the HSA "kernargs" region.

  The attribute parameters configure what values are passed into the kernel
  function by the GPU drivers, via the initial register state.  Some values are
  used by the compiler, and therefore forced on.  Enabling other options may
  break assumptions in the compiler and/or run-time libraries.

  ``private_segment_buffer``
    Set ``enable_sgpr_private_segment_buffer`` flag.  Always on (required to
    locate the stack).

  ``dispatch_ptr``
    Set ``enable_sgpr_dispatch_ptr`` flag.  Always on (required to locate the
    launch dimensions).

  ``queue_ptr``
    Set ``enable_sgpr_queue_ptr`` flag.  Always on (required to convert address
    spaces).

  ``kernarg_segment_ptr``
    Set ``enable_sgpr_kernarg_segment_ptr`` flag.  Always on (required to
    locate the kernel arguments, "kernargs").

  ``dispatch_id``
    Set ``enable_sgpr_dispatch_id`` flag.

  ``flat_scratch_init``
    Set ``enable_sgpr_flat_scratch_init`` flag.

  ``private_segment_size``
    Set ``enable_sgpr_private_segment_size`` flag.

  ``grid_workgroup_count_X``
    Set ``enable_sgpr_grid_workgroup_count_x`` flag.  Always on (required to
    use OpenACC/OpenMP).

  ``grid_workgroup_count_Y``
    Set ``enable_sgpr_grid_workgroup_count_y`` flag.

  ``grid_workgroup_count_Z``
    Set ``enable_sgpr_grid_workgroup_count_z`` flag.

  ``workgroup_id_X``
    Set ``enable_sgpr_workgroup_id_x`` flag.

  ``workgroup_id_Y``
    Set ``enable_sgpr_workgroup_id_y`` flag.

  ``workgroup_id_Z``
    Set ``enable_sgpr_workgroup_id_z`` flag.

  ``workgroup_info``
    Set ``enable_sgpr_workgroup_info`` flag.

  ``private_segment_wave_offset``
    Set ``enable_sgpr_private_segment_wave_byte_offset`` flag.  Always on
    (required to locate the stack).

  ``work_item_id_X``
    Set ``enable_vgpr_workitem_id`` parameter.  Always on (can't be disabled).

  ``work_item_id_Y``
    Set ``enable_vgpr_workitem_id`` parameter.  Always on (required to enable
    vectorization.)

  ``work_item_id_Z``
    Set ``enable_vgpr_workitem_id`` parameter.  Always on (required to use
    OpenACC/OpenMP).

.. _arc-function-attributes:

ARC Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the ARC back end:

.. option:: interrupt

  .. index:: interrupt function attribute, ARC

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

  On the ARC, you must specify the kind of interrupt to be handled
  in a parameter to the interrupt attribute like this:

  .. code-block:: c++

    void f () __attribute__ ((interrupt ("ilink1")));

  Permissible values for this parameter are: ``ilink1`` and
  ``ilink2`` for ARCv1 architecture, and ``ilink`` and
  ``firq`` for ARCv2 architecture.

.. option:: long_call

  .. index:: long_call function attribute, ARC

  .. index:: medium_call function attribute, ARC

  .. index:: short_call function attribute, ARC

  .. index:: indirect calls, ARC

  These attributes specify how a particular function is called.
  These attributes override the
  :option:`-mlong-calls` and :option:`-mmedium-calls` (see :ref:`arc-options`)
  command-line switches and ``#pragma long_calls`` settings.

  For ARC, a function marked with the ``long_call`` attribute is
  always called using register-indirect jump-and-link instructions,
  thereby enabling the called function to be placed anywhere within the
  32-bit address space.  A function marked with the ``medium_call``
  attribute will always be close enough to be called with an unconditional
  branch-and-link instruction, which has a 25-bit offset from
  the call site.  A function marked with the ``short_call``
  attribute will always be close enough to be called with a conditional
  branch-and-link instruction, which has a 21-bit offset from
  the call site.

.. option:: jli_always

  .. index:: jli_always function attribute, ARC

  Forces a particular function to be called using ``jli``
  instruction.  The ``jli`` instruction makes use of a table stored
  into ``.jlitab`` section, which holds the location of the functions
  which are addressed using this instruction.

.. option:: jli_fixed

  .. index:: jli_fixed function attribute, ARC

  Identical like the above one, but the location of the function in the
  ``jli`` table is known and given as an attribute parameter.

.. option:: secure_call

  .. index:: secure_call function attribute, ARC

  This attribute allows one to mark secure-code functions that are
  callable from normal mode.  The location of the secure call function
  into the ``sjli`` table needs to be passed as argument.

.. option:: naked

  .. index:: naked function attribute, ARC

  This attribute allows the compiler to construct the requisite function
  declaration, while allowing the body of the function to be assembly
  code.  The specified function will not have prologue/epilogue
  sequences generated by the compiler.  Only basic ``asm`` statements
  can safely be included in naked functions (see :ref:`basic-asm`).  While
  using extended ``asm`` or a mixture of basic ``asm`` and C code
  may appear to work, they cannot be depended upon to work reliably and
  are not supported.

.. _arm-function-attributes:

ARM Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported for ARM targets:

.. option:: general-regs-only

  .. index:: general-regs-only function attribute, ARM

  Indicates that no floating-point or Advanced SIMD registers should be
  used when generating code for this function.  If the function explicitly
  uses floating-point code, then the compiler gives an error.  This is
  the same behavior as that of the command-line option
  :option:`-mgeneral-regs-only`.

.. option:: interrupt

  .. index:: interrupt function attribute, ARM

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

  You can specify the kind of interrupt to be handled by
  adding an optional parameter to the interrupt attribute like this:

  .. code-block:: c++

    void f () __attribute__ ((interrupt ("IRQ")));

  Permissible values for this parameter are: ``IRQ``, ``FIQ``,
  ``SWI``, ``ABORT`` and ``UNDEF``.

  On ARMv7-M the interrupt type is ignored, and the attribute means the function
  may be called with a word-aligned stack pointer.

.. option:: isr

  .. index:: isr function attribute, ARM

  Use this attribute on ARM to write Interrupt Service Routines. This is an
  alias to the ``interrupt`` attribute above.

.. option:: long_call

  .. index:: long_call function attribute, ARM

  .. index:: short_call function attribute, ARM

  .. index:: indirect calls, ARM

  These attributes specify how a particular function is called.
  These attributes override the
  :option:`-mlong-calls` (see :ref:`arm-options`)
  command-line switch and ``#pragma long_calls`` settings.  For ARM, the
  ``long_call`` attribute indicates that the function might be far
  away from the call site and require a different (more expensive)
  calling sequence.   The ``short_call`` attribute always places
  the offset to the function from the call site into the :samp:`BL`
  instruction directly.

.. option:: naked

  .. index:: naked function attribute, ARM

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. option:: pcs

  .. index:: pcs function attribute, ARM

  The ``pcs`` attribute can be used to control the calling convention
  used for a function on ARM.  The attribute takes an argument that specifies
  the calling convention to use.

  When compiling using the AAPCS ABI (or a variant of it) then valid
  values for the argument are ``"aapcs"`` and ``"aapcs-vfp"``.  In
  order to use a variant other than ``"aapcs"`` then the compiler must
  be permitted to use the appropriate co-processor registers (i.e., the
  VFP registers must be available in order to use ``"aapcs-vfp"`` ).
  For example,

  .. code-block:: c++

    /* Argument passed in r0, and result returned in r0+r1.  */
    double f2d (float) __attribute__((pcs("aapcs")));

  Variadic functions always use the ``"aapcs"`` calling convention and
  the compiler rejects attempts to specify an alternative.

.. option:: target (options)

  .. index:: target function attribute

  As discussed in Common Function Attributes, this attribute 
  allows specification of target-specific compilation options.

  On ARM, the following options are allowed:

  :samp:`thumb`

    .. index:: target("thumb") function attribute, ARM

    Force code generation in the Thumb (T16/T32) ISA, depending on the
    architecture level.

  :samp:`arm`

    .. index:: target("arm") function attribute, ARM

    Force code generation in the ARM (A32) ISA.

    Functions from different modes can be inlined in the caller's mode.

  :samp:`fpu=`

    .. index:: target("fpu=") function attribute, ARM

    Specifies the fpu for which to tune the performance of this function.
    The behavior and permissible arguments are the same as for the :option:`-mfpu` =
    command-line option.

  :samp:`arch=`

    .. index:: arch= function attribute, ARM

    Specifies the architecture version and architectural extensions to use
    for this function.  The behavior and permissible arguments are the same as
    for the :option:`-march` = command-line option.

    The above target attributes can be specified as follows:

    .. code-block:: c++

      __attribute__((target("arch=armv8-a+crc")))
      int
      f (int a)
      {
        return a + 5;
      }

    Additionally, the architectural extension string may be specified on its
    own.  This can be used to turn on and off particular architectural extensions
    without having to specify a particular architecture version or core.  Example:

    .. code-block:: c++

      __attribute__((target("+crc+nocrypto")))
      int
      foo (int a)
      {
        return a + 5;
      }

    In this example ``target("+crc+nocrypto")`` enables the ``crc``
    extension and disables the ``crypto`` extension for the function ``foo``
    without modifying an existing :option:`-march` = or :option:`-mcpu` option.

.. _avr-function-attributes:

AVR Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the AVR back end:

.. option:: interrupt

  .. index:: interrupt function attribute, AVR

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

  On the AVR, the hardware globally disables interrupts when an
  interrupt is executed.  The first instruction of an interrupt handler
  declared with this attribute is a ``SEI`` instruction to
  re-enable interrupts.  See also the ``signal`` function attribute
  that does not insert a ``SEI`` instruction.  If both ``signal`` and
  ``interrupt`` are specified for the same function, ``signal``
  is silently ignored.

.. option:: naked

  .. index:: naked function attribute, AVR

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. option:: no_gccisr

  .. index:: no_gccisr function attribute, AVR

  Do not use ``__gcc_isr`` pseudo instructions in a function with
  the ``interrupt`` or ``signal`` attribute aka. interrupt
  service routine (ISR).
  Use this attribute if the preamble of the ISR prologue should always read

  .. code-block:: c++

    push  __zero_reg__
    push  __tmp_reg__
    in    __tmp_reg__, __SREG__
    push  __tmp_reg__
    clr   __zero_reg__

  and accordingly for the postamble of the epilogue - no matter whether
  the mentioned registers are actually used in the ISR or not.
  Situations where you might want to use this attribute include:

  * Code that (effectively) clobbers bits of ``SREG`` other than the
    ``I`` -flag by writing to the memory location of ``SREG``.

  * Code that uses inline assembler to jump to a different function which
    expects (parts of) the prologue code as outlined above to be present.

  To disable ``__gcc_isr`` generation for the whole compilation unit,
  there is option :option:`-mno-gas-isr-prologues`, see :ref:`avr-options`.

.. option:: OS_main

  .. index:: OS_main function attribute, AVR

  .. index:: OS_task function attribute, AVR

  On AVR, functions with the ``OS_main`` or ``OS_task`` attribute
  do not save/restore any call-saved register in their prologue/epilogue.

  The ``OS_main`` attribute can be used when there *is
  guarantee* that interrupts are disabled at the time when the function
  is entered.  This saves resources when the stack pointer has to be
  changed to set up a frame for local variables.

  The ``OS_task`` attribute can be used when there is *no
  guarantee* that interrupts are disabled at that time when the function
  is entered like for, e.g. task functions in a multi-threading operating
  system. In that case, changing the stack pointer register is
  guarded by save/clear/restore of the global interrupt enable flag.

  The differences to the ``naked`` function attribute are:

  * ``naked`` functions do not have a return instruction whereas 
    ``OS_main`` and ``OS_task`` functions have a ``RET`` or
    ``RETI`` return instruction.

  * ``naked`` functions do not set up a frame for local variables
    or a frame pointer whereas ``OS_main`` and ``OS_task`` do this
    as needed.

.. option:: signal

  .. index:: signal function attribute, AVR

  Use this attribute on the AVR to indicate that the specified
  function is an interrupt handler.  The compiler generates function
  entry and exit sequences suitable for use in an interrupt handler when this
  attribute is present.

  See also the ``interrupt`` function attribute. 

  The AVR hardware globally disables interrupts when an interrupt is executed.
  Interrupt handler functions defined with the ``signal`` attribute
  do not re-enable interrupts.  It is save to enable interrupts in a
  ``signal`` handler.  This 'save' only applies to the code
  generated by the compiler and not to the IRQ layout of the
  application which is responsibility of the application.

  If both ``signal`` and ``interrupt`` are specified for the same
  function, ``signal`` is silently ignored.

.. _blackfin-function-attributes:

Blackfin Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Blackfin back end:

.. option:: exception_handler

  .. index:: exception_handler function attribute

  .. index:: exception handler functions, Blackfin

  Use this attribute on the Blackfin to indicate that the specified function
  is an exception handler.  The compiler generates function entry and
  exit sequences suitable for use in an exception handler when this
  attribute is present.

.. option:: interrupt_handler

  .. index:: interrupt_handler function attribute, Blackfin

  Use this attribute to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.

.. option:: kspisusp

  .. index:: kspisusp function attribute, Blackfin

  .. index:: User stack pointer in interrupts on the Blackfin

  When used together with ``interrupt_handler``, ``exception_handler``
  or ``nmi_handler``, code is generated to load the stack pointer
  from the USP register in the function prologue.

.. option:: l1_text

  .. index:: l1_text function attribute, Blackfin

  This attribute specifies a function to be placed into L1 Instruction
  SRAM. The function is put into a specific section named ``.l1.text``.
  With :option:`-mfdpic`, function calls with a such function as the callee
  or caller uses inlined PLT.

.. option:: l2

  .. index:: l2 function attribute, Blackfin

  This attribute specifies a function to be placed into L2
  SRAM. The function is put into a specific section named
  ``.l2.text``. With :option:`-mfdpic`, callers of such functions use
  an inlined PLT.

.. option:: longcall

  .. index:: indirect calls, Blackfin

  .. index:: longcall function attribute, Blackfin

  .. index:: shortcall function attribute, Blackfin

  The ``longcall`` attribute
  indicates that the function might be far away from the call site and
  require a different (more expensive) calling sequence.  The
  ``shortcall`` attribute indicates that the function is always close
  enough for the shorter calling sequence to be used.  These attributes
  override the :option:`-mlongcall` switch.

.. option:: nesting

  .. index:: nesting function attribute, Blackfin

  .. index:: Allow nesting in an interrupt handler on the Blackfin processor

  Use this attribute together with ``interrupt_handler``,
  ``exception_handler`` or ``nmi_handler`` to indicate that the function
  entry code should enable nested interrupts or exceptions.

.. option:: nmi_handler

  .. index:: nmi_handler function attribute, Blackfin

  .. index:: NMI handler functions on the Blackfin processor

  Use this attribute on the Blackfin to indicate that the specified function
  is an NMI handler.  The compiler generates function entry and
  exit sequences suitable for use in an NMI handler when this
  attribute is present.

.. option:: saveall

  .. index:: saveall function attribute, Blackfin

  .. index:: save all registers on the Blackfin

  Use this attribute to indicate that
  all registers except the stack pointer should be saved in the prologue
  regardless of whether they are used or not.

.. _bpf-function-attributes:

BPF Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the BPF back end:

.. option:: kernel_helper

  .. index:: kernel helper, function attribute, BPF

  use this attribute to indicate the specified function declaration is a
  kernel helper.  The helper function is passed as an argument to the
  attribute.  Example:

  .. code-block:: c++

    int bpf_probe_read (void *dst, int size, const void *unsafe_ptr)
      __attribute__ ((kernel_helper (4)));

.. _cr16-function-attributes:

CR16 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the CR16 back end:

.. option:: interrupt

  .. index:: interrupt function attribute, CR16

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

.. _c-sky-function-attributes:

C-SKY Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the C-SKY back end:

.. option:: interrupt

  .. index:: interrupt function attribute, C-SKY

  .. index:: isr function attribute, C-SKY

  Use these attributes to indicate that the specified function
  is an interrupt handler.
  The compiler generates function entry and exit sequences suitable for
  use in an interrupt handler when either of these attributes are present.

  Use of these options requires the :option:`-mistack` command-line option
  to enable support for the necessary interrupt stack instructions.  They
  are ignored with a warning otherwise.  See :ref:`c-sky-options`.

.. option:: naked

  .. index:: naked function attribute, C-SKY

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. _epiphany-function-attributes:

Epiphany Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Epiphany back end:

.. option:: disinterrupt

  .. index:: disinterrupt function attribute, Epiphany

  This attribute causes the compiler to emit
  instructions to disable interrupts for the duration of the given
  function.

.. option:: forwarder_section

  .. index:: forwarder_section function attribute, Epiphany

  This attribute modifies the behavior of an interrupt handler.
  The interrupt handler may be in external memory which cannot be
  reached by a branch instruction, so generate a local memory trampoline
  to transfer control.  The single parameter identifies the section where
  the trampoline is placed.

.. option:: interrupt

  .. index:: interrupt function attribute, Epiphany

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.  It may also generate
  a special section with code to initialize the interrupt vector table.

  On Epiphany targets one or more optional parameters can be added like this:

  .. code-block:: c++

    void __attribute__ ((interrupt ("dma0, dma1"))) universal_dma_handler ();

  Permissible values for these parameters are: ``reset``,
  ``software_exception``, ``page_miss``,
  ``timer0``, ``timer1``, ``message``,
  ``dma0``, ``dma1``, ``wand`` and ``swi``.
  Multiple parameters indicate that multiple entries in the interrupt
  vector table should be initialized for this function, i.e. for each
  parameter :samp:`{name}`, a jump to the function is emitted in
  the section ivt_entry_ :samp:`{name}`.  The parameter(s) may be omitted
  entirely, in which case no interrupt vector table entry is provided.

  Note that interrupts are enabled inside the function
  unless the ``disinterrupt`` attribute is also specified.

  The following examples are all valid uses of these attributes on
  Epiphany targets:

  .. code-block:: c++

    void __attribute__ ((interrupt)) universal_handler ();
    void __attribute__ ((interrupt ("dma1"))) dma1_handler ();
    void __attribute__ ((interrupt ("dma0, dma1"))) 
      universal_dma_handler ();
    void __attribute__ ((interrupt ("timer0"), disinterrupt))
      fast_timer_handler ();
    void __attribute__ ((interrupt ("dma0, dma1"), 
                         forwarder_section ("tramp")))
      external_dma_handler ();

.. option:: long_call

  .. index:: long_call function attribute, Epiphany

  .. index:: short_call function attribute, Epiphany

  .. index:: indirect calls, Epiphany

  These attributes specify how a particular function is called.
  These attributes override the
  :option:`-mlong-calls` (see :ref:`adapteva-epiphany-options`)
  command-line switch and ``#pragma long_calls`` settings.

.. _h8-300-function-attributes:

H8/300 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are available for H8/300 targets:

.. option:: function_vector

  .. index:: function_vector function attribute, H8/300

  Use this attribute on the H8/300, H8/300H, and H8S to indicate 
  that the specified function should be called through the function vector.
  Calling a function through the function vector reduces code size; however,
  the function vector has a limited size (maximum 128 entries on the H8/300
  and 64 entries on the H8/300H and H8S)
  and shares space with the interrupt vector.

.. option:: interrupt_handler

  .. index:: interrupt_handler function attribute, H8/300

  Use this attribute on the H8/300, H8/300H, and H8S to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.

.. option:: saveall

  .. index:: saveall function attribute, H8/300

  .. index:: save all registers on the H8/300, H8/300H, and H8S

  Use this attribute on the H8/300, H8/300H, and H8S to indicate that
  all registers except the stack pointer should be saved in the prologue
  regardless of whether they are used or not.

.. _ia-64-function-attributes:

IA-64 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported on IA-64 targets:

.. option:: syscall_linkage

  .. index:: syscall_linkage function attribute, IA-64

  This attribute is used to modify the IA-64 calling convention by marking
  all input registers as live at all function exits.  This makes it possible
  to restart a system call after an interrupt without having to save/restore
  the input registers.  This also prevents kernel data from leaking into
  application code.

.. option:: version_id

  .. index:: version_id function attribute, IA-64

  This IA-64 HP-UX attribute, attached to a global variable or function, renames a
  symbol to contain a version string, thus allowing for function level
  versioning.  HP-UX system header files may use function level versioning
  for some system calls.

  .. code-block:: c++

    extern int foo () __attribute__((version_id ("20040821")));

  Calls to ``foo`` are mapped to calls to ``foo{20040821}``.

.. _m32c-function-attributes:

M32C Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the M32C back end:

.. option:: bank_switch

  .. index:: bank_switch function attribute, M32C

  When added to an interrupt handler with the M32C port, causes the
  prologue and epilogue to use bank switching to preserve the registers
  rather than saving them on the stack.

.. option:: fast_interrupt

  .. index:: fast_interrupt function attribute, M32C

  Use this attribute on the M32C port to indicate that the specified
  function is a fast interrupt handler.  This is just like the
  ``interrupt`` attribute, except that ``freit`` is used to return
  instead of ``reit``.

.. option:: function_vector

  .. index:: function_vector function attribute, M16C/M32C

  On M16C/M32C targets, the ``function_vector`` attribute declares a
  special page subroutine call function. Use of this attribute reduces
  the code size by 2 bytes for each call generated to the
  subroutine. The argument to the attribute is the vector number entry
  from the special page vector table which contains the 16 low-order
  bits of the subroutine's entry address. Each vector table has special
  page number (18 to 255) that is used in ``jsrs`` instructions.
  Jump addresses of the routines are generated by adding 0x0F0000 (in
  case of M16C targets) or 0xFF0000 (in case of M32C targets), to the
  2-byte addresses set in the vector table. Therefore you need to ensure
  that all the special page vector routines should get mapped within the
  address range 0x0F0000 to 0x0FFFFF (for M16C) and 0xFF0000 to 0xFFFFFF
  (for M32C).

  In the following example 2 bytes are saved for each call to
  function ``foo``.

  .. code-block:: c++

    void foo (void) __attribute__((function_vector(0x18)));
    void foo (void)
    {
    }

    void bar (void)
    {
        foo();
    }

  If functions are defined in one file and are called in another file,
  then be sure to write this declaration in both files.

  This attribute is ignored for R8C target.

.. option:: interrupt

  .. index:: interrupt function attribute, M32C

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

.. _m32r-d-function-attributes:

M32R/D Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the M32R/D back end:

.. option:: interrupt

  .. index:: interrupt function attribute, M32R/D

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

.. option:: model (model-name)

  .. index:: model function attribute, M32R/D

  .. index:: function addressability on the M32R/D

  On the M32R/D, use this attribute to set the addressability of an
  object, and of the code generated for a function.  The identifier
  :samp:`{model-name}` is one of ``small``, ``medium``, or
  ``large``, representing each of the code models.

  Small model objects live in the lower 16MB of memory (so that their
  addresses can be loaded with the ``ld24`` instruction), and are
  callable with the ``bl`` instruction.

  Medium model objects may live anywhere in the 32-bit address space (the
  compiler generates ``seth/add3`` instructions to load their addresses),
  and are callable with the ``bl`` instruction.

  Large model objects may live anywhere in the 32-bit address space (the
  compiler generates ``seth/add3`` instructions to load their addresses),
  and may not be reachable with the ``bl`` instruction (the compiler
  generates the much slower ``seth/add3/jl`` instruction sequence).

.. _m68k-function-attributes:

m68k Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the m68k back end:

.. option:: interrupt

  .. index:: interrupt function attribute, m68k

  .. index:: interrupt_handler function attribute, m68k

  Use this attribute to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.  Either name may be used.

.. option:: interrupt_thread

  .. index:: interrupt_thread function attribute, fido

  Use this attribute on fido, a subarchitecture of the m68k, to indicate
  that the specified function is an interrupt handler that is designed
  to run as a thread.  The compiler omits generate prologue/epilogue
  sequences and replaces the return instruction with a ``sleep``
  instruction.  This attribute is available only on fido.

.. _mcore-function-attributes:

MCORE Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the MCORE back end:

.. option:: naked

  .. index:: naked function attribute, MCORE

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. _mep-function-attributes:

MeP Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the MeP back end:

.. option:: disinterrupt

  .. index:: disinterrupt function attribute, MeP

  On MeP targets, this attribute causes the compiler to emit
  instructions to disable interrupts for the duration of the given
  function.

.. option:: interrupt

  .. index:: interrupt function attribute, MeP

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

.. option:: near

  .. index:: near function attribute, MeP

  This attribute causes the compiler to assume the called
  function is close enough to use the normal calling convention,
  overriding the :option:`-mtf` command-line option.

.. option:: far

  .. index:: far function attribute, MeP

  On MeP targets this causes the compiler to use a calling convention
  that assumes the called function is too far away for the built-in
  addressing modes.

.. option:: vliw

  .. index:: vliw function attribute, MeP

  The ``vliw`` attribute tells the compiler to emit
  instructions in VLIW mode instead of core mode.  Note that this
  attribute is not allowed unless a VLIW coprocessor has been configured
  and enabled through command-line options.

.. _microblaze-function-attributes:

MicroBlaze Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported on MicroBlaze targets:

.. option:: save_volatiles

  .. index:: save_volatiles function attribute, MicroBlaze

  Use this attribute to indicate that the function is
  an interrupt handler.  All volatile registers (in addition to non-volatile
  registers) are saved in the function prologue.  If the function is a leaf
  function, only volatiles used by the function are saved.  A normal function
  return is generated instead of a return from interrupt.

.. option:: break_handler

  .. index:: break_handler function attribute, MicroBlaze

  .. index:: break handler functions

  Use this attribute to indicate that
  the specified function is a break handler.  The compiler generates function
  entry and exit sequences suitable for use in an break handler when this
  attribute is present. The return from ``break_handler`` is done through
  the ``rtbd`` instead of ``rtsd``.

  .. code-block:: c++

    void f () __attribute__ ((break_handler));

.. option:: interrupt_handler

  .. index:: interrupt_handler function attribute, MicroBlaze

  .. index:: fast_interrupt function attribute, MicroBlaze

  These attributes indicate that the specified function is an interrupt
  handler.  Use the ``fast_interrupt`` attribute to indicate handlers
  used in low-latency interrupt mode, and ``interrupt_handler`` for
  interrupts that do not use low-latency handlers.  In both cases, GCC
  emits appropriate prologue code and generates a return from the handler
  using ``rtid`` instead of ``rtsd``.

.. _microsoft-windows-function-attributes:

Microsoft Windows Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following attributes are available on Microsoft Windows and Symbian OS
targets.

.. option:: dllexport

  .. index:: dllexport function attribute

  .. index:: __declspec(dllexport)

  On Microsoft Windows targets and Symbian OS targets the
  ``dllexport`` attribute causes the compiler to provide a global
  pointer to a pointer in a DLL, so that it can be referenced with the
  ``dllimport`` attribute.  On Microsoft Windows targets, the pointer
  name is formed by combining ``_imp__`` and the function or variable
  name.

  You can use ``__declspec(dllexport)`` as a synonym for
  ``__attribute__ ((dllexport))`` for compatibility with other
  compilers.

  On systems that support the ``visibility`` attribute, this
  attribute also implies 'default' visibility.  It is an error to
  explicitly specify any other visibility.

  GCC's default behavior is to emit all inline functions with the
  ``dllexport`` attribute.  Since this can cause object file-size bloat,
  you can use :option:`-fno-keep-inline-dllexport`, which tells GCC to
  ignore the attribute for inlined functions unless the 
  :option:`-fkeep-inline-functions` flag is used instead.

  The attribute is ignored for undefined symbols.

  When applied to C++ classes, the attribute marks defined non-inlined
  member functions and static data members as exports.  Static consts
  initialized in-class are not marked unless they are also defined
  out-of-class.

  For Microsoft Windows targets there are alternative methods for
  including the symbol in the DLL's export table such as using a
  .def file with an ``EXPORTS`` section or, with GNU ld, using
  the :option:`--export-all` linker flag.

.. option:: dllimport

  .. index:: dllimport function attribute

  .. index:: __declspec(dllimport)

  On Microsoft Windows and Symbian OS targets, the ``dllimport``
  attribute causes the compiler to reference a function or variable via
  a global pointer to a pointer that is set up by the DLL exporting the
  symbol.  The attribute implies ``extern``.  On Microsoft Windows
  targets, the pointer name is formed by combining ``_imp__`` and the
  function or variable name.

  You can use ``__declspec(dllimport)`` as a synonym for
  ``__attribute__ ((dllimport))`` for compatibility with other
  compilers.

  On systems that support the ``visibility`` attribute, this
  attribute also implies 'default' visibility.  It is an error to
  explicitly specify any other visibility.

  Currently, the attribute is ignored for inlined functions.  If the
  attribute is applied to a symbol *definition*, an error is reported.
  If a symbol previously declared ``dllimport`` is later defined, the
  attribute is ignored in subsequent references, and a warning is emitted.
  The attribute is also overridden by a subsequent declaration as
  ``dllexport``.

  When applied to C++ classes, the attribute marks non-inlined
  member functions and static data members as imports.  However, the
  attribute is ignored for virtual methods to allow creation of vtables
  using thunks.

  On the SH Symbian OS target the ``dllimport`` attribute also has
  another affect-it can cause the vtable and run-time type information
  for a class to be exported.  This happens when the class has a
  dllimported constructor or a non-inline, non-pure virtual function
  and, for either of those two conditions, the class also has an inline
  constructor or destructor and has a key function that is defined in
  the current translation unit.

  For Microsoft Windows targets the use of the ``dllimport``
  attribute on functions is not necessary, but provides a small
  performance benefit by eliminating a thunk in the DLL.  The use of the
  ``dllimport`` attribute on imported variables can be avoided by passing the
  :option:`--enable-auto-import` switch to the GNU linker.  As with
  functions, using the attribute for a variable eliminates a thunk in
  the DLL.

  One drawback to using this attribute is that a pointer to a
  *variable* marked as ``dllimport`` cannot be used as a constant
  address. However, a pointer to a *function* with the
  ``dllimport`` attribute can be used as a constant initializer; in
  this case, the address of a stub function in the import lib is
  referenced.  On Microsoft Windows targets, the attribute can be disabled
  for functions by setting the :option:`-mnop-fun-dllimport` flag.

.. _mips-function-attributes:

MIPS Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the MIPS back end:

.. option:: interrupt

  .. index:: interrupt function attribute, MIPS

  Use this attribute to indicate that the specified function is an interrupt
  handler.  The compiler generates function entry and exit sequences suitable
  for use in an interrupt handler when this attribute is present.
  An optional argument is supported for the interrupt attribute which allows
  the interrupt mode to be described.  By default GCC assumes the external
  interrupt controller (EIC) mode is in use, this can be explicitly set using
  ``eic``.  When interrupts are non-masked then the requested Interrupt
  Priority Level (IPL) is copied to the current IPL which has the effect of only
  enabling higher priority interrupts.  To use vectored interrupt mode use
  the argument ``vector=[sw0|sw1|hw0|hw1|hw2|hw3|hw4|hw5]``, this will change
  the behavior of the non-masked interrupt support and GCC will arrange to mask
  all interrupts from sw0 up to and including the specified interrupt vector.

  You can use the following attributes to modify the behavior
  of an interrupt handler:

  ``use_shadow_register_set``

    .. index:: use_shadow_register_set function attribute, MIPS

    Assume that the handler uses a shadow register set, instead of
    the main general-purpose registers.  An optional argument ``intstack`` is
    supported to indicate that the shadow register set contains a valid stack
    pointer.

  ``keep_interrupts_masked``

    .. index:: keep_interrupts_masked function attribute, MIPS

    Keep interrupts masked for the whole function.  Without this attribute,
    GCC tries to reenable interrupts for as much of the function as it can.

  ``use_debug_exception_return``

    .. index:: use_debug_exception_return function attribute, MIPS

    Return using the ``deret`` instruction.  Interrupt handlers that don't
    have this attribute return using ``eret`` instead.

    You can use any combination of these attributes, as shown below:

  .. code-block:: c++

    void __attribute__ ((interrupt)) v0 ();
    void __attribute__ ((interrupt, use_shadow_register_set)) v1 ();
    void __attribute__ ((interrupt, keep_interrupts_masked)) v2 ();
    void __attribute__ ((interrupt, use_debug_exception_return)) v3 ();
    void __attribute__ ((interrupt, use_shadow_register_set,
                         keep_interrupts_masked)) v4 ();
    void __attribute__ ((interrupt, use_shadow_register_set,
                         use_debug_exception_return)) v5 ();
    void __attribute__ ((interrupt, keep_interrupts_masked,
                         use_debug_exception_return)) v6 ();
    void __attribute__ ((interrupt, use_shadow_register_set,
                         keep_interrupts_masked,
                         use_debug_exception_return)) v7 ();
    void __attribute__ ((interrupt("eic"))) v8 ();
    void __attribute__ ((interrupt("vector=hw3"))) v9 ();

.. option:: long_call

  .. index:: indirect calls, MIPS

  .. index:: long_call function attribute, MIPS

  .. index:: short_call function attribute, MIPS

  .. index:: near function attribute, MIPS

  .. index:: far function attribute, MIPS

  These attributes specify how a particular function is called on MIPS.
  The attributes override the :option:`-mlong-calls` (see :ref:`mips-options`)
  command-line switch.  The ``long_call`` and ``far`` attributes are
  synonyms, and cause the compiler to always call
  the function by first loading its address into a register, and then using
  the contents of that register.  The ``short_call`` and ``near``
  attributes are synonyms, and have the opposite
  effect; they specify that non-PIC calls should be made using the more
  efficient ``jal`` instruction.

.. option:: mips16

  .. index:: mips16 function attribute, MIPS

  .. index:: nomips16 function attribute, MIPS

  On MIPS targets, you can use the ``mips16`` and ``nomips16``
  function attributes to locally select or turn off MIPS16 code generation.
  A function with the ``mips16`` attribute is emitted as MIPS16 code,
  while MIPS16 code generation is disabled for functions with the
  ``nomips16`` attribute.  These attributes override the
  :option:`-mips16` and :option:`-mno-mips16` options on the command line
  (see :ref:`mips-options`).

  When compiling files containing mixed MIPS16 and non-MIPS16 code, the
  preprocessor symbol ``__mips16`` reflects the setting on the command line,
  not that within individual functions.  Mixed MIPS16 and non-MIPS16 code
  may interact badly with some GCC extensions such as ``__builtin_apply``
  (see :ref:`constructing-calls`).

.. option:: micromips, MIPS

  .. index:: micromips function attribute

  .. index:: nomicromips function attribute

  On MIPS targets, you can use the ``micromips`` and ``nomicromips``
  function attributes to locally select or turn off microMIPS code generation.
  A function with the ``micromips`` attribute is emitted as microMIPS code,
  while microMIPS code generation is disabled for functions with the
  ``nomicromips`` attribute.  These attributes override the
  :option:`-mmicromips` and :option:`-mno-micromips` options on the command line
  (see :ref:`mips-options`).

  When compiling files containing mixed microMIPS and non-microMIPS code, the
  preprocessor symbol ``__mips_micromips`` reflects the setting on the
  command line,
  not that within individual functions.  Mixed microMIPS and non-microMIPS code
  may interact badly with some GCC extensions such as ``__builtin_apply``
  (see :ref:`constructing-calls`).

.. option:: nocompression

  .. index:: nocompression function attribute, MIPS

  On MIPS targets, you can use the ``nocompression`` function attribute
  to locally turn off MIPS16 and microMIPS code generation.  This attribute
  overrides the :option:`-mips16` and :option:`-mmicromips` options on the
  command line (see :ref:`mips-options`).

.. _msp430-function-attributes:

MSP430 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the MSP430 back end:

.. option:: critical

  .. index:: critical function attribute, MSP430

  Critical functions disable interrupts upon entry and restore the
  previous interrupt state upon exit.  Critical functions cannot also
  have the ``naked``, ``reentrant`` or ``interrupt`` attributes.

  The MSP430 hardware ensures that interrupts are disabled on entry to
  ``interrupt`` functions, and restores the previous interrupt state
  on exit. The ``critical`` attribute is therefore redundant on
  ``interrupt`` functions.

.. option:: interrupt

  .. index:: interrupt function attribute, MSP430

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

  You can provide an argument to the interrupt
  attribute which specifies a name or number.  If the argument is a
  number it indicates the slot in the interrupt vector table (0 - 31) to
  which this handler should be assigned.  If the argument is a name it
  is treated as a symbolic name for the vector slot.  These names should
  match up with appropriate entries in the linker script.  By default
  the names ``watchdog`` for vector 26, ``nmi`` for vector 30 and
  ``reset`` for vector 31 are recognized.

.. option:: naked

  .. index:: naked function attribute, MSP430

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. option:: reentrant

  .. index:: reentrant function attribute, MSP430

  Reentrant functions disable interrupts upon entry and enable them
  upon exit.  Reentrant functions cannot also have the ``naked``
  or ``critical`` attributes.  They can have the ``interrupt``
  attribute.

.. option:: wakeup

  .. index:: wakeup function attribute, MSP430

  This attribute only applies to interrupt functions.  It is silently
  ignored if applied to a non-interrupt function.  A wakeup interrupt
  function will rouse the processor from any low-power state that it
  might be in when the function exits.

.. option:: lower

  .. index:: lower function attribute, MSP430

  .. index:: upper function attribute, MSP430

  .. index:: either function attribute, MSP430

  On the MSP430 target these attributes can be used to specify whether
  the function or variable should be placed into low memory, high
  memory, or the placement should be left to the linker to decide.  The
  attributes are only significant if compiling for the MSP430X
  architecture in the large memory model.

  The attributes work in conjunction with a linker script that has been
  augmented to specify where to place sections with a ``.lower`` and
  a ``.upper`` prefix.  So, for example, as well as placing the
  ``.data`` section, the script also specifies the placement of a
  ``.lower.data`` and a ``.upper.data`` section.  The intention
  is that ``lower`` sections are placed into a small but easier to
  access memory region and the upper sections are placed into a larger, but
  slower to access, region.

  The ``either`` attribute is special.  It tells the linker to place
  the object into the corresponding ``lower`` section if there is
  room for it.  If there is insufficient room then the object is placed
  into the corresponding ``upper`` section instead.  Note that the
  placement algorithm is not very sophisticated.  It does not attempt to
  find an optimal packing of the ``lower`` sections.  It just makes
  one pass over the objects and does the best that it can.  Using the
  :option:`-ffunction-sections` and :option:`-fdata-sections` command-line
  options can help the packing, however, since they produce smaller,
  easier to pack regions.

.. _nds32-function-attributes:

NDS32 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the NDS32 back end:

.. option:: exception

  .. index:: exception function attribute

  .. index:: exception handler functions, NDS32

  Use this attribute on the NDS32 target to indicate that the specified function
  is an exception handler.  The compiler will generate corresponding sections
  for use in an exception handler.

.. option:: interrupt

  .. index:: interrupt function attribute, NDS32

  On NDS32 target, this attribute indicates that the specified function
  is an interrupt handler.  The compiler generates corresponding sections
  for use in an interrupt handler.  You can use the following attributes
  to modify the behavior:

  ``nested``

    .. index:: nested function attribute, NDS32

    This interrupt service routine is interruptible.

  ``not_nested``

    .. index:: not_nested function attribute, NDS32

    This interrupt service routine is not interruptible.

  ``nested_ready``

    .. index:: nested_ready function attribute, NDS32

    This interrupt service routine is interruptible after ``PSW.GIE``
    (global interrupt enable) is set.  This allows interrupt service routine to
    finish some short critical code before enabling interrupts.

  ``save_all``

    .. index:: save_all function attribute, NDS32

    The system will help save all registers into stack before entering
    interrupt handler.

  ``partial_save``

    .. index:: partial_save function attribute, NDS32

    The system will help save caller registers into stack before entering
    interrupt handler.

.. option:: naked

  .. index:: naked function attribute, NDS32

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. option:: reset

  .. index:: reset function attribute, NDS32

  .. index:: reset handler functions

  Use this attribute on the NDS32 target to indicate that the specified function
  is a reset handler.  The compiler will generate corresponding sections
  for use in a reset handler.  You can use the following attributes
  to provide extra exception handling:

  ``nmi``

    .. index:: nmi function attribute, NDS32

    Provide a user-defined function to handle NMI exception.

  ``warm``

    .. index:: warm function attribute, NDS32

    Provide a user-defined function to handle warm reset exception.

.. _nios-ii-function-attributes:

Nios II Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Nios II back end:

.. option:: target (options)

  .. index:: target function attribute

  As discussed in Common Function Attributes, this attribute 
  allows specification of target-specific compilation options.

  When compiling for Nios II, the following options are allowed:

  :samp:`custom-{insn}={N}` :samp:`no-custom-{insn}`

    .. index:: target("custom-insn=N") function attribute, Nios II

    .. index:: target("no-custom-insn") function attribute, Nios II

    Each :samp:`custom-{insn}={N}` attribute locally enables use of a
    custom instruction with encoding :samp:`{N}` when generating code that uses 
    :samp:`{insn}`.  Similarly, :samp:`no-custom-{insn}` locally inhibits use of
    the custom instruction :samp:`{insn}`.
    These target attributes correspond to the
    :option:`-mcustom-`:samp:`{insn}` = :samp:`{N}` and :option:`-mno-custom-`:samp:`{insn}`
    command-line options, and support the same set of :samp:`{insn}` keywords.
    See :ref:`nios-ii-options`, for more information.

  :samp:`custom-fpu-cfg={name}`

    .. index:: target("custom-fpu-cfg=name") function attribute, Nios II

    This attribute corresponds to the :option:`-mcustom-fpu-cfg`:samp:`={name}`
    command-line option, to select a predefined set of custom instructions
    named :samp:`{name}`.
    See :ref:`nios-ii-options`, for more information.

.. _nvidia-ptx-function-attributes:

Nvidia PTX Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Nvidia PTX back end:

.. option:: kernel

  .. index:: kernel attribute, Nvidia PTX

  This attribute indicates that the corresponding function should be compiled
  as a kernel function, which can be invoked from the host via the CUDA RT 
  library.
  By default functions are only callable only from other PTX functions.

  Kernel functions must have ``void`` return type.

.. _powerpc-function-attributes:

PowerPC Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the PowerPC back end:

.. option:: longcall

  .. index:: indirect calls, PowerPC

  .. index:: longcall function attribute, PowerPC

  .. index:: shortcall function attribute, PowerPC

  The ``longcall`` attribute
  indicates that the function might be far away from the call site and
  require a different (more expensive) calling sequence.  The
  ``shortcall`` attribute indicates that the function is always close
  enough for the shorter calling sequence to be used.  These attributes
  override both the :option:`-mlongcall` switch and
  the ``#pragma longcall`` setting.

  See :ref:`rs-6000-and-powerpc-options`, for more information on whether long
  calls are necessary.

.. option:: target (options)

  .. index:: target function attribute

  As discussed in Common Function Attributes, this attribute 
  allows specification of target-specific compilation options.

  On the PowerPC, the following options are allowed:

  :samp:`altivec` :samp:`no-altivec`

    .. index:: target("altivec") function attribute, PowerPC

    Generate code that uses (does not use) AltiVec instructions.  In
    32-bit code, you cannot enable AltiVec instructions unless
    :option:`-mabi`:samp:`=altivec` is used on the command line.

  :samp:`cmpb` :samp:`no-cmpb`

    .. index:: target("cmpb") function attribute, PowerPC

    Generate code that uses (does not use) the compare bytes instruction
    implemented on the POWER6 processor and other processors that support
    the PowerPC V2.05 architecture.

  :samp:`dlmzb` :samp:`no-dlmzb`

    .. index:: target("dlmzb") function attribute, PowerPC

    Generate code that uses (does not use) the string-search :samp:`dlmzb`
    instruction on the IBM 405, 440, 464 and 476 processors.  This instruction is
    generated by default when targeting those processors.

  :samp:`fprnd` :samp:`no-fprnd`

    .. index:: target("fprnd") function attribute, PowerPC

    Generate code that uses (does not use) the FP round to integer
    instructions implemented on the POWER5+ processor and other processors
    that support the PowerPC V2.03 architecture.

  :samp:`hard-dfp` :samp:`no-hard-dfp`

    .. index:: target("hard-dfp") function attribute, PowerPC

    Generate code that uses (does not use) the decimal floating-point
    instructions implemented on some POWER processors.

  :samp:`isel` :samp:`no-isel`

    .. index:: target("isel") function attribute, PowerPC

    Generate code that uses (does not use) ISEL instruction.

  :samp:`mfcrf` :samp:`no-mfcrf`

    .. index:: target("mfcrf") function attribute, PowerPC

    Generate code that uses (does not use) the move from condition
    register field instruction implemented on the POWER4 processor and
    other processors that support the PowerPC V2.01 architecture.

  :samp:`mulhw` :samp:`no-mulhw`

    .. index:: target("mulhw") function attribute, PowerPC

    Generate code that uses (does not use) the half-word multiply and
    multiply-accumulate instructions on the IBM 405, 440, 464 and 476 processors.
    These instructions are generated by default when targeting those
    processors.

  :samp:`multiple` :samp:`no-multiple`

    .. index:: target("multiple") function attribute, PowerPC

    Generate code that uses (does not use) the load multiple word
    instructions and the store multiple word instructions.

  :samp:`update` :samp:`no-update`

    .. index:: target("update") function attribute, PowerPC

    Generate code that uses (does not use) the load or store instructions
    that update the base register to the address of the calculated memory
    location.

  :samp:`popcntb` :samp:`no-popcntb`

    .. index:: target("popcntb") function attribute, PowerPC

    Generate code that uses (does not use) the popcount and double-precision
    FP reciprocal estimate instruction implemented on the POWER5
    processor and other processors that support the PowerPC V2.02
    architecture.

  :samp:`popcntd` :samp:`no-popcntd`

    .. index:: target("popcntd") function attribute, PowerPC

    Generate code that uses (does not use) the popcount instruction
    implemented on the POWER7 processor and other processors that support
    the PowerPC V2.06 architecture.

  :samp:`powerpc-gfxopt` :samp:`no-powerpc-gfxopt`

    .. index:: target("powerpc-gfxopt") function attribute, PowerPC

    Generate code that uses (does not use) the optional PowerPC
    architecture instructions in the Graphics group, including
    floating-point select.

  :samp:`powerpc-gpopt` :samp:`no-powerpc-gpopt`

    .. index:: target("powerpc-gpopt") function attribute, PowerPC

    Generate code that uses (does not use) the optional PowerPC
    architecture instructions in the General Purpose group, including
    floating-point square root.

  :samp:`recip-precision` :samp:`no-recip-precision`

    .. index:: target("recip-precision") function attribute, PowerPC

    Assume (do not assume) that the reciprocal estimate instructions
    provide higher-precision estimates than is mandated by the PowerPC
    ABI.

  :samp:`string` :samp:`no-string`

    .. index:: target("string") function attribute, PowerPC

    Generate code that uses (does not use) the load string instructions
    and the store string word instructions to save multiple registers and
    do small block moves.

  :samp:`vsx` :samp:`no-vsx`

    .. index:: target("vsx") function attribute, PowerPC

    Generate code that uses (does not use) vector/scalar (VSX)
    instructions, and also enable the use of built-in functions that allow
    more direct access to the VSX instruction set.  In 32-bit code, you
    cannot enable VSX or AltiVec instructions unless
    :option:`-mabi`:samp:`=altivec` is used on the command line.

  :samp:`friz` :samp:`no-friz`

    .. index:: target("friz") function attribute, PowerPC

    Generate (do not generate) the ``friz`` instruction when the
    :option:`-funsafe-math-optimizations` option is used to optimize
    rounding a floating-point value to 64-bit integer and back to floating
    point.  The ``friz`` instruction does not return the same value if
    the floating-point number is too large to fit in an integer.

  :samp:`avoid-indexed-addresses` :samp:`no-avoid-indexed-addresses`

    .. index:: target("avoid-indexed-addresses") function attribute, PowerPC

    Generate code that tries to avoid (not avoid) the use of indexed load
    or store instructions.

  :samp:`paired` :samp:`no-paired`

    .. index:: target("paired") function attribute, PowerPC

    Generate code that uses (does not use) the generation of PAIRED simd
    instructions.

  :samp:`longcall` :samp:`no-longcall`

    .. index:: target("longcall") function attribute, PowerPC

    Generate code that assumes (does not assume) that all calls are far
    away so that a longer more expensive calling sequence is required.

  :samp:`cpu={CPU}`

    .. index:: target("cpu=CPU") function attribute, PowerPC

    Specify the architecture to generate code for when compiling the
    function.  If you select the ``target("cpu=power7")`` attribute when
    generating 32-bit code, VSX and AltiVec instructions are not generated
    unless you use the :option:`-mabi`:samp:`=altivec` option on the command line.

  :samp:`tune={TUNE}`

    .. index:: target("tune=TUNE") function attribute, PowerPC

    Specify the architecture to tune for when compiling the function.  If
    you do not specify the ``target("tune=TUNE")`` attribute and
    you do specify the ``target("cpu=CPU")`` attribute,
    compilation tunes for the :samp:`{CPU}` architecture, and not the
    default tuning specified on the command line.

    On the PowerPC, the inliner does not inline a
  function that has different target options than the caller, unless the
  callee has a subset of the target options of the caller.

.. _risc-v-function-attributes:

RISC-V Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the RISC-V back end:

.. option:: naked

  .. index:: naked function attribute, RISC-V

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. option:: interrupt

  .. index:: interrupt function attribute, RISC-V

  Use this attribute to indicate that the specified function is an interrupt
  handler.  The compiler generates function entry and exit sequences suitable
  for use in an interrupt handler when this attribute is present.

  You can specify the kind of interrupt to be handled by adding an optional
  parameter to the interrupt attribute like this:

  .. code-block:: c++

    void f (void) __attribute__ ((interrupt ("user")));

  Permissible values for this parameter are ``user``, ``supervisor``,
  and ``machine``.  If there is no parameter, then it defaults to
  ``machine``.

.. _rl78-function-attributes:

RL78 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the RL78 back end:

.. option:: interrupt

  .. index:: interrupt function attribute, RL78

  .. index:: brk_interrupt function attribute, RL78

  These attributes indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

  Use ``brk_interrupt`` instead of ``interrupt`` for
  handlers intended to be used with the ``BRK`` opcode (i.e. those
  that must end with ``RETB`` instead of ``RETI`` ).

.. option:: naked

  .. index:: naked function attribute, RL78

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. _rx-function-attributes:

RX Function Attributes
^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the RX back end:

.. option:: fast_interrupt

  .. index:: fast_interrupt function attribute, RX

  Use this attribute on the RX port to indicate that the specified
  function is a fast interrupt handler.  This is just like the
  ``interrupt`` attribute, except that ``freit`` is used to return
  instead of ``reit``.

.. option:: interrupt

  .. index:: interrupt function attribute, RX

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

  On RX and RL78 targets, you may specify one or more vector numbers as arguments
  to the attribute, as well as naming an alternate table name.
  Parameters are handled sequentially, so one handler can be assigned to
  multiple entries in multiple tables.  One may also pass the magic
  string ``"$default"`` which causes the function to be used for any
  unfilled slots in the current table.

  This example shows a simple assignment of a function to one vector in
  the default table (note that preprocessor macros may be used for
  chip-specific symbolic vector names):

  .. code-block:: c++

    void __attribute__ ((interrupt (5))) txd1_handler ();

  This example assigns a function to two slots in the default table
  (using preprocessor macros defined elsewhere) and makes it the default
  for the ``dct`` table:

  .. code-block:: c++

    void __attribute__ ((interrupt (RXD1_VECT,RXD2_VECT,"dct","$default")))
    	txd1_handler ();

.. option:: naked

  .. index:: naked function attribute, RX

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. option:: vector

  .. index:: vector function attribute, RX

  This RX attribute is similar to the ``interrupt`` attribute, including its
  parameters, but does not make the function an interrupt-handler type
  function (i.e. it retains the normal C function calling ABI).  See the
  ``interrupt`` attribute for a description of its arguments.

.. _s-390-function-attributes:

S/390 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported on the S/390:

.. option:: hotpatch (halfwords-before-function-label,halfwords-after-function-label)

  .. index:: hotpatch function attribute, S/390

  On S/390 System z targets, you can use this function attribute to
  make GCC generate a 'hot-patching' function prologue.  If the
  :option:`-mhotpatch` = command-line option is used at the same time,
  the ``hotpatch`` attribute takes precedence.  The first of the
  two arguments specifies the number of halfwords to be added before
  the function label.  A second argument can be used to specify the
  number of halfwords to be added after the function label.  For
  both arguments the maximum allowed value is 1000000.

  If both arguments are zero, hotpatching is disabled.

.. option:: target (options)

  .. index:: target function attribute

  As discussed in Common Function Attributes, this attribute
  allows specification of target-specific compilation options.

  On S/390, the following options are supported:

  :samp:`arch=`:samp:`tune=`:samp:`stack-guard=`:samp:`stack-size=`:samp:`branch-cost=`:samp:`warn-framesize=`:samp:`backchain`:samp:`no-backchain`:samp:`hard-dfp`:samp:`no-hard-dfp`:samp:`hard-float`:samp:`soft-float`:samp:`htm`:samp:`no-htm`:samp:`vx`:samp:`no-vx`:samp:`packed-stack`:samp:`no-packed-stack`:samp:`small-exec`:samp:`no-small-exec`:samp:`mvcle`:samp:`no-mvcle`:samp:`warn-dynamicstack`:samp:`no-warn-dynamicstack`The options work exactly like the S/390 specific command line
  options (without the prefix :option:`-m` ) except that they do not
  change any feature macros.  For example,

  .. code-block:: c++

    target("no-vx")

  does not undefine the ``__VEC__`` macro.

.. _sh-function-attributes:

SH Function Attributes
^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported on the SH family of processors:

.. option:: function_vector

  .. index:: function_vector function attribute, SH

  .. index:: calling functions through the function vector on SH2A

  On SH2A targets, this attribute declares a function to be called using the
  TBR relative addressing mode.  The argument to this attribute is the entry
  number of the same function in a vector table containing all the TBR
  relative addressable functions.  For correct operation the TBR must be setup
  accordingly to point to the start of the vector table before any functions with
  this attribute are invoked.  Usually a good place to do the initialization is
  the startup routine.  The TBR relative vector table can have at max 256 function
  entries.  The jumps to these functions are generated using a SH2A specific,
  non delayed branch instruction JSR/N @(disp8,TBR).  You must use GAS and GLD
  from GNU binutils version 2.7 or later for this attribute to work correctly.

  In an application, for a function being called once, this attribute
  saves at least 8 bytes of code; and if other successive calls are being
  made to the same function, it saves 2 bytes of code per each of these
  calls.

.. option:: interrupt_handler

  .. index:: interrupt_handler function attribute, SH

  Use this attribute to
  indicate that the specified function is an interrupt handler.  The compiler
  generates function entry and exit sequences suitable for use in an
  interrupt handler when this attribute is present.

.. option:: nosave_low_regs

  .. index:: nosave_low_regs function attribute, SH

  Use this attribute on SH targets to indicate that an ``interrupt_handler``
  function should not save and restore registers R0..R7.  This can be used on SH3*
  and SH4* targets that have a second R0..R7 register bank for non-reentrant
  interrupt handlers.

.. option:: renesas

  .. index:: renesas function attribute, SH

  On SH targets this attribute specifies that the function or struct follows the
  Renesas ABI.

.. option:: resbank

  .. index:: resbank function attribute, SH

  On the SH2A target, this attribute enables the high-speed register
  saving and restoration using a register bank for ``interrupt_handler``
  routines.  Saving to the bank is performed automatically after the CPU
  accepts an interrupt that uses a register bank.

  The nineteen 32-bit registers comprising general register R0 to R14,
  control register GBR, and system registers MACH, MACL, and PR and the
  vector table address offset are saved into a register bank.  Register
  banks are stacked in first-in last-out (FILO) sequence.  Restoration
  from the bank is executed by issuing a RESBANK instruction.

.. option:: sp_switch

  .. index:: sp_switch function attribute, SH

  Use this attribute on the SH to indicate an ``interrupt_handler``
  function should switch to an alternate stack.  It expects a string
  argument that names a global variable holding the address of the
  alternate stack.

  .. code-block:: c++

    void *alt_stack;
    void f () __attribute__ ((interrupt_handler,
                              sp_switch ("alt_stack")));

.. option:: trap_exit

  .. index:: trap_exit function attribute, SH

  Use this attribute on the SH for an ``interrupt_handler`` to return using
  ``trapa`` instead of ``rte``.  This attribute expects an integer
  argument specifying the trap number to be used.

.. option:: trapa_handler

  .. index:: trapa_handler function attribute, SH

  On SH targets this function attribute is similar to ``interrupt_handler``
  but it does not save and restore all registers.

.. _symbian-os-function-attributes:

Symbian OS Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

See :ref:`microsoft-windows-function-attributes`, for discussion of the
``dllexport`` and ``dllimport`` attributes.

.. _v850-function-attributes:

V850 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

The V850 back end supports these function attributes:

.. option:: interrupt

  .. index:: interrupt function attribute, V850

  .. index:: interrupt_handler function attribute, V850

  Use these attributes to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when either attribute is present.

.. _visium-function-attributes:

Visium Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Visium back end:

.. option:: interrupt

  .. index:: interrupt function attribute, Visium

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

.. _x86-function-attributes:

x86 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the x86 back end:

.. option:: cdecl, -mrtd

  .. index:: cdecl function attribute, x86-32

  .. index:: functions that pop the argument stack on x86-32

  On the x86-32 targets, the ``cdecl`` attribute causes the compiler to
  assume that the calling function pops off the stack space used to
  pass arguments.  This is
  useful to override the effects of the :option:`-mrtd` switch.

.. option:: fastcall

  .. index:: fastcall function attribute, x86-32

  .. index:: functions that pop the argument stack on x86-32

  On x86-32 targets, the ``fastcall`` attribute causes the compiler to
  pass the first argument (if of integral type) in the register ECX and
  the second argument (if of integral type) in the register EDX.  Subsequent
  and other typed arguments are passed on the stack.  The called function
  pops the arguments off the stack.  If the number of arguments is variable all
  arguments are pushed on the stack.

.. option:: thiscall

  .. index:: thiscall function attribute, x86-32

  .. index:: functions that pop the argument stack on x86-32

  On x86-32 targets, the ``thiscall`` attribute causes the compiler to
  pass the first argument (if of integral type) in the register ECX.
  Subsequent and other typed arguments are passed on the stack. The called
  function pops the arguments off the stack.
  If the number of arguments is variable all arguments are pushed on the
  stack.
  The ``thiscall`` attribute is intended for C++ non-static member functions.
  As a GCC extension, this calling convention can be used for C functions
  and for static member methods.

.. option:: ms_abi

  .. index:: ms_abi function attribute, x86

  .. index:: sysv_abi function attribute, x86

  On 32-bit and 64-bit x86 targets, you can use an ABI attribute
  to indicate which calling convention should be used for a function.  The
  ``ms_abi`` attribute tells the compiler to use the Microsoft ABI,
  while the ``sysv_abi`` attribute tells the compiler to use the System V
  ELF ABI, which is used on GNU/Linux and other systems.  The default is to use
  the Microsoft ABI when targeting Windows.  On all other systems, the default
  is the System V ELF ABI.

  Note, the ``ms_abi`` attribute for Microsoft Windows 64-bit targets currently
  requires the :option:`-maccumulate-outgoing-args` option.

.. option:: callee_pop_aggregate_return (number)

  .. index:: callee_pop_aggregate_return function attribute, x86

  On x86-32 targets, you can use this attribute to control how
  aggregates are returned in memory.  If the caller is responsible for
  popping the hidden pointer together with the rest of the arguments, specify
  :samp:`{number}` equal to zero.  If callee is responsible for popping the
  hidden pointer, specify :samp:`{number}` equal to one.  

  The default x86-32 ABI assumes that the callee pops the
  stack for hidden pointer.  However, on x86-32 Microsoft Windows targets,
  the compiler assumes that the
  caller pops the stack for hidden pointer.

.. option:: ms_hook_prologue

  .. index:: ms_hook_prologue function attribute, x86

  On 32-bit and 64-bit x86 targets, you can use
  this function attribute to make GCC generate the 'hot-patching' function
  prologue used in Win32 API functions in Microsoft Windows XP Service Pack 2
  and newer.

.. option:: naked

  .. index:: naked function attribute, x86

  This attribute allows the compiler to construct the
  requisite function declaration, while allowing the body of the
  function to be assembly code. The specified function will not have
  prologue/epilogue sequences generated by the compiler. Only basic
  ``asm`` statements can safely be included in naked functions
  (see :ref:`basic-asm`). While using extended ``asm`` or a mixture of
  basic ``asm`` and C code may appear to work, they cannot be
  depended upon to work reliably and are not supported.

.. option:: regparm (number)

  .. index:: regparm function attribute, x86

  .. index:: functions that are passed arguments in registers on x86-32

  On x86-32 targets, the ``regparm`` attribute causes the compiler to
  pass arguments number one to :samp:`{number}` if they are of integral type
  in registers EAX, EDX, and ECX instead of on the stack.  Functions that
  take a variable number of arguments continue to be passed all of their
  arguments on the stack.

  Beware that on some ELF systems this attribute is unsuitable for
  global functions in shared libraries with lazy binding (which is the
  default).  Lazy binding sends the first call via resolving code in
  the loader, which might assume EAX, EDX and ECX can be clobbered, as
  per the standard calling conventions.  Solaris 8 is affected by this.
  Systems with the GNU C Library version 2.1 or higher
  and FreeBSD are believed to be
  safe since the loaders there save EAX, EDX and ECX.  (Lazy binding can be
  disabled with the linker or the loader if desired, to avoid the
  problem.)

.. option:: sseregparm

  .. index:: sseregparm function attribute, x86

  On x86-32 targets with SSE support, the ``sseregparm`` attribute
  causes the compiler to pass up to 3 floating-point arguments in
  SSE registers instead of on the stack.  Functions that take a
  variable number of arguments continue to pass all of their
  floating-point arguments on the stack.

.. option:: force_align_arg_pointer

  .. index:: force_align_arg_pointer function attribute, x86

  On x86 targets, the ``force_align_arg_pointer`` attribute may be
  applied to individual function definitions, generating an alternate
  prologue and epilogue that realigns the run-time stack if necessary.
  This supports mixing legacy codes that run with a 4-byte aligned stack
  with modern codes that keep a 16-byte stack for SSE compatibility.

.. option:: stdcall

  .. index:: stdcall function attribute, x86-32

  .. index:: functions that pop the argument stack on x86-32

  On x86-32 targets, the ``stdcall`` attribute causes the compiler to
  assume that the called function pops off the stack space used to
  pass arguments, unless it takes a variable number of arguments.

.. option:: no_caller_saved_registers

  .. index:: no_caller_saved_registers function attribute, x86

  Use this attribute to indicate that the specified function has no
  caller-saved registers. That is, all registers are callee-saved. For
  example, this attribute can be used for a function called from an
  interrupt handler. The compiler generates proper function entry and
  exit sequences to save and restore any modified registers, except for
  the EFLAGS register.  Since GCC doesn't preserve SSE, MMX nor x87
  states, the GCC option :option:`-mgeneral-regs-only` should be used to
  compile functions with ``no_caller_saved_registers`` attribute.

.. option:: interrupt

  .. index:: interrupt function attribute, x86

  Use this attribute to indicate that the specified function is an
  interrupt handler or an exception handler (depending on parameters passed
  to the function, explained further).  The compiler generates function
  entry and exit sequences suitable for use in an interrupt handler when
  this attribute is present.  The ``IRET`` instruction, instead of the
  ``RET`` instruction, is used to return from interrupt handlers.  All
  registers, except for the EFLAGS register which is restored by the
  ``IRET`` instruction, are preserved by the compiler.  Since GCC
  doesn't preserve SSE, MMX nor x87 states, the GCC option
  :option:`-mgeneral-regs-only` should be used to compile interrupt and
  exception handlers.

  Any interruptible-without-stack-switch code must be compiled with
  :option:`-mno-red-zone` since interrupt handlers can and will, because
  of the hardware design, touch the red zone.

  An interrupt handler must be declared with a mandatory pointer
  argument:

  .. code-block:: c++

    struct interrupt_frame;

    __attribute__ ((interrupt))
    void
    f (struct interrupt_frame *frame)
    {
    }

  and you must define ``struct interrupt_frame`` as described in the
  processor's manual.

  Exception handlers differ from interrupt handlers because the system
  pushes an error code on the stack.  An exception handler declaration is
  similar to that for an interrupt handler, but with a different mandatory
  function signature.  The compiler arranges to pop the error code off the
  stack before the ``IRET`` instruction.

  .. code-block:: c++

    #ifdef __x86_64__
    typedef unsigned long long int uword_t;
    #else
    typedef unsigned int uword_t;
    #endif

    struct interrupt_frame;

    __attribute__ ((interrupt))
    void
    f (struct interrupt_frame *frame, uword_t error_code)
    {
      ...
    }

  Exception handlers should only be used for exceptions that push an error
  code; you should use an interrupt handler in other cases.  The system
  will crash if the wrong kind of handler is used.

.. option:: target (options)

  .. index:: target function attribute

  As discussed in Common Function Attributes, this attribute 
  allows specification of target-specific compilation options.

  On the x86, the following options are allowed:

  :samp:`3dnow` :samp:`no-3dnow`

    .. index:: target("3dnow") function attribute, x86

    Enable/disable the generation of the 3DNow! instructions.

  :samp:`3dnowa` :samp:`no-3dnowa`

    .. index:: target("3dnowa") function attribute, x86

    Enable/disable the generation of the enhanced 3DNow! instructions.

  :samp:`abm` :samp:`no-abm`

    .. index:: target("abm") function attribute, x86

    Enable/disable the generation of the advanced bit instructions.

  :samp:`adx` :samp:`no-adx`

    .. index:: target("adx") function attribute, x86

    Enable/disable the generation of the ADX instructions.

  :samp:`aes` :samp:`no-aes`

    .. index:: target("aes") function attribute, x86

    Enable/disable the generation of the AES instructions.

  :samp:`avx` :samp:`no-avx`

    .. index:: target("avx") function attribute, x86

    Enable/disable the generation of the AVX instructions.

  :samp:`avx2` :samp:`no-avx2`

    .. index:: target("avx2") function attribute, x86

    Enable/disable the generation of the AVX2 instructions.

  :samp:`avx5124fmaps` :samp:`no-avx5124fmaps`

    .. index:: target("avx5124fmaps") function attribute, x86

    Enable/disable the generation of the AVX5124FMAPS instructions.

  :samp:`avx5124vnniw` :samp:`no-avx5124vnniw`

    .. index:: target("avx5124vnniw") function attribute, x86

    Enable/disable the generation of the AVX5124VNNIW instructions.

  :samp:`avx512bitalg` :samp:`no-avx512bitalg`

    .. index:: target("avx512bitalg") function attribute, x86

    Enable/disable the generation of the AVX512BITALG instructions.

  :samp:`avx512bw` :samp:`no-avx512bw`

    .. index:: target("avx512bw") function attribute, x86

    Enable/disable the generation of the AVX512BW instructions.

  :samp:`avx512cd` :samp:`no-avx512cd`

    .. index:: target("avx512cd") function attribute, x86

    Enable/disable the generation of the AVX512CD instructions.

  :samp:`avx512dq` :samp:`no-avx512dq`

    .. index:: target("avx512dq") function attribute, x86

    Enable/disable the generation of the AVX512DQ instructions.

  :samp:`avx512er` :samp:`no-avx512er`

    .. index:: target("avx512er") function attribute, x86

    Enable/disable the generation of the AVX512ER instructions.

  :samp:`avx512f` :samp:`no-avx512f`

    .. index:: target("avx512f") function attribute, x86

    Enable/disable the generation of the AVX512F instructions.

  :samp:`avx512ifma` :samp:`no-avx512ifma`

    .. index:: target("avx512ifma") function attribute, x86

    Enable/disable the generation of the AVX512IFMA instructions.

  :samp:`avx512pf` :samp:`no-avx512pf`

    .. index:: target("avx512pf") function attribute, x86

    Enable/disable the generation of the AVX512PF instructions.

  :samp:`avx512vbmi` :samp:`no-avx512vbmi`

    .. index:: target("avx512vbmi") function attribute, x86

    Enable/disable the generation of the AVX512VBMI instructions.

  :samp:`avx512vbmi2` :samp:`no-avx512vbmi2`

    .. index:: target("avx512vbmi2") function attribute, x86

    Enable/disable the generation of the AVX512VBMI2 instructions.

  :samp:`avx512vl` :samp:`no-avx512vl`

    .. index:: target("avx512vl") function attribute, x86

    Enable/disable the generation of the AVX512VL instructions.

  :samp:`avx512vnni` :samp:`no-avx512vnni`

    .. index:: target("avx512vnni") function attribute, x86

    Enable/disable the generation of the AVX512VNNI instructions.

  :samp:`avx512vpopcntdq` :samp:`no-avx512vpopcntdq`

    .. index:: target("avx512vpopcntdq") function attribute, x86

    Enable/disable the generation of the AVX512VPOPCNTDQ instructions.

  :samp:`bmi` :samp:`no-bmi`

    .. index:: target("bmi") function attribute, x86

    Enable/disable the generation of the BMI instructions.

  :samp:`bmi2` :samp:`no-bmi2`

    .. index:: target("bmi2") function attribute, x86

    Enable/disable the generation of the BMI2 instructions.

  :samp:`cldemote` :samp:`no-cldemote`

    .. index:: target("cldemote") function attribute, x86

    Enable/disable the generation of the CLDEMOTE instructions.

  :samp:`clflushopt` :samp:`no-clflushopt`

    .. index:: target("clflushopt") function attribute, x86

    Enable/disable the generation of the CLFLUSHOPT instructions.

  :samp:`clwb` :samp:`no-clwb`

    .. index:: target("clwb") function attribute, x86

    Enable/disable the generation of the CLWB instructions.

  :samp:`clzero` :samp:`no-clzero`

    .. index:: target("clzero") function attribute, x86

    Enable/disable the generation of the CLZERO instructions.

  :samp:`crc32` :samp:`no-crc32`

    .. index:: target("crc32") function attribute, x86

    Enable/disable the generation of the CRC32 instructions.

  :samp:`cx16` :samp:`no-cx16`

    .. index:: target("cx16") function attribute, x86

    Enable/disable the generation of the CMPXCHG16B instructions.

  :samp:`default`

    .. index:: target("default") function attribute, x86

    See :ref:`function-multiversioning`, where it is used to specify the
    default function version.

  :samp:`f16c` :samp:`no-f16c`

    .. index:: target("f16c") function attribute, x86

    Enable/disable the generation of the F16C instructions.

  :samp:`fma` :samp:`no-fma`

    .. index:: target("fma") function attribute, x86

    Enable/disable the generation of the FMA instructions.

  :samp:`fma4` :samp:`no-fma4`

    .. index:: target("fma4") function attribute, x86

    Enable/disable the generation of the FMA4 instructions.

  :samp:`fsgsbase` :samp:`no-fsgsbase`

    .. index:: target("fsgsbase") function attribute, x86

    Enable/disable the generation of the FSGSBASE instructions.

  :samp:`fxsr` :samp:`no-fxsr`

    .. index:: target("fxsr") function attribute, x86

    Enable/disable the generation of the FXSR instructions.

  :samp:`gfni` :samp:`no-gfni`

    .. index:: target("gfni") function attribute, x86

    Enable/disable the generation of the GFNI instructions.

  :samp:`hle` :samp:`no-hle`

    .. index:: target("hle") function attribute, x86

    Enable/disable the generation of the HLE instruction prefixes.

  :samp:`lwp` :samp:`no-lwp`

    .. index:: target("lwp") function attribute, x86

    Enable/disable the generation of the LWP instructions.

  :samp:`lzcnt` :samp:`no-lzcnt`

    .. index:: target("lzcnt") function attribute, x86

    Enable/disable the generation of the LZCNT instructions.

  :samp:`mmx` :samp:`no-mmx`

    .. index:: target("mmx") function attribute, x86

    Enable/disable the generation of the MMX instructions.

  :samp:`movbe` :samp:`no-movbe`

    .. index:: target("movbe") function attribute, x86

    Enable/disable the generation of the MOVBE instructions.

  :samp:`movdir64b` :samp:`no-movdir64b`

    .. index:: target("movdir64b") function attribute, x86

    Enable/disable the generation of the MOVDIR64B instructions.

  :samp:`movdiri` :samp:`no-movdiri`

    .. index:: target("movdiri") function attribute, x86

    Enable/disable the generation of the MOVDIRI instructions.

  :samp:`mwait` :samp:`no-mwait`

    .. index:: target("mwait") function attribute, x86

    Enable/disable the generation of the MWAIT and MONITOR instructions.

  :samp:`mwaitx` :samp:`no-mwaitx`

    .. index:: target("mwaitx") function attribute, x86

    Enable/disable the generation of the MWAITX instructions.

  :samp:`pclmul` :samp:`no-pclmul`

    .. index:: target("pclmul") function attribute, x86

    Enable/disable the generation of the PCLMUL instructions.

  :samp:`pconfig` :samp:`no-pconfig`

    .. index:: target("pconfig") function attribute, x86

    Enable/disable the generation of the PCONFIG instructions.

  :samp:`pku` :samp:`no-pku`

    .. index:: target("pku") function attribute, x86

    Enable/disable the generation of the PKU instructions.

  :samp:`popcnt` :samp:`no-popcnt`

    .. index:: target("popcnt") function attribute, x86

    Enable/disable the generation of the POPCNT instruction.

  :samp:`prefetchwt1` :samp:`no-prefetchwt1`

    .. index:: target("prefetchwt1") function attribute, x86

    Enable/disable the generation of the PREFETCHWT1 instructions.

  :samp:`prfchw` :samp:`no-prfchw`

    .. index:: target("prfchw") function attribute, x86

    Enable/disable the generation of the PREFETCHW instruction.

  :samp:`ptwrite` :samp:`no-ptwrite`

    .. index:: target("ptwrite") function attribute, x86

    Enable/disable the generation of the PTWRITE instructions.

  :samp:`rdpid` :samp:`no-rdpid`

    .. index:: target("rdpid") function attribute, x86

    Enable/disable the generation of the RDPID instructions.

  :samp:`rdrnd` :samp:`no-rdrnd`

    .. index:: target("rdrnd") function attribute, x86

    Enable/disable the generation of the RDRND instructions.

  :samp:`rdseed` :samp:`no-rdseed`

    .. index:: target("rdseed") function attribute, x86

    Enable/disable the generation of the RDSEED instructions.

  :samp:`rtm` :samp:`no-rtm`

    .. index:: target("rtm") function attribute, x86

    Enable/disable the generation of the RTM instructions.

  :samp:`sahf` :samp:`no-sahf`

    .. index:: target("sahf") function attribute, x86

    Enable/disable the generation of the SAHF instructions.

  :samp:`sgx` :samp:`no-sgx`

    .. index:: target("sgx") function attribute, x86

    Enable/disable the generation of the SGX instructions.

  :samp:`sha` :samp:`no-sha`

    .. index:: target("sha") function attribute, x86

    Enable/disable the generation of the SHA instructions.

  :samp:`shstk` :samp:`no-shstk`

    .. index:: target("shstk") function attribute, x86

    Enable/disable the shadow stack built-in functions from CET.

  :samp:`sse` :samp:`no-sse`

    .. index:: target("sse") function attribute, x86

    Enable/disable the generation of the SSE instructions.

  :samp:`sse2` :samp:`no-sse2`

    .. index:: target("sse2") function attribute, x86

    Enable/disable the generation of the SSE2 instructions.

  :samp:`sse3` :samp:`no-sse3`

    .. index:: target("sse3") function attribute, x86

    Enable/disable the generation of the SSE3 instructions.

  :samp:`sse4` :samp:`no-sse4`

    .. index:: target("sse4") function attribute, x86

    Enable/disable the generation of the SSE4 instructions (both SSE4.1
    and SSE4.2).

  :samp:`sse4.1` :samp:`no-sse4.1`

    .. index:: target("sse4.1") function attribute, x86

    Enable/disable the generation of the sse4.1 instructions.

  :samp:`sse4.2` :samp:`no-sse4.2`

    .. index:: target("sse4.2") function attribute, x86

    Enable/disable the generation of the sse4.2 instructions.

  :samp:`sse4a` :samp:`no-sse4a`

    .. index:: target("sse4a") function attribute, x86

    Enable/disable the generation of the SSE4A instructions.

  :samp:`ssse3` :samp:`no-ssse3`

    .. index:: target("ssse3") function attribute, x86

    Enable/disable the generation of the SSSE3 instructions.

  :samp:`tbm` :samp:`no-tbm`

    .. index:: target("tbm") function attribute, x86

    Enable/disable the generation of the TBM instructions.

  :samp:`vaes` :samp:`no-vaes`

    .. index:: target("vaes") function attribute, x86

    Enable/disable the generation of the VAES instructions.

  :samp:`vpclmulqdq` :samp:`no-vpclmulqdq`

    .. index:: target("vpclmulqdq") function attribute, x86

    Enable/disable the generation of the VPCLMULQDQ instructions.

  :samp:`waitpkg` :samp:`no-waitpkg`

    .. index:: target("waitpkg") function attribute, x86

    Enable/disable the generation of the WAITPKG instructions.

  :samp:`wbnoinvd` :samp:`no-wbnoinvd`

    .. index:: target("wbnoinvd") function attribute, x86

    Enable/disable the generation of the WBNOINVD instructions.

  :samp:`xop` :samp:`no-xop`

    .. index:: target("xop") function attribute, x86

    Enable/disable the generation of the XOP instructions.

  :samp:`xsave` :samp:`no-xsave`

    .. index:: target("xsave") function attribute, x86

    Enable/disable the generation of the XSAVE instructions.

  :samp:`xsavec` :samp:`no-xsavec`

    .. index:: target("xsavec") function attribute, x86

    Enable/disable the generation of the XSAVEC instructions.

  :samp:`xsaveopt` :samp:`no-xsaveopt`

    .. index:: target("xsaveopt") function attribute, x86

    Enable/disable the generation of the XSAVEOPT instructions.

  :samp:`xsaves` :samp:`no-xsaves`

    .. index:: target("xsaves") function attribute, x86

    Enable/disable the generation of the XSAVES instructions.

  :samp:`amx-tile` :samp:`no-amx-tile`

    .. index:: target("amx-tile") function attribute, x86

    Enable/disable the generation of the AMX-TILE instructions.

  :samp:`amx-int8` :samp:`no-amx-int8`

    .. index:: target("amx-int8") function attribute, x86

    Enable/disable the generation of the AMX-INT8 instructions.

  :samp:`amx-bf16` :samp:`no-amx-bf16`

    .. index:: target("amx-bf16") function attribute, x86

    Enable/disable the generation of the AMX-BF16 instructions.

  :samp:`uintr` :samp:`no-uintr`

    .. index:: target("uintr") function attribute, x86

    Enable/disable the generation of the UINTR instructions.

  :samp:`hreset` :samp:`no-hreset`

    .. index:: target("hreset") function attribute, x86

    Enable/disable the generation of the HRESET instruction.

  :samp:`kl` :samp:`no-kl`

    .. index:: target("kl") function attribute, x86

    Enable/disable the generation of the KEYLOCKER instructions.

  :samp:`widekl` :samp:`no-widekl`

    .. index:: target("widekl") function attribute, x86

    Enable/disable the generation of the WIDEKL instructions.

  :samp:`avxvnni` :samp:`no-avxvnni`

    .. index:: target("avxvnni") function attribute, x86

    Enable/disable the generation of the AVXVNNI instructions.

  :samp:`cld` :samp:`no-cld`

    .. index:: target("cld") function attribute, x86

    Enable/disable the generation of the CLD before string moves.

  :samp:`fancy-math-387` :samp:`no-fancy-math-387`

    .. index:: target("fancy-math-387") function attribute, x86

    Enable/disable the generation of the ``sin``, ``cos``, and
    ``sqrt`` instructions on the 387 floating-point unit.

  :samp:`ieee-fp` :samp:`no-ieee-fp`

    .. index:: target("ieee-fp") function attribute, x86

    Enable/disable the generation of floating point that depends on IEEE arithmetic.

  :samp:`inline-all-stringops` :samp:`no-inline-all-stringops`

    .. index:: target("inline-all-stringops") function attribute, x86

    Enable/disable inlining of string operations.

  :samp:`inline-stringops-dynamically` :samp:`no-inline-stringops-dynamically`

    .. index:: target("inline-stringops-dynamically") function attribute, x86

    Enable/disable the generation of the inline code to do small string
    operations and calling the library routines for large operations.

  :samp:`align-stringops` :samp:`no-align-stringops`

    .. index:: target("align-stringops") function attribute, x86

    Do/do not align destination of inlined string operations.

  :samp:`recip` :samp:`no-recip`

    .. index:: target("recip") function attribute, x86

    Enable/disable the generation of RCPSS, RCPPS, RSQRTSS and RSQRTPS
    instructions followed an additional Newton-Raphson step instead of
    doing a floating-point division.

  :samp:`general-regs-only`

    .. index:: target("general-regs-only") function attribute, x86

    Generate code which uses only the general registers.

  :samp:`arch={ARCH}`

    .. index:: target("arch=ARCH") function attribute, x86

    Specify the architecture to generate code for in compiling the function.

  :samp:`tune={TUNE}`

    .. index:: target("tune=TUNE") function attribute, x86

    Specify the architecture to tune for in compiling the function.

  :samp:`fpmath={FPMATH}`

    .. index:: target("fpmath=FPMATH") function attribute, x86

    Specify which floating-point unit to use.  You must specify the
    ``target("fpmath=sse,387")`` option as
    ``target("fpmath=sse+387")`` because the comma would separate
    different options.

  :samp:`prefer-vector-width={OPT}`

    .. index:: prefer-vector-width function attribute, x86

    On x86 targets, the ``prefer-vector-width`` attribute informs the
    compiler to use :samp:`{OPT}` -bit vector width in instructions
    instead of the default on the selected platform.

    Valid :samp:`{OPT}` values are:

    :samp:`none`
      No extra limitations applied to GCC other than defined by the selected platform.

    :samp:`128`
      Prefer 128-bit vector width for instructions.

    :samp:`256`
      Prefer 256-bit vector width for instructions.

    :samp:`512`
      Prefer 512-bit vector width for instructions.

      On the x86, the inliner does not inline a
    function that has different target options than the caller, unless the
    callee has a subset of the target options of the caller.  For example
    a function declared with ``target("sse3")`` can inline a function
    with ``target("sse2")``, since ``-msse3`` implies ``-msse2``.

.. option:: indirect_branch("choice")

  .. index:: indirect_branch function attribute, x86

  On x86 targets, the ``indirect_branch`` attribute causes the compiler
  to convert indirect call and jump with :samp:`{choice}`.  :samp:`keep`
  keeps indirect call and jump unmodified.  :samp:`thunk` converts indirect
  call and jump to call and return thunk.  :samp:`thunk-inline` converts
  indirect call and jump to inlined call and return thunk.
  :samp:`thunk-extern` converts indirect call and jump to external call
  and return thunk provided in a separate object file.

.. option:: function_return("choice")

  .. index:: function_return function attribute, x86

  On x86 targets, the ``function_return`` attribute causes the compiler
  to convert function return with :samp:`{choice}`.  :samp:`keep` keeps function
  return unmodified.  :samp:`thunk` converts function return to call and
  return thunk.  :samp:`thunk-inline` converts function return to inlined
  call and return thunk.  :samp:`thunk-extern` converts function return to
  external call and return thunk provided in a separate object file.

.. option:: nocf_check

  .. index:: nocf_check function attribute

  The ``nocf_check`` attribute on a function is used to inform the
  compiler that the function's prologue should not be instrumented when
  compiled with the :option:`-fcf-protection`:samp:`=branch` option.  The
  compiler assumes that the function's address is a valid target for a
  control-flow transfer.

  The ``nocf_check`` attribute on a type of pointer to function is
  used to inform the compiler that a call through the pointer should
  not be instrumented when compiled with the
  :option:`-fcf-protection`:samp:`=branch` option.  The compiler assumes
  that the function's address from the pointer is a valid target for
  a control-flow transfer.  A direct function call through a function
  name is assumed to be a safe call thus direct calls are not
  instrumented by the compiler.

  The ``nocf_check`` attribute is applied to an object's type.
  In case of assignment of a function address or a function pointer to
  another pointer, the attribute is not carried over from the right-hand
  object's type; the type of left-hand object stays unchanged.  The
  compiler checks for ``nocf_check`` attribute mismatch and reports
  a warning in case of mismatch.

  .. code-block:: c++

    {
    int foo (void) __attribute__(nocf_check);
    void (*foo1)(void) __attribute__(nocf_check);
    void (*foo2)(void);

    /* foo's address is assumed to be valid.  */
    int
    foo (void) 

      /* This call site is not checked for control-flow 
         validity.  */
      (*foo1)();

      /* A warning is issued about attribute mismatch.  */
      foo1 = foo2; 

      /* This call site is still not checked.  */
      (*foo1)();

      /* This call site is checked.  */
      (*foo2)();

      /* A warning is issued about attribute mismatch.  */
      foo2 = foo1; 

      /* This call site is still checked.  */
      (*foo2)();

      return 0;
    }

.. option:: cf_check

  .. index:: cf_check function attribute, x86

  The ``cf_check`` attribute on a function is used to inform the
  compiler that ENDBR instruction should be placed at the function
  entry when :option:`-fcf-protection`:samp:`=branch` is enabled.

.. option:: indirect_return

  .. index:: indirect_return function attribute, x86

  The ``indirect_return`` attribute can be applied to a function,
  as well as variable or type of function pointer to inform the
  compiler that the function may return via indirect branch.

.. option:: fentry_name("name")

  .. index:: fentry_name function attribute, x86

  On x86 targets, the ``fentry_name`` attribute sets the function to
  call on function entry when function instrumentation is enabled
  with :option:`-pg -mfentry`. When :samp:`{name}` is nop then a 5 byte
  nop sequence is generated.

.. option:: fentry_section("name")

  .. index:: fentry_section function attribute, x86

  On x86 targets, the ``fentry_section`` attribute sets the name
  of the section to record function entry instrumentation calls in when
  enabled with :option:`-pg -mrecord-mcount`

.. _xstormy16-function-attributes:

Xstormy16 Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the Xstormy16 back end:

.. option:: interrupt

  .. index:: interrupt function attribute, Xstormy16

  Use this attribute to indicate
  that the specified function is an interrupt handler.  The compiler generates
  function entry and exit sequences suitable for use in an interrupt handler
  when this attribute is present.

