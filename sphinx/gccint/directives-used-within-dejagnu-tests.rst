.. _test-directives:

Directives used within DejaGnu tests
************************************

.. toctree::

  directives
  selectors
  effective-target-keywords
  add-options
  require-support
  final-actions

.. _directives:

Syntax and Descriptions of test directives
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Test directives appear within comments in a test source file and begin
with ``dg-``.  Some of these are defined within DejaGnu and others
are local to the GCC testsuite.

The order in which test directives appear in a test can be important:
directives local to GCC sometimes override information used by the
DejaGnu directives, which know nothing about the GCC directives, so the
DejaGnu directives must precede GCC directives.

Several test directives include selectors (see :ref:`selectors`)
which are usually preceded by the keyword ``target`` or ``xfail``.

Specify how to build the test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-do {do-what-keyword} [{ target/xfail {selector} }] }`
  :samp:`{do-what-keyword}` specifies how the test is compiled and whether
  it is executed.  It is one of:

  ``preprocess``
    Compile with :option:`-E` to run only the preprocessor.

  ``compile``
    Compile with :option:`-S` to produce an assembly code file.

  ``assemble``
    Compile with :option:`-c` to produce a relocatable object file.

  ``link``
    Compile, assemble, and link to produce an executable file.

  ``run``
    Produce and run an executable file, which is expected to return
    an exit code of 0.

    The default is ``compile``.  That can be overridden for a set of
  tests by redefining ``dg-do-what-default`` within the ``.exp``
  file for those tests.

  If the directive includes the optional :samp:`{ target {selector} }`
  then the test is skipped unless the target system matches the
  :samp:`{selector}`.

  If :samp:`{do-what-keyword}` is ``run`` and the directive includes
  the optional :samp:`{ xfail {selector} }` and the selector is met
  then the test is expected to fail.  The ``xfail`` clause is ignored
  for other values of :samp:`{do-what-keyword}` ; those tests can use
  directive ``dg-xfail-if``.

Specify additional compiler options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-options {options} [{ target {selector} }] }`
  This DejaGnu directive provides a list of compiler options, to be used
  if the target system matches :samp:`{selector}`, that replace the default
  options used for this set of tests.

:samp:`{ dg-add-options {feature} ... }`
  Add any compiler options that are needed to access certain features.
  This directive does nothing on targets that enable the features by
  default, or that don't provide them at all.  It must come after
  all ``dg-options`` directives.
  For supported values of :samp:`{feature}` see Add Options.

:samp:`{ dg-additional-options {options} [{ target {selector} }] }`
  This directive provides a list of compiler options, to be used
  if the target system matches :samp:`{selector}`, that are added to the default
  options used for this set of tests.

Modify the test timeout value
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The normal timeout limit, in seconds, is found by searching the
following in order:

* the value defined by an earlier ``dg-timeout`` directive in
  the test

* variable :samp:`{tool_timeout}` defined by the set of tests

* :samp:`{gcc}`,:samp:`{timeout}` set in the target board

* 300

:samp:`{ dg-timeout {n} [{target {selector} }] }`
  Set the time limit for the compilation and for the execution of the test
  to the specified number of seconds.

:samp:`{ dg-timeout-factor {x} [{ target {selector} }] }`
  Multiply the normal time limit for compilation and execution of the test
  by the specified floating-point factor.

Skip a test for some targets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-skip-if {comment} { {selector} } [{ {include-opts} } [{ {exclude-opts} }]] }`
  Arguments :samp:`{include-opts}` and :samp:`{exclude-opts}` are lists in which
  each element is a string of zero or more GCC options.
  Skip the test if all of the following conditions are met:

  * the test system is included in :samp:`{selector}`

  * for at least one of the option strings in :samp:`{include-opts}`,
    every option from that string is in the set of options with which
    the test would be compiled; use :samp:`"*"` for an :samp:`{include-opts}` list
    that matches any options; that is the default if :samp:`{include-opts}` is
    not specified

  * for each of the option strings in :samp:`{exclude-opts}`, at least one
    option from that string is not in the set of options with which the test
    would be compiled; use :samp:`""` for an empty :samp:`{exclude-opts}` list;
    that is the default if :samp:`{exclude-opts}` is not specified

  For example, to skip a test if option ``-Os`` is present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "-Os" } { "" } } */

  To skip a test if both options ``-O2`` and ``-g`` are present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "-O2 -g" } { "" } } */

  To skip a test if either ``-O2`` or ``-O3`` is present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "-O2" "-O3" } { "" } } */

  To skip a test unless option ``-Os`` is present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "*" } { "-Os" } } */

  To skip a test if either ``-O2`` or ``-O3`` is used with ``-g``
  but not if ``-fpic`` is also present:

  .. code-block:: c++

    /* { dg-skip-if "" { *-*-* }  { "-O2 -g" "-O3 -g" } { "-fpic" } } */

:samp:`{ dg-require-effective-target {keyword} [{ target {selector} }] }`
  Skip the test if the test target, including current multilib flags,
  is not covered by the effective-target keyword.
  If the directive includes the optional :samp:`{ {selector} }`
  then the effective-target test is only performed if the target system
  matches the :samp:`{selector}`.
  This directive must appear after any ``dg-do`` directive in the test
  and before any ``dg-additional-sources`` directive.
  See :ref:`effective-target-keywords`.

:samp:`{ dg-require-{support} args }`
  Skip the test if the target does not provide the required support.
  These directives must appear after any ``dg-do`` directive in the test
  and before any ``dg-additional-sources`` directive.
  They require at least one argument, which can be an empty string if the
  specific procedure does not examine the argument.
  See :ref:`require-support`, for a complete list of these directives.

Expect a test to fail for some targets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-xfail-if {comment} { {selector} } [{ {include-opts} } [{ {exclude-opts} }]] }`
  Expect the test to fail if the conditions (which are the same as for
  ``dg-skip-if`` ) are met.  This does not affect the execute step.

:samp:`{ dg-xfail-run-if {comment} { {selector} } [{ {include-opts} } [{ {exclude-opts} }]] }`
  Expect the execute step of a test to fail if the conditions (which are
  the same as for ``dg-skip-if`` ) are met.

Expect the compiler to crash
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-ice {comment} [{ {selector} } [{ {include-opts} } [{ {exclude-opts} }]]] }`
  Expect the compiler to crash with an internal compiler error and return
  a nonzero exit status if the conditions (which are the same as for
  ``dg-skip-if`` ) are met.  Used for tests that test bugs that have not been
  fixed yet.

Expect the test executable to fail
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-shouldfail {comment} [{ {selector} } [{ {include-opts} } [{ {exclude-opts} }]]] }`
  Expect the test executable to return a nonzero exit status if the
  conditions (which are the same as for ``dg-skip-if`` ) are met.

Verify compiler messages
~~~~~~~~~~~~~~~~~~~~~~~~

Where :samp:`{line}` is an accepted argument for these commands, a value of :samp:`0`
can be used if there is no line associated with the message.

:samp:`{ dg-error {regexp} [{comment} [{ target/xfail {selector} } [{line}] ]] }`
  This DejaGnu directive appears on a source line that is expected to get
  an error message, or else specifies the source line associated with the
  message.  If there is no message for that line or if the text of that
  message is not matched by :samp:`{regexp}` then the check fails and
  :samp:`{comment}` is included in the ``FAIL`` message.  The check does
  not look for the string :samp:`error` unless it is part of :samp:`{regexp}`.

:samp:`{ dg-warning {regexp} [{comment} [{ target/xfail {selector} } [{line}] ]] }`
  This DejaGnu directive appears on a source line that is expected to get
  a warning message, or else specifies the source line associated with the
  message.  If there is no message for that line or if the text of that
  message is not matched by :samp:`{regexp}` then the check fails and
  :samp:`{comment}` is included in the ``FAIL`` message.  The check does
  not look for the string :samp:`warning` unless it is part of :samp:`{regexp}`.

:samp:`{ dg-message {regexp} [{comment} [{ target/xfail {selector} } [{line}] ]] }`
  The line is expected to get a message other than an error or warning.
  If there is no message for that line or if the text of that message is
  not matched by :samp:`{regexp}` then the check fails and :samp:`{comment}` is
  included in the ``FAIL`` message.

:samp:`{ dg-bogus {regexp} [{comment} [{ target/xfail {selector} } [{line}] ]] }`
  This DejaGnu directive appears on a source line that should not get a
  message matching :samp:`{regexp}`, or else specifies the source line
  associated with the bogus message.  It is usually used with :samp:`xfail`
  to indicate that the message is a known problem for a particular set of
  targets.

:samp:`{ dg-line {linenumvar} }`
  This DejaGnu directive sets the variable :samp:`{linenumvar}` to the line number of
  the source line.  The variable :samp:`{linenumvar}` can then be used in subsequent
  ``dg-error``, ``dg-warning``, ``dg-message`` and ``dg-bogus``
  directives.  For example:

  .. code-block:: c++

    int a;   /* { dg-line first_def_a } */
    float a; /* { dg-error "conflicting types of" } */
    /* { dg-message "previous declaration of" "" { target *-*-* } first_def_a } */

:samp:`{ dg-excess-errors {comment} [{ target/xfail {selector} }] }`
  This DejaGnu directive indicates that the test is expected to fail due
  to compiler messages that are not handled by :samp:`dg-error`,
  :samp:`dg-warning` or :samp:`dg-bogus`.  For this directive :samp:`xfail`
  has the same effect as :samp:`target`.

:samp:`{ dg-prune-output {regexp} }`
  Prune messages matching :samp:`{regexp}` from the test output.

Verify output of the test executable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-output {regexp} [{ target/xfail {selector} }] }`
  This DejaGnu directive compares :samp:`{regexp}` to the combined output
  that the test executable writes to stdout and stderr.

Specify environment variables for a test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-set-compiler-env-var {var_name} "{var_value}" }`
  Specify that the environment variable :samp:`{var_name}` needs to be set
  to :samp:`{var_value}` before invoking the compiler on the test file.

:samp:`{ dg-set-target-env-var {var_name} "{var_value}" }`
  Specify that the environment variable :samp:`{var_name}` needs to be set
  to :samp:`{var_value}` before execution of the program created by the test.

Specify additional files for a test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-additional-files "{filelist}" }`
  Specify additional files, other than source files, that must be copied
  to the system where the compiler runs.

:samp:`{ dg-additional-sources "{filelist}" }`
  Specify additional source files to appear in the compile line
  following the main test file.

Add checks at the end of a test
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`{ dg-final { {local-directive} } }`
  This DejaGnu directive is placed within a comment anywhere in the
  source file and is processed after the test has been compiled and run.
  Multiple :samp:`dg-final` commands are processed in the order in which
  they appear in the source file.  See :ref:`final-actions`, for a list
  of directives that can be used within ``dg-final``.

  .. _selectors:

Selecting targets to which a test applies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Several test directives include :samp:`{selector}` s to limit the targets
for which a test is run or to declare that a test is expected to fail
on particular targets.

A selector is:

* one or more target triplets, possibly including wildcard characters;
  use :samp:`*-*-*` to match any target

* a single effective-target keyword (see :ref:`effective-target-keywords`)

* a list of compiler options that should be included or excluded
  (as described in more detail below)

* a logical expression

Depending on the context, the selector specifies whether a test is
skipped and reported as unsupported or is expected to fail.  A context
that allows either :samp:`target` or :samp:`xfail` also allows
:samp:`{ target {selector1} xfail {selector2} }`
to skip the test for targets that don't match :samp:`{selector1}` and the
test to fail for targets that match :samp:`{selector2}`.

A selector expression appears within curly braces and uses a single
logical operator: one of :samp:`!`, :samp:`&&`, or :samp:`||`.  An
operand is one of the following:

* another selector expression, in curly braces

* an effective-target keyword, such as ``lp64``

* a single target triplet

* a list of target triplets within quotes or curly braces

* one of the following:

  :samp:`{ any-opts {opt1} ... {optn} }`
    Each of :samp:`{opt1}` to :samp:`{optn}` is a space-separated list of option globs.
    The selector expression evaluates to true if, for one of these strings,
    every glob in the string matches an option that was passed to the compiler.
    For example:

    .. code-block:: c++

      { any-opts "-O3 -flto" "-O[2g]" }

    is true if any of the following are true:

    * :option:`-O2` was passed to the compiler

    * :option:`-Og` was passed to the compiler

    * both :option:`-O3` and :option:`-flto` were passed to the compiler

    This kind of selector can only be used within ``dg-final`` directives.
    Use ``dg-skip-if``, ``dg-xfail-if`` or ``dg-xfail-run-if`` to
    skip whole tests based on options, or to mark them as expected to fail
    with certain options.

  :samp:`{ no-opts {opt1} ... {optn} }`
    As for ``any-opts`` above, each of :samp:`{opt1}` to :samp:`{optn}` is a
    space-separated list of option globs.  The selector expression
    evaluates to true if, for all of these strings, there is at least
    one glob that does not match an option that was passed to the compiler.
    It is shorthand for:

    .. code-block:: c++

      { ! { any-opts opt1 ... optn } }

    For example:

    .. code-block:: c++

      { no-opts "-O3 -flto" "-O[2g]" }

    is true if all of the following are true:

    * :option:`-O2` was not passed to the compiler

    * :option:`-Og` was not passed to the compiler

    * at least one of :option:`-O3` or :option:`-flto` was not passed to the compiler

    Like ``any-opts``, this kind of selector can only be used within
    ``dg-final`` directives.

Here are some examples of full target selectors:

.. code-block:: c++

  { target { ! "hppa*-*-* ia64*-*-*" } }
  { target { powerpc*-*-* && lp64 } }
  { xfail { lp64 || vect_no_align } }
  { xfail { aarch64*-*-* && { any-opts "-O2" } } }

.. _effective-target-keywords:

Keywords describing target attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Effective-target keywords identify sets of targets that support
particular functionality.  They are used to limit tests to be run only
for particular targets, or to specify that particular sets of targets
are expected to fail some tests.

Effective-target keywords are defined in lib/target-supports.exp in
the GCC testsuite, with the exception of those that are documented as
being local to a particular test directory.

The :samp:`effective target` takes into account all of the compiler options
with which the test will be compiled, including the multilib options.
By convention, keywords ending in ``_nocache`` can also include options
specified for the particular test in an earlier ``dg-options`` or
``dg-add-options`` directive.

Endianness
~~~~~~~~~~

``be``
  Target uses big-endian memory order for multi-byte and multi-word data.

``le``
  Target uses little-endian memory order for multi-byte and multi-word data.

Data type sizes
~~~~~~~~~~~~~~~

``ilp32``
  Target has 32-bit ``int``, ``long``, and pointers.

``lp64``
  Target has 32-bit ``int``, 64-bit ``long`` and pointers.

``llp64``
  Target has 32-bit ``int`` and ``long``, 64-bit ``long long``
  and pointers.

``double64``
  Target has 64-bit ``double``.

``double64plus``
  Target has ``double`` that is 64 bits or longer.

``longdouble128``
  Target has 128-bit ``long double``.

``int32plus``
  Target has ``int`` that is at 32 bits or longer.

``int16``
  Target has ``int`` that is 16 bits or shorter.

``longlong64``
  Target has 64-bit ``long long``.

``long_neq_int``
  Target has ``int`` and ``long`` with different sizes.

``short_eq_int``
  Target has ``short`` and ``int`` with the same size.

``ptr_eq_short``
  Target has pointers ( ``void *`` ) and ``short`` with the same size.

``int_eq_float``
  Target has ``int`` and ``float`` with the same size.

``ptr_eq_long``
  Target has pointers ( ``void *`` ) and ``long`` with the same size.

``large_double``
  Target supports ``double`` that is longer than ``float``.

``large_long_double``
  Target supports ``long double`` that is longer than ``double``.

``ptr32plus``
  Target has pointers that are 32 bits or longer.

``size20plus``
  Target has a 20-bit or larger address space, so supports at least
  16-bit array and structure sizes.

``size24plus``
  Target has a 24-bit or larger address space, so supports at least
  20-bit array and structure sizes.

``size32plus``
  Target has a 32-bit or larger address space, so supports at least
  24-bit array and structure sizes.

``4byte_wchar_t``
  Target has ``wchar_t`` that is at least 4 bytes.

:samp:`float{n}`
  Target has the ``_Floatn`` type.

:samp:`float{n}x`
  Target has the ``_Floatnx`` type.

:samp:`float{n}_runtime`
  Target has the ``_Floatn`` type, including runtime support
  for any options added with ``dg-add-options``.

:samp:`float{n}x_runtime`
  Target has the ``_Floatnx`` type, including runtime support
  for any options added with ``dg-add-options``.

``floatn_nx_runtime``
  Target has runtime support for any options added with
  ``dg-add-options`` for any ``_Floatn`` or
  ``_Floatnx`` type.

``inf``
  Target supports floating point infinite ( ``inf`` ) for type
  ``double``.

``inff``
  Target supports floating point infinite ( ``inf`` ) for type
  ``float``.

Fortran-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``fortran_integer_16``
  Target supports Fortran ``integer`` that is 16 bytes or longer.

``fortran_real_10``
  Target supports Fortran ``real`` that is 10 bytes or longer.

``fortran_real_16``
  Target supports Fortran ``real`` that is 16 bytes or longer.

``fortran_large_int``
  Target supports Fortran ``integer`` kinds larger than ``integer(8)``.

``fortran_large_real``
  Target supports Fortran ``real`` kinds larger than ``real(8)``.

Vector-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~

``vect_align_stack_vars``
  The target's ABI allows stack variables to be aligned to the preferred
  vector alignment.

``vect_avg_qi``
  Target supports both signed and unsigned averaging operations on vectors
  of bytes.

``vect_mulhrs_hi``
  Target supports both signed and unsigned multiply-high-with-round-and-scale
  operations on vectors of half-words.

``vect_sdiv_pow2_si``
  Target supports signed division by constant power-of-2 operations
  on vectors of 4-byte integers.

``vect_condition``
  Target supports vector conditional operations.

``vect_cond_mixed``
  Target supports vector conditional operations where comparison operands
  have different type from the value operands.

``vect_double``
  Target supports hardware vectors of ``double``.

``vect_double_cond_arith``
  Target supports conditional addition, subtraction, multiplication,
  division, minimum and maximum on vectors of ``double``, via the
  ``cond_`` optabs.

``vect_element_align_preferred``
  The target's preferred vector alignment is the same as the element
  alignment.

``vect_float``
  Target supports hardware vectors of ``float`` when
  :option:`-funsafe-math-optimizations` is in effect.

``vect_float_strict``
  Target supports hardware vectors of ``float`` when
  :option:`-funsafe-math-optimizations` is not in effect.
  This implies ``vect_float``.

``vect_int``
  Target supports hardware vectors of ``int``.

``vect_long``
  Target supports hardware vectors of ``long``.

``vect_long_long``
  Target supports hardware vectors of ``long long``.

``vect_check_ptrs``
  Target supports the ``check_raw_ptrs`` and ``check_war_ptrs``
  optabs on vectors.

``vect_fully_masked``
  Target supports fully-masked (also known as fully-predicated) loops,
  so that vector loops can handle partial as well as full vectors.

``vect_masked_load``
  Target supports vector masked loads.

``vect_masked_store``
  Target supports vector masked stores.

``vect_scatter_store``
  Target supports vector scatter stores.

``vect_aligned_arrays``
  Target aligns arrays to vector alignment boundary.

``vect_hw_misalign``
  Target supports a vector misalign access.

``vect_no_align``
  Target does not support a vector alignment mechanism.

``vect_peeling_profitable``
  Target might require to peel loops for alignment purposes.

``vect_no_int_min_max``
  Target does not support a vector min and max instruction on ``int``.

``vect_no_int_add``
  Target does not support a vector add instruction on ``int``.

``vect_no_bitwise``
  Target does not support vector bitwise instructions.

``vect_bool_cmp``
  Target supports comparison of ``bool`` vectors for at least one
  vector length.

``vect_char_add``
  Target supports addition of ``char`` vectors for at least one
  vector length.

``vect_char_mult``
  Target supports ``vector char`` multiplication.

``vect_short_mult``
  Target supports ``vector short`` multiplication.

``vect_int_mult``
  Target supports ``vector int`` multiplication.

``vect_long_mult``
  Target supports 64 bit ``vector long`` multiplication.

``vect_extract_even_odd``
  Target supports vector even/odd element extraction.

``vect_extract_even_odd_wide``
  Target supports vector even/odd element extraction of vectors with elements
  ``SImode`` or larger.

``vect_interleave``
  Target supports vector interleaving.

``vect_strided``
  Target supports vector interleaving and extract even/odd.

``vect_strided_wide``
  Target supports vector interleaving and extract even/odd for wide
  element types.

``vect_perm``
  Target supports vector permutation.

``vect_perm_byte``
  Target supports permutation of vectors with 8-bit elements.

``vect_perm_short``
  Target supports permutation of vectors with 16-bit elements.

``vect_perm3_byte``
  Target supports permutation of vectors with 8-bit elements, and for the
  default vector length it is possible to permute:

  .. code-block:: c++

    { a0, a1, a2, b0, b1, b2, ... }

  to:

  .. code-block:: c++

    { a0, a0, a0, b0, b0, b0, ... }
    { a1, a1, a1, b1, b1, b1, ... }
    { a2, a2, a2, b2, b2, b2, ... }

  using only two-vector permutes, regardless of how long the sequence is.

``vect_perm3_int``
  Like ``vect_perm3_byte``, but for 32-bit elements.

``vect_perm3_short``
  Like ``vect_perm3_byte``, but for 16-bit elements.

``vect_shift``
  Target supports a hardware vector shift operation.

``vect_unaligned_possible``
  Target prefers vectors to have an alignment greater than element
  alignment, but also allows unaligned vector accesses in some
  circumstances.

``vect_variable_length``
  Target has variable-length vectors.

``vect_widen_sum_hi_to_si``
  Target supports a vector widening summation of ``short`` operands
  into ``int`` results, or can promote (unpack) from ``short``
  to ``int``.

``vect_widen_sum_qi_to_hi``
  Target supports a vector widening summation of ``char`` operands
  into ``short`` results, or can promote (unpack) from ``char``
  to ``short``.

``vect_widen_sum_qi_to_si``
  Target supports a vector widening summation of ``char`` operands
  into ``int`` results.

``vect_widen_mult_qi_to_hi``
  Target supports a vector widening multiplication of ``char`` operands
  into ``short`` results, or can promote (unpack) from ``char`` to
  ``short`` and perform non-widening multiplication of ``short``.

``vect_widen_mult_hi_to_si``
  Target supports a vector widening multiplication of ``short`` operands
  into ``int`` results, or can promote (unpack) from ``short`` to
  ``int`` and perform non-widening multiplication of ``int``.

``vect_widen_mult_si_to_di_pattern``
  Target supports a vector widening multiplication of ``int`` operands
  into ``long`` results.

``vect_sdot_qi``
  Target supports a vector dot-product of ``signed char``.

``vect_udot_qi``
  Target supports a vector dot-product of ``unsigned char``.

``vect_sdot_hi``
  Target supports a vector dot-product of ``signed short``.

``vect_udot_hi``
  Target supports a vector dot-product of ``unsigned short``.

``vect_pack_trunc``
  Target supports a vector demotion (packing) of ``short`` to ``char``
  and from ``int`` to ``short`` using modulo arithmetic.

``vect_unpack``
  Target supports a vector promotion (unpacking) of ``char`` to ``short``
  and from ``char`` to ``int``.

``vect_intfloat_cvt``
  Target supports conversion from ``signed int`` to ``float``.

``vect_uintfloat_cvt``
  Target supports conversion from ``unsigned int`` to ``float``.

``vect_floatint_cvt``
  Target supports conversion from ``float`` to ``signed int``.

``vect_floatuint_cvt``
  Target supports conversion from ``float`` to ``unsigned int``.

``vect_intdouble_cvt``
  Target supports conversion from ``signed int`` to ``double``.

``vect_doubleint_cvt``
  Target supports conversion from ``double`` to ``signed int``.

``vect_max_reduc``
  Target supports max reduction for vectors.

``vect_sizes_16B_8B``
  Target supports 16- and 8-bytes vectors.

``vect_sizes_32B_16B``
  Target supports 32- and 16-bytes vectors.

``vect_logical_reduc``
  Target supports AND, IOR and XOR reduction on vectors.

``vect_fold_extract_last``
  Target supports the ``fold_extract_last`` optab.

``vect_len_load_store``
  Target supports the ``len_load`` and ``len_store`` optabs.

``vect_partial_vectors_usage_1``
  Target supports loop vectorization with partial vectors and
  ``vect-partial-vector-usage`` is set to 1.

``vect_partial_vectors_usage_2``
  Target supports loop vectorization with partial vectors and
  ``vect-partial-vector-usage`` is set to 2.

``vect_partial_vectors``
  Target supports loop vectorization with partial vectors and
  ``vect-partial-vector-usage`` is nonzero.

Thread Local Storage attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``tls``
  Target supports thread-local storage.

``tls_native``
  Target supports native (rather than emulated) thread-local storage.

``tls_runtime``
  Test system supports executing TLS executables.

Decimal floating point attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``dfp``
  Targets supports compiling decimal floating point extension to C.

``dfp_nocache``
  Including the options used to compile this particular test, the
  target supports compiling decimal floating point extension to C.

``dfprt``
  Test system can execute decimal floating point tests.

``dfprt_nocache``
  Including the options used to compile this particular test, the
  test system can execute decimal floating point tests.

``hard_dfp``
  Target generates decimal floating point instructions with current options.

ARM-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~

``arm32``
  ARM target generates 32-bit code.

``arm_little_endian``
  ARM target that generates little-endian code.

``arm_eabi``
  ARM target adheres to the ABI for the ARM Architecture.

``arm_fp_ok``
  .. _arm_fp_ok:
  ARM target defines ``__ARM_FP`` using ``-mfloat-abi=softfp`` or
  equivalent options.  Some multilibs may be incompatible with these
  options.

``arm_fp_dp_ok``
  .. _arm_fp_dp_ok:
  ARM target defines ``__ARM_FP`` with double-precision support using
  ``-mfloat-abi=softfp`` or equivalent options.  Some multilibs may
  be incompatible with these options.

``arm_hf_eabi``
  ARM target adheres to the VFP and Advanced SIMD Register Arguments
  variant of the ABI for the ARM Architecture (as selected with
  ``-mfloat-abi=hard`` ).

``arm_softfloat``
  ARM target uses emulated floating point operations.

``arm_hard_vfp_ok``
  ARM target supports ``-mfpu=vfp -mfloat-abi=hard``.
  Some multilibs may be incompatible with these options.

``arm_iwmmxt_ok``
  ARM target supports ``-mcpu=iwmmxt``.
  Some multilibs may be incompatible with this option.

``arm_neon``
  ARM target supports generating NEON instructions.

``arm_tune_string_ops_prefer_neon``
  Test CPU tune supports inlining string operations with NEON instructions.

``arm_neon_hw``
  Test system supports executing NEON instructions.

``arm_neonv2_hw``
  Test system supports executing NEON v2 instructions.

``arm_neon_ok``
  .. _arm_neon_ok:
  ARM Target supports ``-mfpu=neon -mfloat-abi=softfp`` or compatible
  options.  Some multilibs may be incompatible with these options.

``arm_neon_ok_no_float_abi``
  .. _arm_neon_ok_no_float_abi:
  ARM Target supports NEON with ``-mfpu=neon``, but without any
  -mfloat-abi= option.  Some multilibs may be incompatible with this
  option.

``arm_neonv2_ok``
  .. _arm_neonv2_ok:
  ARM Target supports ``-mfpu=neon-vfpv4 -mfloat-abi=softfp`` or compatible
  options.  Some multilibs may be incompatible with these options.

``arm_fp16_ok``
  .. _arm_fp16_ok:
  Target supports options to generate VFP half-precision floating-point
  instructions.  Some multilibs may be incompatible with these
  options.  This test is valid for ARM only.

``arm_fp16_hw``
  Target supports executing VFP half-precision floating-point
  instructions.  This test is valid for ARM only.

``arm_neon_fp16_ok``
  .. _arm_neon_fp16_ok:
  ARM Target supports ``-mfpu=neon-fp16 -mfloat-abi=softfp`` or compatible
  options, including ``-mfp16-format=ieee`` if necessary to obtain the
  ``__fp16`` type.  Some multilibs may be incompatible with these options.

``arm_neon_fp16_hw``
  Test system supports executing Neon half-precision float instructions.
  (Implies previous.)

``arm_fp16_alternative_ok``
  ARM target supports the ARM FP16 alternative format.  Some multilibs
  may be incompatible with the options needed.

``arm_fp16_none_ok``
  ARM target supports specifying none as the ARM FP16 format.

``arm_thumb1_ok``
  ARM target generates Thumb-1 code for ``-mthumb``.

``arm_thumb2_ok``
  ARM target generates Thumb-2 code for ``-mthumb``.

``arm_nothumb``
  ARM target that is not using Thumb.

``arm_vfp_ok``
  ARM target supports ``-mfpu=vfp -mfloat-abi=softfp``.
  Some multilibs may be incompatible with these options.

``arm_vfp3_ok``
  .. _arm_vfp3_ok:
  ARM target supports ``-mfpu=vfp3 -mfloat-abi=softfp``.
  Some multilibs may be incompatible with these options.

``arm_arch_v8a_hard_ok``
  .. _arm_arch_v8a_hard_ok:
  The compiler is targeting ``arm*-*-*`` and can compile and assemble code
  using the options ``-march=armv8-a -mfpu=neon-fp-armv8 -mfloat-abi=hard``.
  This is not enough to guarantee that linking works.

``arm_arch_v8a_hard_multilib``
  The compiler is targeting ``arm*-*-*`` and can build programs using
  the options ``-march=armv8-a -mfpu=neon-fp-armv8 -mfloat-abi=hard``.
  The target can also run the resulting binaries.

``arm_v8_vfp_ok``
  ARM target supports ``-mfpu=fp-armv8 -mfloat-abi=softfp``.
  Some multilibs may be incompatible with these options.

``arm_v8_neon_ok``
  ARM target supports ``-mfpu=neon-fp-armv8 -mfloat-abi=softfp``.
  Some multilibs may be incompatible with these options.

``arm_v8_1a_neon_ok``
  .. _arm_v8_1a_neon_ok:
  ARM target supports options to generate ARMv8.1-A Adv.SIMD instructions.
  Some multilibs may be incompatible with these options.

``arm_v8_1a_neon_hw``
  ARM target supports executing ARMv8.1-A Adv.SIMD instructions.  Some
  multilibs may be incompatible with the options needed.  Implies
  arm_v8_1a_neon_ok.

``arm_acq_rel``
  ARM target supports acquire-release instructions.

``arm_v8_2a_fp16_scalar_ok``
  .. _arm_v8_2a_fp16_scalar_ok:
  ARM target supports options to generate instructions for ARMv8.2-A and
  scalar instructions from the FP16 extension.  Some multilibs may be
  incompatible with these options.

``arm_v8_2a_fp16_scalar_hw``
  ARM target supports executing instructions for ARMv8.2-A and scalar
  instructions from the FP16 extension.  Some multilibs may be
  incompatible with these options.  Implies arm_v8_2a_fp16_neon_ok.

``arm_v8_2a_fp16_neon_ok``
  .. _arm_v8_2a_fp16_neon_ok:
  ARM target supports options to generate instructions from ARMv8.2-A with
  the FP16 extension.  Some multilibs may be incompatible with these
  options.  Implies arm_v8_2a_fp16_scalar_ok.

``arm_v8_2a_fp16_neon_hw``
  ARM target supports executing instructions from ARMv8.2-A with the FP16
  extension.  Some multilibs may be incompatible with these options.
  Implies arm_v8_2a_fp16_neon_ok and arm_v8_2a_fp16_scalar_hw.

``arm_v8_2a_dotprod_neon_ok``
  .. _arm_v8_2a_dotprod_neon_ok:
  ARM target supports options to generate instructions from ARMv8.2-A with
  the Dot Product extension. Some multilibs may be incompatible with these
  options.

``arm_v8_2a_dotprod_neon_hw``
  ARM target supports executing instructions from ARMv8.2-A with the Dot
  Product extension. Some multilibs may be incompatible with these options.
  Implies arm_v8_2a_dotprod_neon_ok.

``arm_fp16fml_neon_ok``
  .. _arm_fp16fml_neon_ok:
  ARM target supports extensions to generate the ``VFMAL`` and ``VFMLS``
  half-precision floating-point instructions available from ARMv8.2-A and
  onwards.  Some multilibs may be incompatible with these options.

``arm_v8_2a_bf16_neon_ok``
  ARM target supports options to generate instructions from ARMv8.2-A with
  the BFloat16 extension (bf16). Some multilibs may be incompatible with these
  options.

``arm_v8_2a_i8mm_ok``
  ARM target supports options to generate instructions from ARMv8.2-A with
  the 8-Bit Integer Matrix Multiply extension (i8mm). Some multilibs may be
  incompatible with these options.

``arm_v8_1m_mve_ok``
  ARM target supports options to generate instructions from ARMv8.1-M with
  the M-Profile Vector Extension (MVE). Some multilibs may be incompatible
  with these options.

``arm_v8_1m_mve_fp_ok``
  ARM target supports options to generate instructions from ARMv8.1-M with
  the Half-precision floating-point instructions (HP), Floating-point Extension
  (FP) along with M-Profile Vector Extension (MVE). Some multilibs may be
  incompatible with these options.

``arm_mve_hw``
  Test system supports executing MVE instructions.

``arm_v8m_main_cde``
  ARM target supports options to generate instructions from ARMv8-M with
  the Custom Datapath Extension (CDE). Some multilibs may be incompatible
  with these options.

``arm_v8m_main_cde_fp``
  ARM target supports options to generate instructions from ARMv8-M with
  the Custom Datapath Extension (CDE) and floating-point (VFP).
  Some multilibs may be incompatible with these options.

``arm_v8_1m_main_cde_mve``
  ARM target supports options to generate instructions from ARMv8.1-M with
  the Custom Datapath Extension (CDE) and M-Profile Vector Extension (MVE).
  Some multilibs may be incompatible with these options.

``arm_prefer_ldrd_strd``
  ARM target prefers ``LDRD`` and ``STRD`` instructions over
  ``LDM`` and ``STM`` instructions.

``arm_thumb1_movt_ok``
  ARM target generates Thumb-1 code for ``-mthumb`` with ``MOVW``
  and ``MOVT`` instructions available.

``arm_thumb1_cbz_ok``
  ARM target generates Thumb-1 code for ``-mthumb`` with
  ``CBZ`` and ``CBNZ`` instructions available.

``arm_divmod_simode``
  ARM target for which divmod transform is disabled, if it supports hardware
  div instruction.

``arm_cmse_ok``
  ARM target supports ARMv8-M Security Extensions, enabled by the ``-mcmse``
  option.

``arm_cmse_hw``
  Test system supports executing CMSE instructions.

``arm_coproc1_ok``
  .. _arm_coproc1_ok:
  ARM target supports the following coprocessor instructions: ``CDP``,
  ``LDC``, ``STC``, ``MCR`` and ``MRC``.

``arm_coproc2_ok``
  .. _arm_coproc2_ok:
  ARM target supports all the coprocessor instructions also listed as supported
  in arm_coproc1_ok in addition to the following: ``CDP2``, ``LDC2``,
  ``LDC2l``, ``STC2``, ``STC2l``, ``MCR2`` and ``MRC2``.

``arm_coproc3_ok``
  .. _arm_coproc3_ok:
  ARM target supports all the coprocessor instructions also listed as supported
  in arm_coproc2_ok in addition the following: ``MCRR`` and ``MRRC``.

``arm_coproc4_ok``
  ARM target supports all the coprocessor instructions also listed as supported
  in arm_coproc3_ok in addition the following: ``MCRR2`` and ``MRRC2``.

``arm_simd32_ok``
  .. _arm_simd32_ok:
  ARM Target supports options suitable for accessing the SIMD32 intrinsics from
  ``arm_acle.h``.
  Some multilibs may be incompatible with these options.

``arm_qbit_ok``
  .. _arm_qbit_ok:
  ARM Target supports options suitable for accessing the Q-bit manipulation
  intrinsics from ``arm_acle.h``.
  Some multilibs may be incompatible with these options.

``arm_dsp_ok``
  .. _arm_dsp_ok:
  ARM Target supports options suitable for accessing the DSP intrinsics
  from ``arm_acle.h``.
  Some multilibs may be incompatible with these options.

``arm_softfp_ok``
  .. _arm_softfp_ok:
  ARM target supports the ``-mfloat-abi=softfp`` option.

``arm_hard_ok``
  .. _arm_hard_ok:
  ARM target supports the ``-mfloat-abi=hard`` option.

``arm_v8_1_lob_ok``
  .. _arm_v8_1_lob_ok:
  ARM Target supports executing the Armv8.1-M Mainline Low Overhead Loop
  instructions ``DLS`` and ``LE``.
  Some multilibs may be incompatible with these options.

``arm_thumb2_no_arm_v8_1_lob``
  ARM target where Thumb-2 is used without options but does not support
  executing the Armv8.1-M Mainline Low Overhead Loop instructions
  ``DLS`` and ``LE``.

``arm_thumb2_ok_no_arm_v8_1_lob``
  ARM target generates Thumb-2 code for ``-mthumb`` but does not
  support executing the Armv8.1-M Mainline Low Overhead Loop
  instructions ``DLS`` and ``LE``.

AArch64-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``aarch64_asm_<ext>_ok``
  AArch64 assembler supports the architecture extension ``ext`` via the
  ``.arch_extension`` pseudo-op.

``aarch64_tiny``
  AArch64 target which generates instruction sequences for tiny memory model.

``aarch64_small``
  AArch64 target which generates instruction sequences for small memory model.

``aarch64_large``
  AArch64 target which generates instruction sequences for large memory model.

``aarch64_little_endian``
  AArch64 target which generates instruction sequences for little endian.

``aarch64_big_endian``
  AArch64 target which generates instruction sequences for big endian.

``aarch64_small_fpic``
  Binutils installed on test system supports relocation types required by -fpic
  for AArch64 small memory model.

``aarch64_sve_hw``
  AArch64 target that is able to generate and execute SVE code (regardless of
  whether it does so by default).

``aarch64_sve128_hw`` ``aarch64_sve256_hw`` ``aarch64_sve512_hw`` ``aarch64_sve1024_hw`` ``aarch64_sve2048_hw``
  Like ``aarch64_sve_hw``, but also test for an exact hardware vector length.

``aarch64_fjcvtzs_hw``
  AArch64 target that is able to generate and execute armv8.3-a FJCVTZS
  instruction.

MIPS-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~

``mips64``
  MIPS target supports 64-bit instructions.

``nomips16``
  MIPS target does not produce MIPS16 code.

``mips16_attribute``
  MIPS target can generate MIPS16 code.

``mips_loongson``
  MIPS target is a Loongson-2E or -2F target using an ABI that supports
  the Loongson vector modes.

``mips_msa``
  MIPS target supports ``-mmsa``, MIPS SIMD Architecture (MSA).

``mips_newabi_large_long_double``
  MIPS target supports ``long double`` larger than ``double``
  when using the new ABI.

``mpaired_single``
  MIPS target supports ``-mpaired-single``.

MSP430-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~

``msp430_small``
  MSP430 target has the small memory model enabled ( ``-msmall`` ).

``msp430_large``
  MSP430 target has the large memory model enabled ( ``-mlarge`` ).

PowerPC-specific attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``dfp_hw``
  PowerPC target supports executing hardware DFP instructions.

``p8vector_hw``
  PowerPC target supports executing VSX instructions (ISA 2.07).

``powerpc64``
  Test system supports executing 64-bit instructions.

``powerpc_altivec``
  PowerPC target supports AltiVec.

``powerpc_altivec_ok``
  PowerPC target supports ``-maltivec``.

``powerpc_eabi_ok``
  PowerPC target supports ``-meabi``.

``powerpc_elfv2``
  PowerPC target supports ``-mabi=elfv2``.

``powerpc_fprs``
  PowerPC target supports floating-point registers.

``powerpc_hard_double``
  PowerPC target supports hardware double-precision floating-point.

``powerpc_htm_ok``
  PowerPC target supports ``-mhtm``

``powerpc_p8vector_ok``
  PowerPC target supports ``-mpower8-vector``

``powerpc_popcntb_ok``
  PowerPC target supports the ``popcntb`` instruction, indicating
  that this target supports ``-mcpu=power5``.

``powerpc_ppu_ok``
  PowerPC target supports ``-mcpu=cell``.

``powerpc_spe``
  PowerPC target supports PowerPC SPE.

``powerpc_spe_nocache``
  Including the options used to compile this particular test, the
  PowerPC target supports PowerPC SPE.

``powerpc_spu``
  PowerPC target supports PowerPC SPU.

``powerpc_vsx_ok``
  PowerPC target supports ``-mvsx``.

``powerpc_405_nocache``
  Including the options used to compile this particular test, the
  PowerPC target supports PowerPC 405.

``ppc_recip_hw``
  PowerPC target supports executing reciprocal estimate instructions.

``vmx_hw``
  PowerPC target supports executing AltiVec instructions.

``vsx_hw``
  PowerPC target supports executing VSX instructions (ISA 2.06).

``has_arch_pwr5``
  PowerPC target pre-defines macro _ARCH_PWR5 which means the ``-mcpu``
  setting is Power5 or later.

``has_arch_pwr6``
  PowerPC target pre-defines macro _ARCH_PWR6 which means the ``-mcpu``
  setting is Power6 or later.

``has_arch_pwr7``
  PowerPC target pre-defines macro _ARCH_PWR7 which means the ``-mcpu``
  setting is Power7 or later.

``has_arch_pwr8``
  PowerPC target pre-defines macro _ARCH_PWR8 which means the ``-mcpu``
  setting is Power8 or later.

``has_arch_pwr9``
  PowerPC target pre-defines macro _ARCH_PWR9 which means the ``-mcpu``
  setting is Power9 or later.

Other hardware attributes
~~~~~~~~~~~~~~~~~~~~~~~~~

.. Please keep this table sorted alphabetically.

``autoincdec``
  Target supports autoincrement/decrement addressing.

``avx``
  Target supports compiling ``avx`` instructions.

``avx_runtime``
  Target supports the execution of ``avx`` instructions.

``avx2``
  Target supports compiling ``avx2`` instructions.

``avx2_runtime``
  Target supports the execution of ``avx2`` instructions.

``avxvnni``
  Target supports the execution of ``avxvnni`` instructions.

``avx512f``
  Target supports compiling ``avx512f`` instructions.

``avx512f_runtime``
  Target supports the execution of ``avx512f`` instructions.

``avx512vp2intersect``
  Target supports the execution of ``avx512vp2intersect`` instructions.

``amx_tile``
  Target supports the execution of ``amx-tile`` instructions.

``amx_int8``
  Target supports the execution of ``amx-int8`` instructions.

``amx_bf16``
  Target supports the execution of ``amx-bf16`` instructions.

``cell_hw``
  Test system can execute AltiVec and Cell PPU instructions.

``coldfire_fpu``
  Target uses a ColdFire FPU.

``divmod``
  Target supporting hardware divmod insn or divmod libcall.

``divmod_simode``
  Target supporting hardware divmod insn or divmod libcall for SImode.

``hard_float``
  Target supports FPU instructions.

``non_strict_align``
  Target does not require strict alignment.

``pie_copyreloc``
  The x86-64 target linker supports PIE with copy reloc.

``rdrand``
  Target supports x86 ``rdrand`` instruction.

``sqrt_insn``
  Target has a square root instruction that the compiler can generate.

``sse``
  Target supports compiling ``sse`` instructions.

``sse_runtime``
  Target supports the execution of ``sse`` instructions.

``sse2``
  Target supports compiling ``sse2`` instructions.

``sse2_runtime``
  Target supports the execution of ``sse2`` instructions.

``sync_char_short``
  Target supports atomic operations on ``char`` and ``short``.

``sync_int_long``
  Target supports atomic operations on ``int`` and ``long``.

``ultrasparc_hw``
  Test environment appears to run executables on a simulator that
  accepts only ``EM_SPARC`` executables and chokes on ``EM_SPARC32PLUS``
  or ``EM_SPARCV9`` executables.

``vect_cmdline_needed``
  Target requires a command line argument to enable a SIMD instruction set.

``xorsign``
  Target supports the xorsign optab expansion.

Environment attributes
~~~~~~~~~~~~~~~~~~~~~~

``c``
  The language for the compiler under test is C.

``c++``
  The language for the compiler under test is C++.

``c99_runtime``
  Target provides a full C99 runtime.

``correct_iso_cpp_string_wchar_protos``
  Target ``string.h`` and ``wchar.h`` headers provide C++ required
  overloads for ``strchr`` etc. functions.

``d_runtime``
  Target provides the D runtime.

``d_runtime_has_std_library``
  Target provides the D standard library (Phobos).

``dummy_wcsftime``
  Target uses a dummy ``wcsftime`` function that always returns zero.

``fd_truncate``
  Target can truncate a file from a file descriptor, as used by
  libgfortran/io/unix.c:fd_truncate; i.e. ``ftruncate`` or
  ``chsize``.

``fenv``
  Target provides fenv.h include file.

``fenv_exceptions``
  Target supports fenv.h with all the standard IEEE exceptions
  and floating-point exceptions are raised by arithmetic operations.

``fenv_exceptions_dfp``
  Target supports fenv.h with all the standard IEEE exceptions
  and floating-point exceptions are raised by arithmetic operations for
  decimal floating point.

``fileio``
  Target offers such file I/O library functions as ``fopen``,
  ``fclose``, ``tmpnam``, and ``remove``.  This is a link-time
  requirement for the presence of the functions in the library; even if
  they fail at runtime, the requirement is still regarded as satisfied.

``freestanding``
  Target is :samp:`freestanding` as defined in section 4 of the C99 standard.
  Effectively, it is a target which supports no extra headers or libraries
  other than what is considered essential.

``gettimeofday``
  Target supports ``gettimeofday``.

``init_priority``
  Target supports constructors with initialization priority arguments.

``inttypes_types``
  Target has the basic signed and unsigned types in ``inttypes.h``.
  This is for tests that GCC's notions of these types agree with those
  in the header, as some systems have only ``inttypes.h``.

``lax_strtofp``
  Target might have errors of a few ULP in string to floating-point
  conversion functions and overflow is not always detected correctly by
  those functions.

``mempcpy``
  Target provides ``mempcpy`` function.

``mmap``
  Target supports ``mmap``.

``newlib``
  Target supports Newlib.

``newlib_nano_io``
  GCC was configured with ``--enable-newlib-nano-formatted-io``, which reduces
  the code size of Newlib formatted I/O functions.

``pow10``
  Target provides ``pow10`` function.

``pthread``
  Target can compile using ``pthread.h`` with no errors or warnings.

``pthread_h``
  Target has ``pthread.h``.

``run_expensive_tests``
  Expensive testcases (usually those that consume excessive amounts of CPU
  time) should be run on this target.  This can be enabled by setting the
  :envvar:`GCC_TEST_RUN_EXPENSIVE` environment variable to a non-empty string.

``simulator``
  Test system runs executables on a simulator (i.e. slowly) rather than
  hardware (i.e. fast).

``signal``
  Target has ``signal.h``.

``stabs``
  Target supports the stabs debugging format.

``stdint_types``
  Target has the basic signed and unsigned C types in ``stdint.h``.
  This will be obsolete when GCC ensures a working ``stdint.h`` for
  all targets.

``stdint_types_mbig_endian``
  Target accepts the option :option:`-mbig-endian` and ``stdint.h``
  can be included without error when :option:`-mbig-endian` is passed.

``stpcpy``
  Target provides ``stpcpy`` function.

``sysconf``
  Target supports ``sysconf``.

``trampolines``
  Target supports trampolines.

``uclibc``
  Target supports uClibc.

``unwrapped``
  Target does not use a status wrapper.

``vxworks_kernel``
  Target is a VxWorks kernel.

``vxworks_rtp``
  Target is a VxWorks RTP.

``wchar``
  Target supports wide characters.

Other attributes
~~~~~~~~~~~~~~~~

``R_flag_in_section``
  Target supports the 'R' flag in .section directive in assembly inputs.

``automatic_stack_alignment``
  Target supports automatic stack alignment.

``branch_cost``
  Target supports :option:`-branch-cost=N`.

``cxa_atexit``
  Target uses ``__cxa_atexit``.

``default_packed``
  Target has packed layout of structure members by default.

``exceptions``
  Target supports exceptions.

``exceptions_enabled``
  Target supports exceptions and they are enabled in the current
  testing configuration.

``fgraphite``
  Target supports Graphite optimizations.

``fixed_point``
  Target supports fixed-point extension to C.

``fopenacc``
  Target supports OpenACC via :option:`-fopenacc`.

``fopenmp``
  Target supports OpenMP via :option:`-fopenmp`.

``fpic``
  Target supports :option:`-fpic` and :option:`-fPIC`.

``freorder``
  Target supports :option:`-freorder-blocks-and-partition`.

``fstack_protector``
  Target supports :option:`-fstack-protector`.

``gas``
  Target uses GNU :command:`as`.

``gc_sections``
  Target supports :option:`--gc-sections`.

``gld``
  Target uses GNU :command:`ld`.

``keeps_null_pointer_checks``
  Target keeps null pointer checks, either due to the use of
  :option:`-fno-delete-null-pointer-checks` or hardwired into the target.

``llvm_binutils``
  Target is using an LLVM assembler and/or linker, instead of GNU Binutils.

``lra``
  Target supports local register allocator (LRA).

``lto``
  Compiler has been configured to support link-time optimization (LTO).

``lto_incremental``
  Compiler and linker support link-time optimization relocatable linking
  with :option:`-r` and :option:`-flto` options.

``naked_functions``
  Target supports the ``naked`` function attribute.

``named_sections``
  Target supports named sections.

``natural_alignment_32``
  Target uses natural alignment (aligned to type size) for types of
  32 bits or less.

``target_natural_alignment_64``
  Target uses natural alignment (aligned to type size) for types of
  64 bits or less.

``noinit``
  Target supports the ``noinit`` variable attribute.

``nonpic``
  Target does not generate PIC by default.

``o_flag_in_section``
  Target supports the 'o' flag in .section directive in assembly inputs.

``offload_gcn``
  Target has been configured for OpenACC/OpenMP offloading on AMD GCN.

``persistent``
  Target supports the ``persistent`` variable attribute.

``pie_enabled``
  Target generates PIE by default.

``pcc_bitfield_type_matters``
  Target defines ``PCC_BITFIELD_TYPE_MATTERS``.

``pe_aligned_commons``
  Target supports :option:`-mpe-aligned-commons`.

``pie``
  Target supports :option:`-pie`, :option:`-fpie` and :option:`-fPIE`.

``rdynamic``
  Target supports :option:`-rdynamic`.

``scalar_all_fma``
  Target supports all four fused multiply-add optabs for both ``float``
  and ``double``.  These optabs are: ``fma_optab``, ``fms_optab``,
  ``fnma_optab`` and ``fnms_optab``.

``section_anchors``
  Target supports section anchors.

``short_enums``
  Target defaults to short enums.

``stack_size``
  .. _stack_size_et:
  Target has limited stack size.  The stack size limit can be obtained using the
  STACK_SIZE macro defined by stack_size_ao``dg-add-options`` feature
  ``stack_size``.

``static``
  Target supports :option:`-static`.

``static_libgfortran``
  Target supports statically linking :samp:`libgfortran`.

``string_merging``
  Target supports merging string constants at link time.

``ucn``
  Target supports compiling and assembling UCN.

``ucn_nocache``
  Including the options used to compile this particular test, the
  target supports compiling and assembling UCN.

``unaligned_stack``
  Target does not guarantee that its ``STACK_BOUNDARY`` is greater than
  or equal to the required vector alignment.

``vector_alignment_reachable``
  Vector alignment is reachable for types of 32 bits or less.

``vector_alignment_reachable_for_64bit``
  Vector alignment is reachable for types of 64 bits or less.

``wchar_t_char16_t_compatible``
  Target supports ``wchar_t`` that is compatible with ``char16_t``.

``wchar_t_char32_t_compatible``
  Target supports ``wchar_t`` that is compatible with ``char32_t``.

``comdat_group``
  Target uses comdat groups.

``indirect_calls``
  Target supports indirect calls, i.e. calls where the target is not
  constant.

``lgccjit``
  Target supports -lgccjit, i.e. libgccjit.so can be linked into jit tests.

Local to tests in gcc.target/i386
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``3dnow``
  Target supports compiling ``3dnow`` instructions.

``aes``
  Target supports compiling ``aes`` instructions.

``fma4``
  Target supports compiling ``fma4`` instructions.

``mfentry``
  Target supports the ``-mfentry`` option that alters the
  position of profiling calls such that they precede the prologue.

``ms_hook_prologue``
  Target supports attribute ``ms_hook_prologue``.

``pclmul``
  Target supports compiling ``pclmul`` instructions.

``sse3``
  Target supports compiling ``sse3`` instructions.

``sse4``
  Target supports compiling ``sse4`` instructions.

``sse4a``
  Target supports compiling ``sse4a`` instructions.

``ssse3``
  Target supports compiling ``ssse3`` instructions.

``vaes``
  Target supports compiling ``vaes`` instructions.

``vpclmul``
  Target supports compiling ``vpclmul`` instructions.

``xop``
  Target supports compiling ``xop`` instructions.

Local to tests in gcc.test-framework
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``no``
  Always returns 0.

``yes``
  Always returns 1.

  .. _add-options:

Features for dg-add-options
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The supported values of :samp:`{feature}` for directive ``dg-add-options``
are:

``arm_fp``
  ``__ARM_FP`` definition.  Only ARM targets support this feature, and only then
  in certain modes; see the arm_fp_okarm_fp_ok effective target
  keyword.

``arm_fp_dp``
  ``__ARM_FP`` definition with double-precision support.  Only ARM
  targets support this feature, and only then in certain modes; see the
  arm_fp_dp_okarm_fp_dp_ok effective target keyword.

``arm_neon``
  NEON support.  Only ARM targets support this feature, and only then
  in certain modes; see the arm_neon_okarm_neon_ok effective target
  keyword.

``arm_fp16``
  VFP half-precision floating point support.  This does not select the
  FP16 format; for that, use arm_fp16_ieeearm_fp16_ieee or
  arm_fp16_alternativearm_fp16_alternative instead.  This
  feature is only supported by ARM targets and then only in certain
  modes; see the arm_fp16_okarm_fp16_ok effective target
  keyword.

``arm_fp16_ieee``
  .. _arm_fp16_ieee:
  ARM IEEE 754-2008 format VFP half-precision floating point support.
  This feature is only supported by ARM targets and then only in certain
  modes; see the arm_fp16_okarm_fp16_ok effective target
  keyword.

``arm_fp16_alternative``
  .. _arm_fp16_alternative:
  ARM Alternative format VFP half-precision floating point support.
  This feature is only supported by ARM targets and then only in certain
  modes; see the arm_fp16_okarm_fp16_ok effective target
  keyword.

``arm_neon_fp16``
  NEON and half-precision floating point support.  Only ARM targets
  support this feature, and only then in certain modes; see
  the arm_neon_fp16_okarm_neon_fp16_ok effective target keyword.

``arm_vfp3``
  arm vfp3 floating point support; see
  the arm_vfp3_okarm_vfp3_ok effective target keyword.

``arm_arch_v8a_hard``
  Add options for ARMv8-A and the hard-float variant of the AAPCS,
  if this is supported by the compiler; see the
  arm_arch_v8a_hard_okarm_arch_v8a_hard_ok effective target keyword.

``arm_v8_1a_neon``
  Add options for ARMv8.1-A with Adv.SIMD support, if this is supported
  by the target; see the arm_v8_1a_neon_okarm_v8_1a_neon_ok
  effective target keyword.

``arm_v8_2a_fp16_scalar``
  Add options for ARMv8.2-A with scalar FP16 support, if this is
  supported by the target; see the
  arm_v8_2a_fp16_scalar_okarm_v8_2a_fp16_scalar_ok effective
  target keyword.

``arm_v8_2a_fp16_neon``
  Add options for ARMv8.2-A with Adv.SIMD FP16 support, if this is
  supported by the target; see the
  arm_v8_2a_fp16_neon_okarm_v8_2a_fp16_neon_ok effective target
  keyword.

``arm_v8_2a_dotprod_neon``
  Add options for ARMv8.2-A with Adv.SIMD Dot Product support, if this is
  supported by the target; see the
  arm_v8_2a_dotprod_neon_ok effective target keyword.

``arm_fp16fml_neon``
  Add options to enable generation of the ``VFMAL`` and ``VFMSL``
  instructions, if this is supported by the target; see the
  arm_fp16fml_neon_ok effective target keyword.

``arm_dsp``
  Add options for ARM DSP intrinsics support, if this is supported by
  the target; see the arm_dsp_okarm_dsp_ok effective target
  keyword.

``bind_pic_locally``
  Add the target-specific flags needed to enable functions to bind
  locally when using pic/PIC passes in the testsuite.

:samp:`float{n}`
  Add the target-specific flags needed to use the ``_Floatn`` type.

:samp:`float{n}x`
  Add the target-specific flags needed to use the ``_Floatnx`` type.

``ieee``
  Add the target-specific flags needed to enable full IEEE
  compliance mode.

``mips16_attribute``
  ``mips16`` function attributes.
  Only MIPS targets support this feature, and only then in certain modes.

``stack_size``
  .. _stack_size_ao:
  Add the flags needed to define macro STACK_SIZE and set it to the stack size
  limit associated with the stack_size_et``stack_size`` effective
  target.

``sqrt_insn``
  Add the target-specific flags needed to enable hardware square root
  instructions, if any.

``tls``
  Add the target-specific flags needed to use thread-local storage.

  .. _require-support:

Variants of dg-require-support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A few of the ``dg-require`` directives take arguments.

:samp:`dg-require-iconv {codeset}`
  Skip the test if the target does not support iconv.  :samp:`{codeset}` is
  the codeset to convert to.

:samp:`dg-require-profiling {profopt}`
  Skip the test if the target does not support profiling with option
  :samp:`{profopt}`.

:samp:`dg-require-stack-check {check}`
  Skip the test if the target does not support the ``-fstack-check``
  option.  If :samp:`{check}` is ``""``, support for ``-fstack-check``
  is checked, for ``-fstack-check=("check")`` otherwise.

:samp:`dg-require-stack-size {size}`
  Skip the test if the target does not support a stack size of :samp:`{size}`.

:samp:`dg-require-visibility {vis}`
  Skip the test if the target does not support the ``visibility`` attribute.
  If :samp:`{vis}` is ``""``, support for ``visibility("hidden")`` is
  checked, for ``visibility("vis")`` otherwise.

  The original ``dg-require`` directives were defined before there
was support for effective-target keywords.  The directives that do not
take arguments could be replaced with effective-target keywords.

``dg-require-alias ""``
  Skip the test if the target does not support the :samp:`alias` attribute.

``dg-require-ascii-locale ""``
  Skip the test if the host does not support an ASCII locale.

``dg-require-compat-dfp ""``
  Skip this test unless both compilers in a compat testsuite
  support decimal floating point.

``dg-require-cxa-atexit ""``
  Skip the test if the target does not support ``__cxa_atexit``.
  This is equivalent to ``dg-require-effective-target cxa_atexit``.

``dg-require-dll ""``
  Skip the test if the target does not support DLL attributes.

``dg-require-dot ""``
  Skip the test if the host does not have :command:`dot`.

``dg-require-fork ""``
  Skip the test if the target does not support ``fork``.

``dg-require-gc-sections ""``
  Skip the test if the target's linker does not support the
  ``--gc-sections`` flags.
  This is equivalent to ``dg-require-effective-target gc-sections``.

``dg-require-host-local ""``
  Skip the test if the host is remote, rather than the same as the build
  system.  Some tests are incompatible with DejaGnu's handling of remote
  hosts, which involves copying the source file to the host and compiling
  it with a relative path and " ``-o a.out`` ".

``dg-require-mkfifo ""``
  Skip the test if the target does not support ``mkfifo``.

``dg-require-named-sections ""``
  Skip the test is the target does not support named sections.
  This is equivalent to ``dg-require-effective-target named_sections``.

``dg-require-weak ""``
  Skip the test if the target does not support weak symbols.

``dg-require-weak-override ""``
  Skip the test if the target does not support overriding weak symbols.

  .. _final-actions:

Commands for use in dg-final
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The GCC testsuite defines the following directives to be used within
``dg-final``.

Scan a particular file
~~~~~~~~~~~~~~~~~~~~~~

:samp:`scan-file {filename}{regexp} [{ target/xfail {selector} }]`
  Passes if :samp:`{regexp}` matches text in :samp:`{filename}`.

:samp:`scan-file-not {filename}{regexp} [{ target/xfail {selector} }]`
  Passes if :samp:`{regexp}` does not match text in :samp:`{filename}`.

:samp:`scan-module {module}{regexp} [{ target/xfail {selector} }]`
  Passes if :samp:`{regexp}` matches in Fortran module :samp:`{module}`.

:samp:`dg-check-dot {filename}`
  Passes if :samp:`{filename}` is a valid .dot file (by running
  ``dot -Tpng`` on it, and verifying the exit code is 0).

Scan the assembly output
~~~~~~~~~~~~~~~~~~~~~~~~

:samp:`scan-assembler {regex} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` matches text in the test's assembler output.

:samp:`scan-assembler-not {regex} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` does not match text in the test's assembler output.

:samp:`scan-assembler-times {regex}{num} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` is matched exactly :samp:`{num}` times in the test's
  assembler output.

:samp:`scan-assembler-dem {regex} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` matches text in the test's demangled assembler output.

:samp:`scan-assembler-dem-not {regex} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` does not match text in the test's demangled assembler
  output.

:samp:`scan-assembler-symbol-section {functions}{section} [{ target/xfail {selector} }]`
  Passes if :samp:`{functions}` are all in :samp:`{section}`.  The caller needs to
  allow for ``USER_LABEL_PREFIX`` and different section name conventions.

:samp:`scan-symbol-section {filename}{functions}{section} [{ target/xfail {selector} }]`
  Passes if :samp:`{functions}` are all in :samp:`{section}` in :samp:`{filename}`.
  The same caveats as for ``scan-assembler-symbol-section`` apply.

:samp:`scan-hidden {symbol} [{ target/xfail {selector} }]`
  Passes if :samp:`{symbol}` is defined as a hidden symbol in the test's
  assembly output.

:samp:`scan-not-hidden {symbol} [{ target/xfail {selector} }]`
  Passes if :samp:`{symbol}` is not defined as a hidden symbol in the test's
  assembly output.

:samp:`check-function-bodies {prefix}{terminator} [{options} [{ target/xfail {selector} }]]`
  Looks through the source file for comments that give the expected assembly
  output for selected functions.  Each line of expected output starts with the
  prefix string :samp:`{prefix}` and the expected output for a function as a whole
  is followed by a line that starts with the string :samp:`{terminator}`.
  Specifying an empty terminator is equivalent to specifying :samp:`"*/"`.

  :samp:`{options}`, if specified, is a list of regular expressions, each of
  which matches a full command-line option.  A non-empty list prevents
  the test from running unless all of the given options are present on the
  command line.  This can help if a source file is compiled both with
  and without optimization, since it is rarely useful to check the full
  function body for unoptimized code.

  The first line of the expected output for a function :samp:`{fn}` has the form:

  .. code-block:: c++

    prefix fn:  [{ target/xfail selector }]

  Subsequent lines of the expected output also start with :samp:`{prefix}`.
  In both cases, whitespace after :samp:`{prefix}` is not significant.

  The test discards assembly directives such as ``.cfi_startproc``
  and local label definitions such as ``.LFB0`` from the compiler's
  assembly output.  It then matches the result against the expected
  output for a function as a single regular expression.  This means that
  later lines can use backslashes to refer back to :samp:`(...)`
  captures on earlier lines.  For example:

  .. code-block:: c++

    /* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */
    ...
    /*
    ** add_w0_s8_m:
    **	mov	(z[0-9]+\.b), w0
    **	add	z0\.b, p0/m, z0\.b, \1
    **	ret
    */
    svint8_t add_w0_s8_m (...) { ... }
    ...
    /*
    ** add_b0_s8_m:
    **	mov	(z[0-9]+\.b), b0
    **	add	z1\.b, p0/m, z1\.b, \1
    **	ret
    */
    svint8_t add_b0_s8_m (...) { ... }

  checks whether the implementations of ``add_w0_s8_m`` and
  ``add_b0_s8_m`` match the regular expressions given.  The test only
  runs when :samp:`-DCHECK_ASM` is passed on the command line.

  It is possible to create non-capturing multi-line regular expression
  groups of the form :samp:`({a}|{b}|...)` by putting the
  :samp:`(`, :samp:`|` and :samp:`)` on separate lines (each still using
  :samp:`{prefix}` ).  For example:

  .. code-block:: c++

    /*
    ** cmple_f16_tied:
    ** (
    **	fcmge	p0\.h, p0/z, z1\.h, z0\.h
    ** |
    **	fcmle	p0\.h, p0/z, z0\.h, z1\.h
    ** )
    **	ret
    */
    svbool_t cmple_f16_tied (...) { ... }

  checks whether ``cmple_f16_tied`` is implemented by the
  ``fcmge`` instruction followed by ``ret`` or by the
  ``fcmle`` instruction followed by ``ret``.  The test is
  still a single regular rexpression.

  A line containing just:

  .. code-block:: c++

    prefix ...

  stands for zero or more unmatched lines; the whitespace after
  :samp:`{prefix}` is again not significant.

Scan optimization dump files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These commands are available for :samp:`{kind}` of ``tree``, ``ltrans-tree``,
``offload-tree``, ``rtl``, ``offload-rtl``, ``ipa``, and
``wpa-ipa``.

:samp:`scan-{kind}-dump {regex}{suffix} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` matches text in the dump file with suffix :samp:`{suffix}`.

:samp:`scan-{kind}-dump-not {regex}{suffix} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` does not match text in the dump file with suffix
  :samp:`{suffix}`.

:samp:`scan-{kind}-dump-times {regex}{num}{suffix} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` is found exactly :samp:`{num}` times in the dump file
  with suffix :samp:`{suffix}`.

:samp:`scan-{kind}-dump-dem {regex}{suffix} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` matches demangled text in the dump file with
  suffix :samp:`{suffix}`.

:samp:`scan-{kind}-dump-dem-not {regex}{suffix} [{ target/xfail {selector} }]`
  Passes if :samp:`{regex}` does not match demangled text in the dump file with
  suffix :samp:`{suffix}`.

  The :samp:`{suffix}` argument which describes the dump file to be scanned
may contain a glob pattern that must expand to exactly one file
name. This is useful if, e.g., different pass instances are executed
depending on torture testing command-line flags, producing dump files
whose names differ only in their pass instance number suffix.  For
example, to scan instances 1, 2, 3 of a tree pass 'mypass' for
occurrences of the string 'code has been optimized', use:

.. code-block:: c++

  /* { dg-options "-fdump-tree-mypass" } */
  /* { dg-final { scan-tree-dump "code has been optimized" "mypass\[1-3\]" } } */

Check for output files
~~~~~~~~~~~~~~~~~~~~~~

:samp:`output-exists [{ target/xfail {selector} }]`
  Passes if compiler output file exists.

:samp:`output-exists-not [{ target/xfail {selector} }]`
  Passes if compiler output file does not exist.

:samp:`scan-symbol {regexp} [{ target/xfail {selector} }]`
  Passes if the pattern is present in the final executable.

:samp:`scan-symbol-not {regexp} [{ target/xfail {selector} }]`
  Passes if the pattern is absent from the final executable.

Checks for gcov tests
~~~~~~~~~~~~~~~~~~~~~

:samp:`run-gcov {sourcefile}`
  Check line counts in :command:`gcov` tests.

:samp:`run-gcov [branches] [calls] { {opts}{sourcefile} }`
  Check branch and/or call counts, in addition to line counts, in
  :command:`gcov` tests.

:samp:`run-gcov-pytest { {sourcefile}{pytest_file} }`
  Check output of :command:`gcov` intermediate format with a pytest
  script.

Clean up generated test files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Usually the test-framework removes files that were generated during
testing. If a testcase, for example, uses any dumping mechanism to
inspect a passes dump file, the testsuite recognized the dump option
passed to the tool and schedules a final cleanup to remove these files.

There are, however, following additional cleanup directives that can be
used to annotate a testcase "manually".

``cleanup-coverage-files``
  Removes coverage data files generated for this test.

:samp:`cleanup-modules "{list-of-extra-modules}"`
  Removes Fortran module files generated for this test, excluding the
  module names listed in keep-modules.
  Cleaning up module files is usually done automatically by the testsuite
  by looking at the source files and removing the modules after the test
  has been executed.

  .. code-block:: c++

    module MoD1
    end module MoD1
    module Mod2
    end module Mod2
    module moD3
    end module moD3
    module mod4
    end module mod4
    ! { dg-final { cleanup-modules "mod1 mod2" } } ! redundant
    ! { dg-final { keep-modules "mod3 mod4" } }

:samp:`keep-modules "{list-of-modules-not-to-delete}"`
  Whitespace separated list of module names that should not be deleted by
  cleanup-modules.
  If the list of modules is empty, all modules defined in this file are kept.

  .. code-block:: c++

    module maybe_unneeded
    end module maybe_unneeded
    module keep1
    end module keep1
    module keep2
    end module keep2
    ! { dg-final { keep-modules "keep1 keep2" } } ! just keep these two
    ! { dg-final { keep-modules "" } } ! keep all

:samp:`dg-keep-saved-temps "{list-of-suffixes-not-to-delete}"`
  Whitespace separated list of suffixes that should not be deleted
  automatically in a testcase that uses :option:`-save-temps`.

  .. code-block:: c++

    // { dg-options "-save-temps -fpch-preprocess -I." }
    int main() { return 0; }
    // { dg-keep-saved-temps ".s" } ! just keep assembler file
    // { dg-keep-saved-temps ".s" ".i" } ! ... and .i
    // { dg-keep-saved-temps ".ii" ".o" } ! or just .ii and .o

``cleanup-profile-file``
  Removes profiling files generated for this test.

