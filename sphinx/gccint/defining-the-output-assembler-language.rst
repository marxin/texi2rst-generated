.. _assembler-format:

Defining the Output Assembler Language
**************************************

This section describes macros whose principal purpose is to describe how
to write instructions in assembler language-rather than what the
instructions do.

.. toctree::

  file-framework
  data-output
  uninitialized-data
  label-output
  initialization
  macros-for-initialization
  instruction-output
  dispatch-tables
  exception-region-output
  alignment-output

.. _file-framework:

The Overall Framework of an Assembler File
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: assembler format

.. index:: output of assembler code

.. prevent bad page break with this line

This describes the overall framework of an assembly file.

.. index:: default_file_start

.. function:: void TARGET_ASM_FILE_START (void)

  Output to ``asm_out_file`` any text which the assembler expects to
  find at the beginning of a file.  The default behavior is controlled
  by two flags, documented below.  Unless your target's assembler is
  quite unusual, if you override the default, you should call
  ``default_file_start`` at some point in your target hook.  This
  lets other target files rely on these variables.

.. c:var:: bool TARGET_ASM_FILE_START_APP_OFF

  If this flag is true, the text of the macro ``ASM_APP_OFF`` will be
  printed as the very first line in the assembly file, unless
  :option:`-fverbose-asm` is in effect.  (If that macro has been defined
  to the empty string, this variable has no effect.)  With the normal
  definition of ``ASM_APP_OFF``, the effect is to notify the GNU
  assembler that it need not bother stripping comments or extra
  whitespace from its input.  This allows it to work a bit faster.

  The default is false.  You should not set it to true unless you have
  verified that your port does not generate any extra whitespace or
  comments that will cause GAS to issue errors in NO_APP mode.

.. c:var:: bool TARGET_ASM_FILE_START_FILE_DIRECTIVE

  If this flag is true, ``output_file_directive`` will be called
  for the primary source file, immediately after printing
  ``ASM_APP_OFF`` (if that is enabled).  Most ELF assemblers expect
  this to be done.  The default is false.

.. function:: void TARGET_ASM_FILE_END (void)

  Output to ``asm_out_file`` any text which the assembler expects
  to find at the end of a file.  The default is to output nothing.

.. function:: void file_end_indicate_exec_stack ()

  Some systems use a common convention, the :samp:`.note.GNU-stack`
  special section, to indicate whether or not an object file relies on
  the stack being executable.  If your system uses this convention, you
  should define ``TARGET_ASM_FILE_END`` to this function.  If you
  need to do other things in that hook, have your hook function call
  this function.

.. function:: void TARGET_ASM_LTO_START (void)

  Output to ``asm_out_file`` any text which the assembler expects
  to find at the start of an LTO section.  The default is to output
  nothing.

.. function:: void TARGET_ASM_LTO_END (void)

  Output to ``asm_out_file`` any text which the assembler expects
  to find at the end of an LTO section.  The default is to output
  nothing.

.. function:: void TARGET_ASM_CODE_END (void)

  Output to ``asm_out_file`` any text which is needed before emitting
  unwind info and debug info at the end of a file.  Some targets emit
  here PIC setup thunks that cannot be emitted at the end of file,
  because they couldn't have unwind info then.  The default is to output
  nothing.

.. c:macro:: ASM_COMMENT_START

  A C string constant describing how to begin a comment in the target
  assembler language.  The compiler assumes that the comment will end at
  the end of the line.

.. c:macro:: ASM_APP_ON

  A C string constant for text to be output before each ``asm``
  statement or group of consecutive ones.  Normally this is
  ``"#APP"``, which is a comment that has no effect on most
  assemblers but tells the GNU assembler that it must check the lines
  that follow for all valid assembler constructs.

.. c:macro:: ASM_APP_OFF

  A C string constant for text to be output after each ``asm``
  statement or group of consecutive ones.  Normally this is
  ``"#NO_APP"``, which tells the GNU assembler to resume making the
  time-saving assumptions that are valid for ordinary compiler output.

.. c:macro:: ASM_OUTPUT_SOURCE_FILENAME (stream, name)

  A C statement to output COFF information or DWARF debugging information
  which indicates that filename :samp:`{name}` is the current source file to
  the stdio stream :samp:`{stream}`.

  This macro need not be defined if the standard form of output
  for the file format in use is appropriate.

.. function:: void TARGET_ASM_OUTPUT_SOURCE_FILENAME (FILE *file, const char *name)

  Output DWARF debugging information which indicates that filename :samp:`{name}` is the current source file to the stdio stream :samp:`{file}`.

  This target hook need not be defined if the standard form of output for the file format in use is appropriate.

.. function:: void TARGET_ASM_OUTPUT_IDENT (const char *name)

  Output a string based on :samp:`{name}`, suitable for the :samp:`#ident`  directive, or the equivalent directive or pragma in non-C-family languages.  If this hook is not defined, nothing is output for the :samp:`#ident`  directive.

.. c:macro:: OUTPUT_QUOTED_STRING (stream, string)

  A C statement to output the string :samp:`{string}` to the stdio stream
  :samp:`{stream}`.  If you do not call the function ``output_quoted_string``
  in your config files, GCC will only call it to output filenames to
  the assembler source.  So you can use it to canonicalize the format
  of the filename using this macro.

.. function:: void TARGET_ASM_NAMED_SECTION (const char *name, unsigned int flags, tree decl)

  Output assembly directives to switch to section :samp:`{name}`.  The section
  should have attributes as specified by :samp:`{flags}`, which is a bit mask
  of the ``SECTION_*`` flags defined in output.h.  If :samp:`{decl}`
  is non-NULL, it is the ``VAR_DECL`` or ``FUNCTION_DECL`` with which
  this section is associated.

.. function:: bool TARGET_ASM_ELF_FLAGS_NUMERIC (unsigned int flags, unsigned int *num)

  This hook can be used to encode ELF section flags for which no letter
  code has been defined in the assembler.  It is called by
  ``default_asm_named_section`` whenever the section flags need to be
  emitted in the assembler output.  If the hook returns true, then the
  numerical value for ELF section flags should be calculated from
  :samp:`{flags}` and saved in :samp:`{*num}` ; the value is printed out instead of the
  normal sequence of letter codes.  If the hook is not defined, or if it
  returns false, then :samp:`{num}` is ignored and the traditional letter sequence
  is emitted.

.. function:: section * TARGET_ASM_FUNCTION_SECTION (tree decl, enum node_frequency freq, bool startup, bool exit)

  Return preferred text (sub)section for function :samp:`{decl}`.
  Main purpose of this function is to separate cold, normal and hot
  functions. :samp:`{startup}` is true when function is known to be used only
  at startup (from static constructors or it is ``main()`` ).
  :samp:`{exit}` is true when function is known to be used only at exit
  (from static destructors).
  Return NULL if function should go to default text section.

.. function:: void TARGET_ASM_FUNCTION_SWITCHED_TEXT_SECTIONS (FILE *file, tree decl, bool new_is_cold)

  Used by the target to emit any assembler directives or additional  labels needed when a function is partitioned between different  sections.  Output should be written to :samp:`{file}`.  The function  decl is available as :samp:`{decl}` and the new section is 'cold' if  :samp:`{new_is_cold}` is ``true``.

.. c:var:: bool TARGET_HAVE_NAMED_SECTIONS

  This flag is true if the target supports ``TARGET_ASM_NAMED_SECTION``.
  It must not be modified by command-line option processing.

.. c:var:: bool TARGET_HAVE_SWITCHABLE_BSS_SECTIONS

  This flag is true if we can create zeroed data by switching to a BSS
  section and then using ``ASM_OUTPUT_SKIP`` to allocate the space.
  This is true on most ELF targets.

.. function:: unsigned int TARGET_SECTION_TYPE_FLAGS (tree decl, const char *name, int reloc)

  Choose a set of section attributes for use by ``TARGET_ASM_NAMED_SECTION``
  based on a variable or function decl, a section name, and whether or not the
  declaration's initializer may contain runtime relocations.  :samp:`{decl}` may be
  null, in which case read-write data should be assumed.

  The default version of this function handles choosing code vs data,
  read-only vs read-write data, and ``flag_pic``.  You should only
  need to override this if your target has special flags that might be
  set via ``__attribute__``.

.. function:: void TARGET_ASM_RECORD_GCC_SWITCHES (const char *)

  Provides the target with the ability to record the gcc command line
  switches provided as argument.

  By default this hook is set to NULL, but an example implementation is
  provided for ELF based targets.  Called :samp:`{elf_record_gcc_switches}`,
  it records the switches as ASCII text inside a new, string mergeable
  section in the assembler output file.  The name of the new section is
  provided by the ``TARGET_ASM_RECORD_GCC_SWITCHES_SECTION`` target
  hook.

.. c:var:: const char * TARGET_ASM_RECORD_GCC_SWITCHES_SECTION

  This is the name of the section that will be created by the example
  ELF implementation of the ``TARGET_ASM_RECORD_GCC_SWITCHES`` target
  hook.

.. _data-output:

Output of Data
^^^^^^^^^^^^^^

.. c:var:: const char * TARGET_ASM_BYTE_OP

  These hooks specify assembly directives for creating certain kinds
  of integer object.  The ``TARGET_ASM_BYTE_OP`` directive creates a
  byte-sized object, the ``TARGET_ASM_ALIGNED_HI_OP`` one creates an
  aligned two-byte object, and so on.  Any of the hooks may be
  ``NULL``, indicating that no suitable directive is available.

  The compiler will print these strings at the start of a new line,
  followed immediately by the object's initial value.  In most cases,
  the string should contain a tab, a pseudo-op, and then another tab.

.. function:: bool TARGET_ASM_INTEGER (rtx x, unsigned int size, int aligned_p)

  The ``assemble_integer`` function uses this hook to output an
  integer object.  :samp:`{x}` is the object's value, :samp:`{size}` is its size
  in bytes and :samp:`{aligned_p}` indicates whether it is aligned.  The
  function should return ``true`` if it was able to output the
  object.  If it returns false, ``assemble_integer`` will try to
  split the object into smaller parts.

  The default implementation of this hook will use the
  ``TARGET_ASM_BYTE_OP`` family of strings, returning ``false``
  when the relevant string is ``NULL``.

.. function:: void TARGET_ASM_DECL_END (void)

  Define this hook if the target assembler requires a special marker to
  terminate an initialized variable declaration.

.. function:: bool TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA (FILE *file, rtx x)

  A target hook to recognize :samp:`{rtx}` patterns that ``output_addr_const``
  can't deal with, and output assembly code to :samp:`{file}` corresponding to
  the pattern :samp:`{x}`.  This may be used to allow machine-dependent
  ``UNSPEC`` s to appear within constants.

  If target hook fails to recognize a pattern, it must return ``false``,
  so that a standard error message is printed.  If it prints an error message
  itself, by calling, for example, ``output_operand_lossage``, it may just
  return ``true``.

.. c:macro:: ASM_OUTPUT_ASCII (stream, ptr, len)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  instruction to assemble a string constant containing the :samp:`{len}`
  bytes at :samp:`{ptr}`.  :samp:`{ptr}` will be a C expression of type
  ``char *`` and :samp:`{len}` a C expression of type ``int``.

  If the assembler has a ``.ascii`` pseudo-op as found in the
  Berkeley Unix assembler, do not define the macro
  ``ASM_OUTPUT_ASCII``.

.. c:macro:: ASM_OUTPUT_FDESC (stream, decl, n)

  A C statement to output word :samp:`{n}` of a function descriptor for
  :samp:`{decl}`.  This must be defined if ``TARGET_VTABLE_USES_DESCRIPTORS``
  is defined, and is otherwise unused.

.. c:macro:: CONSTANT_POOL_BEFORE_FUNCTION

  You may define this macro as a C expression.  You should define the
  expression to have a nonzero value if GCC should output the constant
  pool for a function before the code for the function, or a zero value if
  GCC should output the constant pool after the function.  If you do
  not define this macro, the usual case, GCC will output the constant
  pool before the function.

.. c:macro:: ASM_OUTPUT_POOL_PROLOGUE (file, funname, fundecl, size)

  A C statement to output assembler commands to define the start of the
  constant pool for a function.  :samp:`{funname}` is a string giving
  the name of the function.  Should the return type of the function
  be required, it can be obtained via :samp:`{fundecl}`.  :samp:`{size}`
  is the size, in bytes, of the constant pool that will be written
  immediately after this call.

  If no constant-pool prefix is required, the usual case, this macro need
  not be defined.

.. c:macro:: ASM_OUTPUT_SPECIAL_POOL_ENTRY (file, x, mode, align, labelno, jumpto)

  A C statement (with or without semicolon) to output a constant in the
  constant pool, if it needs special treatment.  (This macro need not do
  anything for RTL expressions that can be output normally.)

  The argument :samp:`{file}` is the standard I/O stream to output the
  assembler code on.  :samp:`{x}` is the RTL expression for the constant to
  output, and :samp:`{mode}` is the machine mode (in case :samp:`{x}` is a
  :samp:`const_int`).  :samp:`{align}` is the required alignment for the value
  :samp:`{x}` ; you should output an assembler directive to force this much
  alignment.

  The argument :samp:`{labelno}` is a number to use in an internal label for
  the address of this pool entry.  The definition of this macro is
  responsible for outputting the label definition at the proper place.
  Here is how to do this:

  .. code-block:: c++

    (*targetm.asm_out.internal_label) (file, "LC", labelno);

  When you output a pool entry specially, you should end with a
  ``goto`` to the label :samp:`{jumpto}`.  This will prevent the same pool
  entry from being output a second time in the usual manner.

  You need not define this macro if it would do nothing.

.. c:macro:: ASM_OUTPUT_POOL_EPILOGUE (file funname fundecl size)

  A C statement to output assembler commands to at the end of the constant
  pool for a function.  :samp:`{funname}` is a string giving the name of the
  function.  Should the return type of the function be required, you can
  obtain it via :samp:`{fundecl}`.  :samp:`{size}` is the size, in bytes, of the
  constant pool that GCC wrote immediately before this call.

  If no constant-pool epilogue is required, the usual case, you need not
  define this macro.

.. c:macro:: IS_ASM_LOGICAL_LINE_SEPARATOR (C, STR)

  Define this macro as a C expression which is nonzero if :samp:`{C}` is
  used as a logical line separator by the assembler.  :samp:`{STR}` points
  to the position in the string where :samp:`{C}` was found; this can be used if
  a line separator uses multiple characters.

  If you do not define this macro, the default is that only
  the character :samp:`;` is treated as a logical line separator.

.. c:var:: const char * TARGET_ASM_OPEN_PAREN

  These target hooks are C string constants, describing the syntax in the
  assembler for grouping arithmetic expressions.  If not overridden, they
  default to normal parentheses, which is correct for most assemblers.

These macros are provided by real.h for writing the definitions
of ``ASM_OUTPUT_DOUBLE`` and the like:

.. c:macro:: REAL_VALUE_TO_TARGET_SINGLE (x, l)

  These translate :samp:`{x}`, of type ``REAL_VALUE_TYPE``, to the
  target's floating point representation, and store its bit pattern in
  the variable :samp:`{l}`.  For ``REAL_VALUE_TO_TARGET_SINGLE`` and
  ``REAL_VALUE_TO_TARGET_DECIMAL32``, this variable should be a
  simple ``long int``.  For the others, it should be an array of
  ``long int``.  The number of elements in this array is determined
  by the size of the desired target floating point data type: 32 bits of
  it go in each ``long int`` array element.  Each array element holds
  32 bits of the result, even if ``long int`` is wider than 32 bits
  on the host machine.

  The array element values are designed so that you can print them out
  using ``fprintf`` in the order they should appear in the target
  machine's memory.

.. _uninitialized-data:

Output of Uninitialized Variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Each of the macros in this section is used to do the whole job of
outputting a single uninitialized variable.

.. c:macro:: ASM_OUTPUT_COMMON (stream, name, size, rounded)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of a common-label named
  :samp:`{name}` whose size is :samp:`{size}` bytes.  The variable :samp:`{rounded}`
  is the size rounded up to whatever alignment the caller wants.  It is
  possible that :samp:`{size}` may be zero, for instance if a struct with no
  other member than a zero-length array is defined.  In this case, the
  backend must output a symbol definition that allocates at least one
  byte, both so that the address of the resulting object does not compare
  equal to any other, and because some object formats cannot even express
  the concept of a zero-sized common symbol, as that is how they represent
  an ordinary undefined external.

  Use the expression ``assemble_name (stream, name)`` to
  output the name itself; before and after that, output the additional
  assembler syntax for defining the name, and a newline.

  This macro controls how the assembler definitions of uninitialized
  common global variables are output.

.. c:macro:: ASM_OUTPUT_ALIGNED_COMMON (stream, name, size, alignment)

  Like ``ASM_OUTPUT_COMMON`` except takes the required alignment as a
  separate, explicit argument.  If you define this macro, it is used in
  place of ``ASM_OUTPUT_COMMON``, and gives you more flexibility in
  handling the required alignment of the variable.  The alignment is specified
  as the number of bits.

.. c:macro:: ASM_OUTPUT_ALIGNED_DECL_COMMON (stream, decl, name, size, alignment)

  Like ``ASM_OUTPUT_ALIGNED_COMMON`` except that :samp:`{decl}` of the
  variable to be output, if there is one, or ``NULL_TREE`` if there
  is no corresponding variable.  If you define this macro, GCC will use it
  in place of both ``ASM_OUTPUT_COMMON`` and
  ``ASM_OUTPUT_ALIGNED_COMMON``.  Define this macro when you need to see
  the variable's decl in order to chose what to output.

.. c:macro:: ASM_OUTPUT_ALIGNED_BSS (stream, decl, name, size, alignment)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of uninitialized global :samp:`{decl}` named
  :samp:`{name}` whose size is :samp:`{size}` bytes.  The variable :samp:`{alignment}`
  is the alignment specified as the number of bits.

  Try to use function ``asm_output_aligned_bss`` defined in file
  varasm.c when defining this macro.  If unable, use the expression
  ``assemble_name (stream, name)`` to output the name itself;
  before and after that, output the additional assembler syntax for defining
  the name, and a newline.

  There are two ways of handling global BSS.  One is to define this macro.
  The other is to have ``TARGET_ASM_SELECT_SECTION`` return a
  switchable BSS section (see :ref:`target_have_switchable_bss_sections`).
  You do not need to do both.

  Some languages do not have ``common`` data, and require a
  non-common form of global BSS in order to handle uninitialized globals
  efficiently.  C++ is one example of this.  However, if the target does
  not support global BSS, the front end may choose to make globals
  common in order to save space in the object file.

.. c:macro:: ASM_OUTPUT_LOCAL (stream, name, size, rounded)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of a local-common-label named
  :samp:`{name}` whose size is :samp:`{size}` bytes.  The variable :samp:`{rounded}`
  is the size rounded up to whatever alignment the caller wants.

  Use the expression ``assemble_name (stream, name)`` to
  output the name itself; before and after that, output the additional
  assembler syntax for defining the name, and a newline.

  This macro controls how the assembler definitions of uninitialized
  static variables are output.

.. c:macro:: ASM_OUTPUT_ALIGNED_LOCAL (stream, name, size, alignment)

  Like ``ASM_OUTPUT_LOCAL`` except takes the required alignment as a
  separate, explicit argument.  If you define this macro, it is used in
  place of ``ASM_OUTPUT_LOCAL``, and gives you more flexibility in
  handling the required alignment of the variable.  The alignment is specified
  as the number of bits.

.. c:macro:: ASM_OUTPUT_ALIGNED_DECL_LOCAL (stream, decl, name, size, alignment)

  Like ``ASM_OUTPUT_ALIGNED_LOCAL`` except that :samp:`{decl}` of the
  variable to be output, if there is one, or ``NULL_TREE`` if there
  is no corresponding variable.  If you define this macro, GCC will use it
  in place of both ``ASM_OUTPUT_LOCAL`` and
  ``ASM_OUTPUT_ALIGNED_LOCAL``.  Define this macro when you need to see
  the variable's decl in order to chose what to output.

.. _label-output:

Output and Generation of Labels
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This is about outputting labels.

.. index:: assemble_name

.. c:macro:: ASM_OUTPUT_LABEL (stream, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of a label named :samp:`{name}`.
  Use the expression ``assemble_name (stream, name)`` to
  output the name itself; before and after that, output the additional
  assembler syntax for defining the name, and a newline.  A default
  definition of this macro is provided which is correct for most systems.

.. c:macro:: ASM_OUTPUT_FUNCTION_LABEL (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` the assembler definition of a label named :samp:`{name}` of
  a function.
  Use the expression ``assemble_name (stream, name)`` to
  output the name itself; before and after that, output the additional
  assembler syntax for defining the name, and a newline.  A default
  definition of this macro is provided which is correct for most systems.

  If this macro is not defined, then the function name is defined in the
  usual manner as a label (by means of ``ASM_OUTPUT_LABEL`` ).

.. index:: assemble_name_raw

.. c:macro:: ASM_OUTPUT_INTERNAL_LABEL (stream, name)

  Identical to ``ASM_OUTPUT_LABEL``, except that :samp:`{name}` is known
  to refer to a compiler-generated label.  The default definition uses
  ``assemble_name_raw``, which is like ``assemble_name`` except
  that it is more efficient.

.. c:macro:: SIZE_ASM_OP

  A C string containing the appropriate assembler directive to specify the
  size of a symbol, without any arguments.  On systems that use ELF, the
  default (in config/elfos.h) is :samp:`"\t.size\t"`; on other
  systems, the default is not to define this macro.

  Define this macro only if it is correct to use the default definitions
  of ``ASM_OUTPUT_SIZE_DIRECTIVE`` and ``ASM_OUTPUT_MEASURED_SIZE``
  for your system.  If you need your own custom definitions of those
  macros, or if you do not need explicit symbol sizes at all, do not
  define this macro.

.. c:macro:: ASM_OUTPUT_SIZE_DIRECTIVE (stream, name, size)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` a directive telling the assembler that the size of the
  symbol :samp:`{name}` is :samp:`{size}`.  :samp:`{size}` is a ``HOST_WIDE_INT``.
  If you define ``SIZE_ASM_OP``, a default definition of this macro is
  provided.

.. c:macro:: ASM_OUTPUT_MEASURED_SIZE (stream, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` a directive telling the assembler to calculate the size of
  the symbol :samp:`{name}` by subtracting its address from the current
  address.

  If you define ``SIZE_ASM_OP``, a default definition of this macro is
  provided.  The default assumes that the assembler recognizes a special
  :samp:`.` symbol as referring to the current address, and can calculate
  the difference between this and another symbol.  If your assembler does
  not recognize :samp:`.` or cannot do calculations with it, you will need
  to redefine ``ASM_OUTPUT_MEASURED_SIZE`` to use some other technique.

.. c:macro:: NO_DOLLAR_IN_LABEL

  Define this macro if the assembler does not accept the character
  :samp:`$` in label names.  By default constructors and destructors in
  G++ have :samp:`$` in the identifiers.  If this macro is defined,
  :samp:`.` is used instead.

.. c:macro:: NO_DOT_IN_LABEL

  Define this macro if the assembler does not accept the character
  :samp:`.` in label names.  By default constructors and destructors in G++
  have names that use :samp:`.`.  If this macro is defined, these names
  are rewritten to avoid :samp:`.`.

.. c:macro:: TYPE_ASM_OP

  A C string containing the appropriate assembler directive to specify the
  type of a symbol, without any arguments.  On systems that use ELF, the
  default (in config/elfos.h) is :samp:`"\t.type\t"`; on other
  systems, the default is not to define this macro.

  Define this macro only if it is correct to use the default definition of
  ``ASM_OUTPUT_TYPE_DIRECTIVE`` for your system.  If you need your own
  custom definition of this macro, or if you do not need explicit symbol
  types at all, do not define this macro.

.. c:macro:: TYPE_OPERAND_FMT

  A C string which specifies (using ``printf`` syntax) the format of
  the second operand to ``TYPE_ASM_OP``.  On systems that use ELF, the
  default (in config/elfos.h) is :samp:`"@%s"`; on other systems,
  the default is not to define this macro.

  Define this macro only if it is correct to use the default definition of
  ``ASM_OUTPUT_TYPE_DIRECTIVE`` for your system.  If you need your own
  custom definition of this macro, or if you do not need explicit symbol
  types at all, do not define this macro.

.. c:macro:: ASM_OUTPUT_TYPE_DIRECTIVE (stream, type)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` a directive telling the assembler that the type of the
  symbol :samp:`{name}` is :samp:`{type}`.  :samp:`{type}` is a C string; currently,
  that string is always either :samp:`"function"` or :samp:`"object"`, but
  you should not count on this.

  If you define ``TYPE_ASM_OP`` and ``TYPE_OPERAND_FMT``, a default
  definition of this macro is provided.

.. c:macro:: ASM_DECLARE_FUNCTION_NAME (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the name :samp:`{name}` of a
  function which is being defined.  This macro is responsible for
  outputting the label definition (perhaps using
  ``ASM_OUTPUT_FUNCTION_LABEL`` ).  The argument :samp:`{decl}` is the
  ``FUNCTION_DECL`` tree node representing the function.

  If this macro is not defined, then the function name is defined in the
  usual manner as a label (by means of ``ASM_OUTPUT_FUNCTION_LABEL`` ).

  You may wish to use ``ASM_OUTPUT_TYPE_DIRECTIVE`` in the definition
  of this macro.

.. c:macro:: ASM_DECLARE_FUNCTION_SIZE (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the size of a function
  which is being defined.  The argument :samp:`{name}` is the name of the
  function.  The argument :samp:`{decl}` is the ``FUNCTION_DECL`` tree node
  representing the function.

  If this macro is not defined, then the function size is not defined.

  You may wish to use ``ASM_OUTPUT_MEASURED_SIZE`` in the definition
  of this macro.

.. c:macro:: ASM_DECLARE_COLD_FUNCTION_NAME (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the name :samp:`{name}` of a
  cold function partition which is being defined.  This macro is responsible
  for outputting the label definition (perhaps using
  ``ASM_OUTPUT_FUNCTION_LABEL`` ).  The argument :samp:`{decl}` is the
  ``FUNCTION_DECL`` tree node representing the function.

  If this macro is not defined, then the cold partition name is defined in the
  usual manner as a label (by means of ``ASM_OUTPUT_LABEL`` ).

  You may wish to use ``ASM_OUTPUT_TYPE_DIRECTIVE`` in the definition
  of this macro.

.. c:macro:: ASM_DECLARE_COLD_FUNCTION_SIZE (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the size of a cold function
  partition which is being defined.  The argument :samp:`{name}` is the name of the
  cold partition of the function.  The argument :samp:`{decl}` is the
  ``FUNCTION_DECL`` tree node representing the function.

  If this macro is not defined, then the partition size is not defined.

  You may wish to use ``ASM_OUTPUT_MEASURED_SIZE`` in the definition
  of this macro.

.. c:macro:: ASM_DECLARE_OBJECT_NAME (stream, name, decl)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the name :samp:`{name}` of an
  initialized variable which is being defined.  This macro must output the
  label definition (perhaps using ``ASM_OUTPUT_LABEL`` ).  The argument
  :samp:`{decl}` is the ``VAR_DECL`` tree node representing the variable.

  If this macro is not defined, then the variable name is defined in the
  usual manner as a label (by means of ``ASM_OUTPUT_LABEL`` ).

  You may wish to use ``ASM_OUTPUT_TYPE_DIRECTIVE`` and/or
  ``ASM_OUTPUT_SIZE_DIRECTIVE`` in the definition of this macro.

.. function:: void TARGET_ASM_DECLARE_CONSTANT_NAME (FILE *file, const char *name, const_tree expr, HOST_WIDE_INT size)

  A target hook to output to the stdio stream :samp:`{file}` any text necessary
  for declaring the name :samp:`{name}` of a constant which is being defined.  This
  target hook is responsible for outputting the label definition (perhaps using
  ``assemble_label`` ).  The argument :samp:`{exp}` is the value of the constant,
  and :samp:`{size}` is the size of the constant in bytes.  The :samp:`{name}`
  will be an internal label.

  The default version of this target hook, define the :samp:`{name}` in the
  usual manner as a label (by means of ``assemble_label`` ).

  You may wish to use ``ASM_OUTPUT_TYPE_DIRECTIVE`` in this target hook.

.. c:macro:: ASM_DECLARE_REGISTER_GLOBAL (stream, decl, regno, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for claiming a register :samp:`{regno}`
  for a global variable :samp:`{decl}` with name :samp:`{name}`.

  If you don't define this macro, that is equivalent to defining it to do
  nothing.

.. c:macro:: ASM_FINISH_DECLARE_OBJECT (stream, decl, toplevel, atend)

  A C statement (sans semicolon) to finish up declaring a variable name
  once the compiler has processed its initializer fully and thus has had a
  chance to determine the size of an array when controlled by an
  initializer.  This is used on systems where it's necessary to declare
  something about the size of the object.

  If you don't define this macro, that is equivalent to defining it to do
  nothing.

  You may wish to use ``ASM_OUTPUT_SIZE_DIRECTIVE`` and/or
  ``ASM_OUTPUT_MEASURED_SIZE`` in the definition of this macro.

.. function:: void TARGET_ASM_GLOBALIZE_LABEL (FILE *stream, const char *name)

  This target hook is a function to output to the stdio stream
  :samp:`{stream}` some commands that will make the label :samp:`{name}` global;
  that is, available for reference from other files.

  The default implementation relies on a proper definition of
  ``GLOBAL_ASM_OP``.

.. function:: void TARGET_ASM_GLOBALIZE_DECL_NAME (FILE *stream, tree decl)

  This target hook is a function to output to the stdio stream
  :samp:`{stream}` some commands that will make the name associated with :samp:`{decl}`
  global; that is, available for reference from other files.

  The default implementation uses the TARGET_ASM_GLOBALIZE_LABEL target hook.

.. function:: void TARGET_ASM_ASSEMBLE_UNDEFINED_DECL (FILE *stream, const char *name, const_tree decl)

  This target hook is a function to output to the stdio stream
  :samp:`{stream}` some commands that will declare the name associated with
  :samp:`{decl}` which is not defined in the current translation unit.  Most
  assemblers do not require anything to be output in this case.

.. c:macro:: ASM_WEAKEN_LABEL (stream, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` some commands that will make the label :samp:`{name}` weak;
  that is, available for reference from other files but only used if
  no other definition is available.  Use the expression
  ``assemble_name (stream, name)`` to output the name
  itself; before and after that, output the additional assembler syntax
  for making that name weak, and a newline.

  If you don't define this macro or ``ASM_WEAKEN_DECL``, GCC will not
  support weak symbols and you should not define the ``SUPPORTS_WEAK``
  macro.

.. c:macro:: ASM_WEAKEN_DECL (stream, decl, name, value)

  Combines (and replaces) the function of ``ASM_WEAKEN_LABEL`` and
  ``ASM_OUTPUT_WEAK_ALIAS``, allowing access to the associated function
  or variable decl.  If :samp:`{value}` is not ``NULL``, this C statement
  should output to the stdio stream :samp:`{stream}` assembler code which
  defines (equates) the weak symbol :samp:`{name}` to have the value
  :samp:`{value}`.  If :samp:`{value}` is ``NULL``, it should output commands
  to make :samp:`{name}` weak.

.. c:macro:: ASM_OUTPUT_WEAKREF (stream, decl, name, value)

  Outputs a directive that enables :samp:`{name}` to be used to refer to
  symbol :samp:`{value}` with weak-symbol semantics.  ``decl`` is the
  declaration of ``name``.

.. c:macro:: SUPPORTS_WEAK

  A preprocessor constant expression which evaluates to true if the target
  supports weak symbols.

  If you don't define this macro, defaults.h provides a default
  definition.  If either ``ASM_WEAKEN_LABEL`` or ``ASM_WEAKEN_DECL``
  is defined, the default definition is :samp:`1`; otherwise, it is :samp:`0`.

.. c:macro:: TARGET_SUPPORTS_WEAK

  A C expression which evaluates to true if the target supports weak symbols.

  If you don't define this macro, defaults.h provides a default
  definition.  The default definition is :samp:`(SUPPORTS_WEAK)`.  Define
  this macro if you want to control weak symbol support with a compiler
  flag such as :option:`-melf`.

.. c:macro:: MAKE_DECL_ONE_ONLY (decl)

  A C statement (sans semicolon) to mark :samp:`{decl}` to be emitted as a
  public symbol such that extra copies in multiple translation units will
  be discarded by the linker.  Define this macro if your object file
  format provides support for this concept, such as the :samp:`COMDAT`
  section flags in the Microsoft Windows PE/COFF format, and this support
  requires changes to :samp:`{decl}`, such as putting it in a separate section.

.. c:macro:: SUPPORTS_ONE_ONLY

  A C expression which evaluates to true if the target supports one-only
  semantics.

  If you don't define this macro, varasm.c provides a default
  definition.  If ``MAKE_DECL_ONE_ONLY`` is defined, the default
  definition is :samp:`1`; otherwise, it is :samp:`0`.  Define this macro if
  you want to control one-only symbol support with a compiler flag, or if
  setting the ``DECL_ONE_ONLY`` flag is enough to mark a declaration to
  be emitted as one-only.

.. function:: void TARGET_ASM_ASSEMBLE_VISIBILITY (tree decl, int visibility)

  This target hook is a function to output to :samp:`{asm_out_file}` some
  commands that will make the symbol(s) associated with :samp:`{decl}` have
  hidden, protected or internal visibility as specified by :samp:`{visibility}`.

.. c:macro:: TARGET_WEAK_NOT_IN_ARCHIVE_TOC

  A C expression that evaluates to true if the target's linker expects
  that weak symbols do not appear in a static archive's table of contents.
  The default is ``0``.

  Leaving weak symbols out of an archive's table of contents means that,
  if a symbol will only have a definition in one translation unit and
  will have undefined references from other translation units, that
  symbol should not be weak.  Defining this macro to be nonzero will
  thus have the effect that certain symbols that would normally be weak
  (explicit template instantiations, and vtables for polymorphic classes
  with noninline key methods) will instead be nonweak.

  The C++ ABI requires this macro to be zero.  Define this macro for
  targets where full C++ ABI compliance is impossible and where linker
  restrictions require weak symbols to be left out of a static archive's
  table of contents.

.. c:macro:: ASM_OUTPUT_EXTERNAL (stream, decl, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` any text necessary for declaring the name of an external
  symbol named :samp:`{name}` which is referenced in this compilation but
  not defined.  The value of :samp:`{decl}` is the tree node for the
  declaration.

  This macro need not be defined if it does not need to output anything.
  The GNU assembler and most Unix assemblers don't require anything.

.. function:: void TARGET_ASM_EXTERNAL_LIBCALL (rtx symref)

  This target hook is a function to output to :samp:`{asm_out_file}` an assembler
  pseudo-op to declare a library function name external.  The name of the
  library function is given by :samp:`{symref}`, which is a ``symbol_ref``.

.. function:: void TARGET_ASM_MARK_DECL_PRESERVED (const char *symbol)

  This target hook is a function to output to :samp:`{asm_out_file}` an assembler
  directive to annotate :samp:`{symbol}` as used.  The Darwin target uses the
  .no_dead_code_strip directive.

.. c:macro:: ASM_OUTPUT_LABELREF (stream, name)

  A C statement (sans semicolon) to output to the stdio stream
  :samp:`{stream}` a reference in assembler syntax to a label named
  :samp:`{name}`.  This should add :samp:`_` to the front of the name, if that
  is customary on your operating system, as it is in most Berkeley Unix
  systems.  This macro is used in ``assemble_name``.

.. function:: tree TARGET_MANGLE_ASSEMBLER_NAME (const char *name)

  Given a symbol :samp:`{name}`, perform same mangling as ``varasm.c`` 's ``assemble_name``, but in memory rather than to a file stream, returning result as an ``IDENTIFIER_NODE``.  Required for correct LTO symtabs.  The default implementation calls the ``TARGET_STRIP_NAME_ENCODING`` hook and then prepends the ``USER_LABEL_PREFIX``, if any.

.. c:macro:: ASM_OUTPUT_SYMBOL_REF (stream, sym)

  A C statement (sans semicolon) to output a reference to
  ``SYMBOL_REF`` :samp:`{sym}`.  If not defined, ``assemble_name``
  will be used to output the name of the symbol.  This macro may be used
  to modify the way a symbol is referenced depending on information
  encoded by ``TARGET_ENCODE_SECTION_INFO``.

.. c:macro:: ASM_OUTPUT_LABEL_REF (stream, buf)

  A C statement (sans semicolon) to output a reference to :samp:`{buf}`, the
  result of ``ASM_GENERATE_INTERNAL_LABEL``.  If not defined,
  ``assemble_name`` will be used to output the name of the symbol.
  This macro is not used by ``output_asm_label``, or the ``%l``
  specifier that calls it; the intention is that this macro should be set
  when it is necessary to output a label differently when its address is
  being taken.

.. function:: void TARGET_ASM_INTERNAL_LABEL (FILE *stream, const char *prefix, unsigned long labelno)

  A function to output to the stdio stream :samp:`{stream}` a label whose
  name is made from the string :samp:`{prefix}` and the number :samp:`{labelno}`.

  It is absolutely essential that these labels be distinct from the labels
  used for user-level functions and variables.  Otherwise, certain programs
  will have name conflicts with internal labels.

  It is desirable to exclude internal labels from the symbol table of the
  object file.  Most assemblers have a naming convention for labels that
  should be excluded; on many systems, the letter :samp:`L` at the
  beginning of a label has this effect.  You should find out what
  convention your system uses, and follow it.

  The default version of this function utilizes ``ASM_GENERATE_INTERNAL_LABEL``.

.. c:macro:: ASM_OUTPUT_DEBUG_LABEL (stream, prefix, num)

  A C statement to output to the stdio stream :samp:`{stream}` a debug info
  label whose name is made from the string :samp:`{prefix}` and the number
  :samp:`{num}`.  This is useful for VLIW targets, where debug info labels
  may need to be treated differently than branch target labels.  On some
  systems, branch target labels must be at the beginning of instruction
  bundles, but debug info labels can occur in the middle of instruction
  bundles.

  If this macro is not defined, then ``(*targetm.asm_out.internal_label)`` will be
  used.

.. c:macro:: ASM_GENERATE_INTERNAL_LABEL (string, prefix, num)

  A C statement to store into the string :samp:`{string}` a label whose name
  is made from the string :samp:`{prefix}` and the number :samp:`{num}`.

  This string, when output subsequently by ``assemble_name``, should
  produce the output that ``(*targetm.asm_out.internal_label)`` would produce
  with the same :samp:`{prefix}` and :samp:`{num}`.

  If the string begins with :samp:`*`, then ``assemble_name`` will
  output the rest of the string unchanged.  It is often convenient for
  ``ASM_GENERATE_INTERNAL_LABEL`` to use :samp:`*` in this way.  If the
  string doesn't start with :samp:`*`, then ``ASM_OUTPUT_LABELREF`` gets
  to output the string, and may change it.  (Of course,
  ``ASM_OUTPUT_LABELREF`` is also part of your machine description, so
  you should know what it does on your machine.)

.. c:macro:: ASM_FORMAT_PRIVATE_NAME (outvar, name, number)

  A C expression to assign to :samp:`{outvar}` (which is a variable of type
  ``char *`` ) a newly allocated string made from the string
  :samp:`{name}` and the number :samp:`{number}`, with some suitable punctuation
  added.  Use ``alloca`` to get space for the string.

  The string will be used as an argument to ``ASM_OUTPUT_LABELREF`` to
  produce an assembler label for an internal static variable whose name is
  :samp:`{name}`.  Therefore, the string must be such as to result in valid
  assembler code.  The argument :samp:`{number}` is different each time this
  macro is executed; it prevents conflicts between similarly-named
  internal static variables in different scopes.

  Ideally this string should not be a valid C identifier, to prevent any
  conflict with the user's own symbols.  Most assemblers allow periods
  or percent signs in assembler symbols; putting at least one of these
  between the name and the number will suffice.

  If this macro is not defined, a default definition will be provided
  which is correct for most systems.

.. c:macro:: ASM_OUTPUT_DEF (stream, name, value)

  A C statement to output to the stdio stream :samp:`{stream}` assembler code
  which defines (equates) the symbol :samp:`{name}` to have the value :samp:`{value}`.

  .. index:: SET_ASM_OP

  If ``SET_ASM_OP`` is defined, a default definition is provided which is
  correct for most systems.

.. c:macro:: ASM_OUTPUT_DEF_FROM_DECLS (stream, decl_of_name, decl_of_value)

  A C statement to output to the stdio stream :samp:`{stream}` assembler code
  which defines (equates) the symbol whose tree node is :samp:`{decl_of_name}`
  to have the value of the tree node :samp:`{decl_of_value}`.  This macro will
  be used in preference to :samp:`ASM_OUTPUT_DEF` if it is defined and if
  the tree nodes are available.

  .. index:: SET_ASM_OP

  If ``SET_ASM_OP`` is defined, a default definition is provided which is
  correct for most systems.

.. c:macro:: TARGET_DEFERRED_OUTPUT_DEFS (decl_of_name, decl_of_value)

  A C statement that evaluates to true if the assembler code which defines
  (equates) the symbol whose tree node is :samp:`{decl_of_name}` to have the value
  of the tree node :samp:`{decl_of_value}` should be emitted near the end of the
  current compilation unit.  The default is to not defer output of defines.
  This macro affects defines output by :samp:`ASM_OUTPUT_DEF` and
  :samp:`ASM_OUTPUT_DEF_FROM_DECLS`.

.. c:macro:: ASM_OUTPUT_WEAK_ALIAS (stream, name, value)

  A C statement to output to the stdio stream :samp:`{stream}` assembler code
  which defines (equates) the weak symbol :samp:`{name}` to have the value
  :samp:`{value}`.  If :samp:`{value}` is ``NULL``, it defines :samp:`{name}` as
  an undefined weak symbol.

  Define this macro if the target only supports weak aliases; define
  ``ASM_OUTPUT_DEF`` instead if possible.

.. c:macro:: OBJC_GEN_METHOD_LABEL (buf, is_inst, class_name, cat_name, sel_name)

  Define this macro to override the default assembler names used for
  Objective-C methods.

  The default name is a unique method number followed by the name of the
  class (e.g. :samp:`_1_Foo`).  For methods in categories, the name of
  the category is also included in the assembler name (e.g.
  :samp:`_1_Foo_Bar`).

  These names are safe on most systems, but make debugging difficult since
  the method's selector is not present in the name.  Therefore, particular
  systems define other ways of computing names.

  :samp:`{buf}` is an expression of type ``char *`` which gives you a
  buffer in which to store the name; its length is as long as
  :samp:`{class_name}`, :samp:`{cat_name}` and :samp:`{sel_name}` put together, plus
  50 characters extra.

  The argument :samp:`{is_inst}` specifies whether the method is an instance
  method or a class method; :samp:`{class_name}` is the name of the class;
  :samp:`{cat_name}` is the name of the category (or ``NULL`` if the method is not
  in a category); and :samp:`{sel_name}` is the name of the selector.

  On systems where the assembler can handle quoted names, you can use this
  macro to provide more human-readable names.

.. _initialization:

How Initialization Functions Are Handled
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: initialization routines

.. index:: termination routines

.. index:: constructors, output of

.. index:: destructors, output of

The compiled code for certain languages includes :dfn:`constructors`
(also called :dfn:`initialization routines`)-functions to initialize
data in the program when the program is started.  These functions need
to be called before the program is 'started'-that is to say, before
``main`` is called.

Compiling some languages generates :dfn:`destructors` (also called
:dfn:`termination routines`) that should be called when the program
terminates.

To make the initialization and termination functions work, the compiler
must output something in the assembler code to cause those functions to
be called at the appropriate time.  When you port the compiler to a new
system, you need to specify how to do this.

There are two major ways that GCC currently supports the execution of
initialization and termination functions.  Each way has two variants.
Much of the structure is common to all four variations.

.. index:: __CTOR_LIST__

.. index:: __DTOR_LIST__

The linker must build two lists of these functions-a list of
initialization functions, called ``__CTOR_LIST__``, and a list of
termination functions, called ``__DTOR_LIST__``.

Each list always begins with an ignored function pointer (which may hold
0, -1, or a count of the function pointers after it, depending on
the environment).  This is followed by a series of zero or more function
pointers to constructors (or destructors), followed by a function
pointer containing zero.

Depending on the operating system and its executable file format, either
crtstuff.c or libgcc2.c traverses these lists at startup
time and exit time.  Constructors are called in reverse order of the
list; destructors in forward order.

The best way to handle static constructors works only for object file
formats which provide arbitrarily-named sections.  A section is set
aside for a list of constructors, and another for a list of destructors.
Traditionally these are called :samp:`.ctors` and :samp:`.dtors`.  Each
object file that defines an initialization function also puts a word in
the constructor section to point to that function.  The linker
accumulates all these words into one contiguous :samp:`.ctors` section.
Termination functions are handled similarly.

This method will be chosen as the default by target-def.h if
``TARGET_ASM_NAMED_SECTION`` is defined.  A target that does not
support arbitrary sections, but does support special designated
constructor and destructor sections may define ``CTORS_SECTION_ASM_OP``
and ``DTORS_SECTION_ASM_OP`` to achieve the same effect.

When arbitrary sections are available, there are two variants, depending
upon how the code in crtstuff.c is called.  On systems that
support a :dfn:`.init` section which is executed at program startup,
parts of crtstuff.c are compiled into that section.  The
program is linked by the :command:`gcc` driver like this:

.. code-block:: c++

  ld -o output_file crti.o crtbegin.o ... -lgcc crtend.o crtn.o

The prologue of a function ( ``__init`` ) appears in the ``.init``
section of crti.o; the epilogue appears in crtn.o.  Likewise
for the function ``__fini`` in the :dfn:`.fini` section.  Normally these
files are provided by the operating system or by the GNU C library, but
are provided by GCC for a few targets.

The objects crtbegin.o and crtend.o are (for most targets)
compiled from crtstuff.c.  They contain, among other things, code
fragments within the ``.init`` and ``.fini`` sections that branch
to routines in the ``.text`` section.  The linker will pull all parts
of a section together, which results in a complete ``__init`` function
that invokes the routines we need at startup.

To use this variant, you must define the ``INIT_SECTION_ASM_OP``
macro properly.

If no init section is available, when GCC compiles any function called
``main`` (or more accurately, any function designated as a program
entry point by the language front end calling ``expand_main_function`` ),
it inserts a procedure call to ``__main`` as the first executable code
after the function prologue.  The ``__main`` function is defined
in libgcc2.c and runs the global constructors.

In file formats that don't support arbitrary sections, there are again
two variants.  In the simplest variant, the GNU linker (GNU ``ld`` )
and an 'a.out' format must be used.  In this case,
``TARGET_ASM_CONSTRUCTOR`` is defined to produce a ``.stabs``
entry of type :samp:`N_SETT`, referencing the name ``__CTOR_LIST__``,
and with the address of the void function containing the initialization
code as its value.  The GNU linker recognizes this as a request to add
the value to a :dfn:`set`; the values are accumulated, and are eventually
placed in the executable as a vector in the format described above, with
a leading (ignored) count and a trailing zero element.
``TARGET_ASM_DESTRUCTOR`` is handled similarly.  Since no init
section is available, the absence of ``INIT_SECTION_ASM_OP`` causes
the compilation of ``main`` to call ``__main`` as above, starting
the initialization process.

The last variant uses neither arbitrary sections nor the GNU linker.
This is preferable when you want to do dynamic linking and when using
file formats which the GNU linker does not support, such as 'ECOFF'.  In
this case, ``TARGET_HAVE_CTORS_DTORS`` is false, initialization and
termination functions are recognized simply by their names.  This requires
an extra program in the linkage step, called :command:`collect2`.  This program
pretends to be the linker, for use with GCC; it does its job by running
the ordinary linker, but also arranges to include the vectors of
initialization and termination functions.  These functions are called
via ``__main`` as described above.  In order to use this method,
``use_collect2`` must be defined in the target in config.gcc.

.. _macros-for-initialization:

Macros Controlling Initialization Routines
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here are the macros that control how the compiler handles initialization
and termination functions:

.. c:macro:: INIT_SECTION_ASM_OP

  If defined, a C string constant, including spacing, for the assembler
  operation to identify the following data as initialization code.  If not
  defined, GCC will assume such a section does not exist.  When you are
  using special sections for initialization and termination functions, this
  macro also controls how crtstuff.c and libgcc2.c arrange to
  run the initialization functions.

.. c:macro:: HAS_INIT_SECTION

  If defined, ``main`` will not call ``__main`` as described above.
  This macro should be defined for systems that control start-up code
  on a symbol-by-symbol basis, such as OSF/1, and should not
  be defined explicitly for systems that support ``INIT_SECTION_ASM_OP``.

.. c:macro:: LD_INIT_SWITCH

  If defined, a C string constant for a switch that tells the linker that
  the following symbol is an initialization routine.

.. c:macro:: LD_FINI_SWITCH

  If defined, a C string constant for a switch that tells the linker that
  the following symbol is a finalization routine.

.. c:macro:: COLLECT_SHARED_INIT_FUNC (stream, func)

  If defined, a C statement that will write a function that can be
  automatically called when a shared library is loaded.  The function
  should call :samp:`{func}`, which takes no arguments.  If not defined, and
  the object format requires an explicit initialization function, then a
  function called ``_GLOBAL__DI`` will be generated.

  This function and the following one are used by collect2 when linking a
  shared library that needs constructors or destructors, or has DWARF2
  exception tables embedded in the code.

.. c:macro:: COLLECT_SHARED_FINI_FUNC (stream, func)

  If defined, a C statement that will write a function that can be
  automatically called when a shared library is unloaded.  The function
  should call :samp:`{func}`, which takes no arguments.  If not defined, and
  the object format requires an explicit finalization function, then a
  function called ``_GLOBAL__DD`` will be generated.

.. c:macro:: INVOKE__main

  If defined, ``main`` will call ``__main`` despite the presence of
  ``INIT_SECTION_ASM_OP``.  This macro should be defined for systems
  where the init section is not actually run automatically, but is still
  useful for collecting the lists of constructors and destructors.

.. c:macro:: SUPPORTS_INIT_PRIORITY

  If nonzero, the C++ ``init_priority`` attribute is supported and the
  compiler should emit instructions to control the order of initialization
  of objects.  If zero, the compiler will issue an error message upon
  encountering an ``init_priority`` attribute.

.. c:var:: bool TARGET_HAVE_CTORS_DTORS

  This value is true if the target supports some 'native' method of
  collecting constructors and destructors to be run at startup and exit.
  It is false if we must use :command:`collect2`.

.. function:: void TARGET_ASM_CONSTRUCTOR (rtx symbol, int priority)

  If defined, a function that outputs assembler code to arrange to call
  the function referenced by :samp:`{symbol}` at initialization time.

  Assume that :samp:`{symbol}` is a ``SYMBOL_REF`` for a function taking
  no arguments and with no return value.  If the target supports initialization
  priorities, :samp:`{priority}` is a value between 0 and ``MAX_INIT_PRIORITY`` ;
  otherwise it must be ``DEFAULT_INIT_PRIORITY``.

  If this macro is not defined by the target, a suitable default will
  be chosen if (1) the target supports arbitrary section names, (2) the
  target defines ``CTORS_SECTION_ASM_OP``, or (3) ``USE_COLLECT2``
  is not defined.

.. function:: void TARGET_ASM_DESTRUCTOR (rtx symbol, int priority)

  This is like ``TARGET_ASM_CONSTRUCTOR`` but used for termination
  functions rather than initialization functions.

If ``TARGET_HAVE_CTORS_DTORS`` is true, the initialization routine
generated for the generated object file will have static linkage.

If your system uses :command:`collect2` as the means of processing
constructors, then that program normally uses :command:`nm` to scan
an object file for constructor functions to be called.

On certain kinds of systems, you can define this macro to make
:command:`collect2` work faster (and, in some cases, make it work at all):

.. c:macro:: OBJECT_FORMAT_COFF

  Define this macro if the system uses COFF (Common Object File Format)
  object files, so that :command:`collect2` can assume this format and scan
  object files directly for dynamic constructor/destructor functions.

  This macro is effective only in a native compiler; :command:`collect2` as
  part of a cross compiler always uses :command:`nm` for the target machine.

.. c:macro:: REAL_NM_FILE_NAME

  Define this macro as a C string constant containing the file name to use
  to execute :command:`nm`.  The default is to search the path normally for
  :command:`nm`.

.. c:macro:: NM_FLAGS

  :command:`collect2` calls :command:`nm` to scan object files for static
  constructors and destructors and LTO info.  By default, :option:`-n` is
  passed.  Define ``NM_FLAGS`` to a C string constant if other options
  are needed to get the same output format as GNU :command:`nm -n`
  produces.

If your system supports shared libraries and has a program to list the
dynamic dependencies of a given library or executable, you can define
these macros to enable support for running initialization and
termination functions in shared libraries:

.. c:macro:: LDD_SUFFIX

  Define this macro to a C string constant containing the name of the program
  which lists dynamic dependencies, like :command:`ldd` under SunOS 4.

.. c:macro:: PARSE_LDD_OUTPUT (ptr)

  Define this macro to be C code that extracts filenames from the output
  of the program denoted by ``LDD_SUFFIX``.  :samp:`{ptr}` is a variable
  of type ``char *`` that points to the beginning of a line of output
  from ``LDD_SUFFIX``.  If the line lists a dynamic dependency, the
  code must advance :samp:`{ptr}` to the beginning of the filename on that
  line.  Otherwise, it must set :samp:`{ptr}` to ``NULL``.

.. c:macro:: SHLIB_SUFFIX

  Define this macro to a C string constant containing the default shared
  library extension of the target (e.g., :samp:`".so"`).  :command:`collect2`
  strips version information after this suffix when generating global
  constructor and destructor names.  This define is only needed on targets
  that use :command:`collect2` to process constructors and destructors.

.. _instruction-output:

Output of Assembler Instructions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This describes assembler instruction output.

.. c:macro:: REGISTER_NAMES

  A C initializer containing the assembler's names for the machine
  registers, each one as a C string constant.  This is what translates
  register numbers in the compiler into assembler language.

.. c:macro:: ADDITIONAL_REGISTER_NAMES

  If defined, a C initializer for an array of structures containing a name
  and a register number.  This macro defines additional names for hard
  registers, thus allowing the ``asm`` option in declarations to refer
  to registers using alternate names.

.. c:macro:: OVERLAPPING_REGISTER_NAMES

  If defined, a C initializer for an array of structures containing a
  name, a register number and a count of the number of consecutive
  machine registers the name overlaps.  This macro defines additional
  names for hard registers, thus allowing the ``asm`` option in
  declarations to refer to registers using alternate names.  Unlike
  ``ADDITIONAL_REGISTER_NAMES``, this macro should be used when the
  register name implies multiple underlying registers.

  This macro should be used when it is important that a clobber in an
  ``asm`` statement clobbers all the underlying values implied by the
  register name.  For example, on ARM, clobbering the double-precision
  VFP register 'd0' implies clobbering both single-precision registers
  's0' and 's1'.

.. c:macro:: ASM_OUTPUT_OPCODE (stream, ptr)

  Define this macro if you are using an unusual assembler that
  requires different names for the machine instructions.

  The definition is a C statement or statements which output an
  assembler instruction opcode to the stdio stream :samp:`{stream}`.  The
  macro-operand :samp:`{ptr}` is a variable of type ``char *`` which
  points to the opcode name in its 'internal' form-the form that is
  written in the machine description.  The definition should output the
  opcode name to :samp:`{stream}`, performing any translation you desire, and
  increment the variable :samp:`{ptr}` to point at the end of the opcode
  so that it will not be output twice.

  In fact, your macro definition may process less than the entire opcode
  name, or more than the opcode name; but if you want to process text
  that includes :samp:`%`-sequences to substitute operands, you must take
  care of the substitution yourself.  Just be sure to increment
  :samp:`{ptr}` over whatever text should not be output normally.

  .. index:: recog_data.operand

  If you need to look at the operand values, they can be found as the
  elements of ``recog_data.operand``.

  If the macro definition does nothing, the instruction is output
  in the usual way.

.. c:macro:: FINAL_PRESCAN_INSN (insn, opvec, noperands)

  If defined, a C statement to be executed just prior to the output of
  assembler code for :samp:`{insn}`, to modify the extracted operands so
  they will be output differently.

  Here the argument :samp:`{opvec}` is the vector containing the operands
  extracted from :samp:`{insn}`, and :samp:`{noperands}` is the number of
  elements of the vector which contain meaningful data for this insn.
  The contents of this vector are what will be used to convert the insn
  template into assembler code, so you can change the assembler output
  by changing the contents of the vector.

  This macro is useful when various assembler syntaxes share a single
  file of instruction patterns; by defining this macro differently, you
  can cause a large class of instructions to be output differently (such
  as with rearranged operands).  Naturally, variations in assembler
  syntax affecting individual insn patterns ought to be handled by
  writing conditional output routines in those patterns.

  If this macro is not defined, it is equivalent to a null statement.

.. function:: void TARGET_ASM_FINAL_POSTSCAN_INSN (FILE *file, rtx_insn *insn, rtx *opvec, int noperands)

  If defined, this target hook is a function which is executed just after the
  output of assembler code for :samp:`{insn}`, to change the mode of the assembler
  if necessary.

  Here the argument :samp:`{opvec}` is the vector containing the operands
  extracted from :samp:`{insn}`, and :samp:`{noperands}` is the number of
  elements of the vector which contain meaningful data for this insn.
  The contents of this vector are what was used to convert the insn
  template into assembler code, so you can change the assembler mode
  by checking the contents of the vector.

.. c:macro:: PRINT_OPERAND (stream, x, code)

  A C compound statement to output to stdio stream :samp:`{stream}` the
  assembler syntax for an instruction operand :samp:`{x}`.  :samp:`{x}` is an
  RTL expression.

  :samp:`{code}` is a value that can be used to specify one of several ways
  of printing the operand.  It is used when identical operands must be
  printed differently depending on the context.  :samp:`{code}` comes from
  the :samp:`%` specification that was used to request printing of the
  operand.  If the specification was just :samp:`%{digit}` then
  :samp:`{code}` is 0; if the specification was :samp:`%{ltr}{digit}` then :samp:`{code}` is the ASCII code for :samp:`{ltr}`.

  .. index:: reg_names

  If :samp:`{x}` is a register, this macro should print the register's name.
  The names can be found in an array ``reg_names`` whose type is
  ``char *[]``.  ``reg_names`` is initialized from
  ``REGISTER_NAMES``.

  When the machine description has a specification :samp:`%{punct}`
  (a :samp:`%` followed by a punctuation character), this macro is called
  with a null pointer for :samp:`{x}` and the punctuation character for
  :samp:`{code}`.

.. c:macro:: PRINT_OPERAND_PUNCT_VALID_P (code)

  A C expression which evaluates to true if :samp:`{code}` is a valid
  punctuation character for use in the ``PRINT_OPERAND`` macro.  If
  ``PRINT_OPERAND_PUNCT_VALID_P`` is not defined, it means that no
  punctuation characters (except for the standard one, :samp:`%`) are used
  in this way.

.. c:macro:: PRINT_OPERAND_ADDRESS (stream, x)

  A C compound statement to output to stdio stream :samp:`{stream}` the
  assembler syntax for an instruction operand that is a memory reference
  whose address is :samp:`{x}`.  :samp:`{x}` is an RTL expression.

  .. index:: TARGET_ENCODE_SECTION_INFO usage

  On some machines, the syntax for a symbolic address depends on the
  section that the address refers to.  On these machines, define the hook
  ``TARGET_ENCODE_SECTION_INFO`` to store the information into the
  ``symbol_ref``, and then check for it here.  See :ref:`assembler-format`.

.. index:: dbr_sequence_length

.. c:macro:: DBR_OUTPUT_SEQEND (file)

  A C statement, to be executed after all slot-filler instructions have
  been output.  If necessary, call ``dbr_sequence_length`` to
  determine the number of slots filled in a sequence (zero if not
  currently outputting a sequence), to decide how many no-ops to output,
  or whatever.

  Don't define this macro if it has nothing to do, but it is helpful in
  reading assembly output if the extent of the delay sequence is made
  explicit (e.g. with white space).

.. index:: final_sequence

Note that output routines for instructions with delay slots must be
prepared to deal with not being output as part of a sequence
(i.e. when the scheduling pass is not run, or when no slot fillers could be
found.)  The variable ``final_sequence`` is null when not
processing a sequence, otherwise it contains the ``sequence`` rtx
being output.

.. index:: asm_fprintf

.. c:macro:: REGISTER_PREFIX

  If defined, C string expressions to be used for the :samp:`%R`, :samp:`%L`,
  :samp:`%U`, and :samp:`%I` options of ``asm_fprintf`` (see
  final.c).  These are useful when a single md file must
  support multiple assembler formats.  In that case, the various tm.h
  files can define these macros differently.

.. c:macro:: ASM_FPRINTF_EXTENSIONS (file, argptr, format)

  If defined this macro should expand to a series of ``case``
  statements which will be parsed inside the ``switch`` statement of
  the ``asm_fprintf`` function.  This allows targets to define extra
  printf formats which may useful when generating their assembler
  statements.  Note that uppercase letters are reserved for future
  generic extensions to asm_fprintf, and so are not available to target
  specific code.  The output file is given by the parameter :samp:`{file}`.
  The varargs input pointer is :samp:`{argptr}` and the rest of the format
  string, starting the character after the one that is being switched
  upon, is pointed to by :samp:`{format}`.

.. c:macro:: ASSEMBLER_DIALECT

  If your target supports multiple dialects of assembler language (such as
  different opcodes), define this macro as a C expression that gives the
  numeric index of the assembler language dialect to use, with zero as the
  first variant.

  If this macro is defined, you may use constructs of the form

  .. code-block:: c++

    {option0|option1|option2...}

  in the output templates of patterns (see :ref:`output-template`) or in the
  first argument of ``asm_fprintf``.  This construct outputs
  :samp:`option0`, :samp:`option1`, :samp:`option2`, etc., if the value of
  ``ASSEMBLER_DIALECT`` is zero, one, two, etc.  Any special characters
  within these strings retain their usual meaning.  If there are fewer
  alternatives within the braces than the value of
  ``ASSEMBLER_DIALECT``, the construct outputs nothing. If it's needed
  to print curly braces or :samp:`|` character in assembler output directly,
  :samp:`%{`, :samp:`%}` and :samp:`%|` can be used.

  If you do not define this macro, the characters :samp:`{`, :samp:`|` and
  :samp:`}` do not have any special meaning when used in templates or
  operands to ``asm_fprintf``.

  Define the macros ``REGISTER_PREFIX``, ``LOCAL_LABEL_PREFIX``,
  ``USER_LABEL_PREFIX`` and ``IMMEDIATE_PREFIX`` if you can express
  the variations in assembler language syntax with that mechanism.  Define
  ``ASSEMBLER_DIALECT`` and use the :samp:`{option0|option1}` syntax
  if the syntax variant are larger and involve such things as different
  opcodes or operand order.

.. c:macro:: ASM_OUTPUT_REG_PUSH (stream, regno)

  A C expression to output to :samp:`{stream}` some assembler code
  which will push hard register number :samp:`{regno}` onto the stack.
  The code need not be optimal, since this macro is used only when
  profiling.

.. c:macro:: ASM_OUTPUT_REG_POP (stream, regno)

  A C expression to output to :samp:`{stream}` some assembler code
  which will pop hard register number :samp:`{regno}` off of the stack.
  The code need not be optimal, since this macro is used only when
  profiling.

.. _dispatch-tables:

Output of Dispatch Tables
^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This concerns dispatch tables.

.. index:: dispatch table

.. c:macro:: ASM_OUTPUT_ADDR_DIFF_ELT (stream, body, value, rel)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  pseudo-instruction to generate a difference between two labels.
  :samp:`{value}` and :samp:`{rel}` are the numbers of two internal labels.  The
  definitions of these labels are output using
  ``(*targetm.asm_out.internal_label)``, and they must be printed in the same
  way here.  For example,

  .. code-block:: c++

    fprintf (stream, "\t.word L%d-L%d\n",
             value, rel)

  You must provide this macro on machines where the addresses in a
  dispatch table are relative to the table's own address.  If defined, GCC
  will also use this macro on all machines when producing PIC.
  :samp:`{body}` is the body of the ``ADDR_DIFF_VEC`` ; it is provided so that the
  mode and flags can be read.

.. c:macro:: ASM_OUTPUT_ADDR_VEC_ELT (stream, value)

  This macro should be provided on machines where the addresses
  in a dispatch table are absolute.

  The definition should be a C statement to output to the stdio stream
  :samp:`{stream}` an assembler pseudo-instruction to generate a reference to
  a label.  :samp:`{value}` is the number of an internal label whose
  definition is output using ``(*targetm.asm_out.internal_label)``.
  For example,

  .. code-block:: c++

    fprintf (stream, "\t.word L%d\n", value)

.. c:macro:: ASM_OUTPUT_CASE_LABEL (stream, prefix, num, table)

  Define this if the label before a jump-table needs to be output
  specially.  The first three arguments are the same as for
  ``(*targetm.asm_out.internal_label)`` ; the fourth argument is the
  jump-table which follows (a ``jump_table_data`` containing an
  ``addr_vec`` or ``addr_diff_vec`` ).

  This feature is used on system V to output a ``swbeg`` statement
  for the table.

  If this macro is not defined, these labels are output with
  ``(*targetm.asm_out.internal_label)``.

.. c:macro:: ASM_OUTPUT_CASE_END (stream, num, table)

  Define this if something special must be output at the end of a
  jump-table.  The definition should be a C statement to be executed
  after the assembler code for the table is written.  It should write
  the appropriate code to stdio stream :samp:`{stream}`.  The argument
  :samp:`{table}` is the jump-table insn, and :samp:`{num}` is the label-number
  of the preceding label.

  If this macro is not defined, nothing special is output at the end of
  the jump-table.

.. function:: void TARGET_ASM_POST_CFI_STARTPROC (FILE *, tree)

  This target hook is used to emit assembly strings required by the target
  after the .cfi_startproc directive.  The first argument is the file stream to
  write the strings to and the second argument is the function's declaration.  The
  expected use is to add more .cfi_* directives.

  The default is to not output any assembly strings.

.. function:: void TARGET_ASM_EMIT_UNWIND_LABEL (FILE *stream, tree decl, int for_eh, int empty)

  This target hook emits a label at the beginning of each FDE.  It
  should be defined on targets where FDEs need special labels, and it
  should write the appropriate label, for the FDE associated with the
  function declaration :samp:`{decl}`, to the stdio stream :samp:`{stream}`.
  The third argument, :samp:`{for_eh}`, is a boolean: true if this is for an
  exception table.  The fourth argument, :samp:`{empty}`, is a boolean:
  true if this is a placeholder label for an omitted FDE.

  The default is that FDEs are not given nonlocal labels.

.. function:: void TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL (FILE *stream)

  This target hook emits a label at the beginning of the exception table.
  It should be defined on targets where it is desirable for the table
  to be broken up according to function.

  The default is that no label is emitted.

.. function:: void TARGET_ASM_EMIT_EXCEPT_PERSONALITY (rtx personality)

  If the target implements ``TARGET_ASM_UNWIND_EMIT``, this hook may be used to emit a directive to install a personality hook into the unwind info.  This hook should not be used if dwarf2 unwind info is used.

.. function:: void TARGET_ASM_UNWIND_EMIT (FILE *stream, rtx_insn *insn)

  This target hook emits assembly directives required to unwind the
  given instruction.  This is only used when ``TARGET_EXCEPT_UNWIND_INFO``
  returns ``UI_TARGET``.

.. function:: rtx TARGET_ASM_MAKE_EH_SYMBOL_INDIRECT (rtx origsymbol, bool pubvis)

  If necessary, modify personality and LSDA references to handle indirection.  The original symbol is in ``origsymbol`` and if ``pubvis`` is true  the symbol is visible outside the TU.

.. c:var:: bool TARGET_ASM_UNWIND_EMIT_BEFORE_INSN

  True if the ``TARGET_ASM_UNWIND_EMIT`` hook should be called before the assembly for :samp:`{insn}` has been emitted, false if the hook should be called afterward.

.. function:: bool TARGET_ASM_SHOULD_RESTORE_CFA_STATE (void)

  For DWARF-based unwind frames, two CFI instructions provide for save and restore of register state.  GCC maintains the current frame address (CFA) separately from the register bank but the unwinder in libgcc preserves this state along with the registers (and this is expected by the code that writes the unwind frames).  This hook allows the target to specify that the CFA data is not saved/restored along with the registers by the target unwinder so that suitable additional instructions should be emitted to restore it.

.. _exception-region-output:

Assembler Commands for Exception Regions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This describes commands marking the start and the end of an exception
region.

.. c:macro:: EH_FRAME_SECTION_NAME

  If defined, a C string constant for the name of the section containing
  exception handling frame unwind information.  If not defined, GCC will
  provide a default definition if the target supports named sections.
  crtstuff.c uses this macro to switch to the appropriate section.

  You should define this symbol if your target supports DWARF 2 frame
  unwind information and the default definition does not work.

.. c:macro:: EH_FRAME_THROUGH_COLLECT2

  If defined, DWARF 2 frame unwind information will identified by
  specially named labels.  The collect2 process will locate these
  labels and generate code to register the frames.

  This might be necessary, for instance, if the system linker will not
  place the eh_frames in-between the sentinals from crtstuff.c,
  or if the system linker does garbage collection and sections cannot
  be marked as not to be collected.

.. c:macro:: EH_TABLES_CAN_BE_READ_ONLY

  Define this macro to 1 if your target is such that no frame unwind
  information encoding used with non-PIC code will ever require a
  runtime relocation, but the linker may not support merging read-only
  and read-write sections into a single read-write section.

.. c:macro:: MASK_RETURN_ADDR

  An rtx used to mask the return address found via ``RETURN_ADDR_RTX``, so
  that it does not contain any extraneous set bits in it.

.. c:macro:: DWARF2_UNWIND_INFO

  Define this macro to 0 if your target supports DWARF 2 frame unwind
  information, but it does not yet work with exception handling.
  Otherwise, if your target supports this information (if it defines
  ``INCOMING_RETURN_ADDR_RTX`` and ``OBJECT_FORMAT_ELF`` ),
  GCC will provide a default definition of 1.

.. function:: enum unwind_info_type TARGET_EXCEPT_UNWIND_INFO (struct gcc_options *opts)

  This hook defines the mechanism that will be used for exception handling
  by the target.  If the target has ABI specified unwind tables, the hook
  should return ``UI_TARGET``.  If the target is to use the
  ``setjmp`` / ``longjmp`` -based exception handling scheme, the hook
  should return ``UI_SJLJ``.  If the target supports DWARF 2 frame unwind
  information, the hook should return ``UI_DWARF2``.

  A target may, if exceptions are disabled, choose to return ``UI_NONE``.
  This may end up simplifying other parts of target-specific code.  The
  default implementation of this hook never returns ``UI_NONE``.

  Note that the value returned by this hook should be constant.  It should
  not depend on anything except the command-line switches described by
  :samp:`{opts}`.  In particular, the
  setting ``UI_SJLJ`` must be fixed at compiler start-up as C pre-processor
  macros and builtin functions related to exception handling are set up
  depending on this setting.

  The default implementation of the hook first honors the
  :option:`--enable-sjlj-exceptions` configure option, then
  ``DWARF2_UNWIND_INFO``, and finally defaults to ``UI_SJLJ``.  If
  ``DWARF2_UNWIND_INFO`` depends on command-line options, the target
  must define this hook so that :samp:`{opts}` is used correctly.

.. c:var:: bool TARGET_UNWIND_TABLES_DEFAULT

  This variable should be set to ``true`` if the target ABI requires unwinding
  tables even when exceptions are not used.  It must not be modified by
  command-line option processing.

.. c:macro:: DONT_USE_BUILTIN_SETJMP

  Define this macro to 1 if the ``setjmp`` / ``longjmp`` -based scheme
  should use the ``setjmp`` / ``longjmp`` functions from the C library
  instead of the ``__builtin_setjmp`` / ``__builtin_longjmp`` machinery.

.. c:macro:: JMP_BUF_SIZE

  This macro has no effect unless ``DONT_USE_BUILTIN_SETJMP`` is also
  defined.  Define this macro if the default size of ``jmp_buf`` buffer
  for the ``setjmp`` / ``longjmp`` -based exception handling mechanism
  is not large enough, or if it is much too large.
  The default size is ``FIRST_PSEUDO_REGISTER * sizeof(void *)``.

.. c:macro:: DWARF_CIE_DATA_ALIGNMENT

  This macro need only be defined if the target might save registers in the
  function prologue at an offset to the stack pointer that is not aligned to
  ``UNITS_PER_WORD``.  The definition should be the negative minimum
  alignment if ``STACK_GROWS_DOWNWARD`` is true, and the positive
  minimum alignment otherwise.  See :ref:`dwarf`.  Only applicable if
  the target supports DWARF 2 frame unwind information.

.. c:var:: bool TARGET_TERMINATE_DW2_EH_FRAME_INFO

  Contains the value true if the target should add a zero word onto the
  end of a Dwarf-2 frame info section when used for exception handling.
  Default value is false if ``EH_FRAME_SECTION_NAME`` is defined, and
  true otherwise.

.. function:: rtx TARGET_DWARF_REGISTER_SPAN (rtx reg)

  Given a register, this hook should return a parallel of registers to
  represent where to find the register pieces.  Define this hook if the
  register and its mode are represented in Dwarf in non-contiguous
  locations, or if the register should be represented in more than one
  register in Dwarf.  Otherwise, this hook should return ``NULL_RTX``.
  If not defined, the default is to return ``NULL_RTX``.

.. function:: machine_mode TARGET_DWARF_FRAME_REG_MODE (int regno)

  Given a register, this hook should return the mode which the
  corresponding Dwarf frame register should have.  This is normally
  used to return a smaller mode than the raw mode to prevent call
  clobbered parts of a register altering the frame register size

.. function:: void TARGET_INIT_DWARF_REG_SIZES_EXTRA (tree address)

  If some registers are represented in Dwarf-2 unwind information in
  multiple pieces, define this hook to fill in information about the
  sizes of those pieces in the table used by the unwinder at runtime.
  It will be called by ``expand_builtin_init_dwarf_reg_sizes`` after
  filling in a single size corresponding to each hard register;
  :samp:`{address}` is the address of the table.

.. function:: bool TARGET_ASM_TTYPE (rtx sym)

  This hook is used to output a reference from a frame unwinding table to
  the type_info object identified by :samp:`{sym}`.  It should return ``true``
  if the reference was output.  Returning ``false`` will cause the
  reference to be output using the normal Dwarf2 routines.

.. c:var:: bool TARGET_ARM_EABI_UNWINDER

  This flag should be set to ``true`` on targets that use an ARM EABI
  based unwinding library, and ``false`` on other targets.  This effects
  the format of unwinding tables, and how the unwinder in entered after
  running a cleanup.  The default is ``false``.

.. _alignment-output:

Assembler Commands for Alignment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This describes commands for alignment.

.. c:macro:: JUMP_ALIGN (label)

  The alignment (log base 2) to put in front of :samp:`{label}`, which is
  a common destination of jumps and has no fallthru incoming edge.

  This macro need not be defined if you don't want any special alignment
  to be done at such a time.  Most machine descriptions do not currently
  define the macro.

  Unless it's necessary to inspect the :samp:`{label}` parameter, it is better
  to set the variable :samp:`{align_jumps}` in the target's
  ``TARGET_OPTION_OVERRIDE``.  Otherwise, you should try to honor the user's
  selection in :samp:`{align_jumps}` in a ``JUMP_ALIGN`` implementation.

.. c:macro:: LABEL_ALIGN_AFTER_BARRIER (label)

  The alignment (log base 2) to put in front of :samp:`{label}`, which follows
  a ``BARRIER``.

  This macro need not be defined if you don't want any special alignment
  to be done at such a time.  Most machine descriptions do not currently
  define the macro.

.. c:macro:: LOOP_ALIGN (label)

  The alignment (log base 2) to put in front of :samp:`{label}` that heads
  a frequently executed basic block (usually the header of a loop).

  This macro need not be defined if you don't want any special alignment
  to be done at such a time.  Most machine descriptions do not currently
  define the macro.

  Unless it's necessary to inspect the :samp:`{label}` parameter, it is better
  to set the variable ``align_loops`` in the target's
  ``TARGET_OPTION_OVERRIDE``.  Otherwise, you should try to honor the user's
  selection in ``align_loops`` in a ``LOOP_ALIGN`` implementation.

.. c:macro:: LABEL_ALIGN (label)

  The alignment (log base 2) to put in front of :samp:`{label}`.
  If ``LABEL_ALIGN_AFTER_BARRIER`` / ``LOOP_ALIGN`` specify a different alignment,
  the maximum of the specified values is used.

  Unless it's necessary to inspect the :samp:`{label}` parameter, it is better
  to set the variable ``align_labels`` in the target's
  ``TARGET_OPTION_OVERRIDE``.  Otherwise, you should try to honor the user's
  selection in ``align_labels`` in a ``LABEL_ALIGN`` implementation.

.. c:macro:: ASM_OUTPUT_SKIP (stream, nbytes)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  instruction to advance the location counter by :samp:`{nbytes}` bytes.
  Those bytes should be zero when loaded.  :samp:`{nbytes}` will be a C
  expression of type ``unsigned HOST_WIDE_INT``.

.. c:macro:: ASM_NO_SKIP_IN_TEXT

  Define this macro if ``ASM_OUTPUT_SKIP`` should not be used in the
  text section because it fails to put zeros in the bytes that are skipped.
  This is true on many Unix systems, where the pseudo-op to skip bytes
  produces no-op instructions rather than zeros when used in the text
  section.

.. c:macro:: ASM_OUTPUT_ALIGN (stream, power)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  command to advance the location counter to a multiple of 2 to the
  :samp:`{power}` bytes.  :samp:`{power}` will be a C expression of type ``int``.

.. c:macro:: ASM_OUTPUT_ALIGN_WITH_NOP (stream, power)

  Like ``ASM_OUTPUT_ALIGN``, except that the 'nop' instruction is used
  for padding, if necessary.

.. c:macro:: ASM_OUTPUT_MAX_SKIP_ALIGN (stream, power, max_skip)

  A C statement to output to the stdio stream :samp:`{stream}` an assembler
  command to advance the location counter to a multiple of 2 to the
  :samp:`{power}` bytes, but only if :samp:`{max_skip}` or fewer bytes are needed to
  satisfy the alignment request.  :samp:`{power}` and :samp:`{max_skip}` will be
  a C expression of type ``int``.

