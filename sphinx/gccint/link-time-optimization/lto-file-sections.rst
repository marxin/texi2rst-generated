.. _lto-object-file-layout:

LTO file sections
*****************

LTO information is stored in several ELF sections inside object files.
Data structures and enum codes for sections are defined in
lto-streamer.h.

These sections are emitted from lto-streamer-out.c and mapped
in all at once from lto/lto.c: ``lto_file_read``.  The
individual functions dealing with the reading/writing of each section
are described below.

* Command line options ( ``.gnu.lto_.opts`` )

  This section contains the command line options used to generate the
  object files.  This is used at link time to determine the optimization
  level and other settings when they are not explicitly specified at the
  linker command line.

  Currently, GCC does not support combining LTO object files compiled
  with different set of the command line options into a single binary.
  At link time, the options given on the command line and the options
  saved on all the files in a link-time set are applied globally.  No
  attempt is made at validating the combination of flags (other than the
  usual validation done by option processing).  This is implemented in
  lto/lto.c: ``lto_read_all_file_options``.

* Symbol table ( ``.gnu.lto_.symtab`` )

  This table replaces the ELF symbol table for functions and variables
  represented in the LTO IL.  Symbols used and exported by the optimized
  assembly code of 'fat' objects might not match the ones used and
  exported by the intermediate code.  This table is necessary because
  the intermediate code is less optimized and thus requires a separate
  symbol table.

  Additionally, the binary code in the 'fat' object will lack a call
  to a function, since the call was optimized out at compilation time
  after the intermediate language was streamed out.  In some special
  cases, the same optimization may not happen during link-time
  optimization.  This would lead to an undefined symbol if only one
  symbol table was used.

  The symbol table is emitted in
  lto-streamer-out.c: ``produce_symtab``.

* Global declarations and types ( ``.gnu.lto_.decls`` )

  This section contains an intermediate language dump of all
  declarations and types required to represent the callgraph, static
  variables and top-level debug info.

  The contents of this section are emitted in
  lto-streamer-out.c: ``produce_asm_for_decls``.  Types and
  symbols are emitted in a topological order that preserves the sharing
  of pointers when the file is read back in
  (lto.c: ``read_cgraph_and_symbols`` ).

* The callgraph ( ``.gnu.lto_.cgraph`` )

  This section contains the basic data structure used by the GCC
  inter-procedural optimization infrastructure.  This section stores an
  annotated multi-graph which represents the functions and call sites as
  well as the variables, aliases and top-level ``asm`` statements.

  This section is emitted in
  lto-streamer-out.c: ``output_cgraph`` and read in
  lto-cgraph.c: ``input_cgraph``.

* IPA references ( ``.gnu.lto_.refs`` )

  This section contains references between function and static
  variables.  It is emitted by lto-cgraph.c: ``output_refs``
  and read by lto-cgraph.c: ``input_refs``.

* Function bodies ( ``.gnu.lto_.function_body.<name>`` )

  This section contains function bodies in the intermediate language
  representation.  Every function body is in a separate section to allow
  copying of the section independently to different object files or
  reading the function on demand.

  Functions are emitted in
  lto-streamer-out.c: ``output_function`` and read in
  lto-streamer-in.c: ``input_function``.

* Static variable initializers ( ``.gnu.lto_.vars`` )

  This section contains all the symbols in the global variable pool.  It
  is emitted by lto-cgraph.c: ``output_varpool`` and read in
  lto-cgraph.c: ``input_cgraph``.

* Summaries and optimization summaries used by IPA passes
  ( ``.gnu.lto_.<xxx>``, where ``<xxx>`` is one of ``jmpfuncs``,
  ``pureconst`` or ``reference`` )

  These sections are used by IPA passes that need to emit summary
  information during LTO generation to be read and aggregated at
  link time.  Each pass is responsible for implementing two pass manager
  hooks: one for writing the summary and another for reading it in.  The
  format of these sections is entirely up to each individual pass.  The
  only requirement is that the writer and reader hooks agree on the
  format.
