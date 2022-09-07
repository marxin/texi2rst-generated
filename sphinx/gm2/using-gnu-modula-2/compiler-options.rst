.. _compiler-options:

Compiler options
****************

This section describes the compiler options specific to GNU Modula-2
for generic flags details See :ref:`gcc:invoking-gcc`.

For any given input file, the file name suffix determines what kind of
compilation is done.  The following kinds of input file names are supported:

:samp:`{file}.mod`
  Modula-2 implementation or program source files.  See the
  :samp:`-fmod=` option if you wish to compile a project which uses a
  different source file extension.

:samp:`{file}.def`
  Modula-2 definition module source files.  Definition modules are not
  compiled separately, in GNU Modula-2 definition modules are parsed as
  required when program or implementation modules are compiled.  See the
  :samp:`-fdef=` option if you wish to compile a project which uses a
  different source file extension.

  You can specify more than one input file on the :command:`gm2` command line,

``-g``
  create debugging information so that debuggers such as :samp:`gdb`
  can inspect and control executables.

``-I``
  used to specify the search path for definition and implementation
  modules.  An example is:  ``gm2 -g -c -I.:../../libs foo.mod``.
  If this option is not specified then the default path is added
  which consists of the current directory followed by the appropriate
  language dialect library directories.

``-fdebug-builtins``
  call a real function, rather than the builtin equivalent.  This can
  be useful for debugging parameter values to a builtin function as
  it allows users to single step code into a real function.

``-fdump-system-exports``
  display all inbuilt system items.
  This is an internal command line option.

``-fswig``
  generate a swig interface file.

``-fshared``
  generate a shared library from the module.

``-fruntime-modules=``
  specify, using a comma separated list, the runtime modules and their
  order.  These modules will initialized first before any other modules
  in the application dependency.  By default the runtime modules list is
  set to ``Storage,SYSTEM,M2RTS,RTExceptions,IOLink``.  Note that
  these modules will only be linked into your executable if they are
  required.  So adding a long list of dependant modules will not effect
  the size of the executable it merely states the initialization order
  should they be required.

``-fnil``
  generate code to detect accessing data through a
  ``NIL`` value pointer.

``-fno-nil``
  do not generate code to detect accessing data through a
  ``NIL`` value pointer.

``-fwholediv``
  generate code to detect whole number division by zero or modulus by zero.

``-fno-wholediv``
  do not generate code to detect whole number division by zero or
  modulus by zero.

  .. @item -fwholevalue
     generate code to detect whole number overflow and underflow.
     @item -fno-wholevalue
     do not generate code to detect whole number overflow and underflow.
     @item -frealdiv
     generate code to detect real number division by zero.
     @item -fno-realdiv
     do not generate code to detect real number division by zero.
     @item -frealvalue
     generate code to detect @code{NaN}s real number overflow and underflow.
     @item -fno-realvalue
     do not generate code to detect @code{NaN}s real number overflow and underflow.

``-findex``
  generate code to check whether array index values are out of bounds.

``-fno-index``
  do not generate code to check whether array index values are out of
  bounds.

``-frange``
  generate code to check the assignment range, return value range
  set range and constructor range.

``-fno-range``
  do not generate code to check the assignment range, return value range
  set range and constructor range.

``-freturn``
  generate code to check that functions always exit with a ``RETURN``
  and do not fall out at the end.

``-fcase``
  turns on compile time checking to check whether a ``CASE``
  statement requires an ``ELSE`` clause when on was not specified.

``-fsoft-check-all``
  turns on all runtime checks.  This is the same as invoking
  GNU Modula-2 using the command options
  ``-fnil`` ``-frange`` ``-findex``

  .. @code{-fwholevalue}  -fixme- add this when working

  ``-fwholediv`` ``-fcase`` ``-freturn``.

``-fauto-init``
  turns on auto initialization of pointers to NIL.  Whenever a block is
  created all pointers declarated within this scope will have their
  addresses assigned to NIL.

``-fno-exceptions``
  turns off all generation of exception handling code and no references
  are made to the runtime exception libraries.

``-v``
  display all calls to subsidiary programs, such as the C preprocessor,
  the GNU Modula-2 linker and compiler.

``-fm2-statistics``
  generates quadruple information: number of quadruples generated,
  number of quadruples remaining after optimisation and number of source
  lines compiled.

``-fm2-whole-program``
  compile all implementation modules and program module at once.  Notice
  that you need to take care if you are compiling different dialect
  modules (particularly with the negative operands to modulus).  But
  this option, when coupled together with ``-O3``, can deliver huge
  performance improvements.

``-fm2-g``
  improve the debugging experience for new programmers at the expense
  of generating ``nop`` instructions if necessary to ensure single
  stepping precision over all code related keywords.  An example
  of this is in termination of a list of nested ``IF`` statements
  where multiple ``END`` keywords are mapped onto a sequence of
  ``nop`` instructions.

``-fm2-lower-case``
  render keywords in error messages using lower case.

``fno-pthread``
  do not automatically link against the pthread library.  This option is
  likely useful if gm2 is configured as a cross compiler targetting
  embedded systems.  By default GNU Modula-2 uses the GCC pthread
  libraries to implement coroutines (see the SYSTEM implementation
  module).

:samp:`-fuse-list={filename}`
  if :samp:`-fscaffold-static` is enabled then use the file
  :samp:`filename` for the initialization order of modules.  Whereas if
  :samp:`-fscaffold-dynamic` is enabled then use this file to force
  linking of all module ctors.
  This option cannot be used if :samp:`-fgen-module-list=` is enabled.

:samp:`-fgen-module-list={filename}`
  attempt to find all modules when linking and generate a module list.
  If the :samp:`filename` is :samp:`-` then the contents are not written
  and only used to force the linking of all module ctors.
  This option cannot be used if :samp:`-fuse-list=` is enabled.

``-fscaffold-static``
  the option ensures that :samp:`gm2` will generate a static scaffold
  within the program module.  The static scaffold is useful for
  debugging and single stepping the initialization blocks of
  implementation modules.

``-fscaffold-dynamic``
  the option ensures that :samp:`gm2` will generate a dynamic scaffold
  infastructure when compiling implementation and program modules.
  By default this option is on.  Use :samp:`-fno-scaffold-dynamic`
  to turn it off or select :samp:`-fno-scaffold-dynamic`.

``-fcpp``
  preprocess the source with :samp:`cpp -lang-asm -traditional-cpp`
  For further details about these options See :ref:`cpp:invocation`.
  If :samp:`-fcpp` is supplied then all definition modules and
  implementation modules which are parsed will be preprocessed by
  :samp:`cpp`.

``-fiso``
  turn on ISO standard features. Currently this enables the ISO
  ``SYSTEM`` module and alters the default library search path so
  that the ISO libraries are searched before the PIM libraries.  It also
  effects the behaviour of ``DIV`` and ``MOD`` operators.
  See :ref:`dialect`.

``-fpim``
  turn on PIM standard features. Currently this enables the PIM
  ``SYSTEM`` module and determines which identifiers are pervasive
  (declared in the base module). If no other :samp:`-fpim[234]` switch is
  used then division and modulus operators behave as defined in PIM4.
  See :ref:`dialect`.

``-fpim2``
  turn on PIM-2 standard features. Currently this removes ``SIZE``
  from being a pervasive identifier (declared in the base module).  It
  places ``SIZE`` in the ``SYSTEM`` module.  It also effects the
  behaviour of ``DIV`` and ``MOD`` operators.
  See :ref:`dialect`.

``-fpim3``
  turn on PIM-3 standard features. Currently this only effects the
  behaviour of ``DIV`` and ``MOD`` operators.
  See :ref:`dialect`.

``-fpim4``
  turn on PIM-4 standard features. Currently this only effects the
  behaviour of ``DIV`` and ``MOD`` operators.
  See :ref:`dialect`.

``-fpositive-mod-floor-div``
  forces the ``DIV`` and ``MOD`` operators to behave as defined by PIM4.
  All modulus results are positive and the results from the division are
  rounded to the floor.
  See :ref:`dialect`.

``-flibs=``
  modifies the default library search path.  The libraries supplied are:
  m2pim, m2iso, m2min, m2log and m2cor.  These map onto the
  Programming in Modula-2 base libraries, ISO standard libraries, minimal
  library support, Logitech compatible library and Programming in
  Modula-2 with coroutines.
  Multiple libraries can be specified and are comma separated with precidence
  going to the first in the list.  It is not necessary to use -flibs=m2pim or
  -flibs=m2iso if you also specify -fpim, -fpim2, -fpim3, -fpim4 or
  -fiso.  Unless you are using -flibs=m2min you should include m2pim as
  the they provide the base modules which all other dialects utilize.
  The option :samp:`-fno-libs=-` disables the :samp:`gm2` driver from
  modifying the search and library paths.

``-fextended-opaque``
  allows opaque types to be implemented as any type. This is a GNU
  Modula-2 extension and it requires that the implementation module
  defining the opaque type is available so that it can be resolved when
  compiling the module which imports the opaque type.

``-fsources``
  displays the path to the source of each module.  This option
  can be used at compile time to check the correct definition module
  is being used.

``-fdef=``
  recognise the specified suffix as a definition module filename.
  The default implmentation and module filename suffix is :samp:`.def`.
  If this option is used GNU Modula-2 will still fall back to this
  default if a requested definition module is not found.

``-fmod=``
  recognise the specified suffix as implementation and module filenames.
  The default implmentation and module filename suffix is :samp:`.mod`.
  If this option is used GNU Modula-2 will still fall back to this
  default if it needs to read an implmentation module and the specified
  suffixed filename does not exist.

``-fxcode``
  issues all errors and warnings in the :samp:`Xcode` format.

``-funbounded-by-reference``
  enable optimization of unbounded parameters by attempting to pass non
  ``VAR`` unbounded parameters by reference.  This optimization
  avoids the implicit copy inside the callee procedure. GNU Modula-2
  will only allow unbounded parameters to be passed by reference if,
  inside the callee procedure, they are not written to, no address is
  calculated on the array and it is not passed as a ``VAR``
  parameter.  Note that it is possible to write code to break this
  optimization, therefore this option should be used carefully.
  For example it would be possible to take the address of an array, pass
  the address and the array to a procedure, read from the array in
  the procedure and write to the location using the address parameter.

  Due to the dangerous nature of this option it is not enabled
  when the -O option is specified.

``-Wverbose-unbounded``
  inform the user which non ``VAR`` unbounded parameters will be
  passed by reference.  This only produces output if the option
  :samp:`-funbounded-by-reference` is also supplied on the command line.

``-Wstyle``
  checks for poor programming style.  This option is aimed at new users of
  Modula-2 in that it checks for situations which might cause confusion
  and thus mistakes.  It checks whether variables of the same name are
  declared in different scopes and whether variables look like keywords.
  Experienced users might find this option too aggressive.

``-Wpedantic``
  forces the compiler to reject nested ``WITH`` statements
  referencing the same record type.  Does not allow multiple imports of
  the same item from a module.  It also checks that: procedure variables
  are written to before being read; variables are not only written to
  but read from; variables are declared and used.  If the compiler
  encounters a variable being read before written it will terminate with
  a message.  It will check that ``FOR`` loop indices are not used
  outside the end of this loop without being reset.

``-Wpedantic-param-names``
  procedure parameter names are checked in the definition module
  against their implementation module counterpart.  This is not
  necessary in ISO or PIM versions of Modula-2.

``-Wpedantic-cast``
  warns if the ISO system function is used and if the size of
  the variable is different from that of the type.  This is legal
  in ISO Modula-2, however it can be dangerous.  Some users may prefer
  to use ``VAL`` instead in these situations and use ``CAST``
  exclusively for changes in type on objects which have the same size.

``-Wunused-variable``
  warns if a variable has been declared and it not used.

``-Wunused-parameter``
  warns if a parameter has been declared and it not used.

``-Wall``
  turn on all Modula-2 warnings.

