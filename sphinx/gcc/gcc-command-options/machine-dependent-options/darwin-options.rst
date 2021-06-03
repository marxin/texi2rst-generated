.. _darwin-options:

Darwin Options
^^^^^^^^^^^^^^

.. index:: Darwin options

These options are defined for all architectures running the Darwin operating
system.

FSF GCC on Darwin does not create 'fat' object files; it creates
an object file for the single architecture that GCC was built to
target.  Apple's GCC on Darwin does create 'fat' files if multiple
:option:`-arch` options are used; it does so by running the compiler or
linker multiple times and joining the results together with
lipo.

The subtype of the file created (like :samp:`ppc7400` or :samp:`ppc970` or
:samp:`i686`) is determined by the flags that specify the ISA
that GCC is targeting, like :option:`-mcpu` or :option:`-march`.  The
:option:`-force_cpusubtype_ALL` option can be used to override this.

The Darwin tools vary in their behavior when presented with an ISA
mismatch.  The assembler, as, only permits instructions to
be used that are valid for the subtype of the file it is generating,
so you cannot put 64-bit instructions in a :samp:`ppc750` object file.
The linker for shared libraries, /usr/bin/libtool, fails
and prints an error if asked to create a shared library with a less
restrictive subtype than its input files (for instance, trying to put
a :samp:`ppc970` object file in a :samp:`ppc7400` library).  The linker
for executables, :command:`ld`, quietly gives the executable the most
restrictive subtype of any of its input files.

.. option:: -Fdir

  Add the framework directory :samp:`{dir}` to the head of the list of
  directories to be searched for header files.  These directories are
  interleaved with those specified by :option:`-I` options and are
  scanned in a left-to-right order.

  A framework directory is a directory with frameworks in it.  A
  framework is a directory with a Headers and/or
  PrivateHeaders directory contained directly in it that ends
  in .framework.  The name of a framework is the name of this
  directory excluding the .framework.  Headers associated with
  the framework are found in one of those two directories, with
  Headers being searched first.  A subframework is a framework
  directory that is in a framework's Frameworks directory.
  Includes of subframework headers can only appear in a header of a
  framework that contains the subframework, or in a sibling subframework
  header.  Two subframeworks are siblings if they occur in the same
  framework.  A subframework should not have the same name as a
  framework; a warning is issued if this is violated.  Currently a
  subframework cannot have subframeworks; in the future, the mechanism
  may be extended to support this.  The standard frameworks can be found
  in /System/Library/Frameworks and
  /Library/Frameworks.  An example include looks like
  ``#include <Framework/header.h>``, where Framework denotes
  the name of the framework and header.h is found in the
  PrivateHeaders or Headers directory.

.. option:: -iframeworkdir

  Like :option:`-F` except the directory is a treated as a system
  directory.  The main difference between this :option:`-iframework` and
  :option:`-F` is that with :option:`-iframework` the compiler does not
  warn about constructs contained within header files found via
  :samp:`{dir}`.  This option is valid only for the C family of languages.

.. option:: -gused

  Emit debugging information for symbols that are used.  For stabs
  debugging format, this enables :option:`-feliminate-unused-debug-symbols`.
  This is by default ON.

.. option:: -gfull

  Emit debugging information for all symbols and types.

:samp:`-mmacosx-version-min={version}`
  The earliest version of MacOS X that this executable will run on
  is :samp:`{version}`.  Typical values of :samp:`{version}` include ``10.1``,
  ``10.2``, and ``10.3.9``.

  If the compiler was built to use the system's headers by default,
  then the default for this option is the system version on which the
  compiler is running, otherwise the default is to make choices that
  are compatible with as many systems and code bases as possible.

.. option:: -mkernel

  Enable kernel development mode.  The :option:`-mkernel` option sets
  :option:`-static`, :option:`-fno-common`, :option:`-fno-use-cxa-atexit`,
  :option:`-fno-exceptions`, :option:`-fno-non-call-exceptions`,
  :option:`-fapple-kext`, :option:`-fno-weak` and :option:`-fno-rtti` where
  applicable.  This mode also sets :option:`-mno-altivec`,
  :option:`-msoft-float`, :option:`-fno-builtin` and
  :option:`-mlong-branch` for PowerPC targets.

.. option:: -mone-byte-bool

  Override the defaults for ``bool`` so that ``sizeof(bool)==1``.
  By default ``sizeof(bool)`` is ``4`` when compiling for
  Darwin/PowerPC and ``1`` when compiling for Darwin/x86, so this
  option has no effect on x86.

  Warning: The :option:`-mone-byte-bool` switch causes GCC
  to generate code that is not binary compatible with code generated
  without that switch.  Using this switch may require recompiling all
  other modules in a program, including system libraries.  Use this
  switch to conform to a non-default data model.

.. option:: -mfix-and-continue, -ffix-and-continue, -findirect-data

  Generate code suitable for fast turnaround development, such as to
  allow GDB to dynamically load .o files into already-running
  programs.  :option:`-findirect-data` and :option:`-ffix-and-continue`
  are provided for backwards compatibility.

.. option:: -all_load

  Loads all members of static archive libraries.
  See man ld(1) for more information.

.. option:: -arch_errors_fatal

  Cause the errors having to do with files that have the wrong architecture
  to be fatal.

.. option:: -bind_at_load

  Causes the output file to be marked such that the dynamic linker will
  bind all undefined references when the file is loaded or launched.

.. option:: -bundle

  Produce a Mach-o bundle format file.
  See man ld(1) for more information.

.. option:: -bundle_loader executable

  This option specifies the :samp:`{executable}` that will load the build
  output file being linked.  See man ld(1) for more information.

.. option:: -dynamiclib

  When passed this option, GCC produces a dynamic library instead of
  an executable when linking, using the Darwin libtool command.

.. option:: -force_cpusubtype_ALL

  This causes GCC's output file to have the :samp:`ALL` subtype, instead of
  one controlled by the :option:`-mcpu` or :option:`-march` option.

.. option:: -allowable_client  client_name, -client_name, -compatibility_version, -current_version, -dead_strip, -dependency-file, -dylib_file, -dylinker_install_name, -dynamic, -exported_symbols_list, -filelist, -flat_namespace, -force_flat_namespace, -headerpad_max_install_names, -image_base, -init, -install_name, -keep_private_externs, -multi_module, -multiply_defined, -multiply_defined_unused, -noall_load, -no_dead_strip_inits_and_terms, -nofixprebinding, -nomultidefs, -noprebind, -noseglinkedit, -pagezero_size, -prebind, -prebind_all_twolevel_modules, -private_bundle, -read_only_relocs, -sectalign, -sectobjectsymbols, -whyload, -seg1addr, -sectcreate, -sectobjectsymbols, -sectorder, -segaddr, -segs_read_only_addr, -segs_read_write_addr, -seg_addr_table, -seg_addr_table_filename, -seglinkedit, -segprot, -segs_read_only_addr, -segs_read_write_addr, -single_module, -static, -sub_library, -sub_umbrella, -twolevel_namespace, -umbrella, -undefined, -unexported_symbols_list, -weak_reference_mismatches, -whatsloaded

  These options are passed to the Darwin linker.  The Darwin linker man page
  describes them in detail.

