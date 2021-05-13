.. _directory-options:

Options for Directory Search
****************************

.. index:: directory options

.. index:: options, directory search

.. index:: search path

These options specify directories to search for header files, for
libraries and for parts of the compiler:

.. Copyright (C) 1999-2021 Free Software Foundation, Inc.
   This is part of the CPP and GCC manuals.
   For copying conditions, see the file gcc.texi.
   -
   Options affecting include directory search in the preprocessor
   -
   If this file is included with the flag ``cppmanual'' set, it is
   formatted for inclusion in the CPP manual; otherwise the main GCC manual.

.. option:: -I dir, -I, -iquote, -isystem, -idirafter

  Add the directory :samp:`{dir}` to the list of directories to be searched
  for header files during preprocessing.
  If :samp:`{dir}` begins with :samp:`=` or ``$SYSROOT``, then the :samp:`=`
  or ``$SYSROOT`` is replaced by the sysroot prefix; see
  :option:`--sysroot` and :option:`-isysroot`.

  Directories specified with :option:`-iquote` apply only to the quote 
  form of the directive, ``#include "file"``.
  Directories specified with :option:`-I`, :option:`-isystem`, 
  or :option:`-idirafter` apply to lookup for both the
  ``#include "file"`` and
  ``#include <file>`` directives.

  You can specify any number or combination of these options on the 
  command line to search for header files in several directories.  
  The lookup order is as follows:

  * For the quote form of the include directive, the directory of the current
    file is searched first.

  * For the quote form of the include directive, the directories specified
    by :option:`-iquote` options are searched in left-to-right order,
    as they appear on the command line.

  * Directories specified with :option:`-I` options are scanned in
    left-to-right order.

  * Directories specified with :option:`-isystem` options are scanned in
    left-to-right order.

  * Standard system directories are scanned.

  * Directories specified with :option:`-idirafter` options are scanned in
    left-to-right order.

  You can use :option:`-I` to override a system header
  file, substituting your own version, since these directories are
  searched before the standard system header file directories.  
  However, you should
  not use this option to add directories that contain vendor-supplied
  system header files; use :option:`-isystem` for that.

  The :option:`-isystem` and :option:`-idirafter` options also mark the directory
  as a system directory, so that it gets the same special treatment that
  is applied to the standard system directories.

  If a standard system include directory, or a directory specified with
  :option:`-isystem`, is also specified with :option:`-I`, the :option:`-I`
  option is ignored.  The directory is still searched but as a
  system directory at its normal position in the system include chain.
  This is to ensure that GCC's procedure to fix buggy system headers and
  the ordering for the ``#include_next`` directive are not inadvertently
  changed.
  If you really need to change the search order for system directories,
  use the :option:`-nostdinc` and/or :option:`-isystem` options.

.. option:: -I-

  Split the include path.
  This option has been deprecated.  Please use :option:`-iquote` instead for
  :option:`-I` directories before the :option:`-I-` and remove the :option:`-I-`
  option.

  Any directories specified with :option:`-I`
  options before :option:`-I-` are searched only for headers requested with
  ``#include "file"`` ; they are not searched for
  ``#include <file>``.  If additional directories are
  specified with :option:`-I` options after the :option:`-I-`, those
  directories are searched for all :samp:`#include` directives.

  In addition, :option:`-I-` inhibits the use of the directory of the current
  file directory as the first search directory for ``#include
  "file"``.  There is no way to override this effect of :option:`-I-`.

.. option:: -iprefix prefix, -iprefix

  Specify :samp:`{prefix}` as the prefix for subsequent :option:`-iwithprefix`
  options.  If the prefix represents a directory, you should include the
  final :samp:`/`.

.. option:: -iwithprefix dir, -iwithprefix, -iwithprefixbefore

  Append :samp:`{dir}` to the prefix specified previously with
  :option:`-iprefix`, and add the resulting directory to the include search
  path.  :option:`-iwithprefixbefore` puts it in the same place :option:`-I`
  would; :option:`-iwithprefix` puts it where :option:`-idirafter` would.

.. option:: -isysroot dir, -isysroot

  This option is like the :option:`--sysroot` option, but applies only to
  header files (except for Darwin targets, where it applies to both header
  files and libraries).  See the :option:`--sysroot` option for more
  information.

.. option:: -imultilib dir, -imultilib

  Use :samp:`{dir}` as a subdirectory of the directory containing
  target-specific C++ headers.

.. option:: -nostdinc

  Do not search the standard system directories for header files.
  Only the directories explicitly specified with :option:`-I`,
  :option:`-iquote`, :option:`-isystem`, and/or :option:`-idirafter`
  options (and the directory of the current file, if appropriate) 
  are searched.

.. option:: -nostdinc++

  Do not search for header files in the C++-specific standard directories,
  but do still search the other standard directories.  (This option is
  used when building the C++ library.)

.. option:: -iplugindir=dir

  Set the directory to search for plugins that are passed
  by :option:`-fplugin`:samp:`={name}` instead of
  :option:`-fplugin`:samp:`={path}` / :samp:`{name}`.so.  This option is not meant
  to be used by the user, but only passed by the driver.

.. option:: -Ldir, -L

  Add directory :samp:`{dir}` to the list of directories to be searched
  for :option:`-l`.

.. option:: -Bprefix, -B

  This option specifies where to find the executables, libraries,
  include files, and data files of the compiler itself.

  The compiler driver program runs one or more of the subprograms
  :command:`cpp`, :command:`cc1`, :command:`as` and :command:`ld`.  It tries
  :samp:`{prefix}` as a prefix for each program it tries to run, both with and
  without :samp:`{machine}/{version}/` for the corresponding target
  machine and compiler version.

  For each subprogram to be run, the compiler driver first tries the
  :option:`-B` prefix, if any.  If that name is not found, or if :option:`-B`
  is not specified, the driver tries two standard prefixes, 
  /usr/lib/gcc/ and /usr/local/lib/gcc/.  If neither of
  those results in a file name that is found, the unmodified program
  name is searched for using the directories specified in your
  :envvar:`PATH` environment variable.

  The compiler checks to see if the path provided by :option:`-B`
  refers to a directory, and if necessary it adds a directory
  separator character at the end of the path.

  :option:`-B` prefixes that effectively specify directory names also apply
  to libraries in the linker, because the compiler translates these
  options into :option:`-L` options for the linker.  They also apply to
  include files in the preprocessor, because the compiler translates these
  options into :option:`-isystem` options for the preprocessor.  In this case,
  the compiler appends :samp:`include` to the prefix.

  The runtime support file libgcc.a can also be searched for using
  the :option:`-B` prefix, if needed.  If it is not found there, the two
  standard prefixes above are tried, and that is all.  The file is left
  out of the link if it is not found by those means.

  Another way to specify a prefix much like the :option:`-B` prefix is to use
  the environment variable :envvar:`GCC_EXEC_PREFIX`.  See :ref:`environment-variables`.

  As a special kludge, if the path provided by :option:`-B` is
  [dir/]stage :samp:`{N}` /, where :samp:`{N}` is a number in the range 0 to
  9, then it is replaced by [dir/]include.  This is to help
  with boot-strapping the compiler.

.. option:: -no-canonical-prefixes

  Do not expand any symbolic links, resolve references to :samp:`/../`
  or :samp:`/./`, or make the path absolute when generating a relative
  prefix.

.. option:: --sysroot=dir

  Use :samp:`{dir}` as the logical root directory for headers and libraries.
  For example, if the compiler normally searches for headers in
  /usr/include and libraries in /usr/lib, it instead
  searches :samp:`{dir}` /usr/include and :samp:`{dir}` /usr/lib.

  If you use both this option and the :option:`-isysroot` option, then
  the :option:`--sysroot` option applies to libraries, but the
  :option:`-isysroot` option applies to header files.

  The GNU linker (beginning with version 2.16) has the necessary support
  for this option.  If your linker does not support this option, the
  header file aspect of :option:`--sysroot` still works, but the
  library aspect does not.

.. option:: --no-sysroot-suffix, -no-sysroot-suffix

  For some targets, a suffix is added to the root directory specified
  with :option:`--sysroot`, depending on the other options used, so that
  headers may for example be found in
  :samp:`{dir}` / :samp:`{suffix}` /usr/include instead of
  :samp:`{dir}` /usr/include.  This option disables the addition of
  such a suffix.

