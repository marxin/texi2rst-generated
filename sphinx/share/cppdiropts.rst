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

