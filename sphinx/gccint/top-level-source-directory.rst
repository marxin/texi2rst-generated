.. _top-level:

Top Level Source Directory
**************************

The top level source directory in a GCC distribution contains several
files and directories that are shared with other software
distributions such as that of GNU Binutils.  It also contains several
subdirectories that contain parts of GCC and its runtime libraries:

boehm-gc
  The Boehm conservative garbage collector, optionally used as part of
  the ObjC runtime library when configured with :option:`--enable-objc-gc`.

config
  Autoconf macros and Makefile fragments used throughout the tree.

contrib
  Contributed scripts that may be found useful in conjunction with GCC.
  One of these, contrib/texi2pod.pl, is used to generate man
  pages from Texinfo manuals as part of the GCC build process.

fixincludes
  The support for fixing system headers to work with GCC.  See
  fixincludes/README for more information.  The headers fixed by
  this mechanism are installed in :samp:`{libsubdir}` /include-fixed.
  Along with those headers, README-fixinc is also installed, as
  :samp:`{libsubdir}` /include-fixed/README.

gcc
  The main sources of GCC itself (except for runtime libraries),
  including optimizers, support for different target architectures,
  language front ends, and testsuites.  See :ref:`gcc-directory`, for details.

gnattools
  Support tools for GNAT.

include
  Headers for the ``libiberty`` library.

intl
  GNU ``libintl``, from GNU ``gettext``, for systems which do not
  include it in ``libc``.

libada
  The Ada runtime library.

libatomic
  The runtime support library for atomic operations (e.g. for ``__sync``
  and ``__atomic`` ).

libcpp
  The C preprocessor library.

libdecnumber
  The Decimal Float support library.

libffi
  The ``libffi`` library, used as part of the Go runtime library.

libgcc
  The GCC runtime library.

libgfortran
  The Fortran runtime library.

libgo
  The Go runtime library.  The bulk of this library is mirrored from the
  `master Go repository <https://github.com//golang/go>`_.

libgomp
  The GNU Offloading and Multi Processing Runtime Library.

libiberty
  The ``libiberty`` library, used for portability and for some
  generally useful data structures and algorithms.  See :ref:`Introduction <top>`, for more information
  about this library.

libitm
  The runtime support library for transactional memory.

libobjc
  The Objective-C and Objective-C++ runtime library.

libquadmath
  The runtime support library for quad-precision math operations.

libphobos
  The D standard and runtime library.  The bulk of this library is mirrored
  from the `master D repositories <https://github.com//dlang>`_.

libssp
  The Stack protector runtime library.

libstdc++-v3
  The C++ runtime library.

lto-plugin
  Plugin used by the linker if link-time optimizations are enabled.

maintainer-scripts
  Scripts used by the ``gccadmin`` account on ``gcc.gnu.org``.

zlib
  The ``zlib`` compression library, used for compressing and
  uncompressing GCC's intermediate language in LTO object files.

  The build system in the top level directory, including how recursion
into subdirectories works and how building runtime libraries for
multilibs is handled, is documented in a separate manual, included
with GNU Binutils.  See :ref:`GNU configure and build system <top>`, for details.

