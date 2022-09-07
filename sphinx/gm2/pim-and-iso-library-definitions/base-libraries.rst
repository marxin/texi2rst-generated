.. _base-libraries:

Base libraries
**************

.. README.texi describes the pim libraries.
    c

These are the base libraries for the GNU Modula-2 compiler.  These
modules originally came from the M2F compiler and have been cleaned up
and extended.  They provide a basic interface to the underlying
operating system via libc.  They also include a number of libraries to
allow access to compiler built-ins. Perhaps the largest difference to
PIM and ISO libraries is the ``DynamicString`` module which
declares the type ``String``.  The heavy use of this opaque data
type results in a number of equivalent modules that can either handle
``ARRAY OF CHAR`` or ``String``.

These modules have been extensively tested and are used throughout
building the GNU Modula-2 compiler.

.. toctree::
  :maxdepth: 2

  base-libraries/gm2-libs-ascii
  base-libraries/gm2-libs-args
  base-libraries/gm2-libs-assertion
  base-libraries/gm2-libs-break
  base-libraries/gm2-libs-builtins
  base-libraries/gm2-libs-coroutines
  base-libraries/gm2-libs-cmdargs
  base-libraries/gm2-libs-debug
  base-libraries/gm2-libs-dynamicstrings
  base-libraries/gm2-libs-environment
  base-libraries/gm2-libs-fio
  base-libraries/gm2-libs-formatstrings
  base-libraries/gm2-libs-fpuio
  base-libraries/gm2-libs-getopt
  base-libraries/gm2-libs-io
  base-libraries/gm2-libs-indexing
  base-libraries/gm2-libs-lmathlib0
  base-libraries/gm2-libs-legacyreal
  base-libraries/gm2-libs-m2dependent
  base-libraries/gm2-libs-m2exception
  base-libraries/gm2-libs-m2link
  base-libraries/gm2-libs-m2rts
  base-libraries/gm2-libs-mathlib0
  base-libraries/gm2-libs-memutils
  base-libraries/gm2-libs-numberio
  base-libraries/gm2-libs-optlib
  base-libraries/gm2-libs-pushbackinput
  base-libraries/gm2-libs-rtexceptions
  base-libraries/gm2-libs-rtint
  base-libraries/gm2-libs-sargs
  base-libraries/gm2-libs-scmdargs
  base-libraries/gm2-libs-senvironment
  base-libraries/gm2-libs-sfio
  base-libraries/gm2-libs-smathlib0
  base-libraries/gm2-libs-system
  base-libraries/gm2-libs-scan
  base-libraries/gm2-libs-selective
  base-libraries/gm2-libs-stdio
  base-libraries/gm2-libs-storage
  base-libraries/gm2-libs-strcase
  base-libraries/gm2-libs-strio
  base-libraries/gm2-libs-strlib
  base-libraries/gm2-libs-stringconvert
  base-libraries/gm2-libs-sysexceptions
  base-libraries/gm2-libs-sysstorage
  base-libraries/gm2-libs-timestring
  base-libraries/gm2-libs-unixargs
  base-libraries/gm2-libs-cbuiltin
  base-libraries/gm2-libs-cgetopt
  base-libraries/gm2-libs-cxxabi
  base-libraries/gm2-libs-dtoa
  base-libraries/gm2-libs-errno
  base-libraries/gm2-libs-gdbif
  base-libraries/gm2-libs-ldtoa
  base-libraries/gm2-libs-libc
  base-libraries/gm2-libs-libm
  base-libraries/gm2-libs-sckt
  base-libraries/gm2-libs-termios
  base-libraries/gm2-libs-wrapc

