..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _downloading-the-source:

Downloading GCC
---------------

.. index:: Downloading GCC

.. index:: Downloading the Source

GCC is distributed via `git <http://gcc.gnu.org/git.html>`_ and via
HTTPS as tarballs compressed with :command:`gzip` or :command:`bzip2`.

Please refer to the `releases web page <http://gcc.gnu.org/releases.html>`_
for information on how to obtain GCC.

The source distribution includes the C, C++, Objective-C, Fortran,
and Ada (in the case of GCC 3.1 and later) compilers, as well as
runtime libraries for C++, Objective-C, and Fortran.
For previous versions these were downloadable as separate components such
as the core GCC distribution, which included the C language front end and
shared components, and language-specific distributions including the
language front end and the language runtime (where appropriate).

If you also intend to build binutils (either to upgrade an existing
installation or for use in place of the corresponding tools of your
OS), unpack the binutils distribution either in the same directory or
a separate one.  In the latter case, add symbolic links to any
components of the binutils you intend to build alongside the compiler
(bfd, binutils, gas, gprof, ld,
opcodes, ...) to the directory containing the GCC sources.

Likewise the GMP, MPFR and MPC libraries can be automatically built
together with GCC.  You may simply run the
:command:`contrib/download_prerequisites` script in the GCC source directory
to set up everything.
Otherwise unpack the GMP, MPFR and/or MPC source
distributions in the directory containing the GCC sources and rename
their directories to gmp, mpfr and mpc,
respectively (or use symbolic links with the same name).

.. ***Configuration***********************************************************

