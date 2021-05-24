.. _standards:

Standards
*********

.. index:: Standards

.. toctree::

  varying-length-character-strings

The GNU Fortran compiler implements
ISO/IEC 1539:1997 (Fortran 95).  As such, it can also compile essentially all
standard-compliant Fortran 90 and Fortran 77 programs.   It also supports
the ISO/IEC TR-15581 enhancements to allocatable arrays.

GNU Fortran also have a partial support for ISO/IEC 1539-1:2004
(Fortran 2003), ISO/IEC 1539-1:2010 (Fortran 2008), the Technical
Specification ``Further Interoperability of Fortran with C``
(ISO/IEC TS 29113:2012).  Full support of those standards and future
Fortran standards is planned.  The current status of the support is
can be found in the Fortran 2003 status, Fortran 2008
status and Fortran 2018 status sections of the documentation.

Additionally, the GNU Fortran compilers supports the OpenMP specification
(version 4.5 and partial support of the features of the 5.0 version,
http://openmp.org//openmp-specifications/).
There also is support for the OpenACC specification (targeting
version 2.6, http://www.openacc.org/).  See
https://gcc.gnu.org/wiki/OpenACC for more information.

.. _varying-length-character-strings:

Varying Length Character Strings
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Varying length character strings

.. index:: Varying length strings

.. index:: strings, varying length

The Fortran 95 standard specifies in Part 2 (ISO/IEC 1539-2:2000)
varying length character strings.  While GNU Fortran currently does not
support such strings directly, there exist two Fortran implementations
for them, which work with GNU Fortran.  They can be found at
http://www.fortran.com//iso_varying_string.f95 and at
ftp://ftp.nag.co.uk//sc22wg5//ISO_VARYING_STRING/.

Deferred-length character strings of Fortran 2003 supports part of
the features of ``ISO_VARYING_STRING`` and should be considered as
replacement. (Namely, allocatable or pointers of the type
``character(len=:)``.)

.. =====================================================================
   PART I: INVOCATION REFERENCE
   =====================================================================

.. -
   Compiler Options
   -
   Copyright (C) 2004-2021 Free Software Foundation, Inc.
   This is part of the GNU Fortran manual.
   For copying conditions, see the file gfortran.texi.

@c man begin COPYRIGHT
Copyright @copyright{} 2004-2021 Free Software Foundation, Inc.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with the
Invariant Sections being ``Funding Free Software'', the Front-Cover
Texts being (a) (see below), and with the Back-Cover Texts being (b)
(see below).  A copy of the license is included in the gfdl(7) man page.

(a) The FSF's Front-Cover Text is:

     A GNU Manual

(b) The FSF's Back-Cover Text is:

     You have freedom to copy and modify this GNU Manual, like GNU
     software.  Copies published by the Free Software Foundation raise
     funds for GNU development.
@c man end
@c Set file name and title for the man page.
@setfilename gfortran
@settitle GNU Fortran compiler.
@c man begin SYNOPSIS
gfortran [@option{-c}|@option{-S}|@option{-E}]
         [@option{-g}] [@option{-pg}] [@option{-O}@var{level}]
         [@option{-W}@var{warn}...] [@option{-pedantic}]
         [@option{-I}@var{dir}...] [@option{-L}@var{dir}...]
         [@option{-D}@var{macro}[=@var{defn}]...] [@option{-U}@var{macro}]
         [@option{-f}@var{option}...]
         [@option{-m}@var{machine-option}...]
         [@option{-o} @var{outfile}] @var{infile}...

Only the most useful options are listed here; see below for the
remainder.
@c man end
@c man begin SEEALSO
gpl(7), gfdl(7), fsf-funding(7),
cpp(1), gcov(1), gcc(1), as(1), ld(1), gdb(1), dbx(1)
and the Info entries for @file{gcc}, @file{cpp}, @file{gfortran}, @file{as},
@file{ld}, @file{binutils} and @file{gdb}.
@c man end
@c man begin BUGS
For instructions on reporting bugs, see
|bugurl|.
@c man end
@c man begin AUTHOR
See the Info entry for @command{gfortran} for contributors to GCC and
GNU Fortran.
@c man end
