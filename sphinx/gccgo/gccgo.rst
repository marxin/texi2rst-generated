.. Merge the standard indexes into a single one.

.. Copyright (C) 2001-2021 Free Software Foundation, Inc.
   This is part of the GCC manual.
   For copying conditions, see the file gcc.texi.
   Version number and development mode.
   version-GCC is @set to the base GCC version number.
   DEVELOPMENT is @set for an in-development version, @clear for a
   release version (corresponding to ``experimental''/anything else
   in gcc/DEV-PHASE).

.. Common macros to support generating man pages:

.. Makeinfo handles the above macro OK, TeX needs manual line breaks;
   they get lost at some point in handling the macro.  But if @macro is
   used here rather than @alias, it produces double line breaks.

.. For FSF printing, define FSFPRINT.  Also update the ISBN and last
   printing date for the manual being printed.
   @set FSFPRINT
   Macro to generate a "For the N.N.N version" subtitle on the title
   page of TeX documentation.  This macro should be used in the
   titlepage environment after the title and any other subtitles have
   been placed, and before any authors are placed.

.. Copyright years for this manual.

.. _top:

Introduction
============

This manual describes how to use :command:`gccgo`, the GNU compiler for
the Go programming language.  This manual is specifically about
:command:`gccgo`.  For more information about the Go programming
language in general, including language specifications and standard
package documentation, see http://golang.org/.

.. toctree::

  gpl-3.0
  gnu_free_documentation_license
  invoking-gccgo
  import-and-export
  compiler-directives
  c-interoperability

