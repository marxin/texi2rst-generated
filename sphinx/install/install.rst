.. @ifnothtml
   %**start of header

.. %**end of header
   @end ifnothtml
   Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

.. Specify title for specific html page
   Copyright (C) 1988-2021 Free Software Foundation, Inc.
   *** Converted to texinfo by Dean Wakerley, dean@wakerley.com
   IMPORTANT: whenever you modify this file, run `install.texi2html' to
   test the generation of HTML documents for the gcc.gnu.org web pages.
    c
   Do not use @footnote{} in this file as it breaks install.texi2html!
   Include everything if we're not making html

.. Part 2 Summary Description and Copyright

Introduction
============

.. Part 3 Titlepage and Copyright

.. Part 4 Top node, Master Menu, and/or Table of Contents
   Part 5 The Body of the Document
   ***Installing GCC**********************************************************

.. toctree::

  installing-gcc
  prerequisites
  downloading-gcc
  configuration
  building
  testing
  final-installation
  binaries
  host-target-specific-installation-notes-for-gcc
  old-installation-documentation

.. _old:
.. _gnu-free-documentation-license:
