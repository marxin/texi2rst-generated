.. %**start of header

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

.. %**end of header
   Use with @@smallbook.
   %** start of document
   Cause even numbered pages to be printed on the left hand side of
   the page and odd numbered pages to be printed on the right hand
   side of the page.  Using this, you can print on both sides of a
   sheet of paper and have the text on the same part of the sheet.
   The text on right hand pages is pushed towards the right hand
   margin and the text on left hand pages is pushed toward the left
   hand margin.
   (To provide the reverse effect, set bindingoffset to -0.75in.)
   @tex
   \global\bindingoffset=0.75in
   \global\normaloffset =0.75in
   @end tex

Copyright (C) 2007-2021 Free Software Foundation, Inc.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with the
Invariant Sections being 'Funding Free Software', the Front-Cover
Texts being (a) (see below), and with the Back-Cover Texts being (b)
(see below).  A copy of the license is included in the section entitled
'GNU Free Documentation License'.

(a) The FSF's Front-Cover Text is:

A GNU Manual

(b) The FSF's Back-Cover Text is:

You have freedom to copy and modify this GNU Manual, like GNU
     software.  Copies published by the Free Software Foundation raise
     funds for GNU development.

.. -
   TexInfo table of contents.
   -

.. _top:

Introduction
============

.. index:: Introduction

This manual documents the internals of :command:`gfortran`, 
the GNU Fortran compiler.

*Warning:* This document, and the compiler it describes, are still
under development.  While efforts are made to keep it up-to-date, it might
not accurately reflect the status of the most recent GNU Fortran compiler.

..  comment
    comment  When you add a new menu item, please keep the right hand
    comment  aligned to the same column.  Do not use tabs.  This provides
    comment  better formatting.
    comment

.. toctree::

  introduction
  user-interface
  frontend-data-structures
  object-orientation
  translating-to-generic
  libgfortran
  gnu_free_documentation_license

.. -
   Introduction
   -

.. toctree::

  code-that-interacts-with-the-user
  internals-of-fortran-2003-oop-features
  generating-the-intermediate-language-for-later-stages
  the-libgfortran-runtime-library

