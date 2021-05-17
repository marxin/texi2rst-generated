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

.. Create a separate index for command line options

.. Merge the standard indexes into a single one.

.. TODO: The following "Part" definitions are included here temporarily
   until they are incorporated into the official Texinfo distribution.
   They borrow heavily from Texinfo's \unnchapentry definitions.

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

Copyright (C) 1999-2021 Free Software Foundation, Inc.

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

.. TODO: The following "Part" definitions are included here temporarily
   until they are incorporated into the official Texinfo distribution.

.. -
   TexInfo table of contents.
   -

.. _top:

Introduction
============

.. index:: Introduction

This manual documents the use of :command:`gfortran`,
the GNU Fortran compiler.  You can find in this manual how to invoke
:command:`gfortran`, as well as its features and incompatibilities.

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

  Part I: Invoking GNU Fortran
  invoking-gnu-fortran
  runtime

  Part II: Language Reference
  fortran-standards-status
  compiler-characteristics
  extensions
  mixed-language-programming
  coarray-programming
  intrinsic-procedures
  intrinsic-modules

  contributing
  gpl-3.0
  gnu_free_documentation_license
  funding
  option-index
  keyword-index

.. -
   Introduction
   -

.. toctree::

  introduction
  gnu-fortran-command-options
  runtime--influencing-runtime-behavior-with-environment-variables
  fortran-standards-status
  compiler-characteristics
  extensions
  mixed-language-programming
  coarray-programming
  intrinsic-procedures
  intrinsic-modules

.. _contributing:

Contributing
============

.. index:: Contributing

Free software is only possible if people contribute to efforts
to create it.
We're always in need of more people helping out with ideas
and comments, writing documentation and contributing code.

If you want to contribute to GNU Fortran,
have a look at the long lists of projects you can take on.
Some of these projects are small,
some of them are large;
some are completely orthogonal to the rest of what is
happening on GNU Fortran,
but others are 'mainstream' projects in need of enthusiastic hackers.
All of these projects are important!
We will eventually get around to the things here,
but they are also things doable by someone who is willing and able.

.. toctree::

  contributors
  projects
  proposed-extensions
  contributors-to-gnu-fortran

.. man begin DESCRIPTION

.. man begin DESCRIPTION

.. _funding:

Funding Free Software
=====================

If you want to have more free software a few years from now, it makes
sense for you to help encourage people to contribute funds for its
development.  The most effective approach known is to encourage
commercial redistributors to donate.

Users of free software systems can boost the pace of development by
encouraging for-a-fee distributors to donate part of their selling price
to free software developers-the Free Software Foundation, and others.

The way to convince distributors to do this is to demand it and expect
it from them.  So when you compare distributors, judge them partly by
how much they give to free software development.  Show distributors
they must compete to be the one who gives the most.

To make this approach work, you must insist on numbers that you can
compare, such as, 'We will donate ten dollars to the Frobnitz project
for each disk sold.'  Don't be satisfied with a vague promise, such as
'A portion of the profits are donated,' since it doesn't give a basis
for comparison.

Even a precise fraction 'of the profits from this disk' is not very
meaningful, since creative accounting and unrelated business decisions
can greatly alter what fraction of the sales price counts as profit.
If the price you pay is $50, ten percent of the profit is probably
less than a dollar; it might be a few cents, or nothing at all.

Some redistributors do development work themselves.  This is useful too;
but to keep everyone honest, you need to inquire how much they do, and
what kind.  Some kinds of development make much more long-term
difference than others.  For example, maintaining a separate version of
a program contributes very little; maintaining the standard version of a
program for the whole community contributes much.  Easy new ports
contribute little, since someone else would surely do them; difficult
ports such as adding a new CPU to the GNU Compiler Collection contribute more;
major new features or packages contribute the most.

By establishing the idea that supporting further development is 'the
proper thing to do' when distributing free software for a fee, we can
assure a steady flow of resources into making more free software.

.. man end

.. man begin COPYRIGHT

Copyright (C) 1994 Free Software Foundation, Inc.
Verbatim copying and redistribution of this section is permitted
without royalty; alteration is not permitted.

.. man end

.. -
   Indices
   -

.. _option-index:

Option Index
============

:command:`gfortran`'s command line options are indexed here without any
initial :samp:`-` or :samp:`--`.  Where an option has both positive and
negative forms (such as -foption and -fno-option), relevant entries in
the manual are indexed under the most appropriate form; it may sometimes
be useful to look up both forms.

.. _keyword-index:

Keyword Index
=============

