.. %**start of header

.. INTERNALS is used by md.texi to determine whether to include the
   whole of that file, in the internals manual, or only the part
   dealing with constraints, in the user manual.

.. See miscellaneous notes in gcc.texi on checks/things to do.
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

.. Create a separate index for command line options.

.. Merge the standard indexes into a single one.

.. %**end of header

Copyright (C) 1988-2021 Free Software Foundation, Inc.

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

This file documents the internals of the GNU compilers.

.. _top:

Introduction
============

.. index:: introduction

This manual documents the internals of the GNU compilers, including
how to port them to new targets and some information about how to
write front ends for new languages.  It corresponds to the compilers
|package_version|
version |gcc_version|.  The use of the GNU compilers is documented in a
separate manual.  See :ref:`Introduction <top>`.

This manual is mainly a reference manual rather than a tutorial.  It
discusses how to contribute to GCC (see :ref:`contributing`), the
characteristics of the machines supported by GCC as hosts and targets
(see :ref:`portability`), how GCC relates to the ABIs on such systems
(see :ref:`interface`), and the characteristics of the languages for
which GCC front ends are written (see :ref:`languages`).  It then
describes the GCC source tree structure and build system, some of the
interfaces to GCC front ends, and how support for a target system is
implemented in GCC.

Additional tutorial information is linked to from
http://gcc.gnu.org/readings.html.

.. toctree::

  contributing
  portability
  interface
  libgcc
  languages
  source-tree
  testsuites
  options
  passes
  poly_int
  generic
  gimple
  tree-ssa
  rtl
  control-flow
  loop-analysis-and-representation
  machine-desc
  target-macros
  host-config
  fragments
  collect2
  header-dirs
  type-information
  plugins
  lto

  match-and-simplify
  static-analyzer
  user-experience-guidelines
  funding
  gnu-project

  gpl-3.0
  gnu_free_documentation_license
  contributors

  option-index
  concept-index

.. Copyright (C) 1988-2021 Free Software Foundation, Inc.
   This is part of the GCC manual.
   For copying conditions, see the file gcc.texi.

.. toctree::

  contributing-to-gcc-development
  gcc-and-portability
  interfacing-to-gcc-output
  the-gcc-low-level-runtime-library
  language-front-ends-in-gcc
  source-tree-structure-and-build-system
  option-specification-files
  passes-and-files-of-the-compiler
  sizes-and-offsets-as-runtime-invariants
  analysis-and-optimization-of-gimple-tuples
  rtl-representation
  control-flow-graph
  analysis-and-representation-of-loops
  machine-descriptions
  target-description-macros-and-functions
  host-configuration
  makefile-fragments
  standard-header-file-directories
  memory-management-and-type-information
  link-time-optimization
  contributors-to-gcc


.. man begin DESCRIPTION

.. _gnu-project:

The GNU Project and GNU/Linux
=============================

The GNU Project was launched in 1984 to develop a complete Unix-like
operating system which is free software: the GNU system.  (GNU is a
recursive acronym for 'GNU's Not Unix'; it is pronounced
'guh-NEW'.)  Variants of the GNU operating system, which use the
kernel Linux, are now widely used; though these systems are often
referred to as 'Linux', they are more accurately called GNU/Linux
systems.

For more information, see:

.. code-block:: c++

  http://www.gnu.org/
  http://www.gnu.org/gnu/linux-and-gnu.html

.. man begin DESCRIPTION

.. _option-index:

Option Index
============

GCC's command line options are indexed here without any initial :samp:`-`
or :samp:`--`.  Where an option has both positive and negative forms
(such as :option:`-f`:samp:`{option}` and :option:`-fno-`:samp:`{option}` ),
relevant entries in the manual are indexed under the most appropriate
form; it may sometimes be useful to look up both forms.

.. _concept-index:

.. -
   Epilogue
   -

