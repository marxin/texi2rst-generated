..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. %**start of header

.. INTERNALS is used by md.texi to determine whether to include the
   whole of that file, in the internals manual, or only the part
   dealing with constraints, in the user manual.

.. See miscellaneous notes in gcc.texi on checks/things to do.

.. Create a separate index for command line options.

.. Merge the standard indexes into a single one.

.. %**end of header

.. _top:

.. index:: introduction

Introduction
============

This manual documents the internals of the GNU compilers, including
how to port them to new targets and some information about how to
write front ends for new languages.  It corresponds to the compilers
|package_version|
version |gcc_version|.  The use of the GNU compilers is documented in a
separate manual.  See :ref:`gcc:top`.

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
https://gcc.gnu.org/readings.html.

.. toctree::
  :maxdepth: 2

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

  general-public-license-3
  gnu-free-documentation-license
  contributors

  option-index
  concept-index

.. toctree::
  :maxdepth: 2

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
  host-configuration
  makefile-fragments
  standard-header-file-directories
  memory-management-and-type-information
  link-time-optimization
  contributors-to-gcc

.. include:: ../share/gnu.rst

.. _option-index:

Option Index
============

GCC's command line options are indexed here without any initial :samp:`-`
or :samp:`--`.  Where an option has both positive and negative forms
(such as :option:`-f`:samp:`{option}` and :option:`-fno-`:samp:`{option}`),
relevant entries in the manual are indexed under the most appropriate
form; it may sometimes be useful to look up both forms.

.. _concept-index:

.. -
   Epilogue
   -

