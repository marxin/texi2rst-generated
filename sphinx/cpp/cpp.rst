.. @smallbook
   @cropmarks
   @finalout
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

.. man begin COPYRIGHT

.. man begin COPYRIGHT

.. Create a separate index for command line options.

.. Used in cppopts.texi and cppenv.texi.

.. _top:

The C preprocessor implements the macro language used to transform C,
C++, and Objective-C programs before they are compiled.  It can also be
useful on its own.

.. toctree::

  overview
  header-files
  macros
  conditionals
  diagnostics
  line-control
  pragmas
  other-directives
  preprocessor-output
  traditional-mode
  implementation-details
  invocation
  environment-variables
  gnu_free_documentation_license
  index-of-directives
  option-index
  concept-index

.. _index-of-directives:
.. _option-index:

Option Index
============

CPP's command-line options and environment variables are indexed here
without any initial :samp:`-` or :samp:`--`.

.. _concept-index:
