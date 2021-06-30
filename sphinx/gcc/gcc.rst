..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. %**start of header

.. INTERNALS is used by md.texi to determine whether to include the
   whole of that file, in the internals manual, or only the part
   dealing with constraints, in the user manual.

.. NOTE: checks/things to do:
    c
   -have bob do a search in all seven files for "mew" (ideally -mew,
    but i may have forgotten the occasional "-"..).
       Just checked... all have `-'!  Bob 22Jul96
       Use this to search:   grep -n '\-\-mew' *.texi
   -item/itemx, text after all (sub/sub)section titles, etc..
   -consider putting the lists of options on pp 17-> etc in columns or
    some such.
   -overfulls.  do a search for "mew" in the files, and you will see
     overfulls that i noted but could not deal with.
   -have to add text:  beginning of chapter 8
    c
   anything else?                       -mew 10feb93

.. Create a separate index for command line options.

.. Merge the standard indexes into a single one.

.. %**end of header

.. _top:

Introduction
============

.. index:: introduction

This manual documents how to use the GNU compilers,
as well as their features and incompatibilities, and how to report
bugs.  It corresponds to the compilers
|package_version|
version |gcc_version|.
The internals of the GNU compilers, including how to port them to new
targets and some information about how to write front ends for new
languages, are documented in a separate manual.  See :ref:`Introduction <top>`.

.. toctree::
  :maxdepth: 2

  g++-and-gcc
  standards
  invoking-gcc
  c-implementation
  c++-implementation
  c-extensions
  c++-extensions
  objective-c
  compatibility
  gcov
  gcov-tool
  gcov-dump
  lto-dump
  trouble
  bugs
  service
  contributing

  funding
  gnu-project

  general-public-license-3
  gnu-free-documentation-license
  contributors

  option-index
  keyword-index

.. toctree::
  :maxdepth: 2

  programming-languages-supported-by-gcc
  language-standards-supported-by-gcc
  gcc-command-options
  c-implementation-defined-behavior
  c++-implementation-defined-behavior
  extensions-to-the-c-language-family
  extensions-to-the-c++-language
  gnu-objective-c-features
  binary-compatibility
  known-causes-of-trouble-with-gcc
  reporting-bugs
  how-to-get-help-with-gcc
  contributing-to-gcc-development
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

.. _keyword-index:

.. -
   Epilogue
   -

