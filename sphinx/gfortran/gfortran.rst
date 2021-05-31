.. %**start of header

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

.. toctree::
  :maxdepth: 2

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
  general-public-license-3
  gnu-free-documentation-license
  funding
  option-index
  keyword-index

.. -
   Introduction
   -

.. toctree::
  :maxdepth: 2

  gnu-fortran-command-options
  runtime-influencing-runtime-behavior-with-environment-variables

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
  :maxdepth: 2

  contributors
  projects
  proposed-extensions
  contributors-to-gnu-fortran

.. _option-index:

Option Index
============

:command:`gfortran`'s command line options are indexed here without any
initial :samp:`-` or :samp:`--`.  Where an option has both positive and
negative forms (such as -foption and -fno-option), relevant entries in
the manual are indexed under the most appropriate form; it may sometimes
be useful to look up both forms.

.. _keyword-index:
