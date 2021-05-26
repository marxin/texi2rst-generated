.. %**start of header

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

.. -
   TexInfo table of contents.
   -

.. _top:

Introduction
============

.. index:: Introduction

This manual documents the internals of :command:`gfortran`, 
the GNU Fortran compiler.

.. only:: development

  .. warning::
    This document, and the compiler it describes, are still
    under development.  While efforts are made to keep it up-to-date, it might
    not accurately reflect the status of the most recent GNU Fortran compiler.

.. toctree::

  introduction
  user-interface
  frontend-data-structures
  object-orientation
  translating-to-generic
  libgfortran
  gnu-free-documentation-license

.. -
   Introduction
   -

.. toctree::

  code-that-interacts-with-the-user
  internals-of-fortran-2003-oop-features
  generating-the-intermediate-language-for-later-stages
  the-libgfortran-runtime-library

