.. %**start of header

.. %**end of header

Copyright (C) 2006-2021 Free Software Foundation, Inc.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with the
Invariant Sections being 'Funding Free Software', the Front-Cover
texts being (a) (see below), and with the Back-Cover Texts being (b)
(see below).  A copy of the license is included in the section entitled
'GNU Free Documentation License'.

(a) The FSF's Front-Cover Text is:

A GNU Manual

(b) The FSF's Back-Cover Text is:

You have freedom to copy and modify this GNU Manual, like GNU
     software.  Copies published by the Free Software Foundation raise
     funds for GNU development.

.. _top:

Introduction
============

.. index:: Introduction

This manual documents the usage of libgomp, the GNU Offloading and
Multi Processing Runtime Library.  This includes the GNU
implementation of the `OpenMP <https://www.openmp.org>`_ Application
Programming Interface (API) for multi-platform shared-memory parallel
programming in C/C++ and Fortran, and the GNU implementation of the
`OpenACC <https://www.openacc.org>`_ Application Programming
Interface (API) for offloading of code to accelerator devices in C/C++
and Fortran.

Originally, libgomp implemented the GNU OpenMP Runtime Library.  Based
on this, support for OpenACC and offloading (both OpenACC and OpenMP
4's target construct) has been added later on, and the library's name
changed to GNU Offloading and Multi Processing Runtime Library.

..  comment
    comment  When you add a new menu item, please keep the right hand
    comment  aligned to the same column.  Do not use tabs.  This provides
    comment  better formatting.
    comment

.. toctree::

  enabling-openmp
  runtime-library-routines
  environment-variables
  enabling-openacc
  openacc-runtime-library-routines
  openacc-environment-variables
  cuda-streams-usage
  openacc-library-interoperability
  openacc-profiling-interface
  the-libgomp-abi
  reporting-bugs
  gpl-3.0
  gnu_free_documentation_license
  funding
  library-index

.. -
   Enabling OpenMP
   -

.. toctree::

  enabling-openmp
  openmp-runtime-library-routines
  openmp-environment-variables
  enabling-openacc
  openacc-runtime-library-routines
  openacc-environment-variables
  cuda-streams-usage
  openacc-library-interoperability
  openacc-profiling-interface
  the-libgomp-abi
  reporting-bugs

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
   Index
   -

.. _library-index:

Library Index
=============

