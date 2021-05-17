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

  openmp-runtime-library-routines
  openmp-environment-variables

.. man begin DESCRIPTION

.. man begin DESCRIPTION

.. _library-index:

Library Index
=============

