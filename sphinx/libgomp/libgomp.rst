.. %**start of header

.. %**end of header

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

.. _library-index:
