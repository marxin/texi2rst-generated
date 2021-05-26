.. _environment-variables:

Environment Variables
---------------------

.. index:: environment variables

This section describes the environment variables that affect how CPP
operates.  You can use them to specify directories or prefixes to use
when searching for include files, or to control dependency output.

Note that you can also specify places to search using options such as
:option:`-I`, and control dependency output with options like
:option:`-M` (see :ref:`invocation`).  These take precedence over
environment variables, which in turn take precedence over the
configuration of GCC.

.. Copyright (C) 1999-2021 Free Software Foundation, Inc.
   This is part of the CPP and GCC manuals.
   For copying conditions, see the file gcc.texi.
   -
   Environment variables affecting the preprocessor
   -
   If this file is included with the flag ``cppmanual'' set, it is
   formatted for inclusion in the CPP manual; otherwise the main GCC manual.

.. include:: ../share/cppenv.rst


.. Special handling for inclusion in the install manual.

