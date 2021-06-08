..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _invoking-gnu-fortran:

GNU Fortran Command Options
---------------------------

.. index:: GNU Fortran command options

.. index:: command options

.. index:: options, gfortran command

Description
^^^^^^^^^^^

The :command:`gfortran` command supports all the options supported by the
:command:`gcc` command.  Only options specific to GNU Fortran are documented
here.

See :ref:`GCC Command Options <invoking-gcc>`, for information
on the non-Fortran-specific aspects of the :command:`gcc` command (and,
therefore, the :command:`gfortran` command).

.. index:: options, negative forms

All GCC and GNU Fortran options
are accepted both by :command:`gfortran` and by :command:`gcc`
(as well as any other drivers built at the same time,
such as :command:`g++`),
since adding GNU Fortran to the GCC distribution
enables acceptance of GNU Fortran options
by all of the relevant drivers.

In some cases, options have positive and negative forms;
the negative form of :samp:`-ffoo` would be :samp:`-fno-foo`.
This manual documents only one of these two forms, whichever
one is not the default.

.. toctree::
  :maxdepth: 2

  gnu-fortran-command-options/option-summary
  gnu-fortran-command-options/options-controlling-fortran-dialect
  gnu-fortran-command-options/enable-and-customize-preprocessing
  gnu-fortran-command-options/options-to-request-or-suppress-errors-and-warnings
  gnu-fortran-command-options/options-for-debugging-your-program-or-gnu-fortran
  gnu-fortran-command-options/options-for-directory-search
  gnu-fortran-command-options/influencing-the-linking-step
  gnu-fortran-command-options/influencing-runtime-behavior
  gnu-fortran-command-options/options-for-code-generation-conventions
  gnu-fortran-command-options/options-for-interoperability-with-other-languages
  gnu-fortran-command-options/environment-variables-affecting-gfortran

  copyright
