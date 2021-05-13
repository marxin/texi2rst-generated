.. _invoking-gnu-fortran:

GNU Fortran Command Options
---------------------------

.. index:: GNU Fortran command options

.. index:: command options

.. index:: options, gfortran command

.. man begin DESCRIPTION

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

.. man end

.. toctree::

  Brief list of all gfortran options,
                          without explanations. <option-summary>
  Controlling the variant of Fortran language
                               compiled. <fortran-dialect-options>
  Enable and customize preprocessing. <preprocessing-options>
  How picky should the compiler be? <error-and-warning-options>
  Symbol tables, measurements, and debugging dumps. <debugging-options>
  Where to find module files <directory-options>
  Influencing the linking step <link-options->
  Influencing runtime behavior <runtime-options>
  Specifying conventions for function calls, data layout
                          and register usage. <code-gen-options>
  Options for interoperability with other
                                languages. <interoperability-options>
  Environment variables that affect gfortran. <environment-variables>

.. toctree::

  option-summary
  options-controlling-fortran-dialect
  enable-and-customize-preprocessing
  options-to-request-or-suppress-errors-and-warnings
  options-for-debugging-your-program-or-gnu-fortran
  options-for-directory-search
  influencing-the-linking-step
  influencing-runtime-behavior
  options-for-code-generation-conventions
  options-for-interoperability-with-other-languages
  environment-variables-affecting-gfortran

