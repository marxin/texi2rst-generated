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
