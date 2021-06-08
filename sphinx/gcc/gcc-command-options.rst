..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _invoking-gcc:

GCC Command Options
-------------------

.. index:: GCC command options

.. index:: command options

.. index:: options, GCC command

Description
^^^^^^^^^^^

When you invoke GCC, it normally does preprocessing, compilation,
assembly and linking.  The 'overall options' allow you to stop this
process at an intermediate stage.  For example, the :option:`-c` option
says not to run the linker.  Then the output consists of object files
output by the assembler.
See :ref:`Options Controlling the Kind of Output <overall-options>`.

Other options are passed on to one or more stages of processing.  Some options
control the preprocessor and others the compiler itself.  Yet other
options control the assembler and linker; most of these are not
documented here, since you rarely need to use any of them.

.. index:: C compilation options

Most of the command-line options that you can use with GCC are useful
for C programs; when an option is only useful with another language
(usually C++), the explanation says so explicitly.  If the description
for a particular option does not mention a source language, you can use
that option with all supported languages.

.. index:: cross compiling

.. index:: specifying machine version

.. index:: specifying compiler version and target machine

.. index:: compiler version, specifying

.. index:: target machine, specifying

The usual way to run GCC is to run the executable called :samp:`gcc`, or
:samp:`{machine}-gcc` when cross-compiling, or
:samp:`{machine}-gcc-{version}` to run a specific version of GCC.
When you compile C++ programs, you should invoke GCC as :samp:`g++` 
instead.  See :ref:`Compiling C++ Programs <invoking-g++>`, 
for information about the differences in behavior between :samp:`gcc` 
and :samp:`g++` when compiling C++ programs.

.. index:: grouping options

.. index:: options, grouping

The :command:`gcc` program accepts options and file names as operands.  Many
options have multi-letter names; therefore multiple single-letter options
may *not* be grouped: :option:`-dv` is very different from :samp:`-d
-v`.

.. index:: order of options

.. index:: options, order

You can mix options and other arguments.  For the most part, the order
you use doesn't matter.  Order does matter when you use several
options of the same kind; for example, if you specify :option:`-L` more
than once, the directories are searched in the order specified.  Also,
the placement of the :option:`-l` option is significant.

Many options have long names starting with :samp:`-f` or with
:samp:`-W`---for example,
:option:`-fmove-loop-invariants`, :option:`-Wformat` and so on.  Most of
these have both positive and negative forms; the negative form of
:samp:`-ffoo` is :samp:`-fno-foo`.  This manual documents
only one of these two forms, whichever one is not the default.

Some options take one or more arguments typically separated either
by a space or by the equals sign (:samp:`=`) from the option name.
Unless documented otherwise, an argument can be either numeric or
a string.  Numeric arguments must typically be small unsigned decimal
or hexadecimal integers.  Hexadecimal arguments must begin with
the :samp:`0x` prefix.  Arguments to options that specify a size
threshold of some sort may be arbitrarily large decimal or hexadecimal
integers followed by a byte size suffix designating a multiple of bytes
such as ``kB`` and ``KiB`` for kilobyte and kibibyte, respectively,
``MB`` and ``MiB`` for megabyte and mebibyte, ``GB`` and
``GiB`` for gigabyte and gigibyte, and so on.  Such arguments are
designated by :samp:`{byte-size}` in the following text.  Refer to the NIST,
IEC, and other relevant national and international standards for the full
listing and explanation of the binary and decimal byte size prefixes.

See :ref:`option-index`, for an index to GCC's options.

.. toctree::
  :maxdepth: 2

  gcc-command-options/option-summary
  gcc-command-options/overall-options
  gcc-command-options/invoking-g++
  gcc-command-options/c-dialect-options
  gcc-command-options/c++-dialect-options
  gcc-command-options/objective-c-and-objective-c++-dialect-options
  gcc-command-options/diagnostic-message-formatting-options
  gcc-command-options/warning-options
  gcc-command-options/static-analyzer-options
  gcc-command-options/debugging-options
  gcc-command-options/optimize-options
  gcc-command-options/instrumentation-options
  gcc-command-options/preprocessor-options
  gcc-command-options/assembler-options
  gcc-command-options/link-options
  gcc-command-options/directory-options
  gcc-command-options/code-gen-options
  gcc-command-options/developer-options
  gcc-command-options/submodel-options
  gcc-command-options/spec-files
  gcc-command-options/environment-variables
  gcc-command-options/precompiled-headers
  gcc-command-options/c++-modules
  gcc-command-options/options-controlling-the-kind-of-output
  gcc-command-options/compiling-c++-programs
  gcc-command-options/options-controlling-c-dialect
  gcc-command-options/options-controlling-c++-dialect
  gcc-command-options/options-controlling-objective-c-and-objective-c++-dialects
  gcc-command-options/options-to-control-diagnostic-messages-formatting
  gcc-command-options/options-to-request-or-suppress-warnings
  gcc-command-options/options-that-control-static-analysis
  gcc-command-options/options-for-debugging-your-program
  gcc-command-options/options-that-control-optimization
  gcc-command-options/program-instrumentation-options
  gcc-command-options/options-controlling-the-preprocessor
  gcc-command-options/passing-options-to-the-assembler
  gcc-command-options/options-for-linking
  gcc-command-options/options-for-directory-search
  gcc-command-options/options-for-code-generation-conventions
  gcc-command-options/gcc-developer-options
  gcc-command-options/machine-dependent-options
  gcc-command-options/specifying-subprocesses-and-the-switches-to-pass-to-them
  gcc-command-options/environment-variables-affecting-gcc
  gcc-command-options/using-precompiled-headers
  copyright

Options
^^^^^^^

