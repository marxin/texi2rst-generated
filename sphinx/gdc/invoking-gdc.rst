..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _invoking-gdc:

Invoking gdc
------------

Synopsis
^^^^^^^^

gdc [ :option:`-c` | :option:`-S` ] [ :option:`-g` ] [ :option:`-pg` ]
    [ :option:`-O`:samp:`{level}` ] [ :option:`-W`:samp:`{warn}`...]
    [ :option:`-I`:samp:`{dir}`...] [ :option:`-L`:samp:`{dir}`...]
    [ :option:`-f`:samp:`{option}`...] [ :option:`-m`:samp:`{machine-option}`...]
    [ :option:`-o` :samp:`{outfile}` ] [@ :samp:`{file}` ] :samp:`{infile}`...

Only the most useful options are listed here; see below for the
remainder.

Description
^^^^^^^^^^^

The :command:`gdc` command is the GNU compiler for the D language and
supports many of the same options as :command:`gcc`.  See :ref:`Option Summary <option-summary>`.
This manual only documents the options specific to :command:`gdc`.

.. toctree::
  :maxdepth: 2

  invoking-gdc/input-and-output-files
  invoking-gdc/runtime-options
  invoking-gdc/directory-options
  invoking-gdc/code-generation
  invoking-gdc/warnings
  invoking-gdc/linking
  invoking-gdc/developer-options
  invoking-gdc/options-for-directory-search
  invoking-gdc/options-for-linking

Options
^^^^^^^
