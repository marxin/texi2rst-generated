.. _invoking-gcov-tool:

Invoking :command:`gcov-tool`
*****************************

.. code-block:: c++

  gcov-tool [global-options] SUB_COMMAND [sub_command-options] profile_dir

:command:`gcov-tool` accepts the following options:

@c man begin SYNOPSIS
gcov-tool [ :option:`-v` | :option:`--version` ] [ :option:`-h` | :option:`--help` ]

gcov-tool merge [merge-options] :samp:`{directory1}` :samp:`{directory2}`
     [ :option:`-o` | :option:`--output` :samp:`{directory}` ]
     [ :option:`-v` | :option:`--verbose` ]
     [ :option:`-w` | :option:`--weight` :samp:`{w1,w2}` ]

gcov-tool rewrite [rewrite-options] :samp:`{directory}`
     [ :option:`-n` | :option:`--normalize` :samp:`{long_long_value}` ]
     [ :option:`-o` | :option:`--output` :samp:`{directory}` ]
     [ :option:`-s` | :option:`--scale` :samp:`{float_or_simple-frac_value}` ]
     [ :option:`-v` | :option:`--verbose` ]

gcov-tool overlap [overlap-options] :samp:`{directory1}` :samp:`{directory2}`
     [ :option:`-f` | :option:`--function` ]
     [ :option:`-F` | :option:`--fullname` ]
     [ :option:`-h` | :option:`--hotonly` ]
     [ :option:`-o` | :option:`--object` ]
     [ :option:`-t` | :option:`--hot_threshold` ] :samp:`{float}`
     [ :option:`-v` | :option:`--verbose` ]

@c man end
@c man begin SEEALSO
gpl(7), gfdl(7), fsf-funding(7), gcc(1), gcov(1) and the Info entry for
@file{gcc}.
@c man end

.. man begin OPTIONS

``-h`` ``--help``
  Display help about using :command:`gcov-tool` (on the standard output), and
  exit without doing any further processing.

``-v`` ``--version``
  Display the :command:`gcov-tool` version number (on the standard output),
  and exit without doing any further processing.

``merge``
  Merge two profile directories.

  :samp:`-o {directory}` :samp:`--output {directory}`
    Set the output profile directory. Default output directory name is
    :samp:`{merged_profile}`.

  ``-v`` ``--verbose``
    Set the verbose mode.

  :samp:`-w {w1},{w2}` :samp:`--weight {w1},{w2}`
    Set the merge weights of the :samp:`{directory1}` and :samp:`{directory2}`,
    respectively. The default weights are 1 for both.

``rewrite``
  Read the specified profile directory and rewrite to a new directory.

  :samp:`-n {long_long_value}` ``--normalize <long_long_value>``
    Normalize the profile. The specified value is the max counter value
    in the new profile.

  :samp:`-o {directory}` :samp:`--output {directory}`
    Set the output profile directory. Default output name is :samp:`{rewrite_profile}`.

  :samp:`-s {float_or_simple-frac_value}` :samp:`--scale {float_or_simple-frac_value}`
    Scale the profile counters. The specified value can be in floating point value,
    or simple fraction value form, such 1, 2, 2/3, and 5/3.

  ``-v`` ``--verbose``
    Set the verbose mode.

``overlap``
  Compute the overlap score between the two specified profile directories.
  The overlap score is computed based on the arc profiles. It is defined as
  the sum of min (p1_counter[i] / p1_sum_all, p2_counter[i] / p2_sum_all),
  for all arc counter i, where p1_counter[i] and p2_counter[i] are two
  matched counters and p1_sum_all and p2_sum_all are the sum of counter
  values in profile 1 and profile 2, respectively.

  ``-f`` ``--function``
    Print function level overlap score.

  ``-F`` ``--fullname``
    Print full gcda filename.

  ``-h`` ``--hotonly``
    Only print info for hot objects/functions.

  ``-o`` ``--object``
    Print object level overlap score.

  :samp:`-t {float}` ``--hot_threshold <float>``
    Set the threshold for hot counter value.

  ``-v`` ``--verbose``
    Set the verbose mode.

.. man end
   Copyright (C) 2017-2021 Free Software Foundation, Inc.
   This is part of the GCC manual.
   For copying conditions, see the file gcc.texi.

@c man begin COPYRIGHT
Copyright @copyright{} 2017-2021 Free Software Foundation, Inc.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with the
Invariant Sections being ``GNU General Public License'' and ``Funding
Free Software'', the Front-Cover texts being (a) (see below), and with
the Back-Cover Texts being (b) (see below).  A copy of the license is
included in the gfdl(7) man page.

(a) The FSF's Front-Cover Text is:

     A GNU Manual

(b) The FSF's Back-Cover Text is:

     You have freedom to copy and modify this GNU Manual, like GNU
     software.  Copies published by the Free Software Foundation raise
     funds for GNU development.
@c man end
@c Set file name and title for the man page.
@setfilename gcov-dump
@settitle offline gcda and gcno profile dump tool
