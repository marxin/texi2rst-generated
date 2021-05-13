.. _invoking-gcov-dump:

Invoking :command:`gcov-dump`
*****************************

.. code-block:: c++

  Usage: gcov-dump [OPTION] ... gcovfiles

:command:`gcov-dump` accepts the following options:

@c man begin SYNOPSIS
gcov-dump [ :option:`-v` | :option:`--version` ]
     [ :option:`-h` | :option:`--help` ]
     [ :option:`-l` | :option:`--long` ]
     [ :option:`-p` | :option:`--positions` ]
     [ :option:`-r` | :option:`--raw` ]
     :samp:`{gcovfiles}`
@c man end

.. man begin OPTIONS

``-h`` ``--help``
  Display help about using :command:`gcov-dump` (on the standard output), and
  exit without doing any further processing.

``-l`` ``--long``
  Dump content of records.

``-p`` ``--positions``
  Dump positions of records.

``-r`` ``--raw``
  Print content records in raw format.

``-v`` ``--version``
  Display the :command:`gcov-dump` version number (on the standard output),
  and exit without doing any further processing.

.. man end
   Copyright (C) 2018-2021 Free Software Foundation, Inc.
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
@setfilename lto-dump
@settitle Tool for dumping LTO object files.
