..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

    .. _gcov-dump:

gcov-dump---an Offline Gcda and Gcno Profile Dump Tool
------------------------------------------------------

Description
^^^^^^^^^^^

:command:`gcov-dump` is a tool you can use in conjunction with GCC to
dump content of gcda and gcno profile files offline.

Synopsis
^^^^^^^^

gcov-dump
     [ :option:`-v` | :option:`--version` ]
     [ :option:`-h` | :option:`--help` ]
     [ :option:`-l` | :option:`--long` ]
     [ :option:`-p` | :option:`--positions` ]
     [ :option:`-r` | :option:`--raw` ]
     [ :option:`-s` | :option:`--stable` ]
     [ :samp:`{gcovfiles}` ]

Options
^^^^^^^

``-h`` ``--help``
  Display help about using :command:`gcov-dump` (on the standard output), and
  exit without doing any further processing.

``-l`` ``--long``
  Dump content of records.

``-p`` ``--positions``
  Dump positions of records.

``-r`` ``--raw``
  Print content records in raw format.

``-s`` ``--stable``
  Print content in stable format usable for comparison.

``-v`` ``--version``
  Display the :command:`gcov-dump` version number (on the standard output),
  and exit without doing any further processing.

