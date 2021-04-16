.. _invoking-lto-dump:

Invoking :command:`lto-dump`
****************************

.. code-block:: c++

  Usage: lto-dump [OPTION] ... objfiles

:command:`lto-dump` accepts the following options:

@c man begin SYNOPSIS
lto-dump [@option{-list}]
     [@option{-demangle}]
     [@option{-defined-only}]
     [@option{-print-value}]
     [@option{-name-sort}]
     [@option{-size-sort}]
     [@option{-reverse-sort}]
     [@option{-no-sort}]
     [@option{-symbol=}]
     [@option{-objects}]
     [@option{-type-stats}]
     [@option{-tree-stats}]
     [@option{-gimple-stats}]
     [@option{-dump-level=}]
     [@option{-dump-body=}]
     [@option{-help}] @var{lto-dump}
@c man end

.. man begin OPTIONS

``-list``
  Dumps list of details of functions and variables.

``-demangle``
  Dump the demangled output.

``-defined-only``
  Dump only the defined symbols.

``-print-value``
  Dump initial values of the variables.

``-name-sort``
  Sort the symbols alphabetically.

``-size-sort``
  Sort the symbols according to size.

``-reverse-sort``
  Dump the symbols in reverse order.

``-no-sort``
  Dump the symbols in order of occurrence.

``-symbol=``
  Dump the details of specific symbol.

``-objects``
  Dump the details of LTO objects.

``-type-stats``
  Dump the statistics of tree types.

``-tree-stats``
  Dump the statistics of trees.

``-gimple-stats``
  Dump the statistics of gimple statements.

``-dump-level=``
  For deciding the optimization level of body.

``-dump-body=``
  Dump the specific gimple body.

``-help``
  Display the dump tool help.

.. man end
   Copyright (C) 1988-2021 Free Software Foundation, Inc.
   This is part of the GCC manual.
   For copying conditions, see the file gcc.texi.

