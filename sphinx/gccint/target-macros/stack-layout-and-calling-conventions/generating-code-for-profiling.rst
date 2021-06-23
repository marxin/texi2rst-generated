..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _profiling:

Generating Code for Profiling
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: profiling, code generation

These macros will help you generate code for profiling.

.. c:macro:: FUNCTION_PROFILER (file, labelno)

  A C statement or compound statement to output to :samp:`{file}` some
  assembler code to call the profiling subroutine ``mcount``.

  .. index:: mcount

  The details of how ``mcount`` expects to be called are determined by
  your operating system environment, not by GCC.  To figure them out,
  compile a small program for profiling using the system's installed C
  compiler and look at the assembler code that results.

  Older implementations of ``mcount`` expect the address of a counter
  variable to be loaded into some register.  The name of this variable is
  :samp:`LP` followed by the number :samp:`{labelno}`, so you would generate
  the name using :samp:`LP%d` in a ``fprintf``.

.. c:macro:: PROFILE_HOOK

  A C statement or compound statement to output to :samp:`{file}` some assembly
  code to call the profiling subroutine ``mcount`` even the target does
  not support profiling.

.. c:macro:: NO_PROFILE_COUNTERS

  Define this macro to be an expression with a nonzero value if the
  ``mcount`` subroutine on your system does not need a counter variable
  allocated for each function.  This is true for almost all modern
  implementations.  If you define this macro, you must not use the
  :samp:`{labelno}` argument to ``FUNCTION_PROFILER``.

.. c:macro:: PROFILE_BEFORE_PROLOGUE

  Define this macro if the code for function profiling should come before
  the function prologue.  Normally, the profiling code comes after.

.. function:: bool TARGET_KEEP_LEAF_WHEN_PROFILED (void)

  .. hook-start:TARGET_KEEP_LEAF_WHEN_PROFILED

  This target hook returns true if the target wants the leaf flag for
  the current function to stay true even if it calls mcount.  This might
  make sense for targets using the leaf flag only to determine whether a
  stack frame needs to be generated or not and for which the call to
  mcount is generated before the function prologue.

.. hook-end

