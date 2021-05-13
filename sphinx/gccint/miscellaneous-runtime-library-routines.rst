.. _miscellaneous-routines:

Miscellaneous runtime library routines
**************************************

Cache control functions
^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void __clear_cache (char *beg, char *end)

  This function clears the instruction cache between :samp:`{beg}` and :samp:`{end}`.

Split stack functions and variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: void * __splitstack_find (void *segment_arg, void *sp, size_t len, void **next_segment, void **next_sp, void **initial_sp)

  When using :option:`-fsplit-stack`, this call may be used to iterate
  over the stack segments.  It may be called like this:

  .. code-block:: c++

      void *next_segment = NULL;
      void *next_sp = NULL;
      void *initial_sp = NULL;
      void *stack;
      size_t stack_size;
      while ((stack = __splitstack_find (next_segment, next_sp,
                                         &stack_size, &next_segment,
                                         &next_sp, &initial_sp))
             != NULL)
        {
          /* Stack segment starts at stack and is
             stack_size bytes long.  */
        }

  There is no way to iterate over the stack segments of a different
  thread.  However, what is permitted is for one thread to call this
  with the :samp:`{segment_arg}` and :samp:`{sp}` arguments NULL, to pass
  :samp:`{next_segment}`, :samp:`{next_sp}`, and :samp:`{initial_sp}` to a different
  thread, and then to suspend one way or another.  A different thread
  may run the subsequent ``__splitstack_find`` iterations.  Of
  course, this will only work if the first thread is suspended while the
  second thread is calling ``__splitstack_find``.  If not, the second
  thread could be looking at the stack while it is changing, and
  anything could happen.

.. index:: __morestack_segments

Variable __morestack_segments
.. index:: __morestack_current_segment

Variable __morestack_current_segment
.. index:: __morestack_initial_sp

Variable __morestack_initial_spInternal variables used by the :option:`-fsplit-stack` implementation.

.. Copyright (C) 2002-2021 Free Software Foundation, Inc.
   This is part of the GCC manual.
   For copying conditions, see the file gcc.texi.

