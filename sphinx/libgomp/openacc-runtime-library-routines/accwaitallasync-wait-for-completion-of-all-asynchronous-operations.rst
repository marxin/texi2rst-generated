  .. _acc_wait_all_async:

acc_wait_all_async -- Wait for completion of all asynchronous operations.
*************************************************************************

Description
  This function enqueues a wait operation on the queue :samp:`{async}` for any
  and all asynchronous operations that have been previously enqueued on
  any queue.

:samp:`{C/C++}:`
  ============  ==================================
  *Prototype*:  ``acc_wait_all_async(int async);``
  ============  ==================================

:samp:`{Fortran}:`
  ============  ========================================
  *Interface*:  ``subroutine acc_wait_all_async(async)``
                ``integer(acc_handle_kind) async``
  ============  ========================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.14.

