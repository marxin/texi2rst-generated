  .. _acc_wait_async:

``acc_wait_async`` - Wait for completion of asynchronous operations.
********************************************************************

Description
  This function enqueues a wait operation on queue :samp:`{async}` for any and all
  asynchronous operations enqueued on queue :samp:`{arg}`.

:samp:`{C/C++}:`
  ============  =======================================
  *Prototype*:  ``acc_wait_async(int arg, int async);``
  ============  =======================================
  ============  =======================================

:samp:`{Fortran}:`
  ============  =========================================
  *Interface*:  ``subroutine acc_wait_async(arg, async)``
  ============  =========================================
                ``integer(acc_handle_kind) arg, async``
  ============  =========================================

:samp:`{Reference}:`
  https://www.openacc.orgOpenACC specification v2.6, section
  3.2.12.

