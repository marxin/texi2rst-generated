  .. _acc_malloc:

``acc_malloc`` - Allocate device memory.
****************************************

Description
  This function allocates :samp:`{len}` bytes of device memory. It returns
  the device address of the allocated memory.

:samp:`{C/C++}:`
  ============  ===================================
  *Prototype*:  ``d_void* acc_malloc(size_t len);``
  ============  ===================================
  ============  ===================================

:samp:`{Reference}:`
  https://www.openacc.orgOpenACC specification v2.6, section
  3.2.18.

