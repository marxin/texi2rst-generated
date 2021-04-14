  .. _acc_get_cuda_stream:

``acc_get_cuda_stream`` - Get CUDA stream handle.
*************************************************

Description
  This function returns the CUDA stream handle for the queue :samp:`{async}`.
  This handle is the same as used by the CUDA Runtime or Driver API's.

:samp:`{C/C++}:`
  ============  =========================================
  *Prototype*:  ``void *acc_get_cuda_stream(int async);``
  ============  =========================================
  ============  =========================================

:samp:`{Reference}:`
  https://www.openacc.orgOpenACC specification v2.6, section
  A.2.1.3.

