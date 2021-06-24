..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_get_current_cuda_context:

acc_get_current_cuda_context -- Get CUDA context handle.
********************************************************

Description
  This function returns the CUDA context handle. This handle is the same
  as used by the CUDA Runtime or Driver API's.

:samp:`{C/C++}:`

  ============  =============================================
  *Prototype*:  ``void *acc_get_current_cuda_context(void);``
  ============  =============================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  A.2.1.2.
