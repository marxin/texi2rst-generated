  .. _acc_map_data:

acc_map_data -- Map previously allocated device memory to host memory.
**********************************************************************

Description
  This function maps previously allocated device and host memory. The device
  memory is specified with the device address :samp:`{d}`. The host memory is
  specified with the host address :samp:`{h}` and a length of :samp:`{len}`.

:samp:`{C/C++}:`

  ============  ===================================================
  *Prototype*:  ``acc_map_data(h_void *h, d_void *d, size_t len);``
  ============  ===================================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.26.

