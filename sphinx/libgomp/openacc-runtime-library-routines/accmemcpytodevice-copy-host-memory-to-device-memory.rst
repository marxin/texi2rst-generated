  .. _acc_memcpy_to_device:

acc_memcpy_to_device -- Copy host memory to device memory.
**********************************************************

Description
  This function copies host memory specified by host address of :samp:`{src}` to
  device memory specified by the device address :samp:`{dest}` for a length of
  :samp:`{bytes}` bytes.

:samp:`{C/C++}:`
  ============  ==================================================================
  *Prototype*:  ``acc_memcpy_to_device(d_void *dest, h_void *src, size_t bytes);``
  ============  ==================================================================
  ============  ==================================================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.31.

