  .. _acc_deviceptr:

acc_deviceptr -- Get device pointer associated with specific host address.
**************************************************************************

Description
  This function returns the device address that has been mapped to the
  host address specified by :samp:`{h}`.

:samp:`{C/C++}:`
  ============  ===================================
  *Prototype*:  ``void *acc_deviceptr(h_void *h);``
  ============  ===================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.28.

