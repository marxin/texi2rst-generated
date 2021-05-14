  .. _acc_shutdown:

acc_shutdown - Shuts down the runtime for a specific device type.
*****************************************************************

Description
  This function shuts down the runtime for the device type specified in
  :samp:`{devicetype}`.

:samp:`{C/C++}:`
  ============  ==========================================
  *Prototype*:  ``acc_shutdown(acc_device_t devicetype);``
  ============  ==========================================
  ============  ==========================================

:samp:`{Fortran}:`
  ============  =======================================
  *Interface*:  ``subroutine acc_shutdown(devicetype)``
  ============  =======================================
                ``integer(acc_device_kind) devicetype``
  ============  =======================================

:samp:`{Reference}:`
  https://www.openacc.orgOpenACC specification v2.6, section
  3.2.8.

