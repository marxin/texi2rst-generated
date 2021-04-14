  .. _acc_init:

``acc_init`` - Initialize runtime for a specific device type.
*************************************************************

Description
  This function initializes the runtime for the device type specified in
  :samp:`{devicetype}`.

:samp:`{C/C++}:`
  ============  ======================================
  *Prototype*:  ``acc_init(acc_device_t devicetype);``
  ============  ======================================
  ============  ======================================

:samp:`{Fortran}:`
  ============  =======================================
  *Interface*:  ``subroutine acc_init(devicetype)``
  ============  =======================================
                ``integer(acc_device_kind) devicetype``
  ============  =======================================

:samp:`{Reference}:`
  https://www.openacc.orgOpenACC specification v2.6, section
  3.2.7.

