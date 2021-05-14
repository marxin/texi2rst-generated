  .. _omp_set_default_device:

omp_set_default_device - Set the default device for target regions
******************************************************************

:samp:`{Description}:`
  Set the default device for target regions without device clause.  The argument
  shall be a nonnegative device number.

:samp:`{C/C++}:`
  ============  ================================================
  *Prototype*:  ``void omp_set_default_device(int device_num);``
  ============  ================================================
  ============  ================================================

:samp:`{Fortran}:`
  ============  =================================================
  *Interface*:  ``subroutine omp_set_default_device(device_num)``
  ============  =================================================
                ``integer device_num``
  ============  =================================================

:samp:`{See also}:`
  OMP_DEFAULT_DEVICE, omp_get_default_device

:samp:`{Reference}:`
  https://www.openmp.orgOpenMP specification v4.5, Section 3.2.29.

