  .. _omp_default_device:

OMP_DEFAULT_DEVICE - Set the device used in target regions
**********************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Set to choose the device which is used in a ``target`` region, unless the
  value is overridden by ``omp_set_default_device`` or by a ``device``
  clause.  The value shall be the nonnegative device number. If no device with
  the given device number exists, the code is executed on the host.  If unset,
  device number 0 will be used.

:samp:`{See also}:`
  omp_get_default_device, omp_set_default_device,

:samp:`{Reference}:`
  https://www.openmp.orgOpenMP specification v4.5, Section 4.13

