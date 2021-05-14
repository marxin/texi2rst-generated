  .. _omp_is_initial_device:

omp_is_initial_device - Whether executing on the host device
************************************************************

:samp:`{Description}:`
  This function returns ``true`` if currently running on the host device,
  ``false`` otherwise.  Here, ``true`` and ``false`` represent
  their language-specific counterparts.

:samp:`{C/C++}:`
  ============  ====================================
  *Prototype*:  ``int omp_is_initial_device(void);``
  ============  ====================================
  ============  ====================================

:samp:`{Fortran}:`
  ============  ============================================
  *Interface*:  ``logical function omp_is_initial_device()``
  ============  ============================================
  ============  ============================================

:samp:`{Reference}:`
  https://www.openmp.orgOpenMP specification v4.5, Section 3.2.34.

