  .. _omp_get_cancellation:

``omp_get_cancellation`` - Whether cancellation support is enabled
******************************************************************

:samp:`{Description}:`
  This function returns ``true`` if cancellation is activated, ``false``
  otherwise.  Here, ``true`` and ``false`` represent their language-specific
  counterparts.  Unless :envvar:`OMP_CANCELLATION` is set true, cancellations are
  deactivated.

:samp:`{C/C++}:`
  ============  ===================================
  *Prototype*:  ``int omp_get_cancellation(void);``
  ============  ===================================
  ============  ===================================

:samp:`{Fortran}:`
  ============  ===========================================
  *Interface*:  ``logical function omp_get_cancellation()``
  ============  ===========================================
  ============  ===========================================

:samp:`{See also}:`
  OMP_CANCELLATION

:samp:`{Reference}:`
  https://www.openmp.orgOpenMP specification v4.5, Section 3.2.9.

