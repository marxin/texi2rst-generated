  .. _omp_in_parallel:

``omp_in_parallel`` - Whether a parallel region is active
*********************************************************

:samp:`{Description}:`
  This function returns ``true`` if currently running in parallel,
  ``false`` otherwise.  Here, ``true`` and ``false`` represent
  their language-specific counterparts.

:samp:`{C/C++}:`
  ============  ==============================
  *Prototype*:  ``int omp_in_parallel(void);``
  ============  ==============================
  ============  ==============================

:samp:`{Fortran}:`
  ============  ======================================
  *Interface*:  ``logical function omp_in_parallel()``
  ============  ======================================
  ============  ======================================

:samp:`{Reference}:`
  https://www.openmp.orgOpenMP specification v4.5, Section 3.2.6.

