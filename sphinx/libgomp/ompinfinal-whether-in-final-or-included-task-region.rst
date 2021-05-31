  .. _omp_in_final:

omp_in_final - Whether in final or included task region
*******************************************************

:samp:`{Description}:`
  This function returns ``true`` if currently running in a final
  or included task region, ``false`` otherwise.  Here, ``true``
  and ``false`` represent their language-specific counterparts.

:samp:`{C/C++}:`
  ============  ===========================
  *Prototype*:  ``int omp_in_final(void);``
  ============  ===========================
  ============  ===========================

:samp:`{Fortran}:`
  ============  ===================================
  *Interface*:  ``logical function omp_in_final()``
  ============  ===================================
  ============  ===================================

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.21.

