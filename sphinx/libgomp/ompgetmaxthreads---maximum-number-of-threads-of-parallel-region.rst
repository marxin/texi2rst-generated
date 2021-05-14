  .. _omp_get_max_threads:

omp_get_max_threads - Maximum number of threads of parallel region
******************************************************************

:samp:`{Description}:`
  Return the maximum number of threads used for the current parallel region
  that does not use the clause ``num_threads``.

:samp:`{C/C++}:`
  ============  ==================================
  *Prototype*:  ``int omp_get_max_threads(void);``
  ============  ==================================
  ============  ==================================

:samp:`{Fortran}:`
  ============  ==========================================
  *Interface*:  ``integer function omp_get_max_threads()``
  ============  ==========================================
  ============  ==========================================

:samp:`{See also}:`
  omp_set_num_threads, omp_set_dynamic, omp_get_thread_limit

:samp:`{Reference}:`
  https://www.openmp.orgOpenMP specification v4.5, Section 3.2.3.

