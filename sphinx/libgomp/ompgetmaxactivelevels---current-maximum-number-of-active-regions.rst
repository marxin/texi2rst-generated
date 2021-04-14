  .. _omp_get_max_active_levels:

``omp_get_max_active_levels`` - Current maximum number of active regions
************************************************************************

:samp:`{Description}:`
  This function obtains the maximum allowed number of nested, active parallel regions.

C/C++
  ============  ========================================
  *Prototype*:  ``int omp_get_max_active_levels(void);``
  ============  ========================================
  ============  ========================================

:samp:`{Fortran}:`
  ============  ================================================
  *Interface*:  ``integer function omp_get_max_active_levels()``
  ============  ================================================
  ============  ================================================

:samp:`{See also}:`
  omp_set_max_active_levels, omp_get_active_level

:samp:`{Reference}:`
  https://www.openmp.orgOpenMP specification v4.5, Section 3.2.16.

