.. _omp_get_active_level:

omp_get_active_level - Number of parallel regions
*************************************************

:samp:`{Description}:`
  This function returns the nesting level for the active parallel blocks,
  which enclose the calling call.

C/C++
  ============  ===================================
  *Prototype*:  ``int omp_get_active_level(void);``
  ============  ===================================
  ============  ===================================

:samp:`{Fortran}:`
  ============  ===========================================
  *Interface*:  ``integer function omp_get_active_level()``
  ============  ===========================================
  ============  ===========================================

:samp:`{See also}:`
  omp_get_level, omp_get_max_active_levels, omp_set_max_active_levels

:samp:`{Reference}:`
  https://www.openmp.orgOpenMP specification v4.5, Section 3.2.20.

