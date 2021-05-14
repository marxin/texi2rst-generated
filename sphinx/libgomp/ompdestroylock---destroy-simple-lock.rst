  .. _omp_destroy_lock:

omp_destroy_lock - Destroy simple lock
**************************************

:samp:`{Description}:`
  Destroy a simple lock.  In order to be destroyed, a simple lock must be
  in the unlocked state.

:samp:`{C/C++}:`
  ============  ============================================
  *Prototype*:  ``void omp_destroy_lock(omp_lock_t *lock);``
  ============  ============================================
  ============  ============================================

:samp:`{Fortran}:`
  ============  =================================================
  *Interface*:  ``subroutine omp_destroy_lock(svar)``
  ============  =================================================
                ``integer(omp_lock_kind), intent(inout) :: svar``
  ============  =================================================

:samp:`{See also}:`
  omp_init_lock

:samp:`{Reference}: `
  https://www.openmp.orgOpenMP specification v4.5, Section 3.3.3.

