  .. _omp_init_lock:

``omp_init_lock`` - Initialize simple lock
******************************************

:samp:`{Description}:`
  Initialize a simple lock.  After initialization, the lock is in
  an unlocked state.

:samp:`{C/C++}:`
  ============  =========================================
  *Prototype*:  ``void omp_init_lock(omp_lock_t *lock);``
  ============  =========================================
  ============  =========================================

:samp:`{Fortran}:`
  ============  ===============================================
  *Interface*:  ``subroutine omp_init_lock(svar)``
  ============  ===============================================
                ``integer(omp_lock_kind), intent(out) :: svar``
  ============  ===============================================

:samp:`{See also}:`
  omp_destroy_lock

:samp:`{Reference}: `
  https://www.openmp.orgOpenMP specification v4.5, Section 3.3.1.

