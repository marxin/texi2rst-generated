  .. _omp_get_proc_bind:

``omp_get_proc_bind`` - Whether theads may be moved between CPUs
****************************************************************

:samp:`{Description}:`
  This functions returns the currently active thread affinity policy, which is
  set via :envvar:`OMP_PROC_BIND`.  Possible values are ``omp_proc_bind_false``,
  ``omp_proc_bind_true``, ``omp_proc_bind_master``,
  ``omp_proc_bind_close`` and ``omp_proc_bind_spread``.

:samp:`{C/C++}:`
  ============  ============================================
  *Prototype*:  ``omp_proc_bind_t omp_get_proc_bind(void);``
  ============  ============================================
  ============  ============================================

:samp:`{Fortran}:`
  ============  =================================================================
  *Interface*:  ``integer(kind=omp_proc_bind_kind) function omp_get_proc_bind()``
  ============  =================================================================
  ============  =================================================================

:samp:`{See also}:`
  OMP_PROC_BIND, OMP_PLACES, GOMP_CPU_AFFINITY,

:samp:`{Reference}:`
  https://www.openmp.orgOpenMP specification v4.5, Section 3.2.22.

