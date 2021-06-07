  .. _omp_get_max_task_priority:

omp_get_max_task_priority -- Maximum priority value
***************************************************

that can be set for tasks.

:samp:`{Description}:`
  This function obtains the maximum allowed priority number for tasks.

C/C++

  ============  ========================================
  *Prototype*:  ``int omp_get_max_task_priority(void);``
  ============  ========================================

:samp:`{Fortran}:`

  ============  ================================================
  *Interface*:  ``integer function omp_get_max_task_priority()``
  ============  ================================================

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.29.

