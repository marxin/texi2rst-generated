  .. _omp_max_task_priority:

OMP_MAX_TASK_PRIORITY - Set the maximum priority
************************************************

number that can be set for a task.

.. index:: Environment Variable

:samp:`{Description}:`
  Specifies the initial value for the maximum priority value that can be
  set for a task.  The value of this variable shall be a non-negative
  integer, and zero is allowed.  If undefined, the default priority is
  0.

:samp:`{See also}:`
  omp_get_max_task_priority

:samp:`{Reference}: `
  https://www.openmp.orgOpenMP specification v4.5, Section 4.14

