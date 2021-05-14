  .. _omp_max_active_levels:

OMP_MAX_ACTIVE_LEVELS - Set the maximum number of nested parallel regions
*************************************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Specifies the initial value for the maximum number of nested parallel
  regions.  The value of this variable shall be a positive integer.
  If undefined, then if :envvar:`OMP_NESTED` is defined and set to true, or
  if :envvar:`OMP_NUM_THREADS` or :envvar:`OMP_PROC_BIND` are defined and set to
  a list with more than one item, the maximum number of nested parallel
  regions will be initialized to the largest number supported, otherwise
  it will be set to one.

:samp:`{See also}:`
  omp_set_max_active_levels, OMP_NESTED

:samp:`{Reference}: `
  https://www.openmp.orgOpenMP specification v4.5, Section 4.9

