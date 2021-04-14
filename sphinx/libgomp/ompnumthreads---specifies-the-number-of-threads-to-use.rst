  .. _omp_num_threads:

:envvar:`OMP_NUM_THREADS` - Specifies the number of threads to use
******************************************************************

.. index:: Environment Variable

.. index:: Implementation specific setting

:samp:`{Description}:`
  Specifies the default number of threads to use in parallel regions.  The 
  value of this variable shall be a comma-separated list of positive integers;
  the value specifies the number of threads to use for the corresponding nested
  level.  Specifying more than one item in the list will automatically enable
  nesting by default.  If undefined one thread per CPU is used.

:samp:`{See also}:`
  omp_set_num_threads, OMP_NESTED

:samp:`{Reference}: `
  https://www.openmp.orgOpenMP specification v4.5, Section 4.2

