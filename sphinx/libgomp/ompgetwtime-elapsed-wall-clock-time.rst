  .. _omp_get_wtime:

omp_get_wtime - Elapsed wall clock time
***************************************

:samp:`{Description}:`
  Elapsed wall clock time in seconds.  The time is measured per thread, no
  guarantee can be made that two distinct threads measure the same time.
  Time is measured from some "time in the past", which is an arbitrary time
  guaranteed not to change during the execution of the program.

:samp:`{C/C++}:`
  ============  ===============================
  *Prototype*:  ``double omp_get_wtime(void);``
  ============  ===============================
  ============  ===============================

:samp:`{Fortran}:`
  ============  =============================================
  *Interface*:  ``double precision function omp_get_wtime()``
  ============  =============================================
  ============  =============================================

:samp:`{See also}:`
  omp_get_wtick

:samp:`{Reference}: `
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.4.1.

