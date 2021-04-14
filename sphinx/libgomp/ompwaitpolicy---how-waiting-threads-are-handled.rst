  .. _omp_wait_policy:

:envvar:`OMP_WAIT_POLICY` - How waiting threads are handled
***********************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Specifies whether waiting threads should be active or passive.  If
  the value is ``PASSIVE``, waiting threads should not consume CPU
  power while waiting; while the value is ``ACTIVE`` specifies that
  they should.  If undefined, threads wait actively for a short time
  before waiting passively.

:samp:`{See also}:`
  GOMP_SPINCOUNT

:samp:`{Reference}: `
  https://www.openmp.orgOpenMP specification v4.5, Section 4.8

