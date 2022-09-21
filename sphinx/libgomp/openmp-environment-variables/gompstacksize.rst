..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

  .. _gomp_stacksize:

.. index:: Environment Variable

.. index:: Implementation specific setting

GOMP_STACKSIZE -- Set default thread stack size
***********************************************

:samp:`{Description}:`
  Set the default thread stack size in kilobytes.  This is different from
  ``pthread_attr_setstacksize`` which gets the number of bytes as an 
  argument.  If the stack size cannot be set due to system constraints, an 
  error is reported and the initial stack size is left unchanged.  If undefined,
  the stack size is system dependent.

:samp:`{See also}:`
  :ref:`OMP_STACKSIZE`

:samp:`{Reference}: `
  `GCC Patches Mailinglist <https://gcc.gnu.org/ml/gcc-patches/2006-06/msg00493.html>`_, 
  `GCC Patches Mailinglist <https://gcc.gnu.org/ml/gcc-patches/2006-06/msg00496.html>`_

