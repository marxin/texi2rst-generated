..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _acc_hostptr:

acc_hostptr -- Get host pointer associated with specific device address.
************************************************************************

Description
  This function returns the host address that has been mapped to the
  device address specified by :samp:`{d}`.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void *acc_hostptr(d_void *d);``

Reference:
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.29.
