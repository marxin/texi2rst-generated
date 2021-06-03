  .. _free:

FREE --- Frees memory
*********************

.. index:: FREE

.. index:: pointer, cray

:samp:`{Description}:`
  Frees memory previously allocated by ``MALLOC``. The ``FREE``
  intrinsic is an extension intended to be used with Cray pointers, and is
  provided in GNU Fortran to allow user to compile legacy code. For
  new code using Fortran 95 pointers, the memory de-allocation intrinsic is
  ``DEALLOCATE``.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL FREE(PTR)``

:samp:`{Arguments}:`
  =============  ===================================================
  :samp:`{PTR}`  The type shall be ``INTEGER``. It represents the
                 location of the memory that should be de-allocated.
  =============  ===================================================
  =============  ===================================================

:samp:`{Return value}:`
  None

:samp:`{Example}:`
  See ``MALLOC`` for an example.

:samp:`{See also}:`
  MALLOC

