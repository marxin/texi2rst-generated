  .. _null:

NULL --- Function that returns an disassociated pointer
*******************************************************

.. index:: NULL

.. index:: pointer, status

.. index:: pointer, disassociated

:samp:`{Description}:`
  Returns a disassociated pointer.

  If :samp:`{MOLD}` is present, a disassociated pointer of the same type is
  returned, otherwise the type is determined by context.

  In Fortran 95, :samp:`{MOLD}` is optional. Please note that Fortran 2003
  includes cases where it is required.

:samp:`{Standard}:`
  Fortran 95 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``PTR => NULL([MOLD])``

:samp:`{Arguments}:`
  ==============  ================================================
  :samp:`{MOLD}`  (Optional) shall be a pointer of any association
                  status and of any type.
  ==============  ================================================

:samp:`{Return value}:`
  A disassociated pointer.

:samp:`{Example}:`

  .. code-block:: fortran

    REAL, POINTER, DIMENSION(:) :: VEC => NULL ()

:samp:`{See also}:`
  ASSOCIATED

