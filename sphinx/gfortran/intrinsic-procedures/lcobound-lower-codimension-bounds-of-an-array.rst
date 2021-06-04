  .. _lcobound:

LCOBOUND --- Lower codimension bounds of an array
*************************************************

.. index:: LCOBOUND

.. index:: coarray, lower bound

:samp:`{Description}:`
  Returns the lower bounds of a coarray, or a single lower cobound
  along the :samp:`{DIM}` codimension.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = LCOBOUND(COARRAY [, DIM [, KIND]])``

:samp:`{Arguments}:`
  ===============  =======================================================
  :samp:`{ARRAY}`  Shall be an coarray, of any type.
  :samp:`{DIM}`    (Optional) Shall be a scalar ``INTEGER``.
  :samp:`{KIND}`   (Optional) An ``INTEGER`` initialization
                   expression indicating the kind parameter of the result.
  ===============  =======================================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
  :samp:`{KIND}` is absent, the return value is of default integer kind.
  If :samp:`{DIM}` is absent, the result is an array of the lower cobounds of
  :samp:`{COARRAY}`.  If :samp:`{DIM}` is present, the result is a scalar
  corresponding to the lower cobound of the array along that codimension.

:samp:`{See also}:`
  UCOBOUND, 
  LBOUND

