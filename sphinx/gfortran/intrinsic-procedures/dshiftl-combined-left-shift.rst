  .. _dshiftl:

DSHIFTL --- Combined left shift
*******************************

.. index:: DSHIFTL

.. index:: left shift, combined

.. index:: shift, left

:samp:`{Description}:`
  ``DSHIFTL(I, J, SHIFT)`` combines bits of :samp:`{I}` and :samp:`{J}`. The
  rightmost :samp:`{SHIFT}` bits of the result are the leftmost :samp:`{SHIFT}`
  bits of :samp:`{J}`, and the remaining bits are the rightmost bits of
  :samp:`{I}`.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = DSHIFTL(I, J, SHIFT)``

:samp:`{Arguments}:`
  ===============  ===========================================================================
  :samp:`{I}`      Shall be of type ``INTEGER`` or a BOZ constant.
  ===============  ===========================================================================
  :samp:`{J}`      Shall be of type ``INTEGER`` or a BOZ constant.
                   If both :samp:`{I}` and :samp:`{J}` have integer type, then they shall have
                   the same kind type parameter. :samp:`{I}` and :samp:`{J}` shall not both be
                   BOZ constants.
  :samp:`{SHIFT}`  Shall be of type ``INTEGER``. It shall
                   be nonnegative.  If :samp:`{I}` is not a BOZ constant, then :samp:`{SHIFT}`
                   shall be less than or equal to ``BIT_SIZE(I)`` ; otherwise,
                   :samp:`{SHIFT}` shall be less than or equal to ``BIT_SIZE(J)``.
  ===============  ===========================================================================

:samp:`{Return value}:`
  If either :samp:`{I}` or :samp:`{J}` is a BOZ constant, it is first converted
  as if by the intrinsic function ``INT`` to an integer type with the
  kind type parameter of the other.

:samp:`{See also}:`
  DSHIFTR

