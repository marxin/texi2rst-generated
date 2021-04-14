  .. _dshiftr:

``DSHIFTR`` - Combined right shift
**********************************

.. index:: DSHIFTR

.. index:: right shift, combined

.. index:: shift, right

:samp:`{Description}:`
  ``DSHIFTR(I, J, SHIFT)`` combines bits of :samp:`{I}` and :samp:`{J}`. The
  leftmost :samp:`{SHIFT}` bits of the result are the rightmost :samp:`{SHIFT}`
  bits of :samp:`{I}` , and the remaining bits are the leftmost bits of
  :samp:`{J}`.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = DSHIFTR(I, J, SHIFT)``

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
                   shall be less than or equal to ``BIT_SIZE(I)``; otherwise,
                   :samp:`{SHIFT}` shall be less than or equal to ``BIT_SIZE(J)``.
  ===============  ===========================================================================

:samp:`{Return value}:`
  If either :samp:`{I}` or :samp:`{J}` is a BOZ constant, it is first converted
  as if by the intrinsic function ``INT`` to an integer type with the
  kind type parameter of the other.

:samp:`{See also}:`
  DSHIFTL

