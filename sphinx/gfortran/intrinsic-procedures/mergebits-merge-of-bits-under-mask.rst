  .. _merge_bits:

MERGE_BITS --- Merge of bits under mask
***************************************

.. index:: MERGE_BITS

.. index:: bits, merge

:samp:`{Description}:`
  ``MERGE_BITS(I, J, MASK)`` merges the bits of :samp:`{I}` and :samp:`{J}`
  as determined by the mask.  The i-th bit of the result is equal to the 
  i-th bit of :samp:`{I}` if the i-th bit of :samp:`{MASK}` is 1; it is equal to
  the i-th bit of :samp:`{J}` otherwise.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = MERGE_BITS(I, J, MASK)``

:samp:`{Arguments}:`
  ==============  ====================================================================
  :samp:`{I}`     Shall be of type ``INTEGER`` or a boz-literal-constant.
  :samp:`{J}`     Shall be of type ``INTEGER`` with the same
                  kind type parameter as :samp:`{I}` or a boz-literal-constant.
                  :samp:`{I}` and :samp:`{J}` shall not both be boz-literal-constants.
  :samp:`{MASK}`  Shall be of type ``INTEGER`` or a boz-literal-constant
                  and of the same kind as :samp:`{I}`.
  ==============  ====================================================================

:samp:`{Return value}:`
  The result is of the same type and kind as :samp:`{I}`.

