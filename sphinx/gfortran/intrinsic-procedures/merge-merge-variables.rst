  .. _merge:

MERGE --- Merge variables
*************************

.. index:: MERGE

.. index:: array, merge arrays

.. index:: array, combine arrays

:samp:`{Description}:`
  Select values from two arrays according to a logical mask.  The result
  is equal to :samp:`{TSOURCE}` if :samp:`{MASK}` is ``.TRUE.``, or equal to
  :samp:`{FSOURCE}` if it is ``.FALSE.``.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = MERGE(TSOURCE, FSOURCE, MASK)``

:samp:`{Arguments}:`
  =================  =============================================
  :samp:`{TSOURCE}`  May be of any type.
  =================  =============================================
  :samp:`{FSOURCE}`  Shall be of the same type and type parameters
                     as :samp:`{TSOURCE}`.
  :samp:`{MASK}`     Shall be of type ``LOGICAL``.
  =================  =============================================

:samp:`{Return value}:`
  The result is of the same type and type parameters as :samp:`{TSOURCE}`.

