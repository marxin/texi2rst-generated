  .. _logical:

LOGICAL - Convert to logical type
*********************************

.. index:: LOGICAL

.. index:: conversion, to logical

:samp:`{Description}:`
  Converts one kind of ``LOGICAL`` variable to another.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = LOGICAL(L [, KIND])``

:samp:`{Arguments}:`
  ==============  =======================================================
  :samp:`{L}`     The type shall be ``LOGICAL``.
  ==============  =======================================================
  :samp:`{KIND}`  (Optional) An ``INTEGER`` initialization
                  expression indicating the kind parameter of the result.
  ==============  =======================================================

:samp:`{Return value}:`
  The return value is a ``LOGICAL`` value equal to :samp:`{L}`, with a
  kind corresponding to :samp:`{KIND}`, or of the default logical kind if
  :samp:`{KIND}` is not given.

:samp:`{See also}:`
  INT, 
  REAL, 
  CMPLX

