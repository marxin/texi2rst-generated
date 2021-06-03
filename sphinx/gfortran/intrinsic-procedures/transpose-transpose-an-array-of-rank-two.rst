  .. _transpose:

TRANSPOSE --- Transpose an array of rank two
********************************************

.. index:: TRANSPOSE

.. index:: array, transpose

.. index:: matrix, transpose

.. index:: transpose

:samp:`{Description}:`
  Transpose an array of rank two. Element (i, j) of the result has the value 
  ``MATRIX(j, i)``, for all i, j.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = TRANSPOSE(MATRIX)``

:samp:`{Arguments}:`
  ================  =====================================================
  :samp:`{MATRIX}`  Shall be an array of any type and have a rank of two.
  ================  =====================================================
  ================  =====================================================

:samp:`{Return value}:`
  The result has the same type as :samp:`{MATRIX}`, and has shape 
  ``(/ m, n /)`` if :samp:`{MATRIX}` has shape ``(/ n, m /)``.

