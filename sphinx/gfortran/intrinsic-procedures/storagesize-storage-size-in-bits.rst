  .. _storage_size:

STORAGE_SIZE --- Storage size in bits
*************************************

.. index:: STORAGE_SIZE

.. index:: storage size

:samp:`{Description}:`
  Returns the storage size of argument :samp:`{A}` in bits.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``RESULT = STORAGE_SIZE(A [, KIND])``

:samp:`{Arguments}:`
  ==============  =========================================================
  :samp:`{A}`     Shall be a scalar or array of any type.
  ==============  =========================================================
  :samp:`{KIND}`  (Optional) shall be a scalar integer constant expression.
  ==============  =========================================================

:samp:`{Return Value}:`
  The result is a scalar integer with the kind type parameter specified by KIND
  (or default integer type if KIND is missing). The result value is the size
  expressed in bits for an element of an array that has the dynamic type and type
  parameters of A.

:samp:`{See also}:`
  C_SIZEOF, 
  SIZEOF

