  .. _index-intrinsic:

INDEX --- Position of a substring within a string
*************************************************

.. index:: INDEX

.. index:: substring position

.. index:: string, find substring

:samp:`{Description}:`
  Returns the position of the start of the first occurrence of string
  :samp:`{SUBSTRING}` as a substring in :samp:`{STRING}`, counting from one.  If
  :samp:`{SUBSTRING}` is not present in :samp:`{STRING}`, zero is returned.  If 
  the :samp:`{BACK}` argument is present and true, the return value is the
  start of the last occurrence rather than the first.

:samp:`{Standard}:`
  Fortran 77 and later, with :samp:`{KIND}` argument Fortran 2003 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = INDEX(STRING, SUBSTRING [, BACK [, KIND]])``

:samp:`{Arguments}:`
  ===================  =======================================================
  :samp:`{STRING}`     Shall be a scalar ``CHARACTER``, with
                       ``INTENT(IN)``
  ===================  =======================================================
  :samp:`{SUBSTRING}`  Shall be a scalar ``CHARACTER``, with
                       ``INTENT(IN)``
  :samp:`{BACK}`       (Optional) Shall be a scalar ``LOGICAL``, with
                       ``INTENT(IN)``
  :samp:`{KIND}`       (Optional) An ``INTEGER`` initialization
                       expression indicating the kind parameter of the result.
  ===================  =======================================================

:samp:`{Return value}:`
  The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
  :samp:`{KIND}` is absent, the return value is of default integer kind.

:samp:`{Specific names}:`
  ============================  =============  ==============  ====================
  Name                          Argument       Return type     Standard
  ============================  =============  ==============  ====================
  ``INDEX(STRING, SUBSTRING)``  ``CHARACTER``  ``INTEGER(4)``  Fortran 77 and later
  ============================  =============  ==============  ====================

:samp:`{See also}:`
  SCAN, 
  VERIFY

