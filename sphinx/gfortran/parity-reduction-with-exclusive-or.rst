  .. _parity:

PARITY - Reduction with exclusive OR
************************************

.. index:: PARITY

.. index:: Parity

.. index:: Reduction, XOR

.. index:: XOR reduction

:samp:`{Description}:`
  Calculates the parity, i.e. the reduction using ``.XOR.``,
  of :samp:`{MASK}` along dimension :samp:`{DIM}`.

:samp:`{Standard}:`
  Fortran 2008 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ================================
  ``RESULT = PARITY(MASK[, DIM])``
  ================================
  ================================

:samp:`{Arguments}:`
  ==============  ===========================================================
  :samp:`{MASK}`  Shall be an array of type ``LOGICAL``
  ==============  ===========================================================
  :samp:`{DIM}`   (Optional) shall be a scalar of type 
                  ``INTEGER`` with a value in the range from 1 to n, where n 
                  equals the rank of :samp:`{MASK}`.
  ==============  ===========================================================

:samp:`{Return value}:`
  The result is of the same type as :samp:`{MASK}`.

  If :samp:`{DIM}` is absent, a scalar with the parity of all elements in
  :samp:`{MASK}` is returned, i.e. true if an odd number of elements is
  ``.true.`` and false otherwise.  If :samp:`{DIM}` is present, an array
  of rank n-1, where n equals the rank of :samp:`{ARRAY}`,
  and a shape similar to that of :samp:`{MASK}` with dimension :samp:`{DIM}`
  dropped is returned.

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_sum
      LOGICAL :: x(2) = [ .true., .false. ]
      print *, PARITY(x) ! prints "T" (true).
    END PROGRAM

