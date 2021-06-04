  .. _transfer:

TRANSFER --- Transfer bit patterns
**********************************

.. index:: TRANSFER

.. index:: bits, move

.. index:: type cast

:samp:`{Description}:`
  Interprets the bitwise representation of :samp:`{SOURCE}` in memory as if it
  is the representation of a variable or array of the same type and type
  parameters as :samp:`{MOLD}`.

  This is approximately equivalent to the C concept of *casting* one
  type to another.

:samp:`{Standard}:`
  Fortran 90 and later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = TRANSFER(SOURCE, MOLD[, SIZE])``

:samp:`{Arguments}:`
  ================  ==========================================
  :samp:`{SOURCE}`  Shall be a scalar or an array of any type.
  ================  ==========================================
  :samp:`{MOLD}`    Shall be a scalar or an array of any type.
  :samp:`{SIZE}`    (Optional) shall be a scalar of type 
                    ``INTEGER``.
  ================  ==========================================

:samp:`{Return value}:`
  The result has the same type as :samp:`{MOLD}`, with the bit level
  representation of :samp:`{SOURCE}`.  If :samp:`{SIZE}` is present, the result is
  a one-dimensional array of length :samp:`{SIZE}`.  If :samp:`{SIZE}` is absent
  but :samp:`{MOLD}` is an array (of any size or shape), the result is a one-
  dimensional array of the minimum length needed to contain the entirety
  of the bitwise representation of :samp:`{SOURCE}`.   If :samp:`{SIZE}` is absent
  and :samp:`{MOLD}` is a scalar, the result is a scalar.

  If the bitwise representation of the result is longer than that of
  :samp:`{SOURCE}`, then the leading bits of the result correspond to those of
  :samp:`{SOURCE}` and any trailing bits are filled arbitrarily.

  When the resulting bit representation does not correspond to a valid
  representation of a variable of the same type as :samp:`{MOLD}`, the results
  are undefined, and subsequent operations on the result cannot be
  guaranteed to produce sensible behavior.  For example, it is possible to
  create ``LOGICAL`` variables for which ``VAR`` and
  ``.NOT.VAR`` both appear to be true.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_transfer
      integer :: x = 2143289344
      print *, transfer(x, 1.0)    ! prints "NaN" on i686
    END PROGRAM

