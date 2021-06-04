  .. _modulo:

MODULO --- Modulo function
**************************

.. index:: MODULO

.. index:: modulo

.. index:: division, modulo

:samp:`{Description}:`
  ``MODULO(A,P)`` computes the :samp:`{A}` modulo :samp:`{P}`.

:samp:`{Standard}:`
  Fortran 95 and later

:samp:`{Class}:`
  Elemental function

:samp:`{Syntax}:`
  ``RESULT = MODULO(A, P)``

:samp:`{Arguments}:`
  ===========  ============================================================================
  :samp:`{A}`  Shall be a scalar of type ``INTEGER`` or ``REAL``.
  :samp:`{P}`  Shall be a scalar of the same type and kind as :samp:`{A}`. 
               It shall not be zero.  (As a GNU extension, arguments of different kinds are
               permitted.)
  ===========  ============================================================================

:samp:`{Return value}:`
  The type and kind of the result are those of the arguments.  (As a GNU
  extension, kind is the largest kind of the actual arguments.)

  :samp:`If {A} and {P} are of type {INTEGER}:`
    ``MODULO(A,P)`` has the value :samp:`{R}` such that ``A=Q*P+R``, where
    :samp:`{Q}` is an integer and :samp:`{R}` is between 0 (inclusive) and :samp:`{P}`
    (exclusive).

  :samp:`If {A} and {P} are of type {REAL}:`
    ``MODULO(A,P)`` has the value of ``A - FLOOR (A / P) * P``.

    The returned value has the same sign as P and a magnitude less than
  the magnitude of P.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_modulo
      print *, modulo(17,3)
      print *, modulo(17.5,5.5)

      print *, modulo(-17,3)
      print *, modulo(-17.5,5.5)

      print *, modulo(17,-3)
      print *, modulo(17.5,-5.5)
    end program

:samp:`{See also}:`
  MOD

