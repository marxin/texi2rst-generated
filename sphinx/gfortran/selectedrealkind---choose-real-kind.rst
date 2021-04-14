  .. _selected_real_kind:

``SELECTED_REAL_KIND`` - Choose real kind
*****************************************

.. index:: SELECTED_REAL_KIND

.. index:: real kind

.. index:: kind, real

.. index:: radix, real

:samp:`{Description}:`
  ``SELECTED_REAL_KIND(P,R)`` returns the kind value of a real data type
  with decimal precision of at least ``P`` digits, exponent range of
  at least ``R``, and with a radix of ``RADIX``.

:samp:`{Standard}:`
  Fortran 90 and later, with ``RADIX`` Fortran 2008 or later

:samp:`{Class}:`
  Transformational function

:samp:`{Syntax}:`
  ``RESULT = SELECTED_REAL_KIND([P, R, RADIX])``

:samp:`{Arguments}:`
  ===============  =====================================================
  :samp:`{P}`      (Optional) shall be a scalar and of type ``INTEGER``.
  ===============  =====================================================
  :samp:`{R}`      (Optional) shall be a scalar and of type ``INTEGER``.
  :samp:`{RADIX}`  (Optional) shall be a scalar and of type ``INTEGER``.
  ===============  =====================================================
  Before Fortran 2008, at least one of the arguments :samp:`{R}` or :samp:`{P}` shall
  be present; since Fortran 2008, they are assumed to be zero if absent.

:samp:`{Return value}:`
  ``SELECTED_REAL_KIND`` returns the value of the kind type parameter of
  a real data type with decimal precision of at least ``P`` digits, a
  decimal exponent range of at least ``R``, and with the requested
  ``RADIX``. If the ``RADIX`` parameter is absent, real kinds with
  any radix can be returned. If more than one real data type meet the
  criteria, the kind of the data type with the smallest decimal precision
  is returned. If no real data type matches the criteria, the result is

  -1 if the processor does not support a real data type with a
    precision greater than or equal to ``P``, but the ``R`` and
    ``RADIX`` requirements can be fulfilled

  -2 if the processor does not support a real type with an exponent
    range greater than or equal to ``R``, but ``P`` and ``RADIX``
    are fulfillable

  :samp:`-3 if {RADIX} but not {P} and {R} requirements`
    are fulfillable

  :samp:`-4 if {RADIX} and either {P} or {R} requirements`
    are fulfillable

    :samp:`-5 if there is no real type with the given {RADIX}`
:samp:`{Example}:`

  .. code-block:: c++

    program real_kinds
      integer,parameter :: p6 = selected_real_kind(6)
      integer,parameter :: p10r100 = selected_real_kind(10,100)
      integer,parameter :: r400 = selected_real_kind(r=400)
      real(kind=p6) :: x
      real(kind=p10r100) :: y
      real(kind=r400) :: z

      print *, precision(x), range(x)
      print *, precision(y), range(y)
      print *, precision(z), range(z)
    end program real_kinds

:samp:`{See also}:`
  PRECISION, 
  RANGE, 
  RADIX

