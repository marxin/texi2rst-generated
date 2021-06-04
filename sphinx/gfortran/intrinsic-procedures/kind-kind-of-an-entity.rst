  .. _kind:

KIND --- Kind of an entity
**************************

.. index:: KIND

.. index:: kind

:samp:`{Description}:`
  ``KIND(X)`` returns the kind value of the entity :samp:`{X}`.

:samp:`{Standard}:`
  Fortran 95 and later

:samp:`{Class}:`
  Inquiry function

:samp:`{Syntax}:`
  ``K = KIND(X)``

:samp:`{Arguments}:`
  ===========  ============================================================
  :samp:`{X}`  Shall be of type ``LOGICAL``, ``INTEGER``,
               ``REAL``, ``COMPLEX`` or ``CHARACTER``.  It may be scalar or
               array valued.
  ===========  ============================================================

:samp:`{Return value}:`
  The return value is a scalar of type ``INTEGER`` and of the default
  integer kind.

:samp:`{Example}:`

  .. code-block:: fortran

    program test_kind
      integer,parameter :: kc = kind(' ')
      integer,parameter :: kl = kind(.true.)

      print *, "The default character kind is ", kc
      print *, "The default logical kind is ", kl
    end program test_kind

