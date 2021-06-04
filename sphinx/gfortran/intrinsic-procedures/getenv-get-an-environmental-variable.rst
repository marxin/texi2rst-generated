  .. _getenv:

GETENV --- Get an environmental variable
****************************************

.. index:: GETENV

.. index:: environment variable

:samp:`{Description}:`
  Get the :samp:`{VALUE}` of the environmental variable :samp:`{NAME}`.

  This intrinsic routine is provided for backwards compatibility with
  GNU Fortran 77.  In new code, programmers should consider the use of
  the GET_ENVIRONMENT_VARIABLE intrinsic defined by the Fortran
  2003 standard.

  Note that ``GETENV`` need not be thread-safe. It is the
  responsibility of the user to ensure that the environment is not being
  updated concurrently with a call to the ``GETENV`` intrinsic.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine

:samp:`{Syntax}:`
  ``CALL GETENV(NAME, VALUE)``

:samp:`{Arguments}:`
  ===============  ===================================================
  :samp:`{NAME}`   Shall be of type ``CHARACTER`` and of default kind.
  :samp:`{VALUE}`  Shall be of type ``CHARACTER`` and of default kind.
  ===============  ===================================================

:samp:`{Return value}:`
  Stores the value of :samp:`{NAME}` in :samp:`{VALUE}`. If :samp:`{VALUE}` is 
  not large enough to hold the data, it is truncated. If :samp:`{NAME}`
  is not set, :samp:`{VALUE}` will be filled with blanks.

:samp:`{Example}:`

  .. code-block:: fortran

    PROGRAM test_getenv
      CHARACTER(len=255) :: homedir
      CALL getenv("HOME", homedir)
      WRITE (*,*) TRIM(homedir)
    END PROGRAM

:samp:`{See also}:`
  GET_ENVIRONMENT_VARIABLE

