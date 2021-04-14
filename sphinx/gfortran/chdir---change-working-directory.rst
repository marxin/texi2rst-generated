  .. _chdir:

``CHDIR`` - Change working directory
************************************

.. index:: CHDIR

.. index:: system, working directory

:samp:`{Description}:`
  Change current working directory to a specified path.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ===============================
  ``CALL CHDIR(NAME [, STATUS])``
  ===============================
  ``STATUS = CHDIR(NAME)``
  ===============================

:samp:`{Arguments}:`
  ================  =========================================================================
  :samp:`{NAME}`    The type shall be ``CHARACTER`` of default
                    kind and shall specify a valid path within the file system.
  ================  =========================================================================
  :samp:`{STATUS}`  (Optional) ``INTEGER`` status flag of the default
                    kind.  Returns 0 on success, and a system specific and nonzero error code
                    otherwise.
  ================  =========================================================================

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_chdir
      CHARACTER(len=255) :: path
      CALL getcwd(path)
      WRITE(*,*) TRIM(path)
      CALL chdir("/tmp")
      CALL getcwd(path)
      WRITE(*,*) TRIM(path)
    END PROGRAM

:samp:`{See also}:`
  GETCWD

