  .. _getcwd:

GETCWD - Get current working directory
**************************************

.. index:: GETCWD

.. index:: system, working directory

:samp:`{Description}:`
  Get current working directory.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  =============================
  ``CALL GETCWD(C [, STATUS])``
  =============================
  ``STATUS = GETCWD(C)``
  =============================

:samp:`{Arguments}:`
  ================  ====================================================
  :samp:`{C}`       The type shall be ``CHARACTER`` and of default kind.
  ================  ====================================================
  :samp:`{STATUS}`  (Optional) status flag. Returns 0 on success, 
                    a system specific and nonzero error code otherwise.
  ================  ====================================================

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_getcwd
      CHARACTER(len=255) :: cwd
      CALL getcwd(cwd)
      WRITE(*,*) TRIM(cwd)
    END PROGRAM

:samp:`{See also}:`
  CHDIR

