  .. _stat:

``STAT`` - Get file status
**************************

.. index:: STAT

.. index:: file system, file status

:samp:`{Description}:`
  This function returns information about a file. No permissions are required on 
  the file itself, but execute (search) permission is required on all of the 
  directories in path that lead to the file.

  The elements that are obtained and stored in the array ``VALUES`` :

  ==============  =====================================================================
  ``VALUES(1)``   Device ID
  ==============  =====================================================================
  ``VALUES(2)``   Inode number
  ``VALUES(3)``   File mode
  ``VALUES(4)``   Number of links
  ``VALUES(5)``   Owner's uid
  ``VALUES(6)``   Owner's gid
  ``VALUES(7)``   ID of device containing directory entry for file (0 if not available)
  ``VALUES(8)``   File size (bytes)
  ``VALUES(9)``   Last access time
  ``VALUES(10)``  Last modification time
  ``VALUES(11)``  Last file status change time
  ``VALUES(12)``  Preferred I/O block size (-1 if not available)
  ``VALUES(13)``  Number of blocks allocated (-1 if not available)
  ==============  =====================================================================
  Not all these elements are relevant on all systems. 
  If an element is not relevant, it is returned as 0.

  This intrinsic is provided in both subroutine and function forms; however,
  only one form can be used in any given program unit.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ======================================
  ``CALL STAT(NAME, VALUES [, STATUS])``
  ======================================
  ``STATUS = STAT(NAME, VALUES)``
  ======================================

:samp:`{Arguments}:`
  ================  =========================================================
  :samp:`{NAME}`    The type shall be ``CHARACTER``, of the
                    default kind and a valid path within the file system.
  ================  =========================================================
  :samp:`{VALUES}`  The type shall be ``INTEGER(4), DIMENSION(13)``.
  :samp:`{STATUS}`  (Optional) status flag of type ``INTEGER(4)``. Returns 0 
                    on success and a system specific error code otherwise.
  ================  =========================================================

:samp:`{Example}:`

  .. code-block:: c++

    PROGRAM test_stat
      INTEGER, DIMENSION(13) :: buff
      INTEGER :: status

      CALL STAT("/etc/passwd", buff, status)

      IF (status == 0) THEN
        WRITE (*, FMT="('Device ID:',               T30, I19)") buff(1)
        WRITE (*, FMT="('Inode number:',            T30, I19)") buff(2)
        WRITE (*, FMT="('File mode (octal):',       T30, O19)") buff(3)
        WRITE (*, FMT="('Number of links:',         T30, I19)") buff(4)
        WRITE (*, FMT="('Owner''s uid:',            T30, I19)") buff(5)
        WRITE (*, FMT="('Owner''s gid:',            T30, I19)") buff(6)
        WRITE (*, FMT="('Device where located:',    T30, I19)") buff(7)
        WRITE (*, FMT="('File size:',               T30, I19)") buff(8)
        WRITE (*, FMT="('Last access time:',        T30, A19)") CTIME(buff(9))
        WRITE (*, FMT="('Last modification time',   T30, A19)") CTIME(buff(10))
        WRITE (*, FMT="('Last status change time:', T30, A19)") CTIME(buff(11))
        WRITE (*, FMT="('Preferred block size:',    T30, I19)") buff(12)
        WRITE (*, FMT="('No. of blocks allocated:', T30, I19)") buff(13)
      END IF
    END PROGRAM

:samp:`{See also}:`
  To stat an open file: 
  FSTAT 
  To stat a link: 
  LSTAT

