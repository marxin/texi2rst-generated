.. _gm2-libs-iso-errnocategory:

gm2-libs-iso/ErrnoCategory
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE ErrnoCategory ;

  (*
     provides an interface to errno (if the system
     supports it) which determines whether the current
     errno is a hard or soft error.  These distinctions
     are needed by the ISO Modula-2 libraries.  Not all
     errno values are tested, only those which could be
     related to a device.
  *)

  IMPORT ChanConsts ;

  (*
     IsErrnoHard - returns TRUE if the value of errno is associated with
                   a hard device error.
  *)

  IsErrnoHard
  PROCEDURE IsErrnoHard (e: INTEGER) : BOOLEAN ;

  (*
     IsErrnoSoft - returns TRUE if the value of errno is associated with
                   a soft device error.
  *)

  IsErrnoSoft
  PROCEDURE IsErrnoSoft (e: INTEGER) : BOOLEAN ;

  (*
     UnAvailable - returns TRUE if the value of errno indicates that
                   the resource or device is unavailable for some
                   reason.
  *)

  UnAvailable
  PROCEDURE UnAvailable (e: INTEGER) : BOOLEAN ;

  (*
     GetOpenResults - maps errno onto the ISO Modula-2 enumerated
                      type, OpenResults.
  *)

  GetOpenResults
  PROCEDURE GetOpenResults (e: INTEGER) : ChanConsts.OpenResults ;

  END ErrnoCategory.

