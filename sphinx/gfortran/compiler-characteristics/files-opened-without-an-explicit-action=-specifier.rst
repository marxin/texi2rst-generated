..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _files-opened-without-an-explicit-action=-specifier:

.. index:: open, action

Files opened without an explicit ACTION= specifier
**************************************************

The Fortran standard says that if an ``OPEN`` statement is executed
without an explicit ``ACTION=`` specifier, the default value is
processor dependent.  GNU Fortran behaves as follows:

* Attempt to open the file with ``ACTION='READWRITE'``

* If that fails, try to open with ``ACTION='READ'``

* If that fails, try to open with ``ACTION='WRITE'``

* If that fails, generate an error

