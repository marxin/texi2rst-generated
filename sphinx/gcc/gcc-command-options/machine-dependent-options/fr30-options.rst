..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _fr30-options:

FR30 Options
^^^^^^^^^^^^

.. index:: FR30 Options

These options are defined specifically for the FR30 port.

.. option:: -msmall-model

  Use the small address space model.  This can produce smaller code, but
  it does assume that all symbolic values and addresses fit into a
  20-bit range.

.. option:: -mno-lsim

  Assume that runtime support has been provided and so there is no need
  to include the simulator library (libsim.a) on the linker
  command line.

