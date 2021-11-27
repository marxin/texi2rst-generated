..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. program:: AMD GCN

.. _amd-gcn-options:

AMD GCN Options
^^^^^^^^^^^^^^^

.. index:: AMD GCN Options

These options are defined specifically for the AMD GCN port.

.. option:: -march=gpu

  Set architecture type or tuning for :samp:`{gpu}`. Supported values for :samp:`{gpu}`
  are

  .. index:: fiji

  :samp:`fiji`
    Compile for GCN3 Fiji devices (gfx803).

  :samp:`gfx900`
    Compile for GCN5 Vega 10 devices (gfx900).

  :samp:`gfx906`
    Compile for GCN5 Vega 20 devices (gfx906).

.. option:: -msram-ecc=on

  Compile binaries suitable for devices with the SRAM-ECC feature enabled,
  disabled, or either mode.  This feature can be enabled per-process on some
  devices.  The compiled code must match the device mode. The default is
  :samp:`any`, for devices that support it.

.. option:: -mstack-size=bytes

  Specify how many :samp:`{bytes}` of stack space will be requested for each GPU
  thread (wave-front).  Beware that there may be many threads and limited memory
  available.  The size of the stack allocation may also have an impact on
  run-time performance.  The default is 32KB when using OpenACC or OpenMP, and
  1MB otherwise.

.. option:: -mxnack

  Compile binaries suitable for devices with the XNACK feature enabled.  Some
  devices always require XNACK and some allow the user to configure XNACK.  The
  compiled code must match the device mode.  The default is :samp:`-mno-xnack`.
  At present this option is a placeholder for support that is not yet
  implemented.

