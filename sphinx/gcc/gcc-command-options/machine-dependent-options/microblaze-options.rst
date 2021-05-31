.. _microblaze-options:

MicroBlaze Options
^^^^^^^^^^^^^^^^^^

.. index:: MicroBlaze Options

.. option:: -msoft-float

  Use software emulation for floating point (default).

.. option:: -mhard-float

  Use hardware floating-point instructions.

.. option:: -mmemcpy

  Do not optimize block moves, use ``memcpy``.

.. option:: -mno-clearbss

  This option is deprecated.  Use :option:`-fno-zero-initialized-in-bss` instead.

.. option:: -mcpu=cpu-type

  Use features of, and schedule code for, the given CPU.
  Supported values are in the format :samp:`v{X}.{YY}.{Z}`,
  where :samp:`{X}` is a major version, :samp:`{YY}` is the minor version, and
  :samp:`{Z}` is compatibility code.  Example values are :samp:`v3.00.a`,
  :samp:`v4.00.b`, :samp:`v5.00.a`, :samp:`v5.00.b`, :samp:`v6.00.a`.

.. option:: -mxl-soft-mul

  Use software multiply emulation (default).

.. option:: -mxl-soft-div

  Use software emulation for divides (default).

.. option:: -mxl-barrel-shift

  Use the hardware barrel shifter.

.. option:: -mxl-pattern-compare

  Use pattern compare instructions.

.. option:: -msmall-divides

  Use table lookup optimization for small signed integer divisions.

.. option:: -mxl-stack-check

  This option is deprecated.  Use :option:`-fstack-check` instead.

.. option:: -mxl-gp-opt

  Use GP-relative ``.sdata`` / ``.sbss`` sections.

.. option:: -mxl-multiply-high

  Use multiply high instructions for high part of 32x32 multiply.

.. option:: -mxl-float-convert

  Use hardware floating-point conversion instructions.

.. option:: -mxl-float-sqrt

  Use hardware floating-point square root instruction.

.. option:: -mbig-endian

  Generate code for a big-endian target.

.. option:: -mlittle-endian

  Generate code for a little-endian target.

.. option:: -mxl-reorder

  Use reorder instructions (swap and byte reversed load/store).

:samp:`-mxl-mode-{app-model}`
  Select application model :samp:`{app-model}`.  Valid models are

  :samp:`executable`
    normal executable (default), uses startup code crt0.o.

  .. option:: -mpic-data-is-text-relative

    Assume that the displacement between the text and data segments is fixed
    at static link time.  This allows data to be referenced by offset from start of
    text address instead of GOT since PC-relative addressing is not supported.

  :samp:`xmdstub`
    for use with Xilinx Microprocessor Debugger (XMD) based
    software intrusive debug agent called xmdstub. This uses startup file
    crt1.o and sets the start address of the program to 0x800.

  :samp:`bootstrap`
    for applications that are loaded using a bootloader.
    This model uses startup file crt2.o which does not contain a processor
    reset vector handler. This is suitable for transferring control on a
    processor reset to the bootloader rather than the application.

  :samp:`novectors`
    for applications that do not require any of the
    MicroBlaze vectors. This option may be useful for applications running
    within a monitoring application. This model uses crt3.o as a startup file.

    Option :option:`-xl-mode-`:samp:`{app-model}` is a deprecated alias for
  :option:`-mxl-mode-`:samp:`{app-model}`.
