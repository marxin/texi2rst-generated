.. _pim-and-logitech-3.0-compatible:

PIM and Logitech 3.0 Compatible
*******************************

.. README.texi describes the additional PIM libraries.
    c

These modules are provided to enable legacy Modula-2 applications to
build with GNU Modula-2. It is advised that these module should not
be used for new projects, maybe the ISO libraries or the native
compiler PIM libraries (FIO) should be used instead.

Here is an outline of the module layering:

.. code-block:: modula2

  InOut  RealInOut  LongIO CardinalIO
      \     |       |      /
             Terminal
  -----------------------------------
                |
             Termbase
             /      \
      Keyboard      Display

Above the line are user level PIM [234] and Logitech 3.0 compatible
modules.  Below the line Logitech 3.0 advised that these modules
should be considered part of the runtime system. The libraries do
not provide all the features found in the Logitech libraries as
a number of these features were MS-DOS related. Essentially the
basic input/output, file system, string manipulation and conversion
routines are provided. Access to DOSCALL, graphics, time and date
are not as these were constrained by the limitations of MS-DOS.

The following libraries are contained within the base GNU Modula-2
libraries but are also Logitech-3.0 compatible: ASCII, Storage
and MathLib0.

.. toctree::
  :maxdepth: 2

  pim-and-logitech-30-compatible/gm2-libs-pim-bitblockops
  pim-and-logitech-30-compatible/gm2-libs-pim-bitbyteops
  pim-and-logitech-30-compatible/gm2-libs-pim-bitwordops
  pim-and-logitech-30-compatible/gm2-libs-pim-blockops
  pim-and-logitech-30-compatible/gm2-libs-pim-break
  pim-and-logitech-30-compatible/gm2-libs-pim-cardinalio
  pim-and-logitech-30-compatible/gm2-libs-pim-conversions
  pim-and-logitech-30-compatible/gm2-libs-pim-debugpmd
  pim-and-logitech-30-compatible/gm2-libs-pim-debugtrace
  pim-and-logitech-30-compatible/gm2-libs-pim-delay
  pim-and-logitech-30-compatible/gm2-libs-pim-display
  pim-and-logitech-30-compatible/gm2-libs-pim-errorcode
  pim-and-logitech-30-compatible/gm2-libs-pim-filesystem
  pim-and-logitech-30-compatible/gm2-libs-pim-floatingutilities
  pim-and-logitech-30-compatible/gm2-libs-pim-inout
  pim-and-logitech-30-compatible/gm2-libs-pim-keyboard
  pim-and-logitech-30-compatible/gm2-libs-pim-longio
  pim-and-logitech-30-compatible/gm2-libs-pim-numberconversion
  pim-and-logitech-30-compatible/gm2-libs-pim-random
  pim-and-logitech-30-compatible/gm2-libs-pim-realconversions
  pim-and-logitech-30-compatible/gm2-libs-pim-realinout
  pim-and-logitech-30-compatible/gm2-libs-pim-strings
  pim-and-logitech-30-compatible/gm2-libs-pim-termbase
  pim-and-logitech-30-compatible/gm2-libs-pim-terminal
  pim-and-logitech-30-compatible/gm2-libs-pim-timedate

