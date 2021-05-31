.. _link-options:

Influencing the linking step
****************************

.. index:: options, linking

.. index:: linking, static

These options come into play when the compiler links object files into an 
executable output file. They are meaningless if the compiler is not doing 
a link step.

.. option:: -static-libgfortran

  .. index:: static-libgfortran

  On systems that provide libgfortran as a shared and a static
  library, this option forces the use of the static version. If no
  shared version of libgfortran was built when the compiler was
  configured, this option has no effect.
