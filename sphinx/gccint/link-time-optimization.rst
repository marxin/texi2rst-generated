.. _lto:

Link Time Optimization
----------------------

.. index:: lto

.. index:: whopr

.. index:: wpa

.. index:: ltrans

Link Time Optimization (LTO) gives GCC the capability of
dumping its internal representation (GIMPLE) to disk,
so that all the different compilation units that make up
a single executable can be optimized as a single module.
This expands the scope of inter-procedural optimizations
to encompass the whole program (or, rather, everything
that is visible at link time).

.. toctree::

  lto-overview
  lto-object-file-layout
  ipa
  whopr
  internal-flags
  design-overview
  lto-file-sections
  using-summary-information-in-ipa-passes
  whole-program-assumptions-linker-plugin-and-symbol-visibilities
  internal-flags-controlling-lto1

