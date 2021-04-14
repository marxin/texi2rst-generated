.. _d-language-and-abi:

D ABI parameters
****************

.. index:: parameters, d abi

.. function:: void TARGET_D_CPU_VERSIONS(void )

  Declare all environmental version identifiers relating to the target CPU
  using the function ``builtin_version``, which takes a string representing
  the name of the version.  Version identifiers predefined by this hook apply
  to all modules that are being compiled and imported.

.. function:: void TARGET_D_OS_VERSIONS(void )

  Similarly to ``TARGET_D_CPU_VERSIONS``, but is used for versions
  relating to the target operating system.

.. index:: TARGET_D_MINFO_SECTION

D Target Hookconst char *TARGET_D_MINFO_SECTIONContains the name of the section in which module info references should be
placed.  This section is expected to be bracketed by two symbols to indicate
the start and end address of the section, so that the runtime library can
collect all modules for each loaded shared library and executable.  The
default value of ``NULL`` disables the use of sections altogether.

.. index:: TARGET_D_MINFO_START_NAME

D Target Hookconst char *TARGET_D_MINFO_START_NAMEIf ``TARGET_D_MINFO_SECTION`` is defined, then this must also be defined
as the name of the symbol indicating the start address of the module info
section

.. index:: TARGET_D_MINFO_END_NAME

D Target Hookconst char *TARGET_D_MINFO_END_NAMEIf ``TARGET_D_MINFO_SECTION`` is defined, then this must also be defined
as the name of the symbol indicating the end address of the module info
section

