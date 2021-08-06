..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _mips-function-attributes:

MIPS Function Attributes
^^^^^^^^^^^^^^^^^^^^^^^^

These function attributes are supported by the MIPS back end:

.. option:: interrupt

  .. index:: interrupt function attribute, MIPS

  Use this attribute to indicate that the specified function is an interrupt
  handler.  The compiler generates function entry and exit sequences suitable
  for use in an interrupt handler when this attribute is present.
  An optional argument is supported for the interrupt attribute which allows
  the interrupt mode to be described.  By default GCC assumes the external
  interrupt controller (EIC) mode is in use, this can be explicitly set using
  ``eic``.  When interrupts are non-masked then the requested Interrupt
  Priority Level (IPL) is copied to the current IPL which has the effect of only
  enabling higher priority interrupts.  To use vectored interrupt mode use
  the argument ``vector=[sw0|sw1|hw0|hw1|hw2|hw3|hw4|hw5]``, this will change
  the behavior of the non-masked interrupt support and GCC will arrange to mask
  all interrupts from sw0 up to and including the specified interrupt vector.

  You can use the following attributes to modify the behavior
  of an interrupt handler:

  ``use_shadow_register_set``

    .. index:: use_shadow_register_set function attribute, MIPS

    Assume that the handler uses a shadow register set, instead of
    the main general-purpose registers.  An optional argument ``intstack`` is
    supported to indicate that the shadow register set contains a valid stack
    pointer.

  ``keep_interrupts_masked``

    .. index:: keep_interrupts_masked function attribute, MIPS

    Keep interrupts masked for the whole function.  Without this attribute,
    GCC tries to reenable interrupts for as much of the function as it can.

  ``use_debug_exception_return``

    .. index:: use_debug_exception_return function attribute, MIPS

    Return using the ``deret`` instruction.  Interrupt handlers that don't
    have this attribute return using ``eret`` instead.

    You can use any combination of these attributes, as shown below:

  .. code-block:: c++

    void __attribute__ ((interrupt)) v0 ();
    void __attribute__ ((interrupt, use_shadow_register_set)) v1 ();
    void __attribute__ ((interrupt, keep_interrupts_masked)) v2 ();
    void __attribute__ ((interrupt, use_debug_exception_return)) v3 ();
    void __attribute__ ((interrupt, use_shadow_register_set,
                         keep_interrupts_masked)) v4 ();
    void __attribute__ ((interrupt, use_shadow_register_set,
                         use_debug_exception_return)) v5 ();
    void __attribute__ ((interrupt, keep_interrupts_masked,
                         use_debug_exception_return)) v6 ();
    void __attribute__ ((interrupt, use_shadow_register_set,
                         keep_interrupts_masked,
                         use_debug_exception_return)) v7 ();
    void __attribute__ ((interrupt("eic"))) v8 ();
    void __attribute__ ((interrupt("vector=hw3"))) v9 ();

.. option:: long_call

  .. index:: indirect calls, MIPS

  .. index:: long_call function attribute, MIPS

  .. index:: short_call function attribute, MIPS

  .. index:: near function attribute, MIPS

  .. index:: far function attribute, MIPS

  These attributes specify how a particular function is called on MIPS.
  The attributes override the :option:`-mlong-calls` (see :ref:`mips-options`)
  command-line switch.  The :option:`long_call` and :option:`far` attributes are
  synonyms, and cause the compiler to always call
  the function by first loading its address into a register, and then using
  the contents of that register.  The ``short_call`` and :option:`near`
  attributes are synonyms, and have the opposite
  effect; they specify that non-PIC calls should be made using the more
  efficient ``jal`` instruction.

.. option:: mips16

  .. index:: mips16 function attribute, MIPS

  .. index:: nomips16 function attribute, MIPS

  On MIPS targets, you can use the :option:`mips16` and ``nomips16``
  function attributes to locally select or turn off MIPS16 code generation.
  A function with the :option:`mips16` attribute is emitted as MIPS16 code,
  while MIPS16 code generation is disabled for functions with the
  ``nomips16`` attribute.  These attributes override the
  :option:`-mips16` and :option:`-mno-mips16` options on the command line
  (see :ref:`mips-options`).

  When compiling files containing mixed MIPS16 and non-MIPS16 code, the
  preprocessor symbol ``__mips16`` reflects the setting on the command line,
  not that within individual functions.  Mixed MIPS16 and non-MIPS16 code
  may interact badly with some GCC extensions such as ``__builtin_apply``
  (see :ref:`constructing-calls`).

.. option:: micromips, MIPS

  .. index:: micromips function attribute

  .. index:: nomicromips function attribute

  On MIPS targets, you can use the ``micromips`` and ``nomicromips``
  function attributes to locally select or turn off microMIPS code generation.
  A function with the ``micromips`` attribute is emitted as microMIPS code,
  while microMIPS code generation is disabled for functions with the
  ``nomicromips`` attribute.  These attributes override the
  :option:`-mmicromips` and :option:`-mno-micromips` options on the command line
  (see :ref:`mips-options`).

  When compiling files containing mixed microMIPS and non-microMIPS code, the
  preprocessor symbol ``__mips_micromips`` reflects the setting on the
  command line,
  not that within individual functions.  Mixed microMIPS and non-microMIPS code
  may interact badly with some GCC extensions such as ``__builtin_apply``
  (see :ref:`constructing-calls`).

.. option:: nocompression

  .. index:: nocompression function attribute, MIPS

  On MIPS targets, you can use the :option:`nocompression` function attribute
  to locally turn off MIPS16 and microMIPS code generation.  This attribute
  overrides the :option:`-mips16` and :option:`-mmicromips` options on the
  command line (see :ref:`mips-options`).

