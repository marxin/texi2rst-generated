.. _cris-options:

CRIS Options
^^^^^^^^^^^^

.. index:: CRIS Options

These options are defined specifically for the CRIS ports.

.. option:: -march=architecture-type

  Generate code for the specified architecture.  The choices for
  :samp:`{architecture-type}` are :samp:`v3`, :samp:`v8` and :samp:`v10` for
  respectively ETRAX4, ETRAX100, and ETRAX100LX.
  Default is :samp:`v0` except for cris-axis-linux-gnu, where the default is
  :samp:`v10`.

.. option:: -mtune=architecture-type

  Tune to :samp:`{architecture-type}` everything applicable about the generated
  code, except for the ABI and the set of available instructions.  The
  choices for :samp:`{architecture-type}` are the same as for
  :option:`-march`:samp:`={architecture-type}`.

.. option:: -mmax-stack-frame=n

  Warn when the stack frame of a function exceeds :samp:`{n}` bytes.

.. option:: -metrax4, -metrax100

  The options :option:`-metrax4` and :option:`-metrax100` are synonyms for
  :option:`-march`:samp:`=v3` and :option:`-march`:samp:`=v8` respectively.

.. option:: -mmul-bug-workaround, -mno-mul-bug-workaround

  Work around a bug in the ``muls`` and ``mulu`` instructions for CPU
  models where it applies.  This option is active by default.

.. option:: -mpdebug

  Enable CRIS-specific verbose debug-related information in the assembly
  code.  This option also has the effect of turning off the :samp:`#NO_APP`
  formatted-code indicator to the assembler at the beginning of the
  assembly file.

.. option:: -mcc-init

  Do not use condition-code results from previous instruction; always emit
  compare and test instructions before use of condition codes.

.. option:: -mno-side-effects, -mside-effects

  Do not emit instructions with side effects in addressing modes other than
  post-increment.

.. option:: -mstack-align, -mno-stack-align, -mdata-align, -mno-data-align, -mconst-align, -mno-const-align

  These options (:samp:`no-` options) arrange (eliminate arrangements) for the
  stack frame, individual data and constants to be aligned for the maximum
  single data access size for the chosen CPU model.  The default is to
  arrange for 32-bit alignment.  ABI details such as structure layout are
  not affected by these options.

.. option:: -m32-bit, -m16-bit, -m8-bit

  Similar to the stack- data- and const-align options above, these options
  arrange for stack frame, writable data and constants to all be 32-bit,
  16-bit or 8-bit aligned.  The default is 32-bit alignment.

.. option:: -mno-prologue-epilogue, -mprologue-epilogue

  With :option:`-mno-prologue-epilogue`, the normal function prologue and
  epilogue which set up the stack frame are omitted and no return
  instructions or return sequences are generated in the code.  Use this
  option only together with visual inspection of the compiled code: no
  warnings or errors are generated when call-saved registers must be saved,
  or storage for local variables needs to be allocated.

.. option:: -mno-gotplt, -mgotplt

  With :option:`-fpic` and :option:`-fPIC`, don't generate (do generate)
  instruction sequences that load addresses for functions from the PLT part
  of the GOT rather than (traditional on other architectures) calls to the
  PLT.  The default is :option:`-mgotplt`.

.. option:: -melf

  Legacy no-op option only recognized with the cris-axis-elf and
  cris-axis-linux-gnu targets.

.. option:: -mlinux

  Legacy no-op option only recognized with the cris-axis-linux-gnu target.

.. option:: -sim

  This option, recognized for the cris-axis-elf, arranges
  to link with input-output functions from a simulator library.  Code,
  initialized data and zero-initialized data are allocated consecutively.

.. option:: -sim2

  Like :option:`-sim`, but pass linker options to locate initialized data at
  0x40000000 and zero-initialized data at 0x80000000.
