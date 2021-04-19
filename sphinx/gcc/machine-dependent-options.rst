.. _submodel-options:

Machine-Dependent Options
*************************

.. index:: submodel options

.. index:: specifying hardware config

.. index:: hardware models and configurations, specifying

.. index:: target-dependent options

.. index:: machine-dependent options

Each target machine supported by GCC can have its own options-for
example, to allow you to compile for a particular processor variant or
ABI, or to control optimizations specific to that machine.  By
convention, the names of machine-specific options start with
:samp:`-m`.

Some configurations of the compiler also support additional target-specific
options, usually for compatibility with other compilers on the same
platform.

.. This list is ordered alphanumerically by subsection name.
   It should be the same order and spelling as these options are listed
   in Machine Dependent Options

.. toctree::

   <aarch64-options>
   <adapteva-epiphany-options>
   <amd-gcn-options>
   <arc-options>
   <arm-options>
   <avr-options>
   <blackfin-options>
   <c6x-options>
   <cris-options>
   <cr16-options>
   <c-sky-options>
   <darwin-options>
   <dec-alpha-options>
   <ebpf-options>
   <fr30-options>
   <ft32-options>
   <frv-options>
   <gnu-linux-options>
   <h8-300-options>
   <hppa-options>
   <ia-64-options>
   <lm32-options>
   <m32c-options>
   <m32r-d-options>
   <m680x0-options>
   <mcore-options>
   <mep-options>
   <microblaze-options>
   <mips-options>
   <mmix-options>
   <mn10300-options>
   <moxie-options>
   <msp430-options>
   <nds32-options>
   <nios-ii-options>
   <nvidia-ptx-options>
   <openrisc-options>
   <pdp-11-options>
   <picochip-options>
   <powerpc-options>
   <pru-options>
   <risc-v-options>
   <rl78-options>
   <rs-6000-and-powerpc-options>
   <rx-options>
   <s-390-and-zseries-options>
   <score-options>
   <sh-options>
   <solaris-2-options>
   <sparc-options>
   <system-v-options>
   <tile-gx-options>
   <tilepro-options>
   <v850-options>
   <vax-options>
   <visium-options>
   <vms-options>
   <vxworks-options>
   <x86-options>
   <x86-windows-options>
   <xstormy16-options>
   <xtensa-options>
   <zseries-options>

.. _aarch64-options:

AArch64 Options
^^^^^^^^^^^^^^^

.. index:: AArch64 Options

These options are defined for AArch64 implementations:

.. option:: -mabi=name

  Generate code for the specified data model.  Permissible values
  are :samp:`ilp32` for SysV-like data model where int, long int and pointers
  are 32 bits, and :samp:`lp64` for SysV-like data model where int is 32 bits,
  but long int and pointers are 64 bits.

  The default depends on the specific target configuration.  Note that
  the LP64 and ILP32 ABIs are not link-compatible; you must compile your
  entire program with the same ABI, and link with a compatible set of libraries.

.. option:: -mbig-endian

  Generate big-endian code.  This is the default when GCC is configured for an
  :samp:`aarch64_be-*-*` target.

.. option:: -mgeneral-regs-only

  Generate code which uses only the general-purpose registers.  This will prevent
  the compiler from using floating-point and Advanced SIMD registers but will not
  impose any restrictions on the assembler.

.. option:: -mlittle-endian

  Generate little-endian code.  This is the default when GCC is configured for an
  :samp:`aarch64-*-*` but not an :samp:`aarch64_be-*-*` target.

.. option:: -mcmodel=tiny

  Generate code for the tiny code model.  The program and its statically defined
  symbols must be within 1MB of each other.  Programs can be statically or
  dynamically linked.

.. option:: -mcmodel=small

  Generate code for the small code model.  The program and its statically defined
  symbols must be within 4GB of each other.  Programs can be statically or
  dynamically linked.  This is the default code model.

.. option:: -mcmodel=large

  Generate code for the large code model.  This makes no assumptions about
  addresses and sizes of sections.  Programs can be statically linked only.  The
  :option:`-mcmodel=large` option is incompatible with :option:`-mabi=ilp32` ,
  :option:`-fpic` and :option:`-fPIC`.

.. option:: -mstrict-align, -mno-strict-align

  Avoid or allow generating memory accesses that may not be aligned on a natural
  object boundary as described in the architecture specification.

.. option:: -momit-leaf-frame-pointer, -mno-omit-leaf-frame-pointer

  Omit or keep the frame pointer in leaf functions.  The former behavior is the
  default.

.. option:: -mstack-protector-guard=guard

  Generate stack protection code using canary at :samp:`{guard}`.  Supported
  locations are :samp:`global` for a global canary or :samp:`sysreg` for a
  canary in an appropriate system register.

  With the latter choice the options
  :option:`-mstack-protector-guard-reg`:samp:`={reg}` and
  :option:`-mstack-protector-guard-offset`:samp:`={offset}` furthermore specify
  which system register to use as base register for reading the canary,
  and from what offset from that base register. There is no default
  register or offset as this is entirely for use within the Linux
  kernel.

.. option:: -mtls-dialect=desc

  Use TLS descriptors as the thread-local storage mechanism for dynamic accesses
  of TLS variables.  This is the default.

.. option:: -mtls-dialect=traditional

  Use traditional TLS as the thread-local storage mechanism for dynamic accesses
  of TLS variables.

.. option:: -mtls-size=size

  Specify bit size of immediate TLS offsets.  Valid values are 12, 24, 32, 48.
  This option requires binutils 2.26 or newer.

.. option:: -mfix-cortex-a53-835769, -mno-fix-cortex-a53-835769

  Enable or disable the workaround for the ARM Cortex-A53 erratum number 835769.
  This involves inserting a NOP instruction between memory instructions and
  64-bit integer multiply-accumulate instructions.

.. option:: -mfix-cortex-a53-843419, -mno-fix-cortex-a53-843419

  Enable or disable the workaround for the ARM Cortex-A53 erratum number 843419.
  This erratum workaround is made at link time and this will only pass the
  corresponding flag to the linker.

.. option:: -mlow-precision-recip-sqrt, -mno-low-precision-recip-sqrt

  Enable or disable the reciprocal square root approximation.
  This option only has an effect if :option:`-ffast-math` or
  :option:`-funsafe-math-optimizations` is used as well.  Enabling this reduces
  precision of reciprocal square root results to about 16 bits for
  single precision and to 32 bits for double precision.

.. option:: -mlow-precision-sqrt, -mno-low-precision-sqrt

  Enable or disable the square root approximation.
  This option only has an effect if :option:`-ffast-math` or
  :option:`-funsafe-math-optimizations` is used as well.  Enabling this reduces
  precision of square root results to about 16 bits for
  single precision and to 32 bits for double precision.
  If enabled, it implies :option:`-mlow-precision-recip-sqrt`.

.. option:: -mlow-precision-div, -mno-low-precision-div

  Enable or disable the division approximation.
  This option only has an effect if :option:`-ffast-math` or
  :option:`-funsafe-math-optimizations` is used as well.  Enabling this reduces
  precision of division results to about 16 bits for
  single precision and to 32 bits for double precision.

``-mtrack-speculation`` ``-mno-track-speculation``
  Enable or disable generation of additional code to track speculative
  execution through conditional branches.  The tracking state can then
  be used by the compiler when expanding calls to
  ``__builtin_speculation_safe_copy`` to permit a more efficient code
  sequence to be generated.

``-moutline-atomics`` ``-mno-outline-atomics``
  Enable or disable calls to out-of-line helpers to implement atomic operations.
  These helpers will, at runtime, determine if the LSE instructions from
  ARMv8.1-A can be used; if not, they will use the load/store-exclusive
  instructions that are present in the base ARMv8.0 ISA.

  This option is only applicable when compiling for the base ARMv8.0
  instruction set.  If using a later revision, e.g. :option:`-march=armv8.1-a`
  or :option:`-march=armv8-a+lse` , the ARMv8.1-Atomics instructions will be
  used directly.  The same applies when using :option:`-mcpu` = when the
  selected cpu supports the :samp:`lse` feature.
  This option is on by default.

.. option:: -march=name

  Specify the name of the target architecture and, optionally, one or
  more feature modifiers.  This option has the form
  :option:`-march`:samp:`={arch}` {+[no ]:samp:`{feature}` }*.

  The table below summarizes the permissible values for :samp:`{arch}`
  and the features that they enable by default:

  ====================  ============  =====================================================================
  :samp:`{arch}` value  Architecture  Includes by default
  ====================  ============  =====================================================================
  :samp:`armv8-a`       Armv8-A       :samp:`+fp`, :samp:`+simd`
  :samp:`armv8.1-a`     Armv8.1-A     :samp:`armv8-a`, :samp:`+crc`, :samp:`+lse`, :samp:`+rdma`
  :samp:`armv8.2-a`     Armv8.2-A     :samp:`armv8.1-a`
  :samp:`armv8.3-a`     Armv8.3-A     :samp:`armv8.2-a`, :samp:`+pauth`
  :samp:`armv8.4-a`     Armv8.4-A     :samp:`armv8.3-a`, :samp:`+flagm`, :samp:`+fp16fml`, :samp:`+dotprod`
  :samp:`armv8.5-a`     Armv8.5-A     :samp:`armv8.4-a`, :samp:`+sb`, :samp:`+ssbs`, :samp:`+predres`
  :samp:`armv8.6-a`     Armv8.6-A     :samp:`armv8.5-a`, :samp:`+bf16`, :samp:`+i8mm`
  :samp:`armv8-r`       Armv8-R       :samp:`armv8-r`
  ====================  ============  =====================================================================
  The value :samp:`native` is available on native AArch64 GNU/Linux and
  causes the compiler to pick the architecture of the host system.  This
  option has no effect if the compiler is unable to recognize the
  architecture of the host system,

  The permissible values for :samp:`{feature}` are listed in the sub-section
  on aarch64-feature-modifiers:option:`-march` and :option:`-mcpu`
  Feature Modifiers.  Where conflicting feature modifiers are
  specified, the right-most feature is used.

  GCC uses :samp:`{name}` to determine what kind of instructions it can emit
  when generating assembly code.  If :option:`-march` is specified
  without either of :option:`-mtune` or :option:`-mcpu` also being
  specified, the code is tuned to perform well across a range of target
  processors implementing the target architecture.

.. option:: -mtune=name

  Specify the name of the target processor for which GCC should tune the
  performance of the code.  Permissible values for this option are:
  :samp:`generic`, :samp:`cortex-a35`, :samp:`cortex-a53`, :samp:`cortex-a55`,
  :samp:`cortex-a57`, :samp:`cortex-a72`, :samp:`cortex-a73`, :samp:`cortex-a75`,
  :samp:`cortex-a76`, :samp:`cortex-a76ae`, :samp:`cortex-a77`,
  :samp:`cortex-a65`, :samp:`cortex-a65ae`, :samp:`cortex-a34`,
  :samp:`cortex-a78`, :samp:`cortex-a78ae`, :samp:`cortex-a78c`,
  :samp:`ares`, :samp:`exynos-m1`, :samp:`emag`, :samp:`falkor`,
  :samp:`neoverse-e1`, :samp:`neoverse-n1`, :samp:`neoverse-n2`,
  :samp:`neoverse-v1`, :samp:`qdf24xx`, :samp:`saphira`,
  :samp:`phecda`, :samp:`xgene1`, :samp:`vulcan`, :samp:`octeontx`,
  :samp:`octeontx81`,  :samp:`octeontx83`,
  :samp:`octeontx2`, :samp:`octeontx2t98`, :samp:`octeontx2t96`
  :samp:`octeontx2t93`, :samp:`octeontx2f95`, :samp:`octeontx2f95n`,
  :samp:`octeontx2f95mm`,
  :samp:`a64fx`,
  :samp:`thunderx`, :samp:`thunderxt88`,
  :samp:`thunderxt88p1`, :samp:`thunderxt81`, :samp:`tsv110`,
  :samp:`thunderxt83`, :samp:`thunderx2t99`, :samp:`thunderx3t110`, :samp:`zeus`,
  :samp:`cortex-a57.cortex-a53`, :samp:`cortex-a72.cortex-a53`,
  :samp:`cortex-a73.cortex-a35`, :samp:`cortex-a73.cortex-a53`,
  :samp:`cortex-a75.cortex-a55`, :samp:`cortex-a76.cortex-a55`,
  :samp:`cortex-r82`, :samp:`cortex-x1`, :samp:`native`.

  The values :samp:`cortex-a57.cortex-a53`, :samp:`cortex-a72.cortex-a53`,
  :samp:`cortex-a73.cortex-a35`, :samp:`cortex-a73.cortex-a53`,
  :samp:`cortex-a75.cortex-a55`, :samp:`cortex-a76.cortex-a55` specify that GCC
  should tune for a big.LITTLE system.

  Additionally on native AArch64 GNU/Linux systems the value
  :samp:`native` tunes performance to the host system.  This option has no effect
  if the compiler is unable to recognize the processor of the host system.

  Where none of :option:`-mtune` =, :option:`-mcpu` = or :option:`-march` =
  are specified, the code is tuned to perform well across a range
  of target processors.

  This option cannot be suffixed by feature modifiers.

.. option:: -mcpu=name

  Specify the name of the target processor, optionally suffixed by one
  or more feature modifiers.  This option has the form
  :option:`-mcpu`:samp:`={cpu}` {+[no ]:samp:`{feature}` }*, where
  the permissible values for :samp:`{cpu}` are the same as those available
  for :option:`-mtune`.  The permissible values for :samp:`{feature}` are
  documented in the sub-section on
  aarch64-feature-modifiers:option:`-march` and :option:`-mcpu`
  Feature Modifiers.  Where conflicting feature modifiers are
  specified, the right-most feature is used.

  GCC uses :samp:`{name}` to determine what kind of instructions it can emit when
  generating assembly code (as if by :option:`-march` ) and to determine
  the target processor for which to tune for performance (as if
  by :option:`-mtune` ).  Where this option is used in conjunction
  with :option:`-march` or :option:`-mtune` , those options take precedence
  over the appropriate part of this option.

.. option:: -moverride=string

  Override tuning decisions made by the back-end in response to a
  :option:`-mtune` = switch.  The syntax, semantics, and accepted values
  for :samp:`{string}` in this option are not guaranteed to be consistent
  across releases.

  This option is only intended to be useful when developing GCC.

.. option:: -mverbose-cost-dump

  Enable verbose cost model dumping in the debug dump files.  This option is
  provided for use in debugging the compiler.

.. option:: -mpc-relative-literal-loads, -mno-pc-relative-literal-loads

  Enable or disable PC-relative literal loads.  With this option literal pools are
  accessed using a single instruction and emitted after each function.  This
  limits the maximum size of functions to 1MB.  This is enabled by default for
  :option:`-mcmodel=tiny`.

.. option:: -msign-return-address=scope

  Select the function scope on which return address signing will be applied.
  Permissible values are :samp:`none`, which disables return address signing,
  :samp:`non-leaf`, which enables pointer signing for functions which are not leaf
  functions, and :samp:`all`, which enables pointer signing for all functions.  The
  default value is :samp:`none`. This option has been deprecated by
  -mbranch-protection.

.. option:: -mbranch-protection=none|standard|pac-ret[+leaf+b-key]|bti

  Select the branch protection features to use.
  :samp:`none` is the default and turns off all types of branch protection.
  :samp:`standard` turns on all types of branch protection features.  If a feature
  has additional tuning options, then :samp:`standard` sets it to its standard
  level.
  :samp:`pac-ret[+ :samp:`{leaf}` ]` turns on return address signing to its standard
  level: signing functions that save the return address to memory (non-leaf
  functions will practically always do this) using the a-key.  The optional
  argument :samp:`leaf` can be used to extend the signing to include leaf
  functions.  The optional argument :samp:`b-key` can be used to sign the functions
  with the B-key instead of the A-key.
  :samp:`bti` turns on branch target identification mechanism.

.. option:: -mharden-sls=opts

  Enable compiler hardening against straight line speculation (SLS).
  :samp:`{opts}` is a comma-separated list of the following options:

  :samp:`retbr`:samp:`blr`In addition, :samp:`-mharden-sls=all` enables all SLS hardening while
  :samp:`-mharden-sls=none` disables all SLS hardening.

.. option:: -msve-vector-bits=bits

  Specify the number of bits in an SVE vector register.  This option only has
  an effect when SVE is enabled.

  GCC supports two forms of SVE code generation: 'vector-length
  agnostic' output that works with any size of vector register and
  'vector-length specific' output that allows GCC to make assumptions
  about the vector length when it is useful for optimization reasons.
  The possible values of :samp:`bits` are: :samp:`scalable`, :samp:`128`,
  :samp:`256`, :samp:`512`, :samp:`1024` and :samp:`2048`.
  Specifying :samp:`scalable` selects vector-length agnostic
  output.  At present :samp:`-msve-vector-bits=128` also generates vector-length
  agnostic output for big-endian targets.  All other values generate
  vector-length specific code.  The behavior of these values may change
  in future releases and no value except :samp:`scalable` should be
  relied on for producing code that is portable across different
  hardware SVE vector lengths.

  The default is :samp:`-msve-vector-bits=scalable`, which produces
  vector-length agnostic code.

:option:`-march` and :option:`-mcpu` Feature Modifiers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _aarch64-feature-modifiers:

.. index:: -march feature modifiers

.. index:: -mcpu feature modifiers

Feature modifiers used with :option:`-march` and :option:`-mcpu` can be any of
the following and their inverses no :samp:`{feature}` :

:samp:`crc`
  Enable CRC extension.  This is on by default for
  :option:`-march=armv8.1-a`.

:samp:`crypto`
  Enable Crypto extension.  This also enables Advanced SIMD and floating-point
  instructions.

:samp:`fp`
  Enable floating-point instructions.  This is on by default for all possible
  values for options :option:`-march` and :option:`-mcpu`.

:samp:`simd`
  Enable Advanced SIMD instructions.  This also enables floating-point
  instructions.  This is on by default for all possible values for options
  :option:`-march` and :option:`-mcpu`.

:samp:`sve`
  Enable Scalable Vector Extension instructions.  This also enables Advanced
  SIMD and floating-point instructions.

:samp:`lse`
  Enable Large System Extension instructions.  This is on by default for
  :option:`-march=armv8.1-a`.

:samp:`rdma`
  Enable Round Double Multiply Accumulate instructions.  This is on by default
  for :option:`-march=armv8.1-a`.

:samp:`fp16`
  Enable FP16 extension.  This also enables floating-point instructions.

:samp:`fp16fml`
  Enable FP16 fmla extension.  This also enables FP16 extensions and
  floating-point instructions. This option is enabled by default for :option:`-march=armv8.4-a`. Use of this option with architectures prior to Armv8.2-A is not supported.

:samp:`rcpc`
  Enable the RcPc extension.  This does not change code generation from GCC,
  but is passed on to the assembler, enabling inline asm statements to use
  instructions from the RcPc extension.

:samp:`dotprod`
  Enable the Dot Product extension.  This also enables Advanced SIMD instructions.

:samp:`aes`
  Enable the Armv8-a aes and pmull crypto extension.  This also enables Advanced
  SIMD instructions.

:samp:`sha2`
  Enable the Armv8-a sha2 crypto extension.  This also enables Advanced SIMD instructions.

:samp:`sha3`
  Enable the sha512 and sha3 crypto extension.  This also enables Advanced SIMD
  instructions. Use of this option with architectures prior to Armv8.2-A is not supported.

:samp:`sm4`
  Enable the sm3 and sm4 crypto extension.  This also enables Advanced SIMD instructions.
  Use of this option with architectures prior to Armv8.2-A is not supported.

:samp:`profile`
  Enable the Statistical Profiling extension.  This option is only to enable the
  extension at the assembler level and does not affect code generation.

:samp:`rng`
  Enable the Armv8.5-a Random Number instructions.  This option is only to
  enable the extension at the assembler level and does not affect code
  generation.

:samp:`memtag`
  Enable the Armv8.5-a Memory Tagging Extensions.
  Use of this option with architectures prior to Armv8.5-A is not supported.

:samp:`sb`
  Enable the Armv8-a Speculation Barrier instruction.  This option is only to
  enable the extension at the assembler level and does not affect code
  generation.  This option is enabled by default for :option:`-march=armv8.5-a`.

:samp:`ssbs`
  Enable the Armv8-a Speculative Store Bypass Safe instruction.  This option
  is only to enable the extension at the assembler level and does not affect code
  generation.  This option is enabled by default for :option:`-march=armv8.5-a`.

:samp:`predres`
  Enable the Armv8-a Execution and Data Prediction Restriction instructions.
  This option is only to enable the extension at the assembler level and does
  not affect code generation.  This option is enabled by default for
  :option:`-march=armv8.5-a`.

:samp:`sve2`
  Enable the Armv8-a Scalable Vector Extension 2.  This also enables SVE
  instructions.

:samp:`sve2-bitperm`
  Enable SVE2 bitperm instructions.  This also enables SVE2 instructions.

:samp:`sve2-sm4`
  Enable SVE2 sm4 instructions.  This also enables SVE2 instructions.

:samp:`sve2-aes`
  Enable SVE2 aes instructions.  This also enables SVE2 instructions.

:samp:`sve2-sha3`
  Enable SVE2 sha3 instructions.  This also enables SVE2 instructions.

:samp:`tme`
  Enable the Transactional Memory Extension.

:samp:`i8mm`
  Enable 8-bit Integer Matrix Multiply instructions.  This also enables
  Advanced SIMD and floating-point instructions.  This option is enabled by
  default for :option:`-march=armv8.6-a`.  Use of this option with architectures
  prior to Armv8.2-A is not supported.

:samp:`f32mm`
  Enable 32-bit Floating point Matrix Multiply instructions.  This also enables
  SVE instructions.  Use of this option with architectures prior to Armv8.2-A is
  not supported.

:samp:`f64mm`
  Enable 64-bit Floating point Matrix Multiply instructions.  This also enables
  SVE instructions.  Use of this option with architectures prior to Armv8.2-A is
  not supported.

:samp:`bf16`
  Enable brain half-precision floating-point instructions.  This also enables
  Advanced SIMD and floating-point instructions.  This option is enabled by
  default for :option:`-march=armv8.6-a`.  Use of this option with architectures
  prior to Armv8.2-A is not supported.

:samp:`flagm`
  Enable the Flag Manipulation instructions Extension.

:samp:`pauth`
  Enable the Pointer Authentication Extension.

  Feature crypto implies aes , sha2 , and simd ,
which implies fp.
Conversely, nofp implies nosimd , which implies
nocrypto , noaes and nosha2.

.. _adapteva-epiphany-options:

Adapteva Epiphany Options
^^^^^^^^^^^^^^^^^^^^^^^^^

These :samp:`-m` options are defined for Adapteva Epiphany:

.. option:: -mhalf-reg-file

  Don't allocate any register in the range ``r32``...``r63``.
  That allows code to run on hardware variants that lack these registers.

.. option:: -mprefer-short-insn-regs

  Preferentially allocate registers that allow short instruction generation.
  This can result in increased instruction count, so this may either reduce or
  increase overall code size.

.. option:: -mbranch-cost=num

  Set the cost of branches to roughly :samp:`{num}` 'simple' instructions.
  This cost is only a heuristic and is not guaranteed to produce
  consistent results across releases.

.. option:: -mcmove

  Enable the generation of conditional moves.

.. option:: -mnops=num

  Emit :samp:`{num}` NOPs before every other generated instruction.

.. option:: -mno-soft-cmpsf, -msoft-cmpsf

  For single-precision floating-point comparisons, emit an ``fsub`` instruction
  and test the flags.  This is faster than a software comparison, but can
  get incorrect results in the presence of NaNs, or when two different small
  numbers are compared such that their difference is calculated as zero.
  The default is :option:`-msoft-cmpsf` , which uses slower, but IEEE-compliant,
  software comparisons.

.. option:: -mstack-offset=num

  Set the offset between the top of the stack and the stack pointer.
  E.g., a value of 8 means that the eight bytes in the range ``sp+0...sp+7``
  can be used by leaf functions without stack allocation.
  Values other than :samp:`8` or :samp:`16` are untested and unlikely to work.
  Note also that this option changes the ABI; compiling a program with a
  different stack offset than the libraries have been compiled with
  generally does not work.
  This option can be useful if you want to evaluate if a different stack
  offset would give you better code, but to actually use a different stack
  offset to build working programs, it is recommended to configure the
  toolchain with the appropriate :option:`--with-stack-offset`:samp:`={num}` option.

.. option:: -mno-round-nearest, -mround-nearest

  Make the scheduler assume that the rounding mode has been set to
  truncating.  The default is :option:`-mround-nearest`.

.. option:: -mlong-calls

  If not otherwise specified by an attribute, assume all calls might be beyond
  the offset range of the ``b`` / ``bl`` instructions, and therefore load the
  function address into a register before performing a (otherwise direct) call.
  This is the default.

.. option:: -mshort-calls, -short-calls

  If not otherwise specified by an attribute, assume all direct calls are
  in the range of the ``b`` / ``bl`` instructions, so use these instructions
  for direct calls.  The default is :option:`-mlong-calls`.

.. option:: -msmall16

  Assume addresses can be loaded as 16-bit unsigned values.  This does not
  apply to function addresses for which :option:`-mlong-calls` semantics
  are in effect.

.. option:: -mfp-mode=mode

  Set the prevailing mode of the floating-point unit.
  This determines the floating-point mode that is provided and expected
  at function call and return time.  Making this mode match the mode you
  predominantly need at function start can make your programs smaller and
  faster by avoiding unnecessary mode switches.

  :samp:`{mode}` can be set to one the following values:

  :samp:`caller`
    Any mode at function entry is valid, and retained or restored when
    the function returns, and when it calls other functions.
    This mode is useful for compiling libraries or other compilation units
    you might want to incorporate into different programs with different
    prevailing FPU modes, and the convenience of being able to use a single
    object file outweighs the size and speed overhead for any extra
    mode switching that might be needed, compared with what would be needed
    with a more specific choice of prevailing FPU mode.

  :samp:`truncate`
    This is the mode used for floating-point calculations with
    truncating (i.e. round towards zero) rounding mode.  That includes
    conversion from floating point to integer.

  :samp:`round-nearest`
    This is the mode used for floating-point calculations with
    round-to-nearest-or-even rounding mode.

  :samp:`int`
    This is the mode used to perform integer calculations in the FPU, e.g.
    integer multiply, or integer multiply-and-accumulate.

    The default is :option:`-mfp-mode=caller`

.. option:: -mno-split-lohi, -msplit-lohi, -mno-postinc, -mpostinc, -mno-postmodify
.. option:: -mpostmodify

  Code generation tweaks that disable, respectively, splitting of 32-bit
  loads, generation of post-increment addresses, and generation of
  post-modify addresses.  The defaults are msplit-lohi ,
  :option:`-mpost-inc` , and :option:`-mpost-modify`.

.. option:: -mnovect-double, -mno-vect-double, -mvect-double

  Change the preferred SIMD mode to SImode.  The default is
  :option:`-mvect-double` , which uses DImode as preferred SIMD mode.

.. option:: -max-vect-align=num

  The maximum alignment for SIMD vector mode types.
  :samp:`{num}` may be 4 or 8.  The default is 8.
  Note that this is an ABI change, even though many library function
  interfaces are unaffected if they don't use SIMD vector modes
  in places that affect size and/or alignment of relevant types.

.. option:: -msplit-vecmove-early

  Split vector moves into single word moves before reload.  In theory this
  can give better register allocation, but so far the reverse seems to be
  generally the case.

.. option:: -m1reg-reg, -m1reg-

  Specify a register to hold the constant -1, which makes loading small negative
  constants and certain bitmasks faster.
  Allowable values for :samp:`{reg}` are :samp:`r43` and :samp:`r63`,
  which specify use of that register as a fixed register,
  and :samp:`none`, which means that no register is used for this
  purpose.  The default is :option:`-m1reg-none`.

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

.. option:: -mstack-size=bytes

  Specify how many :samp:`{bytes}` of stack space will be requested for each GPU
  thread (wave-front).  Beware that there may be many threads and limited memory
  available.  The size of the stack allocation may also have an impact on
  run-time performance.  The default is 32KB when using OpenACC or OpenMP, and
  1MB otherwise.

.. _arc-options:

ARC Options
^^^^^^^^^^^

.. index:: ARC options

The following options control the architecture variant for which code
is being compiled:

.. architecture variants

.. option:: -mbarrel-shifter

  Generate instructions supported by barrel shifter.  This is the default
  unless :option:`-mcpu=ARC601` or :samp:`-mcpu=ARCEM` is in effect.

.. option:: -mjli-always, -mjli-alawys

  Force to call a function using jli_s instruction.  This option is
  valid only for ARCv2 architecture.

.. option:: -mcpu=cpu

  Set architecture type, register usage, and instruction scheduling
  parameters for :samp:`{cpu}`.  There are also shortcut alias options
  available for backward compatibility and convenience.  Supported
  values for :samp:`{cpu}` are

  .. index:: mA6

  .. index:: mARC600

  :samp:`arc600`
    Compile for ARC600.  Aliases: :option:`-mA6` , :option:`-mARC600`.

  .. option:: arc601, -mARC601

    Compile for ARC601.  Alias: :option:`-mARC601`.

  .. option:: arc700, -mA7, -mARC700

    Compile for ARC700.  Aliases: :option:`-mA7` , :option:`-mARC700`.
    This is the default when configured with :option:`--with-cpu=arc700`.

  :samp:`arcem`
    Compile for ARC EM.

  :samp:`archs`
    Compile for ARC HS.

  :samp:`em`
    Compile for ARC EM CPU with no hardware extensions.

  :samp:`em4`
    Compile for ARC EM4 CPU.

  :samp:`em4_dmips`
    Compile for ARC EM4 DMIPS CPU.

  :samp:`em4_fpus`
    Compile for ARC EM4 DMIPS CPU with the single-precision floating-point
    extension.

  :samp:`em4_fpuda`
    Compile for ARC EM4 DMIPS CPU with single-precision floating-point and
    double assist instructions.

  :samp:`hs`
    Compile for ARC HS CPU with no hardware extensions except the atomic
    instructions.

  :samp:`hs34`
    Compile for ARC HS34 CPU.

  :samp:`hs38`
    Compile for ARC HS38 CPU.

  :samp:`hs38_linux`
    Compile for ARC HS38 CPU with all hardware extensions on.

  :samp:`arc600_norm`
    Compile for ARC 600 CPU with ``norm`` instructions enabled.

  :samp:`arc600_mul32x16`
    Compile for ARC 600 CPU with ``norm`` and 32x16-bit multiply 
    instructions enabled.

  :samp:`arc600_mul64`
    Compile for ARC 600 CPU with ``norm`` and ``mul64``-family 
    instructions enabled.

  :samp:`arc601_norm`
    Compile for ARC 601 CPU with ``norm`` instructions enabled.

  :samp:`arc601_mul32x16`
    Compile for ARC 601 CPU with ``norm`` and 32x16-bit multiply
    instructions enabled.

  :samp:`arc601_mul64`
    Compile for ARC 601 CPU with ``norm`` and ``mul64``-family
    instructions enabled.

  :samp:`nps400`
    Compile for ARC 700 on NPS400 chip.

  :samp:`em_mini`
    Compile for ARC EM minimalist configuration featuring reduced register
    set.

.. option:: -mdpfp, -mdpfp-compact

  Generate double-precision FPX instructions, tuned for the compact
  implementation.

.. option:: -mdpfp-fast

  Generate double-precision FPX instructions, tuned for the fast
  implementation.

.. option:: -mno-dpfp-lrsr

  Disable ``lr`` and ``sr`` instructions from using FPX extension
  aux registers.

.. option:: -mea

  Generate extended arithmetic instructions.  Currently only
  ``divaw``, ``adds``, ``subs``, and ``sat16`` are
  supported.  Only valid for :option:`-mcpu=ARC700`.

.. option:: -mno-mpy, -mmpy

  Do not generate ``mpy``-family instructions for ARC700.  This option is
  deprecated.

.. option:: -mmul32x16

  Generate 32x16-bit multiply and multiply-accumulate instructions.

.. option:: -mmul64

  Generate ``mul64`` and ``mulu64`` instructions.  
  Only valid for :option:`-mcpu=ARC600`.

.. option:: -mnorm

  Generate ``norm`` instructions.  This is the default if :option:`-mcpu=ARC700`
  is in effect.

.. option:: -mspfp, -mspfp-compact

  Generate single-precision FPX instructions, tuned for the compact
  implementation.

.. option:: -mspfp-fast

  Generate single-precision FPX instructions, tuned for the fast
  implementation.

.. option:: -msimd

  Enable generation of ARC SIMD instructions via target-specific
  builtins.  Only valid for :option:`-mcpu=ARC700`.

.. option:: -msoft-float

  This option ignored; it is provided for compatibility purposes only.
  Software floating-point code is emitted by default, and this default
  can overridden by FPX options; :option:`-mspfp` , :option:`-mspfp-compact` , or
  :option:`-mspfp-fast` for single precision, and :option:`-mdpfp` ,
  :option:`-mdpfp-compact` , or :option:`-mdpfp-fast` for double precision.

.. option:: -mswap

  Generate ``swap`` instructions.

.. option:: -matomic

  This enables use of the locked load/store conditional extension to implement
  atomic memory built-in functions.  Not available for ARC 6xx or ARC
  EM cores.

.. option:: -mdiv-rem

  Enable ``div`` and ``rem`` instructions for ARCv2 cores.

.. option:: -mcode-density

  Enable code density instructions for ARC EM.  
  This option is on by default for ARC HS.

.. option:: -mll64

  Enable double load/store operations for ARC HS cores.

.. option:: -mtp-regno=regno

  Specify thread pointer register number.

.. option:: -mmpy-option=multo

  Compile ARCv2 code with a multiplier design option.  You can specify 
  the option using either a string or numeric value for :samp:`{multo}`.  
  :samp:`wlh1` is the default value.  The recognized values are:

  :samp:`0` :samp:`none`
    No multiplier available.

  :samp:`1` :samp:`w`
    16x16 multiplier, fully pipelined.
    The following instructions are enabled: ``mpyw`` and ``mpyuw``.

  :samp:`2` :samp:`wlh1`
    32x32 multiplier, fully
    pipelined (1 stage).  The following instructions are additionally
    enabled: ``mpy``, ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`3` :samp:`wlh2`
    32x32 multiplier, fully pipelined
    (2 stages).  The following instructions are additionally enabled: ``mpy``,
    ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`4` :samp:`wlh3`
    Two 16x16 multipliers, blocking,
    sequential.  The following instructions are additionally enabled: ``mpy``,
    ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`5` :samp:`wlh4`
    One 16x16 multiplier, blocking,
    sequential.  The following instructions are additionally enabled: ``mpy``,
    ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`6` :samp:`wlh5`
    One 32x4 multiplier, blocking,
    sequential.  The following instructions are additionally enabled: ``mpy``,
    ``mpyu``, ``mpym``, ``mpymu``, and ``mpy_s``.

  :samp:`7` :samp:`plus_dmpy`
    ARC HS SIMD support.

  :samp:`8` :samp:`plus_macd`
    ARC HS SIMD support.

  :samp:`9` :samp:`plus_qmacw`
    ARC HS SIMD support.

    This option is only available for ARCv2 cores.

.. option:: -mfpu=fpu

  Enables support for specific floating-point hardware extensions for ARCv2
  cores.  Supported values for :samp:`{fpu}` are:

  :samp:`fpus`
    Enables support for single-precision floating-point hardware
    extensions.

  :samp:`fpud`
    Enables support for double-precision floating-point hardware
    extensions.  The single-precision floating-point extension is also
    enabled.  Not available for ARC EM.

  :samp:`fpuda`
    Enables support for double-precision floating-point hardware
    extensions using double-precision assist instructions.  The single-precision
    floating-point extension is also enabled.  This option is
    only available for ARC EM.

  :samp:`fpuda_div`
    Enables support for double-precision floating-point hardware
    extensions using double-precision assist instructions.
    The single-precision floating-point, square-root, and divide 
    extensions are also enabled.  This option is
    only available for ARC EM.

  :samp:`fpuda_fma`
    Enables support for double-precision floating-point hardware
    extensions using double-precision assist instructions.
    The single-precision floating-point and fused multiply and add 
    hardware extensions are also enabled.  This option is
    only available for ARC EM.

  :samp:`fpuda_all`
    Enables support for double-precision floating-point hardware
    extensions using double-precision assist instructions.
    All single-precision floating-point hardware extensions are also
    enabled.  This option is only available for ARC EM.

  :samp:`fpus_div`
    Enables support for single-precision floating-point, square-root and divide 
    hardware extensions.

  :samp:`fpud_div`
    Enables support for double-precision floating-point, square-root and divide 
    hardware extensions.  This option
    includes option :samp:`fpus_div`. Not available for ARC EM.

  :samp:`fpus_fma`
    Enables support for single-precision floating-point and 
    fused multiply and add hardware extensions.

  :samp:`fpud_fma`
    Enables support for double-precision floating-point and 
    fused multiply and add hardware extensions.  This option
    includes option :samp:`fpus_fma`.  Not available for ARC EM.

  :samp:`fpus_all`
    Enables support for all single-precision floating-point hardware
    extensions.

  :samp:`fpud_all`
    Enables support for all single- and double-precision floating-point
    hardware extensions.  Not available for ARC EM.

.. option:: -mirq-ctrl-saved=register-range, blink, lp_count

  Specifies general-purposes registers that the processor automatically
  saves/restores on interrupt entry and exit.  :samp:`{register-range}` is
  specified as two registers separated by a dash.  The register range
  always starts with ``r0``, the upper limit is ``fp`` register.
  :samp:`{blink}` and :samp:`{lp_count}` are optional.  This option is only
  valid for ARC EM and ARC HS cores.

.. option:: -mrgf-banked-regs=number

  Specifies the number of registers replicated in second register bank
  on entry to fast interrupt.  Fast interrupts are interrupts with the
  highest priority level P0.  These interrupts save only PC and STATUS32
  registers to avoid memory transactions during interrupt entry and exit
  sequences.  Use this option when you are using fast interrupts in an
  ARC V2 family processor.  Permitted values are 4, 8, 16, and 32.

.. option:: -mlpc-width=width

  Specify the width of the ``lp_count`` register.  Valid values for
  :samp:`{width}` are 8, 16, 20, 24, 28 and 32 bits.  The default width is
  fixed to 32 bits.  If the width is less than 32, the compiler does not
  attempt to transform loops in your program to use the zero-delay loop
  mechanism unless it is known that the ``lp_count`` register can
  hold the required loop-counter value.  Depending on the width
  specified, the compiler and run-time library might continue to use the
  loop mechanism for various needs.  This option defines macro
  ``__ARC_LPC_WIDTH__`` with the value of :samp:`{width}`.

.. option:: -mrf16

  This option instructs the compiler to generate code for a 16-entry
  register file.  This option defines the ``__ARC_RF16__``
  preprocessor macro.

.. option:: -mbranch-index

  Enable use of ``bi`` or ``bih`` instructions to implement jump
  tables.

The following options are passed through to the assembler, and also
define preprocessor macro symbols.

.. Flags used by the assembler, but for which we define preprocessor
   macro symbols as well.

.. option:: -mdsp-packa

  Passed down to the assembler to enable the DSP Pack A extensions.
  Also sets the preprocessor symbol ``__Xdsp_packa``.  This option is
  deprecated.

.. option:: -mdvbf

  Passed down to the assembler to enable the dual Viterbi butterfly
  extension.  Also sets the preprocessor symbol ``__Xdvbf``.  This
  option is deprecated.

  .. ARC700 4.10 extension instruction

.. option:: -mlock

  Passed down to the assembler to enable the locked load/store
  conditional extension.  Also sets the preprocessor symbol
  ``__Xlock``.

.. option:: -mmac-d16

  Passed down to the assembler.  Also sets the preprocessor symbol
  ``__Xxmac_d16``.  This option is deprecated.

.. option:: -mmac-24

  Passed down to the assembler.  Also sets the preprocessor symbol
  ``__Xxmac_24``.  This option is deprecated.

  .. ARC700 4.10 extension instruction

.. option:: -mrtsc

  Passed down to the assembler to enable the 64-bit time-stamp counter
  extension instruction.  Also sets the preprocessor symbol
  ``__Xrtsc``.  This option is deprecated.

  .. ARC700 4.10 extension instruction

.. option:: -mswape

  Passed down to the assembler to enable the swap byte ordering
  extension instruction.  Also sets the preprocessor symbol
  ``__Xswape``.

.. option:: -mtelephony

  Passed down to the assembler to enable dual- and single-operand
  instructions for telephony.  Also sets the preprocessor symbol
  ``__Xtelephony``.  This option is deprecated.

.. option:: -mxy

  Passed down to the assembler to enable the XY memory extension.  Also
  sets the preprocessor symbol ``__Xxy``.

The following options control how the assembly code is annotated:

.. Assembly annotation options

.. option:: -misize

  Annotate assembler instructions with estimated addresses.

.. option:: -mannotate-align

  Explain what alignment considerations lead to the decision to make an
  instruction short or long.

The following options are passed through to the linker:

.. options passed through to the linker

.. option:: -marclinux

  Passed through to the linker, to specify use of the ``arclinux`` emulation.
  This option is enabled by default in tool chains built for
  ``arc-linux-uclibc`` and ``arceb-linux-uclibc`` targets
  when profiling is not requested.

.. option:: -marclinux_prof

  Passed through to the linker, to specify use of the
  ``arclinux_prof`` emulation.  This option is enabled by default in
  tool chains built for ``arc-linux-uclibc`` and
  ``arceb-linux-uclibc`` targets when profiling is requested.

The following options control the semantics of generated code:

.. semantically relevant code generation options

.. option:: -mlong-calls

  Generate calls as register indirect calls, thus providing access
  to the full 32-bit address range.

.. option:: -mmedium-calls

  Don't use less than 25-bit addressing range for calls, which is the
  offset available for an unconditional branch-and-link
  instruction.  Conditional execution of function calls is suppressed, to
  allow use of the 25-bit range, rather than the 21-bit range with
  conditional branch-and-link.  This is the default for tool chains built
  for ``arc-linux-uclibc`` and ``arceb-linux-uclibc`` targets.

.. option:: -G num, -G

  Put definitions of externally-visible data in a small data section if
  that data is no bigger than :samp:`{num}` bytes.  The default value of
  :samp:`{num}` is 4 for any ARC configuration, or 8 when we have double
  load/store operations.

.. option:: -mno-sdata, -msdata

  Do not generate sdata references.  This is the default for tool chains
  built for ``arc-linux-uclibc`` and ``arceb-linux-uclibc``
  targets.

.. option:: -mvolatile-cache

  Use ordinarily cached memory accesses for volatile references.  This is the
  default.

.. option:: -mno-volatile-cache, -mvolatile-cache

  Enable cache bypass for volatile references.

The following options fine tune code generation:

.. code generation tuning options

.. option:: -malign-call

  Do alignment optimizations for call instructions.

.. option:: -mauto-modify-reg

  Enable the use of pre/post modify with register displacement.

.. option:: -mbbit-peephole

  Enable bbit peephole2.

.. option:: -mno-brcc

  This option disables a target-specific pass in arc_reorg to
  generate compare-and-branch (``brcc``) instructions.  
  It has no effect on
  generation of these instructions driven by the combiner pass.

.. option:: -mcase-vector-pcrel

  Use PC-relative switch case tables to enable case table shortening.
  This is the default for :option:`-Os`.

.. option:: -mcompact-casesi

  Enable compact ``casesi`` pattern.  This is the default for :option:`-Os` ,
  and only available for ARCv1 cores.  This option is deprecated.

.. option:: -mno-cond-exec

  Disable the ARCompact-specific pass to generate conditional 
  execution instructions.

  Due to delay slot scheduling and interactions between operand numbers,
  literal sizes, instruction lengths, and the support for conditional execution,
  the target-independent pass to generate conditional execution is often lacking,
  so the ARC port has kept a special pass around that tries to find more
  conditional execution generation opportunities after register allocation,
  branch shortening, and delay slot scheduling have been done.  This pass
  generally, but not always, improves performance and code size, at the cost of
  extra compilation time, which is why there is an option to switch it off.
  If you have a problem with call instructions exceeding their allowable
  offset range because they are conditionalized, you should consider using
  :option:`-mmedium-calls` instead.

.. option:: -mearly-cbranchsi

  Enable pre-reload use of the ``cbranchsi`` pattern.

.. option:: -mexpand-adddi

  Expand ``adddi3`` and ``subdi3`` at RTL generation time into
  ``add.f``, ``adc`` etc.  This option is deprecated.

.. option:: -mindexed-loads

  Enable the use of indexed loads.  This can be problematic because some
  optimizers then assume that indexed stores exist, which is not
  the case.

.. option:: -mlra

  Enable Local Register Allocation.  This is still experimental for ARC,
  so by default the compiler uses standard reload
  (i.e. :option:`-mno-lra` ).

.. option:: -mlra-priority-none

  Don't indicate any priority for target registers.

.. option:: -mlra-priority-compact

  Indicate target register priority for r0..r3 / r12..r15.

.. option:: -mlra-priority-noncompact

  Reduce target register priority for r0..r3 / r12..r15.

.. option:: -mmillicode

  When optimizing for size (using :option:`-Os` ), prologues and epilogues
  that have to save or restore a large number of registers are often
  shortened by using call to a special function in libgcc; this is
  referred to as a *millicode* call.  As these calls can pose
  performance issues, and/or cause linking issues when linking in a
  nonstandard way, this option is provided to turn on or off millicode
  call generation.

.. option:: -mcode-density-frame

  This option enable the compiler to emit ``enter`` and ``leave``
  instructions.  These instructions are only valid for CPUs with
  code-density feature.

.. option:: -mmixed-code

  Tweak register allocation to help 16-bit instruction generation.
  This generally has the effect of decreasing the average instruction size
  while increasing the instruction count.

.. option:: -mq-class

  Ths option is deprecated.  Enable :samp:`q` instruction alternatives.
  This is the default for :option:`-Os`.

.. option:: -mRcq

  Enable :samp:`Rcq` constraint handling.  
  Most short code generation depends on this.
  This is the default.

.. option:: -mRcw

  Enable :samp:`Rcw` constraint handling.  
  Most ccfsm condexec mostly depends on this.
  This is the default.

.. option:: -msize-level=level

  Fine-tune size optimization with regards to instruction lengths and alignment.
  The recognized values for :samp:`{level}` are:

  :samp:`0`
    No size optimization.  This level is deprecated and treated like :samp:`1`.

  :samp:`1`
    Short instructions are used opportunistically.

  :samp:`2`
    In addition, alignment of loops and of code after barriers are dropped.

  :samp:`3`
    In addition, optional data alignment is dropped, and the option Os is enabled.

    This defaults to :samp:`3` when :option:`-Os` is in effect.  Otherwise,
  the behavior when this is not set is equivalent to level :samp:`1`.

.. option:: -mtune=cpu

  Set instruction scheduling parameters for :samp:`{cpu}` , overriding any implied
  by :option:`-mcpu` =.

  Supported values for :samp:`{cpu}` are

  :samp:`ARC600`
    Tune for ARC600 CPU.

  :samp:`ARC601`
    Tune for ARC601 CPU.

  :samp:`ARC700`
    Tune for ARC700 CPU with standard multiplier block.

  :samp:`ARC700-xmac`
    Tune for ARC700 CPU with XMAC block.

  :samp:`ARC725D`
    Tune for ARC725D CPU.

  :samp:`ARC750D`
    Tune for ARC750D CPU.

.. option:: -mmultcost=num

  Cost to assume for a multiply instruction, with :samp:`4` being equal to a
  normal instruction.

.. option:: -munalign-prob-threshold=probability

  Set probability threshold for unaligning branches.
  When tuning for :samp:`ARC700` and optimizing for speed, branches without
  filled delay slot are preferably emitted unaligned and long, unless
  profiling indicates that the probability for the branch to be taken
  is below :samp:`{probability}`.  See :ref:`cross-profiling`.
  The default is (REG_BR_PROB_BASE/2), i.e. 5000.

The following options are maintained for backward compatibility, but
are now deprecated and will be removed in a future release:

.. Deprecated options

.. option:: -margonaut

  Obsolete FPX.

.. option:: -mbig-endian, -EB

  Compile code for big-endian targets.  Use of these options is now
  deprecated.  Big-endian code is supported by configuring GCC to build
  ``arceb-elf32`` and ``arceb-linux-uclibc`` targets,
  for which big endian is the default.

.. option:: -mlittle-endian, -EL

  Compile code for little-endian targets.  Use of these options is now
  deprecated.  Little-endian code is supported by configuring GCC to build 
  ``arc-elf32`` and ``arc-linux-uclibc`` targets,
  for which little endian is the default.

.. option:: -mbarrel_shifter

  Replaced by :option:`-mbarrel-shifter`.

.. option:: -mdpfp_compact

  Replaced by :option:`-mdpfp-compact`.

.. option:: -mdpfp_fast

  Replaced by :option:`-mdpfp-fast`.

.. option:: -mdsp_packa

  Replaced by :option:`-mdsp-packa`.

.. option:: -mEA

  Replaced by :option:`-mea`.

.. option:: -mmac_24

  Replaced by :option:`-mmac-24`.

.. option:: -mmac_d16

  Replaced by :option:`-mmac-d16`.

.. option:: -mspfp_compact

  Replaced by :option:`-mspfp-compact`.

.. option:: -mspfp_fast

  Replaced by :option:`-mspfp-fast`.

.. option:: -mtune=cpu

  Values :samp:`arc600`, :samp:`arc601`, :samp:`arc700` and
  :samp:`arc700-xmac` for :samp:`{cpu}` are replaced by :samp:`ARC600`,
  :samp:`ARC601`, :samp:`ARC700` and :samp:`ARC700-xmac` respectively.

.. option:: -multcost=num

  Replaced by :option:`-mmultcost`.

.. _arm-options:

ARM Options
^^^^^^^^^^^

.. index:: ARM options

These :samp:`-m` options are defined for the ARM port:

.. option:: -mabi=name

  Generate code for the specified ABI.  Permissible values are: :samp:`apcs-gnu`,
  :samp:`atpcs`, :samp:`aapcs`, :samp:`aapcs-linux` and :samp:`iwmmxt`.

.. option:: -mapcs-frame

  Generate a stack frame that is compliant with the ARM Procedure Call
  Standard for all functions, even if this is not strictly necessary for
  correct execution of the code.  Specifying :option:`-fomit-frame-pointer`
  with this option causes the stack frames not to be generated for
  leaf functions.  The default is :option:`-mno-apcs-frame`.
  This option is deprecated.

.. option:: -mapcs

  This is a synonym for :option:`-mapcs-frame` and is deprecated.

  @c not currently implemented
  @item -mapcs-stack-check
  @opindex mapcs-stack-check
  Generate code to check the amount of stack space available upon entry to
  every function (that actually uses some stack space).  If there is
  insufficient space available then either the function
  @code{__rt_stkovf_split_small} or @code{__rt_stkovf_split_big} is
  called, depending upon the amount of stack space required.  The runtime
  system is required to provide these functions.  The default is
  @option{-mno-apcs-stack-check}, since this produces smaller code.

  @c not currently implemented
  @item -mapcs-reentrant
  @opindex mapcs-reentrant
  Generate reentrant, position-independent code.  The default is
  @option{-mno-apcs-reentrant}.

.. option:: -mthumb-interwork

  Generate code that supports calling between the ARM and Thumb
  instruction sets.  Without this option, on pre-v5 architectures, the
  two instruction sets cannot be reliably used inside one program.  The
  default is :option:`-mno-thumb-interwork` , since slightly larger code
  is generated when :option:`-mthumb-interwork` is specified.  In AAPCS
  configurations this option is meaningless.

.. option:: -mno-sched-prolog, -msched-prolog

  Prevent the reordering of instructions in the function prologue, or the
  merging of those instruction with the instructions in the function's
  body.  This means that all functions start with a recognizable set
  of instructions (or in fact one of a choice from a small set of
  different function prologues), and this information can be used to
  locate the start of functions inside an executable piece of code.  The
  default is :option:`-msched-prolog`.

.. option:: -mfloat-abi=name

  Specifies which floating-point ABI to use.  Permissible values
  are: :samp:`soft`, :samp:`softfp` and :samp:`hard`.

  Specifying :samp:`soft` causes GCC to generate output containing
  library calls for floating-point operations.
  :samp:`softfp` allows the generation of code using hardware floating-point
  instructions, but still uses the soft-float calling conventions.
  :samp:`hard` allows generation of floating-point instructions
  and uses FPU-specific calling conventions.

  The default depends on the specific target configuration.  Note that
  the hard-float and soft-float ABIs are not link-compatible; you must
  compile your entire program with the same ABI, and link with a
  compatible set of libraries.

.. option:: -mgeneral-regs-only

  Generate code which uses only the general-purpose registers.  This will prevent
  the compiler from using floating-point and Advanced SIMD registers but will not
  impose any restrictions on the assembler.

.. option:: -mlittle-endian

  Generate code for a processor running in little-endian mode.  This is
  the default for all standard configurations.

.. option:: -mbig-endian

  Generate code for a processor running in big-endian mode; the default is
  to compile code for a little-endian processor.

.. option:: -mbe8

  When linking a big-endian image select between BE8 and BE32 formats.
  The option has no effect for little-endian images and is ignored.  The
  default is dependent on the selected target architecture.  For ARMv6
  and later architectures the default is BE8, for older architectures
  the default is BE32.  BE32 format has been deprecated by ARM.

.. option:: -march=name[+extension...]

  This specifies the name of the target ARM architecture.  GCC uses this
  name to determine what kind of instructions it can emit when generating
  assembly code.  This option can be used in conjunction with or instead
  of the :option:`-mcpu` = option.

  Permissible names are:
  :samp:`armv4t`,
  :samp:`armv5t`, :samp:`armv5te`,
  :samp:`armv6`, :samp:`armv6j`, :samp:`armv6k`, :samp:`armv6kz`, :samp:`armv6t2`,
  :samp:`armv6z`, :samp:`armv6zk`,
  :samp:`armv7`, :samp:`armv7-a`, :samp:`armv7ve`, 
  :samp:`armv8-a`, :samp:`armv8.1-a`, :samp:`armv8.2-a`, :samp:`armv8.3-a`,
  :samp:`armv8.4-a`,
  :samp:`armv8.5-a`,
  :samp:`armv8.6-a`,
  :samp:`armv7-r`,
  :samp:`armv8-r`,
  :samp:`armv6-m`, :samp:`armv6s-m`,
  :samp:`armv7-m`, :samp:`armv7e-m`,
  :samp:`armv8-m.base`, :samp:`armv8-m.main`,
  :samp:`armv8.1-m.main`,
  :samp:`iwmmxt` and :samp:`iwmmxt2`.

  Additionally, the following architectures, which lack support for the
  Thumb execution state, are recognized but support is deprecated: :samp:`armv4`.

  Many of the architectures support extensions.  These can be added by
  appending :samp:`+ :samp:`{extension}`` to the architecture name.  Extension
  options are processed in order and capabilities accumulate.  An extension
  will also enable any necessary base extensions
  upon which it depends.  For example, the :samp:`+crypto` extension
  will always enable the :samp:`+simd` extension.  The exception to the
  additive construction is for extensions that are prefixed with
  :samp:`+no...`: these extensions disable the specified option and
  any other extensions that may depend on the presence of that
  extension.

  For example, :samp:`-march=armv7-a+simd+nofp+vfpv4` is equivalent to
  writing :samp:`-march=armv7-a+vfpv4` since the :samp:`+simd` option is
  entirely disabled by the :samp:`+nofp` option that follows it.

  Most extension names are generically named, but have an effect that is
  dependent upon the architecture to which it is applied.  For example,
  the :samp:`+simd` option can be applied to both :samp:`armv7-a` and
  :samp:`armv8-a` architectures, but will enable the original ARMv7-A
  Advanced SIMD (Neon) extensions for :samp:`armv7-a` and the ARMv8-A
  variant for :samp:`armv8-a`.

  The table below lists the supported extensions for each architecture.
  Architectures not mentioned do not support any extensions.

  :samp:`armv5te` :samp:`armv6` :samp:`armv6j` :samp:`armv6k` :samp:`armv6kz` :samp:`armv6t2` :samp:`armv6z` :samp:`armv6zk`

    :samp:`+fp`
      The VFPv2 floating-point instructions.  The extension :samp:`+vfpv2` can be
      used as an alias for this extension.

    :samp:`+nofp`
      Disable the floating-point instructions.

  :samp:`armv7`
    The common subset of the ARMv7-A, ARMv7-R and ARMv7-M architectures.

    :samp:`+fp`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers.  The extension :samp:`+vfpv3-d16` can be used as an alias
      for this extension.  Note that floating-point is not supported by the
      base ARMv7-M architecture, but is compatible with both the ARMv7-A and
      ARMv7-R architectures.

    :samp:`+nofp`
      Disable the floating-point instructions.

  :samp:`armv7-a`

    :samp:`+mp`
      The multiprocessing extension.

    :samp:`+sec`
      The security extension.

    :samp:`+fp`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers.  The extension :samp:`+vfpv3-d16` can be used as an alias
      for this extension.

    :samp:`+simd`
      The Advanced SIMD (Neon) v1 and the VFPv3 floating-point instructions.
      The extensions :samp:`+neon` and :samp:`+neon-vfpv3` can be used as aliases
      for this extension.

    :samp:`+vfpv3`
      The VFPv3 floating-point instructions, with 32 double-precision
      registers.

    :samp:`+vfpv3-d16-fp16`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv3-fp16`
      The VFPv3 floating-point instructions, with 32 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv4-d16`
      The VFPv4 floating-point instructions, with 16 double-precision
      registers.

    :samp:`+vfpv4`
      The VFPv4 floating-point instructions, with 32 double-precision
      registers.

    :samp:`+neon-fp16`
      The Advanced SIMD (Neon) v1 and the VFPv3 floating-point instructions, with
      the half-precision floating-point conversion operations.

    :samp:`+neon-vfpv4`
      The Advanced SIMD (Neon) v2 and the VFPv4 floating-point instructions.

    :samp:`+nosimd`
      Disable the Advanced SIMD instructions (does not disable floating point).

    :samp:`+nofp`
      Disable the floating-point and Advanced SIMD instructions.

  :samp:`armv7ve`
    The extended version of the ARMv7-A architecture with support for
    virtualization.

    :samp:`+fp`
      The VFPv4 floating-point instructions, with 16 double-precision registers.
      The extension :samp:`+vfpv4-d16` can be used as an alias for this extension.

    :samp:`+simd`
      The Advanced SIMD (Neon) v2 and the VFPv4 floating-point instructions.  The
      extension :samp:`+neon-vfpv4` can be used as an alias for this extension.

    :samp:`+vfpv3-d16`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers.

    :samp:`+vfpv3`
      The VFPv3 floating-point instructions, with 32 double-precision
      registers.

    :samp:`+vfpv3-d16-fp16`
      The VFPv3 floating-point instructions, with 16 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv3-fp16`
      The VFPv3 floating-point instructions, with 32 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv4-d16`
      The VFPv4 floating-point instructions, with 16 double-precision
      registers.

    :samp:`+vfpv4`
      The VFPv4 floating-point instructions, with 32 double-precision
      registers.

    :samp:`+neon`
      The Advanced SIMD (Neon) v1 and the VFPv3 floating-point instructions.
      The extension :samp:`+neon-vfpv3` can be used as an alias for this extension.

    :samp:`+neon-fp16`
      The Advanced SIMD (Neon) v1 and the VFPv3 floating-point instructions, with
      the half-precision floating-point conversion operations.

    :samp:`+nosimd`
      Disable the Advanced SIMD instructions (does not disable floating point).

    :samp:`+nofp`
      Disable the floating-point and Advanced SIMD instructions.

  :samp:`armv8-a`

    :samp:`+crc`
      The Cyclic Redundancy Check (CRC) instructions.

    :samp:`+simd`
      The ARMv8-A Advanced SIMD and floating-point instructions.

    :samp:`+crypto`
      The cryptographic instructions.

    :samp:`+nocrypto`
      Disable the cryptographic instructions.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+sb`
      Speculation Barrier Instruction.

    :samp:`+predres`
      Execution and Data Prediction Restriction Instructions.

  :samp:`armv8.1-a`

    :samp:`+simd`
      The ARMv8.1-A Advanced SIMD and floating-point instructions.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions.

    :samp:`+nocrypto`
      Disable the cryptographic instructions.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+sb`
      Speculation Barrier Instruction.

    :samp:`+predres`
      Execution and Data Prediction Restriction Instructions.

  :samp:`armv8.2-a` :samp:`armv8.3-a`

    :samp:`+fp16`
      The half-precision floating-point data processing instructions.
      This also enables the Advanced SIMD and floating-point instructions.

    :samp:`+fp16fml`
      The half-precision floating-point fmla extension.  This also enables
      the half-precision floating-point extension and Advanced SIMD and
      floating-point instructions.

    :samp:`+simd`
      The ARMv8.1-A Advanced SIMD and floating-point instructions.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions.

    :samp:`+dotprod`
      Enable the Dot Product extension.  This also enables Advanced SIMD instructions.

    :samp:`+nocrypto`
      Disable the cryptographic extension.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+sb`
      Speculation Barrier Instruction.

    :samp:`+predres`
      Execution and Data Prediction Restriction Instructions.

    :samp:`+i8mm`
      8-bit Integer Matrix Multiply instructions.
      This also enables Advanced SIMD and floating-point instructions.

    :samp:`+bf16`
      Brain half-precision floating-point instructions.
      This also enables Advanced SIMD and floating-point instructions.

  :samp:`armv8.4-a`

    :samp:`+fp16`
      The half-precision floating-point data processing instructions.
      This also enables the Advanced SIMD and floating-point instructions as well
      as the Dot Product extension and the half-precision floating-point fmla
      extension.

    :samp:`+simd`
      The ARMv8.3-A Advanced SIMD and floating-point instructions as well as the
      Dot Product extension.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions as well as the Dot Product extension.

    :samp:`+nocrypto`
      Disable the cryptographic extension.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+sb`
      Speculation Barrier Instruction.

    :samp:`+predres`
      Execution and Data Prediction Restriction Instructions.

    :samp:`+i8mm`
      8-bit Integer Matrix Multiply instructions.
      This also enables Advanced SIMD and floating-point instructions.

    :samp:`+bf16`
      Brain half-precision floating-point instructions.
      This also enables Advanced SIMD and floating-point instructions.

  :samp:`armv8.5-a`

    :samp:`+fp16`
      The half-precision floating-point data processing instructions.
      This also enables the Advanced SIMD and floating-point instructions as well
      as the Dot Product extension and the half-precision floating-point fmla
      extension.

    :samp:`+simd`
      The ARMv8.3-A Advanced SIMD and floating-point instructions as well as the
      Dot Product extension.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions as well as the Dot Product extension.

    :samp:`+nocrypto`
      Disable the cryptographic extension.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+i8mm`
      8-bit Integer Matrix Multiply instructions.
      This also enables Advanced SIMD and floating-point instructions.

    :samp:`+bf16`
      Brain half-precision floating-point instructions.
      This also enables Advanced SIMD and floating-point instructions.

  :samp:`armv8.6-a`

    :samp:`+fp16`
      The half-precision floating-point data processing instructions.
      This also enables the Advanced SIMD and floating-point instructions as well
      as the Dot Product extension and the half-precision floating-point fmla
      extension.

    :samp:`+simd`
      The ARMv8.3-A Advanced SIMD and floating-point instructions as well as the
      Dot Product extension.

    :samp:`+crypto`
      The cryptographic instructions.  This also enables the Advanced SIMD and
      floating-point instructions as well as the Dot Product extension.

    :samp:`+nocrypto`
      Disable the cryptographic extension.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

    :samp:`+i8mm`
      8-bit Integer Matrix Multiply instructions.
      This also enables Advanced SIMD and floating-point instructions.

    :samp:`+bf16`
      Brain half-precision floating-point instructions.
      This also enables Advanced SIMD and floating-point instructions.

  :samp:`armv7-r`

    :samp:`+fp.sp`
      The single-precision VFPv3 floating-point instructions.  The extension
      :samp:`+vfpv3xd` can be used as an alias for this extension.

    :samp:`+fp`
      The VFPv3 floating-point instructions with 16 double-precision registers.
      The extension +vfpv3-d16 can be used as an alias for this extension.

    :samp:`+vfpv3xd-d16-fp16`
      The single-precision VFPv3 floating-point instructions with 16 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+vfpv3-d16-fp16`
      The VFPv3 floating-point instructions with 16 double-precision
      registers and the half-precision floating-point conversion operations.

    :samp:`+nofp`
      Disable the floating-point extension.

    :samp:`+idiv`
      The ARM-state integer division instructions.

    :samp:`+noidiv`
      Disable the ARM-state integer division extension.

  :samp:`armv7e-m`

    :samp:`+fp`
      The single-precision VFPv4 floating-point instructions.

    :samp:`+fpv5`
      The single-precision FPv5 floating-point instructions.

    :samp:`+fp.dp`
      The single- and double-precision FPv5 floating-point instructions.

    :samp:`+nofp`
      Disable the floating-point extensions.

  :samp:`armv8.1-m.main`

    :samp:`+dsp`
      The DSP instructions.

    :samp:`+mve`
      The M-Profile Vector Extension (MVE) integer instructions.

    :samp:`+mve.fp`
      The M-Profile Vector Extension (MVE) integer and single precision
      floating-point instructions.

    :samp:`+fp`
      The single-precision floating-point instructions.

    :samp:`+fp.dp`
      The single- and double-precision floating-point instructions.

    :samp:`+nofp`
      Disable the floating-point extension.

    :samp:`+cdecp0, +cdecp1, ... , +cdecp7`
      Enable the Custom Datapath Extension (CDE) on selected coprocessors according
      to the numbers given in the options in the range 0 to 7.

  :samp:`armv8-m.main`

    :samp:`+dsp`
      The DSP instructions.

    :samp:`+nodsp`
      Disable the DSP extension.

    :samp:`+fp`
      The single-precision floating-point instructions.

    :samp:`+fp.dp`
      The single- and double-precision floating-point instructions.

    :samp:`+nofp`
      Disable the floating-point extension.

    :samp:`+cdecp0, +cdecp1, ... , +cdecp7`
      Enable the Custom Datapath Extension (CDE) on selected coprocessors according
      to the numbers given in the options in the range 0 to 7.

  :samp:`armv8-r`

    :samp:`+crc`
      The Cyclic Redundancy Check (CRC) instructions.

    :samp:`+fp.sp`
      The single-precision FPv5 floating-point instructions.

    :samp:`+simd`
      The ARMv8-A Advanced SIMD and floating-point instructions.

    :samp:`+crypto`
      The cryptographic instructions.

    :samp:`+nocrypto`
      Disable the cryptographic instructions.

    :samp:`+nofp`
      Disable the floating-point, Advanced SIMD and cryptographic instructions.

      :option:`-march=native` causes the compiler to auto-detect the architecture
  of the build computer.  At present, this feature is only supported on
  GNU/Linux, and not all architectures are recognized.  If the auto-detect
  is unsuccessful the option has no effect.

.. option:: -mtune=name

  This option specifies the name of the target ARM processor for
  which GCC should tune the performance of the code.
  For some ARM implementations better performance can be obtained by using
  this option.
  Permissible names are: :samp:`arm7tdmi`, :samp:`arm7tdmi-s`, :samp:`arm710t`,
  :samp:`arm720t`, :samp:`arm740t`, :samp:`strongarm`, :samp:`strongarm110`,
  :samp:`strongarm1100`, 0:samp:`strongarm1110`, :samp:`arm8`, :samp:`arm810`,
  :samp:`arm9`, :samp:`arm9e`, :samp:`arm920`, :samp:`arm920t`, :samp:`arm922t`,
  :samp:`arm946e-s`, :samp:`arm966e-s`, :samp:`arm968e-s`, :samp:`arm926ej-s`,
  :samp:`arm940t`, :samp:`arm9tdmi`, :samp:`arm10tdmi`, :samp:`arm1020t`,
  :samp:`arm1026ej-s`, :samp:`arm10e`, :samp:`arm1020e`, :samp:`arm1022e`,
  :samp:`arm1136j-s`, :samp:`arm1136jf-s`, :samp:`mpcore`, :samp:`mpcorenovfp`,
  :samp:`arm1156t2-s`, :samp:`arm1156t2f-s`, :samp:`arm1176jz-s`, :samp:`arm1176jzf-s`,
  :samp:`generic-armv7-a`, :samp:`cortex-a5`, :samp:`cortex-a7`, :samp:`cortex-a8`,
  :samp:`cortex-a9`, :samp:`cortex-a12`, :samp:`cortex-a15`, :samp:`cortex-a17`,
  :samp:`cortex-a32`, :samp:`cortex-a35`, :samp:`cortex-a53`, :samp:`cortex-a55`,
  :samp:`cortex-a57`, :samp:`cortex-a72`, :samp:`cortex-a73`, :samp:`cortex-a75`,
  :samp:`cortex-a76`, :samp:`cortex-a76ae`, :samp:`cortex-a77`,
  :samp:`cortex-a78`, :samp:`cortex-a78ae`, :samp:`cortex-a78c`,
  :samp:`ares`, :samp:`cortex-r4`, :samp:`cortex-r4f`,
  :samp:`cortex-r5`, :samp:`cortex-r7`, :samp:`cortex-r8`, :samp:`cortex-r52`,
  :samp:`cortex-m0`, :samp:`cortex-m0plus`, :samp:`cortex-m1`, :samp:`cortex-m3`,
  :samp:`cortex-m4`, :samp:`cortex-m7`, :samp:`cortex-m23`, :samp:`cortex-m33`,
  :samp:`cortex-m35p`, :samp:`cortex-m55`, :samp:`cortex-x1`,
  :samp:`cortex-m1.small-multiply`, :samp:`cortex-m0.small-multiply`,
  :samp:`cortex-m0plus.small-multiply`, :samp:`exynos-m1`, :samp:`marvell-pj4`,
  :samp:`neoverse-n1`, :samp:`neoverse-n2`, :samp:`neoverse-v1`, :samp:`xscale`,
  :samp:`iwmmxt`, :samp:`iwmmxt2`, :samp:`ep9312`, :samp:`fa526`, :samp:`fa626`,
  :samp:`fa606te`, :samp:`fa626te`, :samp:`fmp626`, :samp:`fa726te`, :samp:`xgene1`.

  Additionally, this option can specify that GCC should tune the performance
  of the code for a big.LITTLE system.  Permissible names are:
  :samp:`cortex-a15.cortex-a7`, :samp:`cortex-a17.cortex-a7`,
  :samp:`cortex-a57.cortex-a53`, :samp:`cortex-a72.cortex-a53`,
  :samp:`cortex-a72.cortex-a35`, :samp:`cortex-a73.cortex-a53`,
  :samp:`cortex-a75.cortex-a55`, :samp:`cortex-a76.cortex-a55`.

  :option:`-mtune=generic-`:samp:`{arch}` specifies that GCC should tune the
  performance for a blend of processors within architecture :samp:`{arch}`.
  The aim is to generate code that run well on the current most popular
  processors, balancing between optimizations that benefit some CPUs in the
  range, and avoiding performance pitfalls of other CPUs.  The effects of
  this option may change in future GCC versions as CPU models come and go.

  :option:`-mtune` permits the same extension options as :option:`-mcpu` , but
  the extension options do not affect the tuning of the generated code.

  :option:`-mtune=native` causes the compiler to auto-detect the CPU
  of the build computer.  At present, this feature is only supported on
  GNU/Linux, and not all architectures are recognized.  If the auto-detect is
  unsuccessful the option has no effect.

.. option:: -mcpu=name[+extension...]

  This specifies the name of the target ARM processor.  GCC uses this name
  to derive the name of the target ARM architecture (as if specified
  by :option:`-march` ) and the ARM processor type for which to tune for
  performance (as if specified by :option:`-mtune` ).  Where this option
  is used in conjunction with :option:`-march` or :option:`-mtune` ,
  those options take precedence over the appropriate part of this option.

  Many of the supported CPUs implement optional architectural
  extensions.  Where this is so the architectural extensions are
  normally enabled by default.  If implementations that lack the
  extension exist, then the extension syntax can be used to disable
  those extensions that have been omitted.  For floating-point and
  Advanced SIMD (Neon) instructions, the settings of the options
  :option:`-mfloat-abi` and :option:`-mfpu` must also be considered:
  floating-point and Advanced SIMD instructions will only be used if
  :option:`-mfloat-abi` is not set to :samp:`soft`; and any setting of
  :option:`-mfpu` other than :samp:`auto` will override the available
  floating-point and SIMD extension instructions.

  For example, :samp:`cortex-a9` can be found in three major
  configurations: integer only, with just a floating-point unit or with
  floating-point and Advanced SIMD.  The default is to enable all the
  instructions, but the extensions :samp:`+nosimd` and :samp:`+nofp` can
  be used to disable just the SIMD or both the SIMD and floating-point
  instructions respectively.

  Permissible names for this option are the same as those for
  :option:`-mtune`.

  The following extension options are common to the listed CPUs:

  :samp:`+nodsp`
    Disable the DSP instructions on :samp:`cortex-m33`, :samp:`cortex-m35p`.

  :samp:`+nofp`
    Disables the floating-point instructions on :samp:`arm9e`,
    :samp:`arm946e-s`, :samp:`arm966e-s`, :samp:`arm968e-s`, :samp:`arm10e`,
    :samp:`arm1020e`, :samp:`arm1022e`, :samp:`arm926ej-s`,
    :samp:`arm1026ej-s`, :samp:`cortex-r5`, :samp:`cortex-r7`, :samp:`cortex-r8`,
    :samp:`cortex-m4`, :samp:`cortex-m7`, :samp:`cortex-m33` and :samp:`cortex-m35p`.
    Disables the floating-point and SIMD instructions on
    :samp:`generic-armv7-a`, :samp:`cortex-a5`, :samp:`cortex-a7`,
    :samp:`cortex-a8`, :samp:`cortex-a9`, :samp:`cortex-a12`,
    :samp:`cortex-a15`, :samp:`cortex-a17`, :samp:`cortex-a15.cortex-a7`,
    :samp:`cortex-a17.cortex-a7`, :samp:`cortex-a32`, :samp:`cortex-a35`,
    :samp:`cortex-a53` and :samp:`cortex-a55`.

  :samp:`+nofp.dp`
    Disables the double-precision component of the floating-point instructions
    on :samp:`cortex-r5`, :samp:`cortex-r7`, :samp:`cortex-r8`, :samp:`cortex-r52` and
    :samp:`cortex-m7`.

  :samp:`+nosimd`
    Disables the SIMD (but not floating-point) instructions on
    :samp:`generic-armv7-a`, :samp:`cortex-a5`, :samp:`cortex-a7`
    and :samp:`cortex-a9`.

  :samp:`+crypto`
    Enables the cryptographic instructions on :samp:`cortex-a32`,
    :samp:`cortex-a35`, :samp:`cortex-a53`, :samp:`cortex-a55`, :samp:`cortex-a57`,
    :samp:`cortex-a72`, :samp:`cortex-a73`, :samp:`cortex-a75`, :samp:`exynos-m1`,
    :samp:`xgene1`, :samp:`cortex-a57.cortex-a53`, :samp:`cortex-a72.cortex-a53`,
    :samp:`cortex-a73.cortex-a35`, :samp:`cortex-a73.cortex-a53` and
    :samp:`cortex-a75.cortex-a55`.

    Additionally the :samp:`generic-armv7-a` pseudo target defaults to
  VFPv3 with 16 double-precision registers.  It supports the following
  extension options: :samp:`mp`, :samp:`sec`, :samp:`vfpv3-d16`,
  :samp:`vfpv3`, :samp:`vfpv3-d16-fp16`, :samp:`vfpv3-fp16`,
  :samp:`vfpv4-d16`, :samp:`vfpv4`, :samp:`neon`, :samp:`neon-vfpv3`,
  :samp:`neon-fp16`, :samp:`neon-vfpv4`.  The meanings are the same as for
  the extensions to :option:`-march=armv7-a`.

  :option:`-mcpu=generic-`:samp:`{arch}` is also permissible, and is
  equivalent to :option:`-march`:samp:`={arch}` -mtune=generic- :samp:`{arch}`.
  See :option:`-mtune` for more information.

  :option:`-mcpu=native` causes the compiler to auto-detect the CPU
  of the build computer.  At present, this feature is only supported on
  GNU/Linux, and not all architectures are recognized.  If the auto-detect
  is unsuccessful the option has no effect.

.. option:: -mfpu=name

  This specifies what floating-point hardware (or hardware emulation) is
  available on the target.  Permissible names are: :samp:`auto`, :samp:`vfpv2`,
  :samp:`vfpv3`,
  :samp:`vfpv3-fp16`, :samp:`vfpv3-d16`, :samp:`vfpv3-d16-fp16`, :samp:`vfpv3xd`,
  :samp:`vfpv3xd-fp16`, :samp:`neon-vfpv3`, :samp:`neon-fp16`, :samp:`vfpv4`,
  :samp:`vfpv4-d16`, :samp:`fpv4-sp-d16`, :samp:`neon-vfpv4`,
  :samp:`fpv5-d16`, :samp:`fpv5-sp-d16`,
  :samp:`fp-armv8`, :samp:`neon-fp-armv8` and :samp:`crypto-neon-fp-armv8`.
  Note that :samp:`neon` is an alias for :samp:`neon-vfpv3` and :samp:`vfp`
  is an alias for :samp:`vfpv2`.

  The setting :samp:`auto` is the default and is special.  It causes the
  compiler to select the floating-point and Advanced SIMD instructions
  based on the settings of :option:`-mcpu` and :option:`-march`.

  If the selected floating-point hardware includes the NEON extension
  (e.g. :option:`-mfpu=neon` ), note that floating-point
  operations are not generated by GCC's auto-vectorization pass unless
  :option:`-funsafe-math-optimizations` is also specified.  This is
  because NEON hardware does not fully implement the IEEE 754 standard for
  floating-point arithmetic (in particular denormal values are treated as
  zero), so the use of NEON instructions may lead to a loss of precision.

  You can also set the fpu name at function level by using the ``target("fpu=")`` function attributes (see :ref:`arm-function-attributes`) or pragmas (see :ref:`function-specific-option-pragmas`).

.. option:: -mfp16-format=name

  Specify the format of the ``__fp16`` half-precision floating-point type.
  Permissible names are :samp:`none`, :samp:`ieee`, and :samp:`alternative`;
  the default is :samp:`none`, in which case the ``__fp16`` type is not
  defined.  See :ref:`half-precision`, for more information.

.. option:: -mstructure-size-boundary=n

  The sizes of all structures and unions are rounded up to a multiple
  of the number of bits set by this option.  Permissible values are 8, 32
  and 64.  The default value varies for different toolchains.  For the COFF
  targeted toolchain the default value is 8.  A value of 64 is only allowed
  if the underlying ABI supports it.

  Specifying a larger number can produce faster, more efficient code, but
  can also increase the size of the program.  Different values are potentially
  incompatible.  Code compiled with one value cannot necessarily expect to
  work with code or libraries compiled with another value, if they exchange
  information using structures or unions.

  This option is deprecated.

.. option:: -mabort-on-noreturn

  Generate a call to the function ``abort`` at the end of a
  ``noreturn`` function.  It is executed if the function tries to
  return.

.. option:: -mlong-calls, -mno-long-calls

  Tells the compiler to perform function calls by first loading the
  address of the function into a register and then performing a subroutine
  call on this register.  This switch is needed if the target function
  lies outside of the 64-megabyte addressing range of the offset-based
  version of subroutine call instruction.

  Even if this switch is enabled, not all function calls are turned
  into long calls.  The heuristic is that static functions, functions
  that have the ``short_call`` attribute, functions that are inside
  the scope of a ``#pragma no_long_calls`` directive, and functions whose
  definitions have already been compiled within the current compilation
  unit are not turned into long calls.  The exceptions to this rule are
  that weak function definitions, functions with the ``long_call``
  attribute or the ``section`` attribute, and functions that are within
  the scope of a ``#pragma long_calls`` directive are always
  turned into long calls.

  This feature is not enabled by default.  Specifying
  :option:`-mno-long-calls` restores the default behavior, as does
  placing the function calls within the scope of a ``#pragma
  long_calls_off`` directive.  Note these switches have no effect on how
  the compiler generates code to handle function calls via function
  pointers.

.. option:: -msingle-pic-base

  Treat the register used for PIC addressing as read-only, rather than
  loading it in the prologue for each function.  The runtime system is
  responsible for initializing this register with an appropriate value
  before execution begins.

.. option:: -mpic-register=reg

  Specify the register to be used for PIC addressing.
  For standard PIC base case, the default is any suitable register
  determined by compiler.  For single PIC base case, the default is
  :samp:`R9` if target is EABI based or stack-checking is enabled,
  otherwise the default is :samp:`R10`.

.. option:: -mpic-data-is-text-relative

  Assume that the displacement between the text and data segments is fixed
  at static link time.  This permits using PC-relative addressing
  operations to access data known to be in the data segment.  For
  non-VxWorks RTP targets, this option is enabled by default.  When
  disabled on such targets, it will enable :option:`-msingle-pic-base` by
  default.

.. option:: -mpoke-function-name

  Write the name of each function into the text section, directly
  preceding the function prologue.  The generated code is similar to this:

  .. code-block:: c++

         t0
             .ascii "arm_poke_function_name", 0
             .align
         t1
             .word 0xff000000 + (t1 - t0)
         arm_poke_function_name
             mov     ip, sp
             stmfd   sp!, {fp, ip, lr, pc}
             sub     fp, ip, #4

  When performing a stack backtrace, code can inspect the value of
  ``pc`` stored at ``fp + 0``.  If the trace function then looks at
  location ``pc - 12`` and the top 8 bits are set, then we know that
  there is a function name embedded immediately preceding this location
  and has length ``((pc[-3]) & 0xff000000)``.

.. option:: -mthumb, -marm

  Select between generating code that executes in ARM and Thumb
  states.  The default for most configurations is to generate code
  that executes in ARM state, but the default can be changed by
  configuring GCC with the :option:`--with-mode`:samp:`={state}`
  configure option.

  You can also override the ARM and Thumb mode for each function
  by using the ``target("thumb")`` and ``target("arm")`` function attributes
  (see :ref:`arm-function-attributes`) or pragmas (see :ref:`function-specific-option-pragmas`).

.. option:: -mflip-thumb , -mflip-thumb

  Switch ARM/Thumb modes on alternating functions.
  This option is provided for regression testing of mixed Thumb/ARM code
  generation, and is not intended for ordinary use in compiling code.

.. option:: -mtpcs-frame

  Generate a stack frame that is compliant with the Thumb Procedure Call
  Standard for all non-leaf functions.  (A leaf function is one that does
  not call any other functions.)  The default is :option:`-mno-tpcs-frame`.

.. option:: -mtpcs-leaf-frame

  Generate a stack frame that is compliant with the Thumb Procedure Call
  Standard for all leaf functions.  (A leaf function is one that does
  not call any other functions.)  The default is :option:`-mno-apcs-leaf-frame`.

.. option:: -mcallee-super-interworking

  Gives all externally visible functions in the file being compiled an ARM
  instruction set header which switches to Thumb mode before executing the
  rest of the function.  This allows these functions to be called from
  non-interworking code.  This option is not valid in AAPCS configurations
  because interworking is enabled by default.

.. option:: -mcaller-super-interworking

  Allows calls via function pointers (including virtual functions) to
  execute correctly regardless of whether the target code has been
  compiled for interworking or not.  There is a small overhead in the cost
  of executing a function pointer if this option is enabled.  This option
  is not valid in AAPCS configurations because interworking is enabled
  by default.

.. option:: -mtp=name

  Specify the access model for the thread local storage pointer.  The valid
  models are :samp:`soft`, which generates calls to ``__aeabi_read_tp``,
  :samp:`cp15`, which fetches the thread pointer from ``cp15`` directly
  (supported in the arm6k architecture), and :samp:`auto`, which uses the
  best available method for the selected processor.  The default setting is
  :samp:`auto`.

.. option:: -mtls-dialect=dialect

  Specify the dialect to use for accessing thread local storage.  Two
  :samp:`{dialect}` s are supported-:samp:`gnu` and :samp:`gnu2`.  The
  :samp:`gnu` dialect selects the original GNU scheme for supporting
  local and global dynamic TLS models.  The :samp:`gnu2` dialect
  selects the GNU descriptor scheme, which provides better performance
  for shared libraries.  The GNU descriptor scheme is compatible with
  the original scheme, but does require new assembler, linker and
  library support.  Initial and local exec TLS models are unaffected by
  this option and always use the original scheme.

.. option:: -mword-relocations

  Only generate absolute relocations on word-sized values (i.e. R_ARM_ABS32).
  This is enabled by default on targets (uClinux, SymbianOS) where the runtime
  loader imposes this restriction, and when :option:`-fpic` or :option:`-fPIC`
  is specified. This option conflicts with :option:`-mslow-flash-data`.

.. option:: -mfix-cortex-m3-ldrd

  Some Cortex-M3 cores can cause data corruption when ``ldrd`` instructions
  with overlapping destination and base registers are used.  This option avoids
  generating these instructions.  This option is enabled by default when
  :option:`-mcpu=cortex-m3` is specified.

.. option:: -munaligned-access, -mno-unaligned-access

  Enables (or disables) reading and writing of 16- and 32- bit values
  from addresses that are not 16- or 32- bit aligned.  By default
  unaligned access is disabled for all pre-ARMv6, all ARMv6-M and for
  ARMv8-M Baseline architectures, and enabled for all other
  architectures.  If unaligned access is not enabled then words in packed
  data structures are accessed a byte at a time.

  The ARM attribute ``Tag_CPU_unaligned_access`` is set in the
  generated object file to either true or false, depending upon the
  setting of this option.  If unaligned access is enabled then the
  preprocessor symbol ``__ARM_FEATURE_UNALIGNED`` is also
  defined.

.. option:: -mneon-for-64bits

  This option is deprecated and has no effect.

.. option:: -mslow-flash-data

  Assume loading data from flash is slower than fetching instruction.
  Therefore literal load is minimized for better performance.
  This option is only supported when compiling for ARMv7 M-profile and
  off by default. It conflicts with :option:`-mword-relocations`.

.. option:: -masm-syntax-unified

  Assume inline assembler is using unified asm syntax.  The default is
  currently off which implies divided syntax.  This option has no impact
  on Thumb2. However, this may change in future releases of GCC.
  Divided syntax should be considered deprecated.

.. option:: -mrestrict-it

  Restricts generation of IT blocks to conform to the rules of ARMv8-A.
  IT blocks can only contain a single 16-bit instruction from a select
  set of instructions. This option is on by default for ARMv8-A Thumb mode.

.. option:: -mprint-tune-info

  Print CPU tuning information as comment in assembler file.  This is
  an option used only for regression testing of the compiler and not
  intended for ordinary use in compiling code.  This option is disabled
  by default.

.. option:: -mverbose-cost-dump

  Enable verbose cost model dumping in the debug dump files.  This option is
  provided for use in debugging the compiler.

.. option:: -mpure-code

  Do not allow constant data to be placed in code sections.
  Additionally, when compiling for ELF object format give all text sections the
  ELF processor-specific section attribute ``SHF_ARM_PURECODE``.  This option
  is only available when generating non-pic code for M-profile targets.

.. option:: -mcmse

  Generate secure code as per the "ARMv8-M Security Extensions: Requirements on
  Development Tools Engineering Specification", which can be found on
  https://developer.arm.com/documentation/ecm0359818/latest/.

.. option:: -mfdpic, -mno-fdpic

  Select the FDPIC ABI, which uses 64-bit function descriptors to
  represent pointers to functions.  When the compiler is configured for
  ``arm-*-uclinuxfdpiceabi`` targets, this option is on by default
  and implies :option:`-fPIE` if none of the PIC/PIE-related options is
  provided.  On other targets, it only enables the FDPIC-specific code
  generation features, and the user should explicitly provide the
  PIC/PIE-related options as needed.

  Note that static linking is not supported because it would still
  involve the dynamic linker when the program self-relocates.  If such
  behavior is acceptable, use -static and -Wl,-dynamic-linker options.

  The opposite :option:`-mno-fdpic` option is useful (and required) to
  build the Linux kernel using the same (``arm-*-uclinuxfdpiceabi``)
  toolchain as the one used to build the userland programs.

.. _avr-options:

AVR Options
^^^^^^^^^^^

.. index:: AVR Options

These options are defined for AVR implementations:

.. option:: -mmcu=mcu

  Specify Atmel AVR instruction set architectures (ISA) or MCU type.

  The default for this option is :samp:`avr2`.

  GCC supports the following AVR devices and ISAs:

  .. Copyright (C) 2012-2021 Free Software Foundation, Inc.
     This is part of the GCC manual.
     For copying conditions, see the file gcc/doc/include/fdl.texi.
     This file is generated automatically using
     gcc/config/avr/gen-avr-mmcu-texi.c from:
      c	 gcc/config/avr/avr-arch.h
      c	 gcc/config/avr/avr-devices.c
      c	 gcc/config/avr/avr-mcus.def
     Please do not edit manually.

  ``avr2``
    'Classic' devices with up to 8 KiB of program memory.

    :samp:`{mcu}` = ``attiny22``, ``attiny26``, ``at90s2313``, ``at90s2323``, ``at90s2333``, ``at90s2343``, ``at90s4414``, ``at90s4433``, ``at90s4434``, ``at90c8534``, ``at90s8515``, ``at90s8535``.

  ``avr25``
    'Classic' devices with up to 8 KiB of program memory and with the ``MOVW`` instruction.

    :samp:`{mcu}` = ``attiny13``, ``attiny13a``, ``attiny24``, ``attiny24a``, ``attiny25``, ``attiny261``, ``attiny261a``, ``attiny2313``, ``attiny2313a``, ``attiny43u``, ``attiny44``, ``attiny44a``, ``attiny45``, ``attiny48``, ``attiny441``, ``attiny461``, ``attiny461a``, ``attiny4313``, ``attiny84``, ``attiny84a``, ``attiny85``, ``attiny87``, ``attiny88``, ``attiny828``, ``attiny841``, ``attiny861``, ``attiny861a``, ``ata5272``, ``ata6616c``, ``at86rf401``.

  ``avr3``
    'Classic' devices with 16 KiB up to 64 KiB of program memory.

    :samp:`{mcu}` = ``at76c711``, ``at43usb355``.

  ``avr31``
    'Classic' devices with 128 KiB of program memory.

    :samp:`{mcu}` = ``atmega103``, ``at43usb320``.

  ``avr35``
    'Classic' devices with 16 KiB up to 64 KiB of program memory and with the ``MOVW`` instruction.

    :samp:`{mcu}` = ``attiny167``, ``attiny1634``, ``atmega8u2``, ``atmega16u2``, ``atmega32u2``, ``ata5505``, ``ata6617c``, ``ata664251``, ``at90usb82``, ``at90usb162``.

  ``avr4``
    'Enhanced' devices with up to 8 KiB of program memory.

    :samp:`{mcu}` = ``atmega48``, ``atmega48a``, ``atmega48p``, ``atmega48pa``, ``atmega48pb``, ``atmega8``, ``atmega8a``, ``atmega8hva``, ``atmega88``, ``atmega88a``, ``atmega88p``, ``atmega88pa``, ``atmega88pb``, ``atmega8515``, ``atmega8535``, ``ata6285``, ``ata6286``, ``ata6289``, ``ata6612c``, ``at90pwm1``, ``at90pwm2``, ``at90pwm2b``, ``at90pwm3``, ``at90pwm3b``, ``at90pwm81``.

  ``avr5``
    'Enhanced' devices with 16 KiB up to 64 KiB of program memory.

    :samp:`{mcu}` = ``atmega16``, ``atmega16a``, ``atmega16hva``, ``atmega16hva2``, ``atmega16hvb``, ``atmega16hvbrevb``, ``atmega16m1``, ``atmega16u4``, ``atmega161``, ``atmega162``, ``atmega163``, ``atmega164a``, ``atmega164p``, ``atmega164pa``, ``atmega165``, ``atmega165a``, ``atmega165p``, ``atmega165pa``, ``atmega168``, ``atmega168a``, ``atmega168p``, ``atmega168pa``, ``atmega168pb``, ``atmega169``, ``atmega169a``, ``atmega169p``, ``atmega169pa``, ``atmega32``, ``atmega32a``, ``atmega32c1``, ``atmega32hvb``, ``atmega32hvbrevb``, ``atmega32m1``, ``atmega32u4``, ``atmega32u6``, ``atmega323``, ``atmega324a``, ``atmega324p``, ``atmega324pa``, ``atmega325``, ``atmega325a``, ``atmega325p``, ``atmega325pa``, ``atmega328``, ``atmega328p``, ``atmega328pb``, ``atmega329``, ``atmega329a``, ``atmega329p``, ``atmega329pa``, ``atmega3250``, ``atmega3250a``, ``atmega3250p``, ``atmega3250pa``, ``atmega3290``, ``atmega3290a``, ``atmega3290p``, ``atmega3290pa``, ``atmega406``, ``atmega64``, ``atmega64a``, ``atmega64c1``, ``atmega64hve``, ``atmega64hve2``, ``atmega64m1``, ``atmega64rfr2``, ``atmega640``, ``atmega644``, ``atmega644a``, ``atmega644p``, ``atmega644pa``, ``atmega644rfr2``, ``atmega645``, ``atmega645a``, ``atmega645p``, ``atmega649``, ``atmega649a``, ``atmega649p``, ``atmega6450``, ``atmega6450a``, ``atmega6450p``, ``atmega6490``, ``atmega6490a``, ``atmega6490p``, ``ata5795``, ``ata5790``, ``ata5790n``, ``ata5791``, ``ata6613c``, ``ata6614q``, ``ata5782``, ``ata5831``, ``ata8210``, ``ata8510``, ``ata5702m322``, ``at90pwm161``, ``at90pwm216``, ``at90pwm316``, ``at90can32``, ``at90can64``, ``at90scr100``, ``at90usb646``, ``at90usb647``, ``at94k``, ``m3000``.

  ``avr51``
    'Enhanced' devices with 128 KiB of program memory.

    :samp:`{mcu}` = ``atmega128``, ``atmega128a``, ``atmega128rfa1``, ``atmega128rfr2``, ``atmega1280``, ``atmega1281``, ``atmega1284``, ``atmega1284p``, ``atmega1284rfr2``, ``at90can128``, ``at90usb1286``, ``at90usb1287``.

  ``avr6``
    'Enhanced' devices with 3-byte PC, i.e. with more than 128 KiB of program memory.

    :samp:`{mcu}` = ``atmega256rfr2``, ``atmega2560``, ``atmega2561``, ``atmega2564rfr2``.

  ``avrxmega2``
    'XMEGA' devices with more than 8 KiB and up to 64 KiB of program memory.

    :samp:`{mcu}` = ``atxmega8e5``, ``atxmega16a4``, ``atxmega16a4u``, ``atxmega16c4``, ``atxmega16d4``, ``atxmega16e5``, ``atxmega32a4``, ``atxmega32a4u``, ``atxmega32c3``, ``atxmega32c4``, ``atxmega32d3``, ``atxmega32d4``, ``atxmega32e5``.

  ``avrxmega3``
    'XMEGA' devices with up to 64 KiB of combined program memory and RAM, and with program memory visible in the RAM address space.

    :samp:`{mcu}` = ``attiny202``, ``attiny204``, ``attiny212``, ``attiny214``, ``attiny402``, ``attiny404``, ``attiny406``, ``attiny412``, ``attiny414``, ``attiny416``, ``attiny417``, ``attiny804``, ``attiny806``, ``attiny807``, ``attiny814``, ``attiny816``, ``attiny817``, ``attiny1604``, ``attiny1606``, ``attiny1607``, ``attiny1614``, ``attiny1616``, ``attiny1617``, ``attiny3214``, ``attiny3216``, ``attiny3217``, ``atmega808``, ``atmega809``, ``atmega1608``, ``atmega1609``, ``atmega3208``, ``atmega3209``, ``atmega4808``, ``atmega4809``.

  ``avrxmega4``
    'XMEGA' devices with more than 64 KiB and up to 128 KiB of program memory.

    :samp:`{mcu}` = ``atxmega64a3``, ``atxmega64a3u``, ``atxmega64a4u``, ``atxmega64b1``, ``atxmega64b3``, ``atxmega64c3``, ``atxmega64d3``, ``atxmega64d4``.

  ``avrxmega5``
    'XMEGA' devices with more than 64 KiB and up to 128 KiB of program memory and more than 64 KiB of RAM.

    :samp:`{mcu}` = ``atxmega64a1``, ``atxmega64a1u``.

  ``avrxmega6``
    'XMEGA' devices with more than 128 KiB of program memory.

    :samp:`{mcu}` = ``atxmega128a3``, ``atxmega128a3u``, ``atxmega128b1``, ``atxmega128b3``, ``atxmega128c3``, ``atxmega128d3``, ``atxmega128d4``, ``atxmega192a3``, ``atxmega192a3u``, ``atxmega192c3``, ``atxmega192d3``, ``atxmega256a3``, ``atxmega256a3b``, ``atxmega256a3bu``, ``atxmega256a3u``, ``atxmega256c3``, ``atxmega256d3``, ``atxmega384c3``, ``atxmega384d3``.

  ``avrxmega7``
    'XMEGA' devices with more than 128 KiB of program memory and more than 64 KiB of RAM.

    :samp:`{mcu}` = ``atxmega128a1``, ``atxmega128a1u``, ``atxmega128a4u``.

  ``avrtiny``
    'TINY' Tiny core devices with 512 B up to 4 KiB of program memory.

    :samp:`{mcu}` = ``attiny4``, ``attiny5``, ``attiny9``, ``attiny10``, ``attiny20``, ``attiny40``.

  ``avr1``
    This ISA is implemented by the minimal AVR core and supported for assembler only.

    :samp:`{mcu}` = ``attiny11``, ``attiny12``, ``attiny15``, ``attiny28``, ``at90s1200``.

.. option:: -mabsdata

  Assume that all data in static storage can be accessed by LDS / STS
  instructions.  This option has only an effect on reduced Tiny devices like
  ATtiny40.  See also the ``absdata``
  AVR Variable Attributesvariable attribute.

.. option:: -maccumulate-args

  Accumulate outgoing function arguments and acquire/release the needed
  stack space for outgoing function arguments once in function
  prologue/epilogue.  Without this option, outgoing arguments are pushed
  before calling a function and popped afterwards.

  Popping the arguments after the function call can be expensive on
  AVR so that accumulating the stack space might lead to smaller
  executables because arguments need not be removed from the
  stack after such a function call.

  This option can lead to reduced code size for functions that perform
  several calls to functions that get their arguments on the stack like
  calls to printf-like functions.

.. option:: -mbranch-cost=cost

  Set the branch costs for conditional branch instructions to
  :samp:`{cost}`.  Reasonable values for :samp:`{cost}` are small, non-negative
  integers. The default branch cost is 0.

.. option:: -mcall-prologues

  Functions prologues/epilogues are expanded as calls to appropriate
  subroutines.  Code size is smaller.

.. option:: -mdouble=bits

  Set the size (in bits) of the ``double`` or ``long double`` type,
  respectively.  Possible values for :samp:`{bits}` are 32 and 64.
  Whether or not a specific value for :samp:`{bits}` is allowed depends on
  the ``--with-double=`` and ``--with-long-double=``
  https://gcc.gnu.org/install/configure.html#avrconfigure options,
  and the same applies for the default values of the options.

.. option:: -mgas-isr-prologues

  Interrupt service routines (ISRs) may use the ``__gcc_isr`` pseudo
  instruction supported by GNU Binutils.
  If this option is on, the feature can still be disabled for individual
  ISRs by means of the AVR Function Attributes``no_gccisr``
  function attribute.  This feature is activated per default
  if optimization is on (but not with :option:`-Og` , see :ref:`optimize-options`),
  and if GNU Binutils support https://sourceware.org/PR21683PR21683.

.. option:: -mint8

  Assume ``int`` to be 8-bit integer.  This affects the sizes of all types: a
  ``char`` is 1 byte, an ``int`` is 1 byte, a ``long`` is 2 bytes,
  and ``long long`` is 4 bytes.  Please note that this option does not
  conform to the C standards, but it results in smaller code
  size.

.. option:: -mmain-is-OS_task

  Do not save registers in ``main``.  The effect is the same like
  attaching attribute AVR Function Attributes``OS_task``
  to ``main``. It is activated per default if optimization is on.

.. option:: -mn-flash=num

  Assume that the flash memory has a size of 
  :samp:`{num}` times 64 KiB.

.. option:: -mno-interrupts

  Generated code is not compatible with hardware interrupts.
  Code size is smaller.

.. option:: -mrelax

  Try to replace ``CALL`` resp. ``JMP`` instruction by the shorter
  ``RCALL`` resp. ``RJMP`` instruction if applicable.
  Setting :option:`-mrelax` just adds the :option:`--mlink-relax` option to
  the assembler's command line and the :option:`--relax` option to the
  linker's command line.

  Jump relaxing is performed by the linker because jump offsets are not
  known before code is located. Therefore, the assembler code generated by the
  compiler is the same, but the instructions in the executable may
  differ from instructions in the assembler code.

  Relaxing must be turned on if linker stubs are needed, see the
  section on ``EIND`` and linker stubs below.

.. option:: -mrmw

  Assume that the device supports the Read-Modify-Write
  instructions ``XCH``, ``LAC``, ``LAS`` and ``LAT``.

.. option:: -mshort-calls

  Assume that ``RJMP`` and ``RCALL`` can target the whole
  program memory.

  This option is used internally for multilib selection.  It is
  not an optimization option, and you don't need to set it by hand.

.. option:: -msp8

  Treat the stack pointer register as an 8-bit register,
  i.e. assume the high byte of the stack pointer is zero.
  In general, you don't need to set this option by hand.

  This option is used internally by the compiler to select and
  build multilibs for architectures ``avr2`` and ``avr25``.
  These architectures mix devices with and without ``SPH``.
  For any setting other than :option:`-mmcu=avr2` or :option:`-mmcu=avr25`
  the compiler driver adds or removes this option from the compiler
  proper's command line, because the compiler then knows if the device
  or architecture has an 8-bit stack pointer and thus no ``SPH``
  register or not.

.. option:: -mstrict-X

  Use address register ``X`` in a way proposed by the hardware.  This means
  that ``X`` is only used in indirect, post-increment or
  pre-decrement addressing.

  Without this option, the ``X`` register may be used in the same way
  as ``Y`` or ``Z`` which then is emulated by additional
  instructions.  
  For example, loading a value with ``X+const`` addressing with a
  small non-negative ``const < 64`` to a register :samp:`{Rn}` is
  performed as

  .. code-block:: c++

    adiw r26, const   ; X += const
    ld   Rn, X        ; Rn = *X
    sbiw r26, const   ; X -= const

.. option:: -mtiny-stack

  Only change the lower 8 bits of the stack pointer.

.. option:: -mfract-convert-truncate

  Allow to use truncation instead of rounding towards zero for fractional fixed-point types.

.. option:: -nodevicelib

  Don't link against AVR-LibC's device specific library ``lib<mcu>.a``.

.. option:: -nodevicespecs

  Don't add :option:`-specs=device-specs/specs-`:samp:`{mcu}` to the compiler driver's
  command line.  The user takes responsibility for supplying the sub-processes
  like compiler proper, assembler and linker with appropriate command line
  options.  This means that the user has to supply her private device specs
  file by means of :option:`-specs`:samp:`={path-to-specs-file}`.  There is no
  more need for option :option:`-mmcu`:samp:`={mcu}`.

  This option can also serve as a replacement for the older way of
  specifying custom device-specs files that needed :option:`-B `:samp:`{some-path}` to point to a directory
  which contains a folder named ``device-specs`` which contains a specs file named
  ``specs-mcu``, where :samp:`{mcu}` was specified by :option:`-mmcu`:samp:`={mcu}`.

.. option:: -Waddr-space-convert, -Wno-addr-space-convert

  Warn about conversions between address spaces in the case where the
  resulting address space is not contained in the incoming address space.

.. option:: -Wmisspelled-isr, -Wno-misspelled-isr

  Warn if the ISR is misspelled, i.e. without __vector prefix.
  Enabled by default.

``EIND`` and Devices with More Than 128 Ki Bytes of Flash
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: EIND

Pointers in the implementation are 16 bits wide.
The address of a function or label is represented as word address so
that indirect jumps and calls can target any code address in the
range of 64 Ki words.

In order to facilitate indirect jump on devices with more than 128 Ki
bytes of program memory space, there is a special function register called
``EIND`` that serves as most significant part of the target address
when ``EICALL`` or ``EIJMP`` instructions are used.

Indirect jumps and calls on these devices are handled as follows by
the compiler and are subject to some limitations:

* The compiler never sets ``EIND``.

* The compiler uses ``EIND`` implicitly in ``EICALL``/``EIJMP``
  instructions or might read ``EIND`` directly in order to emulate an
  indirect call/jump by means of a ``RET`` instruction.

* The compiler assumes that ``EIND`` never changes during the startup
  code or during the application. In particular, ``EIND`` is not
  saved/restored in function or interrupt service routine
  prologue/epilogue.

* For indirect calls to functions and computed goto, the linker
  generates *stubs*. Stubs are jump pads sometimes also called
  *trampolines*. Thus, the indirect call/jump jumps to such a stub.
  The stub contains a direct jump to the desired address.

* Linker relaxation must be turned on so that the linker generates
  the stubs correctly in all situations. See the compiler option
  :option:`-mrelax` and the linker option :option:`--relax`.
  There are corner cases where the linker is supposed to generate stubs
  but aborts without relaxation and without a helpful error message.

* The default linker script is arranged for code with ``EIND = 0``.
  If code is supposed to work for a setup with ``EIND != 0``, a custom
  linker script has to be used in order to place the sections whose
  name start with ``.trampolines`` into the segment where ``EIND``
  points to.

* The startup code from libgcc never sets ``EIND``.
  Notice that startup code is a blend of code from libgcc and AVR-LibC.
  For the impact of AVR-LibC on ``EIND``, see the
  http://nongnu.org/avr-libc/user-manual/AVR-LibC user manual.

* It is legitimate for user-specific startup code to set up ``EIND``
  early, for example by means of initialization code located in
  section ``.init3``. Such code runs prior to general startup code
  that initializes RAM and calls constructors, but after the bit
  of startup code from AVR-LibC that sets ``EIND`` to the segment
  where the vector table is located.

  .. code-block:: c++

    #include <avr/io.h>

    static void
    __attribute__((section(".init3"),naked,used,no_instrument_function))
    init3_set_eind (void)
    {
      __asm volatile ("ldi r24,pm_hh8(__trampolines_start)\n\t"
                      "out %i0,r24" :: "n" (&EIND) : "r24","memory");
    }

  The ``__trampolines_start`` symbol is defined in the linker script.

* Stubs are generated automatically by the linker if
  the following two conditions are met:

  * The address of a label is taken by means of the ``gs`` modifier
    (short for *generate stubs*) like so:

    .. code-block:: c++

      LDI r24, lo8(gs(func))
      LDI r25, hi8(gs(func))

  * The final location of that label is in a code segment
    *outside* the segment where the stubs are located.

* The compiler emits such ``gs`` modifiers for code labels in the
  following situations:

  * Taking address of a function or code label.

  * Computed goto.

  * If prologue-save function is used, see :option:`-mcall-prologues`
    command-line option.

  * Switch/case dispatch tables. If you do not want such dispatch
    tables you can specify the :option:`-fno-jump-tables` command-line option.

  * C and C++ constructors/destructors called during startup/shutdown.

  * If the tools hit a ``gs()`` modifier explained above.

* Jumping to non-symbolic addresses like so is *not* supported:

  .. code-block:: c++

    int main (void)
    {
        /* Call function at word address 0x2 */
        return ((int(*)(void)) 0x2)();
    }

  Instead, a stub has to be set up, i.e. the function has to be called
  through a symbol (``func_4`` in the example):

  .. code-block:: c++

    int main (void)
    {
        extern int func_4 (void);

        /* Call function at byte address 0x4 */
        return func_4();
    }

  and the application be linked with :option:`-Wl,--defsym,func_4=0x4`.
  Alternatively, ``func_4`` can be defined in the linker script.

Handling of the ``RAMPD``, ``RAMPX``, ``RAMPY`` and ``RAMPZ`` Special Function Registers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index:: RAMPD

.. index:: RAMPX

.. index:: RAMPY

.. index:: RAMPZ

Some AVR devices support memories larger than the 64 KiB range
that can be accessed with 16-bit pointers.  To access memory locations
outside this 64 KiB range, the content of a ``RAMP``
register is used as high part of the address:
The ``X``, ``Y``, ``Z`` address register is concatenated
with the ``RAMPX``, ``RAMPY``, ``RAMPZ`` special function
register, respectively, to get a wide address. Similarly,
``RAMPD`` is used together with direct addressing.

** The startup code initializes the ``RAMP`` special function
  registers with zero.

* If a AVR Named Address Spacesnamed address space other than
  generic or ``__flash`` is used, then ``RAMPZ`` is set
  as needed before the operation.

* If the device supports RAM larger than 64 KiB and the compiler
  needs to change ``RAMPZ`` to accomplish an operation, ``RAMPZ``
  is reset to zero after the operation.

* If the device comes with a specific ``RAMP`` register, the ISR
  prologue/epilogue saves/restores that SFR and initializes it with
  zero in case the ISR code might (implicitly) use it.

* RAM larger than 64 KiB is not supported by GCC for AVR targets.
  If you use inline assembler to read from locations outside the
  16-bit address range and change one of the ``RAMP`` registers,
  you must reset it to zero after the access.

AVR Built-in Macros
~~~~~~~~~~~~~~~~~~~

GCC defines several built-in macros so that the user code can test
for the presence or absence of features.  Almost any of the following
built-in macros are deduced from device capabilities and thus
triggered by the :option:`-mmcu` = command-line option.

For even more AVR-specific built-in macros see
AVR Named Address Spaces and AVR Built-in Functions.

``__AVR_ARCH__``
  Build-in macro that resolves to a decimal number that identifies the
  architecture and depends on the :option:`-mmcu`:samp:`={mcu}` option.
  Possible values are:

  ``2``, ``25``, ``3``, ``31``, ``35``,
  ``4``, ``5``, ``51``, ``6``

  for :samp:`{mcu}` =``avr2``, ``avr25``, ``avr3``, ``avr31``,
  ``avr35``, ``avr4``, ``avr5``, ``avr51``, ``avr6``,

  respectively and

  ``100``,
  ``102``, ``103``, ``104``,
  ``105``, ``106``, ``107``

  for :samp:`{mcu}` =``avrtiny``,
  ``avrxmega2``, ``avrxmega3``, ``avrxmega4``,
  ``avrxmega5``, ``avrxmega6``, ``avrxmega7``, respectively.
  If :samp:`{mcu}` specifies a device, this built-in macro is set
  accordingly. For example, with :option:`-mmcu=atmega8` the macro is
  defined to ``4``.

:samp:`__AVR_{Device}__`
  Setting :option:`-mmcu`:samp:`={device}` defines this built-in macro which reflects
  the device's name. For example, :option:`-mmcu=atmega8` defines the
  built-in macro ``__AVR_ATmega8__``, :option:`-mmcu=attiny261a` defines
  ``__AVR_ATtiny261A__``, etc.

  The built-in macros' names follow
  the scheme ``__AVR_Device__`` where :samp:`{Device}` is
  the device name as from the AVR user manual. The difference between
  :samp:`{Device}` in the built-in macro and :samp:`{device}` in
  :option:`-mmcu`:samp:`={device}` is that the latter is always lowercase.

  If :samp:`{device}` is not a device but only a core architecture like
  :samp:`avr51`, this macro is not defined.

``__AVR_DEVICE_NAME__``
  Setting :option:`-mmcu`:samp:`={device}` defines this built-in macro to
  the device's name. For example, with :option:`-mmcu=atmega8` the macro
  is defined to ``atmega8``.

  If :samp:`{device}` is not a device but only a core architecture like
  :samp:`avr51`, this macro is not defined.

``__AVR_XMEGA__``
  The device / architecture belongs to the XMEGA family of devices.

``__AVR_HAVE_ELPM__``
  The device has the ``ELPM`` instruction.

``__AVR_HAVE_ELPMX__``
  The device has the ``ELPM Rn,Z`` and ``ELPM
  Rn,Z+`` instructions.

``__AVR_HAVE_MOVW__``
  The device has the ``MOVW`` instruction to perform 16-bit
  register-register moves.

``__AVR_HAVE_LPMX__``
  The device has the ``LPM Rn,Z`` and
  ``LPM Rn,Z+`` instructions.

``__AVR_HAVE_MUL__``
  The device has a hardware multiplier.

``__AVR_HAVE_JMP_CALL__``
  The device has the ``JMP`` and ``CALL`` instructions.
  This is the case for devices with more than 8 KiB of program
  memory.

``__AVR_HAVE_EIJMP_EICALL__`` ``__AVR_3_BYTE_PC__``
  The device has the ``EIJMP`` and ``EICALL`` instructions.
  This is the case for devices with more than 128 KiB of program memory.
  This also means that the program counter
  (PC) is 3 bytes wide.

``__AVR_2_BYTE_PC__``
  The program counter (PC) is 2 bytes wide. This is the case for devices
  with up to 128 KiB of program memory.

``__AVR_HAVE_8BIT_SP__`` ``__AVR_HAVE_16BIT_SP__``
  The stack pointer (SP) register is treated as 8-bit respectively
  16-bit register by the compiler.
  The definition of these macros is affected by :option:`-mtiny-stack`.

``__AVR_HAVE_SPH__`` ``__AVR_SP8__``
  The device has the SPH (high part of stack pointer) special function
  register or has an 8-bit stack pointer, respectively.
  The definition of these macros is affected by :option:`-mmcu` = and
  in the cases of :option:`-mmcu=avr2` and :option:`-mmcu=avr25` also
  by :option:`-msp8`.

``__AVR_HAVE_RAMPD__`` ``__AVR_HAVE_RAMPX__`` ``__AVR_HAVE_RAMPY__`` ``__AVR_HAVE_RAMPZ__``
  The device has the ``RAMPD``, ``RAMPX``, ``RAMPY``,
  ``RAMPZ`` special function register, respectively.

``__NO_INTERRUPTS__``
  This macro reflects the :option:`-mno-interrupts` command-line option.

``__AVR_ERRATA_SKIP__`` ``__AVR_ERRATA_SKIP_JMP_CALL__``
  Some AVR devices (AT90S8515, ATmega103) must not skip 32-bit
  instructions because of a hardware erratum.  Skip instructions are
  ``SBRS``, ``SBRC``, ``SBIS``, ``SBIC`` and ``CPSE``.
  The second macro is only defined if ``__AVR_HAVE_JMP_CALL__`` is also
  set.

``__AVR_ISA_RMW__``
  The device has Read-Modify-Write instructions (XCH, LAC, LAS and LAT).

:samp:`__AVR_SFR_OFFSET__={offset}`
  Instructions that can address I/O special function registers directly
  like ``IN``, ``OUT``, ``SBI``, etc. may use a different
  address as if addressed by an instruction to access RAM like ``LD``
  or ``STS``. This offset depends on the device architecture and has
  to be subtracted from the RAM address in order to get the
  respective I/O address.

``__AVR_SHORT_CALLS__``
  The :option:`-mshort-calls` command line option is set.

:samp:`__AVR_PM_BASE_ADDRESS__={addr}`
  Some devices support reading from flash memory by means of ``LD*``
  instructions.  The flash memory is seen in the data address space
  at an offset of ``__AVR_PM_BASE_ADDRESS__``.  If this macro
  is not defined, this feature is not available.  If defined,
  the address space is linear and there is no need to put
  ``.rodata`` into RAM.  This is handled by the default linker
  description file, and is currently available for
  ``avrtiny`` and ``avrxmega3``.  Even more convenient,
  there is no need to use address spaces like ``__flash`` or
  features like attribute ``progmem`` and ``pgm_read_*``.

``__WITH_AVRLIBC__``
  The compiler is configured to be used together with AVR-Libc.
  See the :option:`--with-avrlibc` configure option.

``__HAVE_DOUBLE_MULTILIB__``
  Defined if :option:`-mdouble` = acts as a multilib option.

``__HAVE_DOUBLE32__`` ``__HAVE_DOUBLE64__``
  Defined if the compiler supports 32-bit double resp. 64-bit double.
  The actual layout is specified by option :option:`-mdouble` =.

``__DEFAULT_DOUBLE__``
  The size in bits of ``double`` if :option:`-mdouble` = is not set.
  To test the layout of ``double`` in a program, use the built-in
  macro ``__SIZEOF_DOUBLE__``.

``__HAVE_LONG_DOUBLE32__`` ``__HAVE_LONG_DOUBLE64__`` ``__HAVE_LONG_DOUBLE_MULTILIB__`` ``__DEFAULT_LONG_DOUBLE__``
  Same as above, but for ``long double`` instead of ``double``.

``__WITH_DOUBLE_COMPARISON__``
  Reflects the ``--with-double-comparison={tristate|bool|libf7}``
  https://gcc.gnu.org/install/configure.html#avrconfigure option
  and is defined to ``2`` or ``3``.

``__WITH_LIBF7_LIBGCC__`` ``__WITH_LIBF7_MATH__`` ``__WITH_LIBF7_MATH_SYMBOLS__``
  Reflects the ``--with-libf7={libgcc|math|math-symbols}``
  https://gcc.gnu.org/install/configure.html#avrconfigure option.

  .. _blackfin-options:

Blackfin Options
^^^^^^^^^^^^^^^^

.. index:: Blackfin Options

.. option:: -mcpu=cpu[-sirevision]

  Specifies the name of the target Blackfin processor.  Currently, :samp:`{cpu}`
  can be one of :samp:`bf512`, :samp:`bf514`, :samp:`bf516`, :samp:`bf518`,
  :samp:`bf522`, :samp:`bf523`, :samp:`bf524`, :samp:`bf525`, :samp:`bf526`,
  :samp:`bf527`, :samp:`bf531`, :samp:`bf532`, :samp:`bf533`,
  :samp:`bf534`, :samp:`bf536`, :samp:`bf537`, :samp:`bf538`, :samp:`bf539`,
  :samp:`bf542`, :samp:`bf544`, :samp:`bf547`, :samp:`bf548`, :samp:`bf549`,
  :samp:`bf542m`, :samp:`bf544m`, :samp:`bf547m`, :samp:`bf548m`, :samp:`bf549m`,
  :samp:`bf561`, :samp:`bf592`.

  The optional :samp:`{sirevision}` specifies the silicon revision of the target
  Blackfin processor.  Any workarounds available for the targeted silicon revision
  are enabled.  If :samp:`{sirevision}` is :samp:`none`, no workarounds are enabled.
  If :samp:`{sirevision}` is :samp:`any`, all workarounds for the targeted processor
  are enabled.  The ``__SILICON_REVISION__`` macro is defined to two
  hexadecimal digits representing the major and minor numbers in the silicon
  revision.  If :samp:`{sirevision}` is :samp:`none`, the ``__SILICON_REVISION__``
  is not defined.  If :samp:`{sirevision}` is :samp:`any`, the
  ``__SILICON_REVISION__`` is defined to be ``0xffff``.
  If this optional :samp:`{sirevision}` is not used, GCC assumes the latest known
  silicon revision of the targeted Blackfin processor.

  GCC defines a preprocessor macro for the specified :samp:`{cpu}`.
  For the :samp:`bfin-elf` toolchain, this option causes the hardware BSP
  provided by libgloss to be linked in if :option:`-msim` is not given.

  Without this option, :samp:`bf532` is used as the processor by default.

  Note that support for :samp:`bf561` is incomplete.  For :samp:`bf561`,
  only the preprocessor macro is defined.

.. option:: -msim

  Specifies that the program will be run on the simulator.  This causes
  the simulator BSP provided by libgloss to be linked in.  This option
  has effect only for :samp:`bfin-elf` toolchain.
  Certain other options, such as :option:`-mid-shared-library` and
  :option:`-mfdpic` , imply :option:`-msim`.

.. option:: -momit-leaf-frame-pointer

  Don't keep the frame pointer in a register for leaf functions.  This
  avoids the instructions to save, set up and restore frame pointers and
  makes an extra register available in leaf functions.

.. option:: -mspecld-anomaly

  When enabled, the compiler ensures that the generated code does not
  contain speculative loads after jump instructions. If this option is used,
  ``__WORKAROUND_SPECULATIVE_LOADS`` is defined.

.. option:: -mno-specld-anomaly, -mspecld-anomaly

  Don't generate extra code to prevent speculative loads from occurring.

.. option:: -mcsync-anomaly

  When enabled, the compiler ensures that the generated code does not
  contain CSYNC or SSYNC instructions too soon after conditional branches.
  If this option is used, ``__WORKAROUND_SPECULATIVE_SYNCS`` is defined.

.. option:: -mno-csync-anomaly, -mcsync-anomaly

  Don't generate extra code to prevent CSYNC or SSYNC instructions from
  occurring too soon after a conditional branch.

.. option:: -mlow64k

  When enabled, the compiler is free to take advantage of the knowledge that
  the entire program fits into the low 64k of memory.

.. option:: -mno-low64k

  Assume that the program is arbitrarily large.  This is the default.

.. option:: -mstack-check-l1

  Do stack checking using information placed into L1 scratchpad memory by the
  uClinux kernel.

.. option:: -mid-shared-library

  Generate code that supports shared libraries via the library ID method.
  This allows for execute in place and shared libraries in an environment
  without virtual memory management.  This option implies :option:`-fPIC`.
  With a :samp:`bfin-elf` target, this option implies :option:`-msim`.

.. option:: -mno-id-shared-library, -mid-shared-library

  Generate code that doesn't assume ID-based shared libraries are being used.
  This is the default.

.. option:: -mleaf-id-shared-library

  Generate code that supports shared libraries via the library ID method,
  but assumes that this library or executable won't link against any other
  ID shared libraries.  That allows the compiler to use faster code for jumps
  and calls.

.. option:: -mno-leaf-id-shared-library, -mleaf-id-shared-library

  Do not assume that the code being compiled won't link against any ID shared
  libraries.  Slower code is generated for jump and call insns.

.. option:: -mshared-library-id=n

  Specifies the identification number of the ID-based shared library being
  compiled.  Specifying a value of 0 generates more compact code; specifying
  other values forces the allocation of that number to the current
  library but is no more space- or time-efficient than omitting this option.

.. option:: -msep-data

  Generate code that allows the data segment to be located in a different
  area of memory from the text segment.  This allows for execute in place in
  an environment without virtual memory management by eliminating relocations
  against the text section.

.. option:: -mno-sep-data, -msep-data

  Generate code that assumes that the data segment follows the text segment.
  This is the default.

.. option:: -mlong-calls, -mno-long-calls

  Tells the compiler to perform function calls by first loading the
  address of the function into a register and then performing a subroutine
  call on this register.  This switch is needed if the target function
  lies outside of the 24-bit addressing range of the offset-based
  version of subroutine call instruction.

  This feature is not enabled by default.  Specifying
  :option:`-mno-long-calls` restores the default behavior.  Note these
  switches have no effect on how the compiler generates code to handle
  function calls via function pointers.

.. option:: -mfast-fp

  Link with the fast floating-point library. This library relaxes some of
  the IEEE floating-point standard's rules for checking inputs against
  Not-a-Number (NAN), in the interest of performance.

.. option:: -minline-plt

  Enable inlining of PLT entries in function calls to functions that are
  not known to bind locally.  It has no effect without :option:`-mfdpic`.

.. option:: -mmulticore

  Build a standalone application for multicore Blackfin processors. 
  This option causes proper start files and link scripts supporting 
  multicore to be used, and defines the macro ``__BFIN_MULTICORE``. 
  It can only be used with :option:`-mcpu=bf561` [- :samp:`{sirevision}` ]. 

  This option can be used with :option:`-mcorea` or :option:`-mcoreb` , which
  selects the one-application-per-core programming model.  Without
  :option:`-mcorea` or :option:`-mcoreb` , the single-application/dual-core
  programming model is used. In this model, the main function of Core B
  should be named as ``coreb_main``.

  If this option is not used, the single-core application programming
  model is used.

.. option:: -mcorea

  Build a standalone application for Core A of BF561 when using
  the one-application-per-core programming model. Proper start files
  and link scripts are used to support Core A, and the macro
  ``__BFIN_COREA`` is defined.
  This option can only be used in conjunction with :option:`-mmulticore`.

.. option:: -mcoreb

  Build a standalone application for Core B of BF561 when using
  the one-application-per-core programming model. Proper start files
  and link scripts are used to support Core B, and the macro
  ``__BFIN_COREB`` is defined. When this option is used, ``coreb_main``
  should be used instead of ``main``. 
  This option can only be used in conjunction with :option:`-mmulticore`.

.. option:: -msdram

  Build a standalone application for SDRAM. Proper start files and
  link scripts are used to put the application into SDRAM, and the macro
  ``__BFIN_SDRAM`` is defined.
  The loader should initialize SDRAM before loading the application.

.. option:: -micplb

  Assume that ICPLBs are enabled at run time.  This has an effect on certain
  anomaly workarounds.  For Linux targets, the default is to assume ICPLBs
  are enabled; for standalone applications the default is off.

.. _c6x-options:

C6X Options
^^^^^^^^^^^

.. index:: C6X Options

.. option:: -march=name

  This specifies the name of the target architecture.  GCC uses this
  name to determine what kind of instructions it can emit when generating
  assembly code.  Permissible names are: :samp:`c62x`,
  :samp:`c64x`, :samp:`c64x+`, :samp:`c67x`, :samp:`c67x+`, :samp:`c674x`.

.. option:: -mbig-endian

  Generate code for a big-endian target.

.. option:: -mlittle-endian

  Generate code for a little-endian target.  This is the default.

.. option:: -msim

  Choose startup files and linker script suitable for the simulator.

.. option:: -msdata=default

  Put small global and static data in the ``.neardata`` section,
  which is pointed to by register ``B14``.  Put small uninitialized
  global and static data in the ``.bss`` section, which is adjacent
  to the ``.neardata`` section.  Put small read-only data into the
  ``.rodata`` section.  The corresponding sections used for large
  pieces of data are ``.fardata``, ``.far`` and ``.const``.

.. option:: -msdata=all

  Put all data, not just small objects, into the sections reserved for
  small data, and use addressing relative to the ``B14`` register to
  access them.

.. option:: -msdata=none

  Make no use of the sections reserved for small data, and use absolute
  addresses to access all data.  Put all initialized global and static
  data in the ``.fardata`` section, and all uninitialized data in the
  ``.far`` section.  Put all constant data into the ``.const``
  section.

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
  :option:`-march=v3` and :option:`-march=v8` respectively.

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

.. option:: -mstack-align, -mno-stack-align, -mdata-align, -mno-data-align
.. option:: -mconst-align, -mno-const-align

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

  With :option:`-mno-prologue-epilogue` , the normal function prologue and
  epilogue which set up the stack frame are omitted and no return
  instructions or return sequences are generated in the code.  Use this
  option only together with visual inspection of the compiled code: no
  warnings or errors are generated when call-saved registers must be saved,
  or storage for local variables needs to be allocated.

.. option:: -mno-gotplt, -mgotplt

  With :option:`-fpic` and :option:`-fPIC` , don't generate (do generate)
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

  Like :option:`-sim` , but pass linker options to locate initialized data at
  0x40000000 and zero-initialized data at 0x80000000.

.. _cr16-options:

CR16 Options
^^^^^^^^^^^^

.. index:: CR16 Options

These options are defined specifically for the CR16 ports.

.. option:: -mmac

  Enable the use of multiply-accumulate instructions. Disabled by default.

.. option:: -mcr16cplus, -mcr16c

  Generate code for CR16C or CR16C+ architecture. CR16C+ architecture 
  is default.

.. option:: -msim

  Links the library libsim.a which is in compatible with simulator. Applicable
  to ELF compiler only.

.. option:: -mint32

  Choose integer type as 32-bit wide.

.. option:: -mbit-ops

  Generates ``sbit``/``cbit`` instructions for bit manipulations.

.. option:: -mdata-model=model

  Choose a data model. The choices for :samp:`{model}` are :samp:`near`,
  :samp:`far` or :samp:`medium`. :samp:`medium` is default.
  However, :samp:`far` is not valid with :option:`-mcr16c` , as the
  CR16C architecture does not support the far data model.

.. _c-sky-options:

C-SKY Options
^^^^^^^^^^^^^

.. index:: C-SKY Options

GCC supports these options when compiling for C-SKY V2 processors.

.. option:: -march=arch

  Specify the C-SKY target architecture.  Valid values for :samp:`{arch}` are:
  :samp:`ck801`, :samp:`ck802`, :samp:`ck803`, :samp:`ck807`, and :samp:`ck810`.
  The default is :samp:`ck810`.

.. option:: -mcpu=cpu

  Specify the C-SKY target processor.  Valid values for :samp:`{cpu}` are:
  :samp:`ck801`, :samp:`ck801t`,
  :samp:`ck802`, :samp:`ck802t`, :samp:`ck802j`,
  :samp:`ck803`, :samp:`ck803h`, :samp:`ck803t`, :samp:`ck803ht`,
  :samp:`ck803f`, :samp:`ck803fh`, :samp:`ck803e`, :samp:`ck803eh`,
  :samp:`ck803et`, :samp:`ck803eht`, :samp:`ck803ef`, :samp:`ck803efh`,
  :samp:`ck803ft`, :samp:`ck803eft`, :samp:`ck803efht`, :samp:`ck803r1`,
  :samp:`ck803hr1`, :samp:`ck803tr1`, :samp:`ck803htr1`, :samp:`ck803fr1`,
  :samp:`ck803fhr1`, :samp:`ck803er1`, :samp:`ck803ehr1`, :samp:`ck803etr1`,
  :samp:`ck803ehtr1`, :samp:`ck803efr1`, :samp:`ck803efhr1`, :samp:`ck803ftr1`,
  :samp:`ck803eftr1`, :samp:`ck803efhtr1`,
  :samp:`ck803s`, :samp:`ck803st`, :samp:`ck803se`, :samp:`ck803sf`,
  :samp:`ck803sef`, :samp:`ck803seft`,
  :samp:`ck807e`, :samp:`ck807ef`, :samp:`ck807`, :samp:`ck807f`,
  :samp:`ck810e`, :samp:`ck810et`, :samp:`ck810ef`, :samp:`ck810eft`,
  :samp:`ck810`, :samp:`ck810v`, :samp:`ck810f`, :samp:`ck810t`, :samp:`ck810fv`,
  :samp:`ck810tv`, :samp:`ck810ft`, and :samp:`ck810ftv`.

.. option:: -mbig-endian, -EL

  Select big- or little-endian code.  The default is little-endian.

.. option:: -mfloat-abi=name

  Specifies which floating-point ABI to use.  Permissible values
  are: :samp:`soft`, :samp:`softfp` and :samp:`hard`.

  Specifying :samp:`soft` causes GCC to generate output containing
  library calls for floating-point operations.
  :samp:`softfp` allows the generation of code using hardware floating-point
  instructions, but still uses the soft-float calling conventions.
  :samp:`hard` allows generation of floating-point instructions
  and uses FPU-specific calling conventions.

  The default depends on the specific target configuration.  Note that
  the hard-float and soft-float ABIs are not link-compatible; you must
  compile your entire program with the same ABI, and link with a
  compatible set of libraries.

.. option:: -mhard-float, -msoft-float

  Select hardware or software floating-point implementations.
  The default is soft float.

.. option:: -mdouble-float

  When :option:`-mhard-float` is in effect, enable generation of
  double-precision float instructions.  This is the default except
  when compiling for CK803.

.. option:: -mfdivdu

  When :option:`-mhard-float` is in effect, enable generation of
  ``frecipd``, ``fsqrtd``, and ``fdivd`` instructions.
  This is the default except when compiling for CK803.

.. option:: -mfpu=fpu

  Select the floating-point processor.  This option can only be used with
  :option:`-mhard-float`.
  Values for :samp:`{fpu}` are
  :samp:`fpv2_sf` (equivalent to :samp:`-mno-double-float -mno-fdivdu`),
  :samp:`fpv2` (:samp:`-mdouble-float -mno-divdu`), and
  :samp:`fpv2_divd` (:samp:`-mdouble-float -mdivdu`).

.. option:: -melrw

  Enable the extended ``lrw`` instruction.  This option defaults to on
  for CK801 and off otherwise.

.. option:: -mistack

  Enable interrupt stack instructions; the default is off.

  The :option:`-mistack` option is required to handle the
  ``interrupt`` and ``isr`` function attributes
  (see :ref:`c-sky-function-attributes`).

.. option:: -mmp

  Enable multiprocessor instructions; the default is off.

.. option:: -mcp

  Enable coprocessor instructions; the default is off.

.. option:: -mcache

  Enable coprocessor instructions; the default is off.

.. option:: -msecurity

  Enable C-SKY security instructions; the default is off.

.. option:: -mtrust

  Enable C-SKY trust instructions; the default is off.

.. option:: -mdsp, -mvdsp

  Enable C-SKY DSP, Enhanced DSP, or Vector DSP instructions, respectively.
  All of these options default to off.

.. option:: -mdiv

  Generate divide instructions.  Default is off.

.. option:: -msmart

  Generate code for Smart Mode, using only registers numbered 0-7 to allow
  use of 16-bit instructions.  This option is ignored for CK801 where this
  is the required behavior, and it defaults to on for CK802.
  For other targets, the default is off.

.. option:: -mhigh-registers

  Generate code using the high registers numbered 16-31.  This option
  is not supported on CK801, CK802, or CK803, and is enabled by default
  for other processors.

.. option:: -manchor

  Generate code using global anchor symbol addresses.

.. option:: -mpushpop

  Generate code using ``push`` and ``pop`` instructions.  This option
  defaults to on.

.. option:: -mmultiple-stld

  Generate code using ``stm`` and ``ldm`` instructions.  This option
  isn't supported on CK801 but is enabled by default on other processors.

.. option:: -mconstpool

  Create constant pools in the compiler instead of deferring it to the
  assembler.  This option is the default and required for correct code
  generation on CK801 and CK802, and is optional on other processors.

``-mstack-size``
.. option:: -mno-stack-size, -mstack-size

  Emit ``.stack_size`` directives for each function in the assembly
  output.  This option defaults to off.

.. option:: -mccrt

  Generate code for the C-SKY compiler runtime instead of libgcc.  This
  option defaults to off.

.. option:: -mbranch-cost=n

  Set the branch costs to roughly ``n`` instructions.  The default is 1.

.. option:: -msched-prolog

  Permit scheduling of function prologue and epilogue sequences.  Using
  this option can result in code that is not compliant with the C-SKY V2 ABI
  prologue requirements and that cannot be debugged or backtraced.
  It is disabled by default.

.. option:: -msim

  Links the library libsemi.a which is in compatible with simulator. Applicable
  to ELF compiler only.

.. _darwin-options:

Darwin Options
^^^^^^^^^^^^^^

.. index:: Darwin options

These options are defined for all architectures running the Darwin operating
system.

FSF GCC on Darwin does not create 'fat' object files; it creates
an object file for the single architecture that GCC was built to
target.  Apple's GCC on Darwin does create 'fat' files if multiple
:option:`-arch` options are used; it does so by running the compiler or
linker multiple times and joining the results together with
lipo.

The subtype of the file created (like :samp:`ppc7400` or :samp:`ppc970` or
:samp:`i686`) is determined by the flags that specify the ISA
that GCC is targeting, like :option:`-mcpu` or :option:`-march`.  The
:option:`-force_cpusubtype_ALL` option can be used to override this.

The Darwin tools vary in their behavior when presented with an ISA
mismatch.  The assembler, as, only permits instructions to
be used that are valid for the subtype of the file it is generating,
so you cannot put 64-bit instructions in a :samp:`ppc750` object file.
The linker for shared libraries, /usr/bin/libtool, fails
and prints an error if asked to create a shared library with a less
restrictive subtype than its input files (for instance, trying to put
a :samp:`ppc970` object file in a :samp:`ppc7400` library).  The linker
for executables, :command:`ld`, quietly gives the executable the most
restrictive subtype of any of its input files.

.. option:: -Fdir, -F

  Add the framework directory :samp:`{dir}` to the head of the list of
  directories to be searched for header files.  These directories are
  interleaved with those specified by :option:`-I` options and are
  scanned in a left-to-right order.

  A framework directory is a directory with frameworks in it.  A
  framework is a directory with a Headers and/or
  PrivateHeaders directory contained directly in it that ends
  in .framework.  The name of a framework is the name of this
  directory excluding the .framework.  Headers associated with
  the framework are found in one of those two directories, with
  Headers being searched first.  A subframework is a framework
  directory that is in a framework's Frameworks directory.
  Includes of subframework headers can only appear in a header of a
  framework that contains the subframework, or in a sibling subframework
  header.  Two subframeworks are siblings if they occur in the same
  framework.  A subframework should not have the same name as a
  framework; a warning is issued if this is violated.  Currently a
  subframework cannot have subframeworks; in the future, the mechanism
  may be extended to support this.  The standard frameworks can be found
  in /System/Library/Frameworks and
  /Library/Frameworks.  An example include looks like
  ``#include <Framework/header.h>``, where Framework denotes
  the name of the framework and header.h is found in the
  PrivateHeaders or Headers directory.

.. option:: -iframeworkdir, -iframework

  Like :option:`-F` except the directory is a treated as a system
  directory.  The main difference between this :option:`-iframework` and
  :option:`-F` is that with :option:`-iframework` the compiler does not
  warn about constructs contained within header files found via
  :samp:`{dir}`.  This option is valid only for the C family of languages.

.. option:: -gused

  Emit debugging information for symbols that are used.  For stabs
  debugging format, this enables :option:`-feliminate-unused-debug-symbols`.
  This is by default ON.

.. option:: -gfull

  Emit debugging information for all symbols and types.

:samp:`-mmacosx-version-min={version}`
  The earliest version of MacOS X that this executable will run on
  is :samp:`{version}`.  Typical values of :samp:`{version}` include ``10.1``,
  ``10.2``, and ``10.3.9``.

  If the compiler was built to use the system's headers by default,
  then the default for this option is the system version on which the
  compiler is running, otherwise the default is to make choices that
  are compatible with as many systems and code bases as possible.

.. option:: -mkernel

  Enable kernel development mode.  The :option:`-mkernel` option sets
  :option:`-static` , :option:`-fno-common` , :option:`-fno-use-cxa-atexit` ,
  :option:`-fno-exceptions` , :option:`-fno-non-call-exceptions` ,
  :option:`-fapple-kext` , :option:`-fno-weak` and :option:`-fno-rtti` where
  applicable.  This mode also sets :option:`-mno-altivec` ,
  :option:`-msoft-float` , :option:`-fno-builtin` and
  :option:`-mlong-branch` for PowerPC targets.

.. option:: -mone-byte-bool

  Override the defaults for ``bool`` so that ``sizeof(bool)==1``.
  By default ``sizeof(bool)`` is ``4`` when compiling for
  Darwin/PowerPC and ``1`` when compiling for Darwin/x86, so this
  option has no effect on x86.

  Warning: The :option:`-mone-byte-bool` switch causes GCC
  to generate code that is not binary compatible with code generated
  without that switch.  Using this switch may require recompiling all
  other modules in a program, including system libraries.  Use this
  switch to conform to a non-default data model.

.. option:: -mfix-and-continue, -ffix-and-continue, -findirect-data

  Generate code suitable for fast turnaround development, such as to
  allow GDB to dynamically load .o files into already-running
  programs.  :option:`-findirect-data` and :option:`-ffix-and-continue`
  are provided for backwards compatibility.

.. option:: -all_load

  Loads all members of static archive libraries.
  See man ld(1) for more information.

.. option:: -arch_errors_fatal

  Cause the errors having to do with files that have the wrong architecture
  to be fatal.

.. option:: -bind_at_load

  Causes the output file to be marked such that the dynamic linker will
  bind all undefined references when the file is loaded or launched.

.. option:: -bundle

  Produce a Mach-o bundle format file.
  See man ld(1) for more information.

.. option:: -bundle_loader executable, -bundle_loader

  This option specifies the :samp:`{executable}` that will load the build
  output file being linked.  See man ld(1) for more information.

.. option:: -dynamiclib

  When passed this option, GCC produces a dynamic library instead of
  an executable when linking, using the Darwin libtool command.

.. option:: -force_cpusubtype_ALL

  This causes GCC's output file to have the :samp:`ALL` subtype, instead of
  one controlled by the :option:`-mcpu` or :option:`-march` option.

.. option:: -allowable_client  client_name, -allowable_client, -client_name
.. option:: -compatibility_version, -current_version, -dead_strip, -dependency-file
.. option:: -dylib_file, -dylinker_install_name, -dynamic, -exported_symbols_list
.. option:: -filelist, -flat_namespace, -force_flat_namespace
.. option:: -headerpad_max_install_names, -image_base, -init, -install_name
.. option:: -keep_private_externs, -multi_module, -multiply_defined
.. option:: -multiply_defined_unused, -noall_load, -no_dead_strip_inits_and_terms
.. option:: -nofixprebinding, -nomultidefs, -noprebind, -noseglinkedit
.. option:: -pagezero_size, -prebind, -prebind_all_twolevel_modules
.. option:: -private_bundle, -read_only_relocs, -sectalign, -sectobjectsymbols
.. option:: -whyload, -seg1addr, -sectcreate, -sectorder, -segaddr
.. option:: -segs_read_only_addr, -segs_read_write_addr, -seg_addr_table
.. option:: -seg_addr_table_filename, -seglinkedit, -segprot, -single_module
.. option:: -static, -sub_library, -sub_umbrella, -twolevel_namespace, -umbrella
.. option:: -undefined, -unexported_symbols_list, -weak_reference_mismatches
.. option:: -whatsloaded

  These options are passed to the Darwin linker.  The Darwin linker man page
  describes them in detail.

.. _dec-alpha-options:

DEC Alpha Options
^^^^^^^^^^^^^^^^^

These :samp:`-m` options are defined for the DEC Alpha implementations:

.. option:: -mno-soft-float, -msoft-float

  Use (do not use) the hardware floating-point instructions for
  floating-point operations.  When :option:`-msoft-float` is specified,
  functions in libgcc.a are used to perform floating-point
  operations.  Unless they are replaced by routines that emulate the
  floating-point operations, or compiled in such a way as to call such
  emulations routines, these routines issue floating-point
  operations.   If you are compiling for an Alpha without floating-point
  operations, you must ensure that the library is built so as not to call
  them.

  Note that Alpha implementations without floating-point operations are
  required to have floating-point registers.

.. option:: -mfp-reg, -mno-fp-regs

  Generate code that uses (does not use) the floating-point register set.
  :option:`-mno-fp-regs` implies :option:`-msoft-float`.  If the floating-point
  register set is not used, floating-point operands are passed in integer
  registers as if they were integers and floating-point results are passed
  in ``$0`` instead of ``$f0``.  This is a non-standard calling sequence,
  so any function with a floating-point argument or return value called by code
  compiled with :option:`-mno-fp-regs` must also be compiled with that
  option.

  A typical use of this option is building a kernel that does not use,
  and hence need not save and restore, any floating-point registers.

.. option:: -mieee

  The Alpha architecture implements floating-point hardware optimized for
  maximum performance.  It is mostly compliant with the IEEE floating-point
  standard.  However, for full compliance, software assistance is
  required.  This option generates code fully IEEE-compliant code
  *except* that the :samp:`{inexact-flag}` is not maintained (see below).
  If this option is turned on, the preprocessor macro ``_IEEE_FP`` is
  defined during compilation.  The resulting code is less efficient but is
  able to correctly support denormalized numbers and exceptional IEEE
  values such as not-a-number and plus/minus infinity.  Other Alpha
  compilers call this option :option:`-ieee_with_no_inexact`.

.. option:: -mieee-with-inexact

  This is like :option:`-mieee` except the generated code also maintains
  the IEEE :samp:`{inexact-flag}`.  Turning on this option causes the
  generated code to implement fully-compliant IEEE math.  In addition to
  ``_IEEE_FP``, ``_IEEE_FP_EXACT`` is defined as a preprocessor
  macro.  On some Alpha implementations the resulting code may execute
  significantly slower than the code generated by default.  Since there is
  very little code that depends on the :samp:`{inexact-flag}` , you should
  normally not specify this option.  Other Alpha compilers call this
  option :option:`-ieee_with_inexact`.

.. option:: -mfp-trap-mode=trap-mode

  This option controls what floating-point related traps are enabled.
  Other Alpha compilers call this option :option:`-fptm `:samp:`{trap-mode}`.
  The trap mode can be set to one of four values:

  :samp:`n`
    This is the default (normal) setting.  The only traps that are enabled
    are the ones that cannot be disabled in software (e.g., division by zero
    trap).

  :samp:`u`
    In addition to the traps enabled by :samp:`n`, underflow traps are enabled
    as well.

  :samp:`su`
    Like :samp:`u`, but the instructions are marked to be safe for software
    completion (see Alpha architecture manual for details).

  :samp:`sui`
    Like :samp:`su`, but inexact traps are enabled as well.

.. option:: -mfp-rounding-mode=rounding-mode

  Selects the IEEE rounding mode.  Other Alpha compilers call this option
  :option:`-fprm `:samp:`{rounding-mode}`.  The :samp:`{rounding-mode}` can be one
  of:

  :samp:`n`
    Normal IEEE rounding mode.  Floating-point numbers are rounded towards
    the nearest machine number or towards the even machine number in case
    of a tie.

  :samp:`m`
    Round towards minus infinity.

  :samp:`c`
    Chopped rounding mode.  Floating-point numbers are rounded towards zero.

  :samp:`d`
    Dynamic rounding mode.  A field in the floating-point control register
    ( :samp:`{fpcr}` , see Alpha architecture reference manual) controls the
    rounding mode in effect.  The C library initializes this register for
    rounding towards plus infinity.  Thus, unless your program modifies the
    :samp:`{fpcr}` , :samp:`d` corresponds to round towards plus infinity.

.. option:: -mtrap-precision=trap-precision

  In the Alpha architecture, floating-point traps are imprecise.  This
  means without software assistance it is impossible to recover from a
  floating trap and program execution normally needs to be terminated.
  GCC can generate code that can assist operating system trap handlers
  in determining the exact location that caused a floating-point trap.
  Depending on the requirements of an application, different levels of
  precisions can be selected:

  :samp:`p`
    Program precision.  This option is the default and means a trap handler
    can only identify which program caused a floating-point exception.

  :samp:`f`
    Function precision.  The trap handler can determine the function that
    caused a floating-point exception.

  :samp:`i`
    Instruction precision.  The trap handler can determine the exact
    instruction that caused a floating-point exception.

    Other Alpha compilers provide the equivalent options called
  :option:`-scope_safe` and :option:`-resumption_safe`.

.. option:: -mieee-conformant

  This option marks the generated code as IEEE conformant.  You must not
  use this option unless you also specify :option:`-mtrap-precision=i` and either
  :option:`-mfp-trap-mode=su` or :option:`-mfp-trap-mode=sui`.  Its only effect
  is to emit the line :samp:`.eflag 48` in the function prologue of the
  generated assembly file.

.. option:: -mbuild-constants

  Normally GCC examines a 32- or 64-bit integer constant to
  see if it can construct it from smaller constants in two or three
  instructions.  If it cannot, it outputs the constant as a literal and
  generates code to load it from the data segment at run time.

  Use this option to require GCC to construct *all* integer constants
  using code, even if it takes more instructions (the maximum is six).

  You typically use this option to build a shared library dynamic
  loader.  Itself a shared library, it must relocate itself in memory
  before it can find the variables and constants in its own data segment.

.. option:: -mbwx, -mno-bwx, -mcix, -mno-cix, -mfix, -mno-fix, -mmax, -mno-max

  Indicate whether GCC should generate code to use the optional BWX,
  CIX, FIX and MAX instruction sets.  The default is to use the instruction
  sets supported by the CPU type specified via :option:`-mcpu` = option or that
  of the CPU on which GCC was built if none is specified.

.. option:: -mfloat-vax, -mfloat-ieee

  Generate code that uses (does not use) VAX F and G floating-point
  arithmetic instead of IEEE single and double precision.

.. option:: -mexplicit-relocs, -mno-explicit-relocs

  Older Alpha assemblers provided no way to generate symbol relocations
  except via assembler macros.  Use of these macros does not allow
  optimal instruction scheduling.  GNU binutils as of version 2.12
  supports a new syntax that allows the compiler to explicitly mark
  which relocations should apply to which instructions.  This option
  is mostly useful for debugging, as GCC detects the capabilities of
  the assembler when it is built and sets the default accordingly.

.. option:: -msmall-data, -mlarge-data

  When :option:`-mexplicit-relocs` is in effect, static data is
  accessed via :dfn:`gp-relative` relocations.  When :option:`-msmall-data`
  is used, objects 8 bytes long or smaller are placed in a :dfn:`small data area`
  (the ``.sdata`` and ``.sbss`` sections) and are accessed via
  16-bit relocations off of the ``$gp`` register.  This limits the
  size of the small data area to 64KB, but allows the variables to be
  directly accessed via a single instruction.

  The default is :option:`-mlarge-data`.  With this option the data area
  is limited to just below 2GB.  Programs that require more than 2GB of
  data must use ``malloc`` or ``mmap`` to allocate the data in the
  heap instead of in the program's data segment.

  When generating code for shared libraries, :option:`-fpic` implies
  :option:`-msmall-data` and :option:`-fPIC` implies :option:`-mlarge-data`.

.. option:: -msmall-text, -mlarge-text

  When :option:`-msmall-text` is used, the compiler assumes that the
  code of the entire program (or shared library) fits in 4MB, and is
  thus reachable with a branch instruction.  When :option:`-msmall-data`
  is used, the compiler can assume that all local symbols share the
  same ``$gp`` value, and thus reduce the number of instructions
  required for a function call from 4 to 1.

  The default is :option:`-mlarge-text`.

.. option:: -mcpu=cpu_type

  Set the instruction set and instruction scheduling parameters for
  machine type :samp:`{cpu_type}`.  You can specify either the :samp:`EV`
  style name or the corresponding chip number.  GCC supports scheduling
  parameters for the EV4, EV5 and EV6 family of processors and
  chooses the default values for the instruction set from the processor
  you specify.  If you do not specify a processor type, GCC defaults
  to the processor on which the compiler was built.

  Supported values for :samp:`{cpu_type}` are

  :samp:`ev4` :samp:`ev45` :samp:`21064`
    Schedules as an EV4 and has no instruction set extensions.

  :samp:`ev5` :samp:`21164`
    Schedules as an EV5 and has no instruction set extensions.

  :samp:`ev56` :samp:`21164a`
    Schedules as an EV5 and supports the BWX extension.

  :samp:`pca56` :samp:`21164pc` :samp:`21164PC`
    Schedules as an EV5 and supports the BWX and MAX extensions.

  :samp:`ev6` :samp:`21264`
    Schedules as an EV6 and supports the BWX, FIX, and MAX extensions.

  :samp:`ev67` :samp:`21264a`
    Schedules as an EV6 and supports the BWX, CIX, FIX, and MAX extensions.

    Native toolchains also support the value :samp:`native`,
  which selects the best architecture option for the host processor.
  :option:`-mcpu=native` has no effect if GCC does not recognize
  the processor.

.. option:: -mtune=cpu_type

  Set only the instruction scheduling parameters for machine type
  :samp:`{cpu_type}`.  The instruction set is not changed.

  Native toolchains also support the value :samp:`native`,
  which selects the best architecture option for the host processor.
  :option:`-mtune=native` has no effect if GCC does not recognize
  the processor.

.. option:: -mmemory-latency=time

  Sets the latency the scheduler should assume for typical memory
  references as seen by the application.  This number is highly
  dependent on the memory access patterns used by the application
  and the size of the external cache on the machine.

  Valid options for :samp:`{time}` are

  :samp:`number`
    A decimal number representing clock cycles.

  :samp:`L1` :samp:`L2` :samp:`L3` :samp:`main`
    The compiler contains estimates of the number of clock cycles for
    'typical' EV4 & EV5 hardware for the Level 1, 2 & 3 caches
    (also called Dcache, Scache, and Bcache), as well as to main memory.
    Note that L3 is only valid for EV5.

.. _ebpf-options:

eBPF Options
^^^^^^^^^^^^

.. index:: eBPF Options

:samp:`-mframe-limit={bytes}`
  This specifies the hard limit for frame sizes, in bytes.  Currently,
  the value that can be specified should be less than or equal to
  :samp:`32767`.  Defaults to whatever limit is imposed by the version of
  the Linux kernel targeted.

.. option:: -mkernel=version

  This specifies the minimum version of the kernel that will run the
  compiled program.  GCC uses this version to determine which
  instructions to use, what kernel helpers to allow, etc.  Currently,
  :samp:`{version}` can be one of :samp:`4.0`, :samp:`4.1`, :samp:`4.2`,
  :samp:`4.3`, :samp:`4.4`, :samp:`4.5`, :samp:`4.6`, :samp:`4.7`,
  :samp:`4.8`, :samp:`4.9`, :samp:`4.10`, :samp:`4.11`, :samp:`4.12`,
  :samp:`4.13`, :samp:`4.14`, :samp:`4.15`, :samp:`4.16`, :samp:`4.17`,
  :samp:`4.18`, :samp:`4.19`, :samp:`4.20`, :samp:`5.0`, :samp:`5.1`,
  :samp:`5.2`, :samp:`latest` and :samp:`native`.

.. option:: -mbig-endian

  Generate code for a big-endian target.

.. option:: -mlittle-endian

  Generate code for a little-endian target.  This is the default.

``-mxbpf``
  Generate code for an expanded version of BPF, which relaxes some of
  the restrictions imposed by the BPF architecture:

  * Save and restore callee-saved registers at function entry and
    exit, respectively.

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

.. _ft32-options:

FT32 Options
^^^^^^^^^^^^

.. index:: FT32 Options

These options are defined specifically for the FT32 port.

.. option:: -msim

  Specifies that the program will be run on the simulator.  This causes
  an alternate runtime startup and library to be linked.
  You must not use this option when generating programs that will run on
  real hardware; you must provide your own runtime library for whatever
  I/O functions are needed.

.. option:: -mlra

  Enable Local Register Allocation.  This is still experimental for FT32,
  so by default the compiler uses standard reload.

.. option:: -mnodiv

  Do not use div and mod instructions.

.. option:: -mft32b

  Enable use of the extended instructions of the FT32B processor.

.. option:: -mcompress

  Compress all code using the Ft32B code compression scheme.

.. option:: -mnopm

  Do not generate code that reads program memory.

.. _frv-options:

FRV Options
^^^^^^^^^^^

.. index:: FRV Options

.. option:: -mgpr-32

  Only use the first 32 general-purpose registers.

.. option:: -mgpr-64

  Use all 64 general-purpose registers.

.. option:: -mfpr-32

  Use only the first 32 floating-point registers.

.. option:: -mfpr-64

  Use all 64 floating-point registers.

.. option:: -mhard-float

  Use hardware instructions for floating-point operations.

.. option:: -msoft-float

  Use library routines for floating-point operations.

.. option:: -malloc-cc

  Dynamically allocate condition code registers.

.. option:: -mfixed-cc

  Do not try to dynamically allocate condition code registers, only
  use ``icc0`` and ``fcc0``.

.. option:: -mdword

  Change ABI to use double word insns.

.. option:: -mno-dword, -mdword

  Do not use double word instructions.

.. option:: -mdouble

  Use floating-point double instructions.

.. option:: -mno-double

  Do not use floating-point double instructions.

.. option:: -mmedia

  Use media instructions.

.. option:: -mno-media

  Do not use media instructions.

.. option:: -mmuladd

  Use multiply and add/subtract instructions.

.. option:: -mno-muladd

  Do not use multiply and add/subtract instructions.

.. option:: -mfdpic

  Select the FDPIC ABI, which uses function descriptors to represent
  pointers to functions.  Without any PIC/PIE-related options, it
  implies :option:`-fPIE`.  With :option:`-fpic` or :option:`-fpie` , it
  assumes GOT entries and small data are within a 12-bit range from the
  GOT base address; with :option:`-fPIC` or :option:`-fPIE` , GOT offsets
  are computed with 32 bits.
  With a :samp:`bfin-elf` target, this option implies :option:`-msim`.

.. option:: -minline-plt

  Enable inlining of PLT entries in function calls to functions that are
  not known to bind locally.  It has no effect without :option:`-mfdpic`.
  It's enabled by default if optimizing for speed and compiling for
  shared libraries (i.e., :option:`-fPIC` or :option:`-fpic` ), or when an
  optimization option such as :option:`-O3` or above is present in the
  command line.

.. option:: -mTLS

  Assume a large TLS segment when generating thread-local code.

.. option:: -mtls

  Do not assume a large TLS segment when generating thread-local code.

.. option:: -mgprel-ro

  Enable the use of ``GPREL`` relocations in the FDPIC ABI for data
  that is known to be in read-only sections.  It's enabled by default,
  except for :option:`-fpic` or :option:`-fpie` : even though it may help
  make the global offset table smaller, it trades 1 instruction for 4.
  With :option:`-fPIC` or :option:`-fPIE` , it trades 3 instructions for 4,
  one of which may be shared by multiple symbols, and it avoids the need
  for a GOT entry for the referenced symbol, so it's more likely to be a
  win.  If it is not, :option:`-mno-gprel-ro` can be used to disable it.

.. option:: -multilib-library-pic

  Link with the (library, not FD) pic libraries.  It's implied by
  :option:`-mlibrary-pic` , as well as by :option:`-fPIC` and
  :option:`-fpic` without :option:`-mfdpic`.  You should never have to use
  it explicitly.

.. option:: -mlinked-fp

  Follow the EABI requirement of always creating a frame pointer whenever
  a stack frame is allocated.  This option is enabled by default and can
  be disabled with :option:`-mno-linked-fp`.

.. option:: -mlong-calls

  Use indirect addressing to call functions outside the current
  compilation unit.  This allows the functions to be placed anywhere
  within the 32-bit address space.

.. option:: -malign-labels

  Try to align labels to an 8-byte boundary by inserting NOPs into the
  previous packet.  This option only has an effect when VLIW packing
  is enabled.  It doesn't create new packets; it merely adds NOPs to
  existing ones.

.. option:: -mlibrary-pic

  Generate position-independent EABI code.

.. option:: -macc-4

  Use only the first four media accumulator registers.

.. option:: -macc-8

  Use all eight media accumulator registers.

.. option:: -mpack

  Pack VLIW instructions.

.. option:: -mno-pack

  Do not pack VLIW instructions.

.. option:: -mno-eflags

  Do not mark ABI switches in e_flags.

.. option:: -mcond-move

  Enable the use of conditional-move instructions (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-cond-move

  Disable the use of conditional-move instructions.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mscc

  Enable the use of conditional set instructions (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-scc

  Disable the use of conditional set instructions.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mcond-exec

  Enable the use of conditional execution (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-cond-exec

  Disable the use of conditional execution.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mvliw-branch

  Run a pass to pack branches into VLIW instructions (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-vliw-branch

  Do not run a pass to pack branches into VLIW instructions.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mmulti-cond-exec

  Enable optimization of ``&&`` and ``||`` in conditional execution
  (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-multi-cond-exec

  Disable optimization of ``&&`` and ``||`` in conditional execution.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mnested-cond-exec

  Enable nested conditional execution optimizations (default).

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -mno-nested-cond-exec

  Disable nested conditional execution optimizations.

  This switch is mainly for debugging the compiler and will likely be removed
  in a future version.

.. option:: -moptimize-membar

  This switch removes redundant ``membar`` instructions from the
  compiler-generated code.  It is enabled by default.

.. option:: -mno-optimize-membar, -moptimize-membar

  This switch disables the automatic removal of redundant ``membar``
  instructions from the generated code.

.. option:: -mtomcat-stats

  Cause gas to print out tomcat statistics.

.. option:: -mcpu=cpu

  Select the processor type for which to generate code.  Possible values are
  :samp:`frv`, :samp:`fr550`, :samp:`tomcat`, :samp:`fr500`, :samp:`fr450`,
  :samp:`fr405`, :samp:`fr400`, :samp:`fr300` and :samp:`simple`.

.. _gnu-linux-options:

GNU/Linux Options
^^^^^^^^^^^^^^^^^

These :samp:`-m` options are defined for GNU/Linux targets:

.. option:: -mglibc

  Use the GNU C library.  This is the default except
  on :samp:`*-*-linux-*uclibc*`, :samp:`*-*-linux-*musl*` and
  :samp:`*-*-linux-*android*` targets.

.. option:: -muclibc

  Use uClibc C library.  This is the default on
  :samp:`*-*-linux-*uclibc*` targets.

.. option:: -mmusl

  Use the musl C library.  This is the default on
  :samp:`*-*-linux-*musl*` targets.

.. option:: -mbionic

  Use Bionic C library.  This is the default on
  :samp:`*-*-linux-*android*` targets.

.. option:: -mandroid

  Compile code compatible with Android platform.  This is the default on
  :samp:`*-*-linux-*android*` targets.

  When compiling, this option enables :option:`-mbionic` , :option:`-fPIC` ,
  :option:`-fno-exceptions` and :option:`-fno-rtti` by default.  When linking,
  this option makes the GCC driver pass Android-specific options to the linker.
  Finally, this option causes the preprocessor macro ``__ANDROID__``
  to be defined.

.. option:: -tno-android-cc

  Disable compilation effects of :option:`-mandroid` , i.e., do not enable
  :option:`-mbionic` , :option:`-fPIC` , :option:`-fno-exceptions` and
  :option:`-fno-rtti` by default.

.. option:: -tno-android-ld

  Disable linking effects of :option:`-mandroid` , i.e., pass standard Linux
  linking options to the linker.

.. _h8-300-options:

H8/300 Options
^^^^^^^^^^^^^^

These :samp:`-m` options are defined for the H8/300 implementations:

.. option:: -mrelax

  Shorten some address references at link time, when possible; uses the
  linker option :option:`-relax`.  See :ref:`h8-300`, for a fuller description.

.. option:: -mh

  Generate code for the H8/300H.

.. option:: -ms

  Generate code for the H8S.

.. option:: -mn

  Generate code for the H8S and H8/300H in the normal mode.  This switch
  must be used either with :option:`-mh` or :option:`-ms`.

.. option:: -ms2600

  Generate code for the H8S/2600.  This switch must be used with :option:`-ms`.

.. option:: -mexr

  Extended registers are stored on stack before execution of function
  with monitor attribute. Default option is :option:`-mexr`.
  This option is valid only for H8S targets.

.. option:: -mno-exr, -mexr

  Extended registers are not stored on stack before execution of function 
  with monitor attribute. Default option is :option:`-mno-exr`. 
  This option is valid only for H8S targets.

.. option:: -mint32

  Make ``int`` data 32 bits by default.

.. option:: -malign-300

  On the H8/300H and H8S, use the same alignment rules as for the H8/300.
  The default for the H8/300H and H8S is to align longs and floats on
  4-byte boundaries.
  :option:`-malign-300` causes them to be aligned on 2-byte boundaries.
  This option has no effect on the H8/300.

.. _hppa-options:

HPPA Options
^^^^^^^^^^^^

.. index:: HPPA Options

These :samp:`-m` options are defined for the HPPA family of computers:

.. option:: -march=architecture-type

  Generate code for the specified architecture.  The choices for
  :samp:`{architecture-type}` are :samp:`1.0` for PA 1.0, :samp:`1.1` for PA
  1.1, and :samp:`2.0` for PA 2.0 processors.  Refer to
  /usr/lib/sched.models on an HP-UX system to determine the proper
  architecture option for your machine.  Code compiled for lower numbered
  architectures runs on higher numbered architectures, but not the
  other way around.

.. option:: -mpa-risc-1-0, -mpa-risc-1-1, -mpa-risc-2-0

  Synonyms for :option:`-march=1.0` , :option:`-march=1.1` , and :option:`-march=2.0` respectively.

.. option:: -mcaller-copies

  The caller copies function arguments passed by hidden reference.  This
  option should be used with care as it is not compatible with the default
  32-bit runtime.  However, only aggregates larger than eight bytes are
  passed by hidden reference and the option provides better compatibility
  with OpenMP.

.. option:: -mjump-in-delay

  This option is ignored and provided for compatibility purposes only.

.. option:: -mdisable-fpregs

  Prevent floating-point registers from being used in any manner.  This is
  necessary for compiling kernels that perform lazy context switching of
  floating-point registers.  If you use this option and attempt to perform
  floating-point operations, the compiler aborts.

.. option:: -mdisable-indexing

  Prevent the compiler from using indexing address modes.  This avoids some
  rather obscure problems when compiling MIG generated code under MACH.

.. option:: -mno-space-regs, -mspace-regs

  Generate code that assumes the target has no space registers.  This allows
  GCC to generate faster indirect calls and use unscaled index address modes.

  Such code is suitable for level 0 PA systems and kernels.

.. option:: -mfast-indirect-calls

  Generate code that assumes calls never cross space boundaries.  This
  allows GCC to emit code that performs faster indirect calls.

  This option does not work in the presence of shared libraries or nested
  functions.

.. option:: -mfixed-range=register-range

  Generate code treating the given register range as fixed registers.
  A fixed register is one that the register allocator cannot use.  This is
  useful when compiling kernel code.  A register range is specified as
  two registers separated by a dash.  Multiple register ranges can be
  specified separated by a comma.

.. option:: -mlong-load-store

  Generate 3-instruction load and store sequences as sometimes required by
  the HP-UX 10 linker.  This is equivalent to the :samp:`+k` option to
  the HP compilers.

.. option:: -mportable-runtime

  Use the portable calling conventions proposed by HP for ELF systems.

.. option:: -mgas

  Enable the use of assembler directives only GAS understands.

.. option:: -mschedule=cpu-type

  Schedule code according to the constraints for the machine type
  :samp:`{cpu-type}`.  The choices for :samp:`{cpu-type}` are :samp:`700`
  :samp:`7100`, :samp:`7100LC`, :samp:`7200`, :samp:`7300` and :samp:`8000`.  Refer
  to /usr/lib/sched.models on an HP-UX system to determine the
  proper scheduling option for your machine.  The default scheduling is
  :samp:`8000`.

.. option:: -mlinker-opt

  Enable the optimization pass in the HP-UX linker.  Note this makes symbolic
  debugging impossible.  It also triggers a bug in the HP-UX 8 and HP-UX 9
  linkers in which they give bogus error messages when linking some programs.

.. option:: -msoft-float

  Generate output containing library calls for floating point.
  Warning: the requisite libraries are not available for all HPPA
  targets.  Normally the facilities of the machine's usual C compiler are
  used, but this cannot be done directly in cross-compilation.  You must make
  your own arrangements to provide suitable library functions for
  cross-compilation.

  :option:`-msoft-float` changes the calling convention in the output file;
  therefore, it is only useful if you compile *all* of a program with
  this option.  In particular, you need to compile libgcc.a, the
  library that comes with GCC, with :option:`-msoft-float` in order for
  this to work.

.. option:: -msio

  Generate the predefine, ``_SIO``, for server IO.  The default is
  :option:`-mwsio`.  This generates the predefines, ``__hp9000s700``,
  ``__hp9000s700__`` and ``_WSIO``, for workstation IO.  These
  options are available under HP-UX and HI-UX.

.. option:: -mgnu-ld

  Use options specific to GNU :command:`ld`.
  This passes :option:`-shared` to :command:`ld` when
  building a shared library.  It is the default when GCC is configured,
  explicitly or implicitly, with the GNU linker.  This option does not
  affect which :command:`ld` is called; it only changes what parameters
  are passed to that :command:`ld`.
  The :command:`ld` that is called is determined by the
  :option:`--with-ld` configure option, GCC's program search path, and
  finally by the user's :envvar:`PATH`.  The linker used by GCC can be printed
  using :samp:`which `gcc -print-prog-name=ld``.  This option is only available
  on the 64-bit HP-UX GCC, i.e. configured with :samp:`hppa*64*-*-hpux*`.

.. option:: -mhp-ld

  Use options specific to HP :command:`ld`.
  This passes :option:`-b` to :command:`ld` when building
  a shared library and passes +Accept TypeMismatch to :command:`ld` on all
  links.  It is the default when GCC is configured, explicitly or
  implicitly, with the HP linker.  This option does not affect
  which :command:`ld` is called; it only changes what parameters are passed to that
  :command:`ld`.
  The :command:`ld` that is called is determined by the :option:`--with-ld`
  configure option, GCC's program search path, and finally by the user's
  :envvar:`PATH`.  The linker used by GCC can be printed using :samp:`which
  `gcc -print-prog-name=ld``.  This option is only available on the 64-bit
  HP-UX GCC, i.e. configured with :samp:`hppa*64*-*-hpux*`.

.. option:: -mlong-calls, -mno-long-calls

  Generate code that uses long call sequences.  This ensures that a call
  is always able to reach linker generated stubs.  The default is to generate
  long calls only when the distance from the call site to the beginning
  of the function or translation unit, as the case may be, exceeds a
  predefined limit set by the branch type being used.  The limits for
  normal calls are 7,600,000 and 240,000 bytes, respectively for the
  PA 2.0 and PA 1.X architectures.  Sibcalls are always limited at
  240,000 bytes.

  Distances are measured from the beginning of functions when using the
  :option:`-ffunction-sections` option, or when using the :option:`-mgas`
  and :option:`-mno-portable-runtime` options together under HP-UX with
  the SOM linker.

  It is normally not desirable to use this option as it degrades
  performance.  However, it may be useful in large applications,
  particularly when partial linking is used to build the application.

  The types of long calls used depends on the capabilities of the
  assembler and linker, and the type of code being generated.  The
  impact on systems that support long absolute calls, and long pic
  symbol-difference or pc-relative calls should be relatively small.
  However, an indirect call is used on 32-bit ELF systems in pic code
  and it is quite long.

.. option:: -munix=unix-std

  Generate compiler predefines and select a startfile for the specified
  UNIX standard.  The choices for :samp:`{unix-std}` are :samp:`93`, :samp:`95`
  and :samp:`98`.  :samp:`93` is supported on all HP-UX versions.  :samp:`95`
  is available on HP-UX 10.10 and later.  :samp:`98` is available on HP-UX
  11.11 and later.  The default values are :samp:`93` for HP-UX 10.00,
  :samp:`95` for HP-UX 10.10 though to 11.00, and :samp:`98` for HP-UX 11.11
  and later.

  :option:`-munix=93` provides the same predefines as GCC 3.3 and 3.4.
  :option:`-munix=95` provides additional predefines for ``XOPEN_UNIX``
  and ``_XOPEN_SOURCE_EXTENDED``, and the startfile unix95.o.
  :option:`-munix=98` provides additional predefines for ``_XOPEN_UNIX``,
  ``_XOPEN_SOURCE_EXTENDED``, ``_INCLUDE__STDC_A1_SOURCE`` and
  ``_INCLUDE_XOPEN_SOURCE_500``, and the startfile unix98.o.

  It is *important* to note that this option changes the interfaces
  for various library routines.  It also affects the operational behavior
  of the C library.  Thus, *extreme* care is needed in using this
  option.

  Library code that is intended to operate with more than one UNIX
  standard must test, set and restore the variable ``__xpg4_extended_mask``
  as appropriate.  Most GNU software doesn't provide this capability.

.. option:: -nolibdld

  Suppress the generation of link options to search libdld.sl when the
  :option:`-static` option is specified on HP-UX 10 and later.

.. option:: -static

  The HP-UX implementation of setlocale in libc has a dependency on
  libdld.sl.  There isn't an archive version of libdld.sl.  Thus,
  when the :option:`-static` option is specified, special link options
  are needed to resolve this dependency.

  On HP-UX 10 and later, the GCC driver adds the necessary options to
  link with libdld.sl when the :option:`-static` option is specified.
  This causes the resulting binary to be dynamic.  On the 64-bit port,
  the linkers generate dynamic binaries by default in any case.  The
  :option:`-nolibdld` option can be used to prevent the GCC driver from
  adding these link options.

.. option:: -threads

  Add support for multithreading with the :dfn:`dce thread` library
  under HP-UX.  This option sets flags for both the preprocessor and
  linker.

.. _ia-64-options:

IA-64 Options
^^^^^^^^^^^^^

.. index:: IA-64 Options

These are the :samp:`-m` options defined for the Intel IA-64 architecture.

.. option:: -mbig-endian

  Generate code for a big-endian target.  This is the default for HP-UX.

.. option:: -mlittle-endian

  Generate code for a little-endian target.  This is the default for AIX5
  and GNU/Linux.

.. option:: -mgnu-as, -mno-gnu-as

  Generate (or don't) code for the GNU assembler.  This is the default.

  .. Also, this is the default if the configure option @option{-with-gnu-as}

  .. is used.

.. option:: -mgnu-ld, -mno-gnu-ld

  Generate (or don't) code for the GNU linker.  This is the default.

  .. Also, this is the default if the configure option @option{-with-gnu-ld}

  .. is used.

.. option:: -mno-pic

  Generate code that does not use a global pointer register.  The result
  is not position independent code, and violates the IA-64 ABI.

.. option:: -mvolatile-asm-stop, -mno-volatile-asm-stop

  Generate (or don't) a stop bit immediately before and after volatile asm
  statements.

.. option:: -mregister-names, -mno-register-names

  Generate (or don't) :samp:`in`, :samp:`loc`, and :samp:`out` register names for
  the stacked registers.  This may make assembler output more readable.

.. option:: -mno-sdata, -msdata

  Disable (or enable) optimizations that use the small data section.  This may
  be useful for working around optimizer bugs.

.. option:: -mconstant-gp

  Generate code that uses a single constant global pointer value.  This is
  useful when compiling kernel code.

.. option:: -mauto-pic

  Generate code that is self-relocatable.  This implies :option:`-mconstant-gp`.
  This is useful when compiling firmware code.

.. option:: -minline-float-divide-min-latency

  Generate code for inline divides of floating-point values
  using the minimum latency algorithm.

.. option:: -minline-float-divide-max-throughput

  Generate code for inline divides of floating-point values
  using the maximum throughput algorithm.

.. option:: -mno-inline-float-divide

  Do not generate inline code for divides of floating-point values.

.. option:: -minline-int-divide-min-latency

  Generate code for inline divides of integer values
  using the minimum latency algorithm.

.. option:: -minline-int-divide-max-throughput

  Generate code for inline divides of integer values
  using the maximum throughput algorithm.

.. option:: -mno-inline-int-divide, -minline-int-divide

  Do not generate inline code for divides of integer values.

.. option:: -minline-sqrt-min-latency

  Generate code for inline square roots
  using the minimum latency algorithm.

.. option:: -minline-sqrt-max-throughput

  Generate code for inline square roots
  using the maximum throughput algorithm.

.. option:: -mno-inline-sqrt

  Do not generate inline code for ``sqrt``.

.. option:: -mfused-madd, -mno-fused-madd

  Do (don't) generate code that uses the fused multiply/add or multiply/subtract
  instructions.  The default is to use these instructions.

.. option:: -mno-dwarf2-asm, -mdwarf2-asm

  Don't (or do) generate assembler code for the DWARF line number debugging
  info.  This may be useful when not using the GNU assembler.

.. option:: -mearly-stop-bits, -mno-early-stop-bits

  Allow stop bits to be placed earlier than immediately preceding the
  instruction that triggered the stop bit.  This can improve instruction
  scheduling, but does not always do so.

.. option:: -mfixed-range=register-range

  Generate code treating the given register range as fixed registers.
  A fixed register is one that the register allocator cannot use.  This is
  useful when compiling kernel code.  A register range is specified as
  two registers separated by a dash.  Multiple register ranges can be
  specified separated by a comma.

.. option:: -mtls-size=tls-size

  Specify bit size of immediate TLS offsets.  Valid values are 14, 22, and
  64.

.. option:: -mtune=cpu-type

  Tune the instruction scheduling for a particular CPU, Valid values are
  :samp:`itanium`, :samp:`itanium1`, :samp:`merced`, :samp:`itanium2`,
  and :samp:`mckinley`.

.. option:: -milp32, -mlp64

  Generate code for a 32-bit or 64-bit environment.
  The 32-bit environment sets int, long and pointer to 32 bits.
  The 64-bit environment sets int to 32 bits and long and pointer
  to 64 bits.  These are HP-UX specific flags.

.. option:: -mno-sched-br-data-spec, -msched-br-data-spec

  (Dis/En)able data speculative scheduling before reload.
  This results in generation of ``ld.a`` instructions and
  the corresponding check instructions (``ld.c`` / ``chk.a``).
  The default setting is disabled.

.. option:: -msched-ar-data-spec, -mno-sched-ar-data-spec

  (En/Dis)able data speculative scheduling after reload.
  This results in generation of ``ld.a`` instructions and
  the corresponding check instructions (``ld.c`` / ``chk.a``).
  The default setting is enabled.

.. option:: -mno-sched-control-spec, -msched-control-spec

  (Dis/En)able control speculative scheduling.  This feature is
  available only during region scheduling (i.e. before reload).
  This results in generation of the ``ld.s`` instructions and
  the corresponding check instructions ``chk.s``.
  The default setting is disabled.

.. option:: -msched-br-in-data-spec, -mno-sched-br-in-data-spec

  (En/Dis)able speculative scheduling of the instructions that
  are dependent on the data speculative loads before reload.
  This is effective only with :option:`-msched-br-data-spec` enabled.
  The default setting is enabled.

.. option:: -msched-ar-in-data-spec, -mno-sched-ar-in-data-spec

  (En/Dis)able speculative scheduling of the instructions that
  are dependent on the data speculative loads after reload.
  This is effective only with :option:`-msched-ar-data-spec` enabled.
  The default setting is enabled.

.. option:: -msched-in-control-spec, -mno-sched-in-control-spec

  (En/Dis)able speculative scheduling of the instructions that
  are dependent on the control speculative loads.
  This is effective only with :option:`-msched-control-spec` enabled.
  The default setting is enabled.

.. option:: -mno-sched-prefer-non-data-spec-insns
.. option:: -msched-prefer-non-data-spec-insns

  If enabled, data-speculative instructions are chosen for schedule
  only if there are no other choices at the moment.  This makes
  the use of the data speculation much more conservative.
  The default setting is disabled.

.. option:: -mno-sched-prefer-non-control-spec-insns
.. option:: -msched-prefer-non-control-spec-insns

  If enabled, control-speculative instructions are chosen for schedule
  only if there are no other choices at the moment.  This makes
  the use of the control speculation much more conservative.
  The default setting is disabled.

.. option:: -mno-sched-count-spec-in-critical-path
.. option:: -msched-count-spec-in-critical-path

  If enabled, speculative dependencies are considered during
  computation of the instructions priorities.  This makes the use of the
  speculation a bit more conservative.
  The default setting is disabled.

.. option:: -msched-spec-ldc

  Use a simple data speculation check.  This option is on by default.

.. option:: -msched-control-spec-ldc, -msched-spec-ldc

  Use a simple check for control speculation.  This option is on by default.

.. option:: -msched-stop-bits-after-every-cycle

  Place a stop bit after every cycle when scheduling.  This option is on
  by default.

.. option:: -msched-fp-mem-deps-zero-cost

  Assume that floating-point stores and loads are not likely to cause a conflict
  when placed into the same instruction group.  This option is disabled by
  default.

.. option:: -msel-sched-dont-check-control-spec

  Generate checks for control speculation in selective scheduling.
  This flag is disabled by default.

.. option:: -msched-max-memory-insns=max-insns

  Limit on the number of memory insns per instruction group, giving lower
  priority to subsequent memory insns attempting to schedule in the same
  instruction group. Frequently useful to prevent cache bank conflicts.
  The default value is 1.

.. option:: -msched-max-memory-insns-hard-limit

  Makes the limit specified by msched-max-memory-insns a hard limit,
  disallowing more than that number in an instruction group.
  Otherwise, the limit is 'soft', meaning that non-memory operations
  are preferred when the limit is reached, but memory operations may still
  be scheduled.

.. _lm32-options:

LM32 Options
^^^^^^^^^^^^

.. index:: LM32 options

These :option:`-m` options are defined for the LatticeMico32 architecture:

.. option:: -mbarrel-shift-enabled

  Enable barrel-shift instructions.

.. option:: -mdivide-enabled

  Enable divide and modulus instructions.

.. option:: -mmultiply-enabled, -multiply-enabled

  Enable multiply instructions.

.. option:: -msign-extend-enabled

  Enable sign extend instructions.

.. option:: -muser-enabled

  Enable user-defined instructions.

.. _m32c-options:

M32C Options
^^^^^^^^^^^^

.. index:: M32C options

.. option:: -mcpu=name

  Select the CPU for which code is generated.  :samp:`{name}` may be one of
  :samp:`r8c` for the R8C/Tiny series, :samp:`m16c` for the M16C (up to
  /60) series, :samp:`m32cm` for the M16C/80 series, or :samp:`m32c` for
  the M32C/80 series.

.. option:: -msim

  Specifies that the program will be run on the simulator.  This causes
  an alternate runtime library to be linked in which supports, for
  example, file I/O.  You must not use this option when generating
  programs that will run on real hardware; you must provide your own
  runtime library for whatever I/O functions are needed.

.. option:: -memregs=number

  Specifies the number of memory-based pseudo-registers GCC uses
  during code generation.  These pseudo-registers are used like real
  registers, so there is a tradeoff between GCC's ability to fit the
  code into available registers, and the performance penalty of using
  memory instead of registers.  Note that all modules in a program must
  be compiled with the same value for this option.  Because of that, you
  must not use this option with GCC's default runtime libraries.

.. _m32r-d-options:

M32R/D Options
^^^^^^^^^^^^^^

.. index:: M32R/D options

These :option:`-m` options are defined for Renesas M32R/D architectures:

.. option:: -m32r2

  Generate code for the M32R/2.

.. option:: -m32rx

  Generate code for the M32R/X.

.. option:: -m32r

  Generate code for the M32R.  This is the default.

.. option:: -mmodel=small

  Assume all objects live in the lower 16MB of memory (so that their addresses
  can be loaded with the ``ld24`` instruction), and assume all subroutines
  are reachable with the ``bl`` instruction.
  This is the default.

  The addressability of a particular object can be set with the
  ``model`` attribute.

.. option:: -mmodel=medium

  Assume objects may be anywhere in the 32-bit address space (the compiler
  generates ``seth/add3`` instructions to load their addresses), and
  assume all subroutines are reachable with the ``bl`` instruction.

.. option:: -mmodel=large

  Assume objects may be anywhere in the 32-bit address space (the compiler
  generates ``seth/add3`` instructions to load their addresses), and
  assume subroutines may not be reachable with the ``bl`` instruction
  (the compiler generates the much slower ``seth/add3/jl``
  instruction sequence).

.. option:: -msdata=none

  Disable use of the small data area.  Variables are put into
  one of ``.data``, ``.bss``, or ``.rodata`` (unless the
  ``section`` attribute has been specified).
  This is the default.

  The small data area consists of sections ``.sdata`` and ``.sbss``.
  Objects may be explicitly put in the small data area with the
  ``section`` attribute using one of these sections.

.. option:: -msdata=sdata

  Put small global and static data in the small data area, but do not
  generate special code to reference them.

.. option:: -msdata=use

  Put small global and static data in the small data area, and generate
  special instructions to reference them.

.. option:: -G num, -G

  .. index:: smaller data references

  Put global and static objects less than or equal to :samp:`{num}` bytes
  into the small data or BSS sections instead of the normal data or BSS
  sections.  The default value of :samp:`{num}` is 8.
  The :option:`-msdata` option must be set to one of :samp:`sdata` or :samp:`use`
  for this option to have any effect.

  All modules should be compiled with the same :option:`-G `:samp:`{num}` value.
  Compiling with different values of :samp:`{num}` may or may not work; if it
  doesn't the linker gives an error message-incorrect code is not
  generated.

.. option:: -mdebug

  Makes the M32R-specific code in the compiler display some statistics
  that might help in debugging programs.

.. option:: -malign-loops

  Align all loops to a 32-byte boundary.

.. option:: -mno-align-loops

  Do not enforce a 32-byte alignment for loops.  This is the default.

.. option:: -missue-rate=number

  .. index:: missue-rate=number

  Issue :samp:`{number}` instructions per cycle.  :samp:`{number}` can only be 1
  or 2.

.. option:: -mbranch-cost=number

  .. index:: mbranch-cost=number

  :samp:`{number}` can only be 1 or 2.  If it is 1 then branches are
  preferred over conditional code, if it is 2, then the opposite applies.

.. option:: -mflush-trap=number

  .. index:: mflush-trap=number

  Specifies the trap number to use to flush the cache.  The default is
  12.  Valid numbers are between 0 and 15 inclusive.

.. option:: -mno-flush-trap

  Specifies that the cache cannot be flushed by using a trap.

.. option:: -mflush-func=name

  .. index:: mflush-func=name

  Specifies the name of the operating system function to call to flush
  the cache.  The default is :samp:`_flush_cache`, but a function call
  is only used if a trap is not available.

.. option:: -mno-flush-func

  Indicates that there is no OS function for flushing the cache.

.. _m680x0-options:

M680x0 Options
^^^^^^^^^^^^^^

.. index:: M680x0 options

These are the :samp:`-m` options defined for M680x0 and ColdFire processors.
The default settings depend on which architecture was selected when
the compiler was configured; the defaults for the most common choices
are given below.

.. option:: -march=arch

  Generate code for a specific M680x0 or ColdFire instruction set
  architecture.  Permissible values of :samp:`{arch}` for M680x0
  architectures are: :samp:`68000`, :samp:`68010`, :samp:`68020`,
  :samp:`68030`, :samp:`68040`, :samp:`68060` and :samp:`cpu32`.  ColdFire
  architectures are selected according to Freescale's ISA classification
  and the permissible values are: :samp:`isaa`, :samp:`isaaplus`,
  :samp:`isab` and :samp:`isac`.

  GCC defines a macro ``__mcfarch__`` whenever it is generating
  code for a ColdFire target.  The :samp:`{arch}` in this macro is one of the
  :option:`-march` arguments given above.

  When used together, :option:`-march` and :option:`-mtune` select code
  that runs on a family of similar processors but that is optimized
  for a particular microarchitecture.

.. option:: -mcpu=cpu

  Generate code for a specific M680x0 or ColdFire processor.
  The M680x0 :samp:`{cpu}` s are: :samp:`68000`, :samp:`68010`, :samp:`68020`,
  :samp:`68030`, :samp:`68040`, :samp:`68060`, :samp:`68302`, :samp:`68332`
  and :samp:`cpu32`.  The ColdFire :samp:`{cpu}` s are given by the table
  below, which also classifies the CPUs into families:

  =============  ========================================================================================================================================================================
  Family         :samp:`-mcpu` arguments
  =============  ========================================================================================================================================================================
  :samp:`51`     :samp:`51` :samp:`51ac` :samp:`51ag` :samp:`51cn` :samp:`51em` :samp:`51je` :samp:`51jf` :samp:`51jg` :samp:`51jm` :samp:`51mm` :samp:`51qe` :samp:`51qm`
  :samp:`5206`   :samp:`5202` :samp:`5204` :samp:`5206`
  :samp:`5206e`  :samp:`5206e`
  :samp:`5208`   :samp:`5207` :samp:`5208`
  :samp:`5211a`  :samp:`5210a` :samp:`5211a`
  :samp:`5213`   :samp:`5211` :samp:`5212` :samp:`5213`
  :samp:`5216`   :samp:`5214` :samp:`5216`
  :samp:`52235`  :samp:`52230` :samp:`52231` :samp:`52232` :samp:`52233` :samp:`52234` :samp:`52235`
  :samp:`5225`   :samp:`5224` :samp:`5225`
  :samp:`52259`  :samp:`52252` :samp:`52254` :samp:`52255` :samp:`52256` :samp:`52258` :samp:`52259`
  :samp:`5235`   :samp:`5232` :samp:`5233` :samp:`5234` :samp:`5235` :samp:`523x`
  :samp:`5249`   :samp:`5249`
  :samp:`5250`   :samp:`5250`
  :samp:`5271`   :samp:`5270` :samp:`5271`
  :samp:`5272`   :samp:`5272`
  :samp:`5275`   :samp:`5274` :samp:`5275`
  :samp:`5282`   :samp:`5280` :samp:`5281` :samp:`5282` :samp:`528x`
  :samp:`53017`  :samp:`53011` :samp:`53012` :samp:`53013` :samp:`53014` :samp:`53015` :samp:`53016` :samp:`53017`
  :samp:`5307`   :samp:`5307`
  :samp:`5329`   :samp:`5327` :samp:`5328` :samp:`5329` :samp:`532x`
  :samp:`5373`   :samp:`5372` :samp:`5373` :samp:`537x`
  :samp:`5407`   :samp:`5407`
  :samp:`5475`   :samp:`5470` :samp:`5471` :samp:`5472` :samp:`5473` :samp:`5474` :samp:`5475` :samp:`547x` :samp:`5480` :samp:`5481` :samp:`5482` :samp:`5483` :samp:`5484` :samp:`5485`
  =============  ========================================================================================================================================================================
  :option:`-mcpu`:samp:`={cpu}` overrides :option:`-march`:samp:`={arch}` if
  :samp:`{arch}` is compatible with :samp:`{cpu}`.  Other combinations of
  :option:`-mcpu` and :option:`-march` are rejected.

  GCC defines the macro ``__mcf_cpu_cpu`` when ColdFire target
  :samp:`{cpu}` is selected.  It also defines ``__mcf_family_family``,
  where the value of :samp:`{family}` is given by the table above.

.. option:: -mtune=tune

  Tune the code for a particular microarchitecture within the
  constraints set by :option:`-march` and :option:`-mcpu`.
  The M680x0 microarchitectures are: :samp:`68000`, :samp:`68010`,
  :samp:`68020`, :samp:`68030`, :samp:`68040`, :samp:`68060`
  and :samp:`cpu32`.  The ColdFire microarchitectures
  are: :samp:`cfv1`, :samp:`cfv2`, :samp:`cfv3`, :samp:`cfv4` and :samp:`cfv4e`.

  You can also use :option:`-mtune=68020-40` for code that needs
  to run relatively well on 68020, 68030 and 68040 targets.
  :option:`-mtune=68020-60` is similar but includes 68060 targets
  as well.  These two options select the same tuning decisions as
  :option:`-m68020-40` and :option:`-m68020-60` respectively.

  GCC defines the macros ``__mcarch`` and ``__mcarch__``
  when tuning for 680x0 architecture :samp:`{arch}`.  It also defines
  ``mcarch`` unless either :option:`-ansi` or a non-GNU :option:`-std`
  option is used.  If GCC is tuning for a range of architectures,
  as selected by :option:`-mtune=68020-40` or :option:`-mtune=68020-60` ,
  it defines the macros for every architecture in the range.

  GCC also defines the macro ``__muarch__`` when tuning for
  ColdFire microarchitecture :samp:`{uarch}` , where :samp:`{uarch}` is one
  of the arguments given above.

.. option:: -m68000, -mc68000

  Generate output for a 68000.  This is the default
  when the compiler is configured for 68000-based systems.
  It is equivalent to :option:`-march=68000`.

  Use this option for microcontrollers with a 68000 or EC000 core,
  including the 68008, 68302, 68306, 68307, 68322, 68328 and 68356.

.. option:: -m68010

  Generate output for a 68010.  This is the default
  when the compiler is configured for 68010-based systems.
  It is equivalent to :option:`-march=68010`.

.. option:: -m68020, -mc68020

  Generate output for a 68020.  This is the default
  when the compiler is configured for 68020-based systems.
  It is equivalent to :option:`-march=68020`.

.. option:: -m68030

  Generate output for a 68030.  This is the default when the compiler is
  configured for 68030-based systems.  It is equivalent to
  :option:`-march=68030`.

.. option:: -m68040

  Generate output for a 68040.  This is the default when the compiler is
  configured for 68040-based systems.  It is equivalent to
  :option:`-march=68040`.

  This option inhibits the use of 68881/68882 instructions that have to be
  emulated by software on the 68040.  Use this option if your 68040 does not
  have code to emulate those instructions.

.. option:: -m68060

  Generate output for a 68060.  This is the default when the compiler is
  configured for 68060-based systems.  It is equivalent to
  :option:`-march=68060`.

  This option inhibits the use of 68020 and 68881/68882 instructions that
  have to be emulated by software on the 68060.  Use this option if your 68060
  does not have code to emulate those instructions.

.. option:: -mcpu32

  Generate output for a CPU32.  This is the default
  when the compiler is configured for CPU32-based systems.
  It is equivalent to :option:`-march=cpu32`.

  Use this option for microcontrollers with a
  CPU32 or CPU32+ core, including the 68330, 68331, 68332, 68333, 68334,
  68336, 68340, 68341, 68349 and 68360.

.. option:: -m5200

  Generate output for a 520X ColdFire CPU.  This is the default
  when the compiler is configured for 520X-based systems.
  It is equivalent to :option:`-mcpu=5206` , and is now deprecated
  in favor of that option.

  Use this option for microcontroller with a 5200 core, including
  the MCF5202, MCF5203, MCF5204 and MCF5206.

.. option:: -m5206e

  Generate output for a 5206e ColdFire CPU.  The option is now
  deprecated in favor of the equivalent :option:`-mcpu=5206e`.

.. option:: -m528x

  Generate output for a member of the ColdFire 528X family.
  The option is now deprecated in favor of the equivalent
  :option:`-mcpu=528x`.

.. option:: -m5307

  Generate output for a ColdFire 5307 CPU.  The option is now deprecated
  in favor of the equivalent :option:`-mcpu=5307`.

.. option:: -m5407

  Generate output for a ColdFire 5407 CPU.  The option is now deprecated
  in favor of the equivalent :option:`-mcpu=5407`.

.. option:: -mcfv4e

  Generate output for a ColdFire V4e family CPU (e.g. 547x/548x).
  This includes use of hardware floating-point instructions.
  The option is equivalent to :option:`-mcpu=547x` , and is now
  deprecated in favor of that option.

.. option:: -m68020-40

  Generate output for a 68040, without using any of the new instructions.
  This results in code that can run relatively efficiently on either a
  68020/68881 or a 68030 or a 68040.  The generated code does use the
  68881 instructions that are emulated on the 68040.

  The option is equivalent to :option:`-march=68020` :option:`-mtune=68020-40`.

.. option:: -m68020-60

  Generate output for a 68060, without using any of the new instructions.
  This results in code that can run relatively efficiently on either a
  68020/68881 or a 68030 or a 68040.  The generated code does use the
  68881 instructions that are emulated on the 68060.

  The option is equivalent to :option:`-march=68020` :option:`-mtune=68020-60`.

.. option:: -mhard-float, -m68881

  Generate floating-point instructions.  This is the default for 68020
  and above, and for ColdFire devices that have an FPU.  It defines the
  macro ``__HAVE_68881__`` on M680x0 targets and ``__mcffpu__``
  on ColdFire targets.

.. option:: -msoft-float

  Do not generate floating-point instructions; use library calls instead.
  This is the default for 68000, 68010, and 68832 targets.  It is also
  the default for ColdFire devices that have no FPU.

.. option:: -mdiv, -mno-div

  Generate (do not generate) ColdFire hardware divide and remainder
  instructions.  If :option:`-march` is used without :option:`-mcpu` ,
  the default is 'on' for ColdFire architectures and 'off' for M680x0
  architectures.  Otherwise, the default is taken from the target CPU
  (either the default CPU, or the one specified by :option:`-mcpu` ).  For
  example, the default is 'off' for :option:`-mcpu=5206` and 'on' for
  :option:`-mcpu=5206e`.

  GCC defines the macro ``__mcfhwdiv__`` when this option is enabled.

.. option:: -mshort

  Consider type ``int`` to be 16 bits wide, like ``short int``.
  Additionally, parameters passed on the stack are also aligned to a
  16-bit boundary even on targets whose API mandates promotion to 32-bit.

.. option:: -mno-short

  Do not consider type ``int`` to be 16 bits wide.  This is the default.

.. option:: -mnobitfield, -mno-bitfield

  Do not use the bit-field instructions.  The :option:`-m68000` , :option:`-mcpu32`
  and :option:`-m5200` options imply :option:`-mnobitfield`.

.. option:: -mbitfield

  Do use the bit-field instructions.  The :option:`-m68020` option implies
  :option:`-mbitfield`.  This is the default if you use a configuration
  designed for a 68020.

.. option:: -mrtd

  Use a different function-calling convention, in which functions
  that take a fixed number of arguments return with the ``rtd``
  instruction, which pops their arguments while returning.  This
  saves one instruction in the caller since there is no need to pop
  the arguments there.

  This calling convention is incompatible with the one normally
  used on Unix, so you cannot use it if you need to call libraries
  compiled with the Unix compiler.

  Also, you must provide function prototypes for all functions that
  take variable numbers of arguments (including ``printf``);
  otherwise incorrect code is generated for calls to those
  functions.

  In addition, seriously incorrect code results if you call a
  function with too many arguments.  (Normally, extra arguments are
  harmlessly ignored.)

  The ``rtd`` instruction is supported by the 68010, 68020, 68030,
  68040, 68060 and CPU32 processors, but not by the 68000 or 5200.

  The default is :option:`-mno-rtd`.

.. option:: -malign-int, -mno-align-int, -mpcrel

  Control whether GCC aligns ``int``, ``long``, ``long long``,
  ``float``, ``double``, and ``long double`` variables on a 32-bit
  boundary ( :option:`-malign-int` ) or a 16-bit boundary ( :option:`-mno-align-int` ).
  Aligning variables on 32-bit boundaries produces code that runs somewhat
  faster on processors with 32-bit busses at the expense of more memory.

  Warning: if you use the :option:`-malign-int` switch, GCC
  aligns structures containing the above types differently than
  most published application binary interface specifications for the m68k.

  Use the pc-relative addressing mode of the 68000 directly, instead of
  using a global offset table.  At present, this option implies :option:`-fpic` ,
  allowing at most a 16-bit offset for pc-relative addressing.  :option:`-fPIC` is
  not presently supported with :option:`-mpcrel` , though this could be supported for
  68020 and higher processors.

.. option:: -mno-strict-align, -mstrict-align

  Do not (do) assume that unaligned memory references are handled by
  the system.

``-msep-data``
  Generate code that allows the data segment to be located in a different
  area of memory from the text segment.  This allows for execute-in-place in
  an environment without virtual memory management.  This option implies
  :option:`-fPIC`.

``-mno-sep-data``
  Generate code that assumes that the data segment follows the text segment.
  This is the default.

``-mid-shared-library``
  Generate code that supports shared libraries via the library ID method.
  This allows for execute-in-place and shared libraries in an environment
  without virtual memory management.  This option implies :option:`-fPIC`.

``-mno-id-shared-library``
  Generate code that doesn't assume ID-based shared libraries are being used.
  This is the default.

``-mshared-library-id=n``
  Specifies the identification number of the ID-based shared library being
  compiled.  Specifying a value of 0 generates more compact code; specifying
  other values forces the allocation of that number to the current
  library, but is no more space- or time-efficient than omitting this option.

.. option:: -mxgot, -mno-xgot

  When generating position-independent code for ColdFire, generate code
  that works if the GOT has more than 8192 entries.  This code is
  larger and slower than code generated without this option.  On M680x0
  processors, this option is not needed; :option:`-fPIC` suffices.

  GCC normally uses a single instruction to load values from the GOT.
  While this is relatively efficient, it only works if the GOT
  is smaller than about 64k.  Anything larger causes the linker
  to report an error such as:

  .. index:: relocation truncated to fit (ColdFire)

  .. code-block:: c++

    relocation truncated to fit: R_68K_GOT16O foobar

  If this happens, you should recompile your code with :option:`-mxgot`.
  It should then work with very large GOTs.  However, code generated with
  :option:`-mxgot` is less efficient, since it takes 4 instructions to fetch
  the value of a global symbol.

  Note that some linkers, including newer versions of the GNU linker,
  can create multiple GOTs and sort GOT entries.  If you have such a linker,
  you should only need to use :option:`-mxgot` when compiling a single
  object file that accesses more than 8192 GOT entries.  Very few do.

  These options have no effect unless GCC is generating
  position-independent code.

.. option:: -mlong-jump-table-offsets

  Use 32-bit offsets in ``switch`` tables.  The default is to use
  16-bit offsets.

.. _mcore-options:

MCore Options
^^^^^^^^^^^^^

.. index:: MCore options

These are the :samp:`-m` options defined for the Motorola M*Core
processors.

.. option:: -mhardlit, -mno-hardlit

  Inline constants into the code stream if it can be done in two
  instructions or less.

.. option:: -mdiv, -mno-div

  Use the divide instruction.  (Enabled by default).

.. option:: -mrelax-immediate, -mno-relax-immediate

  Allow arbitrary-sized immediates in bit operations.

.. option:: -mwide-bitfields, -mno-wide-bitfields

  Always treat bit-fields as ``int``-sized.

.. option:: -m4byte-functions, -mno-4byte-functions

  Force all functions to be aligned to a 4-byte boundary.

.. option:: -mcallgraph-data, -mno-callgraph-data

  Emit callgraph information.

.. option:: -mslow-bytes, -mno-slow-bytes

  Prefer word access when reading byte quantities.

.. option:: -mlittle-endian, -mbig-endian

  Generate code for a little-endian target.

.. option:: -m210, -m340

  Generate code for the 210 processor.

.. option:: -mno-lsim

  Assume that runtime support has been provided and so omit the
  simulator library (libsim.a) from the linker command line.

.. option:: -mstack-increment=size

  Set the maximum amount for a single stack increment operation.  Large
  values can increase the speed of programs that contain functions
  that need a large amount of stack space, but they can also trigger a
  segmentation fault if the stack is extended too much.  The default
  value is 0x1000.

.. _mep-options:

MeP Options
^^^^^^^^^^^

.. index:: MeP options

.. option:: -mabsdiff

  Enables the ``abs`` instruction, which is the absolute difference
  between two registers.

.. option:: -mall-opts

  Enables all the optional instructions-average, multiply, divide, bit
  operations, leading zero, absolute difference, min/max, clip, and
  saturation.

.. option:: -maverage

  Enables the ``ave`` instruction, which computes the average of two
  registers.

.. option:: -mbased=n

  Variables of size :samp:`{n}` bytes or smaller are placed in the
  ``.based`` section by default.  Based variables use the ``$tp``
  register as a base register, and there is a 128-byte limit to the
  ``.based`` section.

.. option:: -mbitops

  Enables the bit operation instructions-bit test (``btstm``), set
  (``bsetm``), clear (``bclrm``), invert (``bnotm``), and
  test-and-set (``tas``).

.. option:: -mc=name

  Selects which section constant data is placed in.  :samp:`{name}` may
  be :samp:`tiny`, :samp:`near`, or :samp:`far`.

.. option:: -mclip

  Enables the ``clip`` instruction.  Note that :option:`-mclip` is not
  useful unless you also provide :option:`-mminmax`.

.. option:: -mconfig=name

  Selects one of the built-in core configurations.  Each MeP chip has
  one or more modules in it; each module has a core CPU and a variety of
  coprocessors, optional instructions, and peripherals.  The
  ``MeP-Integrator`` tool, not part of GCC, provides these
  configurations through this option; using this option is the same as
  using all the corresponding command-line options.  The default
  configuration is :samp:`default`.

.. option:: -mcop

  Enables the coprocessor instructions.  By default, this is a 32-bit
  coprocessor.  Note that the coprocessor is normally enabled via the
  :option:`-mconfig` = option.

.. option:: -mcop32

  Enables the 32-bit coprocessor's instructions.

.. option:: -mcop64

  Enables the 64-bit coprocessor's instructions.

.. option:: -mivc2

  Enables IVC2 scheduling.  IVC2 is a 64-bit VLIW coprocessor.

.. option:: -mdc

  Causes constant variables to be placed in the ``.near`` section.

.. option:: -mdiv

  Enables the ``div`` and ``divu`` instructions.

.. option:: -meb

  Generate big-endian code.

.. option:: -mel

  Generate little-endian code.

.. option:: -mio-volatile

  Tells the compiler that any variable marked with the ``io``
  attribute is to be considered volatile.

.. option:: -ml

  Causes variables to be assigned to the ``.far`` section by default.

.. option:: -mleadz

  Enables the ``leadz`` (leading zero) instruction.

.. option:: -mm

  Causes variables to be assigned to the ``.near`` section by default.

.. option:: -mminmax

  Enables the ``min`` and ``max`` instructions.

.. option:: -mmult

  Enables the multiplication and multiply-accumulate instructions.

.. option:: -mno-opts

  Disables all the optional instructions enabled by :option:`-mall-opts`.

.. option:: -mrepeat

  Enables the ``repeat`` and ``erepeat`` instructions, used for
  low-overhead looping.

.. option:: -ms

  Causes all variables to default to the ``.tiny`` section.  Note
  that there is a 65536-byte limit to this section.  Accesses to these
  variables use the ``%gp`` base register.

.. option:: -msatur

  Enables the saturation instructions.  Note that the compiler does not
  currently generate these itself, but this option is included for
  compatibility with other tools, like ``as``.

.. option:: -msdram

  Link the SDRAM-based runtime instead of the default ROM-based runtime.

.. option:: -msim

  Link the simulator run-time libraries.

.. option:: -msimnovec

  Link the simulator runtime libraries, excluding built-in support
  for reset and exception vectors and tables.

.. option:: -mtf

  Causes all functions to default to the ``.far`` section.  Without
  this option, functions default to the ``.near`` section.

.. option:: -mtiny=n

  Variables that are :samp:`{n}` bytes or smaller are allocated to the
  ``.tiny`` section.  These variables use the ``$gp`` base
  register.  The default for this option is 4, but note that there's a
  65536-byte limit to the ``.tiny`` section.

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
  Supported values are in the format :samp:`v :samp:`{X}`.:samp:`{YY}`.:samp:`{Z}``,
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

  Use GP-relative ``.sdata``/``.sbss`` sections.

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

  .. _mips-options:

MIPS Options
^^^^^^^^^^^^

.. index:: MIPS options

.. option:: -EB

  Generate big-endian code.

.. option:: -EL

  Generate little-endian code.  This is the default for :samp:`mips*el-*-*`
  configurations.

.. option:: -march=arch

  Generate code that runs on :samp:`{arch}` , which can be the name of a
  generic MIPS ISA, or the name of a particular processor.
  The ISA names are:
  :samp:`mips1`, :samp:`mips2`, :samp:`mips3`, :samp:`mips4`,
  :samp:`mips32`, :samp:`mips32r2`, :samp:`mips32r3`, :samp:`mips32r5`,
  :samp:`mips32r6`, :samp:`mips64`, :samp:`mips64r2`, :samp:`mips64r3`,
  :samp:`mips64r5` and :samp:`mips64r6`.
  The processor names are:
  :samp:`4kc`, :samp:`4km`, :samp:`4kp`, :samp:`4ksc`,
  :samp:`4kec`, :samp:`4kem`, :samp:`4kep`, :samp:`4ksd`,
  :samp:`5kc`, :samp:`5kf`,
  :samp:`20kc`,
  :samp:`24kc`, :samp:`24kf2_1`, :samp:`24kf1_1`,
  :samp:`24kec`, :samp:`24kef2_1`, :samp:`24kef1_1`,
  :samp:`34kc`, :samp:`34kf2_1`, :samp:`34kf1_1`, :samp:`34kn`,
  :samp:`74kc`, :samp:`74kf2_1`, :samp:`74kf1_1`, :samp:`74kf3_2`,
  :samp:`1004kc`, :samp:`1004kf2_1`, :samp:`1004kf1_1`,
  :samp:`i6400`, :samp:`i6500`,
  :samp:`interaptiv`,
  :samp:`loongson2e`, :samp:`loongson2f`, :samp:`loongson3a`, :samp:`gs464`,
  :samp:`gs464e`, :samp:`gs264e`,
  :samp:`m4k`,
  :samp:`m14k`, :samp:`m14kc`, :samp:`m14ke`, :samp:`m14kec`,
  :samp:`m5100`, :samp:`m5101`,
  :samp:`octeon`, :samp:`octeon+`, :samp:`octeon2`, :samp:`octeon3`,
  :samp:`orion`,
  :samp:`p5600`, :samp:`p6600`,
  :samp:`r2000`, :samp:`r3000`, :samp:`r3900`, :samp:`r4000`, :samp:`r4400`,
  :samp:`r4600`, :samp:`r4650`, :samp:`r4700`, :samp:`r5900`,
  :samp:`r6000`, :samp:`r8000`,
  :samp:`rm7000`, :samp:`rm9000`,
  :samp:`r10000`, :samp:`r12000`, :samp:`r14000`, :samp:`r16000`,
  :samp:`sb1`,
  :samp:`sr71000`,
  :samp:`vr4100`, :samp:`vr4111`, :samp:`vr4120`, :samp:`vr4130`, :samp:`vr4300`,
  :samp:`vr5000`, :samp:`vr5400`, :samp:`vr5500`,
  :samp:`xlr` and :samp:`xlp`.
  The special value :samp:`from-abi` selects the
  most compatible architecture for the selected ABI (that is,
  :samp:`mips1` for 32-bit ABIs and :samp:`mips3` for 64-bit ABIs).

  The native Linux/GNU toolchain also supports the value :samp:`native`,
  which selects the best architecture option for the host processor.
  :option:`-march=native` has no effect if GCC does not recognize
  the processor.

  In processor names, a final :samp:`000` can be abbreviated as :samp:`k`
  (for example, :option:`-march=r2k` ).  Prefixes are optional, and
  :samp:`vr` may be written :samp:`r`.

  Names of the form :samp:`:samp:`{n}` f2_1` refer to processors with
  FPUs clocked at half the rate of the core, names of the form
  :samp:`:samp:`{n}` f1_1` refer to processors with FPUs clocked at the same
  rate as the core, and names of the form :samp:`:samp:`{n}` f3_2` refer to
  processors with FPUs clocked a ratio of 3:2 with respect to the core.
  For compatibility reasons, :samp:`:samp:`{n}` f` is accepted as a synonym
  for :samp:`:samp:`{n}` f2_1` while :samp:`:samp:`{n}` x` and :samp:`:samp:`{b}` fx` are
  accepted as synonyms for :samp:`:samp:`{n}` f1_1`.

  GCC defines two macros based on the value of this option.  The first
  is ``_MIPS_ARCH``, which gives the name of target architecture, as
  a string.  The second has the form ``_MIPS_ARCH_foo``,
  where :samp:`{foo}` is the capitalized value of ``_MIPS_ARCH``.
  For example, :option:`-march=r2000` sets ``_MIPS_ARCH``
  to ``"r2000"`` and defines the macro ``_MIPS_ARCH_R2000``.

  Note that the ``_MIPS_ARCH`` macro uses the processor names given
  above.  In other words, it has the full prefix and does not
  abbreviate :samp:`000` as :samp:`k`.  In the case of :samp:`from-abi`,
  the macro names the resolved architecture (either ``"mips1"`` or
  ``"mips3"``).  It names the default architecture when no
  :option:`-march` option is given.

.. option:: -mtune=arch

  Optimize for :samp:`{arch}`.  Among other things, this option controls
  the way instructions are scheduled, and the perceived cost of arithmetic
  operations.  The list of :samp:`{arch}` values is the same as for
  :option:`-march`.

  When this option is not used, GCC optimizes for the processor
  specified by :option:`-march`.  By using :option:`-march` and
  :option:`-mtune` together, it is possible to generate code that
  runs on a family of processors, but optimize the code for one
  particular member of that family.

  :option:`-mtune` defines the macros ``_MIPS_TUNE`` and
  ``_MIPS_TUNE_foo``, which work in the same way as the
  :option:`-march` ones described above.

.. option:: -mips1

  Equivalent to :option:`-march=mips1`.

.. option:: -mips2

  Equivalent to :option:`-march=mips2`.

.. option:: -mips3

  Equivalent to :option:`-march=mips3`.

.. option:: -mips4

  Equivalent to :option:`-march=mips4`.

.. option:: -mips32

  Equivalent to :option:`-march=mips32`.

.. option:: -mips32r3

  Equivalent to :option:`-march=mips32r3`.

.. option:: -mips32r5

  Equivalent to :option:`-march=mips32r5`.

.. option:: -mips32r6

  Equivalent to :option:`-march=mips32r6`.

.. option:: -mips64

  Equivalent to :option:`-march=mips64`.

.. option:: -mips64r2

  Equivalent to :option:`-march=mips64r2`.

.. option:: -mips64r3

  Equivalent to :option:`-march=mips64r3`.

.. option:: -mips64r5

  Equivalent to :option:`-march=mips64r5`.

.. option:: -mips64r6

  Equivalent to :option:`-march=mips64r6`.

.. option:: -mips16, -mno-mips16

  Generate (do not generate) MIPS16 code.  If GCC is targeting a
  MIPS32 or MIPS64 architecture, it makes use of the MIPS16e ASE.

  MIPS16 code generation can also be controlled on a per-function basis
  by means of ``mips16`` and ``nomips16`` attributes.
  See :ref:`function-attributes`, for more information.

.. option:: -mflip-mips16

  Generate MIPS16 code on alternating functions.  This option is provided
  for regression testing of mixed MIPS16/non-MIPS16 code generation, and is
  not intended for ordinary use in compiling user code.

.. option:: -minterlink-compressed, -mno-interlink-compressed

  Require (do not require) that code using the standard (uncompressed) MIPS ISA
  be link-compatible with MIPS16 and microMIPS code, and vice versa.

  For example, code using the standard ISA encoding cannot jump directly
  to MIPS16 or microMIPS code; it must either use a call or an indirect jump.
  :option:`-minterlink-compressed` therefore disables direct jumps unless GCC
  knows that the target of the jump is not compressed.

.. option:: -minterlink-mips16, -mno-interlink-mips16

  Aliases of :option:`-minterlink-compressed` and
  :option:`-mno-interlink-compressed`.  These options predate the microMIPS ASE
  and are retained for backwards compatibility.

.. option:: -mabi=32

  Generate code for the given ABI.

  Note that the EABI has a 32-bit and a 64-bit variant.  GCC normally
  generates 64-bit code when you select a 64-bit architecture, but you
  can use :option:`-mgp32` to get 32-bit code instead.

  For information about the O64 ABI, see
  http://gcc.gnu.org//projects//mipso64-abi.html.

  GCC supports a variant of the o32 ABI in which floating-point registers
  are 64 rather than 32 bits wide.  You can select this combination with
  :option:`-mabi=32` :option:`-mfp64`.  This ABI relies on the ``mthc1``
  and ``mfhc1`` instructions and is therefore only supported for
  MIPS32R2, MIPS32R3 and MIPS32R5 processors.

  The register assignments for arguments and return values remain the
  same, but each scalar value is passed in a single 64-bit register
  rather than a pair of 32-bit registers.  For example, scalar
  floating-point values are returned in :samp:`$f0` only, not a
  :samp:`$f0`/:samp:`$f1` pair.  The set of call-saved registers also
  remains the same in that the even-numbered double-precision registers
  are saved.

  Two additional variants of the o32 ABI are supported to enable
  a transition from 32-bit to 64-bit registers.  These are FPXX
  ( :option:`-mfpxx` ) and FP64A ( :option:`-mfp64` :option:`-mno-odd-spreg` ).
  The FPXX extension mandates that all code must execute correctly
  when run using 32-bit or 64-bit registers.  The code can be interlinked
  with either FP32 or FP64, but not both.
  The FP64A extension is similar to the FP64 extension but forbids the
  use of odd-numbered single-precision registers.  This can be used
  in conjunction with the ``FRE`` mode of FPUs in MIPS32R5
  processors and allows both FP32 and FP64A code to interlink and
  run in the same process without changing FPU modes.

.. option:: -mabicalls, -mno-abicalls

  Generate (do not generate) code that is suitable for SVR4-style
  dynamic objects.  :option:`-mabicalls` is the default for SVR4-based
  systems.

``-mshared`` ``-mno-shared``
  Generate (do not generate) code that is fully position-independent,
  and that can therefore be linked into shared libraries.  This option
  only affects :option:`-mabicalls`.

  All :option:`-mabicalls` code has traditionally been position-independent,
  regardless of options like :option:`-fPIC` and :option:`-fpic`.  However,
  as an extension, the GNU toolchain allows executables to use absolute
  accesses for locally-binding symbols.  It can also use shorter GP
  initialization sequences and generate direct calls to locally-defined
  functions.  This mode is selected by :option:`-mno-shared`.

  :option:`-mno-shared` depends on binutils 2.16 or higher and generates
  objects that can only be linked by the GNU linker.  However, the option
  does not affect the ABI of the final executable; it only affects the ABI
  of relocatable objects.  Using :option:`-mno-shared` generally makes
  executables both smaller and quicker.

  :option:`-mshared` is the default.

.. option:: -mplt, -mno-plt

  Assume (do not assume) that the static and dynamic linkers
  support PLTs and copy relocations.  This option only affects
  :option:`-mno-shared -mabicalls`.  For the n64 ABI, this option
  has no effect without :option:`-msym32`.

  You can make :option:`-mplt` the default by configuring
  GCC with :option:`--with-mips-plt`.  The default is
  :option:`-mno-plt` otherwise.

.. option:: -mxgot, -mno-xgot

  Lift (do not lift) the usual restrictions on the size of the global
  offset table.

  GCC normally uses a single instruction to load values from the GOT.
  While this is relatively efficient, it only works if the GOT
  is smaller than about 64k.  Anything larger causes the linker
  to report an error such as:

  .. index:: relocation truncated to fit (MIPS)

  .. code-block:: c++

    relocation truncated to fit: R_MIPS_GOT16 foobar

  If this happens, you should recompile your code with :option:`-mxgot`.
  This works with very large GOTs, although the code is also
  less efficient, since it takes three instructions to fetch the
  value of a global symbol.

  Note that some linkers can create multiple GOTs.  If you have such a
  linker, you should only need to use :option:`-mxgot` when a single object
  file accesses more than 64k's worth of GOT entries.  Very few do.

  These options have no effect unless GCC is generating position
  independent code.

.. option:: -mgp32

  Assume that general-purpose registers are 32 bits wide.

.. option:: -mgp64

  Assume that general-purpose registers are 64 bits wide.

.. option:: -mfp32

  Assume that floating-point registers are 32 bits wide.

.. option:: -mfp64

  Assume that floating-point registers are 64 bits wide.

.. option:: -mfpxx

  Do not assume the width of floating-point registers.

.. option:: -mhard-float

  Use floating-point coprocessor instructions.

.. option:: -msoft-float

  Do not use floating-point coprocessor instructions.  Implement
  floating-point calculations using library calls instead.

.. option:: -mno-float

  Equivalent to :option:`-msoft-float` , but additionally asserts that the
  program being compiled does not perform any floating-point operations.
  This option is presently supported only by some bare-metal MIPS
  configurations, where it may select a special set of libraries
  that lack all floating-point support (including, for example, the
  floating-point ``printf`` formats).  
  If code compiled with :option:`-mno-float` accidentally contains
  floating-point operations, it is likely to suffer a link-time
  or run-time failure.

.. option:: -msingle-float

  Assume that the floating-point coprocessor only supports single-precision
  operations.

.. option:: -mdouble-float

  Assume that the floating-point coprocessor supports double-precision
  operations.  This is the default.

.. option:: -modd-spreg, -mno-odd-spreg

  Enable the use of odd-numbered single-precision floating-point registers
  for the o32 ABI.  This is the default for processors that are known to
  support these registers.  When using the o32 FPXX ABI, :option:`-mno-odd-spreg`
  is set by default.

.. option:: -mabs=2008

  These options control the treatment of the special not-a-number (NaN)
  IEEE 754 floating-point data with the ``abs.fmt`` and
  ``neg.fmt`` machine instructions.

  By default or when :option:`-mabs=legacy` is used the legacy
  treatment is selected.  In this case these instructions are considered
  arithmetic and avoided where correct operation is required and the
  input operand might be a NaN.  A longer sequence of instructions that
  manipulate the sign bit of floating-point datum manually is used
  instead unless the :option:`-ffinite-math-only` option has also been
  specified.

  The :option:`-mabs=2008` option selects the IEEE 754-2008 treatment.  In
  this case these instructions are considered non-arithmetic and therefore
  operating correctly in all cases, including in particular where the
  input operand is a NaN.  These instructions are therefore always used
  for the respective operations.

.. option:: -mnan=2008

  These options control the encoding of the special not-a-number (NaN)
  IEEE 754 floating-point data.

  The :option:`-mnan=legacy` option selects the legacy encoding.  In this
  case quiet NaNs (qNaNs) are denoted by the first bit of their trailing
  significand field being 0, whereas signaling NaNs (sNaNs) are denoted
  by the first bit of their trailing significand field being 1.

  The :option:`-mnan=2008` option selects the IEEE 754-2008 encoding.  In
  this case qNaNs are denoted by the first bit of their trailing
  significand field being 1, whereas sNaNs are denoted by the first bit of
  their trailing significand field being 0.

  The default is :option:`-mnan=legacy` unless GCC has been configured with
  :option:`--with-nan=2008`.

.. option:: -mllsc, -mno-llsc

  Use (do not use) :samp:`ll`, :samp:`sc`, and :samp:`sync` instructions to
  implement atomic memory built-in functions.  When neither option is
  specified, GCC uses the instructions if the target architecture
  supports them.

  :option:`-mllsc` is useful if the runtime environment can emulate the
  instructions and :option:`-mno-llsc` can be useful when compiling for
  nonstandard ISAs.  You can make either option the default by
  configuring GCC with :option:`--with-llsc` and :option:`--without-llsc`
  respectively.  :option:`--with-llsc` is the default for some
  configurations; see the installation documentation for details.

.. option:: -mdsp, -mno-dsp

  Use (do not use) revision 1 of the MIPS DSP ASE.
  See :ref:`mips-dsp-built-in-functions`.  This option defines the
  preprocessor macro ``__mips_dsp``.  It also defines
  ``__mips_dsp_rev`` to 1.

.. option:: -mdspr2, -mno-dspr2

  Use (do not use) revision 2 of the MIPS DSP ASE.
  See :ref:`mips-dsp-built-in-functions`.  This option defines the
  preprocessor macros ``__mips_dsp`` and ``__mips_dspr2``.
  It also defines ``__mips_dsp_rev`` to 2.

.. option:: -msmartmips, -mno-smartmips

  Use (do not use) the MIPS SmartMIPS ASE.

.. option:: -mpaired-single, -mno-paired-single

  Use (do not use) paired-single floating-point instructions.
  See :ref:`mips-paired-single-support`.  This option requires
  hardware floating-point support to be enabled.

.. option:: -mdmx, -mno-mdmx

  Use (do not use) MIPS Digital Media Extension instructions.
  This option can only be used when generating 64-bit code and requires
  hardware floating-point support to be enabled.

.. option:: -mips3d, -mno-mips3d

  Use (do not use) the MIPS-3D ASE.  See :ref:`mips-3d-built-in-functions`.
  The option :option:`-mips3d` implies :option:`-mpaired-single`.

.. option:: -mmicromips, -mno-mmicromips

  Generate (do not generate) microMIPS code.

  MicroMIPS code generation can also be controlled on a per-function basis
  by means of ``micromips`` and ``nomicromips`` attributes.
  See :ref:`function-attributes`, for more information.

.. option:: -mmt, -mno-mt

  Use (do not use) MT Multithreading instructions.

.. option:: -mmcu, -mno-mcu

  Use (do not use) the MIPS MCU ASE instructions.

.. option:: -meva, -mno-eva

  Use (do not use) the MIPS Enhanced Virtual Addressing instructions.

.. option:: -mvirt, -mno-virt

  Use (do not use) the MIPS Virtualization (VZ) instructions.

.. option:: -mxpa, -mno-xpa

  Use (do not use) the MIPS eXtended Physical Address (XPA) instructions.

.. option:: -mcrc, -mno-crc

  Use (do not use) the MIPS Cyclic Redundancy Check (CRC) instructions.

.. option:: -mginv, -mno-ginv

  Use (do not use) the MIPS Global INValidate (GINV) instructions.

.. option:: -mloongson-mmi, -mno-loongson-mmi

  Use (do not use) the MIPS Loongson MultiMedia extensions Instructions (MMI).

.. option:: -mloongson-ext, -mno-loongson-ext

  Use (do not use) the MIPS Loongson EXTensions (EXT) instructions.

.. option:: -mloongson-ext2, -mno-loongson-ext2

  Use (do not use) the MIPS Loongson EXTensions r2 (EXT2) instructions.

.. option:: -mlong64

  Force ``long`` types to be 64 bits wide.  See :option:`-mlong32` for
  an explanation of the default and the way that the pointer size is
  determined.

.. option:: -mlong32

  Force ``long``, ``int``, and pointer types to be 32 bits wide.

  The default size of ``int``s, ``long``s and pointers depends on
  the ABI.  All the supported ABIs use 32-bit ``int``s.  The n64 ABI
  uses 64-bit ``long``s, as does the 64-bit EABI; the others use
  32-bit ``long``s.  Pointers are the same size as ``long``s,
  or the same size as integer registers, whichever is smaller.

.. option:: -msym32, -mno-sym32

  Assume (do not assume) that all symbols have 32-bit values, regardless
  of the selected ABI.  This option is useful in combination with
  :option:`-mabi=64` and :option:`-mno-abicalls` because it allows GCC
  to generate shorter and faster references to symbolic addresses.

.. option:: -G num, -G

  Put definitions of externally-visible data in a small data section
  if that data is no bigger than :samp:`{num}` bytes.  GCC can then generate
  more efficient accesses to the data; see :option:`-mgpopt` for details.

  The default :option:`-G` option depends on the configuration.

.. option:: -mlocal-sdata, -mno-local-sdata

  Extend (do not extend) the :option:`-G` behavior to local data too,
  such as to static variables in C.  :option:`-mlocal-sdata` is the
  default for all configurations.

  If the linker complains that an application is using too much small data,
  you might want to try rebuilding the less performance-critical parts with
  :option:`-mno-local-sdata`.  You might also want to build large
  libraries with :option:`-mno-local-sdata` , so that the libraries leave
  more room for the main program.

.. option:: -mextern-sdata, -mno-extern-sdata

  Assume (do not assume) that externally-defined data is in
  a small data section if the size of that data is within the :option:`-G` limit.
  :option:`-mextern-sdata` is the default for all configurations.

  If you compile a module :samp:`{Mod}` with :option:`-mextern-sdata` :option:`-G
  `:samp:`{num}` :option:`-mgpopt` , and :samp:`{Mod}` references a variable :samp:`{Var}`
  that is no bigger than :samp:`{num}` bytes, you must make sure that :samp:`{Var}`
  is placed in a small data section.  If :samp:`{Var}` is defined by another
  module, you must either compile that module with a high-enough
  :option:`-G` setting or attach a ``section`` attribute to :samp:`{Var}` 's
  definition.  If :samp:`{Var}` is common, you must link the application
  with a high-enough :option:`-G` setting.

  The easiest way of satisfying these restrictions is to compile
  and link every module with the same :option:`-G` option.  However,
  you may wish to build a library that supports several different
  small data limits.  You can do this by compiling the library with
  the highest supported :option:`-G` setting and additionally using
  :option:`-mno-extern-sdata` to stop the library from making assumptions
  about externally-defined data.

.. option:: -mgpopt, -mno-gpopt

  Use (do not use) GP-relative accesses for symbols that are known to be
  in a small data section; see :option:`-G` , :option:`-mlocal-sdata` and
  :option:`-mextern-sdata`.  :option:`-mgpopt` is the default for all
  configurations.

  :option:`-mno-gpopt` is useful for cases where the ``$gp`` register
  might not hold the value of ``_gp``.  For example, if the code is
  part of a library that might be used in a boot monitor, programs that
  call boot monitor routines pass an unknown value in ``$gp``.
  (In such situations, the boot monitor itself is usually compiled
  with :option:`-G0`.)

  :option:`-mno-gpopt` implies :option:`-mno-local-sdata` and
  :option:`-mno-extern-sdata`.

.. option:: -membedded-data, -mno-embedded-data

  Allocate variables to the read-only data section first if possible, then
  next in the small data section if possible, otherwise in data.  This gives
  slightly slower code than the default, but reduces the amount of RAM required
  when executing, and thus may be preferred for some embedded systems.

.. option:: -muninit-const-in-rodata, -mno-uninit-const-in-rodata

  Put uninitialized ``const`` variables in the read-only data section.
  This option is only meaningful in conjunction with :option:`-membedded-data`.

.. option:: -mcode-readable=setting

  Specify whether GCC may generate code that reads from executable sections.
  There are three possible settings:

  ``-mcode-readable=yes``
    Instructions may freely access executable sections.  This is the
    default setting.

  ``-mcode-readable=pcrel``
    MIPS16 PC-relative load instructions can access executable sections,
    but other instructions must not do so.  This option is useful on 4KSc
    and 4KSd processors when the code TLBs have the Read Inhibit bit set.
    It is also useful on processors that can be configured to have a dual
    instruction/data SRAM interface and that, like the M4K, automatically
    redirect PC-relative loads to the instruction RAM.

  ``-mcode-readable=no``
    Instructions must not access executable sections.  This option can be
    useful on targets that are configured to have a dual instruction/data
    SRAM interface but that (unlike the M4K) do not automatically redirect
    PC-relative loads to the instruction RAM.

.. option:: -msplit-addresses, -mno-split-addresses

  Enable (disable) use of the ``%hi()`` and ``%lo()`` assembler
  relocation operators.  This option has been superseded by
  :option:`-mexplicit-relocs` but is retained for backwards compatibility.

.. option:: -mexplicit-relocs, -mno-explicit-relocs

  Use (do not use) assembler relocation operators when dealing with symbolic
  addresses.  The alternative, selected by :option:`-mno-explicit-relocs` ,
  is to use assembler macros instead.

  :option:`-mexplicit-relocs` is the default if GCC was configured
  to use an assembler that supports relocation operators.

.. option:: -mcheck-zero-division, -mno-check-zero-division

  Trap (do not trap) on integer division by zero.

  The default is :option:`-mcheck-zero-division`.

.. option:: -mdivide-traps, -mdivide-breaks

  MIPS systems check for division by zero by generating either a
  conditional trap or a break instruction.  Using traps results in
  smaller code, but is only supported on MIPS II and later.  Also, some
  versions of the Linux kernel have a bug that prevents trap from
  generating the proper signal (``SIGFPE``).  Use :option:`-mdivide-traps` to
  allow conditional traps on architectures that support them and
  :option:`-mdivide-breaks` to force the use of breaks.

  The default is usually :option:`-mdivide-traps` , but this can be
  overridden at configure time using :option:`--with-divide=breaks`.
  Divide-by-zero checks can be completely disabled using
  :option:`-mno-check-zero-division`.

.. option:: -mload-store-pairs, -mno-load-store-pairs

  Enable (disable) an optimization that pairs consecutive load or store
  instructions to enable load/store bonding.  This option is enabled by
  default but only takes effect when the selected architecture is known
  to support bonding.

.. option:: -mmemcpy, -mno-memcpy

  Force (do not force) the use of ``memcpy`` for non-trivial block
  moves.  The default is :option:`-mno-memcpy` , which allows GCC to inline
  most constant-sized copies.

.. option:: -mlong-calls, -mno-long-calls

  Disable (do not disable) use of the ``jal`` instruction.  Calling
  functions using ``jal`` is more efficient but requires the caller
  and callee to be in the same 256 megabyte segment.

  This option has no effect on abicalls code.  The default is
  :option:`-mno-long-calls`.

.. option:: -mmad, -mno-mad

  Enable (disable) use of the ``mad``, ``madu`` and ``mul``
  instructions, as provided by the R4650 ISA.

.. option:: -mimadd, -mno-imadd

  Enable (disable) use of the ``madd`` and ``msub`` integer
  instructions.  The default is :option:`-mimadd` on architectures
  that support ``madd`` and ``msub`` except for the 74k 
  architecture where it was found to generate slower code.

.. option:: -mfused-madd, -mno-fused-madd

  Enable (disable) use of the floating-point multiply-accumulate
  instructions, when they are available.  The default is
  :option:`-mfused-madd`.

  On the R8000 CPU when multiply-accumulate instructions are used,
  the intermediate product is calculated to infinite precision
  and is not subject to the FCSR Flush to Zero bit.  This may be
  undesirable in some circumstances.  On other processors the result
  is numerically identical to the equivalent computation using
  separate multiply, add, subtract and negate instructions.

.. option:: -nocpp

  Tell the MIPS assembler to not run its preprocessor over user
  assembler files (with a :samp:`.s` suffix) when assembling them.

.. option:: -mfix-24k, -mno-fix-24k

  Work around the 24K E48 (lost data on stores during refill) errata.
  The workarounds are implemented by the assembler rather than by GCC.

.. option:: -mfix-r4000, -mno-fix-r4000

  Work around certain R4000 CPU errata:

  * A double-word or a variable shift may give an incorrect result if executed
    immediately after starting an integer division.

  * A double-word or a variable shift may give an incorrect result if executed
    while an integer multiplication is in progress.

  * An integer division may give an incorrect result if started in a delay slot
    of a taken branch or a jump.

.. option:: -mfix-r4400, -mno-fix-r4400

  Work around certain R4400 CPU errata:

  * A double-word or a variable shift may give an incorrect result if executed
    immediately after starting an integer division.

.. option:: -mfix-r10000, -mno-fix-r10000

  Work around certain R10000 errata:

  * ``ll``/``sc`` sequences may not behave atomically on revisions
    prior to 3.0.  They may deadlock on revisions 2.6 and earlier.

  This option can only be used if the target architecture supports
  branch-likely instructions.  :option:`-mfix-r10000` is the default when
  :option:`-march=r10000` is used; :option:`-mno-fix-r10000` is the default
  otherwise.

.. option:: -mfix-r5900

  Do not attempt to schedule the preceding instruction into the delay slot
  of a branch instruction placed at the end of a short loop of six
  instructions or fewer and always schedule a ``nop`` instruction there
  instead.  The short loop bug under certain conditions causes loops to
  execute only once or twice, due to a hardware bug in the R5900 chip.  The
  workaround is implemented by the assembler rather than by GCC.

.. option:: -mfix-rm7000

  Work around the RM7000 ``dmult``/``dmultu`` errata.  The
  workarounds are implemented by the assembler rather than by GCC.

.. option:: -mfix-vr4120

  Work around certain VR4120 errata:

  * ``dmultu`` does not always produce the correct result.

  * ``div`` and ``ddiv`` do not always produce the correct result if one
    of the operands is negative.

  The workarounds for the division errata rely on special functions in
  libgcc.a.  At present, these functions are only provided by
  the ``mips64vr*-elf`` configurations.

  Other VR4120 errata require a NOP to be inserted between certain pairs of
  instructions.  These errata are handled by the assembler, not by GCC itself.

.. option:: -mfix-vr4130

  Work around the VR4130 ``mflo``/``mfhi`` errata.  The
  workarounds are implemented by the assembler rather than by GCC,
  although GCC avoids using ``mflo`` and ``mfhi`` if the
  VR4130 ``macc``, ``macchi``, ``dmacc`` and ``dmacchi``
  instructions are available instead.

.. option:: -mfix-sb1

  Work around certain SB-1 CPU core errata.
  (This flag currently works around the SB-1 revision 2
  'F1' and 'F2' floating-point errata.)

.. option:: -mr10k-cache-barrier=setting

  Specify whether GCC should insert cache barriers to avoid the
  side effects of speculation on R10K processors.

  In common with many processors, the R10K tries to predict the outcome
  of a conditional branch and speculatively executes instructions from
  the 'taken' branch.  It later aborts these instructions if the
  predicted outcome is wrong.  However, on the R10K, even aborted
  instructions can have side effects.

  This problem only affects kernel stores and, depending on the system,
  kernel loads.  As an example, a speculatively-executed store may load
  the target memory into cache and mark the cache line as dirty, even if
  the store itself is later aborted.  If a DMA operation writes to the
  same area of memory before the 'dirty' line is flushed, the cached
  data overwrites the DMA-ed data.  See the R10K processor manual
  for a full description, including other potential problems.

  One workaround is to insert cache barrier instructions before every memory
  access that might be speculatively executed and that might have side
  effects even if aborted.  :option:`-mr10k-cache-barrier`:samp:`={setting}`
  controls GCC's implementation of this workaround.  It assumes that
  aborted accesses to any byte in the following regions does not have
  side effects:

  * the memory occupied by the current function's stack frame;

  * the memory occupied by an incoming stack argument;

  * the memory occupied by an object with a link-time-constant address.

  It is the kernel's responsibility to ensure that speculative
  accesses to these regions are indeed safe.

  If the input program contains a function declaration such as:

  .. code-block:: c++

    void foo (void);

  then the implementation of ``foo`` must allow ``j foo`` and
  ``jal foo`` to be executed speculatively.  GCC honors this
  restriction for functions it compiles itself.  It expects non-GCC
  functions (such as hand-written assembly code) to do the same.

  The option has three forms:

  ``-mr10k-cache-barrier=load-store``
    Insert a cache barrier before a load or store that might be
    speculatively executed and that might have side effects even
    if aborted.

  ``-mr10k-cache-barrier=store``
    Insert a cache barrier before a store that might be speculatively
    executed and that might have side effects even if aborted.

  ``-mr10k-cache-barrier=none``
    Disable the insertion of cache barriers.  This is the default setting.

.. option:: -mflush-func=func

  Specifies the function to call to flush the I and D caches, or to not
  call any such function.  If called, the function must take the same
  arguments as the common ``_flush_func``, that is, the address of the
  memory range for which the cache is being flushed, the size of the
  memory range, and the number 3 (to flush both caches).  The default
  depends on the target GCC was configured for, but commonly is either
  ``_flush_func`` or ``__cpu_flush``.

.. option:: mbranch-cost=num

  Set the cost of branches to roughly :samp:`{num}` 'simple' instructions.
  This cost is only a heuristic and is not guaranteed to produce
  consistent results across releases.  A zero cost redundantly selects
  the default, which is based on the :option:`-mtune` setting.

.. option:: -mbranch-likely, -mno-branch-likely

  Enable or disable use of Branch Likely instructions, regardless of the
  default for the selected architecture.  By default, Branch Likely
  instructions may be generated if they are supported by the selected
  architecture.  An exception is for the MIPS32 and MIPS64 architectures
  and processors that implement those architectures; for those, Branch
  Likely instructions are not be generated by default because the MIPS32
  and MIPS64 architectures specifically deprecate their use.

.. option:: -mcompact-branches=never

  These options control which form of branches will be generated.  The
  default is :option:`-mcompact-branches=optimal`.

  The :option:`-mcompact-branches=never` option ensures that compact branch
  instructions will never be generated.

  The :option:`-mcompact-branches=always` option ensures that a compact
  branch instruction will be generated if available.  If a compact branch
  instruction is not available, a delay slot form of the branch will be
  used instead.

  This option is supported from MIPS Release 6 onwards.

  The :option:`-mcompact-branches=optimal` option will cause a delay slot
  branch to be used if one is available in the current ISA and the delay
  slot is successfully filled.  If the delay slot is not filled, a compact
  branch will be chosen if one is available.

.. option:: -mfp-exceptions

  Specifies whether FP exceptions are enabled.  This affects how
  FP instructions are scheduled for some processors.
  The default is that FP exceptions are
  enabled.

  For instance, on the SB-1, if FP exceptions are disabled, and we are emitting
  64-bit code, then we can use both FP pipes.  Otherwise, we can only use one
  FP pipe.

.. option:: -mvr4130-align

  The VR4130 pipeline is two-way superscalar, but can only issue two
  instructions together if the first one is 8-byte aligned.  When this
  option is enabled, GCC aligns pairs of instructions that it
  thinks should execute in parallel.

  This option only has an effect when optimizing for the VR4130.
  It normally makes code faster, but at the expense of making it bigger.
  It is enabled by default at optimization level :option:`-O3`.

.. option:: -msynci

  Enable (disable) generation of ``synci`` instructions on
  architectures that support it.  The ``synci`` instructions (if
  enabled) are generated when ``__builtin___clear_cache`` is
  compiled.

  This option defaults to :option:`-mno-synci` , but the default can be
  overridden by configuring GCC with :option:`--with-synci`.

  When compiling code for single processor systems, it is generally safe
  to use ``synci``.  However, on many multi-core (SMP) systems, it
  does not invalidate the instruction caches on all cores and may lead
  to undefined behavior.

.. option:: -mrelax-pic-calls

  Try to turn PIC calls that are normally dispatched via register
  ``$25`` into direct calls.  This is only possible if the linker can
  resolve the destination at link time and if the destination is within
  range for a direct call.

  :option:`-mrelax-pic-calls` is the default if GCC was configured to use
  an assembler and a linker that support the ``.reloc`` assembly
  directive and :option:`-mexplicit-relocs` is in effect.  With
  :option:`-mno-explicit-relocs` , this optimization can be performed by the
  assembler and the linker alone without help from the compiler.

.. option:: -mmcount-ra-address, -mno-mcount-ra-address

  Emit (do not emit) code that allows ``_mcount`` to modify the
  calling function's return address.  When enabled, this option extends
  the usual ``_mcount`` interface with a new :samp:`{ra-address}`
  parameter, which has type ``intptr_t *`` and is passed in register
  ``$12``.  ``_mcount`` can then modify the return address by
  doing both of the following:

  ** Returning the new address in register ``$31``.

  * Storing the new address in ``*ra-address``,
    if :samp:`{ra-address}` is nonnull.

  The default is :option:`-mno-mcount-ra-address`.

.. option:: -mframe-header-opt

  Enable (disable) frame header optimization in the o32 ABI.  When using the
  o32 ABI, calling functions will allocate 16 bytes on the stack for the called
  function to write out register arguments.  When enabled, this optimization
  will suppress the allocation of the frame header if it can be determined that
  it is unused.

  This optimization is off by default at all optimization levels.

.. option:: -mlxc1-sxc1

  When applicable, enable (disable) the generation of ``lwxc1``,
  ``swxc1``, ``ldxc1``, ``sdxc1`` instructions.  Enabled by default.

.. option:: -mmadd4

  When applicable, enable (disable) the generation of 4-operand ``madd.s``,
  ``madd.d`` and related instructions.  Enabled by default.

.. _mmix-options:

MMIX Options
^^^^^^^^^^^^

.. index:: MMIX Options

These options are defined for the MMIX:

.. option:: -mlibfuncs, -mno-libfuncs

  Specify that intrinsic library functions are being compiled, passing all
  values in registers, no matter the size.

.. option:: -mepsilon, -mno-epsilon

  Generate floating-point comparison instructions that compare with respect
  to the ``rE`` epsilon register.

.. option:: -mabi=mmixware

  Generate code that passes function parameters and return values that (in
  the called function) are seen as registers ``$0`` and up, as opposed to
  the GNU ABI which uses global registers ``$231`` and up.

.. option:: -mzero-extend, -mno-zero-extend

  When reading data from memory in sizes shorter than 64 bits, use (do not
  use) zero-extending load instructions by default, rather than
  sign-extending ones.

.. option:: -mknuthdiv, -mno-knuthdiv

  Make the result of a division yielding a remainder have the same sign as
  the divisor.  With the default, :option:`-mno-knuthdiv` , the sign of the
  remainder follows the sign of the dividend.  Both methods are
  arithmetically valid, the latter being almost exclusively used.

.. option:: -mtoplevel-symbols, -mno-toplevel-symbols

  Prepend (do not prepend) a :samp:`:` to all global symbols, so the assembly
  code can be used with the ``PREFIX`` assembly directive.

.. option:: -melf

  Generate an executable in the ELF format, rather than the default
  :samp:`mmo` format used by the :command:`mmix` simulator.

.. option:: -mbranch-predict, -mno-branch-predict

  Use (do not use) the probable-branch instructions, when static branch
  prediction indicates a probable branch.

.. option:: -mbase-addresses, -mno-base-addresses

  Generate (do not generate) code that uses *base addresses*.  Using a
  base address automatically generates a request (handled by the assembler
  and the linker) for a constant to be set up in a global register.  The
  register is used for one or more base address requests within the range 0
  to 255 from the value held in the register.  The generally leads to short
  and fast code, but the number of different data items that can be
  addressed is limited.  This means that a program that uses lots of static
  data may require :option:`-mno-base-addresses`.

.. option:: -msingle-exit, -mno-single-exit

  Force (do not force) generated code to have a single exit point in each
  function.

.. _mn10300-options:

MN10300 Options
^^^^^^^^^^^^^^^

.. index:: MN10300 options

These :option:`-m` options are defined for Matsushita MN10300 architectures:

.. option:: -mmult-bug

  Generate code to avoid bugs in the multiply instructions for the MN10300
  processors.  This is the default.

.. option:: -mno-mult-bug

  Do not generate code to avoid bugs in the multiply instructions for the
  MN10300 processors.

.. option:: -mam33

  Generate code using features specific to the AM33 processor.

.. option:: -mno-am33

  Do not generate code using features specific to the AM33 processor.  This
  is the default.

.. option:: -mam33-2

  Generate code using features specific to the AM33/2.0 processor.

.. option:: -mam34

  Generate code using features specific to the AM34 processor.

.. option:: -mtune=cpu-type

  Use the timing characteristics of the indicated CPU type when
  scheduling instructions.  This does not change the targeted processor
  type.  The CPU type must be one of :samp:`mn10300`, :samp:`am33`,
  :samp:`am33-2` or :samp:`am34`.

.. option:: -mreturn-pointer-on-d0

  When generating a function that returns a pointer, return the pointer
  in both ``a0`` and ``d0``.  Otherwise, the pointer is returned
  only in ``a0``, and attempts to call such functions without a prototype
  result in errors.  Note that this option is on by default; use
  :option:`-mno-return-pointer-on-d0` to disable it.

.. option:: -mno-crt0

  Do not link in the C run-time initialization object file.

.. option:: -mrelax

  Indicate to the linker that it should perform a relaxation optimization pass
  to shorten branches, calls and absolute memory addresses.  This option only
  has an effect when used on the command line for the final link step.

  This option makes symbolic debugging impossible.

.. option:: -mliw

  Allow the compiler to generate *Long Instruction Word*
  instructions if the target is the :samp:`AM33` or later.  This is the
  default.  This option defines the preprocessor macro ``__LIW__``.

.. option:: -mno-liw

  Do not allow the compiler to generate *Long Instruction Word*
  instructions.  This option defines the preprocessor macro
  ``__NO_LIW__``.

.. option:: -msetlb

  Allow the compiler to generate the *SETLB* and *Lcc*
  instructions if the target is the :samp:`AM33` or later.  This is the
  default.  This option defines the preprocessor macro ``__SETLB__``.

.. option:: -mno-setlb

  Do not allow the compiler to generate *SETLB* or *Lcc*
  instructions.  This option defines the preprocessor macro
  ``__NO_SETLB__``.

.. _moxie-options:

Moxie Options
^^^^^^^^^^^^^

.. index:: Moxie Options

.. option:: -meb

  Generate big-endian code.  This is the default for :samp:`moxie-*-*`
  configurations.

.. option:: -mel

  Generate little-endian code.

.. option:: -mmul.x

  Generate mul.x and umul.x instructions.  This is the default for
  :samp:`moxiebox-*-*` configurations.

.. option:: -mno-crt0

  Do not link in the C run-time initialization object file.

.. _msp430-options:

MSP430 Options
^^^^^^^^^^^^^^

.. index:: MSP430 Options

These options are defined for the MSP430:

.. option:: -masm-hex

  Force assembly output to always use hex constants.  Normally such
  constants are signed decimals, but this option is available for
  testsuite and/or aesthetic purposes.

.. option:: -mmcu=

  Select the MCU to target.  This is used to create a C preprocessor
  symbol based upon the MCU name, converted to upper case and pre- and
  post-fixed with :samp:`__`.  This in turn is used by the
  msp430.h header file to select an MCU-specific supplementary
  header file.

  The option also sets the ISA to use.  If the MCU name is one that is
  known to only support the 430 ISA then that is selected, otherwise the
  430X ISA is selected.  A generic MCU name of :samp:`msp430` can also be
  used to select the 430 ISA.  Similarly the generic :samp:`msp430x` MCU
  name selects the 430X ISA.

  In addition an MCU-specific linker script is added to the linker
  command line.  The script's name is the name of the MCU with
  .ld appended.  Thus specifying :option:`-mmcu=xxx` on the :command:`gcc`
  command line defines the C preprocessor symbol ``__XXX__`` and
  cause the linker to search for a script called xxx.ld.

  The ISA and hardware multiply supported for the different MCUs is hard-coded
  into GCC.  However, an external :samp:`devices.csv` file can be used to
  extend device support beyond those that have been hard-coded.

  GCC searches for the :samp:`devices.csv` file using the following methods in the
  given precedence order, where the first method takes precendence over the
  second which takes precedence over the third.

  :samp:`Include path specified with {-I} and {-L}`
    :samp:`devices.csv` will be searched for in each of the directories specified by
    include paths and linker library search paths.

  :samp:`Path specified by the environment variable {MSP430_GCC_INCLUDE_DIR}`
    Define the value of the global environment variable
    :samp:`MSP430_GCC_INCLUDE_DIR`
    to the full path to the directory containing devices.csv, and GCC will search
    this directory for devices.csv.  If devices.csv is found, this directory will
    also be registered as an include path, and linker library path.  Header files
    and linker scripts in this directory can therefore be used without manually
    specifying ``-I`` and ``-L`` on the command line.

  :samp:`The {msp430-elf{,bare}/include/devices} directory`
    Finally, GCC will examine :samp:`msp430-elf{,bare}/include/devices` from the
    toolchain root directory.  This directory does not exist in a default
    installation, but if the user has created it and copied :samp:`devices.csv`
    there, then the MCU data will be read.  As above, this directory will
    also be registered as an include path, and linker library path.

    If none of the above search methods find :samp:`devices.csv`, then the
  hard-coded MCU data is used.

.. option:: -mwarn-mcu, -mno-warn-mcu

  This option enables or disables warnings about conflicts between the
  MCU name specified by the :option:`-mmcu` option and the ISA set by the
  :option:`-mcpu` option and/or the hardware multiply support set by the
  :option:`-mhwmult` option.  It also toggles warnings about unrecognized
  MCU names.  This option is on by default.

.. option:: -mcpu=

  Specifies the ISA to use.  Accepted values are :samp:`msp430`,
  :samp:`msp430x` and :samp:`msp430xv2`.  This option is deprecated.  The
  :option:`-mmcu` = option should be used to select the ISA.

.. option:: -msim

  Link to the simulator runtime libraries and linker script.  Overrides
  any scripts that would be selected by the :option:`-mmcu` = option.

.. option:: -mlarge

  Use large-model addressing (20-bit pointers, 20-bit ``size_t``).

.. option:: -msmall

  Use small-model addressing (16-bit pointers, 16-bit ``size_t``).

.. option:: -mrelax

  This option is passed to the assembler and linker, and allows the
  linker to perform certain optimizations that cannot be done until
  the final link.

.. option:: mhwmult=

  Describes the type of hardware multiply supported by the target.
  Accepted values are :samp:`none` for no hardware multiply, :samp:`16bit`
  for the original 16-bit-only multiply supported by early MCUs.
  :samp:`32bit` for the 16/32-bit multiply supported by later MCUs and
  :samp:`f5series` for the 16/32-bit multiply supported by F5-series MCUs.
  A value of :samp:`auto` can also be given.  This tells GCC to deduce
  the hardware multiply support based upon the MCU name provided by the
  :option:`-mmcu` option.  If no :option:`-mmcu` option is specified or if
  the MCU name is not recognized then no hardware multiply support is
  assumed.  ``auto`` is the default setting.

  Hardware multiplies are normally performed by calling a library
  routine.  This saves space in the generated code.  When compiling at
  :option:`-O3` or higher however the hardware multiplier is invoked
  inline.  This makes for bigger, but faster code.

  The hardware multiply routines disable interrupts whilst running and
  restore the previous interrupt state when they finish.  This makes
  them safe to use inside interrupt handlers as well as in normal code.

.. option:: -minrt

  Enable the use of a minimum runtime environment - no static
  initializers or constructors.  This is intended for memory-constrained
  devices.  The compiler includes special symbols in some objects
  that tell the linker and runtime which code fragments are required.

.. option:: -mtiny-printf

  Enable reduced code size ``printf`` and ``puts`` library functions.
  The :samp:`tiny` implementations of these functions are not reentrant, so
  must be used with caution in multi-threaded applications.

  Support for streams has been removed and the string to be printed will
  always be sent to stdout via the ``write`` syscall.  The string is not
  buffered before it is sent to write.

  This option requires Newlib Nano IO, so GCC must be configured with
  :samp:`--enable-newlib-nano-formatted-io`.

.. option:: -mmax-inline-shift=

  This option takes an integer between 0 and 64 inclusive, and sets
  the maximum number of inline shift instructions which should be emitted to
  perform a shift operation by a constant amount.  When this value needs to be
  exceeded, an mspabi helper function is used instead.  The default value is 4.

  This only affects cases where a shift by multiple positions cannot be
  completed with a single instruction (e.g. all shifts >1 on the 430 ISA).

  Shifts of a 32-bit value are at least twice as costly, so the value passed for
  this option is divided by 2 and the resulting value used instead.

.. option:: -mcode-region=

  These options tell the compiler where to place functions and data that
  do not have one of the ``lower``, ``upper``, ``either`` or
  ``section`` attributes.  Possible values are ``lower``,
  ``upper``, ``either`` or ``any``.  The first three behave
  like the corresponding attribute.  The fourth possible value -
  ``any`` - is the default.  It leaves placement entirely up to the
  linker script and how it assigns the standard sections
  (``.text``, ``.data``, etc) to the memory regions.

.. option:: -msilicon-errata=

  This option passes on a request to assembler to enable the fixes for
  the named silicon errata.

.. option:: -msilicon-errata-warn=

  This option passes on a request to the assembler to enable warning
  messages when a silicon errata might need to be applied.

.. option:: -mwarn-devices-csv, -mno-warn-devices-csv

  Warn if :samp:`devices.csv` is not found or there are problem parsing it
  (default: on).

.. _nds32-options:

NDS32 Options
^^^^^^^^^^^^^

.. index:: NDS32 Options

These options are defined for NDS32 implementations:

.. option:: -mbig-endian

  Generate code in big-endian mode.

.. option:: -mlittle-endian

  Generate code in little-endian mode.

.. option:: -mreduced-regs

  Use reduced-set registers for register allocation.

.. option:: -mfull-regs

  Use full-set registers for register allocation.

.. option:: -mcmov

  Generate conditional move instructions.

.. option:: -mno-cmov

  Do not generate conditional move instructions.

.. option:: -mext-perf

  Generate performance extension instructions.

.. option:: -mno-ext-perf

  Do not generate performance extension instructions.

.. option:: -mext-perf2

  Generate performance extension 2 instructions.

.. option:: -mno-ext-perf2

  Do not generate performance extension 2 instructions.

.. option:: -mext-string

  Generate string extension instructions.

.. option:: -mno-ext-string

  Do not generate string extension instructions.

.. option:: -mv3push

  Generate v3 push25/pop25 instructions.

.. option:: -mno-v3push

  Do not generate v3 push25/pop25 instructions.

.. option:: -m16-bit

  Generate 16-bit instructions.

.. option:: -mno-16-bit

  Do not generate 16-bit instructions.

.. option:: -misr-vector-size=num

  Specify the size of each interrupt vector, which must be 4 or 16.

.. option:: -mcache-block-size=num

  Specify the size of each cache block,
  which must be a power of 2 between 4 and 512.

.. option:: -march=arch

  Specify the name of the target architecture.

.. option:: -mcmodel=code-model

  Set the code model to one of

  small
    All the data and read-only data segments must be within 512KB addressing space.
    The text segment must be within 16MB addressing space.

  medium
    The data segment must be within 512KB while the read-only data segment can be
    within 4GB addressing space.  The text segment should be still within 16MB
    addressing space.

  large
    All the text and data segments can be within 4GB addressing space.

.. option:: -mctor-dtor

  Enable constructor/destructor feature.

.. option:: -mrelax

  Guide linker to relax instructions.

.. _nios-ii-options:

Nios II Options
^^^^^^^^^^^^^^^

.. index:: Nios II options

.. index:: Altera Nios II options

These are the options defined for the Altera Nios II processor.

.. option:: -G num, -G

  .. index:: smaller data references

  Put global and static objects less than or equal to :samp:`{num}` bytes
  into the small data or BSS sections instead of the normal data or BSS
  sections.  The default value of :samp:`{num}` is 8.

.. option:: -mgpopt=option

  Generate (do not generate) GP-relative accesses.  The following 
  :samp:`{option}` names are recognized:

  :samp:`none`
    Do not generate GP-relative accesses.

  :samp:`local`
    Generate GP-relative accesses for small data objects that are not 
    external, weak, or uninitialized common symbols.  
    Also use GP-relative addressing for objects that
    have been explicitly placed in a small data section via a ``section``
    attribute.

  :samp:`global`
    As for :samp:`local`, but also generate GP-relative accesses for
    small data objects that are external, weak, or common.  If you use this option,
    you must ensure that all parts of your program (including libraries) are
    compiled with the same :option:`-G` setting.

  :samp:`data`
    Generate GP-relative accesses for all data objects in the program.  If you
    use this option, the entire data and BSS segments
    of your program must fit in 64K of memory and you must use an appropriate
    linker script to allocate them within the addressable range of the
    global pointer.

  :samp:`all`
    Generate GP-relative addresses for function pointers as well as data
    pointers.  If you use this option, the entire text, data, and BSS segments
    of your program must fit in 64K of memory and you must use an appropriate
    linker script to allocate them within the addressable range of the
    global pointer.

    :option:`-mgpopt` is equivalent to :option:`-mgpopt=local` , and
  :option:`-mno-gpopt` is equivalent to :option:`-mgpopt=none`.

  The default is :option:`-mgpopt` except when :option:`-fpic` or
  :option:`-fPIC` is specified to generate position-independent code.
  Note that the Nios II ABI does not permit GP-relative accesses from
  shared libraries.

  You may need to specify :option:`-mno-gpopt` explicitly when building
  programs that include large amounts of small data, including large
  GOT data sections.  In this case, the 16-bit offset for GP-relative
  addressing may not be large enough to allow access to the entire 
  small data section.

.. option:: -mgprel-sec=regexp

  This option specifies additional section names that can be accessed via
  GP-relative addressing.  It is most useful in conjunction with 
  ``section`` attributes on variable declarations 
  (see :ref:`common-variable-attributes`) and a custom linker script.  
  The :samp:`{regexp}` is a POSIX Extended Regular Expression.

  This option does not affect the behavior of the :option:`-G` option, and 
  the specified sections are in addition to the standard ``.sdata``
  and ``.sbss`` small-data sections that are recognized by :option:`-mgpopt`.

.. option:: -mr0rel-sec=regexp

  This option specifies names of sections that can be accessed via a 
  16-bit offset from ``r0``; that is, in the low 32K or high 32K 
  of the 32-bit address space.  It is most useful in conjunction with 
  ``section`` attributes on variable declarations 
  (see :ref:`common-variable-attributes`) and a custom linker script.  
  The :samp:`{regexp}` is a POSIX Extended Regular Expression.

  In contrast to the use of GP-relative addressing for small data, 
  zero-based addressing is never generated by default and there are no 
  conventional section names used in standard linker scripts for sections
  in the low or high areas of memory.

.. option:: -mel, -meb

  Generate little-endian (default) or big-endian (experimental) code,
  respectively.

.. option:: -march=arch

  This specifies the name of the target Nios II architecture.  GCC uses this
  name to determine what kind of instructions it can emit when generating
  assembly code.  Permissible names are: :samp:`r1`, :samp:`r2`.

  The preprocessor macro ``__nios2_arch__`` is available to programs,
  with value 1 or 2, indicating the targeted ISA level.

.. option:: -mbypass-cache, -mno-bypass-cache

  Force all load and store instructions to always bypass cache by 
  using I/O variants of the instructions. The default is not to
  bypass the cache.

.. option:: -mno-cache-volatile , -mcache-volatile, -mno-cache-volatile

  Volatile memory access bypass the cache using the I/O variants of 
  the load and store instructions. The default is not to bypass the cache.

.. option:: -mno-fast-sw-div, -mfast-sw-div

  Do not use table-based fast divide for small numbers. The default 
  is to use the fast divide at :option:`-O3` and above.

.. option:: -mno-hw-mul, -mhw-mul, -mno-hw-mulx, -mhw-mulx, -mno-hw-div, -mhw-div

  Enable or disable emitting ``mul``, ``mulx`` and ``div`` family of 
  instructions by the compiler. The default is to emit ``mul``
  and not emit ``div`` and ``mulx``.

``-mbmx`` ``-mno-bmx`` ``-mcdx`` ``-mno-cdx``
  Enable or disable generation of Nios II R2 BMX (bit manipulation) and
  CDX (code density) instructions.  Enabling these instructions also
  requires :option:`-march=r2`.  Since these instructions are optional
  extensions to the R2 architecture, the default is not to emit them.

.. option:: -mcustom-insn=N

  .. index:: mcustom-insn

  .. index:: mno-custom-insn

  Each :option:`-mcustom-`:samp:`{insn}` = :samp:`{N}` option enables use of a
  custom instruction with encoding :samp:`{N}` when generating code that uses 
  :samp:`{insn}`.  For example, :option:`-mcustom-fadds=253` generates custom
  instruction 253 for single-precision floating-point add operations instead
  of the default behavior of using a library call.

  The following values of :samp:`{insn}` are supported.  Except as otherwise
  noted, floating-point operations are expected to be implemented with
  normal IEEE 754 semantics and correspond directly to the C operators or the
  equivalent GCC built-in functions (see :ref:`other-builtins`).

  Single-precision floating point:

  :samp:`{fadds}, {fsubs}, {fdivs}, {fmuls}`
    Binary arithmetic operations.

  fnegs
    Unary negation.

  fabss
    Unary absolute value.

  :samp:`{fcmpeqs}, {fcmpges}, {fcmpgts}, {fcmples}, {fcmplts}, {fcmpnes}`
    Comparison operations.

  :samp:`{fmins}, {fmaxs}`
    Floating-point minimum and maximum.  These instructions are only
    generated if :option:`-ffinite-math-only` is specified.

  fsqrts
    Unary square root operation.

  :samp:`{fcoss}, {fsins}, {ftans}, {fatans}, {fexps}, {flogs}`
    Floating-point trigonometric and exponential functions.  These instructions
    are only generated if :option:`-funsafe-math-optimizations` is also specified.

    Double-precision floating point:

  :samp:`{faddd}, {fsubd}, {fdivd}, {fmuld}`
    Binary arithmetic operations.

  fnegd
    Unary negation.

  fabsd
    Unary absolute value.

  :samp:`{fcmpeqd}, {fcmpged}, {fcmpgtd}, {fcmpled}, {fcmpltd}, {fcmpned}`
    Comparison operations.

  :samp:`{fmind}, {fmaxd}`
    Double-precision minimum and maximum.  These instructions are only
    generated if :option:`-ffinite-math-only` is specified.

  fsqrtd
    Unary square root operation.

  :samp:`{fcosd}, {fsind}, {ftand}, {fatand}, {fexpd}, {flogd}`
    Double-precision trigonometric and exponential functions.  These instructions
    are only generated if :option:`-funsafe-math-optimizations` is also specified.

    Conversions:

  fextsd
    Conversion from single precision to double precision.

  ftruncds
    Conversion from double precision to single precision.

  :samp:`{fixsi}, {fixsu}, {fixdi}, {fixdu}`
    Conversion from floating point to signed or unsigned integer types, with
    truncation towards zero.

  round
    Conversion from single-precision floating point to signed integer,
    rounding to the nearest integer and ties away from zero.
    This corresponds to the ``__builtin_lroundf`` function when
    :option:`-fno-math-errno` is used.

  :samp:`{floatis}, {floatus}, {floatid}, {floatud}`
    Conversion from signed or unsigned integer types to floating-point types.

    In addition, all of the following transfer instructions for internal
  registers X and Y must be provided to use any of the double-precision
  floating-point instructions.  Custom instructions taking two
  double-precision source operands expect the first operand in the
  64-bit register X.  The other operand (or only operand of a unary
  operation) is given to the custom arithmetic instruction with the
  least significant half in source register :samp:`{src1}` and the most
  significant half in :samp:`{src2}`.  A custom instruction that returns a
  double-precision result returns the most significant 32 bits in the
  destination register and the other half in 32-bit register Y.  
  GCC automatically generates the necessary code sequences to write
  register X and/or read register Y when double-precision floating-point
  instructions are used.

  fwrx
    Write :samp:`{src1}` into the least significant half of X and :samp:`{src2}` into
    the most significant half of X.

  fwry
    Write :samp:`{src1}` into Y.

  :samp:`{frdxhi}, {frdxlo}`
    Read the most or least (respectively) significant half of X and store it in
    :samp:`{dest}`.

  frdy
    Read the value of Y and store it into :samp:`{dest}`.

    Note that you can gain more local control over generation of Nios II custom
  instructions by using the ``target("custom-insn=N")``
  and ``target("no-custom-insn")`` function attributes
  (see :ref:`function-attributes`)
  or pragmas (see :ref:`function-specific-option-pragmas`).

.. option:: -mcustom-fpu-cfg=name

  This option enables a predefined, named set of custom instruction encodings
  (see :option:`-mcustom-`:samp:`{insn}` above).  
  Currently, the following sets are defined:

  :option:`-mcustom-fpu-cfg=60-1` is equivalent to:

  :option:`-mcustom-fmuls=252` 
  :option:`-mcustom-fadds=253` 
  :option:`-mcustom-fsubs=254` 
  :option:`-fsingle-precision-constant`
  :option:`-mcustom-fpu-cfg=60-2` is equivalent to:

  :option:`-mcustom-fmuls=252` 
  :option:`-mcustom-fadds=253` 
  :option:`-mcustom-fsubs=254` 
  :option:`-mcustom-fdivs=255` 
  :option:`-fsingle-precision-constant`
  :option:`-mcustom-fpu-cfg=72-3` is equivalent to:

  :option:`-mcustom-floatus=243` 
  :option:`-mcustom-fixsi=244` 
  :option:`-mcustom-floatis=245` 
  :option:`-mcustom-fcmpgts=246` 
  :option:`-mcustom-fcmples=249` 
  :option:`-mcustom-fcmpeqs=250` 
  :option:`-mcustom-fcmpnes=251` 
  :option:`-mcustom-fmuls=252` 
  :option:`-mcustom-fadds=253` 
  :option:`-mcustom-fsubs=254` 
  :option:`-mcustom-fdivs=255` 
  :option:`-fsingle-precision-constant`
  :option:`-mcustom-fpu-cfg=fph2` is equivalent to:

  :option:`-mcustom-fabss=224` 
  :option:`-mcustom-fnegs=225` 
  :option:`-mcustom-fcmpnes=226` 
  :option:`-mcustom-fcmpeqs=227` 
  :option:`-mcustom-fcmpges=228` 
  :option:`-mcustom-fcmpgts=229` 
  :option:`-mcustom-fcmples=230` 
  :option:`-mcustom-fcmplts=231` 
  :option:`-mcustom-fmaxs=232` 
  :option:`-mcustom-fmins=233` 
  :option:`-mcustom-round=248` 
  :option:`-mcustom-fixsi=249` 
  :option:`-mcustom-floatis=250` 
  :option:`-mcustom-fsqrts=251` 
  :option:`-mcustom-fmuls=252` 
  :option:`-mcustom-fadds=253` 
  :option:`-mcustom-fsubs=254` 
  :option:`-mcustom-fdivs=255` 
  Custom instruction assignments given by individual
  :option:`-mcustom-`:samp:`{insn}` = options override those given by
  :option:`-mcustom-fpu-cfg` =, regardless of the
  order of the options on the command line.

  Note that you can gain more local control over selection of a FPU
  configuration by using the ``target("custom-fpu-cfg=name")``
  function attribute (see :ref:`function-attributes`)
  or pragma (see :ref:`function-specific-option-pragmas`).

  The name :samp:`{fph2}` is an abbreviation for *Nios II Floating Point
  Hardware 2 Component*.  Please note that the custom instructions enabled by
  :option:`-mcustom-fmins=233` and :option:`-mcustom-fmaxs=234` are only generated
  if :option:`-ffinite-math-only` is specified.  The custom instruction enabled by
  :option:`-mcustom-round=248` is only generated if :option:`-fno-math-errno` is
  specified.  In contrast to the other configurations,
  :option:`-fsingle-precision-constant` is not set.

These additional :samp:`-m` options are available for the Altera Nios II
ELF (bare-metal) target:

.. option:: -mhal

  Link with HAL BSP.  This suppresses linking with the GCC-provided C runtime
  startup and termination code, and is typically used in conjunction with
  :option:`-msys-crt0` = to specify the location of the alternate startup code
  provided by the HAL BSP.

.. option:: -msmallc

  Link with a limited version of the C library, :option:`-lsmallc` , rather than
  Newlib.

.. option:: -msys-crt0=startfile

  :samp:`{startfile}` is the file name of the startfile (crt0) to use 
  when linking.  This option is only useful in conjunction with :option:`-mhal`.

.. option:: -msys-lib=systemlib

  :samp:`{systemlib}` is the library name of the library that provides
  low-level system calls required by the C library,
  e.g. ``read`` and ``write``.
  This option is typically used to link with a library provided by a HAL BSP.

.. _nvidia-ptx-options:

Nvidia PTX Options
^^^^^^^^^^^^^^^^^^

.. index:: Nvidia PTX options

.. index:: nvptx options

These options are defined for Nvidia PTX:

.. option:: -m64

  Ignored, but preserved for backward compatibility.  Only 64-bit ABI is
  supported.

.. option:: -misa=ISA-string

  Generate code for given the specified PTX ISA (e.g. :samp:`sm_35`).  ISA
  strings must be lower-case.  Valid ISA strings include :samp:`sm_30` and
  :samp:`sm_35`.  The default ISA is sm_35.

.. option:: -mmainkernel

  Link in code for a __main kernel.  This is for stand-alone instead of
  offloading execution.

.. option:: -moptimize

  Apply partitioned execution optimizations.  This is the default when any
  level of optimization is selected.

.. option:: -msoft-stack

  Generate code that does not use ``.local`` memory
  directly for stack storage. Instead, a per-warp stack pointer is
  maintained explicitly. This enables variable-length stack allocation (with
  variable-length arrays or ``alloca``), and when global memory is used for
  underlying storage, makes it possible to access automatic variables from other
  threads, or with atomic instructions. This code generation variant is used
  for OpenMP offloading, but the option is exposed on its own for the purpose
  of testing the compiler; to generate code suitable for linking into programs
  using OpenMP offloading, use option :option:`-mgomp`.

.. option:: -muniform-simt

  Switch to code generation variant that allows to execute all threads in each
  warp, while maintaining memory state and side effects as if only one thread
  in each warp was active outside of OpenMP SIMD regions.  All atomic operations
  and calls to runtime (malloc, free, vprintf) are conditionally executed (iff
  current lane index equals the master lane index), and the register being
  assigned is copied via a shuffle instruction from the master lane.  Outside of
  SIMD regions lane 0 is the master; inside, each thread sees itself as the
  master.  Shared memory array ``int __nvptx_uni[]`` stores all-zeros or
  all-ones bitmasks for each warp, indicating current mode (0 outside of SIMD
  regions).  Each thread can bitwise-and the bitmask at position ``tid.y``
  with current lane index to compute the master lane index.

.. option:: -mgomp

  Generate code for use in OpenMP offloading: enables :option:`-msoft-stack` and
  :option:`-muniform-simt` options, and selects corresponding multilib variant.

.. _openrisc-options:

OpenRISC Options
^^^^^^^^^^^^^^^^

.. index:: OpenRISC Options

These options are defined for OpenRISC:

.. option:: -mboard=name

  Configure a board specific runtime.  This will be passed to the linker for
  newlib board library linking.  The default is ``or1ksim``.

.. option:: -mnewlib

  This option is ignored; it is for compatibility purposes only.  This used to
  select linker and preprocessor options for use with newlib.

.. option:: -msoft-div, -mhard-div

  Select software or hardware divide (``l.div``, ``l.divu``) instructions.
  This default is hardware divide.

.. option:: -msoft-mul, -mhard-mul

  Select software or hardware multiply (``l.mul``, ``l.muli``) instructions.
  This default is hardware multiply.

.. option:: -msoft-float, -mhard-float

  Select software or hardware for floating point operations.
  The default is software.

.. option:: -mdouble-float

  When :option:`-mhard-float` is selected, enables generation of double-precision
  floating point instructions.  By default functions from libgcc are used
  to perform double-precision floating point operations.

.. option:: -munordered-float

  When :option:`-mhard-float` is selected, enables generation of unordered
  floating point compare and set flag (``lf.sfun*``) instructions.  By default
  functions from libgcc are used to perform unordered floating point
  compare and set flag operations.

.. option:: -mcmov

  Enable generation of conditional move (``l.cmov``) instructions.  By
  default the equivalent will be generated using set and branch.

.. option:: -mror

  Enable generation of rotate right (``l.ror``) instructions.  By default
  functions from libgcc are used to perform rotate right operations.

.. option:: -mrori

  Enable generation of rotate right with immediate (``l.rori``) instructions.
  By default functions from libgcc are used to perform rotate right with
  immediate operations.

.. option:: -msext

  Enable generation of sign extension (``l.ext*``) instructions.  By default
  memory loads are used to perform sign extension.

.. option:: -msfimm

  Enable generation of compare and set flag with immediate (``l.sf*i``)
  instructions.  By default extra instructions will be generated to store the
  immediate to a register first.

.. option:: -mshftimm

  Enable generation of shift with immediate (``l.srai``, ``l.srli``,
  ``l.slli``) instructions.  By default extra instructions will be generated
  to store the immediate to a register first.

.. _pdp-11-options:

PDP-11 Options
^^^^^^^^^^^^^^

.. index:: PDP-11 Options

These options are defined for the PDP-11:

.. option:: -mfpu

  Use hardware FPP floating point.  This is the default.  (FIS floating
  point on the PDP-11/40 is not supported.)  Implies -m45.

.. option:: -msoft-float

  Do not use hardware floating point.

.. option:: -mac0

  Return floating-point results in ac0 (fr0 in Unix assembler syntax).

.. option:: -mno-ac0

  Return floating-point results in memory.  This is the default.

.. option:: -m40

  Generate code for a PDP-11/40.  Implies -msoft-float -mno-split.

.. option:: -m45

  Generate code for a PDP-11/45.  This is the default.

.. option:: -m10

  Generate code for a PDP-11/10.  Implies -msoft-float -mno-split.

.. option:: -mint16, -mno-int32

  Use 16-bit ``int``.  This is the default.

.. option:: -mint32, -mno-int16

  Use 32-bit ``int``.

.. option:: -msplit

  Target has split instruction and data space.  Implies -m45.

.. option:: -munix-asm

  Use Unix assembler syntax.

.. option:: -mdec-asm

  Use DEC assembler syntax.

.. option:: -mgnu-asm

  Use GNU assembler syntax.  This is the default.

.. option:: -mlra

  Use the new LRA register allocator.  By default, the old 'reload'
  allocator is used.

.. _picochip-options:

picoChip Options
^^^^^^^^^^^^^^^^

.. index:: picoChip options

These :samp:`-m` options are defined for picoChip implementations:

.. option:: -mae=ae_type

  Set the instruction set, register set, and instruction scheduling
  parameters for array element type :samp:`{ae_type}`.  Supported values
  for :samp:`{ae_type}` are :samp:`ANY`, :samp:`MUL`, and :samp:`MAC`.

  :option:`-mae=ANY` selects a completely generic AE type.  Code
  generated with this option runs on any of the other AE types.  The
  code is not as efficient as it would be if compiled for a specific
  AE type, and some types of operation (e.g., multiplication) do not
  work properly on all types of AE.

  :option:`-mae=MUL` selects a MUL AE type.  This is the most useful AE type
  for compiled code, and is the default.

  :option:`-mae=MAC` selects a DSP-style MAC AE.  Code compiled with this
  option may suffer from poor performance of byte (char) manipulation,
  since the DSP AE does not provide hardware support for byte load/stores.

``-msymbol-as-address``
  Enable the compiler to directly use a symbol name as an address in a
  load/store instruction, without first loading it into a
  register.  Typically, the use of this option generates larger
  programs, which run faster than when the option isn't used.  However, the
  results vary from program to program, so it is left as a user option,
  rather than being permanently enabled.

``-mno-inefficient-warnings``
  Disables warnings about the generation of inefficient code.  These
  warnings can be generated, for example, when compiling code that
  performs byte-level memory operations on the MAC AE type.  The MAC AE has
  no hardware support for byte-level memory operations, so all byte
  load/stores must be synthesized from word load/store operations.  This is
  inefficient and a warning is generated to indicate
  that you should rewrite the code to avoid byte operations, or to target
  an AE type that has the necessary hardware support.  This option disables
  these warnings.

  .. _powerpc-options:

PowerPC Options
^^^^^^^^^^^^^^^

.. index:: PowerPC options

These are listed under See :ref:`rs-6000-and-powerpc-options`.

.. _pru-options:

PRU Options
^^^^^^^^^^^

.. index:: PRU Options

These command-line options are defined for PRU target:

.. option:: -minrt

  Link with a minimum runtime environment, with no support for static
  initializers and constructors.  Using this option can significantly reduce
  the size of the final ELF binary.  Beware that the compiler could still
  generate code with static initializers and constructors.  It is up to the
  programmer to ensure that the source program will not use those features.

.. option:: -mmcu=mcu

  Specify the PRU MCU variant to use.  Check Newlib for the exact list of
  supported MCUs.

.. option:: -mno-relax

  Make GCC pass the :option:`--no-relax` command-line option to the linker
  instead of the :option:`--relax` option.

.. option:: -mloop

  Allow (or do not allow) GCC to use the LOOP instruction.

.. option:: -mabi=variant

  Specify the ABI variant to output code for.  :option:`-mabi=ti` selects the
  unmodified TI ABI while :option:`-mabi=gnu` selects a GNU variant that copes
  more naturally with certain GCC assumptions.  These are the differences:

  :samp:`Function Pointer Size`
    TI ABI specifies that function (code) pointers are 16-bit, whereas GNU
    supports only 32-bit data and code pointers.

  :samp:`Optional Return Value Pointer`
    Function return values larger than 64 bits are passed by using a hidden
    pointer as the first argument of the function.  TI ABI, though, mandates that
    the pointer can be NULL in case the caller is not using the returned value.
    GNU always passes and expects a valid return value pointer.

    The current :option:`-mabi=ti` implementation simply raises a compile error
  when any of the above code constructs is detected.  As a consequence
  the standard C library cannot be built and it is omitted when linking with
  :option:`-mabi=ti`.

  Relaxation is a GNU feature and for safety reasons is disabled when using
  :option:`-mabi=ti`.  The TI toolchain does not emit relocations for QBBx
  instructions, so the GNU linker cannot adjust them when shortening adjacent
  LDI32 pseudo instructions.

.. _risc-v-options:

RISC-V Options
^^^^^^^^^^^^^^

.. index:: RISC-V Options

These command-line options are defined for RISC-V targets:

.. option:: -mbranch-cost=n

  Set the cost of branches to roughly :samp:`{n}` instructions.

.. option:: -mplt, -plt

  When generating PIC code, do or don't allow the use of PLTs. Ignored for
  non-PIC.  The default is :option:`-mplt`.

.. option:: -mabi=ABI-string

  Specify integer and floating-point calling convention.  :samp:`{ABI-string}`
  contains two parts: the size of integer types and the registers used for
  floating-point types.  For example :samp:`-march=rv64ifd -mabi=lp64d` means that
  :samp:`long` and pointers are 64-bit (implicitly defining :samp:`int` to be
  32-bit), and that floating-point values up to 64 bits wide are passed in F
  registers.  Contrast this with :samp:`-march=rv64ifd -mabi=lp64f`, which still
  allows the compiler to generate code that uses the F and D extensions but only
  allows floating-point values up to 32 bits long to be passed in registers; or
  :samp:`-march=rv64ifd -mabi=lp64`, in which no floating-point arguments will be
  passed in registers.

  The default for this argument is system dependent, users who want a specific
  calling convention should specify one explicitly.  The valid calling
  conventions are: :samp:`ilp32`, :samp:`ilp32f`, :samp:`ilp32d`, :samp:`lp64`,
  :samp:`lp64f`, and :samp:`lp64d`.  Some calling conventions are impossible to
  implement on some ISAs: for example, :samp:`-march=rv32if -mabi=ilp32d` is
  invalid because the ABI requires 64-bit values be passed in F registers, but F
  registers are only 32 bits wide.  There is also the :samp:`ilp32e` ABI that can
  only be used with the :samp:`rv32e` architecture.  This ABI is not well
  specified at present, and is subject to change.

.. option:: -mfdiv

  Do or don't use hardware floating-point divide and square root instructions.
  This requires the F or D extensions for floating-point registers.  The default
  is to use them if the specified architecture has these instructions.

.. option:: -mdiv

  Do or don't use hardware instructions for integer division.  This requires the
  M extension.  The default is to use them if the specified architecture has
  these instructions.

.. option:: -march=ISA-string

  Generate code for given RISC-V ISA (e.g. :samp:`rv64im`).  ISA strings must be
  lower-case.  Examples include :samp:`rv64i`, :samp:`rv32g`, :samp:`rv32e`, and
  :samp:`rv32imaf`.

  When :option:`-march` = is not specified, use the setting from :option:`-mcpu`.

  If both :option:`-march` and :option:`-mcpu` = are not specified, the default for
  this argument is system dependent, users who want a specific architecture
  extensions should specify one explicitly.

.. option:: -mcpu=processor-string

  Use architecture of and optimize the output for the given processor, specified
  by particular CPU name.
  Permissible values for this option are: :samp:`sifive-e20`, :samp:`sifive-e21`,
  :samp:`sifive-e24`, :samp:`sifive-e31`, :samp:`sifive-e34`, :samp:`sifive-e76`,
  :samp:`sifive-s21`, :samp:`sifive-s51`, :samp:`sifive-s54`, :samp:`sifive-s76`,
  :samp:`sifive-u54`, and :samp:`sifive-u74`.

.. option:: -mtune=processor-string

  Optimize the output for the given processor, specified by microarchitecture or
  particular CPU name.  Permissible values for this option are: :samp:`rocket`,
  :samp:`sifive-3-series`, :samp:`sifive-5-series`, :samp:`sifive-7-series`,
  :samp:`size`, and all valid options for :option:`-mcpu` =.

  When :option:`-mtune` = is not specified, use the setting from :option:`-mcpu` ,
  the default is :samp:`rocket` if both are not specified.

  The :samp:`size` choice is not intended for use by end-users.  This is used
  when :option:`-Os` is specified.  It overrides the instruction cost info
  provided by :option:`-mtune` =, but does not override the pipeline info.  This
  helps reduce code size while still giving good performance.

.. option:: -mpreferred-stack-boundary=num

  Attempt to keep the stack boundary aligned to a 2 raised to :samp:`{num}`
  byte boundary.  If :option:`-mpreferred-stack-boundary` is not specified,
  the default is 4 (16 bytes or 128-bits).

  Warning: If you use this switch, then you must build all modules with
  the same value, including any libraries.  This includes the system libraries
  and startup modules.

.. option:: -msmall-data-limit=n

  Put global and static data smaller than :samp:`{n}` bytes into a special section
  (on some targets).

.. option:: -msave-restore

  Do or don't use smaller but slower prologue and epilogue code that uses
  library function calls.  The default is to use fast inline prologues and
  epilogues.

.. option:: -mshorten-memrefs

  Do or do not attempt to make more use of compressed load/store instructions by
  replacing a load/store of 'base register + large offset' with a new load/store
  of 'new base + small offset'.  If the new base gets stored in a compressed
  register, then the new load/store can be compressed.  Currently targets 32-bit
  integer load/stores only.

.. option:: -mstrict-align

  Do not or do generate unaligned memory accesses.  The default is set depending
  on whether the processor we are optimizing for supports fast unaligned access
  or not.

.. option:: -mcmodel=medlow

  Generate code for the medium-low code model. The program and its statically
  defined symbols must lie within a single 2 GiB address range and must lie
  between absolute addresses -2 GiB and +2 GiB. Programs can be
  statically or dynamically linked. This is the default code model.

.. option:: -mcmodel=medany

  Generate code for the medium-any code model. The program and its statically
  defined symbols must be within any single 2 GiB address range. Programs can be
  statically or dynamically linked.

``-mexplicit-relocs`` ``-mno-exlicit-relocs``
  Use or do not use assembler relocation operators when dealing with symbolic
  addresses.  The alternative is to use assembler macros instead, which may
  limit optimization.

``-mrelax`` ``-mno-relax``
  Take advantage of linker relaxations to reduce the number of instructions
  required to materialize symbol addresses. The default is to take advantage of
  linker relaxations.

``-memit-attribute`` ``-mno-emit-attribute``
  Emit (do not emit) RISC-V attribute to record extra information into ELF
  objects.  This feature requires at least binutils 2.32.

.. option:: -malign-data=type

  Control how GCC aligns variables and constants of array, structure, or union
  types.  Supported values for :samp:`{type}` are :samp:`xlen` which uses x register
  width as the alignment value, and :samp:`natural` which uses natural alignment.
  :samp:`xlen` is the default.

.. option:: -mbig-endian

  Generate big-endian code.  This is the default when GCC is configured for a
  :samp:`riscv64be-*-*` or :samp:`riscv32be-*-*` target.

.. option:: -mlittle-endian

  Generate little-endian code.  This is the default when GCC is configured for a
  :samp:`riscv64-*-*` or :samp:`riscv32-*-*` but not a :samp:`riscv64be-*-*` or
  :samp:`riscv32be-*-*` target.

.. option:: -mstack-protector-guard=guard

  Generate stack protection code using canary at :samp:`{guard}`.  Supported
  locations are :samp:`global` for a global canary or :samp:`tls` for per-thread
  canary in the TLS block.

  With the latter choice the options
  :option:`-mstack-protector-guard-reg`:samp:`={reg}` and
  :option:`-mstack-protector-guard-offset`:samp:`={offset}` furthermore specify
  which register to use as base register for reading the canary,
  and from what offset from that base register. There is no default
  register or offset as this is entirely for use within the Linux
  kernel.

.. _rl78-options:

RL78 Options
^^^^^^^^^^^^

.. index:: RL78 Options

.. option:: -msim

  Links in additional target libraries to support operation within a
  simulator.

.. option:: -mmul=none

  Specifies the type of hardware multiplication and division support to
  be used.  The simplest is ``none``, which uses software for both
  multiplication and division.  This is the default.  The ``g13``
  value is for the hardware multiply/divide peripheral found on the
  RL78/G13 (S2 core) targets.  The ``g14`` value selects the use of
  the multiplication and division instructions supported by the RL78/G14
  (S3 core) parts.  The value ``rl78`` is an alias for ``g14`` and
  the value ``mg10`` is an alias for ``none``.

  In addition a C preprocessor macro is defined, based upon the setting
  of this option.  Possible values are: ``__RL78_MUL_NONE__``,
  ``__RL78_MUL_G13__`` or ``__RL78_MUL_G14__``.

.. option:: -mcpu=g10

  Specifies the RL78 core to target.  The default is the G14 core, also
  known as an S3 core or just RL78.  The G13 or S2 core does not have
  multiply or divide instructions, instead it uses a hardware peripheral
  for these operations.  The G10 or S1 core does not have register
  banks, so it uses a different calling convention.

  If this option is set it also selects the type of hardware multiply
  support to use, unless this is overridden by an explicit
  :option:`-mmul=none` option on the command line.  Thus specifying
  :option:`-mcpu=g13` enables the use of the G13 hardware multiply
  peripheral and specifying :option:`-mcpu=g10` disables the use of
  hardware multiplications altogether.

  Note, although the RL78/G14 core is the default target, specifying
  :option:`-mcpu=g14` or :option:`-mcpu=rl78` on the command line does
  change the behavior of the toolchain since it also enables G14
  hardware multiply support.  If these options are not specified on the
  command line then software multiplication routines will be used even
  though the code targets the RL78 core.  This is for backwards
  compatibility with older toolchains which did not have hardware
  multiply and divide support.

  In addition a C preprocessor macro is defined, based upon the setting
  of this option.  Possible values are: ``__RL78_G10__``,
  ``__RL78_G13__`` or ``__RL78_G14__``.

.. option:: -mg10, -mg13, -mg14, -mrl78

  These are aliases for the corresponding :option:`-mcpu` = option.  They
  are provided for backwards compatibility.

.. option:: -mallregs

  Allow the compiler to use all of the available registers.  By default
  registers ``r24..r31`` are reserved for use in interrupt handlers.
  With this option enabled these registers can be used in ordinary
  functions as well.

.. option:: -m64bit-doubles, -m32bit-doubles

  Make the ``double`` data type be 64 bits ( :option:`-m64bit-doubles` )
  or 32 bits ( :option:`-m32bit-doubles` ) in size.  The default is
  :option:`-m32bit-doubles`.

.. option:: -msave-mduc-in-interrupts, -mno-save-mduc-in-interrupts

  Specifies that interrupt handler functions should preserve the
  MDUC registers.  This is only necessary if normal code might use
  the MDUC registers, for example because it performs multiplication
  and division operations.  The default is to ignore the MDUC registers
  as this makes the interrupt handlers faster.  The target option -mg13
  needs to be passed for this to work as this feature is only available
  on the G13 target (S2 core).  The MDUC registers will only be saved
  if the interrupt handler performs a multiplication or division
  operation or it calls another function.

.. _rs-6000-and-powerpc-options:

IBM RS/6000 and PowerPC Options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: RS/6000 and PowerPC Options

.. index:: IBM RS/6000 and PowerPC Options

These :samp:`-m` options are defined for the IBM RS/6000 and PowerPC:

.. option:: -mpowerpc-gpopt, -mno-powerpc-gpopt, -mpowerpc-gfxopt
.. option:: -mno-powerpc-gfxopt, -mpowerpc64, -mno-powerpc64, -mmfcrf, -mno-mfcrf
.. option:: -mpopcntb, -mno-popcntb, -mpopcntd, -mno-popcntd, -mfprnd, -mno-fprnd
.. option:: -mcmpb, -mno-cmpb, -mhard-dfp, -mno-hard-dfp

  You use these options to specify which instructions are available on the
  processor you are using.  The default value of these options is
  determined when configuring GCC.  Specifying the
  :option:`-mcpu`:samp:`={cpu_type}` overrides the specification of these
  options.  We recommend you use the :option:`-mcpu`:samp:`={cpu_type}` option
  rather than the options listed above.

  Specifying :option:`-mpowerpc-gpopt` allows
  GCC to use the optional PowerPC architecture instructions in the
  General Purpose group, including floating-point square root.  Specifying
  :option:`-mpowerpc-gfxopt` allows GCC to
  use the optional PowerPC architecture instructions in the Graphics
  group, including floating-point select.

  The :option:`-mmfcrf` option allows GCC to generate the move from
  condition register field instruction implemented on the POWER4
  processor and other processors that support the PowerPC V2.01
  architecture.
  The :option:`-mpopcntb` option allows GCC to generate the popcount and
  double-precision FP reciprocal estimate instruction implemented on the
  POWER5 processor and other processors that support the PowerPC V2.02
  architecture.
  The :option:`-mpopcntd` option allows GCC to generate the popcount
  instruction implemented on the POWER7 processor and other processors
  that support the PowerPC V2.06 architecture.
  The :option:`-mfprnd` option allows GCC to generate the FP round to
  integer instructions implemented on the POWER5+ processor and other
  processors that support the PowerPC V2.03 architecture.
  The :option:`-mcmpb` option allows GCC to generate the compare bytes
  instruction implemented on the POWER6 processor and other processors
  that support the PowerPC V2.05 architecture.
  The :option:`-mhard-dfp` option allows GCC to generate the decimal
  floating-point instructions implemented on some POWER processors.

  The :option:`-mpowerpc64` option allows GCC to generate the additional
  64-bit instructions that are found in the full PowerPC64 architecture
  and to treat GPRs as 64-bit, doubleword quantities.  GCC defaults to
  :option:`-mno-powerpc64`.

.. option:: -mcpu=cpu_type

  Set architecture type, register usage, and
  instruction scheduling parameters for machine type :samp:`{cpu_type}`.
  Supported values for :samp:`{cpu_type}` are :samp:`401`, :samp:`403`,
  :samp:`405`, :samp:`405fp`, :samp:`440`, :samp:`440fp`, :samp:`464`, :samp:`464fp`,
  :samp:`476`, :samp:`476fp`, :samp:`505`, :samp:`601`, :samp:`602`, :samp:`603`,
  :samp:`603e`, :samp:`604`, :samp:`604e`, :samp:`620`, :samp:`630`, :samp:`740`,
  :samp:`7400`, :samp:`7450`, :samp:`750`, :samp:`801`, :samp:`821`, :samp:`823`,
  :samp:`860`, :samp:`970`, :samp:`8540`, :samp:`a2`, :samp:`e300c2`,
  :samp:`e300c3`, :samp:`e500mc`, :samp:`e500mc64`, :samp:`e5500`,
  :samp:`e6500`, :samp:`ec603e`, :samp:`G3`, :samp:`G4`, :samp:`G5`,
  :samp:`titan`, :samp:`power3`, :samp:`power4`, :samp:`power5`, :samp:`power5+`,
  :samp:`power6`, :samp:`power6x`, :samp:`power7`, :samp:`power8`,
  :samp:`power9`, :samp:`future`, :samp:`powerpc`, :samp:`powerpc64`,
  :samp:`powerpc64le`, :samp:`rs64`, and :samp:`native`.

  :option:`-mcpu=powerpc` , :option:`-mcpu=powerpc64` , and
  :option:`-mcpu=powerpc64le` specify pure 32-bit PowerPC (either
  endian), 64-bit big endian PowerPC and 64-bit little endian PowerPC
  architecture machine types, with an appropriate, generic processor
  model assumed for scheduling purposes.

  Specifying :samp:`native` as cpu type detects and selects the
  architecture option that corresponds to the host processor of the
  system performing the compilation.
  :option:`-mcpu=native` has no effect if GCC does not recognize the
  processor.

  The other options specify a specific processor.  Code generated under
  those options runs best on that processor, and may not run at all on
  others.

  The :option:`-mcpu` options automatically enable or disable the
  following options:

  :option:`-maltivec`  :option:`-mfprnd`  :option:`-mhard-float`  :option:`-mmfcrf`  :option:`-mmultiple` 
  :option:`-mpopcntb`  :option:`-mpopcntd`  :option:`-mpowerpc64` 
  :option:`-mpowerpc-gpopt`  :option:`-mpowerpc-gfxopt` 
  :option:`-mmulhw`  :option:`-mdlmzb`  :option:`-mmfpgpr`  :option:`-mvsx` 
  :option:`-mcrypto`  :option:`-mhtm`  :option:`-mpower8-fusion`  :option:`-mpower8-vector` 
  :option:`-mquad-memory`  :option:`-mquad-memory-atomic`  :option:`-mfloat128` 
  :option:`-mfloat128-hardware` :option:`-mprefixed` :option:`-mpcrel` :option:`-mmma`
  The particular options set for any particular CPU varies between
  compiler versions, depending on what setting seems to produce optimal
  code for that CPU; it doesn't necessarily reflect the actual hardware's
  capabilities.  If you wish to set an individual option to a particular
  value, you may specify it after the :option:`-mcpu` option, like
  :option:`-mcpu=970 -mno-altivec`.

  On AIX, the :option:`-maltivec` and :option:`-mpowerpc64` options are
  not enabled or disabled by the :option:`-mcpu` option at present because
  AIX does not have full support for these options.  You may still
  enable or disable them individually if you're sure it'll work in your
  environment.

.. option:: -mtune=cpu_type

  Set the instruction scheduling parameters for machine type
  :samp:`{cpu_type}` , but do not set the architecture type or register usage,
  as :option:`-mcpu`:samp:`={cpu_type}` does.  The same
  values for :samp:`{cpu_type}` are used for :option:`-mtune` as for
  :option:`-mcpu`.  If both are specified, the code generated uses the
  architecture and registers set by :option:`-mcpu` , but the
  scheduling parameters set by :option:`-mtune`.

.. option:: -mcmodel=small

  Generate PowerPC64 code for the small model: The TOC is limited to
  64k.

.. option:: -mcmodel=medium

  Generate PowerPC64 code for the medium model: The TOC and other static
  data may be up to a total of 4G in size.  This is the default for 64-bit
  Linux.

.. option:: -mcmodel=large

  Generate PowerPC64 code for the large model: The TOC may be up to 4G
  in size.  Other data and code is only limited by the 64-bit address
  space.

.. option:: -maltivec, -mno-altivec

  Generate code that uses (does not use) AltiVec instructions, and also
  enable the use of built-in functions that allow more direct access to
  the AltiVec instruction set.  You may also need to set
  :option:`-mabi=altivec` to adjust the current ABI with AltiVec ABI
  enhancements.

  When :option:`-maltivec` is used, the element order for AltiVec intrinsics
  such as ``vec_splat``, ``vec_extract``, and ``vec_insert`` 
  match array element order corresponding to the endianness of the
  target.  That is, element zero identifies the leftmost element in a
  vector register when targeting a big-endian platform, and identifies
  the rightmost element in a vector register when targeting a
  little-endian platform.

.. option:: -mvrsave, -mno-vrsave

  Generate VRSAVE instructions when generating AltiVec code.

.. option:: -msecure-plt

  Generate code that allows :command:`ld` and :command:`ld.so`
  to build executables and shared
  libraries with non-executable ``.plt`` and ``.got`` sections.
  This is a PowerPC
  32-bit SYSV ABI option.

.. option:: -mbss-plt

  Generate code that uses a BSS ``.plt`` section that :command:`ld.so`
  fills in, and
  requires ``.plt`` and ``.got``
  sections that are both writable and executable.
  This is a PowerPC 32-bit SYSV ABI option.

.. option:: -misel, -mno-isel

  This switch enables or disables the generation of ISEL instructions.

.. option:: -mvsx, -mno-vsx

  Generate code that uses (does not use) vector/scalar (VSX)
  instructions, and also enable the use of built-in functions that allow
  more direct access to the VSX instruction set.

.. option:: -mcrypto, -mno-crypto

  Enable the use (disable) of the built-in functions that allow direct
  access to the cryptographic instructions that were added in version
  2.07 of the PowerPC ISA.

.. option:: -mhtm, -mno-htm

  Enable (disable) the use of the built-in functions that allow direct
  access to the Hardware Transactional Memory (HTM) instructions that
  were added in version 2.07 of the PowerPC ISA.

.. option:: -mpower8-fusion, -mno-power8-fusion

  Generate code that keeps (does not keeps) some integer operations
  adjacent so that the instructions can be fused together on power8 and
  later processors.

.. option:: -mpower8-vector, -mno-power8-vector

  Generate code that uses (does not use) the vector and scalar
  instructions that were added in version 2.07 of the PowerPC ISA.  Also
  enable the use of built-in functions that allow more direct access to
  the vector instructions.

.. option:: -mquad-memory, -mno-quad-memory

  Generate code that uses (does not use) the non-atomic quad word memory
  instructions.  The :option:`-mquad-memory` option requires use of
  64-bit mode.

.. option:: -mquad-memory-atomic, -mno-quad-memory-atomic

  Generate code that uses (does not use) the atomic quad word memory
  instructions.  The :option:`-mquad-memory-atomic` option requires use of
  64-bit mode.

.. option:: -mfloat128, -mno-float128

  Enable/disable the :samp:`{__float128}` keyword for IEEE 128-bit floating point
  and use either software emulation for IEEE 128-bit floating point or
  hardware instructions.

  The VSX instruction set ( :option:`-mvsx` , :option:`-mcpu=power7` ,
  :option:`-mcpu=power8` ), or :option:`-mcpu=power9` must be enabled to
  use the IEEE 128-bit floating point support.  The IEEE 128-bit
  floating point support only works on PowerPC Linux systems.

  The default for :option:`-mfloat128` is enabled on PowerPC Linux
  systems using the VSX instruction set, and disabled on other systems.

  If you use the ISA 3.0 instruction set ( :option:`-mpower9-vector` or
  :option:`-mcpu=power9` ) on a 64-bit system, the IEEE 128-bit floating
  point support will also enable the generation of ISA 3.0 IEEE 128-bit
  floating point instructions.  Otherwise, if you do not specify to
  generate ISA 3.0 instructions or you are targeting a 32-bit big endian
  system, IEEE 128-bit floating point will be done with software
  emulation.

.. option:: -mfloat128-hardware, -mno-float128-hardware

  Enable/disable using ISA 3.0 hardware instructions to support the
  :samp:`{__float128}` data type.

  The default for :option:`-mfloat128-hardware` is enabled on PowerPC
  Linux systems using the ISA 3.0 instruction set, and disabled on other
  systems.

.. option:: -m32, -m64

  Generate code for 32-bit or 64-bit environments of Darwin and SVR4
  targets (including GNU/Linux).  The 32-bit environment sets int, long
  and pointer to 32 bits and generates code that runs on any PowerPC
  variant.  The 64-bit environment sets int to 32 bits and long and
  pointer to 64 bits, and generates code for PowerPC64, as for
  :option:`-mpowerpc64`.

.. option:: -mfull-toc, -mno-fp-in-toc, -mno-sum-in-toc, -mminimal-toc

  Modify generation of the TOC (Table Of Contents), which is created for
  every executable file.  The :option:`-mfull-toc` option is selected by
  default.  In that case, GCC allocates at least one TOC entry for
  each unique non-automatic variable reference in your program.  GCC
  also places floating-point constants in the TOC.  However, only
  16,384 entries are available in the TOC.

  If you receive a linker error message that saying you have overflowed
  the available TOC space, you can reduce the amount of TOC space used
  with the :option:`-mno-fp-in-toc` and :option:`-mno-sum-in-toc` options.
  :option:`-mno-fp-in-toc` prevents GCC from putting floating-point
  constants in the TOC and :option:`-mno-sum-in-toc` forces GCC to
  generate code to calculate the sum of an address and a constant at
  run time instead of putting that sum into the TOC.  You may specify one
  or both of these options.  Each causes GCC to produce very slightly
  slower and larger code at the expense of conserving TOC space.

  If you still run out of space in the TOC even when you specify both of
  these options, specify :option:`-mminimal-toc` instead.  This option causes
  GCC to make only one TOC entry for every file.  When you specify this
  option, GCC produces code that is slower and larger but which
  uses extremely little TOC space.  You may wish to use this option
  only on files that contain less frequently-executed code.

.. option:: -maix64, -maix32

  Enable 64-bit AIX ABI and calling convention: 64-bit pointers, 64-bit
  ``long`` type, and the infrastructure needed to support them.
  Specifying :option:`-maix64` implies :option:`-mpowerpc64` ,
  while :option:`-maix32` disables the 64-bit ABI and
  implies :option:`-mno-powerpc64`.  GCC defaults to :option:`-maix32`.

.. option:: -mxl-compat, -mno-xl-compat

  Produce code that conforms more closely to IBM XL compiler semantics
  when using AIX-compatible ABI.  Pass floating-point arguments to
  prototyped functions beyond the register save area (RSA) on the stack
  in addition to argument FPRs.  Do not assume that most significant
  double in 128-bit long double value is properly rounded when comparing
  values and converting to double.  Use XL symbol names for long double
  support routines.

  The AIX calling convention was extended but not initially documented to
  handle an obscure K&R C case of calling a function that takes the
  address of its arguments with fewer arguments than declared.  IBM XL
  compilers access floating-point arguments that do not fit in the
  RSA from the stack when a subroutine is compiled without
  optimization.  Because always storing floating-point arguments on the
  stack is inefficient and rarely needed, this option is not enabled by
  default and only is necessary when calling subroutines compiled by IBM
  XL compilers without optimization.

.. option:: -mpe

  Support :dfn:`IBM RS/6000 SP` :dfn:`Parallel Environment` (PE).  Link an
  application written to use message passing with special startup code to
  enable the application to run.  The system must have PE installed in the
  standard location (/usr/lpp/ppe.poe/), or the specs file
  must be overridden with the :option:`-specs` = option to specify the
  appropriate directory location.  The Parallel Environment does not
  support threads, so the :option:`-mpe` option and the :option:`-pthread`
  option are incompatible.

.. option:: -malign-natural, -malign-power

  On AIX, 32-bit Darwin, and 64-bit PowerPC GNU/Linux, the option
  :option:`-malign-natural` overrides the ABI-defined alignment of larger
  types, such as floating-point doubles, on their natural size-based boundary.
  The option :option:`-malign-power` instructs GCC to follow the ABI-specified
  alignment rules.  GCC defaults to the standard alignment defined in the ABI.

  On 64-bit Darwin, natural alignment is the default, and :option:`-malign-power`
  is not supported.

.. option:: -msoft-float, -mhard-float

  Generate code that does not use (uses) the floating-point register set.
  Software floating-point emulation is provided if you use the
  :option:`-msoft-float` option, and pass the option to GCC when linking.

.. option:: -mmultiple, -mno-multiple

  Generate code that uses (does not use) the load multiple word
  instructions and the store multiple word instructions.  These
  instructions are generated by default on POWER systems, and not
  generated on PowerPC systems.  Do not use :option:`-mmultiple` on little-endian
  PowerPC systems, since those instructions do not work when the
  processor is in little-endian mode.  The exceptions are PPC740 and
  PPC750 which permit these instructions in little-endian mode.

.. option:: -mupdate, -mno-update

  Generate code that uses (does not use) the load or store instructions
  that update the base register to the address of the calculated memory
  location.  These instructions are generated by default.  If you use
  :option:`-mno-update` , there is a small window between the time that the
  stack pointer is updated and the address of the previous frame is
  stored, which means code that walks the stack frame across interrupts or
  signals may get corrupted data.

.. option:: -mavoid-indexed-addresses, -mno-avoid-indexed-addresses

  Generate code that tries to avoid (not avoid) the use of indexed load
  or store instructions. These instructions can incur a performance
  penalty on Power6 processors in certain situations, such as when
  stepping through large arrays that cross a 16M boundary.  This option
  is enabled by default when targeting Power6 and disabled otherwise.

.. option:: -mfused-madd, -mno-fused-madd

  Generate code that uses (does not use) the floating-point multiply and
  accumulate instructions.  These instructions are generated by default
  if hardware floating point is used.  The machine-dependent
  :option:`-mfused-madd` option is now mapped to the machine-independent
  :option:`-ffp-contract=fast` option, and :option:`-mno-fused-madd` is
  mapped to :option:`-ffp-contract=off`.

.. option:: -mmulhw, -mno-mulhw

  Generate code that uses (does not use) the half-word multiply and
  multiply-accumulate instructions on the IBM 405, 440, 464 and 476 processors.
  These instructions are generated by default when targeting those
  processors.

.. option:: -mdlmzb, -mno-dlmzb

  Generate code that uses (does not use) the string-search :samp:`dlmzb`
  instruction on the IBM 405, 440, 464 and 476 processors.  This instruction is
  generated by default when targeting those processors.

.. option:: -mno-bit-align, -mbit-align

  On System V.4 and embedded PowerPC systems do not (do) force structures
  and unions that contain bit-fields to be aligned to the base type of the
  bit-field.

  For example, by default a structure containing nothing but 8
  ``unsigned`` bit-fields of length 1 is aligned to a 4-byte
  boundary and has a size of 4 bytes.  By using :option:`-mno-bit-align` ,
  the structure is aligned to a 1-byte boundary and is 1 byte in
  size.

.. option:: -mno-strict-align, -mstrict-align

  On System V.4 and embedded PowerPC systems do not (do) assume that
  unaligned memory references are handled by the system.

.. option:: -mrelocatable, -mno-relocatable

  Generate code that allows (does not allow) a static executable to be
  relocated to a different address at run time.  A simple embedded
  PowerPC system loader should relocate the entire contents of
  ``.got2`` and 4-byte locations listed in the ``.fixup`` section,
  a table of 32-bit addresses generated by this option.  For this to
  work, all objects linked together must be compiled with
  :option:`-mrelocatable` or :option:`-mrelocatable-lib`.
  :option:`-mrelocatable` code aligns the stack to an 8-byte boundary.

.. option:: -mrelocatable-lib, -mno-relocatable-lib

  Like :option:`-mrelocatable` , :option:`-mrelocatable-lib` generates a
  ``.fixup`` section to allow static executables to be relocated at
  run time, but :option:`-mrelocatable-lib` does not use the smaller stack
  alignment of :option:`-mrelocatable`.  Objects compiled with
  :option:`-mrelocatable-lib` may be linked with objects compiled with
  any combination of the :option:`-mrelocatable` options.

.. option:: -mno-toc, -mtoc

  On System V.4 and embedded PowerPC systems do not (do) assume that
  register 2 contains a pointer to a global area pointing to the addresses
  used in the program.

.. option:: -mlittle, -mlittle-endian

  On System V.4 and embedded PowerPC systems compile code for the
  processor in little-endian mode.  The :option:`-mlittle-endian` option is
  the same as :option:`-mlittle`.

.. option:: -mbig, -mbig-endian

  On System V.4 and embedded PowerPC systems compile code for the
  processor in big-endian mode.  The :option:`-mbig-endian` option is
  the same as :option:`-mbig`.

.. option:: -mdynamic-no-pic

  On Darwin and Mac OS X systems, compile code so that it is not
  relocatable, but that its external references are relocatable.  The
  resulting code is suitable for applications, but not shared
  libraries.

.. option:: -msingle-pic-base

  Treat the register used for PIC addressing as read-only, rather than
  loading it in the prologue for each function.  The runtime system is
  responsible for initializing this register with an appropriate value
  before execution begins.

.. option:: -mprioritize-restricted-insns=priority

  This option controls the priority that is assigned to
  dispatch-slot restricted instructions during the second scheduling
  pass.  The argument :samp:`{priority}` takes the value :samp:`0`, :samp:`1`,
  or :samp:`2` to assign no, highest, or second-highest (respectively) 
  priority to dispatch-slot restricted
  instructions.

.. option:: -msched-costly-dep=dependence_type

  This option controls which dependences are considered costly
  by the target during instruction scheduling.  The argument
  :samp:`{dependence_type}` takes one of the following values:

  no
    No dependence is costly.

  all
    All dependences are costly.

  true_store_to_load
    A true dependence from store to load is costly.

  store_to_load
    Any dependence from store to load is costly.

  number
    Any dependence for which the latency is greater than or equal to 
    :samp:`{number}` is costly.

.. option:: -minsert-sched-nops=scheme

  This option controls which NOP insertion scheme is used during
  the second scheduling pass.  The argument :samp:`{scheme}` takes one of the
  following values:

  no
    Don't insert NOPs.

  pad
    Pad with NOPs any dispatch group that has vacant issue slots,
    according to the scheduler's grouping.

  regroup_exact
    Insert NOPs to force costly dependent insns into
    separate groups.  Insert exactly as many NOPs as needed to force an insn
    to a new group, according to the estimated processor grouping.

  number
    Insert NOPs to force costly dependent insns into
    separate groups.  Insert :samp:`{number}` NOPs to force an insn to a new group.

.. option:: -mcall-sysv

  On System V.4 and embedded PowerPC systems compile code using calling
  conventions that adhere to the March 1995 draft of the System V
  Application Binary Interface, PowerPC processor supplement.  This is the
  default unless you configured GCC using :samp:`powerpc-*-eabiaix`.

.. option:: -mcall-sysv-eabi, -mcall-eabi

  Specify both :option:`-mcall-sysv` and :option:`-meabi` options.

.. option:: -mcall-sysv-noeabi

  Specify both :option:`-mcall-sysv` and :option:`-mno-eabi` options.

.. option:: -mcall-aixdesc, -m

  On System V.4 and embedded PowerPC systems compile code for the AIX
  operating system.

.. option:: -mcall-linux

  On System V.4 and embedded PowerPC systems compile code for the
  Linux-based GNU system.

.. option:: -mcall-freebsd

  On System V.4 and embedded PowerPC systems compile code for the
  FreeBSD operating system.

.. option:: -mcall-netbsd

  On System V.4 and embedded PowerPC systems compile code for the
  NetBSD operating system.

.. option:: -mcall-openbsd, -mcall-netbsd

  On System V.4 and embedded PowerPC systems compile code for the
  OpenBSD operating system.

.. option:: -mtraceback=traceback_type

  Select the type of traceback table. Valid values for :samp:`{traceback_type}`
  are :samp:`full`, :samp:`part`, and :samp:`no`.

.. option:: -maix-struct-return

  Return all structures in memory (as specified by the AIX ABI).

.. option:: -msvr4-struct-return

  Return structures smaller than 8 bytes in registers (as specified by the
  SVR4 ABI).

.. option:: -mabi=abi-type

  Extend the current ABI with a particular extension, or remove such extension.
  Valid values are: :samp:`altivec`, :samp:`no-altivec`,
  :samp:`ibmlongdouble`, :samp:`ieeelongdouble`,
  :samp:`elfv1`, :samp:`elfv2`,
  and for AIX: :samp:`vec-extabi`, :samp:`vec-default`.

.. option:: -mabi=ibmlongdouble

  Change the current ABI to use IBM extended-precision long double.
  This is not likely to work if your system defaults to using IEEE
  extended-precision long double.  If you change the long double type
  from IEEE extended-precision, the compiler will issue a warning unless
  you use the :option:`-Wno-psabi` option.  Requires :option:`-mlong-double-128`
  to be enabled.

.. option:: -mabi=ieeelongdouble

  Change the current ABI to use IEEE extended-precision long double.
  This is not likely to work if your system defaults to using IBM
  extended-precision long double.  If you change the long double type
  from IBM extended-precision, the compiler will issue a warning unless
  you use the :option:`-Wno-psabi` option.  Requires :option:`-mlong-double-128`
  to be enabled.

.. option:: -mabi=elfv1

  Change the current ABI to use the ELFv1 ABI.
  This is the default ABI for big-endian PowerPC 64-bit Linux.
  Overriding the default ABI requires special system support and is
  likely to fail in spectacular ways.

.. option:: -mabi=elfv2

  Change the current ABI to use the ELFv2 ABI.
  This is the default ABI for little-endian PowerPC 64-bit Linux.
  Overriding the default ABI requires special system support and is
  likely to fail in spectacular ways.

.. option:: -mgnu-attribute, -mno-gnu-attribute

  Emit .gnu_attribute assembly directives to set tag/value pairs in a
  .gnu.attributes section that specify ABI variations in function
  parameters or return values.

.. option:: -mprototype, -mno-prototype

  On System V.4 and embedded PowerPC systems assume that all calls to
  variable argument functions are properly prototyped.  Otherwise, the
  compiler must insert an instruction before every non-prototyped call to
  set or clear bit 6 of the condition code register (``CR``) to
  indicate whether floating-point values are passed in the floating-point
  registers in case the function takes variable arguments.  With
  :option:`-mprototype` , only calls to prototyped variable argument functions
  set or clear the bit.

.. option:: -msim

  On embedded PowerPC systems, assume that the startup module is called
  sim-crt0.o and that the standard C libraries are libsim.a and
  libc.a.  This is the default for :samp:`powerpc-*-eabisim`
  configurations.

.. option:: -mmvme

  On embedded PowerPC systems, assume that the startup module is called
  crt0.o and the standard C libraries are libmvme.a and
  libc.a.

.. option:: -mads

  On embedded PowerPC systems, assume that the startup module is called
  crt0.o and the standard C libraries are libads.a and
  libc.a.

.. option:: -myellowknife

  On embedded PowerPC systems, assume that the startup module is called
  crt0.o and the standard C libraries are libyk.a and
  libc.a.

.. option:: -mvxworks

  On System V.4 and embedded PowerPC systems, specify that you are
  compiling for a VxWorks system.

.. option:: -memb

  On embedded PowerPC systems, set the ``PPC_EMB`` bit in the ELF flags
  header to indicate that :samp:`eabi` extended relocations are used.

.. option:: -meabi, -mno-eabi

  On System V.4 and embedded PowerPC systems do (do not) adhere to the
  Embedded Applications Binary Interface (EABI), which is a set of
  modifications to the System V.4 specifications.  Selecting :option:`-meabi`
  means that the stack is aligned to an 8-byte boundary, a function
  ``__eabi`` is called from ``main`` to set up the EABI
  environment, and the :option:`-msdata` option can use both ``r2`` and
  ``r13`` to point to two separate small data areas.  Selecting
  :option:`-mno-eabi` means that the stack is aligned to a 16-byte boundary,
  no EABI initialization function is called from ``main``, and the
  :option:`-msdata` option only uses ``r13`` to point to a single
  small data area.  The :option:`-meabi` option is on by default if you
  configured GCC using one of the :samp:`powerpc*-*-eabi*` options.

.. option:: -msdata=eabi

  On System V.4 and embedded PowerPC systems, put small initialized
  ``const`` global and static data in the ``.sdata2`` section, which
  is pointed to by register ``r2``.  Put small initialized
  non-``const`` global and static data in the ``.sdata`` section,
  which is pointed to by register ``r13``.  Put small uninitialized
  global and static data in the ``.sbss`` section, which is adjacent to
  the ``.sdata`` section.  The :option:`-msdata=eabi` option is
  incompatible with the :option:`-mrelocatable` option.  The
  :option:`-msdata=eabi` option also sets the :option:`-memb` option.

.. option:: -msdata=sysv

  On System V.4 and embedded PowerPC systems, put small global and static
  data in the ``.sdata`` section, which is pointed to by register
  ``r13``.  Put small uninitialized global and static data in the
  ``.sbss`` section, which is adjacent to the ``.sdata`` section.
  The :option:`-msdata=sysv` option is incompatible with the
  :option:`-mrelocatable` option.

.. option:: -msdata=default

  On System V.4 and embedded PowerPC systems, if :option:`-meabi` is used,
  compile code the same as :option:`-msdata=eabi` , otherwise compile code the
  same as :option:`-msdata=sysv`.

.. option:: -msdata=data

  On System V.4 and embedded PowerPC systems, put small global
  data in the ``.sdata`` section.  Put small uninitialized global
  data in the ``.sbss`` section.  Do not use register ``r13``
  to address small data however.  This is the default behavior unless
  other :option:`-msdata` options are used.

.. option:: -msdata=none

  On embedded PowerPC systems, put all initialized global and static data
  in the ``.data`` section, and all uninitialized data in the
  ``.bss`` section.

.. option:: -mreadonly-in-sdata, -mno-readonly-in-sdata

  Put read-only objects in the ``.sdata`` section as well.  This is the
  default.

.. option:: -mblock-move-inline-limit=num

  Inline all block moves (such as calls to ``memcpy`` or structure
  copies) less than or equal to :samp:`{num}` bytes.  The minimum value for
  :samp:`{num}` is 32 bytes on 32-bit targets and 64 bytes on 64-bit
  targets.  The default value is target-specific.

.. option:: -mblock-compare-inline-limit=num

  Generate non-looping inline code for all block compares (such as calls
  to ``memcmp`` or structure compares) less than or equal to :samp:`{num}`
  bytes. If :samp:`{num}` is 0, all inline expansion (non-loop and loop) of
  block compare is disabled. The default value is target-specific.

.. option:: -mblock-compare-inline-loop-limit=num

  Generate an inline expansion using loop code for all block compares that
  are less than or equal to :samp:`{num}` bytes, but greater than the limit
  for non-loop inline block compare expansion. If the block length is not
  constant, at most :samp:`{num}` bytes will be compared before ``memcmp``
  is called to compare the remainder of the block. The default value is
  target-specific.

.. option:: -mstring-compare-inline-limit=num

  Compare at most :samp:`{num}` string bytes with inline code.
  If the difference or end of string is not found at the
  end of the inline compare a call to ``strcmp`` or ``strncmp`` will
  take care of the rest of the comparison. The default is 64 bytes.

.. option:: -G num, -G

  .. index:: smaller data references (PowerPC)

  .. index:: .sdata/.sdata2 references (PowerPC)

  On embedded PowerPC systems, put global and static items less than or
  equal to :samp:`{num}` bytes into the small data or BSS sections instead of
  the normal data or BSS section.  By default, :samp:`{num}` is 8.  The
  :option:`-G `:samp:`{num}` switch is also passed to the linker.
  All modules should be compiled with the same :option:`-G `:samp:`{num}` value.

.. option:: -mregnames, -mno-regnames

  On System V.4 and embedded PowerPC systems do (do not) emit register
  names in the assembly language output using symbolic forms.

.. option:: -mlongcall, -mno-longcall

  By default assume that all calls are far away so that a longer and more
  expensive calling sequence is required.  This is required for calls
  farther than 32 megabytes (33,554,432 bytes) from the current location.
  A short call is generated if the compiler knows
  the call cannot be that far away.  This setting can be overridden by
  the ``shortcall`` function attribute, or by ``#pragma
  longcall(0)``.

  Some linkers are capable of detecting out-of-range calls and generating
  glue code on the fly.  On these systems, long calls are unnecessary and
  generate slower code.  As of this writing, the AIX linker can do this,
  as can the GNU linker for PowerPC/64.  It is planned to add this feature
  to the GNU linker for 32-bit PowerPC systems as well.

  On PowerPC64 ELFv2 and 32-bit PowerPC systems with newer GNU linkers,
  GCC can generate long calls using an inline PLT call sequence (see
  :option:`-mpltseq` ).  PowerPC with :option:`-mbss-plt` and PowerPC64
  ELFv1 (big-endian) do not support inline PLT calls.

  On Darwin/PPC systems, ``#pragma longcall`` generates ``jbsr
  callee, L42``, plus a :dfn:`branch island` (glue code).  The two target
  addresses represent the callee and the branch island.  The
  Darwin/PPC linker prefers the first address and generates a ``bl
  callee`` if the PPC ``bl`` instruction reaches the callee directly;
  otherwise, the linker generates ``bl L42`` to call the branch
  island.  The branch island is appended to the body of the
  calling function; it computes the full 32-bit address of the callee
  and jumps to it.

  On Mach-O (Darwin) systems, this option directs the compiler emit to
  the glue for every direct call, and the Darwin linker decides whether
  to use or discard it.

  In the future, GCC may ignore all longcall specifications
  when the linker is known to generate glue.

.. option:: -mpltseq, -mno-pltseq

  Implement (do not implement) -fno-plt and long calls using an inline
  PLT call sequence that supports lazy linking and long calls to
  functions in dlopen'd shared libraries.  Inline PLT calls are only
  supported on PowerPC64 ELFv2 and 32-bit PowerPC systems with newer GNU
  linkers, and are enabled by default if the support is detected when
  configuring GCC, and, in the case of 32-bit PowerPC, if GCC is
  configured with :option:`--enable-secureplt`.  :option:`-mpltseq` code
  and :option:`-mbss-plt` 32-bit PowerPC relocatable objects may not be
  linked together.

.. option:: -mtls-markers, -mno-tls-markers

  Mark (do not mark) calls to ``__tls_get_addr`` with a relocation
  specifying the function argument.  The relocation allows the linker to
  reliably associate function call with argument setup instructions for
  TLS optimization, which in turn allows GCC to better schedule the
  sequence.

.. option:: -mrecip

  This option enables use of the reciprocal estimate and
  reciprocal square root estimate instructions with additional
  Newton-Raphson steps to increase precision instead of doing a divide or
  square root and divide for floating-point arguments.  You should use
  the :option:`-ffast-math` option when using :option:`-mrecip` (or at
  least :option:`-funsafe-math-optimizations` ,
  :option:`-ffinite-math-only` , :option:`-freciprocal-math` and
  :option:`-fno-trapping-math` ).  Note that while the throughput of the
  sequence is generally higher than the throughput of the non-reciprocal
  instruction, the precision of the sequence can be decreased by up to 2
  ulp (i.e. the inverse of 1.0 equals 0.99999994) for reciprocal square
  roots.

.. option:: -mrecip=opt

  This option controls which reciprocal estimate instructions
  may be used.  :samp:`{opt}` is a comma-separated list of options, which may
  be preceded by a ``!`` to invert the option:

  :samp:`all`
    Enable all estimate instructions.

  :samp:`default`
    Enable the default instructions, equivalent to :option:`-mrecip`.

  :samp:`none`
    Disable all estimate instructions, equivalent to :option:`-mno-recip`.

  :samp:`div`
    Enable the reciprocal approximation instructions for both 
    single and double precision.

  :samp:`divf`
    Enable the single-precision reciprocal approximation instructions.

  :samp:`divd`
    Enable the double-precision reciprocal approximation instructions.

  :samp:`rsqrt`
    Enable the reciprocal square root approximation instructions for both
    single and double precision.

  :samp:`rsqrtf`
    Enable the single-precision reciprocal square root approximation instructions.

  :samp:`rsqrtd`
    Enable the double-precision reciprocal square root approximation instructions.

    So, for example, :option:`-mrecip=all,!rsqrtd` enables
  all of the reciprocal estimate instructions, except for the
  ``FRSQRTE``, ``XSRSQRTEDP``, and ``XVRSQRTEDP`` instructions
  which handle the double-precision reciprocal square root calculations.

.. option:: -mrecip-precision

  Assume (do not assume) that the reciprocal estimate instructions
  provide higher-precision estimates than is mandated by the PowerPC
  ABI.  Selecting :option:`-mcpu=power6` , :option:`-mcpu=power7` or
  :option:`-mcpu=power8` automatically selects :option:`-mrecip-precision`.
  The double-precision square root estimate instructions are not generated by
  default on low-precision machines, since they do not provide an
  estimate that converges after three steps.

.. option:: -mveclibabi=type

  Specifies the ABI type to use for vectorizing intrinsics using an
  external library.  The only type supported at present is :samp:`mass`,
  which specifies to use IBM's Mathematical Acceleration Subsystem
  (MASS) libraries for vectorizing intrinsics using external libraries.
  GCC currently emits calls to ``acosd2``, ``acosf4``,
  ``acoshd2``, ``acoshf4``, ``asind2``, ``asinf4``,
  ``asinhd2``, ``asinhf4``, ``atan2d2``, ``atan2f4``,
  ``atand2``, ``atanf4``, ``atanhd2``, ``atanhf4``,
  ``cbrtd2``, ``cbrtf4``, ``cosd2``, ``cosf4``,
  ``coshd2``, ``coshf4``, ``erfcd2``, ``erfcf4``,
  ``erfd2``, ``erff4``, ``exp2d2``, ``exp2f4``,
  ``expd2``, ``expf4``, ``expm1d2``, ``expm1f4``,
  ``hypotd2``, ``hypotf4``, ``lgammad2``, ``lgammaf4``,
  ``log10d2``, ``log10f4``, ``log1pd2``, ``log1pf4``,
  ``log2d2``, ``log2f4``, ``logd2``, ``logf4``,
  ``powd2``, ``powf4``, ``sind2``, ``sinf4``, ``sinhd2``,
  ``sinhf4``, ``sqrtd2``, ``sqrtf4``, ``tand2``,
  ``tanf4``, ``tanhd2``, and ``tanhf4`` when generating code
  for power7.  Both :option:`-ftree-vectorize` and
  :option:`-funsafe-math-optimizations` must also be enabled.  The MASS
  libraries must be specified at link time.

.. option:: -mfriz

  Generate (do not generate) the ``friz`` instruction when the
  :option:`-funsafe-math-optimizations` option is used to optimize
  rounding of floating-point values to 64-bit integer and back to floating
  point.  The ``friz`` instruction does not return the same value if
  the floating-point number is too large to fit in an integer.

.. option:: -mpointers-to-nested-functions

  Generate (do not generate) code to load up the static chain register
  (``r11``) when calling through a pointer on AIX and 64-bit Linux
  systems where a function pointer points to a 3-word descriptor giving
  the function address, TOC value to be loaded in register ``r2``, and
  static chain value to be loaded in register ``r11``.  The
  :option:`-mpointers-to-nested-functions` is on by default.  You cannot
  call through pointers to nested functions or pointers
  to functions compiled in other languages that use the static chain if
  you use :option:`-mno-pointers-to-nested-functions`.

.. option:: -msave-toc-indirect

  Generate (do not generate) code to save the TOC value in the reserved
  stack location in the function prologue if the function calls through
  a pointer on AIX and 64-bit Linux systems.  If the TOC value is not
  saved in the prologue, it is saved just before the call through the
  pointer.  The :option:`-mno-save-toc-indirect` option is the default.

.. option:: -mcompat-align-parm

  Generate (do not generate) code to pass structure parameters with a
  maximum alignment of 64 bits, for compatibility with older versions
  of GCC.

  Older versions of GCC (prior to 4.9.0) incorrectly did not align a
  structure parameter on a 128-bit boundary when that structure contained
  a member requiring 128-bit alignment.  This is corrected in more
  recent versions of GCC.  This option may be used to generate code
  that is compatible with functions compiled with older versions of
  GCC.

  The :option:`-mno-compat-align-parm` option is the default.

.. option:: -mstack-protector-guard=guard

  Generate stack protection code using canary at :samp:`{guard}`.  Supported
  locations are :samp:`global` for global canary or :samp:`tls` for per-thread
  canary in the TLS block (the default with GNU libc version 2.4 or later).

  With the latter choice the options
  :option:`-mstack-protector-guard-reg`:samp:`={reg}` and
  :option:`-mstack-protector-guard-offset`:samp:`={offset}` furthermore specify
  which register to use as base register for reading the canary, and from what
  offset from that base register. The default for those is as specified in the
  relevant ABI.  :option:`-mstack-protector-guard-symbol`:samp:`={symbol}` overrides
  the offset with a symbol reference to a canary in the TLS block.

.. option:: -mpcrel, -mno-pcrel

  Generate (do not generate) pc-relative addressing when the option
  :option:`-mcpu=future` is used.  The :option:`-mpcrel` option requires
  that the medium code model ( :option:`-mcmodel=medium` ) and prefixed
  addressing ( :option:`-mprefixed` ) options are enabled.

.. option:: -mprefixed, -mno-prefixed

  Generate (do not generate) addressing modes using prefixed load and
  store instructions when the option :option:`-mcpu=future` is used.

.. option:: -mmma, -mno-mma

  Generate (do not generate) the MMA instructions when the option
  :option:`-mcpu=future` is used.

.. option:: -mblock-ops-unaligned-vsx, -block-ops-unaligned-vsx
.. option:: -no-block-ops-unaligned-vsx

  Generate (do not generate) unaligned vsx loads and stores for
  inline expansion of ``memcpy`` and ``memmove``.

.. _rx-options:

RX Options
^^^^^^^^^^

.. index:: RX Options

These command-line options are defined for RX targets:

.. option:: -m64bit-doubles, -m32bit-doubles

  Make the ``double`` data type be 64 bits ( :option:`-m64bit-doubles` )
  or 32 bits ( :option:`-m32bit-doubles` ) in size.  The default is
  :option:`-m32bit-doubles`.  *Note* RX floating-point hardware only
  works on 32-bit values, which is why the default is
  :option:`-m32bit-doubles`.

.. option:: -fpu, -nofpu

  Enables ( :option:`-fpu` ) or disables ( :option:`-nofpu` ) the use of RX
  floating-point hardware.  The default is enabled for the RX600
  series and disabled for the RX200 series.

  Floating-point instructions are only generated for 32-bit floating-point 
  values, however, so the FPU hardware is not used for doubles if the
  :option:`-m64bit-doubles` option is used.

  *Note* If the :option:`-fpu` option is enabled then
  :option:`-funsafe-math-optimizations` is also enabled automatically.
  This is because the RX FPU instructions are themselves unsafe.

.. option:: -mcpu=name

  Selects the type of RX CPU to be targeted.  Currently three types are
  supported, the generic :samp:`RX600` and :samp:`RX200` series hardware and
  the specific :samp:`RX610` CPU.  The default is :samp:`RX600`.

  The only difference between :samp:`RX600` and :samp:`RX610` is that the
  :samp:`RX610` does not support the ``MVTIPL`` instruction.

  The :samp:`RX200` series does not have a hardware floating-point unit
  and so :option:`-nofpu` is enabled by default when this type is
  selected.

.. option:: -mbig-endian-data, -mlittle-endian-data

  Store data (but not code) in the big-endian format.  The default is
  :option:`-mlittle-endian-data` , i.e. to store data in the little-endian
  format.

.. option:: -msmall-data-limit=N

  Specifies the maximum size in bytes of global and static variables
  which can be placed into the small data area.  Using the small data
  area can lead to smaller and faster code, but the size of area is
  limited and it is up to the programmer to ensure that the area does
  not overflow.  Also when the small data area is used one of the RX's
  registers (usually ``r13``) is reserved for use pointing to this
  area, so it is no longer available for use by the compiler.  This
  could result in slower and/or larger code if variables are pushed onto
  the stack instead of being held in this register.

  Note, common variables (variables that have not been initialized) and
  constants are not placed into the small data area as they are assigned
  to other sections in the output executable.

  The default value is zero, which disables this feature.  Note, this
  feature is not enabled by default with higher optimization levels
  ( :option:`-O2` etc) because of the potentially detrimental effects of
  reserving a register.  It is up to the programmer to experiment and
  discover whether this feature is of benefit to their program.  See the
  description of the :option:`-mpid` option for a description of how the
  actual register to hold the small data area pointer is chosen.

.. option:: -msim, -mno-sim

  Use the simulator runtime.  The default is to use the libgloss
  board-specific runtime.

.. option:: -mas100-syntax, -mno-as100-syntax

  When generating assembler output use a syntax that is compatible with
  Renesas's AS100 assembler.  This syntax can also be handled by the GAS
  assembler, but it has some restrictions so it is not generated by default.

.. option:: -mmax-constant-size=N

  Specifies the maximum size, in bytes, of a constant that can be used as
  an operand in a RX instruction.  Although the RX instruction set does
  allow constants of up to 4 bytes in length to be used in instructions,
  a longer value equates to a longer instruction.  Thus in some
  circumstances it can be beneficial to restrict the size of constants
  that are used in instructions.  Constants that are too big are instead
  placed into a constant pool and referenced via register indirection.

  The value :samp:`{N}` can be between 0 and 4.  A value of 0 (the default)
  or 4 means that constants of any size are allowed.

.. option:: -mrelax

  Enable linker relaxation.  Linker relaxation is a process whereby the
  linker attempts to reduce the size of a program by finding shorter
  versions of various instructions.  Disabled by default.

.. option:: -mint-register=N

  Specify the number of registers to reserve for fast interrupt handler
  functions.  The value :samp:`{N}` can be between 0 and 4.  A value of 1
  means that register ``r13`` is reserved for the exclusive use
  of fast interrupt handlers.  A value of 2 reserves ``r13`` and
  ``r12``.  A value of 3 reserves ``r13``, ``r12`` and
  ``r11``, and a value of 4 reserves ``r13`` through ``r10``.
  A value of 0, the default, does not reserve any registers.

.. option:: -msave-acc-in-interrupts

  Specifies that interrupt handler functions should preserve the
  accumulator register.  This is only necessary if normal code might use
  the accumulator register, for example because it performs 64-bit
  multiplications.  The default is to ignore the accumulator as this
  makes the interrupt handlers faster.

.. option:: -mpid, -mno-pid

  Enables the generation of position independent data.  When enabled any
  access to constant data is done via an offset from a base address
  held in a register.  This allows the location of constant data to be
  determined at run time without requiring the executable to be
  relocated, which is a benefit to embedded applications with tight
  memory constraints.  Data that can be modified is not affected by this
  option.

  Note, using this feature reserves a register, usually ``r13``, for
  the constant data base address.  This can result in slower and/or
  larger code, especially in complicated functions.

  The actual register chosen to hold the constant data base address
  depends upon whether the :option:`-msmall-data-limit` and/or the
  :option:`-mint-register` command-line options are enabled.  Starting
  with register ``r13`` and proceeding downwards, registers are
  allocated first to satisfy the requirements of :option:`-mint-register` ,
  then :option:`-mpid` and finally :option:`-msmall-data-limit`.  Thus it
  is possible for the small data area register to be ``r8`` if both
  :option:`-mint-register=4` and :option:`-mpid` are specified on the
  command line.

  By default this feature is not enabled.  The default can be restored
  via the :option:`-mno-pid` command-line option.

.. option:: -mno-warn-multiple-fast-interrupts, -mwarn-multiple-fast-interrupts

  Prevents GCC from issuing a warning message if it finds more than one
  fast interrupt handler when it is compiling a file.  The default is to
  issue a warning for each extra fast interrupt handler found, as the RX
  only supports one such interrupt.

.. option:: -mallow-string-insns, -mno-allow-string-insns

  Enables or disables the use of the string manipulation instructions
  ``SMOVF``, ``SCMPU``, ``SMOVB``, ``SMOVU``, ``SUNTIL``
  ``SWHILE`` and also the ``RMPA`` instruction.  These
  instructions may prefetch data, which is not safe to do if accessing
  an I/O register.  (See section 12.2.7 of the RX62N Group User's Manual
  for more information).

  The default is to allow these instructions, but it is not possible for
  GCC to reliably detect all circumstances where a string instruction
  might be used to access an I/O register, so their use cannot be
  disabled automatically.  Instead it is reliant upon the programmer to
  use the :option:`-mno-allow-string-insns` option if their program
  accesses I/O space.

  When the instructions are enabled GCC defines the C preprocessor
  symbol ``__RX_ALLOW_STRING_INSNS__``, otherwise it defines the
  symbol ``__RX_DISALLOW_STRING_INSNS__``.

.. option:: -mjsr, -mno-jsr

  Use only (or not only) ``JSR`` instructions to access functions.
  This option can be used when code size exceeds the range of ``BSR``
  instructions.  Note that :option:`-mno-jsr` does not mean to not use
  ``JSR`` but instead means that any type of branch may be used.

*Note:* The generic GCC command-line option :option:`-ffixed-`:samp:`{reg}`
has special significance to the RX port when used with the
``interrupt`` function attribute.  This attribute indicates a
function intended to process fast interrupts.  GCC ensures
that it only uses the registers ``r10``, ``r11``, ``r12``
and/or ``r13`` and only provided that the normal use of the
corresponding registers have been restricted via the
:option:`-ffixed-`:samp:`{reg}` or :option:`-mint-register` command-line
options.

.. _s-390-and-zseries-options:

S/390 and zSeries Options
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: S/390 and zSeries Options

These are the :samp:`-m` options defined for the S/390 and zSeries architecture.

.. option:: -mhard-float, -msoft-float

  Use (do not use) the hardware floating-point instructions and registers
  for floating-point operations.  When :option:`-msoft-float` is specified,
  functions in libgcc.a are used to perform floating-point
  operations.  When :option:`-mhard-float` is specified, the compiler
  generates IEEE floating-point instructions.  This is the default.

.. option:: -mhard-dfp, -mno-hard-dfp

  Use (do not use) the hardware decimal-floating-point instructions for
  decimal-floating-point operations.  When :option:`-mno-hard-dfp` is
  specified, functions in libgcc.a are used to perform
  decimal-floating-point operations.  When :option:`-mhard-dfp` is
  specified, the compiler generates decimal-floating-point hardware
  instructions.  This is the default for :option:`-march=z9-ec` or higher.

.. option:: -mlong-double-64, -mlong-double-128

  These switches control the size of ``long double`` type. A size
  of 64 bits makes the ``long double`` type equivalent to the ``double``
  type. This is the default.

.. option:: -mbackchain, -mno-backchain

  Store (do not store) the address of the caller's frame as backchain pointer
  into the callee's stack frame.
  A backchain may be needed to allow debugging using tools that do not understand
  DWARF call frame information.
  When :option:`-mno-packed-stack` is in effect, the backchain pointer is stored
  at the bottom of the stack frame; when :option:`-mpacked-stack` is in effect,
  the backchain is placed into the topmost word of the 96/160 byte register
  save area.

  In general, code compiled with :option:`-mbackchain` is call-compatible with
  code compiled with :option:`-mno-backchain` ; however, use of the backchain
  for debugging purposes usually requires that the whole binary is built with
  :option:`-mbackchain`.  Note that the combination of :option:`-mbackchain` ,
  :option:`-mpacked-stack` and :option:`-mhard-float` is not supported.  In order
  to build a linux kernel use :option:`-msoft-float`.

  The default is to not maintain the backchain.

.. option:: -mpacked-stack, -mno-packed-stack

  Use (do not use) the packed stack layout.  When :option:`-mno-packed-stack` is
  specified, the compiler uses the all fields of the 96/160 byte register save
  area only for their default purpose; unused fields still take up stack space.
  When :option:`-mpacked-stack` is specified, register save slots are densely
  packed at the top of the register save area; unused space is reused for other
  purposes, allowing for more efficient use of the available stack space.
  However, when :option:`-mbackchain` is also in effect, the topmost word of
  the save area is always used to store the backchain, and the return address
  register is always saved two words below the backchain.

  As long as the stack frame backchain is not used, code generated with
  :option:`-mpacked-stack` is call-compatible with code generated with
  :option:`-mno-packed-stack`.  Note that some non-FSF releases of GCC 2.95 for
  S/390 or zSeries generated code that uses the stack frame backchain at run
  time, not just for debugging purposes.  Such code is not call-compatible
  with code compiled with :option:`-mpacked-stack`.  Also, note that the
  combination of :option:`-mbackchain` ,
  :option:`-mpacked-stack` and :option:`-mhard-float` is not supported.  In order
  to build a linux kernel use :option:`-msoft-float`.

  The default is to not use the packed stack layout.

.. option:: -msmall-exec, -mno-small-exec

  Generate (or do not generate) code using the ``bras`` instruction
  to do subroutine calls.
  This only works reliably if the total executable size does not
  exceed 64k.  The default is to use the ``basr`` instruction instead,
  which does not have this limitation.

.. option:: -m64, -m31

  When :option:`-m31` is specified, generate code compliant to the
  GNU/Linux for S/390 ABI.  When :option:`-m64` is specified, generate
  code compliant to the GNU/Linux for zSeries ABI.  This allows GCC in
  particular to generate 64-bit instructions.  For the :samp:`s390`
  targets, the default is :option:`-m31` , while the :samp:`s390x`
  targets default to :option:`-m64`.

.. option:: -mzarch, -mesa

  When :option:`-mzarch` is specified, generate code using the
  instructions available on z/Architecture.
  When :option:`-mesa` is specified, generate code using the
  instructions available on ESA/390.  Note that :option:`-mesa` is
  not possible with :option:`-m64`.
  When generating code compliant to the GNU/Linux for S/390 ABI,
  the default is :option:`-mesa`.  When generating code compliant
  to the GNU/Linux for zSeries ABI, the default is :option:`-mzarch`.

.. option:: -mhtm, -mno-htm

  The :option:`-mhtm` option enables a set of builtins making use of
  instructions available with the transactional execution facility
  introduced with the IBM zEnterprise EC12 machine generation
  S/390 System z Built-in Functions.
  :option:`-mhtm` is enabled by default when using :option:`-march=zEC12`.

.. option:: -mvx, -mno-vx

  When :option:`-mvx` is specified, generate code using the instructions
  available with the vector extension facility introduced with the IBM
  z13 machine generation.
  This option changes the ABI for some vector type values with regard to
  alignment and calling conventions.  In case vector type values are
  being used in an ABI-relevant context a GAS :samp:`.gnu_attribute`
  command will be added to mark the resulting binary with the ABI used.
  :option:`-mvx` is enabled by default when using :option:`-march=z13`.

.. option:: -mzvector, -mno-zvector

  The :option:`-mzvector` option enables vector language extensions and
  builtins using instructions available with the vector extension
  facility introduced with the IBM z13 machine generation.
  This option adds support for :samp:`vector` to be used as a keyword to
  define vector type variables and arguments.  :samp:`vector` is only
  available when GNU extensions are enabled.  It will not be expanded
  when requesting strict standard compliance e.g. with :option:`-std=c99`.
  In addition to the GCC low-level builtins :option:`-mzvector` enables
  a set of builtins added for compatibility with AltiVec-style
  implementations like Power and Cell.  In order to make use of these
  builtins the header file vecintrin.h needs to be included.
  :option:`-mzvector` is disabled by default.

.. option:: -mmvcle, -mno-mvcle

  Generate (or do not generate) code using the ``mvcle`` instruction
  to perform block moves.  When :option:`-mno-mvcle` is specified,
  use a ``mvc`` loop instead.  This is the default unless optimizing for
  size.

.. option:: -mdebug, -mno-debug

  Print (or do not print) additional debug information when compiling.
  The default is to not print debug information.

.. option:: -march=cpu-type

  Generate code that runs on :samp:`{cpu-type}` , which is the name of a
  system representing a certain processor type.  Possible values for
  :samp:`{cpu-type}` are :samp:`z900`/:samp:`arch5`, :samp:`z990`/:samp:`arch6`,
  :samp:`z9-109`, :samp:`z9-ec`/:samp:`arch7`, :samp:`z10`/:samp:`arch8`,
  :samp:`z196`/:samp:`arch9`, :samp:`zEC12`, :samp:`z13`/:samp:`arch11`,
  :samp:`z14`/:samp:`arch12`, :samp:`z15`/:samp:`arch13`, and :samp:`native`.

  The default is :option:`-march=z900`.

  Specifying :samp:`native` as cpu type can be used to select the best
  architecture option for the host processor.
  :option:`-march=native` has no effect if GCC does not recognize the
  processor.

.. option:: -mtune=cpu-type

  Tune to :samp:`{cpu-type}` everything applicable about the generated code,
  except for the ABI and the set of available instructions.
  The list of :samp:`{cpu-type}` values is the same as for :option:`-march`.
  The default is the value used for :option:`-march`.

.. option:: -mtpf-trace, -mno-tpf-trace

  Generate code that adds (does not add) in TPF OS specific branches to trace
  routines in the operating system.  This option is off by default, even
  when compiling for the TPF OS.

.. option:: -mtpf-trace-skip, -mno-tpf-trace-skip

  Generate code that changes (does not change) the default branch
  targets enabled by :option:`-mtpf-trace` to point to specialized trace
  routines providing the ability of selectively skipping function trace
  entries for the TPF OS.  This option is off by default, even when
  compiling for the TPF OS and specifying :option:`-mtpf-trace`.

.. option:: -mfused-madd, -mno-fused-madd

  Generate code that uses (does not use) the floating-point multiply and
  accumulate instructions.  These instructions are generated by default if
  hardware floating point is used.

.. option:: -mwarn-framesize=framesize

  Emit a warning if the current function exceeds the given frame size.  Because
  this is a compile-time check it doesn't need to be a real problem when the program
  runs.  It is intended to identify functions that most probably cause
  a stack overflow.  It is useful to be used in an environment with limited stack
  size e.g. the linux kernel.

.. option:: -mwarn-dynamicstack

  Emit a warning if the function calls ``alloca`` or uses dynamically-sized
  arrays.  This is generally a bad idea with a limited stack size.

.. option:: -mstack-guard=stack-guard

  If these options are provided the S/390 back end emits additional instructions in
  the function prologue that trigger a trap if the stack size is :samp:`{stack-guard}`
  bytes above the :samp:`{stack-size}` (remember that the stack on S/390 grows downward).
  If the :samp:`{stack-guard}` option is omitted the smallest power of 2 larger than
  the frame size of the compiled function is chosen.
  These options are intended to be used to help debugging stack overflow problems.
  The additionally emitted code causes only little overhead and hence can also be
  used in production-like systems without greater performance degradation.  The given
  values have to be exact powers of 2 and :samp:`{stack-size}` has to be greater than
  :samp:`{stack-guard}` without exceeding 64k.
  In order to be efficient the extra code makes the assumption that the stack starts
  at an address aligned to the value given by :samp:`{stack-size}`.
  The :samp:`{stack-guard}` option can only be used in conjunction with :samp:`{stack-size}`.

.. option:: -mhotpatch=pre-halfwords,post-halfwords

  If the hotpatch option is enabled, a 'hot-patching' function
  prologue is generated for all functions in the compilation unit.
  The funtion label is prepended with the given number of two-byte
  NOP instructions ( :samp:`{pre-halfwords}` , maximum 1000000).  After
  the label, 2 * :samp:`{post-halfwords}` bytes are appended, using the
  largest NOP like instructions the architecture allows (maximum
  1000000).

  If both arguments are zero, hotpatching is disabled.

  This option can be overridden for individual functions with the
  ``hotpatch`` attribute.

.. _score-options:

Score Options
^^^^^^^^^^^^^

.. index:: Score Options

These options are defined for Score implementations:

.. option:: -meb

  Compile code for big-endian mode.  This is the default.

.. option:: -mel

  Compile code for little-endian mode.

.. option:: -mnhwloop

  Disable generation of ``bcnz`` instructions.

.. option:: -muls

  Enable generation of unaligned load and store instructions.

.. option:: -mmac

  Enable the use of multiply-accumulate instructions. Disabled by default.

.. option:: -mscore5

  Specify the SCORE5 as the target architecture.

.. option:: -mscore5u

  Specify the SCORE5U of the target architecture.

.. option:: -mscore7

  Specify the SCORE7 as the target architecture. This is the default.

.. option:: -mscore7d

  Specify the SCORE7D as the target architecture.

.. _sh-options:

SH Options
^^^^^^^^^^

These :samp:`-m` options are defined for the SH implementations:

.. option:: -m1

  Generate code for the SH1.

.. option:: -m2

  Generate code for the SH2.

``-m2e``
  Generate code for the SH2e.

.. option:: -m2a-nofpu

  Generate code for the SH2a without FPU, or for a SH2a-FPU in such a way
  that the floating-point unit is not used.

.. option:: -m2a-single-only

  Generate code for the SH2a-FPU, in such a way that no double-precision
  floating-point operations are used.

.. option:: -m2a-single

  Generate code for the SH2a-FPU assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m2a

  Generate code for the SH2a-FPU assuming the floating-point unit is in
  double-precision mode by default.

.. option:: -m3

  Generate code for the SH3.

.. option:: -m3e

  Generate code for the SH3e.

.. option:: -m4-nofpu

  Generate code for the SH4 without a floating-point unit.

.. option:: -m4-single-only

  Generate code for the SH4 with a floating-point unit that only
  supports single-precision arithmetic.

.. option:: -m4-single

  Generate code for the SH4 assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m4

  Generate code for the SH4.

.. option:: -m4-100

  Generate code for SH4-100.

.. option:: -m4-100-nofpu

  Generate code for SH4-100 in such a way that the
  floating-point unit is not used.

.. option:: -m4-100-single

  Generate code for SH4-100 assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m4-100-single-only

  Generate code for SH4-100 in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4-200

  Generate code for SH4-200.

.. option:: -m4-200-nofpu

  Generate code for SH4-200 without in such a way that the
  floating-point unit is not used.

.. option:: -m4-200-single

  Generate code for SH4-200 assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m4-200-single-only

  Generate code for SH4-200 in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4-300

  Generate code for SH4-300.

.. option:: -m4-300-nofpu

  Generate code for SH4-300 without in such a way that the
  floating-point unit is not used.

.. option:: -m4-300-single

  Generate code for SH4-300 in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4-300-single-only

  Generate code for SH4-300 in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4-340

  Generate code for SH4-340 (no MMU, no FPU).

.. option:: -m4-500

  Generate code for SH4-500 (no FPU).  Passes :option:`-isa=sh4-nofpu` to the
  assembler.

.. option:: -m4a-nofpu

  Generate code for the SH4al-dsp, or for a SH4a in such a way that the
  floating-point unit is not used.

.. option:: -m4a-single-only

  Generate code for the SH4a, in such a way that no double-precision
  floating-point operations are used.

.. option:: -m4a-single

  Generate code for the SH4a assuming the floating-point unit is in
  single-precision mode by default.

.. option:: -m4a

  Generate code for the SH4a.

.. option:: -m4al

  Same as :option:`-m4a-nofpu` , except that it implicitly passes
  :option:`-dsp` to the assembler.  GCC doesn't generate any DSP
  instructions at the moment.

.. option:: -mb

  Compile code for the processor in big-endian mode.

.. option:: -ml

  Compile code for the processor in little-endian mode.

.. option:: -mdalign

  Align doubles at 64-bit boundaries.  Note that this changes the calling
  conventions, and thus some functions from the standard C library do
  not work unless you recompile it first with :option:`-mdalign`.

.. option:: -mrelax

  Shorten some address references at link time, when possible; uses the
  linker option :option:`-relax`.

.. option:: -mbigtable

  Use 32-bit offsets in ``switch`` tables.  The default is to use
  16-bit offsets.

.. option:: -mbitops

  Enable the use of bit manipulation instructions on SH2A.

.. option:: -mfmovd

  Enable the use of the instruction ``fmovd``.  Check :option:`-mdalign` for
  alignment constraints.

.. option:: -mrenesas

  Comply with the calling conventions defined by Renesas.

.. option:: -mno-renesas

  Comply with the calling conventions defined for GCC before the Renesas
  conventions were available.  This option is the default for all
  targets of the SH toolchain.

.. option:: -mnomacsave

  Mark the ``MAC`` register as call-clobbered, even if
  :option:`-mrenesas` is given.

.. option:: -mieee, -mno-ieee

  Control the IEEE compliance of floating-point comparisons, which affects the
  handling of cases where the result of a comparison is unordered.  By default
  :option:`-mieee` is implicitly enabled.  If :option:`-ffinite-math-only` is
  enabled :option:`-mno-ieee` is implicitly set, which results in faster
  floating-point greater-equal and less-equal comparisons.  The implicit settings
  can be overridden by specifying either :option:`-mieee` or :option:`-mno-ieee`.

.. option:: -minline-ic_invalidate

  Inline code to invalidate instruction cache entries after setting up
  nested function trampolines.
  This option has no effect if :option:`-musermode` is in effect and the selected
  code generation option (e.g. :option:`-m4` ) does not allow the use of the ``icbi``
  instruction.
  If the selected code generation option does not allow the use of the ``icbi``
  instruction, and :option:`-musermode` is not in effect, the inlined code
  manipulates the instruction cache address array directly with an associative
  write.  This not only requires privileged mode at run time, but it also
  fails if the cache line had been mapped via the TLB and has become unmapped.

.. option:: -misize

  Dump instruction size and location in the assembly code.

.. option:: -mpadstruct

  This option is deprecated.  It pads structures to multiple of 4 bytes,
  which is incompatible with the SH ABI.

.. option:: -matomic-model=model

  .. index:: matomic-model=model

  Sets the model of atomic operations and additional parameters as a comma
  separated list.  For details on the atomic built-in functions see
  __atomic Builtins.  The following models and parameters are supported:

  :samp:`none`
    Disable compiler generated atomic sequences and emit library calls for atomic
    operations.  This is the default if the target is not ``sh*-*-linux*``.

  :samp:`soft-gusa`
    Generate GNU/Linux compatible gUSA software atomic sequences for the atomic
    built-in functions.  The generated atomic sequences require additional support
    from the interrupt/exception handling code of the system and are only suitable
    for SH3* and SH4* single-core systems.  This option is enabled by default when
    the target is ``sh*-*-linux*`` and SH3* or SH4*.  When the target is SH4A,
    this option also partially utilizes the hardware atomic instructions
    ``movli.l`` and ``movco.l`` to create more efficient code, unless
    :samp:`strict` is specified.  

  :samp:`soft-tcb`
    Generate software atomic sequences that use a variable in the thread control
    block.  This is a variation of the gUSA sequences which can also be used on
    SH1* and SH2* targets.  The generated atomic sequences require additional
    support from the interrupt/exception handling code of the system and are only
    suitable for single-core systems.  When using this model, the :samp:`gbr-offset=`
    parameter has to be specified as well.

  :samp:`soft-imask`
    Generate software atomic sequences that temporarily disable interrupts by
    setting ``SR.IMASK = 1111``.  This model works only when the program runs
    in privileged mode and is only suitable for single-core systems.  Additional
    support from the interrupt/exception handling code of the system is not
    required.  This model is enabled by default when the target is
    ``sh*-*-linux*`` and SH1* or SH2*.

  :samp:`hard-llcs`
    Generate hardware atomic sequences using the ``movli.l`` and ``movco.l``
    instructions only.  This is only available on SH4A and is suitable for
    multi-core systems.  Since the hardware instructions support only 32 bit atomic
    variables access to 8 or 16 bit variables is emulated with 32 bit accesses.
    Code compiled with this option is also compatible with other software
    atomic model interrupt/exception handling systems if executed on an SH4A
    system.  Additional support from the interrupt/exception handling code of the
    system is not required for this model.

  :samp:`gbr-offset=`
    This parameter specifies the offset in bytes of the variable in the thread
    control block structure that should be used by the generated atomic sequences
    when the :samp:`soft-tcb` model has been selected.  For other models this
    parameter is ignored.  The specified value must be an integer multiple of four
    and in the range 0-1020.

  :samp:`strict`
    This parameter prevents mixed usage of multiple atomic models, even if they
    are compatible, and makes the compiler generate atomic sequences of the
    specified model only.

.. option:: -mtas

  Generate the ``tas.b`` opcode for ``__atomic_test_and_set``.
  Notice that depending on the particular hardware and software configuration
  this can degrade overall performance due to the operand cache line flushes
  that are implied by the ``tas.b`` instruction.  On multi-core SH4A
  processors the ``tas.b`` instruction must be used with caution since it
  can result in data corruption for certain cache configurations.

.. option:: -mprefergot

  When generating position-independent code, emit function calls using
  the Global Offset Table instead of the Procedure Linkage Table.

.. option:: -musermode, -mno-usermode

  Don't allow (allow) the compiler generating privileged mode code.  Specifying
  :option:`-musermode` also implies :option:`-mno-inline-ic_invalidate` if the
  inlined code would not work in user mode.  :option:`-musermode` is the default
  when the target is ``sh*-*-linux*``.  If the target is SH1* or SH2*
  :option:`-musermode` has no effect, since there is no user mode.

.. option:: -multcost=number

  .. index:: multcost=number

  Set the cost to assume for a multiply insn.

.. option:: -mdiv=strategy

  .. index:: mdiv=strategy

  Set the division strategy to be used for integer division operations.
  :samp:`{strategy}` can be one of: 

  :samp:`call-div1`
    Calls a library function that uses the single-step division instruction
    ``div1`` to perform the operation.  Division by zero calculates an
    unspecified result and does not trap.  This is the default except for SH4,
    SH2A and SHcompact.

  :samp:`call-fp`
    Calls a library function that performs the operation in double precision
    floating point.  Division by zero causes a floating-point exception.  This is
    the default for SHcompact with FPU.  Specifying this for targets that do not
    have a double precision FPU defaults to ``call-div1``.

  :samp:`call-table`
    Calls a library function that uses a lookup table for small divisors and
    the ``div1`` instruction with case distinction for larger divisors.  Division
    by zero calculates an unspecified result and does not trap.  This is the default
    for SH4.  Specifying this for targets that do not have dynamic shift
    instructions defaults to ``call-div1``.

    When a division strategy has not been specified the default strategy is
  selected based on the current target.  For SH2A the default strategy is to
  use the ``divs`` and ``divu`` instructions instead of library function
  calls.

.. option:: -maccumulate-outgoing-args

  Reserve space once for outgoing arguments in the function prologue rather
  than around each call.  Generally beneficial for performance and size.  Also
  needed for unwinding to avoid changing the stack frame around conditional code.

.. option:: -mdivsi3_libfunc=name

  .. index:: mdivsi3_libfunc=name

  Set the name of the library function used for 32-bit signed division to
  :samp:`{name}`.
  This only affects the name used in the :samp:`call` division strategies, and
  the compiler still expects the same sets of input/output/clobbered registers as
  if this option were not present.

.. option:: -mfixed-range=register-range

  Generate code treating the given register range as fixed registers.
  A fixed register is one that the register allocator cannot use.  This is
  useful when compiling kernel code.  A register range is specified as
  two registers separated by a dash.  Multiple register ranges can be
  specified separated by a comma.

.. option:: -mbranch-cost=num

  .. index:: mbranch-cost=num

  Assume :samp:`{num}` to be the cost for a branch instruction.  Higher numbers
  make the compiler try to generate more branch-free code if possible.  
  If not specified the value is selected depending on the processor type that
  is being compiled for.

.. option:: -mzdcbranch, -mno-zdcbranch

  Assume (do not assume) that zero displacement conditional branch instructions
  ``bt`` and ``bf`` are fast.  If :option:`-mzdcbranch` is specified, the
  compiler prefers zero displacement branch code sequences.  This is
  enabled by default when generating code for SH4 and SH4A.  It can be explicitly
  disabled by specifying :option:`-mno-zdcbranch`.

.. option:: -mcbranch-force-delay-slot

  Force the usage of delay slots for conditional branches, which stuffs the delay
  slot with a ``nop`` if a suitable instruction cannot be found.  By default
  this option is disabled.  It can be enabled to work around hardware bugs as
  found in the original SH7055.

.. option:: -mfused-madd, -mno-fused-madd

  Generate code that uses (does not use) the floating-point multiply and
  accumulate instructions.  These instructions are generated by default
  if hardware floating point is used.  The machine-dependent
  :option:`-mfused-madd` option is now mapped to the machine-independent
  :option:`-ffp-contract=fast` option, and :option:`-mno-fused-madd` is
  mapped to :option:`-ffp-contract=off`.

.. option:: -mfsca, -mno-fsca

  Allow or disallow the compiler to emit the ``fsca`` instruction for sine
  and cosine approximations.  The option :option:`-mfsca` must be used in
  combination with :option:`-funsafe-math-optimizations`.  It is enabled by default
  when generating code for SH4A.  Using :option:`-mno-fsca` disables sine and cosine
  approximations even if :option:`-funsafe-math-optimizations` is in effect.

.. option:: -mfsrra, -mno-fsrra

  Allow or disallow the compiler to emit the ``fsrra`` instruction for
  reciprocal square root approximations.  The option :option:`-mfsrra` must be used
  in combination with :option:`-funsafe-math-optimizations` and
  :option:`-ffinite-math-only`.  It is enabled by default when generating code for
  SH4A.  Using :option:`-mno-fsrra` disables reciprocal square root approximations
  even if :option:`-funsafe-math-optimizations` and :option:`-ffinite-math-only` are
  in effect.

.. option:: -mpretend-cmove

  Prefer zero-displacement conditional branches for conditional move instruction
  patterns.  This can result in faster code on the SH4 processor.

.. option:: -mfdpic, -fdpic

  Generate code using the FDPIC ABI.

.. _solaris-2-options:

Solaris 2 Options
^^^^^^^^^^^^^^^^^

.. index:: Solaris 2 options

These :samp:`-m` options are supported on Solaris 2:

.. option:: -mclear-hwcap

  :option:`-mclear-hwcap` tells the compiler to remove the hardware
  capabilities generated by the Solaris assembler.  This is only necessary
  when object files use ISA extensions not supported by the current
  machine, but check at runtime whether or not to use them.

.. option:: -mimpure-text

  :option:`-mimpure-text` , used in addition to :option:`-shared` , tells
  the compiler to not pass :option:`-z text` to the linker when linking a
  shared object.  Using this option, you can link position-dependent
  code into a shared object.

  :option:`-mimpure-text` suppresses the 'relocations remain against
  allocatable but non-writable sections' linker error message.
  However, the necessary relocations trigger copy-on-write, and the
  shared object is not actually shared across processes.  Instead of
  using :option:`-mimpure-text` , you should compile all source code with
  :option:`-fpic` or :option:`-fPIC`.

These switches are supported in addition to the above on Solaris 2:

.. option:: -pthreads

  This is a synonym for :option:`-pthread`.

.. _sparc-options:

SPARC Options
^^^^^^^^^^^^^

.. index:: SPARC options

These :samp:`-m` options are supported on the SPARC:

.. option:: -mno-app-regs, -mapp-regs

  Specify :option:`-mapp-regs` to generate output using the global registers
  2 through 4, which the SPARC SVR4 ABI reserves for applications.  Like the
  global register 1, each global register 2 through 4 is then treated as an
  allocable register that is clobbered by function calls.  This is the default.

  To be fully SVR4 ABI-compliant at the cost of some performance loss,
  specify :option:`-mno-app-regs`.  You should compile libraries and system
  software with this option.

.. option:: -mflat, -mno-flat

  With :option:`-mflat` , the compiler does not generate save/restore instructions
  and uses a 'flat' or single register window model.  This model is compatible
  with the regular register window model.  The local registers and the input
  registers (0-5) are still treated as 'call-saved' registers and are
  saved on the stack as needed.

  With :option:`-mno-flat` (the default), the compiler generates save/restore
  instructions (except for leaf functions).  This is the normal operating mode.

.. option:: -mfpu, -mhard-float

  Generate output containing floating-point instructions.  This is the
  default.

.. option:: -mno-fpu, -msoft-float

  Generate output containing library calls for floating point.
  Warning: the requisite libraries are not available for all SPARC
  targets.  Normally the facilities of the machine's usual C compiler are
  used, but this cannot be done directly in cross-compilation.  You must make
  your own arrangements to provide suitable library functions for
  cross-compilation.  The embedded targets :samp:`sparc-*-aout` and
  :samp:`sparclite-*-*` do provide software floating-point support.

  :option:`-msoft-float` changes the calling convention in the output file;
  therefore, it is only useful if you compile *all* of a program with
  this option.  In particular, you need to compile libgcc.a, the
  library that comes with GCC, with :option:`-msoft-float` in order for
  this to work.

.. option:: -mhard-quad-float

  Generate output containing quad-word (long double) floating-point
  instructions.

.. option:: -msoft-quad-float

  Generate output containing library calls for quad-word (long double)
  floating-point instructions.  The functions called are those specified
  in the SPARC ABI.  This is the default.

  As of this writing, there are no SPARC implementations that have hardware
  support for the quad-word floating-point instructions.  They all invoke
  a trap handler for one of these instructions, and then the trap handler
  emulates the effect of the instruction.  Because of the trap handler overhead,
  this is much slower than calling the ABI library routines.  Thus the
  :option:`-msoft-quad-float` option is the default.

.. option:: -mno-unaligned-doubles, -munaligned-doubles

  Assume that doubles have 8-byte alignment.  This is the default.

  With :option:`-munaligned-doubles` , GCC assumes that doubles have 8-byte
  alignment only if they are contained in another type, or if they have an
  absolute address.  Otherwise, it assumes they have 4-byte alignment.
  Specifying this option avoids some rare compatibility problems with code
  generated by other compilers.  It is not the default because it results
  in a performance loss, especially for floating-point code.

.. option:: -muser-mode, -mno-user-mode

  Do not generate code that can only run in supervisor mode.  This is relevant
  only for the ``casa`` instruction emitted for the LEON3 processor.  This
  is the default.

.. option:: -mfaster-structs, -mno-faster-structs

  With :option:`-mfaster-structs` , the compiler assumes that structures
  should have 8-byte alignment.  This enables the use of pairs of
  ``ldd`` and ``std`` instructions for copies in structure
  assignment, in place of twice as many ``ld`` and ``st`` pairs.
  However, the use of this changed alignment directly violates the SPARC
  ABI.  Thus, it's intended only for use on targets where the developer
  acknowledges that their resulting code is not directly in line with
  the rules of the ABI.

.. option:: -mstd-struct-return, -mno-std-struct-return

  With :option:`-mstd-struct-return` , the compiler generates checking code
  in functions returning structures or unions to detect size mismatches
  between the two sides of function calls, as per the 32-bit ABI.

  The default is :option:`-mno-std-struct-return`.  This option has no effect
  in 64-bit mode.

.. option:: -mlra, -mno-lra

  Enable Local Register Allocation.  This is the default for SPARC since GCC 7
  so :option:`-mno-lra` needs to be passed to get old Reload.

.. option:: -mcpu=cpu_type

  Set the instruction set, register set, and instruction scheduling parameters
  for machine type :samp:`{cpu_type}`.  Supported values for :samp:`{cpu_type}` are
  :samp:`v7`, :samp:`cypress`, :samp:`v8`, :samp:`supersparc`, :samp:`hypersparc`,
  :samp:`leon`, :samp:`leon3`, :samp:`leon3v7`, :samp:`sparclite`, :samp:`f930`,
  :samp:`f934`, :samp:`sparclite86x`, :samp:`sparclet`, :samp:`tsc701`, :samp:`v9`,
  :samp:`ultrasparc`, :samp:`ultrasparc3`, :samp:`niagara`, :samp:`niagara2`,
  :samp:`niagara3`, :samp:`niagara4`, :samp:`niagara7` and :samp:`m8`.

  Native Solaris and GNU/Linux toolchains also support the value :samp:`native`,
  which selects the best architecture option for the host processor.
  :option:`-mcpu=native` has no effect if GCC does not recognize
  the processor.

  Default instruction scheduling parameters are used for values that select
  an architecture and not an implementation.  These are :samp:`v7`, :samp:`v8`,
  :samp:`sparclite`, :samp:`sparclet`, :samp:`v9`.

  Here is a list of each supported architecture and their supported
  implementations.

  v7
    cypress, leon3v7

  v8
    supersparc, hypersparc, leon, leon3

  sparclite
    f930, f934, sparclite86x

  sparclet
    tsc701

  v9
    ultrasparc, ultrasparc3, niagara, niagara2, niagara3, niagara4,
    niagara7, m8

    By default (unless configured otherwise), GCC generates code for the V7
  variant of the SPARC architecture.  With :option:`-mcpu=cypress` , the compiler
  additionally optimizes it for the Cypress CY7C602 chip, as used in the
  SPARCStation/SPARCServer 3xx series.  This is also appropriate for the older
  SPARCStation 1, 2, IPX etc.

  With :option:`-mcpu=v8` , GCC generates code for the V8 variant of the SPARC
  architecture.  The only difference from V7 code is that the compiler emits
  the integer multiply and integer divide instructions which exist in SPARC-V8
  but not in SPARC-V7.  With :option:`-mcpu=supersparc` , the compiler additionally
  optimizes it for the SuperSPARC chip, as used in the SPARCStation 10, 1000 and
  2000 series.

  With :option:`-mcpu=sparclite` , GCC generates code for the SPARClite variant of
  the SPARC architecture.  This adds the integer multiply, integer divide step
  and scan (``ffs``) instructions which exist in SPARClite but not in SPARC-V7.
  With :option:`-mcpu=f930` , the compiler additionally optimizes it for the
  Fujitsu MB86930 chip, which is the original SPARClite, with no FPU.  With
  :option:`-mcpu=f934` , the compiler additionally optimizes it for the Fujitsu
  MB86934 chip, which is the more recent SPARClite with FPU.

  With :option:`-mcpu=sparclet` , GCC generates code for the SPARClet variant of
  the SPARC architecture.  This adds the integer multiply, multiply/accumulate,
  integer divide step and scan (``ffs``) instructions which exist in SPARClet
  but not in SPARC-V7.  With :option:`-mcpu=tsc701` , the compiler additionally
  optimizes it for the TEMIC SPARClet chip.

  With :option:`-mcpu=v9` , GCC generates code for the V9 variant of the SPARC
  architecture.  This adds 64-bit integer and floating-point move instructions,
  3 additional floating-point condition code registers and conditional move
  instructions.  With :option:`-mcpu=ultrasparc` , the compiler additionally
  optimizes it for the Sun UltraSPARC I/II/IIi chips.  With
  :option:`-mcpu=ultrasparc3` , the compiler additionally optimizes it for the
  Sun UltraSPARC III/III+/IIIi/IIIi+/IV/IV+ chips.  With
  :option:`-mcpu=niagara` , the compiler additionally optimizes it for
  Sun UltraSPARC T1 chips.  With :option:`-mcpu=niagara2` , the compiler
  additionally optimizes it for Sun UltraSPARC T2 chips. With
  :option:`-mcpu=niagara3` , the compiler additionally optimizes it for Sun
  UltraSPARC T3 chips.  With :option:`-mcpu=niagara4` , the compiler
  additionally optimizes it for Sun UltraSPARC T4 chips.  With
  :option:`-mcpu=niagara7` , the compiler additionally optimizes it for
  Oracle SPARC M7 chips.  With :option:`-mcpu=m8` , the compiler
  additionally optimizes it for Oracle M8 chips.

.. option:: -mtune=cpu_type

  Set the instruction scheduling parameters for machine type
  :samp:`{cpu_type}` , but do not set the instruction set or register set that the
  option :option:`-mcpu`:samp:`={cpu_type}` does.

  The same values for :option:`-mcpu`:samp:`={cpu_type}` can be used for
  :option:`-mtune`:samp:`={cpu_type}` , but the only useful values are those
  that select a particular CPU implementation.  Those are
  :samp:`cypress`, :samp:`supersparc`, :samp:`hypersparc`, :samp:`leon`,
  :samp:`leon3`, :samp:`leon3v7`, :samp:`f930`, :samp:`f934`,
  :samp:`sparclite86x`, :samp:`tsc701`, :samp:`ultrasparc`,
  :samp:`ultrasparc3`, :samp:`niagara`, :samp:`niagara2`, :samp:`niagara3`,
  :samp:`niagara4`, :samp:`niagara7` and :samp:`m8`.  With native Solaris
  and GNU/Linux toolchains, :samp:`native` can also be used.

.. option:: -mv8plus, -mno-v8plus

  With :option:`-mv8plus` , GCC generates code for the SPARC-V8+ ABI.  The
  difference from the V8 ABI is that the global and out registers are
  considered 64 bits wide.  This is enabled by default on Solaris in 32-bit
  mode for all SPARC-V9 processors.

.. option:: -mvis, -mno-vis

  With :option:`-mvis` , GCC generates code that takes advantage of the UltraSPARC
  Visual Instruction Set extensions.  The default is :option:`-mno-vis`.

.. option:: -mvis2, -mno-vis2

  With :option:`-mvis2` , GCC generates code that takes advantage of
  version 2.0 of the UltraSPARC Visual Instruction Set extensions.  The
  default is :option:`-mvis2` when targeting a cpu that supports such
  instructions, such as UltraSPARC-III and later.  Setting :option:`-mvis2`
  also sets :option:`-mvis`.

.. option:: -mvis3, -mno-vis3

  With :option:`-mvis3` , GCC generates code that takes advantage of
  version 3.0 of the UltraSPARC Visual Instruction Set extensions.  The
  default is :option:`-mvis3` when targeting a cpu that supports such
  instructions, such as niagara-3 and later.  Setting :option:`-mvis3`
  also sets :option:`-mvis2` and :option:`-mvis`.

.. option:: -mvis4, -mno-vis4

  With :option:`-mvis4` , GCC generates code that takes advantage of
  version 4.0 of the UltraSPARC Visual Instruction Set extensions.  The
  default is :option:`-mvis4` when targeting a cpu that supports such
  instructions, such as niagara-7 and later.  Setting :option:`-mvis4`
  also sets :option:`-mvis3` , :option:`-mvis2` and :option:`-mvis`.

.. option:: -mvis4b, -mno-vis4b

  With :option:`-mvis4b` , GCC generates code that takes advantage of
  version 4.0 of the UltraSPARC Visual Instruction Set extensions, plus
  the additional VIS instructions introduced in the Oracle SPARC
  Architecture 2017.  The default is :option:`-mvis4b` when targeting a
  cpu that supports such instructions, such as m8 and later.  Setting
  :option:`-mvis4b` also sets :option:`-mvis4` , :option:`-mvis3` ,
  :option:`-mvis2` and :option:`-mvis`.

.. option:: -mcbcond, -mno-cbcond

  With :option:`-mcbcond` , GCC generates code that takes advantage of the UltraSPARC
  Compare-and-Branch-on-Condition instructions.  The default is :option:`-mcbcond`
  when targeting a CPU that supports such instructions, such as Niagara-4 and
  later.

.. option:: -mfmaf, -mno-fmaf

  With :option:`-mfmaf` , GCC generates code that takes advantage of the UltraSPARC
  Fused Multiply-Add Floating-point instructions.  The default is :option:`-mfmaf`
  when targeting a CPU that supports such instructions, such as Niagara-3 and
  later.

.. option:: -mfsmuld, -mno-fsmuld

  With :option:`-mfsmuld` , GCC generates code that takes advantage of the
  Floating-point Multiply Single to Double (FsMULd) instruction.  The default is
  :option:`-mfsmuld` when targeting a CPU supporting the architecture versions V8
  or V9 with FPU except :option:`-mcpu=leon`.

.. option:: -mpopc, -mno-popc

  With :option:`-mpopc` , GCC generates code that takes advantage of the UltraSPARC
  Population Count instruction.  The default is :option:`-mpopc`
  when targeting a CPU that supports such an instruction, such as Niagara-2 and
  later.

.. option:: -msubxc, -mno-subxc

  With :option:`-msubxc` , GCC generates code that takes advantage of the UltraSPARC
  Subtract-Extended-with-Carry instruction.  The default is :option:`-msubxc`
  when targeting a CPU that supports such an instruction, such as Niagara-7 and
  later.

.. option:: -mfix-at697f

  Enable the documented workaround for the single erratum of the Atmel AT697F
  processor (which corresponds to erratum #13 of the AT697E processor).

.. option:: -mfix-ut699

  Enable the documented workarounds for the floating-point errata and the data
  cache nullify errata of the UT699 processor.

.. option:: -mfix-ut700

  Enable the documented workaround for the back-to-back store errata of
  the UT699E/UT700 processor.

.. option:: -mfix-gr712rc

  Enable the documented workaround for the back-to-back store errata of
  the GR712RC processor.

These :samp:`-m` options are supported in addition to the above
on SPARC-V9 processors in 64-bit environments:

.. option:: -m32, -m64

  Generate code for a 32-bit or 64-bit environment.
  The 32-bit environment sets int, long and pointer to 32 bits.
  The 64-bit environment sets int to 32 bits and long and pointer
  to 64 bits.

.. option:: -mcmodel=which

  Set the code model to one of

  :samp:`medlow`
    The Medium/Low code model: 64-bit addresses, programs
    must be linked in the low 32 bits of memory.  Programs can be statically
    or dynamically linked.

  :samp:`medmid`
    The Medium/Middle code model: 64-bit addresses, programs
    must be linked in the low 44 bits of memory, the text and data segments must
    be less than 2GB in size and the data segment must be located within 2GB of
    the text segment.

  :samp:`medany`
    The Medium/Anywhere code model: 64-bit addresses, programs
    may be linked anywhere in memory, the text and data segments must be less
    than 2GB in size and the data segment must be located within 2GB of the
    text segment.

  :samp:`embmedany`
    The Medium/Anywhere code model for embedded systems:
    64-bit addresses, the text and data segments must be less than 2GB in
    size, both starting anywhere in memory (determined at link time).  The
    global register %g4 points to the base of the data segment.  Programs
    are statically linked and PIC is not supported.

.. option:: -mmemory-model=mem-model

  Set the memory model in force on the processor to one of

  :samp:`default`
    The default memory model for the processor and operating system.

  :samp:`rmo`
    Relaxed Memory Order

  :samp:`pso`
    Partial Store Order

  :samp:`tso`
    Total Store Order

  :samp:`sc`
    Sequential Consistency

    These memory models are formally defined in Appendix D of the SPARC-V9
  architecture manual, as set in the processor's ``PSTATE.MM`` field.

.. option:: -mstack-bias, -mno-stack-bias

  With :option:`-mstack-bias` , GCC assumes that the stack pointer, and
  frame pointer if present, are offset by -2047 which must be added back
  when making stack frame references.  This is the default in 64-bit mode.
  Otherwise, assume no such offset is present.

.. _system-v-options:

Options for System V
^^^^^^^^^^^^^^^^^^^^

These additional options are available on System V Release 4 for
compatibility with other compilers on those systems:

.. option:: -G

  Create a shared object.
  It is recommended that :option:`-symbolic` or :option:`-shared` be used instead.

.. option:: -Qy

  Identify the versions of each tool used by the compiler, in a
  ``.ident`` assembler directive in the output.

.. option:: -Qn

  Refrain from adding ``.ident`` directives to the output file (this is
  the default).

.. option:: -YP,dirs, -YP

  Search the directories :samp:`{dirs}` , and no others, for libraries
  specified with :option:`-l`.

.. option:: -Ym,dir, -Ym

  Look in the directory :samp:`{dir}` to find the M4 preprocessor.
  The assembler uses this option.

  .. This is supposed to go with a -Yd for predefined M4 macro files, but

  .. the generic assembler that comes with Solaris takes just -Ym.

.. _tile-gx-options:

TILE-Gx Options
^^^^^^^^^^^^^^^

.. index:: TILE-Gx options

These :samp:`-m` options are supported on the TILE-Gx:

.. option:: -mcmodel=small

  Generate code for the small model.  The distance for direct calls is
  limited to 500M in either direction.  PC-relative addresses are 32
  bits.  Absolute addresses support the full address range.

.. option:: -mcmodel=large

  Generate code for the large model.  There is no limitation on call
  distance, pc-relative addresses, or absolute addresses.

.. option:: -mcpu=name

  Selects the type of CPU to be targeted.  Currently the only supported
  type is :samp:`tilegx`.

.. option:: -m32, -m64

  Generate code for a 32-bit or 64-bit environment.  The 32-bit
  environment sets int, long, and pointer to 32 bits.  The 64-bit
  environment sets int to 32 bits and long and pointer to 64 bits.

.. option:: -mbig-endian, -mlittle-endian

  Generate code in big/little endian mode, respectively.

.. _tilepro-options:

TILEPro Options
^^^^^^^^^^^^^^^

.. index:: TILEPro options

These :samp:`-m` options are supported on the TILEPro:

.. option:: -mcpu=name

  Selects the type of CPU to be targeted.  Currently the only supported
  type is :samp:`tilepro`.

.. option:: -m32

  Generate code for a 32-bit environment, which sets int, long, and
  pointer to 32 bits.  This is the only supported behavior so the flag
  is essentially ignored.

.. _v850-options:

V850 Options
^^^^^^^^^^^^

.. index:: V850 Options

These :samp:`-m` options are defined for V850 implementations:

.. option:: -mlong-calls, -mno-long-calls

  Treat all calls as being far away (near).  If calls are assumed to be
  far away, the compiler always loads the function's address into a
  register, and calls indirect through the pointer.

.. option:: -mno-ep, -mep

  Do not optimize (do optimize) basic blocks that use the same index
  pointer 4 or more times to copy pointer into the ``ep`` register, and
  use the shorter ``sld`` and ``sst`` instructions.  The :option:`-mep`
  option is on by default if you optimize.

.. option:: -mno-prolog-function, -mprolog-function

  Do not use (do use) external functions to save and restore registers
  at the prologue and epilogue of a function.  The external functions
  are slower, but use less code space if more than one function saves
  the same number of registers.  The :option:`-mprolog-function` option
  is on by default if you optimize.

.. option:: -mspace

  Try to make the code as small as possible.  At present, this just turns
  on the :option:`-mep` and :option:`-mprolog-function` options.

.. option:: -mtda=n

  Put static or global variables whose size is :samp:`{n}` bytes or less into
  the tiny data area that register ``ep`` points to.  The tiny data
  area can hold up to 256 bytes in total (128 bytes for byte references).

.. option:: -msda=n

  Put static or global variables whose size is :samp:`{n}` bytes or less into
  the small data area that register ``gp`` points to.  The small data
  area can hold up to 64 kilobytes.

.. option:: -mzda=n

  Put static or global variables whose size is :samp:`{n}` bytes or less into
  the first 32 kilobytes of memory.

.. option:: -mv850

  Specify that the target processor is the V850.

.. option:: -mv850e3v5

  Specify that the target processor is the V850E3V5.  The preprocessor
  constant ``__v850e3v5__`` is defined if this option is used.

.. option:: -mv850e2v4

  Specify that the target processor is the V850E3V5.  This is an alias for
  the :option:`-mv850e3v5` option.

.. option:: -mv850e2v3

  Specify that the target processor is the V850E2V3.  The preprocessor
  constant ``__v850e2v3__`` is defined if this option is used.

.. option:: -mv850e2

  Specify that the target processor is the V850E2.  The preprocessor
  constant ``__v850e2__`` is defined if this option is used.

.. option:: -mv850e1

  Specify that the target processor is the V850E1.  The preprocessor
  constants ``__v850e1__`` and ``__v850e__`` are defined if
  this option is used.

.. option:: -mv850es

  Specify that the target processor is the V850ES.  This is an alias for
  the :option:`-mv850e1` option.

.. option:: -mv850e

  Specify that the target processor is the V850E.  The preprocessor
  constant ``__v850e__`` is defined if this option is used.

  If neither :option:`-mv850` nor :option:`-mv850e` nor :option:`-mv850e1`
  nor :option:`-mv850e2` nor :option:`-mv850e2v3` nor :option:`-mv850e3v5`
  are defined then a default target processor is chosen and the
  relevant :samp:`__v850*__` preprocessor constant is defined.

  The preprocessor constants ``__v850`` and ``__v851__`` are always
  defined, regardless of which processor variant is the target.

.. option:: -mdisable-callt, -mno-disable-callt

  This option suppresses generation of the ``CALLT`` instruction for the
  v850e, v850e1, v850e2, v850e2v3 and v850e3v5 flavors of the v850
  architecture.

  This option is enabled by default when the RH850 ABI is
  in use (see :option:`-mrh850-abi` ), and disabled by default when the
  GCC ABI is in use.  If ``CALLT`` instructions are being generated
  then the C preprocessor symbol ``__V850_CALLT__`` is defined.

.. option:: -mrelax, -mno-relax

  Pass on (or do not pass on) the :option:`-mrelax` command-line option
  to the assembler.

.. option:: -mlong-jumps, -mno-long-jumps

  Disable (or re-enable) the generation of PC-relative jump instructions.

.. option:: -msoft-float, -mhard-float

  Disable (or re-enable) the generation of hardware floating point
  instructions.  This option is only significant when the target
  architecture is :samp:`V850E2V3` or higher.  If hardware floating point
  instructions are being generated then the C preprocessor symbol
  ``__FPU_OK__`` is defined, otherwise the symbol
  ``__NO_FPU__`` is defined.

.. option:: -mloop

  Enables the use of the e3v5 LOOP instruction.  The use of this
  instruction is not enabled by default when the e3v5 architecture is
  selected because its use is still experimental.

.. option:: -mrh850-abi, -mghs

  Enables support for the RH850 version of the V850 ABI.  This is the
  default.  With this version of the ABI the following rules apply:

  ** Integer sized structures and unions are returned via a memory pointer
    rather than a register.

  * Large structures and unions (more than 8 bytes in size) are passed by
    value.

  * Functions are aligned to 16-bit boundaries.

  * The :option:`-m8byte-align` command-line option is supported.

  * The :option:`-mdisable-callt` command-line option is enabled by
    default.  The :option:`-mno-disable-callt` command-line option is not
    supported.

  When this version of the ABI is enabled the C preprocessor symbol
  ``__V850_RH850_ABI__`` is defined.

.. option:: -mgcc-abi

  Enables support for the old GCC version of the V850 ABI.  With this
  version of the ABI the following rules apply:

  ** Integer sized structures and unions are returned in register ``r10``.

  * Large structures and unions (more than 8 bytes in size) are passed by
    reference.

  * Functions are aligned to 32-bit boundaries, unless optimizing for
    size.

  * The :option:`-m8byte-align` command-line option is not supported.

  * The :option:`-mdisable-callt` command-line option is supported but not
    enabled by default.

  When this version of the ABI is enabled the C preprocessor symbol
  ``__V850_GCC_ABI__`` is defined.

.. option:: -m8byte-align, -mno-8byte-align

  Enables support for ``double`` and ``long long`` types to be
  aligned on 8-byte boundaries.  The default is to restrict the
  alignment of all objects to at most 4-bytes.  When
  :option:`-m8byte-align` is in effect the C preprocessor symbol
  ``__V850_8BYTE_ALIGN__`` is defined.

.. option:: -mbig-switch

  Generate code suitable for big switch tables.  Use this option only if
  the assembler/linker complain about out of range branches within a switch
  table.

.. option:: -mapp-regs

  This option causes r2 and r5 to be used in the code generated by
  the compiler.  This setting is the default.

.. option:: -mno-app-regs

  This option causes r2 and r5 to be treated as fixed registers.

.. _vax-options:

VAX Options
^^^^^^^^^^^

.. index:: VAX options

These :samp:`-m` options are defined for the VAX:

.. option:: -munix

  Do not output certain jump instructions (``aobleq`` and so on)
  that the Unix assembler for the VAX cannot handle across long
  ranges.

.. option:: -mgnu

  Do output those jump instructions, on the assumption that the
  GNU assembler is being used.

.. option:: -mg

  Output code for G-format floating-point numbers instead of D-format.

.. _visium-options:

Visium Options
^^^^^^^^^^^^^^

.. index:: Visium options

.. option:: -mdebug

  A program which performs file I/O and is destined to run on an MCM target
  should be linked with this option.  It causes the libraries libc.a and
  libdebug.a to be linked.  The program should be run on the target under
  the control of the GDB remote debugging stub.

.. option:: -msim

  A program which performs file I/O and is destined to run on the simulator
  should be linked with option.  This causes libraries libc.a and libsim.a to
  be linked.

.. option:: -mfpu, -mhard-float

  Generate code containing floating-point instructions.  This is the
  default.

.. option:: -mno-fpu, -msoft-float

  Generate code containing library calls for floating-point.

  :option:`-msoft-float` changes the calling convention in the output file;
  therefore, it is only useful if you compile *all* of a program with
  this option.  In particular, you need to compile libgcc.a, the
  library that comes with GCC, with :option:`-msoft-float` in order for
  this to work.

.. option:: -mcpu=cpu_type

  Set the instruction set, register set, and instruction scheduling parameters
  for machine type :samp:`{cpu_type}`.  Supported values for :samp:`{cpu_type}` are
  :samp:`mcm`, :samp:`gr5` and :samp:`gr6`.

  :samp:`mcm` is a synonym of :samp:`gr5` present for backward compatibility.

  By default (unless configured otherwise), GCC generates code for the GR5
  variant of the Visium architecture.

  With :option:`-mcpu=gr6` , GCC generates code for the GR6 variant of the Visium
  architecture.  The only difference from GR5 code is that the compiler will
  generate block move instructions.

.. option:: -mtune=cpu_type

  Set the instruction scheduling parameters for machine type :samp:`{cpu_type}` ,
  but do not set the instruction set or register set that the option
  :option:`-mcpu`:samp:`={cpu_type}` would.

.. option:: -msv-mode

  Generate code for the supervisor mode, where there are no restrictions on
  the access to general registers.  This is the default.

.. option:: -muser-mode

  Generate code for the user mode, where the access to some general registers
  is forbidden: on the GR5, registers r24 to r31 cannot be accessed in this
  mode; on the GR6, only registers r29 to r31 are affected.

.. _vms-options:

VMS Options
^^^^^^^^^^^

These :samp:`-m` options are defined for the VMS implementations:

.. option:: -mvms-return-codes

  Return VMS condition codes from ``main``. The default is to return POSIX-style
  condition (e.g. error) codes.

.. option:: -mdebug-main=prefix

  .. index:: mdebug-main=prefix

  Flag the first routine whose name starts with :samp:`{prefix}` as the main
  routine for the debugger.

.. option:: -mmalloc64

  Default to 64-bit memory allocation routines.

.. option:: -mpointer-size=size

  .. index:: mpointer-size=size

  Set the default size of pointers. Possible options for :samp:`{size}` are
  :samp:`32` or :samp:`short` for 32 bit pointers, :samp:`64` or :samp:`long`
  for 64 bit pointers, and :samp:`no` for supporting only 32 bit pointers.
  The later option disables ``pragma pointer_size``.

.. _vxworks-options:

VxWorks Options
^^^^^^^^^^^^^^^

.. index:: VxWorks Options

The options in this section are defined for all VxWorks targets.
Options specific to the target hardware are listed with the other
options for that target.

.. option:: -mrtp

  GCC can generate code for both VxWorks kernels and real time processes
  (RTPs).  This option switches from the former to the latter.  It also
  defines the preprocessor macro ``__RTP__``.

.. option:: -non-static

  Link an RTP executable against shared libraries rather than static
  libraries.  The options :option:`-static` and :option:`-shared` can
  also be used for RTPs (see :ref:`link-options`); :option:`-static`
  is the default.

.. option:: -Bstatic, -Bdynamic

  These options are passed down to the linker.  They are defined for
  compatibility with Diab.

.. option:: -Xbind-lazy

  Enable lazy binding of function calls.  This option is equivalent to
  :option:`-Wl,-z,now` and is defined for compatibility with Diab.

.. option:: -Xbind-now

  Disable lazy binding of function calls.  This option is the default and
  is defined for compatibility with Diab.

.. _x86-options:

x86 Options
^^^^^^^^^^^

.. index:: x86 Options

These :samp:`-m` options are defined for the x86 family of computers.

.. option:: -march=cpu-type

  Generate instructions for the machine type :samp:`{cpu-type}`.  In contrast to
  :option:`-mtune`:samp:`={cpu-type}` , which merely tunes the generated code 
  for the specified :samp:`{cpu-type}` , :option:`-march`:samp:`={cpu-type}` allows GCC
  to generate code that may not run at all on processors other than the one
  indicated.  Specifying :option:`-march`:samp:`={cpu-type}` implies 
  :option:`-mtune`:samp:`={cpu-type}` , except where noted otherwise.

  The choices for :samp:`{cpu-type}` are:

  :samp:`native`
    This selects the CPU to generate code for at compilation time by determining
    the processor type of the compiling machine.  Using :option:`-march=native`
    enables all instruction subsets supported by the local machine (hence
    the result might not run on different machines).  Using :option:`-mtune=native`
    produces code optimized for the local machine under the constraints
    of the selected instruction set.  

  :samp:`x86-64`
    A generic CPU with 64-bit extensions.

  :samp:`x86-64-v2` :samp:`x86-64-v3` :samp:`x86-64-v4`
    These choices for :samp:`{cpu-type}` select the corresponding
    micro-architecture level from the x86-64 psABI.  On ABIs other than
    the x86-64 psABI they select the same CPU features as the x86-64 psABI
    documents for the particular micro-architecture level.

    Since these :samp:`{cpu-type}` values do not have a corresponding
    :option:`-mtune` setting, using :option:`-march` with these values enables
    generic tuning.  Specific tuning can be enabled using the
    :option:`-mtune`:samp:`={other-cpu-type}` option with an appropriate
    :samp:`{other-cpu-type}` value.

  :samp:`i386`
    Original Intel i386 CPU.

  :samp:`i486`
    Intel i486 CPU.  (No scheduling is implemented for this chip.)

  :samp:`i586` :samp:`pentium`
    Intel Pentium CPU with no MMX support.

  :samp:`lakemont`
    Intel Lakemont MCU, based on Intel Pentium CPU.

  :samp:`pentium-mmx`
    Intel Pentium MMX CPU, based on Pentium core with MMX instruction set support.

  :samp:`pentiumpro`
    Intel Pentium Pro CPU.

  :samp:`i686`
    When used with :option:`-march` , the Pentium Pro
    instruction set is used, so the code runs on all i686 family chips.
    When used with :option:`-mtune` , it has the same meaning as :samp:`generic`.

  :samp:`pentium2`
    Intel Pentium II CPU, based on Pentium Pro core with MMX instruction set
    support.

  :samp:`pentium3` :samp:`pentium3m`
    Intel Pentium III CPU, based on Pentium Pro core with MMX and SSE instruction
    set support.

  :samp:`pentium-m`
    Intel Pentium M; low-power version of Intel Pentium III CPU
    with MMX, SSE and SSE2 instruction set support.  Used by Centrino notebooks.

  :samp:`pentium4` :samp:`pentium4m`
    Intel Pentium 4 CPU with MMX, SSE and SSE2 instruction set support.

  :samp:`prescott`
    Improved version of Intel Pentium 4 CPU with MMX, SSE, SSE2 and SSE3 instruction
    set support.

  :samp:`nocona`
    Improved version of Intel Pentium 4 CPU with 64-bit extensions, MMX, SSE,
    SSE2 and SSE3 instruction set support.

  :samp:`core2`
    Intel Core 2 CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3 and SSSE3
    instruction set support.

  :samp:`nehalem`
    Intel Nehalem CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2 and POPCNT instruction set support.

  :samp:`westmere`
    Intel Westmere CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AES and PCLMUL instruction set support.

  :samp:`sandybridge`
    Intel Sandy Bridge CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AVX, AES and PCLMUL instruction set support.

  :samp:`ivybridge`
    Intel Ivy Bridge CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AVX, AES, PCLMUL, FSGSBASE, RDRND and F16C
    instruction set support.

  :samp:`haswell`
    Intel Haswell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA,
    BMI, BMI2 and F16C instruction set support.

  :samp:`broadwell`
    Intel Broadwell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2,
    F16C, RDSEED ADCX and PREFETCHW instruction set support.

  :samp:`skylake`
    Intel Skylake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA,
    BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC and XSAVES
    instruction set support.

  :samp:`bonnell`
    Intel Bonnell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3 and SSSE3
    instruction set support.

  :samp:`silvermont`
    Intel Silvermont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AES, PREFETCHW, PCLMUL and RDRND instruction set support.

  :samp:`goldmont`
    Intel Goldmont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AES, PREFETCHW, PCLMUL, RDRND, XSAVE, XSAVEC, XSAVES,
    XSAVEOPT and FSGSBASE instruction set support.

  :samp:`goldmont-plus`
    Intel Goldmont Plus CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, AES, PREFETCHW, PCLMUL, RDRND, XSAVE, XSAVEC,
    XSAVES, XSAVEOPT, FSGSBASE, PTWRITE, RDPID, SGX and UMIP instruction set support.

  :samp:`tremont`
    Intel Tremont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AES, PREFETCHW, PCLMUL, RDRND, XSAVE, XSAVEC, XSAVES,
    XSAVEOPT, FSGSBASE, PTWRITE, RDPID, SGX, UMIP, GFNI-SSE, CLWB, MOVDIRI,
    MOVDIR64B, CLDEMOTE and WAITPKG instruction set support.

  :samp:`knl`
    Intel Knight's Landing CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA,
    BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, PREFETCHWT1, AVX512F, AVX512PF,
    AVX512ER and AVX512CD instruction set support.

  :samp:`knm`
    Intel Knights Mill CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA,
    BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, PREFETCHWT1, AVX512F, AVX512PF,
    AVX512ER, AVX512CD, AVX5124VNNIW, AVX5124FMAPS and AVX512VPOPCNTDQ instruction
    set support.

  :samp:`skylake-avx512`
    Intel Skylake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA,
    BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, AVX512F,
    CLWB, AVX512VL, AVX512BW, AVX512DQ and AVX512CD instruction set support.

  :samp:`cannonlake`
    Intel Cannonlake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2,
    SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE,
    RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC,
    XSAVES, AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VBMI,
    AVX512IFMA, SHA and UMIP instruction set support.

  :samp:`icelake-client`
    Intel Icelake Client CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2,
    SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE,
    RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC,
    XSAVES, AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VBMI,
    AVX512IFMA, SHA, CLWB, UMIP, RDPID, GFNI, AVX512VBMI2, AVX512VPOPCNTDQ,
    AVX512BITALG, AVX512VNNI, VPCLMULQDQ, VAES instruction set support.

  :samp:`icelake-server`
    Intel Icelake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2,
    SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE,
    RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC,
    XSAVES, AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VBMI,
    AVX512IFMA, SHA, CLWB, UMIP, RDPID, GFNI, AVX512VBMI2, AVX512VPOPCNTDQ,
    AVX512BITALG, AVX512VNNI, VPCLMULQDQ, VAES, PCONFIG and WBNOINVD instruction
    set support.

  :samp:`cascadelake`
    Intel Cascadelake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI,
    BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, AVX512F, CLWB,
    AVX512VL, AVX512BW, AVX512DQ, AVX512CD and AVX512VNNI instruction set support.

  :samp:`cooperlake`
    Intel cooperlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI,
    BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, AVX512F, CLWB,
    AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VNNI and AVX512BF16 instruction
    set support.

  :samp:`tigerlake`
    Intel Tigerlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI,
    BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, AVX512F,
    AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VBMI, AVX512IFMA, SHA, CLWB, UMIP,
    RDPID, GFNI, AVX512VBMI2, AVX512VPOPCNTDQ, AVX512BITALG, AVX512VNNI, VPCLMULQDQ,
    VAES, PCONFIG, WBNOINVD, MOVDIRI, MOVDIR64B, AVX512VP2INTERSECT and KEYLOCKER
    instruction set support.

  :samp:`sapphirerapids`
    Intel sapphirerapids CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3,
    SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND,
    FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES,
    AVX512F, CLWB, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VNNI, AVX512BF16,
    MOVDIRI, MOVDIR64B, AVX512VP2INTERSECT, ENQCMD, CLDEMOTE, PTWRITE, WAITPKG,
    SERIALIZE, TSXLDTRK, UINTR, AMX-BF16, AMX-TILE, AMX-INT8 and AVX-VNNI
    instruction set support.

  :samp:`alderlake`
    Intel Alderlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
    SSE4.1, SSE4.2, POPCNT, AES, PREFETCHW, PCLMUL, RDRND, XSAVE, XSAVEC, XSAVES,
    XSAVEOPT, FSGSBASE, PTWRITE, RDPID, SGX, UMIP, GFNI-SSE, CLWB, MOVDIRI,
    MOVDIR64B, CLDEMOTE, WAITPKG, ADCX, AVX, AVX2, BMI, BMI2, F16C, FMA, LZCNT,
    PCONFIG, PKU, VAES, VPCLMULQDQ, SERIALIZE, HRESET, KL, WIDEKL and AVX-VNNI
    instruction set support.

  :samp:`rocketlake`
    Intel Rocketlake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2,
    SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE,
    RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC,
    XSAVES, AVX512F, AVX512VL, AVX512BW, AVX512DQ, AVX512CD, AVX512VBMI,
    AVX512IFMA, SHA, CLWB, UMIP, RDPID, GFNI, AVX512VBMI2, AVX512VPOPCNTDQ,
    AVX512BITALG, AVX512VNNI, VPCLMULQDQ, VAES instruction set support.

  :samp:`k6`
    AMD K6 CPU with MMX instruction set support.

  :samp:`k6-2` :samp:`k6-3`
    Improved versions of AMD K6 CPU with MMX and 3DNow! instruction set support.

  :samp:`athlon` :samp:`athlon-tbird`
    AMD Athlon CPU with MMX, 3dNOW!, enhanced 3DNow! and SSE prefetch instructions
    support.

  :samp:`athlon-4` :samp:`athlon-xp` :samp:`athlon-mp`
    Improved AMD Athlon CPU with MMX, 3DNow!, enhanced 3DNow! and full SSE
    instruction set support.

  :samp:`k8` :samp:`opteron` :samp:`athlon64` :samp:`athlon-fx`
    Processors based on the AMD K8 core with x86-64 instruction set support,
    including the AMD Opteron, Athlon 64, and Athlon 64 FX processors.
    (This supersets MMX, SSE, SSE2, 3DNow!, enhanced 3DNow! and 64-bit
    instruction set extensions.)

  :samp:`k8-sse3` :samp:`opteron-sse3` :samp:`athlon64-sse3`
    Improved versions of AMD K8 cores with SSE3 instruction set support.

  :samp:`amdfam10` :samp:`barcelona`
    CPUs based on AMD Family 10h cores with x86-64 instruction set support.  (This
    supersets MMX, SSE, SSE2, SSE3, SSE4A, 3DNow!, enhanced 3DNow!, ABM and 64-bit
    instruction set extensions.)

  :samp:`bdver1`
    CPUs based on AMD Family 15h cores with x86-64 instruction set support.  (This
    supersets FMA4, AVX, XOP, LWP, AES, PCLMUL, CX16, MMX, SSE, SSE2, SSE3, SSE4A,
    SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set extensions.)

  :samp:`bdver2`
    AMD Family 15h core based CPUs with x86-64 instruction set support.  (This
    supersets BMI, TBM, F16C, FMA, FMA4, AVX, XOP, LWP, AES, PCLMUL, CX16, MMX,
    SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set 
    extensions.)

  :samp:`bdver3`
    AMD Family 15h core based CPUs with x86-64 instruction set support.  (This
    supersets BMI, TBM, F16C, FMA, FMA4, FSGSBASE, AVX, XOP, LWP, AES, 
    PCLMUL, CX16, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM and
    64-bit instruction set extensions.)

  :samp:`bdver4`
    AMD Family 15h core based CPUs with x86-64 instruction set support.  (This
    supersets BMI, BMI2, TBM, F16C, FMA, FMA4, FSGSBASE, AVX, AVX2, XOP, LWP, 
    AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1,
    SSE4.2, ABM and 64-bit instruction set extensions.)

  :samp:`znver1`
    AMD Family 17h core based CPUs with x86-64 instruction set support.  (This
    supersets BMI, BMI2, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED, MWAITX,
    SHA, CLZERO, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3,
    SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES, CLFLUSHOPT, POPCNT, and 64-bit
    instruction set extensions.)

  :samp:`znver2`
    AMD Family 17h core based CPUs with x86-64 instruction set support. (This
    supersets BMI, BMI2, CLWB, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED,
    MWAITX, SHA, CLZERO, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A,
    SSSE3, SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES, CLFLUSHOPT, POPCNT, RDPID,
    WBNOINVD, and 64-bit instruction set extensions.)

  :samp:`znver3`
    AMD Family 19h core based CPUs with x86-64 instruction set support. (This
    supersets BMI, BMI2, CLWB, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED,
    MWAITX, SHA, CLZERO, AES, PCLMUL, CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A,
    SSSE3, SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES, CLFLUSHOPT, POPCNT, RDPID,
    WBNOINVD, PKU, VPCLMULQDQ, VAES, and 64-bit instruction set extensions.)

  :samp:`btver1`
    CPUs based on AMD Family 14h cores with x86-64 instruction set support.  (This
    supersets MMX, SSE, SSE2, SSE3, SSSE3, SSE4A, CX16, ABM and 64-bit
    instruction set extensions.)

  :samp:`btver2`
    CPUs based on AMD Family 16h cores with x86-64 instruction set support. This
    includes MOVBE, F16C, BMI, AVX, PCLMUL, AES, SSE4.2, SSE4.1, CX16, ABM,
    SSE4A, SSSE3, SSE3, SSE2, SSE, MMX and 64-bit instruction set extensions.

  :samp:`winchip-c6`
    IDT WinChip C6 CPU, dealt in same way as i486 with additional MMX instruction
    set support.

  :samp:`winchip2`
    IDT WinChip 2 CPU, dealt in same way as i486 with additional MMX and 3DNow!
    instruction set support.

  :samp:`c3`
    VIA C3 CPU with MMX and 3DNow! instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`c3-2`
    VIA C3-2 (Nehemiah/C5XL) CPU with MMX and SSE instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`c7`
    VIA C7 (Esther) CPU with MMX, SSE, SSE2 and SSE3 instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`samuel-2`
    VIA Eden Samuel 2 CPU with MMX and 3DNow! instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nehemiah`
    VIA Eden Nehemiah CPU with MMX and SSE instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`esther`
    VIA Eden Esther CPU with MMX, SSE, SSE2 and SSE3 instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`eden-x2`
    VIA Eden X2 CPU with x86-64, MMX, SSE, SSE2 and SSE3 instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`eden-x4`
    VIA Eden X4 CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2,
    AVX and AVX2 instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano`
    Generic VIA Nano CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-1000`
    VIA Nano 1xxx CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-2000`
    VIA Nano 2xxx CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-3000`
    VIA Nano 3xxx CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-x2`
    VIA Nano Dual Core CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`nano-x4`
    VIA Nano Quad Core CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1
    instruction set support.
    (No scheduling is implemented for this chip.)

  :samp:`geode`
    AMD Geode embedded processor with MMX and 3DNow! instruction set support.

.. option:: -mtune=cpu-type

  Tune to :samp:`{cpu-type}` everything applicable about the generated code, except
  for the ABI and the set of available instructions.  
  While picking a specific :samp:`{cpu-type}` schedules things appropriately
  for that particular chip, the compiler does not generate any code that
  cannot run on the default machine type unless you use a
  :option:`-march`:samp:`={cpu-type}` option.
  For example, if GCC is configured for i686-pc-linux-gnu
  then :option:`-mtune=pentium4` generates code that is tuned for Pentium 4
  but still runs on i686 machines.

  The choices for :samp:`{cpu-type}` are the same as for :option:`-march`.
  In addition, :option:`-mtune` supports 2 extra choices for :samp:`{cpu-type}` :

  :samp:`generic`
    Produce code optimized for the most common IA32//AMD64//EM64T processors.
    If you know the CPU on which your code will run, then you should use
    the corresponding :option:`-mtune` or :option:`-march` option instead of
    :option:`-mtune=generic`.  But, if you do not know exactly what CPU users
    of your application will have, then you should use this option.

    As new processors are deployed in the marketplace, the behavior of this
    option will change.  Therefore, if you upgrade to a newer version of
    GCC, code generation controlled by this option will change to reflect
    the processors
    that are most common at the time that version of GCC is released.

    There is no :option:`-march=generic` option because :option:`-march`
    indicates the instruction set the compiler can use, and there is no
    generic instruction set applicable to all processors.  In contrast,
    :option:`-mtune` indicates the processor (or, in this case, collection of
    processors) for which the code is optimized.

  :samp:`intel`
    Produce code optimized for the most current Intel processors, which are
    Haswell and Silvermont for this version of GCC.  If you know the CPU
    on which your code will run, then you should use the corresponding
    :option:`-mtune` or :option:`-march` option instead of :option:`-mtune=intel`.
    But, if you want your application performs better on both Haswell and
    Silvermont, then you should use this option.

    As new Intel processors are deployed in the marketplace, the behavior of
    this option will change.  Therefore, if you upgrade to a newer version of
    GCC, code generation controlled by this option will change to reflect
    the most current Intel processors at the time that version of GCC is
    released.

    There is no :option:`-march=intel` option because :option:`-march` indicates
    the instruction set the compiler can use, and there is no common
    instruction set applicable to all processors.  In contrast,
    :option:`-mtune` indicates the processor (or, in this case, collection of
    processors) for which the code is optimized.

.. option:: -mcpu=cpu-type

  A deprecated synonym for :option:`-mtune`.

.. option:: -mfpmath=unit

  Generate floating-point arithmetic for selected unit :samp:`{unit}`.  The choices
  for :samp:`{unit}` are:

  :samp:`387`
    Use the standard 387 floating-point coprocessor present on the majority of chips and
    emulated otherwise.  Code compiled with this option runs almost everywhere.
    The temporary results are computed in 80-bit precision instead of the precision
    specified by the type, resulting in slightly different results compared to most
    of other chips.  See :option:`-ffloat-store` for more detailed description.

    This is the default choice for non-Darwin x86-32 targets.

  :samp:`sse`
    Use scalar floating-point instructions present in the SSE instruction set.
    This instruction set is supported by Pentium III and newer chips,
    and in the AMD line
    by Athlon-4, Athlon XP and Athlon MP chips.  The earlier version of the SSE
    instruction set supports only single-precision arithmetic, thus the double and
    extended-precision arithmetic are still done using 387.  A later version, present
    only in Pentium 4 and AMD x86-64 chips, supports double-precision
    arithmetic too.

    For the x86-32 compiler, you must use :option:`-march`:samp:`={cpu-type}` , :option:`-msse`
    or :option:`-msse2` switches to enable SSE extensions and make this option
    effective.  For the x86-64 compiler, these extensions are enabled by default.

    The resulting code should be considerably faster in the majority of cases and avoid
    the numerical instability problems of 387 code, but may break some existing
    code that expects temporaries to be 80 bits.

    This is the default choice for the x86-64 compiler, Darwin x86-32 targets,
    and the default choice for x86-32 targets with the SSE2 instruction set
    when :option:`-ffast-math` is enabled.

  :samp:`sse,387` :samp:`sse+387` :samp:`both`
    Attempt to utilize both instruction sets at once.  This effectively doubles the
    amount of available registers, and on chips with separate execution units for
    387 and SSE the execution resources too.  Use this option with care, as it is
    still experimental, because the GCC register allocator does not model separate
    functional units well, resulting in unstable performance.

.. option:: -masm=dialect

  .. index:: masm=dialect

  Output assembly instructions using selected :samp:`{dialect}`.  Also affects
  which dialect is used for basic ``asm`` (see :ref:`basic-asm`) and
  extended ``asm`` (see :ref:`extended-asm`). Supported choices (in dialect
  order) are :samp:`att` or :samp:`intel`. The default is :samp:`att`. Darwin does
  not support :samp:`intel`.

.. option:: -mieee-fp, -mno-ieee-fp

  Control whether or not the compiler uses IEEE floating-point
  comparisons.  These correctly handle the case where the result of a
  comparison is unordered.

.. option:: -m80387, -80387, -mhard-float

  Generate output containing 80387 instructions for floating point.

.. option:: -mno-80387, -no-80387, -msoft-float

  Generate output containing library calls for floating point.

  Warning: the requisite libraries are not part of GCC.
  Normally the facilities of the machine's usual C compiler are used, but
  this cannot be done directly in cross-compilation.  You must make your
  own arrangements to provide suitable library functions for
  cross-compilation.

  On machines where a function returns floating-point results in the 80387
  register stack, some floating-point opcodes may be emitted even if
  :option:`-msoft-float` is used.

.. option:: -mno-fp-ret-in-387, -mfp-ret-in-387

  Do not use the FPU registers for return values of functions.

  The usual calling convention has functions return values of types
  ``float`` and ``double`` in an FPU register, even if there
  is no FPU.  The idea is that the operating system should emulate
  an FPU.

  The option :option:`-mno-fp-ret-in-387` causes such values to be returned
  in ordinary CPU registers instead.

.. option:: -mno-fancy-math-387, -mfancy-math-387

  Some 387 emulators do not support the ``sin``, ``cos`` and
  ``sqrt`` instructions for the 387.  Specify this option to avoid
  generating those instructions.
  This option is overridden when :option:`-march`
  indicates that the target CPU always has an FPU and so the
  instruction does not need emulation.  These
  instructions are not generated unless you also use the
  :option:`-funsafe-math-optimizations` switch.

.. option:: -malign-double, -mno-align-double

  Control whether GCC aligns ``double``, ``long double``, and
  ``long long`` variables on a two-word boundary or a one-word
  boundary.  Aligning ``double`` variables on a two-word boundary
  produces code that runs somewhat faster on a Pentium at the
  expense of more memory.

  On x86-64, :option:`-malign-double` is enabled by default.

  Warning: if you use the :option:`-malign-double` switch,
  structures containing the above types are aligned differently than
  the published application binary interface specifications for the x86-32
  and are not binary compatible with structures in code compiled
  without that switch.

.. option:: -m96bit-long-double, -m128bit-long-double

  These switches control the size of ``long double`` type.  The x86-32
  application binary interface specifies the size to be 96 bits,
  so :option:`-m96bit-long-double` is the default in 32-bit mode.

  Modern architectures (Pentium and newer) prefer ``long double``
  to be aligned to an 8- or 16-byte boundary.  In arrays or structures
  conforming to the ABI, this is not possible.  So specifying
  :option:`-m128bit-long-double` aligns ``long double``
  to a 16-byte boundary by padding the ``long double`` with an additional
  32-bit zero.

  In the x86-64 compiler, :option:`-m128bit-long-double` is the default choice as
  its ABI specifies that ``long double`` is aligned on 16-byte boundary.

  Notice that neither of these options enable any extra precision over the x87
  standard of 80 bits for a ``long double``.

  Warning: if you override the default value for your target ABI, this
  changes the size of 
  structures and arrays containing ``long double`` variables,
  as well as modifying the function calling convention for functions taking
  ``long double``.  Hence they are not binary-compatible
  with code compiled without that switch.

.. option:: -mlong-double-64, -mlong-double-80, -mlong-double-128

  These switches control the size of ``long double`` type. A size
  of 64 bits makes the ``long double`` type equivalent to the ``double``
  type. This is the default for 32-bit Bionic C library.  A size
  of 128 bits makes the ``long double`` type equivalent to the
  ``__float128`` type. This is the default for 64-bit Bionic C library.

  Warning: if you override the default value for your target ABI, this
  changes the size of
  structures and arrays containing ``long double`` variables,
  as well as modifying the function calling convention for functions taking
  ``long double``.  Hence they are not binary-compatible
  with code compiled without that switch.

.. option:: -malign-data=type

  Control how GCC aligns variables.  Supported values for :samp:`{type}` are
  :samp:`compat` uses increased alignment value compatible uses GCC 4.8
  and earlier, :samp:`abi` uses alignment value as specified by the
  psABI, and :samp:`cacheline` uses increased alignment value to match
  the cache line size.  :samp:`compat` is the default.

.. option:: -mlarge-data-threshold=threshold

  When :option:`-mcmodel=medium` is specified, data objects larger than
  :samp:`{threshold}` are placed in the large data section.  This value must be the
  same across all objects linked into the binary, and defaults to 65535.

.. option:: -mrtd

  Use a different function-calling convention, in which functions that
  take a fixed number of arguments return with the ``ret num``
  instruction, which pops their arguments while returning.  This saves one
  instruction in the caller since there is no need to pop the arguments
  there.

  You can specify that an individual function is called with this calling
  sequence with the function attribute ``stdcall``.  You can also
  override the :option:`-mrtd` option by using the function attribute
  ``cdecl``.  See :ref:`function-attributes`.

  Warning: this calling convention is incompatible with the one
  normally used on Unix, so you cannot use it if you need to call
  libraries compiled with the Unix compiler.

  Also, you must provide function prototypes for all functions that
  take variable numbers of arguments (including ``printf``);
  otherwise incorrect code is generated for calls to those
  functions.

  In addition, seriously incorrect code results if you call a
  function with too many arguments.  (Normally, extra arguments are
  harmlessly ignored.)

.. option:: -mregparm=num

  Control how many registers are used to pass integer arguments.  By
  default, no registers are used to pass arguments, and at most 3
  registers can be used.  You can control this behavior for a specific
  function by using the function attribute ``regparm``.
  See :ref:`function-attributes`.

  Warning: if you use this switch, and
  :samp:`{num}` is nonzero, then you must build all modules with the same
  value, including any libraries.  This includes the system libraries and
  startup modules.

.. option:: -msseregparm

  Use SSE register passing conventions for float and double arguments
  and return values.  You can control this behavior for a specific
  function by using the function attribute ``sseregparm``.
  See :ref:`function-attributes`.

  Warning: if you use this switch then you must build all
  modules with the same value, including any libraries.  This includes
  the system libraries and startup modules.

.. option:: -mvect8-ret-in-mem

  Return 8-byte vectors in memory instead of MMX registers.  This is the
  default on VxWorks to match the ABI of the Sun Studio compilers until
  version 12.  *Only* use this option if you need to remain
  compatible with existing code produced by those previous compiler
  versions or older versions of GCC.

.. option:: -mpc32, -mpc64, -mpc80

  Set 80387 floating-point precision to 32, 64 or 80 bits.  When :option:`-mpc32`
  is specified, the significands of results of floating-point operations are
  rounded to 24 bits (single precision); :option:`-mpc64` rounds the
  significands of results of floating-point operations to 53 bits (double
  precision) and :option:`-mpc80` rounds the significands of results of
  floating-point operations to 64 bits (extended double precision), which is
  the default.  When this option is used, floating-point operations in higher
  precisions are not available to the programmer without setting the FPU
  control word explicitly.

  Setting the rounding of floating-point operations to less than the default
  80 bits can speed some programs by 2% or more.  Note that some mathematical
  libraries assume that extended-precision (80-bit) floating-point operations
  are enabled by default; routines in such libraries could suffer significant
  loss of accuracy, typically through so-called 'catastrophic cancellation',
  when this option is used to set the precision to less than extended precision.

.. option:: -mstackrealign

  Realign the stack at entry.  On the x86, the :option:`-mstackrealign`
  option generates an alternate prologue and epilogue that realigns the
  run-time stack if necessary.  This supports mixing legacy codes that keep
  4-byte stack alignment with modern codes that keep 16-byte stack alignment for
  SSE compatibility.  See also the attribute ``force_align_arg_pointer``,
  applicable to individual functions.

.. option:: -mpreferred-stack-boundary=num

  Attempt to keep the stack boundary aligned to a 2 raised to :samp:`{num}`
  byte boundary.  If :option:`-mpreferred-stack-boundary` is not specified,
  the default is 4 (16 bytes or 128 bits).

  Warning: When generating code for the x86-64 architecture with
  SSE extensions disabled, :option:`-mpreferred-stack-boundary=3` can be
  used to keep the stack boundary aligned to 8 byte boundary.  Since
  x86-64 ABI require 16 byte stack alignment, this is ABI incompatible and
  intended to be used in controlled environment where stack space is
  important limitation.  This option leads to wrong code when functions
  compiled with 16 byte stack alignment (such as functions from a standard
  library) are called with misaligned stack.  In this case, SSE
  instructions may lead to misaligned memory access traps.  In addition,
  variable arguments are handled incorrectly for 16 byte aligned
  objects (including x87 long double and __int128), leading to wrong
  results.  You must build all modules with
  :option:`-mpreferred-stack-boundary=3` , including any libraries.  This
  includes the system libraries and startup modules.

.. option:: -mincoming-stack-boundary=num

  Assume the incoming stack is aligned to a 2 raised to :samp:`{num}` byte
  boundary.  If :option:`-mincoming-stack-boundary` is not specified,
  the one specified by :option:`-mpreferred-stack-boundary` is used.

  On Pentium and Pentium Pro, ``double`` and ``long double`` values
  should be aligned to an 8-byte boundary (see :option:`-malign-double` ) or
  suffer significant run time performance penalties.  On Pentium III, the
  Streaming SIMD Extension (SSE) data type ``__m128`` may not work
  properly if it is not 16-byte aligned.

  To ensure proper alignment of this values on the stack, the stack boundary
  must be as aligned as that required by any value stored on the stack.
  Further, every function must be generated such that it keeps the stack
  aligned.  Thus calling a function compiled with a higher preferred
  stack boundary from a function compiled with a lower preferred stack
  boundary most likely misaligns the stack.  It is recommended that
  libraries that use callbacks always use the default setting.

  This extra alignment does consume extra stack space, and generally
  increases code size.  Code that is sensitive to stack space usage, such
  as embedded systems and operating system kernels, may want to reduce the
  preferred alignment to :option:`-mpreferred-stack-boundary=2`.

.. option:: -mmmx, -mwidekl

  These switches enable the use of instructions in the MMX, SSE,
  SSE2, SSE3, SSSE3, SSE4, SSE4A, SSE4.1, SSE4.2, AVX, AVX2, AVX512F, AVX512PF,
  AVX512ER, AVX512CD, AVX512VL, AVX512BW, AVX512DQ, AVX512IFMA, AVX512VBMI, SHA,
  AES, PCLMUL, CLFLUSHOPT, CLWB, FSGSBASE, PTWRITE, RDRND, F16C, FMA, PCONFIG,
  WBNOINVD, FMA4, PREFETCHW, RDPID, PREFETCHWT1, RDSEED, SGX, XOP, LWP,
  3DNow!, enhanced 3DNow!, POPCNT, ABM, ADX, BMI, BMI2, LZCNT, FXSR, XSAVE,
  XSAVEOPT, XSAVEC, XSAVES, RTM, HLE, TBM, MWAITX, CLZERO, PKU, AVX512VBMI2,
  GFNI, VAES, WAITPKG, VPCLMULQDQ, AVX512BITALG, MOVDIRI, MOVDIR64B, AVX512BF16,
  ENQCMD, AVX512VPOPCNTDQ, AVX5124FMAPS, AVX512VNNI, AVX5124VNNIW, SERIALIZE,
  UINTR, HRESET, AMXTILE, AMXINT8, AMXBF16, KL, WIDEKL, AVXVNNI or CLDEMOTE
  extended instruction sets. Each has a corresponding :option:`-mno-` option to
  disable use of these instructions.

  These extensions are also available as built-in functions: see
  x86 Built-in Functions, for details of the functions enabled and
  disabled by these switches.

  To generate SSE/SSE2 instructions automatically from floating-point
  code (as opposed to 387 instructions), see :option:`-mfpmath=sse`.

  GCC depresses SSEx instructions when :option:`-mavx` is used. Instead, it
  generates new AVX instructions or AVX equivalence for all SSEx instructions
  when needed.

  These options enable GCC to use these extended instructions in
  generated code, even without :option:`-mfpmath=sse`.  Applications that
  perform run-time CPU detection must compile separate files for each
  supported architecture, using the appropriate flags.  In particular,
  the file containing the CPU detection code should be compiled without
  these options.

.. option:: -mdump-tune-features

  This option instructs GCC to dump the names of the x86 performance 
  tuning features and default settings. The names can be used in 
  :option:`-mtune-ctrl`:samp:`={feature-list}`.

.. option:: -mtune-ctrl=feature-list

  .. index:: mtune-ctrl=feature-list

  This option is used to do fine grain control of x86 code generation features.
  :samp:`{feature-list}` is a comma separated list of :samp:`{feature}` names. See also
  :option:`-mdump-tune-features`. When specified, the :samp:`{feature}` is turned
  on if it is not preceded with :samp:`^`, otherwise, it is turned off. 
  :option:`-mtune-ctrl`:samp:`={feature-list}` is intended to be used by GCC
  developers. Using it may lead to code paths not covered by testing and can
  potentially result in compiler ICEs or runtime errors.

.. option:: -mno-default

  This option instructs GCC to turn off all tunable features. See also 
  :option:`-mtune-ctrl`:samp:`={feature-list}` and :option:`-mdump-tune-features`.

.. option:: -mcld

  This option instructs GCC to emit a ``cld`` instruction in the prologue
  of functions that use string instructions.  String instructions depend on
  the DF flag to select between autoincrement or autodecrement mode.  While the
  ABI specifies the DF flag to be cleared on function entry, some operating
  systems violate this specification by not clearing the DF flag in their
  exception dispatchers.  The exception handler can be invoked with the DF flag
  set, which leads to wrong direction mode when string instructions are used.
  This option can be enabled by default on 32-bit x86 targets by configuring
  GCC with the :option:`--enable-cld` configure option.  Generation of ``cld``
  instructions can be suppressed with the :option:`-mno-cld` compiler option
  in this case.

.. option:: -mvzeroupper

  This option instructs GCC to emit a ``vzeroupper`` instruction
  before a transfer of control flow out of the function to minimize
  the AVX to SSE transition penalty as well as remove unnecessary ``zeroupper``
  intrinsics.

.. option:: -mprefer-avx128

  This option instructs GCC to use 128-bit AVX instructions instead of
  256-bit AVX instructions in the auto-vectorizer.

.. option:: -mprefer-vector-width=opt

  This option instructs GCC to use :samp:`{opt}` -bit vector width in instructions
  instead of default on the selected platform.

  :samp:`none`
    No extra limitations applied to GCC other than defined by the selected platform.

  :samp:`128`
    Prefer 128-bit vector width for instructions.

  :samp:`256`
    Prefer 256-bit vector width for instructions.

  :samp:`512`
    Prefer 512-bit vector width for instructions.

.. option:: -mcx16

  This option enables GCC to generate ``CMPXCHG16B`` instructions in 64-bit
  code to implement compare-and-exchange operations on 16-byte aligned 128-bit
  objects.  This is useful for atomic updates of data structures exceeding one
  machine word in size.  The compiler uses this instruction to implement
  __sync Builtins.  However, for __atomic Builtins operating on
  128-bit integers, a library call is always used.

.. option:: -msahf

  This option enables generation of ``SAHF`` instructions in 64-bit code.
  Early Intel Pentium 4 CPUs with Intel 64 support,
  prior to the introduction of Pentium 4 G1 step in December 2005,
  lacked the ``LAHF`` and ``SAHF`` instructions
  which are supported by AMD64.
  These are load and store instructions, respectively, for certain status flags.
  In 64-bit mode, the ``SAHF`` instruction is used to optimize ``fmod``,
  ``drem``, and ``remainder`` built-in functions;
  see Other Builtins for details.

.. option:: -mmovbe

  This option enables use of the ``movbe`` instruction to implement
  ``__builtin_bswap32`` and ``__builtin_bswap64``.

.. option:: -mshstk

  The :option:`-mshstk` option enables shadow stack built-in functions
  from x86 Control-flow Enforcement Technology (CET).

.. option:: -mcrc32

  This option enables built-in functions ``__builtin_ia32_crc32qi``,
  ``__builtin_ia32_crc32hi``, ``__builtin_ia32_crc32si`` and
  ``__builtin_ia32_crc32di`` to generate the ``crc32`` machine instruction.

.. option:: -mrecip

  This option enables use of ``RCPSS`` and ``RSQRTSS`` instructions
  (and their vectorized variants ``RCPPS`` and ``RSQRTPS``)
  with an additional Newton-Raphson step
  to increase precision instead of ``DIVSS`` and ``SQRTSS``
  (and their vectorized
  variants) for single-precision floating-point arguments.  These instructions
  are generated only when :option:`-funsafe-math-optimizations` is enabled
  together with :option:`-ffinite-math-only` and :option:`-fno-trapping-math`.
  Note that while the throughput of the sequence is higher than the throughput
  of the non-reciprocal instruction, the precision of the sequence can be
  decreased by up to 2 ulp (i.e. the inverse of 1.0 equals 0.99999994).

  Note that GCC implements ``1.0f/sqrtf(x)`` in terms of ``RSQRTSS``
  (or ``RSQRTPS``) already with :option:`-ffast-math` (or the above option
  combination), and doesn't need :option:`-mrecip`.

  Also note that GCC emits the above sequence with additional Newton-Raphson step
  for vectorized single-float division and vectorized ``sqrtf(x)``
  already with :option:`-ffast-math` (or the above option combination), and
  doesn't need :option:`-mrecip`.

.. option:: -mrecip=opt

  This option controls which reciprocal estimate instructions
  may be used.  :samp:`{opt}` is a comma-separated list of options, which may
  be preceded by a :samp:`!` to invert the option:

  :samp:`all`
    Enable all estimate instructions.

  :samp:`default`
    Enable the default instructions, equivalent to :option:`-mrecip`.

  :samp:`none`
    Disable all estimate instructions, equivalent to :option:`-mno-recip`.

  :samp:`div`
    Enable the approximation for scalar division.

  :samp:`vec-div`
    Enable the approximation for vectorized division.

  :samp:`sqrt`
    Enable the approximation for scalar square root.

  :samp:`vec-sqrt`
    Enable the approximation for vectorized square root.

    So, for example, :option:`-mrecip=all,!sqrt` enables
  all of the reciprocal approximations, except for square root.

.. option:: -mveclibabi=type

  Specifies the ABI type to use for vectorizing intrinsics using an
  external library.  Supported values for :samp:`{type}` are :samp:`svml` 
  for the Intel short
  vector math library and :samp:`acml` for the AMD math core library.
  To use this option, both :option:`-ftree-vectorize` and
  :option:`-funsafe-math-optimizations` have to be enabled, and an SVML or ACML 
  ABI-compatible library must be specified at link time.

  GCC currently emits calls to ``vmldExp2``,
  ``vmldLn2``, ``vmldLog102``, ``vmldPow2``,
  ``vmldTanh2``, ``vmldTan2``, ``vmldAtan2``, ``vmldAtanh2``,
  ``vmldCbrt2``, ``vmldSinh2``, ``vmldSin2``, ``vmldAsinh2``,
  ``vmldAsin2``, ``vmldCosh2``, ``vmldCos2``, ``vmldAcosh2``,
  ``vmldAcos2``, ``vmlsExp4``, ``vmlsLn4``,
  ``vmlsLog104``, ``vmlsPow4``, ``vmlsTanh4``, ``vmlsTan4``,
  ``vmlsAtan4``, ``vmlsAtanh4``, ``vmlsCbrt4``, ``vmlsSinh4``,
  ``vmlsSin4``, ``vmlsAsinh4``, ``vmlsAsin4``, ``vmlsCosh4``,
  ``vmlsCos4``, ``vmlsAcosh4`` and ``vmlsAcos4`` for corresponding
  function type when :option:`-mveclibabi=svml` is used, and ``__vrd2_sin``,
  ``__vrd2_cos``, ``__vrd2_exp``, ``__vrd2_log``, ``__vrd2_log2``,
  ``__vrd2_log10``, ``__vrs4_sinf``, ``__vrs4_cosf``,
  ``__vrs4_expf``, ``__vrs4_logf``, ``__vrs4_log2f``,
  ``__vrs4_log10f`` and ``__vrs4_powf`` for the corresponding function type
  when :option:`-mveclibabi=acml` is used.  

.. option:: -mabi=name

  Generate code for the specified calling convention.  Permissible values
  are :samp:`sysv` for the ABI used on GNU/Linux and other systems, and
  :samp:`ms` for the Microsoft ABI.  The default is to use the Microsoft
  ABI when targeting Microsoft Windows and the SysV ABI on all other systems.
  You can control this behavior for specific functions by
  using the function attributes ``ms_abi`` and ``sysv_abi``.
  See :ref:`function-attributes`.

.. option:: -mforce-indirect-call

  Force all calls to functions to be indirect. This is useful
  when using Intel Processor Trace where it generates more precise timing
  information for function calls.

.. option:: -mmanual-endbr

  Insert ENDBR instruction at function entry only via the ``cf_check``
  function attribute. This is useful when used with the option
  :option:`-fcf-protection=branch` to control ENDBR insertion at the
  function entry.

.. option:: -mcall-ms2sysv-xlogues, -mno-call-ms2sysv-xlogues

  Due to differences in 64-bit ABIs, any Microsoft ABI function that calls a
  System V ABI function must consider RSI, RDI and XMM6-15 as clobbered.  By
  default, the code for saving and restoring these registers is emitted inline,
  resulting in fairly lengthy prologues and epilogues.  Using
  :option:`-mcall-ms2sysv-xlogues` emits prologues and epilogues that
  use stubs in the static portion of libgcc to perform these saves and restores,
  thus reducing function size at the cost of a few extra instructions.

.. option:: -mtls-dialect=type

  Generate code to access thread-local storage using the :samp:`gnu` or
  :samp:`gnu2` conventions.  :samp:`gnu` is the conservative default;
  :samp:`gnu2` is more efficient, but it may add compile- and run-time
  requirements that cannot be satisfied on all systems.

.. option:: -mpush-args, -mno-push-args

  Use PUSH operations to store outgoing parameters.  This method is shorter
  and usually equally fast as method using SUB/MOV operations and is enabled
  by default.  In some cases disabling it may improve performance because of
  improved scheduling and reduced dependencies.

.. option:: -maccumulate-outgoing-args

  If enabled, the maximum amount of space required for outgoing arguments is
  computed in the function prologue.  This is faster on most modern CPUs
  because of reduced dependencies, improved scheduling and reduced stack usage
  when the preferred stack boundary is not equal to 2.  The drawback is a notable
  increase in code size.  This switch implies :option:`-mno-push-args`.

.. option:: -mthreads

  Support thread-safe exception handling on MinGW.  Programs that rely
  on thread-safe exception handling must compile and link all code with the
  :option:`-mthreads` option.  When compiling, :option:`-mthreads` defines
  :option:`-D_MT` ; when linking, it links in a special thread helper library
  :option:`-lmingwthrd` which cleans up per-thread exception-handling data.

.. option:: -mms-bitfields, -mno-ms-bitfields

  Enable/disable bit-field layout compatible with the native Microsoft
  Windows compiler.

  If ``packed`` is used on a structure, or if bit-fields are used,
  it may be that the Microsoft ABI lays out the structure differently
  than the way GCC normally does.  Particularly when moving packed
  data between functions compiled with GCC and the native Microsoft compiler
  (either via function call or as data in a file), it may be necessary to access
  either format.

  This option is enabled by default for Microsoft Windows
  targets.  This behavior can also be controlled locally by use of variable
  or type attributes.  For more information, see x86 Variable Attributes
  and x86 Type Attributes.

  The Microsoft structure layout algorithm is fairly simple with the exception
  of the bit-field packing.  
  The padding and alignment of members of structures and whether a bit-field 
  can straddle a storage-unit boundary are determine by these rules:

  * Structure members are stored sequentially in the order in which they are
    declared: the first member has the lowest memory address and the last member
    the highest.

  * Every data object has an alignment requirement.  The alignment requirement
    for all data except structures, unions, and arrays is either the size of the
    object or the current packing size (specified with either the
    ``aligned`` attribute or the ``pack`` pragma),
    whichever is less.  For structures, unions, and arrays,
    the alignment requirement is the largest alignment requirement of its members.
    Every object is allocated an offset so that:

    .. code-block:: c++

      offset % alignment_requirement == 0

  * Adjacent bit-fields are packed into the same 1-, 2-, or 4-byte allocation
    unit if the integral types are the same size and if the next bit-field fits
    into the current allocation unit without crossing the boundary imposed by the
    common alignment requirements of the bit-fields.

  MSVC interprets zero-length bit-fields in the following ways:

  * If a zero-length bit-field is inserted between two bit-fields that
    are normally coalesced, the bit-fields are not coalesced.

    For example:

    .. code-block:: c++

      struct
       {
         unsigned long bf_1 : 12;
         unsigned long : 0;
         unsigned long bf_2 : 12;
       } t1;

    The size of ``t1`` is 8 bytes with the zero-length bit-field.  If the
    zero-length bit-field were removed, ``t1``'s size would be 4 bytes.

  * If a zero-length bit-field is inserted after a bit-field, ``foo``, and the
    alignment of the zero-length bit-field is greater than the member that follows it,
    ``bar``, ``bar`` is aligned as the type of the zero-length bit-field.

    For example:

    .. code-block:: c++

      struct
       {
         char foo : 4;
         short : 0;
         char bar;
       } t2;

      struct
       {
         char foo : 4;
         short : 0;
         double bar;
       } t3;

    For ``t2``, ``bar`` is placed at offset 2, rather than offset 1.
    Accordingly, the size of ``t2`` is 4.  For ``t3``, the zero-length
    bit-field does not affect the alignment of ``bar`` or, as a result, the size
    of the structure.

    Taking this into account, it is important to note the following:

    * If a zero-length bit-field follows a normal bit-field, the type of the
      zero-length bit-field may affect the alignment of the structure as whole. For
      example, ``t2`` has a size of 4 bytes, since the zero-length bit-field follows a
      normal bit-field, and is of type short.

    * Even if a zero-length bit-field is not followed by a normal bit-field, it may
      still affect the alignment of the structure:

      .. code-block:: c++

        struct
         {
           char foo : 6;
           long : 0;
         } t4;

      Here, ``t4`` takes up 4 bytes.

  * Zero-length bit-fields following non-bit-field members are ignored:

    .. code-block:: c++

      struct
       {
         char foo;
         long : 0;
         char bar;
       } t5;

    Here, ``t5`` takes up 2 bytes.

.. option:: -mno-align-stringops, -malign-stringops

  Do not align the destination of inlined string operations.  This switch reduces
  code size and improves performance in case the destination is already aligned,
  but GCC doesn't know about it.

.. option:: -minline-all-stringops

  By default GCC inlines string operations only when the destination is 
  known to be aligned to least a 4-byte boundary.  
  This enables more inlining and increases code
  size, but may improve performance of code that depends on fast
  ``memcpy`` and ``memset`` for short lengths.
  The option enables inline expansion of ``strlen`` for all
  pointer alignments.

.. option:: -minline-stringops-dynamically

  For string operations of unknown size, use run-time checks with
  inline code for small blocks and a library call for large blocks.

.. option:: -mstringop-strategy=alg

  .. index:: mstringop-strategy=alg

  Override the internal decision heuristic for the particular algorithm to use
  for inlining string operations.  The allowed values for :samp:`{alg}` are:

  :samp:`rep_byte` :samp:`rep_4byte` :samp:`rep_8byte`
    Expand using i386 ``rep`` prefix of the specified size.

  :samp:`byte_loop` :samp:`loop` :samp:`unrolled_loop`
    Expand into an inline loop.

  :samp:`libcall`
    Always use a library call.

.. option:: -mmemcpy-strategy=strategy

  .. index:: mmemcpy-strategy=strategy

  Override the internal decision heuristic to decide if ``__builtin_memcpy``
  should be inlined and what inline algorithm to use when the expected size
  of the copy operation is known. :samp:`{strategy}` 
  is a comma-separated list of :samp:`{alg}` : :samp:`{max_size}` : :samp:`{dest_align}` triplets. 
  :samp:`{alg}` is specified in :option:`-mstringop-strategy` , :samp:`{max_size}` specifies
  the max byte size with which inline algorithm :samp:`{alg}` is allowed.  For the last
  triplet, the :samp:`{max_size}` must be ``-1``. The :samp:`{max_size}` of the triplets
  in the list must be specified in increasing order.  The minimal byte size for 
  :samp:`{alg}` is ``0`` for the first triplet and ``max_size + 1`` of the 
  preceding range.

.. option:: -mmemset-strategy=strategy

  .. index:: mmemset-strategy=strategy

  The option is similar to :option:`-mmemcpy-strategy` = except that it is to control
  ``__builtin_memset`` expansion.

.. option:: -momit-leaf-frame-pointer

  Don't keep the frame pointer in a register for leaf functions.  This
  avoids the instructions to save, set up, and restore frame pointers and
  makes an extra register available in leaf functions.  The option
  :option:`-fomit-leaf-frame-pointer` removes the frame pointer for leaf functions,
  which might make debugging harder.

.. option:: -mtls-direct-seg-refs

  Controls whether TLS variables may be accessed with offsets from the
  TLS segment register (``%gs`` for 32-bit, ``%fs`` for 64-bit),
  or whether the thread base pointer must be added.  Whether or not this
  is valid depends on the operating system, and whether it maps the
  segment to cover the entire TLS area.

  For systems that use the GNU C Library, the default is on.

.. option:: -msse2avx

  Specify that the assembler should encode SSE instructions with VEX
  prefix.  The option :option:`-mavx` turns this on by default.

.. option:: -mfentry

  If profiling is active ( :option:`-pg` ), put the profiling
  counter call before the prologue.
  Note: On x86 architectures the attribute ``ms_hook_prologue``
  isn't possible at the moment for :option:`-mfentry` and :option:`-pg`.

.. option:: -mrecord-mcount

  If profiling is active ( :option:`-pg` ), generate a __mcount_loc section
  that contains pointers to each profiling call. This is useful for
  automatically patching and out calls.

.. option:: -mnop-mcount

  If profiling is active ( :option:`-pg` ), generate the calls to
  the profiling functions as NOPs. This is useful when they
  should be patched in later dynamically. This is likely only
  useful together with :option:`-mrecord-mcount`.

.. option:: -minstrument-return=type

  Instrument function exit in -pg -mfentry instrumented functions with
  call to specified function. This only instruments true returns ending
  with ret, but not sibling calls ending with jump. Valid types
  are :samp:`{none}` to not instrument, :samp:`{call}` to generate a call to __return__,
  or :samp:`{nop5}` to generate a 5 byte nop.

.. option:: -mrecord-return

  Generate a __return_loc section pointing to all return instrumentation code.

.. option:: -mfentry-name=name

  Set name of __fentry__ symbol called at function entry for -pg -mfentry functions.

.. option:: -mfentry-section=name

  Set name of section to record -mrecord-mcount calls (default __mcount_loc).

.. option:: -mskip-rax-setup

  When generating code for the x86-64 architecture with SSE extensions
  disabled, :option:`-mskip-rax-setup` can be used to skip setting up RAX
  register when there are no variable arguments passed in vector registers.

  Warning: Since RAX register is used to avoid unnecessarily
  saving vector registers on stack when passing variable arguments, the
  impacts of this option are callees may waste some stack space,
  misbehave or jump to a random location.  GCC 4.4 or newer don't have
  those issues, regardless the RAX register value.

.. option:: -m8bit-idiv

  On some processors, like Intel Atom, 8-bit unsigned integer divide is
  much faster than 32-bit/64-bit integer divide.  This option generates a
  run-time check.  If both dividend and divisor are within range of 0
  to 255, 8-bit unsigned integer divide is used instead of
  32-bit/64-bit integer divide.

.. option:: -mavx256-split-unaligned-load, -mavx256-split-unaligned-store

  Split 32-byte AVX unaligned load and store.

.. option:: -mstack-protector-guard=guard

  Generate stack protection code using canary at :samp:`{guard}`.  Supported
  locations are :samp:`global` for global canary or :samp:`tls` for per-thread
  canary in the TLS block (the default).  This option has effect only when
  :option:`-fstack-protector` or :option:`-fstack-protector-all` is specified.

  With the latter choice the options
  :option:`-mstack-protector-guard-reg`:samp:`={reg}` and
  :option:`-mstack-protector-guard-offset`:samp:`={offset}` furthermore specify
  which segment register (``%fs`` or ``%gs``) to use as base register
  for reading the canary, and from what offset from that base register.
  The default for those is as specified in the relevant ABI.

.. option:: -mgeneral-regs-only

  Generate code that uses only the general-purpose registers.  This
  prevents the compiler from using floating-point, vector, mask and bound
  registers.

.. option:: -mindirect-branch=choice

  Convert indirect call and jump with :samp:`{choice}`.  The default is
  :samp:`keep`, which keeps indirect call and jump unmodified.
  :samp:`thunk` converts indirect call and jump to call and return thunk.
  :samp:`thunk-inline` converts indirect call and jump to inlined call
  and return thunk.  :samp:`thunk-extern` converts indirect call and jump
  to external call and return thunk provided in a separate object file.
  You can control this behavior for a specific function by using the
  function attribute ``indirect_branch``.  See :ref:`function-attributes`.

  Note that :option:`-mcmodel=large` is incompatible with
  :option:`-mindirect-branch=thunk` and
  :option:`-mindirect-branch=thunk-extern` since the thunk function may
  not be reachable in the large code model.

  Note that :option:`-mindirect-branch=thunk-extern` is compatible with
  :option:`-fcf-protection=branch` since the external thunk can be made
  to enable control-flow check.

.. option:: -mfunction-return=choice

  Convert function return with :samp:`{choice}`.  The default is :samp:`keep`,
  which keeps function return unmodified.  :samp:`thunk` converts function
  return to call and return thunk.  :samp:`thunk-inline` converts function
  return to inlined call and return thunk.  :samp:`thunk-extern` converts
  function return to external call and return thunk provided in a separate
  object file.  You can control this behavior for a specific function by
  using the function attribute ``function_return``.
  See :ref:`function-attributes`.

  Note that :option:`-mindirect-return=thunk-extern` is compatible with
  :option:`-fcf-protection=branch` since the external thunk can be made
  to enable control-flow check.

  Note that :option:`-mcmodel=large` is incompatible with
  :option:`-mfunction-return=thunk` and
  :option:`-mfunction-return=thunk-extern` since the thunk function may
  not be reachable in the large code model.

.. option:: -mindirect-branch-register

  Force indirect call and jump via register.

These :samp:`-m` switches are supported in addition to the above
on x86-64 processors in 64-bit environments.

.. option:: -m32, -m64, -mx32, -m16, -miamcu

  Generate code for a 16-bit, 32-bit or 64-bit environment.
  The :option:`-m32` option sets ``int``, ``long``, and pointer types
  to 32 bits, and
  generates code that runs on any i386 system.

  The :option:`-m64` option sets ``int`` to 32 bits and ``long`` and pointer
  types to 64 bits, and generates code for the x86-64 architecture.
  For Darwin only the :option:`-m64` option also turns off the :option:`-fno-pic`
  and :option:`-mdynamic-no-pic` options.

  The :option:`-mx32` option sets ``int``, ``long``, and pointer types
  to 32 bits, and
  generates code for the x86-64 architecture.

  The :option:`-m16` option is the same as :option:`-m32` , except for that
  it outputs the ``.code16gcc`` assembly directive at the beginning of
  the assembly output so that the binary can run in 16-bit mode.

  The :option:`-miamcu` option generates code which conforms to Intel MCU
  psABI.  It requires the :option:`-m32` option to be turned on.

.. option:: -mno-red-zone, -mred-zone

  Do not use a so-called 'red zone' for x86-64 code.  The red zone is mandated
  by the x86-64 ABI; it is a 128-byte area beyond the location of the
  stack pointer that is not modified by signal or interrupt handlers
  and therefore can be used for temporary data without adjusting the stack
  pointer.  The flag :option:`-mno-red-zone` disables this red zone.

.. option:: -mcmodel=small

  Generate code for the small code model: the program and its symbols must
  be linked in the lower 2 GB of the address space.  Pointers are 64 bits.
  Programs can be statically or dynamically linked.  This is the default
  code model.

.. option:: -mcmodel=kernel

  Generate code for the kernel code model.  The kernel runs in the
  negative 2 GB of the address space.
  This model has to be used for Linux kernel code.

.. option:: -mcmodel=medium

  Generate code for the medium model: the program is linked in the lower 2
  GB of the address space.  Small symbols are also placed there.  Symbols
  with sizes larger than :option:`-mlarge-data-threshold` are put into
  large data or BSS sections and can be located above 2GB.  Programs can
  be statically or dynamically linked.

.. option:: -mcmodel=large

  Generate code for the large model.  This model makes no assumptions
  about addresses and sizes of sections.

.. option:: -maddress-mode=long

  Generate code for long address mode.  This is only supported for 64-bit
  and x32 environments.  It is the default address mode for 64-bit
  environments.

.. option:: -maddress-mode=short

  Generate code for short address mode.  This is only supported for 32-bit
  and x32 environments.  It is the default address mode for 32-bit and
  x32 environments.

.. option:: -mneeded

  Emit GNU_PROPERTY_X86_ISA_1_NEEDED GNU property for Linux target to
  indicate the micro-architecture ISA level required to execute the binary.

.. _x86-windows-options:

x86 Windows Options
^^^^^^^^^^^^^^^^^^^

.. index:: x86 Windows Options

.. index:: Windows Options for x86

These additional options are available for Microsoft Windows targets:

.. option:: -mconsole

  This option
  specifies that a console application is to be generated, by
  instructing the linker to set the PE header subsystem type
  required for console applications.
  This option is available for Cygwin and MinGW targets and is
  enabled by default on those targets.

.. option:: -mdll

  This option is available for Cygwin and MinGW targets.  It
  specifies that a DLL-a dynamic link library-is to be
  generated, enabling the selection of the required runtime
  startup object and entry point.

.. option:: -mnop-fun-dllimport

  This option is available for Cygwin and MinGW targets.  It
  specifies that the ``dllimport`` attribute should be ignored.

.. option:: -mthread

  This option is available for MinGW targets. It specifies
  that MinGW-specific thread support is to be used.

.. option:: -municode

  This option is available for MinGW-w64 targets.  It causes
  the ``UNICODE`` preprocessor macro to be predefined, and
  chooses Unicode-capable runtime startup code.

.. option:: -mwin32

  This option is available for Cygwin and MinGW targets.  It
  specifies that the typical Microsoft Windows predefined macros are to
  be set in the pre-processor, but does not influence the choice
  of runtime library/startup code.

.. option:: -mwindows

  This option is available for Cygwin and MinGW targets.  It
  specifies that a GUI application is to be generated by
  instructing the linker to set the PE header subsystem type
  appropriately.

.. option:: -fno-set-stack-executable, -fset-stack-executable

  This option is available for MinGW targets. It specifies that
  the executable flag for the stack used by nested functions isn't
  set. This is necessary for binaries running in kernel mode of
  Microsoft Windows, as there the User32 API, which is used to set executable
  privileges, isn't available.

.. option:: -fwritable-relocated-rdata, -fno-writable-relocated-rdata

  This option is available for MinGW and Cygwin targets.  It specifies
  that relocated-data in read-only section is put into the ``.data``
  section.  This is a necessary for older runtimes not supporting
  modification of ``.rdata`` sections for pseudo-relocation.

.. option:: -mpe-aligned-commons

  This option is available for Cygwin and MinGW targets.  It
  specifies that the GNU extension to the PE file format that
  permits the correct alignment of COMMON variables should be
  used when generating code.  It is enabled by default if
  GCC detects that the target assembler found during configuration
  supports the feature.

See also under x86 Options for standard options.

.. _xstormy16-options:

Xstormy16 Options
^^^^^^^^^^^^^^^^^

.. index:: Xstormy16 Options

These options are defined for Xstormy16:

.. option:: -msim

  Choose startup files and linker script suitable for the simulator.

.. _xtensa-options:

Xtensa Options
^^^^^^^^^^^^^^

.. index:: Xtensa Options

These options are supported for Xtensa targets:

.. option:: -mconst16, -mno-const16

  Enable or disable use of ``CONST16`` instructions for loading
  constant values.  The ``CONST16`` instruction is currently not a
  standard option from Tensilica.  When enabled, ``CONST16``
  instructions are always used in place of the standard ``L32R``
  instructions.  The use of ``CONST16`` is enabled by default only if
  the ``L32R`` instruction is not available.

.. option:: -mfused-madd, -mno-fused-madd

  Enable or disable use of fused multiply/add and multiply/subtract
  instructions in the floating-point option.  This has no effect if the
  floating-point option is not also enabled.  Disabling fused multiply/add
  and multiply/subtract instructions forces the compiler to use separate
  instructions for the multiply and add/subtract operations.  This may be
  desirable in some cases where strict IEEE 754-compliant results are
  required: the fused multiply add/subtract instructions do not round the
  intermediate result, thereby producing results with *more* bits of
  precision than specified by the IEEE standard.  Disabling fused multiply
  add/subtract instructions also ensures that the program output is not
  sensitive to the compiler's ability to combine multiply and add/subtract
  operations.

.. option:: -mserialize-volatile, -mno-serialize-volatile

  When this option is enabled, GCC inserts ``MEMW`` instructions before
  ``volatile`` memory references to guarantee sequential consistency.
  The default is :option:`-mserialize-volatile`.  Use
  :option:`-mno-serialize-volatile` to omit the ``MEMW`` instructions.

.. option:: -mforce-no-pic

  For targets, like GNU/Linux, where all user-mode Xtensa code must be
  position-independent code (PIC), this option disables PIC for compiling
  kernel code.

.. option:: -mtext-section-literals, -mno-text-section-literals

  These options control the treatment of literal pools.  The default is
  :option:`-mno-text-section-literals` , which places literals in a separate
  section in the output file.  This allows the literal pool to be placed
  in a data RAM/ROM, and it also allows the linker to combine literal
  pools from separate object files to remove redundant literals and
  improve code size.  With :option:`-mtext-section-literals` , the literals
  are interspersed in the text section in order to keep them as close as
  possible to their references.  This may be necessary for large assembly
  files.  Literals for each function are placed right before that function.

.. option:: -mauto-litpools, -mno-auto-litpools

  These options control the treatment of literal pools.  The default is
  :option:`-mno-auto-litpools` , which places literals in a separate
  section in the output file unless :option:`-mtext-section-literals` is
  used.  With :option:`-mauto-litpools` the literals are interspersed in
  the text section by the assembler.  Compiler does not produce explicit
  ``.literal`` directives and loads literals into registers with
  ``MOVI`` instructions instead of ``L32R`` to let the assembler
  do relaxation and place literals as necessary.  This option allows
  assembler to create several literal pools per function and assemble
  very big functions, which may not be possible with
  :option:`-mtext-section-literals`.

.. option:: -mtarget-align, -mno-target-align

  When this option is enabled, GCC instructs the assembler to
  automatically align instructions to reduce branch penalties at the
  expense of some code density.  The assembler attempts to widen density
  instructions to align branch targets and the instructions following call
  instructions.  If there are not enough preceding safe density
  instructions to align a target, no widening is performed.  The
  default is :option:`-mtarget-align`.  These options do not affect the
  treatment of auto-aligned instructions like ``LOOP``, which the
  assembler always aligns, either by widening density instructions or
  by inserting NOP instructions.

.. option:: -mlongcalls, -mno-longcalls

  When this option is enabled, GCC instructs the assembler to translate
  direct calls to indirect calls unless it can determine that the target
  of a direct call is in the range allowed by the call instruction.  This
  translation typically occurs for calls to functions in other source
  files.  Specifically, the assembler translates a direct ``CALL``
  instruction into an ``L32R`` followed by a ``CALLX`` instruction.
  The default is :option:`-mno-longcalls`.  This option should be used in
  programs where the call target can potentially be out of range.  This
  option is implemented in the assembler, not the compiler, so the
  assembly code generated by GCC still shows direct call
  instructions-look at the disassembled object code to see the actual
  instructions.  Note that the assembler uses an indirect call for
  every cross-file call, not just those that really are out of range.

.. option:: -mabi=name

  Generate code for the specified ABI.  Permissible values are: :samp:`call0`,
  :samp:`windowed`.  Default ABI is chosen by the Xtensa core configuration.

.. option:: -mabi=call0

  When this option is enabled function parameters are passed in registers
  ``a2`` through ``a7``, registers ``a12`` through ``a15`` are
  caller-saved, and register ``a15`` may be used as a frame pointer.
  When this version of the ABI is enabled the C preprocessor symbol
  ``__XTENSA_CALL0_ABI__`` is defined.

.. option:: -mabi=windowed

  When this option is enabled function parameters are passed in registers
  ``a10`` through ``a15``, and called function rotates register window
  by 8 registers on entry so that its arguments are found in registers
  ``a2`` through ``a7``.  Register ``a7`` may be used as a frame
  pointer.  Register window is rotated 8 registers back upon return.
  When this version of the ABI is enabled the C preprocessor symbol
  ``__XTENSA_WINDOWED_ABI__`` is defined.

.. _zseries-options:

zSeries Options
^^^^^^^^^^^^^^^

.. index:: zSeries options

These are listed under See :ref:`s-390-and-zseries-options`.

.. man end

