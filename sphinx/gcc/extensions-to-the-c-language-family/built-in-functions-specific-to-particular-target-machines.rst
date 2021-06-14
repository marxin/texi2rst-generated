..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _target-builtins:

Built-in Functions Specific to Particular Target Machines
*********************************************************

On some target machines, GCC supports many built-in functions specific
to those machines.  Generally these generate calls to specific machine
instructions, but allow the compiler to schedule those calls.

.. toctree::
  :maxdepth: 2

  built-in-functions-specific-to-particular-target-machines/aarch64-built-in-functions
  built-in-functions-specific-to-particular-target-machines/alpha-built-in-functions
  built-in-functions-specific-to-particular-target-machines/altera-nios-ii-built-in-functions
  built-in-functions-specific-to-particular-target-machines/arc-built-in-functions
  built-in-functions-specific-to-particular-target-machines/arc-simd-built-in-functions
  built-in-functions-specific-to-particular-target-machines/arm-iwmmxt-built-in-functions
  built-in-functions-specific-to-particular-target-machines/avr-built-in-functions
  built-in-functions-specific-to-particular-target-machines/blackfin-built-in-functions
  built-in-functions-specific-to-particular-target-machines/bpf-built-in-functions
  built-in-functions-specific-to-particular-target-machines/fr-v-built-in-functions
  built-in-functions-specific-to-particular-target-machines/mips-dsp-built-in-functions
  built-in-functions-specific-to-particular-target-machines/mips-loongson-built-in-functions
  built-in-functions-specific-to-particular-target-machines/other-mips-built-in-functions
  built-in-functions-specific-to-particular-target-machines/msp430-built-in-functions
  built-in-functions-specific-to-particular-target-machines/nds32-built-in-functions
  built-in-functions-specific-to-particular-target-machines/picochip-built-in-functions
  built-in-functions-specific-to-particular-target-machines/basic-powerpc-built-in-functions
  built-in-functions-specific-to-particular-target-machines/powerpc-altivec-vsx-built-in-functions
  built-in-functions-specific-to-particular-target-machines/powerpc-hardware-transactional-memory-built-in-functions
  built-in-functions-specific-to-particular-target-machines/powerpc-matrix-multiply-assist-built-in-functions
  built-in-functions-specific-to-particular-target-machines/pru-built-in-functions
  built-in-functions-specific-to-particular-target-machines/risc-v-built-in-functions
  built-in-functions-specific-to-particular-target-machines/rx-built-in-functions
  built-in-functions-specific-to-particular-target-machines/s-390-system-z-built-in-functions
  built-in-functions-specific-to-particular-target-machines/sh-built-in-functions
  built-in-functions-specific-to-particular-target-machines/sparc-vis-built-in-functions
  built-in-functions-specific-to-particular-target-machines/ti-c6x-built-in-functions
  built-in-functions-specific-to-particular-target-machines/tile-gx-built-in-functions
  built-in-functions-specific-to-particular-target-machines/tilepro-built-in-functions
  built-in-functions-specific-to-particular-target-machines/x86-built-in-functions

.. _arm-c-language-extensions-(acle):

ARM C Language Extensions (ACLE)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC implements extensions for C as described in the ARM C Language
Extensions (ACLE) specification, which can be found at
https://developer.arm.com/documentation/ihi0053/latest/.

As a part of ACLE, GCC implements extensions for Advanced SIMD as described in
the ARM C Language Extensions Specification.  The complete list of Advanced SIMD
intrinsics can be found at
https://developer.arm.com/documentation/ihi0073/latest/.
The built-in intrinsics for the Advanced SIMD extension are available when
NEON is enabled.

Currently, ARM and AArch64 back ends do not support ACLE 2.0 fully.  Both
back ends support CRC32 intrinsics and the ARM back end supports the
Coprocessor intrinsics, all from :samp:`arm_acle.h`.  The ARM back end's 16-bit
floating-point Advanced SIMD intrinsics currently comply to ACLE v1.1.
AArch64's back end does not have support for 16-bit floating point Advanced SIMD
intrinsics yet.

See ARM Options and AArch64 Options for more information on the
availability of extensions.

.. _arm-floating-point-status-and-control-intrinsics:

ARM Floating Point Status and Control Intrinsics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the ARM family of
processors with floating-point unit.

.. code-block:: c++

  unsigned int __builtin_arm_get_fpscr ()
  void __builtin_arm_set_fpscr (unsigned int)

.. _arm-armv8-m-security-extensions:

ARM ARMv8-M Security Extensions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC implements the ARMv8-M Security Extensions as described in the ARMv8-M
Security Extensions: Requirements on Development Tools Engineering
Specification, which can be found at
https://developer.arm.com/documentation/ecm0359818/latest/.

As part of the Security Extensions GCC implements two new function attributes:
``cmse_nonsecure_entry`` and ``cmse_nonsecure_call``.

As part of the Security Extensions GCC implements the intrinsics below.  FPTR
is used here to mean any function pointer type.

.. code-block:: c++

  cmse_address_info_t cmse_TT (void *)
  cmse_address_info_t cmse_TT_fptr (FPTR)
  cmse_address_info_t cmse_TTT (void *)
  cmse_address_info_t cmse_TTT_fptr (FPTR)
  cmse_address_info_t cmse_TTA (void *)
  cmse_address_info_t cmse_TTA_fptr (FPTR)
  cmse_address_info_t cmse_TTAT (void *)
  cmse_address_info_t cmse_TTAT_fptr (FPTR)
  void * cmse_check_address_range (void *, size_t, int)
  typeof(p) cmse_nsfptr_create (FPTR p)
  intptr_t cmse_is_nsfptr (FPTR)
  int cmse_nonsecure_caller (void)

.. _mips-paired-single-support:

MIPS Paired-Single Support
^^^^^^^^^^^^^^^^^^^^^^^^^^

The MIPS64 architecture includes a number of instructions that
operate on pairs of single-precision floating-point values.
Each pair is packed into a 64-bit floating-point register,
with one element being designated the 'upper half' and
the other being designated the 'lower half'.

GCC supports paired-single operations using both the generic
vector extensions (see :ref:`vector-extensions`) and a collection of
MIPS-specific built-in functions.  Both kinds of support are
enabled by the :option:`-mpaired-single` command-line option.

The vector type associated with paired-single values is usually
called ``v2sf``.  It can be defined in C as follows:

.. code-block:: c++

  typedef float v2sf __attribute__ ((vector_size (8)));

``v2sf`` values are initialized in the same way as aggregates.
For example:

.. code-block:: c++

  v2sf a = {1.5, 9.1};
  v2sf b;
  float e, f;
  b = (v2sf) {e, f};

*Note:* The CPU's endianness determines which value is stored in
the upper half of a register and which value is stored in the lower half.
On little-endian targets, the first value is the lower one and the second
value is the upper one.  The opposite order applies to big-endian targets.
For example, the code above sets the lower half of ``a`` to
``1.5`` on little-endian targets and ``9.1`` on big-endian targets.

  .. _mips-simd-architecture-(msa)-support:

MIPS SIMD Architecture (MSA) Support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. toctree::
  :maxdepth: 2


GCC provides intrinsics to access the SIMD instructions provided by the
MSA MIPS SIMD Architecture.  The interface is made available by including
``<msa.h>`` and using :option:`-mmsa -mhard-float -mfp64 -mnan`:samp:`=2008`.
For each ``__builtin_msa_*``, there is a shortened name of the intrinsic,
``__msa_*``.

MSA implements 128-bit wide vector registers, operating on 8-, 16-, 32- and
64-bit integer, 16- and 32-bit fixed-point, or 32- and 64-bit floating point
data elements.  The following vectors typedefs are included in ``msa.h`` :

* ``v16i8``, a vector of sixteen signed 8-bit integers;

* ``v16u8``, a vector of sixteen unsigned 8-bit integers;

* ``v8i16``, a vector of eight signed 16-bit integers;

* ``v8u16``, a vector of eight unsigned 16-bit integers;

* ``v4i32``, a vector of four signed 32-bit integers;

* ``v4u32``, a vector of four unsigned 32-bit integers;

* ``v2i64``, a vector of two signed 64-bit integers;

* ``v2u64``, a vector of two unsigned 64-bit integers;

* ``v4f32``, a vector of four 32-bit floats;

* ``v2f64``, a vector of two 64-bit doubles.

Instructions and corresponding built-ins may have additional restrictions and/or
input/output values manipulated:

* ``imm0_1``, an integer literal in range 0 to 1;

* ``imm0_3``, an integer literal in range 0 to 3;

* ``imm0_7``, an integer literal in range 0 to 7;

* ``imm0_15``, an integer literal in range 0 to 15;

* ``imm0_31``, an integer literal in range 0 to 31;

* ``imm0_63``, an integer literal in range 0 to 63;

* ``imm0_255``, an integer literal in range 0 to 255;

* ``imm_n16_15``, an integer literal in range -16 to 15;

* ``imm_n512_511``, an integer literal in range -512 to 511;

* ``imm_n1024_1022``, an integer literal in range -512 to 511 left
  shifted by 1 bit, i.e., -1024, -1022, ..., 1020, 1022;

* ``imm_n2048_2044``, an integer literal in range -512 to 511 left
  shifted by 2 bits, i.e., -2048, -2044, ..., 2040, 2044;

* ``imm_n4096_4088``, an integer literal in range -512 to 511 left
  shifted by 3 bits, i.e., -4096, -4088, ..., 4080, 4088;

* ``imm1_4``, an integer literal in range 1 to 4;

* ``i32, i64, u32, u64, f32, f64``, defined as follows:

.. code-block:: c++

  {
  typedef int i32;
  #if __LONG_MAX__ == __LONG_LONG_MAX__
  typedef long i64;
  #else
  typedef long long i64;
  #endif

  typedef unsigned int u32;
  #if __LONG_MAX__ == __LONG_LONG_MAX__
  typedef unsigned long u64;
  #else
  typedef unsigned long long u64;
  #endif

  typedef double f64;
  typedef float f32;
  }

.. _mips-simd-architecture-built-in-functions:

MIPS SIMD Architecture Built-in Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The intrinsics provided are listed below; each is named after the
machine instruction.

.. code-block:: c++

  v16i8 __builtin_msa_add_a_b (v16i8, v16i8);
  v8i16 __builtin_msa_add_a_h (v8i16, v8i16);
  v4i32 __builtin_msa_add_a_w (v4i32, v4i32);
  v2i64 __builtin_msa_add_a_d (v2i64, v2i64);

  v16i8 __builtin_msa_adds_a_b (v16i8, v16i8);
  v8i16 __builtin_msa_adds_a_h (v8i16, v8i16);
  v4i32 __builtin_msa_adds_a_w (v4i32, v4i32);
  v2i64 __builtin_msa_adds_a_d (v2i64, v2i64);

  v16i8 __builtin_msa_adds_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_adds_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_adds_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_adds_s_d (v2i64, v2i64);

  v16u8 __builtin_msa_adds_u_b (v16u8, v16u8);
  v8u16 __builtin_msa_adds_u_h (v8u16, v8u16);
  v4u32 __builtin_msa_adds_u_w (v4u32, v4u32);
  v2u64 __builtin_msa_adds_u_d (v2u64, v2u64);

  v16i8 __builtin_msa_addv_b (v16i8, v16i8);
  v8i16 __builtin_msa_addv_h (v8i16, v8i16);
  v4i32 __builtin_msa_addv_w (v4i32, v4i32);
  v2i64 __builtin_msa_addv_d (v2i64, v2i64);

  v16i8 __builtin_msa_addvi_b (v16i8, imm0_31);
  v8i16 __builtin_msa_addvi_h (v8i16, imm0_31);
  v4i32 __builtin_msa_addvi_w (v4i32, imm0_31);
  v2i64 __builtin_msa_addvi_d (v2i64, imm0_31);

  v16u8 __builtin_msa_and_v (v16u8, v16u8);

  v16u8 __builtin_msa_andi_b (v16u8, imm0_255);

  v16i8 __builtin_msa_asub_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_asub_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_asub_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_asub_s_d (v2i64, v2i64);

  v16u8 __builtin_msa_asub_u_b (v16u8, v16u8);
  v8u16 __builtin_msa_asub_u_h (v8u16, v8u16);
  v4u32 __builtin_msa_asub_u_w (v4u32, v4u32);
  v2u64 __builtin_msa_asub_u_d (v2u64, v2u64);

  v16i8 __builtin_msa_ave_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_ave_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_ave_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_ave_s_d (v2i64, v2i64);

  v16u8 __builtin_msa_ave_u_b (v16u8, v16u8);
  v8u16 __builtin_msa_ave_u_h (v8u16, v8u16);
  v4u32 __builtin_msa_ave_u_w (v4u32, v4u32);
  v2u64 __builtin_msa_ave_u_d (v2u64, v2u64);

  v16i8 __builtin_msa_aver_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_aver_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_aver_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_aver_s_d (v2i64, v2i64);

  v16u8 __builtin_msa_aver_u_b (v16u8, v16u8);
  v8u16 __builtin_msa_aver_u_h (v8u16, v8u16);
  v4u32 __builtin_msa_aver_u_w (v4u32, v4u32);
  v2u64 __builtin_msa_aver_u_d (v2u64, v2u64);

  v16u8 __builtin_msa_bclr_b (v16u8, v16u8);
  v8u16 __builtin_msa_bclr_h (v8u16, v8u16);
  v4u32 __builtin_msa_bclr_w (v4u32, v4u32);
  v2u64 __builtin_msa_bclr_d (v2u64, v2u64);

  v16u8 __builtin_msa_bclri_b (v16u8, imm0_7);
  v8u16 __builtin_msa_bclri_h (v8u16, imm0_15);
  v4u32 __builtin_msa_bclri_w (v4u32, imm0_31);
  v2u64 __builtin_msa_bclri_d (v2u64, imm0_63);

  v16u8 __builtin_msa_binsl_b (v16u8, v16u8, v16u8);
  v8u16 __builtin_msa_binsl_h (v8u16, v8u16, v8u16);
  v4u32 __builtin_msa_binsl_w (v4u32, v4u32, v4u32);
  v2u64 __builtin_msa_binsl_d (v2u64, v2u64, v2u64);

  v16u8 __builtin_msa_binsli_b (v16u8, v16u8, imm0_7);
  v8u16 __builtin_msa_binsli_h (v8u16, v8u16, imm0_15);
  v4u32 __builtin_msa_binsli_w (v4u32, v4u32, imm0_31);
  v2u64 __builtin_msa_binsli_d (v2u64, v2u64, imm0_63);

  v16u8 __builtin_msa_binsr_b (v16u8, v16u8, v16u8);
  v8u16 __builtin_msa_binsr_h (v8u16, v8u16, v8u16);
  v4u32 __builtin_msa_binsr_w (v4u32, v4u32, v4u32);
  v2u64 __builtin_msa_binsr_d (v2u64, v2u64, v2u64);

  v16u8 __builtin_msa_binsri_b (v16u8, v16u8, imm0_7);
  v8u16 __builtin_msa_binsri_h (v8u16, v8u16, imm0_15);
  v4u32 __builtin_msa_binsri_w (v4u32, v4u32, imm0_31);
  v2u64 __builtin_msa_binsri_d (v2u64, v2u64, imm0_63);

  v16u8 __builtin_msa_bmnz_v (v16u8, v16u8, v16u8);

  v16u8 __builtin_msa_bmnzi_b (v16u8, v16u8, imm0_255);

  v16u8 __builtin_msa_bmz_v (v16u8, v16u8, v16u8);

  v16u8 __builtin_msa_bmzi_b (v16u8, v16u8, imm0_255);

  v16u8 __builtin_msa_bneg_b (v16u8, v16u8);
  v8u16 __builtin_msa_bneg_h (v8u16, v8u16);
  v4u32 __builtin_msa_bneg_w (v4u32, v4u32);
  v2u64 __builtin_msa_bneg_d (v2u64, v2u64);

  v16u8 __builtin_msa_bnegi_b (v16u8, imm0_7);
  v8u16 __builtin_msa_bnegi_h (v8u16, imm0_15);
  v4u32 __builtin_msa_bnegi_w (v4u32, imm0_31);
  v2u64 __builtin_msa_bnegi_d (v2u64, imm0_63);

  i32 __builtin_msa_bnz_b (v16u8);
  i32 __builtin_msa_bnz_h (v8u16);
  i32 __builtin_msa_bnz_w (v4u32);
  i32 __builtin_msa_bnz_d (v2u64);

  i32 __builtin_msa_bnz_v (v16u8);

  v16u8 __builtin_msa_bsel_v (v16u8, v16u8, v16u8);

  v16u8 __builtin_msa_bseli_b (v16u8, v16u8, imm0_255);

  v16u8 __builtin_msa_bset_b (v16u8, v16u8);
  v8u16 __builtin_msa_bset_h (v8u16, v8u16);
  v4u32 __builtin_msa_bset_w (v4u32, v4u32);
  v2u64 __builtin_msa_bset_d (v2u64, v2u64);

  v16u8 __builtin_msa_bseti_b (v16u8, imm0_7);
  v8u16 __builtin_msa_bseti_h (v8u16, imm0_15);
  v4u32 __builtin_msa_bseti_w (v4u32, imm0_31);
  v2u64 __builtin_msa_bseti_d (v2u64, imm0_63);

  i32 __builtin_msa_bz_b (v16u8);
  i32 __builtin_msa_bz_h (v8u16);
  i32 __builtin_msa_bz_w (v4u32);
  i32 __builtin_msa_bz_d (v2u64);

  i32 __builtin_msa_bz_v (v16u8);

  v16i8 __builtin_msa_ceq_b (v16i8, v16i8);
  v8i16 __builtin_msa_ceq_h (v8i16, v8i16);
  v4i32 __builtin_msa_ceq_w (v4i32, v4i32);
  v2i64 __builtin_msa_ceq_d (v2i64, v2i64);

  v16i8 __builtin_msa_ceqi_b (v16i8, imm_n16_15);
  v8i16 __builtin_msa_ceqi_h (v8i16, imm_n16_15);
  v4i32 __builtin_msa_ceqi_w (v4i32, imm_n16_15);
  v2i64 __builtin_msa_ceqi_d (v2i64, imm_n16_15);

  i32 __builtin_msa_cfcmsa (imm0_31);

  v16i8 __builtin_msa_cle_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_cle_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_cle_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_cle_s_d (v2i64, v2i64);

  v16i8 __builtin_msa_cle_u_b (v16u8, v16u8);
  v8i16 __builtin_msa_cle_u_h (v8u16, v8u16);
  v4i32 __builtin_msa_cle_u_w (v4u32, v4u32);
  v2i64 __builtin_msa_cle_u_d (v2u64, v2u64);

  v16i8 __builtin_msa_clei_s_b (v16i8, imm_n16_15);
  v8i16 __builtin_msa_clei_s_h (v8i16, imm_n16_15);
  v4i32 __builtin_msa_clei_s_w (v4i32, imm_n16_15);
  v2i64 __builtin_msa_clei_s_d (v2i64, imm_n16_15);

  v16i8 __builtin_msa_clei_u_b (v16u8, imm0_31);
  v8i16 __builtin_msa_clei_u_h (v8u16, imm0_31);
  v4i32 __builtin_msa_clei_u_w (v4u32, imm0_31);
  v2i64 __builtin_msa_clei_u_d (v2u64, imm0_31);

  v16i8 __builtin_msa_clt_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_clt_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_clt_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_clt_s_d (v2i64, v2i64);

  v16i8 __builtin_msa_clt_u_b (v16u8, v16u8);
  v8i16 __builtin_msa_clt_u_h (v8u16, v8u16);
  v4i32 __builtin_msa_clt_u_w (v4u32, v4u32);
  v2i64 __builtin_msa_clt_u_d (v2u64, v2u64);

  v16i8 __builtin_msa_clti_s_b (v16i8, imm_n16_15);
  v8i16 __builtin_msa_clti_s_h (v8i16, imm_n16_15);
  v4i32 __builtin_msa_clti_s_w (v4i32, imm_n16_15);
  v2i64 __builtin_msa_clti_s_d (v2i64, imm_n16_15);

  v16i8 __builtin_msa_clti_u_b (v16u8, imm0_31);
  v8i16 __builtin_msa_clti_u_h (v8u16, imm0_31);
  v4i32 __builtin_msa_clti_u_w (v4u32, imm0_31);
  v2i64 __builtin_msa_clti_u_d (v2u64, imm0_31);

  i32 __builtin_msa_copy_s_b (v16i8, imm0_15);
  i32 __builtin_msa_copy_s_h (v8i16, imm0_7);
  i32 __builtin_msa_copy_s_w (v4i32, imm0_3);
  i64 __builtin_msa_copy_s_d (v2i64, imm0_1);

  u32 __builtin_msa_copy_u_b (v16i8, imm0_15);
  u32 __builtin_msa_copy_u_h (v8i16, imm0_7);
  u32 __builtin_msa_copy_u_w (v4i32, imm0_3);
  u64 __builtin_msa_copy_u_d (v2i64, imm0_1);

  void __builtin_msa_ctcmsa (imm0_31, i32);

  v16i8 __builtin_msa_div_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_div_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_div_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_div_s_d (v2i64, v2i64);

  v16u8 __builtin_msa_div_u_b (v16u8, v16u8);
  v8u16 __builtin_msa_div_u_h (v8u16, v8u16);
  v4u32 __builtin_msa_div_u_w (v4u32, v4u32);
  v2u64 __builtin_msa_div_u_d (v2u64, v2u64);

  v8i16 __builtin_msa_dotp_s_h (v16i8, v16i8);
  v4i32 __builtin_msa_dotp_s_w (v8i16, v8i16);
  v2i64 __builtin_msa_dotp_s_d (v4i32, v4i32);

  v8u16 __builtin_msa_dotp_u_h (v16u8, v16u8);
  v4u32 __builtin_msa_dotp_u_w (v8u16, v8u16);
  v2u64 __builtin_msa_dotp_u_d (v4u32, v4u32);

  v8i16 __builtin_msa_dpadd_s_h (v8i16, v16i8, v16i8);
  v4i32 __builtin_msa_dpadd_s_w (v4i32, v8i16, v8i16);
  v2i64 __builtin_msa_dpadd_s_d (v2i64, v4i32, v4i32);

  v8u16 __builtin_msa_dpadd_u_h (v8u16, v16u8, v16u8);
  v4u32 __builtin_msa_dpadd_u_w (v4u32, v8u16, v8u16);
  v2u64 __builtin_msa_dpadd_u_d (v2u64, v4u32, v4u32);

  v8i16 __builtin_msa_dpsub_s_h (v8i16, v16i8, v16i8);
  v4i32 __builtin_msa_dpsub_s_w (v4i32, v8i16, v8i16);
  v2i64 __builtin_msa_dpsub_s_d (v2i64, v4i32, v4i32);

  v8i16 __builtin_msa_dpsub_u_h (v8i16, v16u8, v16u8);
  v4i32 __builtin_msa_dpsub_u_w (v4i32, v8u16, v8u16);
  v2i64 __builtin_msa_dpsub_u_d (v2i64, v4u32, v4u32);

  v4f32 __builtin_msa_fadd_w (v4f32, v4f32);
  v2f64 __builtin_msa_fadd_d (v2f64, v2f64);

  v4i32 __builtin_msa_fcaf_w (v4f32, v4f32);
  v2i64 __builtin_msa_fcaf_d (v2f64, v2f64);

  v4i32 __builtin_msa_fceq_w (v4f32, v4f32);
  v2i64 __builtin_msa_fceq_d (v2f64, v2f64);

  v4i32 __builtin_msa_fclass_w (v4f32);
  v2i64 __builtin_msa_fclass_d (v2f64);

  v4i32 __builtin_msa_fcle_w (v4f32, v4f32);
  v2i64 __builtin_msa_fcle_d (v2f64, v2f64);

  v4i32 __builtin_msa_fclt_w (v4f32, v4f32);
  v2i64 __builtin_msa_fclt_d (v2f64, v2f64);

  v4i32 __builtin_msa_fcne_w (v4f32, v4f32);
  v2i64 __builtin_msa_fcne_d (v2f64, v2f64);

  v4i32 __builtin_msa_fcor_w (v4f32, v4f32);
  v2i64 __builtin_msa_fcor_d (v2f64, v2f64);

  v4i32 __builtin_msa_fcueq_w (v4f32, v4f32);
  v2i64 __builtin_msa_fcueq_d (v2f64, v2f64);

  v4i32 __builtin_msa_fcule_w (v4f32, v4f32);
  v2i64 __builtin_msa_fcule_d (v2f64, v2f64);

  v4i32 __builtin_msa_fcult_w (v4f32, v4f32);
  v2i64 __builtin_msa_fcult_d (v2f64, v2f64);

  v4i32 __builtin_msa_fcun_w (v4f32, v4f32);
  v2i64 __builtin_msa_fcun_d (v2f64, v2f64);

  v4i32 __builtin_msa_fcune_w (v4f32, v4f32);
  v2i64 __builtin_msa_fcune_d (v2f64, v2f64);

  v4f32 __builtin_msa_fdiv_w (v4f32, v4f32);
  v2f64 __builtin_msa_fdiv_d (v2f64, v2f64);

  v8i16 __builtin_msa_fexdo_h (v4f32, v4f32);
  v4f32 __builtin_msa_fexdo_w (v2f64, v2f64);

  v4f32 __builtin_msa_fexp2_w (v4f32, v4i32);
  v2f64 __builtin_msa_fexp2_d (v2f64, v2i64);

  v4f32 __builtin_msa_fexupl_w (v8i16);
  v2f64 __builtin_msa_fexupl_d (v4f32);

  v4f32 __builtin_msa_fexupr_w (v8i16);
  v2f64 __builtin_msa_fexupr_d (v4f32);

  v4f32 __builtin_msa_ffint_s_w (v4i32);
  v2f64 __builtin_msa_ffint_s_d (v2i64);

  v4f32 __builtin_msa_ffint_u_w (v4u32);
  v2f64 __builtin_msa_ffint_u_d (v2u64);

  v4f32 __builtin_msa_ffql_w (v8i16);
  v2f64 __builtin_msa_ffql_d (v4i32);

  v4f32 __builtin_msa_ffqr_w (v8i16);
  v2f64 __builtin_msa_ffqr_d (v4i32);

  v16i8 __builtin_msa_fill_b (i32);
  v8i16 __builtin_msa_fill_h (i32);
  v4i32 __builtin_msa_fill_w (i32);
  v2i64 __builtin_msa_fill_d (i64);

  v4f32 __builtin_msa_flog2_w (v4f32);
  v2f64 __builtin_msa_flog2_d (v2f64);

  v4f32 __builtin_msa_fmadd_w (v4f32, v4f32, v4f32);
  v2f64 __builtin_msa_fmadd_d (v2f64, v2f64, v2f64);

  v4f32 __builtin_msa_fmax_w (v4f32, v4f32);
  v2f64 __builtin_msa_fmax_d (v2f64, v2f64);

  v4f32 __builtin_msa_fmax_a_w (v4f32, v4f32);
  v2f64 __builtin_msa_fmax_a_d (v2f64, v2f64);

  v4f32 __builtin_msa_fmin_w (v4f32, v4f32);
  v2f64 __builtin_msa_fmin_d (v2f64, v2f64);

  v4f32 __builtin_msa_fmin_a_w (v4f32, v4f32);
  v2f64 __builtin_msa_fmin_a_d (v2f64, v2f64);

  v4f32 __builtin_msa_fmsub_w (v4f32, v4f32, v4f32);
  v2f64 __builtin_msa_fmsub_d (v2f64, v2f64, v2f64);

  v4f32 __builtin_msa_fmul_w (v4f32, v4f32);
  v2f64 __builtin_msa_fmul_d (v2f64, v2f64);

  v4f32 __builtin_msa_frint_w (v4f32);
  v2f64 __builtin_msa_frint_d (v2f64);

  v4f32 __builtin_msa_frcp_w (v4f32);
  v2f64 __builtin_msa_frcp_d (v2f64);

  v4f32 __builtin_msa_frsqrt_w (v4f32);
  v2f64 __builtin_msa_frsqrt_d (v2f64);

  v4i32 __builtin_msa_fsaf_w (v4f32, v4f32);
  v2i64 __builtin_msa_fsaf_d (v2f64, v2f64);

  v4i32 __builtin_msa_fseq_w (v4f32, v4f32);
  v2i64 __builtin_msa_fseq_d (v2f64, v2f64);

  v4i32 __builtin_msa_fsle_w (v4f32, v4f32);
  v2i64 __builtin_msa_fsle_d (v2f64, v2f64);

  v4i32 __builtin_msa_fslt_w (v4f32, v4f32);
  v2i64 __builtin_msa_fslt_d (v2f64, v2f64);

  v4i32 __builtin_msa_fsne_w (v4f32, v4f32);
  v2i64 __builtin_msa_fsne_d (v2f64, v2f64);

  v4i32 __builtin_msa_fsor_w (v4f32, v4f32);
  v2i64 __builtin_msa_fsor_d (v2f64, v2f64);

  v4f32 __builtin_msa_fsqrt_w (v4f32);
  v2f64 __builtin_msa_fsqrt_d (v2f64);

  v4f32 __builtin_msa_fsub_w (v4f32, v4f32);
  v2f64 __builtin_msa_fsub_d (v2f64, v2f64);

  v4i32 __builtin_msa_fsueq_w (v4f32, v4f32);
  v2i64 __builtin_msa_fsueq_d (v2f64, v2f64);

  v4i32 __builtin_msa_fsule_w (v4f32, v4f32);
  v2i64 __builtin_msa_fsule_d (v2f64, v2f64);

  v4i32 __builtin_msa_fsult_w (v4f32, v4f32);
  v2i64 __builtin_msa_fsult_d (v2f64, v2f64);

  v4i32 __builtin_msa_fsun_w (v4f32, v4f32);
  v2i64 __builtin_msa_fsun_d (v2f64, v2f64);

  v4i32 __builtin_msa_fsune_w (v4f32, v4f32);
  v2i64 __builtin_msa_fsune_d (v2f64, v2f64);

  v4i32 __builtin_msa_ftint_s_w (v4f32);
  v2i64 __builtin_msa_ftint_s_d (v2f64);

  v4u32 __builtin_msa_ftint_u_w (v4f32);
  v2u64 __builtin_msa_ftint_u_d (v2f64);

  v8i16 __builtin_msa_ftq_h (v4f32, v4f32);
  v4i32 __builtin_msa_ftq_w (v2f64, v2f64);

  v4i32 __builtin_msa_ftrunc_s_w (v4f32);
  v2i64 __builtin_msa_ftrunc_s_d (v2f64);

  v4u32 __builtin_msa_ftrunc_u_w (v4f32);
  v2u64 __builtin_msa_ftrunc_u_d (v2f64);

  v8i16 __builtin_msa_hadd_s_h (v16i8, v16i8);
  v4i32 __builtin_msa_hadd_s_w (v8i16, v8i16);
  v2i64 __builtin_msa_hadd_s_d (v4i32, v4i32);

  v8u16 __builtin_msa_hadd_u_h (v16u8, v16u8);
  v4u32 __builtin_msa_hadd_u_w (v8u16, v8u16);
  v2u64 __builtin_msa_hadd_u_d (v4u32, v4u32);

  v8i16 __builtin_msa_hsub_s_h (v16i8, v16i8);
  v4i32 __builtin_msa_hsub_s_w (v8i16, v8i16);
  v2i64 __builtin_msa_hsub_s_d (v4i32, v4i32);

  v8i16 __builtin_msa_hsub_u_h (v16u8, v16u8);
  v4i32 __builtin_msa_hsub_u_w (v8u16, v8u16);
  v2i64 __builtin_msa_hsub_u_d (v4u32, v4u32);

  v16i8 __builtin_msa_ilvev_b (v16i8, v16i8);
  v8i16 __builtin_msa_ilvev_h (v8i16, v8i16);
  v4i32 __builtin_msa_ilvev_w (v4i32, v4i32);
  v2i64 __builtin_msa_ilvev_d (v2i64, v2i64);

  v16i8 __builtin_msa_ilvl_b (v16i8, v16i8);
  v8i16 __builtin_msa_ilvl_h (v8i16, v8i16);
  v4i32 __builtin_msa_ilvl_w (v4i32, v4i32);
  v2i64 __builtin_msa_ilvl_d (v2i64, v2i64);

  v16i8 __builtin_msa_ilvod_b (v16i8, v16i8);
  v8i16 __builtin_msa_ilvod_h (v8i16, v8i16);
  v4i32 __builtin_msa_ilvod_w (v4i32, v4i32);
  v2i64 __builtin_msa_ilvod_d (v2i64, v2i64);

  v16i8 __builtin_msa_ilvr_b (v16i8, v16i8);
  v8i16 __builtin_msa_ilvr_h (v8i16, v8i16);
  v4i32 __builtin_msa_ilvr_w (v4i32, v4i32);
  v2i64 __builtin_msa_ilvr_d (v2i64, v2i64);

  v16i8 __builtin_msa_insert_b (v16i8, imm0_15, i32);
  v8i16 __builtin_msa_insert_h (v8i16, imm0_7, i32);
  v4i32 __builtin_msa_insert_w (v4i32, imm0_3, i32);
  v2i64 __builtin_msa_insert_d (v2i64, imm0_1, i64);

  v16i8 __builtin_msa_insve_b (v16i8, imm0_15, v16i8);
  v8i16 __builtin_msa_insve_h (v8i16, imm0_7, v8i16);
  v4i32 __builtin_msa_insve_w (v4i32, imm0_3, v4i32);
  v2i64 __builtin_msa_insve_d (v2i64, imm0_1, v2i64);

  v16i8 __builtin_msa_ld_b (const void *, imm_n512_511);
  v8i16 __builtin_msa_ld_h (const void *, imm_n1024_1022);
  v4i32 __builtin_msa_ld_w (const void *, imm_n2048_2044);
  v2i64 __builtin_msa_ld_d (const void *, imm_n4096_4088);

  v16i8 __builtin_msa_ldi_b (imm_n512_511);
  v8i16 __builtin_msa_ldi_h (imm_n512_511);
  v4i32 __builtin_msa_ldi_w (imm_n512_511);
  v2i64 __builtin_msa_ldi_d (imm_n512_511);

  v8i16 __builtin_msa_madd_q_h (v8i16, v8i16, v8i16);
  v4i32 __builtin_msa_madd_q_w (v4i32, v4i32, v4i32);

  v8i16 __builtin_msa_maddr_q_h (v8i16, v8i16, v8i16);
  v4i32 __builtin_msa_maddr_q_w (v4i32, v4i32, v4i32);

  v16i8 __builtin_msa_maddv_b (v16i8, v16i8, v16i8);
  v8i16 __builtin_msa_maddv_h (v8i16, v8i16, v8i16);
  v4i32 __builtin_msa_maddv_w (v4i32, v4i32, v4i32);
  v2i64 __builtin_msa_maddv_d (v2i64, v2i64, v2i64);

  v16i8 __builtin_msa_max_a_b (v16i8, v16i8);
  v8i16 __builtin_msa_max_a_h (v8i16, v8i16);
  v4i32 __builtin_msa_max_a_w (v4i32, v4i32);
  v2i64 __builtin_msa_max_a_d (v2i64, v2i64);

  v16i8 __builtin_msa_max_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_max_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_max_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_max_s_d (v2i64, v2i64);

  v16u8 __builtin_msa_max_u_b (v16u8, v16u8);
  v8u16 __builtin_msa_max_u_h (v8u16, v8u16);
  v4u32 __builtin_msa_max_u_w (v4u32, v4u32);
  v2u64 __builtin_msa_max_u_d (v2u64, v2u64);

  v16i8 __builtin_msa_maxi_s_b (v16i8, imm_n16_15);
  v8i16 __builtin_msa_maxi_s_h (v8i16, imm_n16_15);
  v4i32 __builtin_msa_maxi_s_w (v4i32, imm_n16_15);
  v2i64 __builtin_msa_maxi_s_d (v2i64, imm_n16_15);

  v16u8 __builtin_msa_maxi_u_b (v16u8, imm0_31);
  v8u16 __builtin_msa_maxi_u_h (v8u16, imm0_31);
  v4u32 __builtin_msa_maxi_u_w (v4u32, imm0_31);
  v2u64 __builtin_msa_maxi_u_d (v2u64, imm0_31);

  v16i8 __builtin_msa_min_a_b (v16i8, v16i8);
  v8i16 __builtin_msa_min_a_h (v8i16, v8i16);
  v4i32 __builtin_msa_min_a_w (v4i32, v4i32);
  v2i64 __builtin_msa_min_a_d (v2i64, v2i64);

  v16i8 __builtin_msa_min_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_min_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_min_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_min_s_d (v2i64, v2i64);

  v16u8 __builtin_msa_min_u_b (v16u8, v16u8);
  v8u16 __builtin_msa_min_u_h (v8u16, v8u16);
  v4u32 __builtin_msa_min_u_w (v4u32, v4u32);
  v2u64 __builtin_msa_min_u_d (v2u64, v2u64);

  v16i8 __builtin_msa_mini_s_b (v16i8, imm_n16_15);
  v8i16 __builtin_msa_mini_s_h (v8i16, imm_n16_15);
  v4i32 __builtin_msa_mini_s_w (v4i32, imm_n16_15);
  v2i64 __builtin_msa_mini_s_d (v2i64, imm_n16_15);

  v16u8 __builtin_msa_mini_u_b (v16u8, imm0_31);
  v8u16 __builtin_msa_mini_u_h (v8u16, imm0_31);
  v4u32 __builtin_msa_mini_u_w (v4u32, imm0_31);
  v2u64 __builtin_msa_mini_u_d (v2u64, imm0_31);

  v16i8 __builtin_msa_mod_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_mod_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_mod_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_mod_s_d (v2i64, v2i64);

  v16u8 __builtin_msa_mod_u_b (v16u8, v16u8);
  v8u16 __builtin_msa_mod_u_h (v8u16, v8u16);
  v4u32 __builtin_msa_mod_u_w (v4u32, v4u32);
  v2u64 __builtin_msa_mod_u_d (v2u64, v2u64);

  v16i8 __builtin_msa_move_v (v16i8);

  v8i16 __builtin_msa_msub_q_h (v8i16, v8i16, v8i16);
  v4i32 __builtin_msa_msub_q_w (v4i32, v4i32, v4i32);

  v8i16 __builtin_msa_msubr_q_h (v8i16, v8i16, v8i16);
  v4i32 __builtin_msa_msubr_q_w (v4i32, v4i32, v4i32);

  v16i8 __builtin_msa_msubv_b (v16i8, v16i8, v16i8);
  v8i16 __builtin_msa_msubv_h (v8i16, v8i16, v8i16);
  v4i32 __builtin_msa_msubv_w (v4i32, v4i32, v4i32);
  v2i64 __builtin_msa_msubv_d (v2i64, v2i64, v2i64);

  v8i16 __builtin_msa_mul_q_h (v8i16, v8i16);
  v4i32 __builtin_msa_mul_q_w (v4i32, v4i32);

  v8i16 __builtin_msa_mulr_q_h (v8i16, v8i16);
  v4i32 __builtin_msa_mulr_q_w (v4i32, v4i32);

  v16i8 __builtin_msa_mulv_b (v16i8, v16i8);
  v8i16 __builtin_msa_mulv_h (v8i16, v8i16);
  v4i32 __builtin_msa_mulv_w (v4i32, v4i32);
  v2i64 __builtin_msa_mulv_d (v2i64, v2i64);

  v16i8 __builtin_msa_nloc_b (v16i8);
  v8i16 __builtin_msa_nloc_h (v8i16);
  v4i32 __builtin_msa_nloc_w (v4i32);
  v2i64 __builtin_msa_nloc_d (v2i64);

  v16i8 __builtin_msa_nlzc_b (v16i8);
  v8i16 __builtin_msa_nlzc_h (v8i16);
  v4i32 __builtin_msa_nlzc_w (v4i32);
  v2i64 __builtin_msa_nlzc_d (v2i64);

  v16u8 __builtin_msa_nor_v (v16u8, v16u8);

  v16u8 __builtin_msa_nori_b (v16u8, imm0_255);

  v16u8 __builtin_msa_or_v (v16u8, v16u8);

  v16u8 __builtin_msa_ori_b (v16u8, imm0_255);

  v16i8 __builtin_msa_pckev_b (v16i8, v16i8);
  v8i16 __builtin_msa_pckev_h (v8i16, v8i16);
  v4i32 __builtin_msa_pckev_w (v4i32, v4i32);
  v2i64 __builtin_msa_pckev_d (v2i64, v2i64);

  v16i8 __builtin_msa_pckod_b (v16i8, v16i8);
  v8i16 __builtin_msa_pckod_h (v8i16, v8i16);
  v4i32 __builtin_msa_pckod_w (v4i32, v4i32);
  v2i64 __builtin_msa_pckod_d (v2i64, v2i64);

  v16i8 __builtin_msa_pcnt_b (v16i8);
  v8i16 __builtin_msa_pcnt_h (v8i16);
  v4i32 __builtin_msa_pcnt_w (v4i32);
  v2i64 __builtin_msa_pcnt_d (v2i64);

  v16i8 __builtin_msa_sat_s_b (v16i8, imm0_7);
  v8i16 __builtin_msa_sat_s_h (v8i16, imm0_15);
  v4i32 __builtin_msa_sat_s_w (v4i32, imm0_31);
  v2i64 __builtin_msa_sat_s_d (v2i64, imm0_63);

  v16u8 __builtin_msa_sat_u_b (v16u8, imm0_7);
  v8u16 __builtin_msa_sat_u_h (v8u16, imm0_15);
  v4u32 __builtin_msa_sat_u_w (v4u32, imm0_31);
  v2u64 __builtin_msa_sat_u_d (v2u64, imm0_63);

  v16i8 __builtin_msa_shf_b (v16i8, imm0_255);
  v8i16 __builtin_msa_shf_h (v8i16, imm0_255);
  v4i32 __builtin_msa_shf_w (v4i32, imm0_255);

  v16i8 __builtin_msa_sld_b (v16i8, v16i8, i32);
  v8i16 __builtin_msa_sld_h (v8i16, v8i16, i32);
  v4i32 __builtin_msa_sld_w (v4i32, v4i32, i32);
  v2i64 __builtin_msa_sld_d (v2i64, v2i64, i32);

  v16i8 __builtin_msa_sldi_b (v16i8, v16i8, imm0_15);
  v8i16 __builtin_msa_sldi_h (v8i16, v8i16, imm0_7);
  v4i32 __builtin_msa_sldi_w (v4i32, v4i32, imm0_3);
  v2i64 __builtin_msa_sldi_d (v2i64, v2i64, imm0_1);

  v16i8 __builtin_msa_sll_b (v16i8, v16i8);
  v8i16 __builtin_msa_sll_h (v8i16, v8i16);
  v4i32 __builtin_msa_sll_w (v4i32, v4i32);
  v2i64 __builtin_msa_sll_d (v2i64, v2i64);

  v16i8 __builtin_msa_slli_b (v16i8, imm0_7);
  v8i16 __builtin_msa_slli_h (v8i16, imm0_15);
  v4i32 __builtin_msa_slli_w (v4i32, imm0_31);
  v2i64 __builtin_msa_slli_d (v2i64, imm0_63);

  v16i8 __builtin_msa_splat_b (v16i8, i32);
  v8i16 __builtin_msa_splat_h (v8i16, i32);
  v4i32 __builtin_msa_splat_w (v4i32, i32);
  v2i64 __builtin_msa_splat_d (v2i64, i32);

  v16i8 __builtin_msa_splati_b (v16i8, imm0_15);
  v8i16 __builtin_msa_splati_h (v8i16, imm0_7);
  v4i32 __builtin_msa_splati_w (v4i32, imm0_3);
  v2i64 __builtin_msa_splati_d (v2i64, imm0_1);

  v16i8 __builtin_msa_sra_b (v16i8, v16i8);
  v8i16 __builtin_msa_sra_h (v8i16, v8i16);
  v4i32 __builtin_msa_sra_w (v4i32, v4i32);
  v2i64 __builtin_msa_sra_d (v2i64, v2i64);

  v16i8 __builtin_msa_srai_b (v16i8, imm0_7);
  v8i16 __builtin_msa_srai_h (v8i16, imm0_15);
  v4i32 __builtin_msa_srai_w (v4i32, imm0_31);
  v2i64 __builtin_msa_srai_d (v2i64, imm0_63);

  v16i8 __builtin_msa_srar_b (v16i8, v16i8);
  v8i16 __builtin_msa_srar_h (v8i16, v8i16);
  v4i32 __builtin_msa_srar_w (v4i32, v4i32);
  v2i64 __builtin_msa_srar_d (v2i64, v2i64);

  v16i8 __builtin_msa_srari_b (v16i8, imm0_7);
  v8i16 __builtin_msa_srari_h (v8i16, imm0_15);
  v4i32 __builtin_msa_srari_w (v4i32, imm0_31);
  v2i64 __builtin_msa_srari_d (v2i64, imm0_63);

  v16i8 __builtin_msa_srl_b (v16i8, v16i8);
  v8i16 __builtin_msa_srl_h (v8i16, v8i16);
  v4i32 __builtin_msa_srl_w (v4i32, v4i32);
  v2i64 __builtin_msa_srl_d (v2i64, v2i64);

  v16i8 __builtin_msa_srli_b (v16i8, imm0_7);
  v8i16 __builtin_msa_srli_h (v8i16, imm0_15);
  v4i32 __builtin_msa_srli_w (v4i32, imm0_31);
  v2i64 __builtin_msa_srli_d (v2i64, imm0_63);

  v16i8 __builtin_msa_srlr_b (v16i8, v16i8);
  v8i16 __builtin_msa_srlr_h (v8i16, v8i16);
  v4i32 __builtin_msa_srlr_w (v4i32, v4i32);
  v2i64 __builtin_msa_srlr_d (v2i64, v2i64);

  v16i8 __builtin_msa_srlri_b (v16i8, imm0_7);
  v8i16 __builtin_msa_srlri_h (v8i16, imm0_15);
  v4i32 __builtin_msa_srlri_w (v4i32, imm0_31);
  v2i64 __builtin_msa_srlri_d (v2i64, imm0_63);

  void __builtin_msa_st_b (v16i8, void *, imm_n512_511);
  void __builtin_msa_st_h (v8i16, void *, imm_n1024_1022);
  void __builtin_msa_st_w (v4i32, void *, imm_n2048_2044);
  void __builtin_msa_st_d (v2i64, void *, imm_n4096_4088);

  v16i8 __builtin_msa_subs_s_b (v16i8, v16i8);
  v8i16 __builtin_msa_subs_s_h (v8i16, v8i16);
  v4i32 __builtin_msa_subs_s_w (v4i32, v4i32);
  v2i64 __builtin_msa_subs_s_d (v2i64, v2i64);

  v16u8 __builtin_msa_subs_u_b (v16u8, v16u8);
  v8u16 __builtin_msa_subs_u_h (v8u16, v8u16);
  v4u32 __builtin_msa_subs_u_w (v4u32, v4u32);
  v2u64 __builtin_msa_subs_u_d (v2u64, v2u64);

  v16u8 __builtin_msa_subsus_u_b (v16u8, v16i8);
  v8u16 __builtin_msa_subsus_u_h (v8u16, v8i16);
  v4u32 __builtin_msa_subsus_u_w (v4u32, v4i32);
  v2u64 __builtin_msa_subsus_u_d (v2u64, v2i64);

  v16i8 __builtin_msa_subsuu_s_b (v16u8, v16u8);
  v8i16 __builtin_msa_subsuu_s_h (v8u16, v8u16);
  v4i32 __builtin_msa_subsuu_s_w (v4u32, v4u32);
  v2i64 __builtin_msa_subsuu_s_d (v2u64, v2u64);

  v16i8 __builtin_msa_subv_b (v16i8, v16i8);
  v8i16 __builtin_msa_subv_h (v8i16, v8i16);
  v4i32 __builtin_msa_subv_w (v4i32, v4i32);
  v2i64 __builtin_msa_subv_d (v2i64, v2i64);

  v16i8 __builtin_msa_subvi_b (v16i8, imm0_31);
  v8i16 __builtin_msa_subvi_h (v8i16, imm0_31);
  v4i32 __builtin_msa_subvi_w (v4i32, imm0_31);
  v2i64 __builtin_msa_subvi_d (v2i64, imm0_31);

  v16i8 __builtin_msa_vshf_b (v16i8, v16i8, v16i8);
  v8i16 __builtin_msa_vshf_h (v8i16, v8i16, v8i16);
  v4i32 __builtin_msa_vshf_w (v4i32, v4i32, v4i32);
  v2i64 __builtin_msa_vshf_d (v2i64, v2i64, v2i64);

  v16u8 __builtin_msa_xor_v (v16u8, v16u8);

  v16u8 __builtin_msa_xori_b (v16u8, imm0_255);

.. _powerpc-atomic-memory-operation-functions:

PowerPC Atomic Memory Operation Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

ISA 3.0 of the PowerPC added new atomic memory operation (amo)
instructions.  GCC provides support for these instructions in 64-bit
environments.  All of the functions are declared in the include file
``amo.h``.

The functions supported are:

.. code-block:: c++

  #include <amo.h>

  uint32_t amo_lwat_add (uint32_t *, uint32_t);
  uint32_t amo_lwat_xor (uint32_t *, uint32_t);
  uint32_t amo_lwat_ior (uint32_t *, uint32_t);
  uint32_t amo_lwat_and (uint32_t *, uint32_t);
  uint32_t amo_lwat_umax (uint32_t *, uint32_t);
  uint32_t amo_lwat_umin (uint32_t *, uint32_t);
  uint32_t amo_lwat_swap (uint32_t *, uint32_t);

  int32_t amo_lwat_sadd (int32_t *, int32_t);
  int32_t amo_lwat_smax (int32_t *, int32_t);
  int32_t amo_lwat_smin (int32_t *, int32_t);
  int32_t amo_lwat_sswap (int32_t *, int32_t);

  uint64_t amo_ldat_add (uint64_t *, uint64_t);
  uint64_t amo_ldat_xor (uint64_t *, uint64_t);
  uint64_t amo_ldat_ior (uint64_t *, uint64_t);
  uint64_t amo_ldat_and (uint64_t *, uint64_t);
  uint64_t amo_ldat_umax (uint64_t *, uint64_t);
  uint64_t amo_ldat_umin (uint64_t *, uint64_t);
  uint64_t amo_ldat_swap (uint64_t *, uint64_t);

  int64_t amo_ldat_sadd (int64_t *, int64_t);
  int64_t amo_ldat_smax (int64_t *, int64_t);
  int64_t amo_ldat_smin (int64_t *, int64_t);
  int64_t amo_ldat_sswap (int64_t *, int64_t);

  void amo_stwat_add (uint32_t *, uint32_t);
  void amo_stwat_xor (uint32_t *, uint32_t);
  void amo_stwat_ior (uint32_t *, uint32_t);
  void amo_stwat_and (uint32_t *, uint32_t);
  void amo_stwat_umax (uint32_t *, uint32_t);
  void amo_stwat_umin (uint32_t *, uint32_t);

  void amo_stwat_sadd (int32_t *, int32_t);
  void amo_stwat_smax (int32_t *, int32_t);
  void amo_stwat_smin (int32_t *, int32_t);

  void amo_stdat_add (uint64_t *, uint64_t);
  void amo_stdat_xor (uint64_t *, uint64_t);
  void amo_stdat_ior (uint64_t *, uint64_t);
  void amo_stdat_and (uint64_t *, uint64_t);
  void amo_stdat_umax (uint64_t *, uint64_t);
  void amo_stdat_umin (uint64_t *, uint64_t);

  void amo_stdat_sadd (int64_t *, int64_t);
  void amo_stdat_smax (int64_t *, int64_t);
  void amo_stdat_smin (int64_t *, int64_t);

.. _x86-transactional-memory-intrinsics:

x86 Transactional Memory Intrinsics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These hardware transactional memory intrinsics for x86 allow you to use
memory transactions with RTM (Restricted Transactional Memory).
This support is enabled with the :option:`-mrtm` option.
For using HLE (Hardware Lock Elision) see 
x86 specific memory model extensions for transactional memory instead.

A memory transaction commits all changes to memory in an atomic way,
as visible to other threads. If the transaction fails it is rolled back
and all side effects discarded.

Generally there is no guarantee that a memory transaction ever succeeds
and suitable fallback code always needs to be supplied.

.. function:: unsigned _xbegin ()

  Start a RTM (Restricted Transactional Memory) transaction. 
  Returns ``_XBEGIN_STARTED`` when the transaction
  started successfully (note this is not 0, so the constant has to be 
  explicitly tested).  

  If the transaction aborts, all side effects
  are undone and an abort code encoded as a bit mask is returned.
  The following macros are defined:

  ``_XABORT_EXPLICIT``
    Transaction was explicitly aborted with ``_xabort``.  The parameter passed
    to ``_xabort`` is available with ``_XABORT_CODE(status)``.

  ``_XABORT_RETRY``
    Transaction retry is possible.

  ``_XABORT_CONFLICT``
    Transaction abort due to a memory conflict with another thread.

  ``_XABORT_CAPACITY``
    Transaction abort due to the transaction using too much memory.

  ``_XABORT_DEBUG``
    Transaction abort due to a debug trap.

  ``_XABORT_NESTED``
    Transaction abort in an inner nested transaction.

    There is no guarantee
  any transaction ever succeeds, so there always needs to be a valid
  fallback path.

.. function:: void _xend ()

  Commit the current transaction. When no transaction is active this faults.
  All memory side effects of the transaction become visible
  to other threads in an atomic manner.

.. function:: int _xtest ()

  Return a nonzero value if a transaction is currently active, otherwise 0.

.. function:: void _xabort (status)

  Abort the current transaction. When no transaction is active this is a no-op.
  The :samp:`{status}` is an 8-bit constant; its value is encoded in the return 
  value from ``_xbegin``.

Here is an example showing handling for ``_XABORT_RETRY``
and a fallback path for other failures:

.. code-block:: c++

  #include <immintrin.h>

  int n_tries, max_tries;
  unsigned status = _XABORT_EXPLICIT;
  ...

  for (n_tries = 0; n_tries < max_tries; n_tries++) 
    {
      status = _xbegin ();
      if (status == _XBEGIN_STARTED || !(status & _XABORT_RETRY))
        break;
    }
  if (status == _XBEGIN_STARTED) 
    {
      ... transaction code...
      _xend ();
    } 
  else 
    {
      ... non-transactional fallback path...
    }

Note that, in most cases, the transactional and non-transactional code
must synchronize together to ensure consistency.

.. _x86-control-flow-protection-intrinsics:

x86 Control-Flow Protection Intrinsics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: ret_type _get_ssp (void)

  Get the current value of shadow stack pointer if shadow stack support
  from Intel CET is enabled in the hardware or ``0`` otherwise.
  The ``ret_type`` is ``unsigned long long`` for 64-bit targets 
  and ``unsigned int`` for 32-bit targets.

.. function:: void _inc_ssp (unsigned int)

  Increment the current shadow stack pointer by the size specified by the
  function argument.  The argument is masked to a byte value for security
  reasons, so to increment by more than 255 bytes you must call the function
  multiple times.

The shadow stack unwind code looks like:

.. code-block:: c++

  #include <immintrin.h>

  /* Unwind the shadow stack for EH.  */
  #define _Unwind_Frames_Extra(x)       \
    do                                  \
      {                                \
        _Unwind_Word ssp = _get_ssp (); \
        if (ssp != 0)                   \
          {                            \
            _Unwind_Word tmp = (x);     \
            while (tmp > 255)           \
              {                        \
                _inc_ssp (tmp);         \
                tmp -= 255;             \
              }                        \
            _inc_ssp (tmp);             \
          }                            \
      }                                \
      while (0)

This code runs unconditionally on all 64-bit processors.  For 32-bit
processors the code runs on those that support multi-byte NOP instructions.

