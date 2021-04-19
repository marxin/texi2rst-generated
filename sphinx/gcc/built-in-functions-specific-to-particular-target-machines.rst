.. _target-builtins:

Built-in Functions Specific to Particular Target Machines
*********************************************************

On some target machines, GCC supports many built-in functions specific
to those machines.  Generally these generate calls to specific machine
instructions, but allow the compiler to schedule those calls.

.. toctree::

   <aarch64-built-in-functions>
   <alpha-built-in-functions>
   <altera-nios-ii-built-in-functions>
   <arc-built-in-functions>
   <arc-simd-built-in-functions>
   <arm-iwmmxt-built-in-functions>
   <arm-c-language-extensions-(acle)>
   <arm-floating-point-status-and-control-intrinsics>
   <arm-armv8-m-security-extensions>
   <avr-built-in-functions>
   <blackfin-built-in-functions>
   <bpf-built-in-functions>
   <fr-v-built-in-functions>
   <mips-dsp-built-in-functions>
   <mips-paired-single-support>
   <mips-loongson-built-in-functions>
   <mips-simd-architecture-(msa)-support>
   <other-mips-built-in-functions>
   <msp430-built-in-functions>
   <nds32-built-in-functions>
   <picochip-built-in-functions>
   <basic-powerpc-built-in-functions>
   <powerpc-altivec-vsx-built-in-functions>
   <powerpc-hardware-transactional-memory-built-in-functions>
   <powerpc-atomic-memory-operation-functions>
   <powerpc-matrix-multiply-assist-built-in-functions>
   <pru-built-in-functions>
   <risc-v-built-in-functions>
   <rx-built-in-functions>
   <s-390-system-z-built-in-functions>
   <sh-built-in-functions>
   <sparc-vis-built-in-functions>
   <ti-c6x-built-in-functions>
   <tile-gx-built-in-functions>
   <tilepro-built-in-functions>
   <x86-built-in-functions>
   <x86-transactional-memory-intrinsics>
   <x86-control-flow-protection-intrinsics>

.. _aarch64-built-in-functions:

AArch64 Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the AArch64 family of
processors.

.. code-block:: c++

  unsigned int __builtin_aarch64_get_fpcr ()
  void __builtin_aarch64_set_fpcr (unsigned int)
  unsigned int __builtin_aarch64_get_fpsr ()
  void __builtin_aarch64_set_fpsr (unsigned int)

  unsigned long long __builtin_aarch64_get_fpcr64 ()
  void __builtin_aarch64_set_fpcr64 (unsigned long long)
  unsigned long long __builtin_aarch64_get_fpsr64 ()
  void __builtin_aarch64_set_fpsr64 (unsigned long long)

.. _alpha-built-in-functions:

Alpha Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the Alpha family of
processors, depending on the command-line switches used.

The following built-in functions are always available.  They
all generate the machine instruction that is part of the name.

.. code-block:: c++

  long __builtin_alpha_implver (void)
  long __builtin_alpha_rpcc (void)
  long __builtin_alpha_amask (long)
  long __builtin_alpha_cmpbge (long, long)
  long __builtin_alpha_extbl (long, long)
  long __builtin_alpha_extwl (long, long)
  long __builtin_alpha_extll (long, long)
  long __builtin_alpha_extql (long, long)
  long __builtin_alpha_extwh (long, long)
  long __builtin_alpha_extlh (long, long)
  long __builtin_alpha_extqh (long, long)
  long __builtin_alpha_insbl (long, long)
  long __builtin_alpha_inswl (long, long)
  long __builtin_alpha_insll (long, long)
  long __builtin_alpha_insql (long, long)
  long __builtin_alpha_inswh (long, long)
  long __builtin_alpha_inslh (long, long)
  long __builtin_alpha_insqh (long, long)
  long __builtin_alpha_mskbl (long, long)
  long __builtin_alpha_mskwl (long, long)
  long __builtin_alpha_mskll (long, long)
  long __builtin_alpha_mskql (long, long)
  long __builtin_alpha_mskwh (long, long)
  long __builtin_alpha_msklh (long, long)
  long __builtin_alpha_mskqh (long, long)
  long __builtin_alpha_umulh (long, long)
  long __builtin_alpha_zap (long, long)
  long __builtin_alpha_zapnot (long, long)

The following built-in functions are always with :option:`-mmax`
or :option:`-mcpu`:samp:`={cpu}` where :samp:`{cpu}` is ``pca56`` or
later.  They all generate the machine instruction that is part
of the name.

.. code-block:: c++

  long __builtin_alpha_pklb (long)
  long __builtin_alpha_pkwb (long)
  long __builtin_alpha_unpkbl (long)
  long __builtin_alpha_unpkbw (long)
  long __builtin_alpha_minub8 (long, long)
  long __builtin_alpha_minsb8 (long, long)
  long __builtin_alpha_minuw4 (long, long)
  long __builtin_alpha_minsw4 (long, long)
  long __builtin_alpha_maxub8 (long, long)
  long __builtin_alpha_maxsb8 (long, long)
  long __builtin_alpha_maxuw4 (long, long)
  long __builtin_alpha_maxsw4 (long, long)
  long __builtin_alpha_perr (long, long)

The following built-in functions are always with :option:`-mcix`
or :option:`-mcpu`:samp:`={cpu}` where :samp:`{cpu}` is ``ev67`` or
later.  They all generate the machine instruction that is part
of the name.

.. code-block:: c++

  long __builtin_alpha_cttz (long)
  long __builtin_alpha_ctlz (long)
  long __builtin_alpha_ctpop (long)

The following built-in functions are available on systems that use the OSF/1
PALcode.  Normally they invoke the ``rduniq`` and ``wruniq``
PAL calls, but when invoked with :option:`-mtls-kernel` , they invoke
``rdval`` and ``wrval``.

.. code-block:: c++

  void *__builtin_thread_pointer (void)
  void __builtin_set_thread_pointer (void *)

.. _altera-nios-ii-built-in-functions:

Altera Nios II Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the Altera Nios II
family of processors.

The following built-in functions are always available.  They
all generate the machine instruction that is part of the name.

.. code-block:: c++

  int __builtin_ldbio (volatile const void *)
  int __builtin_ldbuio (volatile const void *)
  int __builtin_ldhio (volatile const void *)
  int __builtin_ldhuio (volatile const void *)
  int __builtin_ldwio (volatile const void *)
  void __builtin_stbio (volatile void *, int)
  void __builtin_sthio (volatile void *, int)
  void __builtin_stwio (volatile void *, int)
  void __builtin_sync (void)
  int __builtin_rdctl (int) 
  int __builtin_rdprs (int, int)
  void __builtin_wrctl (int, int)
  void __builtin_flushd (volatile void *)
  void __builtin_flushda (volatile void *)
  int __builtin_wrpie (int);
  void __builtin_eni (int);
  int __builtin_ldex (volatile const void *)
  int __builtin_stex (volatile void *, int)
  int __builtin_ldsex (volatile const void *)
  int __builtin_stsex (volatile void *, int)

The following built-in functions are always available.  They
all generate a Nios II Custom Instruction. The name of the
function represents the types that the function takes and
returns. The letter before the ``n`` is the return type
or void if absent. The ``n`` represents the first parameter
to all the custom instructions, the custom instruction number.
The two letters after the ``n`` represent the up to two
parameters to the function.

The letters represent the following data types:

``<no letter>``
  ``void`` for return type and no parameter for parameter types.

``i``
  ``int`` for return type and parameter type

``f``
  ``float`` for return type and parameter type

``p``
  ``void *`` for return type and parameter type

  And the function names are:

.. code-block:: c++

  void __builtin_custom_n (void)
  void __builtin_custom_ni (int)
  void __builtin_custom_nf (float)
  void __builtin_custom_np (void *)
  void __builtin_custom_nii (int, int)
  void __builtin_custom_nif (int, float)
  void __builtin_custom_nip (int, void *)
  void __builtin_custom_nfi (float, int)
  void __builtin_custom_nff (float, float)
  void __builtin_custom_nfp (float, void *)
  void __builtin_custom_npi (void *, int)
  void __builtin_custom_npf (void *, float)
  void __builtin_custom_npp (void *, void *)
  int __builtin_custom_in (void)
  int __builtin_custom_ini (int)
  int __builtin_custom_inf (float)
  int __builtin_custom_inp (void *)
  int __builtin_custom_inii (int, int)
  int __builtin_custom_inif (int, float)
  int __builtin_custom_inip (int, void *)
  int __builtin_custom_infi (float, int)
  int __builtin_custom_inff (float, float)
  int __builtin_custom_infp (float, void *)
  int __builtin_custom_inpi (void *, int)
  int __builtin_custom_inpf (void *, float)
  int __builtin_custom_inpp (void *, void *)
  float __builtin_custom_fn (void)
  float __builtin_custom_fni (int)
  float __builtin_custom_fnf (float)
  float __builtin_custom_fnp (void *)
  float __builtin_custom_fnii (int, int)
  float __builtin_custom_fnif (int, float)
  float __builtin_custom_fnip (int, void *)
  float __builtin_custom_fnfi (float, int)
  float __builtin_custom_fnff (float, float)
  float __builtin_custom_fnfp (float, void *)
  float __builtin_custom_fnpi (void *, int)
  float __builtin_custom_fnpf (void *, float)
  float __builtin_custom_fnpp (void *, void *)
  void * __builtin_custom_pn (void)
  void * __builtin_custom_pni (int)
  void * __builtin_custom_pnf (float)
  void * __builtin_custom_pnp (void *)
  void * __builtin_custom_pnii (int, int)
  void * __builtin_custom_pnif (int, float)
  void * __builtin_custom_pnip (int, void *)
  void * __builtin_custom_pnfi (float, int)
  void * __builtin_custom_pnff (float, float)
  void * __builtin_custom_pnfp (float, void *)
  void * __builtin_custom_pnpi (void *, int)
  void * __builtin_custom_pnpf (void *, float)
  void * __builtin_custom_pnpp (void *, void *)

.. _arc-built-in-functions:

ARC Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

The following built-in functions are provided for ARC targets.  The
built-ins generate the corresponding assembly instructions.  In the
examples given below, the generated code often requires an operand or
result to be in a register.  Where necessary further code will be
generated to ensure this is true, but for brevity this is not
described in each case.

*Note:* Using a built-in to generate an instruction not supported
by a target may cause problems. At present the compiler is not
guaranteed to detect such misuse, and as a result an internal compiler
error may be generated.

.. function:: int __builtin_arc_aligned(void *val,int alignval)

  Return 1 if :samp:`{val}` is known to have the byte alignment given
  by :samp:`{alignval}` , otherwise return 0.
  Note that this is different from

  .. code-block:: c++

    __alignof__(*(char *)val) >= alignval

  because __alignof__ sees only the type of the dereference, whereas
  __builtin_arc_align uses alignment information from the pointer
  as well as from the pointed-to type.
  The information available will depend on optimization level.

.. function:: void __builtin_arc_brk(void )

  Generates

  .. code-block:: c++

    brk

.. function:: unsigned int __builtin_arc_core_read(unsigned intregno)

  The operand is the number of a register to be read.  Generates:

  .. code-block:: c++

    mov  dest, rregno

  where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_core_write(unsigned intregno,unsigned intval)

  The first operand is the number of a register to be written, the
  second operand is a compile time constant to write into that
  register.  Generates:

  .. code-block:: c++

    mov  rregno, val

.. function:: int __builtin_arc_divaw(int a,int b)

  Only available if either :option:`-mcpu=ARC700` or :option:`-meA` is set.
  Generates:

  .. code-block:: c++

    divaw  dest, a, b

  where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_flag(unsigned inta)

  Generates

  .. code-block:: c++

    flag  a

.. function:: unsigned int __builtin_arc_lr(unsigned intauxr)

  The operand, :samp:`{auxv}` , is the address of an auxiliary register and
  must be a compile time constant.  Generates:

  .. code-block:: c++

    lr  dest, [auxr]

  Where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_mul64(int a,int b)

  Only available with :option:`-mmul64`.  Generates:

  .. code-block:: c++

    mul64  a, b

.. function:: void __builtin_arc_mulu64(unsigned inta,unsigned intb)

  Only available with :option:`-mmul64`.  Generates:

  .. code-block:: c++

    mulu64  a, b

.. function:: void __builtin_arc_nop(void )

  Generates:

  .. code-block:: c++

    nop

.. function:: int __builtin_arc_norm(int src)

  Only valid if the :samp:`norm` instruction is available through the
  :option:`-mnorm` option or by default with :option:`-mcpu=ARC700`.
  Generates:

  .. code-block:: c++

    norm  dest, src

  Where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: short int __builtin_arc_normw(short intsrc)

  Only valid if the :samp:`normw` instruction is available through the
  :option:`-mnorm` option or by default with :option:`-mcpu=ARC700`.
  Generates:

  .. code-block:: c++

    normw  dest, src

  Where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_rtie(void )

  Generates:

  .. code-block:: c++

    rtie

.. function:: void __builtin_arc_sleep(int a

  Generates:

  .. code-block:: c++

    sleep  a

.. function:: void __builtin_arc_sr(unsigned intauxr,unsigned intval)

  The first argument, :samp:`{auxv}` , is the address of an auxiliary
  register, the second argument, :samp:`{val}` , is a compile time constant
  to be written to the register.  Generates:

  .. code-block:: c++

    sr  auxr, [val]

.. function:: int __builtin_arc_swap(int src)

  Only valid with :option:`-mswap`.  Generates:

  .. code-block:: c++

    swap  dest, src

  Where the value in :samp:`{dest}` will be the result returned from the
  built-in.

.. function:: void __builtin_arc_swi(void )

  Generates:

  .. code-block:: c++

    swi

.. function:: void __builtin_arc_sync(void )

  Only available with :option:`-mcpu=ARC700`.  Generates:

  .. code-block:: c++

    sync

.. function:: void __builtin_arc_trap_s(unsigned intc)

  Only available with :option:`-mcpu=ARC700`.  Generates:

  .. code-block:: c++

    trap_s  c

.. function:: void __builtin_arc_unimp_s(void )

  Only available with :option:`-mcpu=ARC700`.  Generates:

  .. code-block:: c++

    unimp_s

The instructions generated by the following builtins are not
considered as candidates for scheduling.  They are not moved around by
the compiler during scheduling, and thus can be expected to appear
where they are put in the C code:

.. code-block:: c++

  __builtin_arc_brk()
  __builtin_arc_core_read()
  __builtin_arc_core_write()
  __builtin_arc_flag()
  __builtin_arc_lr()
  __builtin_arc_sleep()
  __builtin_arc_sr()
  __builtin_arc_swi()

.. _arc-simd-built-in-functions:

ARC SIMD Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

SIMD builtins provided by the compiler can be used to generate the
vector instructions.  This section describes the available builtins
and their usage in programs.  With the :option:`-msimd` option, the
compiler provides 128-bit vector types, which can be specified using
the ``vector_size`` attribute.  The header file arc-simd.h
can be included to use the following predefined types:

.. code-block:: c++

  typedef int __v4si   __attribute__((vector_size(16)));
  typedef short __v8hi __attribute__((vector_size(16)));

These types can be used to define 128-bit variables.  The built-in
functions listed in the following section can be used on these
variables to generate the vector operations.

For all builtins, ``__builtin_arc_someinsn``, the header file
arc-simd.h also provides equivalent macros called
``_someinsn`` that can be used for programming ease and
improved readability.  The following macros for DMA control are also
provided:

.. code-block:: c++

  #define _setup_dma_in_channel_reg _vdiwr
  #define _setup_dma_out_channel_reg _vdowr

The following is a complete list of all the SIMD built-ins provided
for ARC, grouped by calling signature.

The following take two ``__v8hi`` arguments and return a
``__v8hi`` result:

.. code-block:: c++

  __v8hi __builtin_arc_vaddaw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vaddw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vand (__v8hi, __v8hi)
  __v8hi __builtin_arc_vandaw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vavb (__v8hi, __v8hi)
  __v8hi __builtin_arc_vavrb (__v8hi, __v8hi)
  __v8hi __builtin_arc_vbic (__v8hi, __v8hi)
  __v8hi __builtin_arc_vbicaw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vdifaw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vdifw (__v8hi, __v8hi)
  __v8hi __builtin_arc_veqw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vh264f (__v8hi, __v8hi)
  __v8hi __builtin_arc_vh264ft (__v8hi, __v8hi)
  __v8hi __builtin_arc_vh264fw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vlew (__v8hi, __v8hi)
  __v8hi __builtin_arc_vltw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmaxaw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmaxw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vminaw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vminw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr1aw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr1w (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr2aw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr2w (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr3aw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr3w (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr4aw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr4w (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr5aw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr5w (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr6aw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr6w (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr7aw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmr7w (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmrb (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmulaw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmulfaw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmulfw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vmulw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vnew (__v8hi, __v8hi)
  __v8hi __builtin_arc_vor (__v8hi, __v8hi)
  __v8hi __builtin_arc_vsubaw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vsubw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vsummw (__v8hi, __v8hi)
  __v8hi __builtin_arc_vvc1f (__v8hi, __v8hi)
  __v8hi __builtin_arc_vvc1ft (__v8hi, __v8hi)
  __v8hi __builtin_arc_vxor (__v8hi, __v8hi)
  __v8hi __builtin_arc_vxoraw (__v8hi, __v8hi)

The following take one ``__v8hi`` and one ``int`` argument and return a
``__v8hi`` result:

.. code-block:: c++

  __v8hi __builtin_arc_vbaddw (__v8hi, int)
  __v8hi __builtin_arc_vbmaxw (__v8hi, int)
  __v8hi __builtin_arc_vbminw (__v8hi, int)
  __v8hi __builtin_arc_vbmulaw (__v8hi, int)
  __v8hi __builtin_arc_vbmulfw (__v8hi, int)
  __v8hi __builtin_arc_vbmulw (__v8hi, int)
  __v8hi __builtin_arc_vbrsubw (__v8hi, int)
  __v8hi __builtin_arc_vbsubw (__v8hi, int)

The following take one ``__v8hi`` argument and one ``int`` argument which
must be a 3-bit compile time constant indicating a register number
I0-I7.  They return a ``__v8hi`` result.

.. code-block:: c++

  __v8hi __builtin_arc_vasrw (__v8hi, const int)
  __v8hi __builtin_arc_vsr8 (__v8hi, const int)
  __v8hi __builtin_arc_vsr8aw (__v8hi, const int)

The following take one ``__v8hi`` argument and one ``int``
argument which must be a 6-bit compile time constant.  They return a
``__v8hi`` result.

.. code-block:: c++

  __v8hi __builtin_arc_vasrpwbi (__v8hi, const int)
  __v8hi __builtin_arc_vasrrpwbi (__v8hi, const int)
  __v8hi __builtin_arc_vasrrwi (__v8hi, const int)
  __v8hi __builtin_arc_vasrsrwi (__v8hi, const int)
  __v8hi __builtin_arc_vasrwi (__v8hi, const int)
  __v8hi __builtin_arc_vsr8awi (__v8hi, const int)
  __v8hi __builtin_arc_vsr8i (__v8hi, const int)

The following take one ``__v8hi`` argument and one ``int`` argument which
must be a 8-bit compile time constant.  They return a ``__v8hi``
result.

.. code-block:: c++

  __v8hi __builtin_arc_vd6tapf (__v8hi, const int)
  __v8hi __builtin_arc_vmvaw (__v8hi, const int)
  __v8hi __builtin_arc_vmvw (__v8hi, const int)
  __v8hi __builtin_arc_vmvzw (__v8hi, const int)

The following take two ``int`` arguments, the second of which which
must be a 8-bit compile time constant.  They return a ``__v8hi``
result:

.. code-block:: c++

  __v8hi __builtin_arc_vmovaw (int, const int)
  __v8hi __builtin_arc_vmovw (int, const int)
  __v8hi __builtin_arc_vmovzw (int, const int)

The following take a single ``__v8hi`` argument and return a
``__v8hi`` result:

.. code-block:: c++

  __v8hi __builtin_arc_vabsaw (__v8hi)
  __v8hi __builtin_arc_vabsw (__v8hi)
  __v8hi __builtin_arc_vaddsuw (__v8hi)
  __v8hi __builtin_arc_vexch1 (__v8hi)
  __v8hi __builtin_arc_vexch2 (__v8hi)
  __v8hi __builtin_arc_vexch4 (__v8hi)
  __v8hi __builtin_arc_vsignw (__v8hi)
  __v8hi __builtin_arc_vupbaw (__v8hi)
  __v8hi __builtin_arc_vupbw (__v8hi)
  __v8hi __builtin_arc_vupsbaw (__v8hi)
  __v8hi __builtin_arc_vupsbw (__v8hi)

The following take two ``int`` arguments and return no result:

.. code-block:: c++

  void __builtin_arc_vdirun (int, int)
  void __builtin_arc_vdorun (int, int)

The following take two ``int`` arguments and return no result.  The
first argument must a 3-bit compile time constant indicating one of
the DR0-DR7 DMA setup channels:

.. code-block:: c++

  void __builtin_arc_vdiwr (const int, int)
  void __builtin_arc_vdowr (const int, int)

The following take an ``int`` argument and return no result:

.. code-block:: c++

  void __builtin_arc_vendrec (int)
  void __builtin_arc_vrec (int)
  void __builtin_arc_vrecrun (int)
  void __builtin_arc_vrun (int)

The following take a ``__v8hi`` argument and two ``int``
arguments and return a ``__v8hi`` result.  The second argument must
be a 3-bit compile time constants, indicating one the registers I0-I7,
and the third argument must be an 8-bit compile time constant.

*Note:* Although the equivalent hardware instructions do not take
an SIMD register as an operand, these builtins overwrite the relevant
bits of the ``__v8hi`` register provided as the first argument with
the value loaded from the ``[Ib, u8]`` location in the SDM.

.. code-block:: c++

  __v8hi __builtin_arc_vld32 (__v8hi, const int, const int)
  __v8hi __builtin_arc_vld32wh (__v8hi, const int, const int)
  __v8hi __builtin_arc_vld32wl (__v8hi, const int, const int)
  __v8hi __builtin_arc_vld64 (__v8hi, const int, const int)

The following take two ``int`` arguments and return a ``__v8hi``
result.  The first argument must be a 3-bit compile time constants,
indicating one the registers I0-I7, and the second argument must be an
8-bit compile time constant.

.. code-block:: c++

  __v8hi __builtin_arc_vld128 (const int, const int)
  __v8hi __builtin_arc_vld64w (const int, const int)

The following take a ``__v8hi`` argument and two ``int``
arguments and return no result.  The second argument must be a 3-bit
compile time constants, indicating one the registers I0-I7, and the
third argument must be an 8-bit compile time constant.

.. code-block:: c++

  void __builtin_arc_vst128 (__v8hi, const int, const int)
  void __builtin_arc_vst64 (__v8hi, const int, const int)

The following take a ``__v8hi`` argument and three ``int``
arguments and return no result.  The second argument must be a 3-bit
compile-time constant, identifying the 16-bit sub-register to be
stored, the third argument must be a 3-bit compile time constants,
indicating one the registers I0-I7, and the fourth argument must be an
8-bit compile time constant.

.. code-block:: c++

  void __builtin_arc_vst16_n (__v8hi, const int, const int, const int)
  void __builtin_arc_vst32_n (__v8hi, const int, const int, const int)

.. _arm-iwmmxt-built-in-functions:

ARM iWMMXt Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the ARM family of
processors when the :option:`-mcpu=iwmmxt` switch is used:

.. code-block:: c++

  typedef int v2si __attribute__ ((vector_size (8)));
  typedef short v4hi __attribute__ ((vector_size (8)));
  typedef char v8qi __attribute__ ((vector_size (8)));

  int __builtin_arm_getwcgr0 (void)
  void __builtin_arm_setwcgr0 (int)
  int __builtin_arm_getwcgr1 (void)
  void __builtin_arm_setwcgr1 (int)
  int __builtin_arm_getwcgr2 (void)
  void __builtin_arm_setwcgr2 (int)
  int __builtin_arm_getwcgr3 (void)
  void __builtin_arm_setwcgr3 (int)
  int __builtin_arm_textrmsb (v8qi, int)
  int __builtin_arm_textrmsh (v4hi, int)
  int __builtin_arm_textrmsw (v2si, int)
  int __builtin_arm_textrmub (v8qi, int)
  int __builtin_arm_textrmuh (v4hi, int)
  int __builtin_arm_textrmuw (v2si, int)
  v8qi __builtin_arm_tinsrb (v8qi, int, int)
  v4hi __builtin_arm_tinsrh (v4hi, int, int)
  v2si __builtin_arm_tinsrw (v2si, int, int)
  long long __builtin_arm_tmia (long long, int, int)
  long long __builtin_arm_tmiabb (long long, int, int)
  long long __builtin_arm_tmiabt (long long, int, int)
  long long __builtin_arm_tmiaph (long long, int, int)
  long long __builtin_arm_tmiatb (long long, int, int)
  long long __builtin_arm_tmiatt (long long, int, int)
  int __builtin_arm_tmovmskb (v8qi)
  int __builtin_arm_tmovmskh (v4hi)
  int __builtin_arm_tmovmskw (v2si)
  long long __builtin_arm_waccb (v8qi)
  long long __builtin_arm_wacch (v4hi)
  long long __builtin_arm_waccw (v2si)
  v8qi __builtin_arm_waddb (v8qi, v8qi)
  v8qi __builtin_arm_waddbss (v8qi, v8qi)
  v8qi __builtin_arm_waddbus (v8qi, v8qi)
  v4hi __builtin_arm_waddh (v4hi, v4hi)
  v4hi __builtin_arm_waddhss (v4hi, v4hi)
  v4hi __builtin_arm_waddhus (v4hi, v4hi)
  v2si __builtin_arm_waddw (v2si, v2si)
  v2si __builtin_arm_waddwss (v2si, v2si)
  v2si __builtin_arm_waddwus (v2si, v2si)
  v8qi __builtin_arm_walign (v8qi, v8qi, int)
  long long __builtin_arm_wand(long long, long long)
  long long __builtin_arm_wandn (long long, long long)
  v8qi __builtin_arm_wavg2b (v8qi, v8qi)
  v8qi __builtin_arm_wavg2br (v8qi, v8qi)
  v4hi __builtin_arm_wavg2h (v4hi, v4hi)
  v4hi __builtin_arm_wavg2hr (v4hi, v4hi)
  v8qi __builtin_arm_wcmpeqb (v8qi, v8qi)
  v4hi __builtin_arm_wcmpeqh (v4hi, v4hi)
  v2si __builtin_arm_wcmpeqw (v2si, v2si)
  v8qi __builtin_arm_wcmpgtsb (v8qi, v8qi)
  v4hi __builtin_arm_wcmpgtsh (v4hi, v4hi)
  v2si __builtin_arm_wcmpgtsw (v2si, v2si)
  v8qi __builtin_arm_wcmpgtub (v8qi, v8qi)
  v4hi __builtin_arm_wcmpgtuh (v4hi, v4hi)
  v2si __builtin_arm_wcmpgtuw (v2si, v2si)
  long long __builtin_arm_wmacs (long long, v4hi, v4hi)
  long long __builtin_arm_wmacsz (v4hi, v4hi)
  long long __builtin_arm_wmacu (long long, v4hi, v4hi)
  long long __builtin_arm_wmacuz (v4hi, v4hi)
  v4hi __builtin_arm_wmadds (v4hi, v4hi)
  v4hi __builtin_arm_wmaddu (v4hi, v4hi)
  v8qi __builtin_arm_wmaxsb (v8qi, v8qi)
  v4hi __builtin_arm_wmaxsh (v4hi, v4hi)
  v2si __builtin_arm_wmaxsw (v2si, v2si)
  v8qi __builtin_arm_wmaxub (v8qi, v8qi)
  v4hi __builtin_arm_wmaxuh (v4hi, v4hi)
  v2si __builtin_arm_wmaxuw (v2si, v2si)
  v8qi __builtin_arm_wminsb (v8qi, v8qi)
  v4hi __builtin_arm_wminsh (v4hi, v4hi)
  v2si __builtin_arm_wminsw (v2si, v2si)
  v8qi __builtin_arm_wminub (v8qi, v8qi)
  v4hi __builtin_arm_wminuh (v4hi, v4hi)
  v2si __builtin_arm_wminuw (v2si, v2si)
  v4hi __builtin_arm_wmulsm (v4hi, v4hi)
  v4hi __builtin_arm_wmulul (v4hi, v4hi)
  v4hi __builtin_arm_wmulum (v4hi, v4hi)
  long long __builtin_arm_wor (long long, long long)
  v2si __builtin_arm_wpackdss (long long, long long)
  v2si __builtin_arm_wpackdus (long long, long long)
  v8qi __builtin_arm_wpackhss (v4hi, v4hi)
  v8qi __builtin_arm_wpackhus (v4hi, v4hi)
  v4hi __builtin_arm_wpackwss (v2si, v2si)
  v4hi __builtin_arm_wpackwus (v2si, v2si)
  long long __builtin_arm_wrord (long long, long long)
  long long __builtin_arm_wrordi (long long, int)
  v4hi __builtin_arm_wrorh (v4hi, long long)
  v4hi __builtin_arm_wrorhi (v4hi, int)
  v2si __builtin_arm_wrorw (v2si, long long)
  v2si __builtin_arm_wrorwi (v2si, int)
  v2si __builtin_arm_wsadb (v2si, v8qi, v8qi)
  v2si __builtin_arm_wsadbz (v8qi, v8qi)
  v2si __builtin_arm_wsadh (v2si, v4hi, v4hi)
  v2si __builtin_arm_wsadhz (v4hi, v4hi)
  v4hi __builtin_arm_wshufh (v4hi, int)
  long long __builtin_arm_wslld (long long, long long)
  long long __builtin_arm_wslldi (long long, int)
  v4hi __builtin_arm_wsllh (v4hi, long long)
  v4hi __builtin_arm_wsllhi (v4hi, int)
  v2si __builtin_arm_wsllw (v2si, long long)
  v2si __builtin_arm_wsllwi (v2si, int)
  long long __builtin_arm_wsrad (long long, long long)
  long long __builtin_arm_wsradi (long long, int)
  v4hi __builtin_arm_wsrah (v4hi, long long)
  v4hi __builtin_arm_wsrahi (v4hi, int)
  v2si __builtin_arm_wsraw (v2si, long long)
  v2si __builtin_arm_wsrawi (v2si, int)
  long long __builtin_arm_wsrld (long long, long long)
  long long __builtin_arm_wsrldi (long long, int)
  v4hi __builtin_arm_wsrlh (v4hi, long long)
  v4hi __builtin_arm_wsrlhi (v4hi, int)
  v2si __builtin_arm_wsrlw (v2si, long long)
  v2si __builtin_arm_wsrlwi (v2si, int)
  v8qi __builtin_arm_wsubb (v8qi, v8qi)
  v8qi __builtin_arm_wsubbss (v8qi, v8qi)
  v8qi __builtin_arm_wsubbus (v8qi, v8qi)
  v4hi __builtin_arm_wsubh (v4hi, v4hi)
  v4hi __builtin_arm_wsubhss (v4hi, v4hi)
  v4hi __builtin_arm_wsubhus (v4hi, v4hi)
  v2si __builtin_arm_wsubw (v2si, v2si)
  v2si __builtin_arm_wsubwss (v2si, v2si)
  v2si __builtin_arm_wsubwus (v2si, v2si)
  v4hi __builtin_arm_wunpckehsb (v8qi)
  v2si __builtin_arm_wunpckehsh (v4hi)
  long long __builtin_arm_wunpckehsw (v2si)
  v4hi __builtin_arm_wunpckehub (v8qi)
  v2si __builtin_arm_wunpckehuh (v4hi)
  long long __builtin_arm_wunpckehuw (v2si)
  v4hi __builtin_arm_wunpckelsb (v8qi)
  v2si __builtin_arm_wunpckelsh (v4hi)
  long long __builtin_arm_wunpckelsw (v2si)
  v4hi __builtin_arm_wunpckelub (v8qi)
  v2si __builtin_arm_wunpckeluh (v4hi)
  long long __builtin_arm_wunpckeluw (v2si)
  v8qi __builtin_arm_wunpckihb (v8qi, v8qi)
  v4hi __builtin_arm_wunpckihh (v4hi, v4hi)
  v2si __builtin_arm_wunpckihw (v2si, v2si)
  v8qi __builtin_arm_wunpckilb (v8qi, v8qi)
  v4hi __builtin_arm_wunpckilh (v4hi, v4hi)
  v2si __builtin_arm_wunpckilw (v2si, v2si)
  long long __builtin_arm_wxor (long long, long long)
  long long __builtin_arm_wzero ()

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
Coprocessor intrinsics, all from arm_acle.h.  The ARM back end's 16-bit
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

.. _avr-built-in-functions:

AVR Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

For each built-in function for AVR, there is an equally named,
uppercase built-in macro defined. That way users can easily query if
or if not a specific built-in is implemented or not. For example, if
``__builtin_avr_nop`` is available the macro
``__BUILTIN_AVR_NOP`` is defined to ``1`` and undefined otherwise.

``void __builtin_avr_nop (void)`` ``void __builtin_avr_sei (void)`` ``void __builtin_avr_cli (void)`` ``void __builtin_avr_sleep (void)`` ``void __builtin_avr_wdr (void)`` ``unsigned char __builtin_avr_swap (unsigned char)`` ``unsigned int __builtin_avr_fmul (unsigned char, unsigned char)`` ``int __builtin_avr_fmuls (char, char)`` ``int __builtin_avr_fmulsu (char, unsigned char)``
  These built-in functions map to the respective machine
  instruction, i.e. ``nop``, ``sei``, ``cli``, ``sleep``,
  ``wdr``, ``swap``, ``fmul``, ``fmuls``
  resp. ``fmulsu``. The three ``fmul*`` built-ins are implemented
  as library call if no hardware multiplier is available.

``void __builtin_avr_delay_cycles (unsigned long ticks)``
  Delay execution for :samp:`{ticks}` cycles. Note that this
  built-in does not take into account the effect of interrupts that
  might increase delay time. :samp:`{ticks}` must be a compile-time
  integer constant; delays with a variable number of cycles are not supported.

``char __builtin_avr_flash_segment (const __memx void*)``
  This built-in takes a byte address to the 24-bit
  AVR Named Address Spacesaddress space ``__memx`` and returns
  the number of the flash segment (the 64 KiB chunk) where the address
  points to.  Counting starts at ``0``.
  If the address does not point to flash memory, return ``-1``.

``uint8_t __builtin_avr_insert_bits (uint32_t map, uint8_t bits, uint8_t val)``
  Insert bits from :samp:`{bits}` into :samp:`{val}` and return the resulting
  value. The nibbles of :samp:`{map}` determine how the insertion is
  performed: Let :samp:`{X}` be the :samp:`{n}` -th nibble of :samp:`{map}`

  * If :samp:`{X}` is ``0xf``,
    then the :samp:`{n}` -th bit of :samp:`{val}` is returned unaltered.

  * If X is in the range 0...7,
    then the :samp:`{n}` -th result bit is set to the :samp:`{X}` -th bit of :samp:`{bits}`

  * If X is in the range 8...``0xe``,
    then the :samp:`{n}` -th result bit is undefined.

  One typical use case for this built-in is adjusting input and
  output values to non-contiguous port layouts. Some examples:

  .. code-block:: c++

    // same as val, bits is unused
    __builtin_avr_insert_bits (0xffffffff, bits, val)

  .. code-block:: c++

    // same as bits, val is unused
    __builtin_avr_insert_bits (0x76543210, bits, val)

  .. code-block:: c++

    // same as rotating bits by 4
    __builtin_avr_insert_bits (0x32107654, bits, 0)

  .. code-block:: c++

    // high nibble of result is the high nibble of val
    // low nibble of result is the low nibble of bits
    __builtin_avr_insert_bits (0xffff3210, bits, val)

  .. code-block:: c++

    // reverse the bit order of bits
    __builtin_avr_insert_bits (0x01234567, bits, 0)

``void __builtin_avr_nops (unsigned count)``
  Insert :samp:`{count}` ``NOP`` instructions.
  The number of instructions must be a compile-time integer constant.

  There are many more AVR-specific built-in functions that are used to
implement the ISO/IEC TR 18037 'Embedded C' fixed-point functions of
section 7.18a.6.  You don't need to use these built-ins directly.
Instead, use the declarations as supplied by the ``stdfix.h`` header
with GNU-C99:

.. code-block:: c++

  #include <stdfix.h>

  // Re-interpret the bit representation of unsigned 16-bit
  // integer uval as Q-format 0.16 value.
  unsigned fract get_bits (uint_ur_t uval)
  {
      return urbits (uval);
  }

.. _blackfin-built-in-functions:

Blackfin Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently, there are two Blackfin-specific built-in functions.  These are
used for generating ``CSYNC`` and ``SSYNC`` machine insns without
using inline assembly; by using these built-in functions the compiler can
automatically add workarounds for hardware errata involving these
instructions.  These functions are named as follows:

.. code-block:: c++

  void __builtin_bfin_csync (void)
  void __builtin_bfin_ssync (void)

.. _bpf-built-in-functions:

BPF Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

The following built-in functions are available for eBPF targets.

.. function:: unsigned longlong __builtin_bpf_load_byte(unsigned longlong offset)

  Load a byte from the ``struct sk_buff`` packet data pointed by the register ``%r6`` and return it.

.. function:: unsigned longlong __builtin_bpf_load_half(unsigned longlong offset)

  Load 16-bits from the ``struct sk_buff`` packet data pointed by the register ``%r6`` and return it.

.. function:: unsigned longlong __builtin_bpf_load_word(unsigned longlong offset)

  Load 32-bits from the ``struct sk_buff`` packet data pointed by the register ``%r6`` and return it.

.. _fr-v-built-in-functions:

FR-V Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^

GCC provides many FR-V-specific built-in functions.  In general,
these functions are intended to be compatible with those described
by FR-V Family, Softune C/C++ Compiler Manual (V6), Fujitsu
Semiconductor.  The two exceptions are ``__MDUNPACKH`` and
``__MBTOHE``, the GCC forms of which pass 128-bit values by
pointer rather than by value.

Most of the functions are named after specific FR-V instructions.
Such functions are said to be 'directly mapped' and are summarized
here in tabular form.

.. toctree::

   <argument-types>
   <directly-mapped-integer-functions>
   <directly-mapped-media-functions>
   <raw-read-write-functions>
   <other-built-in-functions>

.. _argument-types:

Argument Types
~~~~~~~~~~~~~~

The arguments to the built-in functions can be divided into three groups:
register numbers, compile-time constants and run-time values.  In order
to make this classification clear at a glance, the arguments and return
values are given the following pseudo types:

===========  ======================  =========  =======================
Pseudo type  Real C type             Constant?  Description
===========  ======================  =========  =======================
``uh``       ``unsigned short``      No         an unsigned halfword
``uw1``      ``unsigned int``        No         an unsigned word
``sw1``      ``int``                 No         a signed word
``uw2``      ``unsigned long long``  No         an unsigned doubleword
``sw2``      ``long long``           No         a signed doubleword
``const``    ``int``                 Yes        an integer constant
``acc``      ``int``                 Yes        an ACC register number
``iacc``     ``int``                 Yes        an IACC register number
===========  ======================  =========  =======================
These pseudo types are not defined by GCC, they are simply a notational
convenience used in this manual.

Arguments of type ``uh``, ``uw1``, ``sw1``, ``uw2``
and ``sw2`` are evaluated at run time.  They correspond to
register operands in the underlying FR-V instructions.

``const`` arguments represent immediate operands in the underlying
FR-V instructions.  They must be compile-time constants.

``acc`` arguments are evaluated at compile time and specify the number
of an accumulator register.  For example, an ``acc`` argument of 2
selects the ACC2 register.

``iacc`` arguments are similar to ``acc`` arguments but specify the
number of an IACC register.  See see :ref:`other-built-in-functions`
for more details.

.. _directly-mapped-integer-functions:

Directly-Mapped Integer Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The functions listed below map directly to FR-V I-type instructions.

===========================  ======================  ===============
Function prototype           Example usage           Assembly output
===========================  ======================  ===============
``sw1 __ADDSS (sw1, sw1)``   ``c = __ADDSS (a, b)``  ``ADDSS a,b,c``
``sw1 __SCAN (sw1, sw1)``    ``c = __SCAN (a, b)``   ``SCAN a,b,c``
``sw1 __SCUTSS (sw1)``       ``b = __SCUTSS (a)``    ``SCUTSS a,b``
``sw1 __SLASS (sw1, sw1)``   ``c = __SLASS (a, b)``  ``SLASS a,b,c``
``void __SMASS (sw1, sw1)``  ``__SMASS (a, b)``      ``SMASS a,b``
``void __SMSSS (sw1, sw1)``  ``__SMSSS (a, b)``      ``SMSSS a,b``
``void __SMU (sw1, sw1)``    ``__SMU (a, b)``        ``SMU a,b``
``sw2 __SMUL (sw1, sw1)``    ``c = __SMUL (a, b)``   ``SMUL a,b,c``
``sw1 __SUBSS (sw1, sw1)``   ``c = __SUBSS (a, b)``  ``SUBSS a,b,c``
``uw2 __UMUL (uw1, uw1)``    ``c = __UMUL (a, b)``   ``UMUL a,b,c``
===========================  ======================  ===============
.. _directly-mapped-media-functions:

Directly-Mapped Media Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The functions listed below map directly to FR-V M-type instructions.

====================================  =========================  ===================
Function prototype                    Example usage              Assembly output
====================================  =========================  ===================
``uw1 __MABSHS (sw1)``                ``b = __MABSHS (a)``       ``MABSHS a,b``
``void __MADDACCS (acc, acc)``        ``__MADDACCS (b, a)``      ``MADDACCS a,b``
``sw1 __MADDHSS (sw1, sw1)``          ``c = __MADDHSS (a, b)``   ``MADDHSS a,b,c``
``uw1 __MADDHUS (uw1, uw1)``          ``c = __MADDHUS (a, b)``   ``MADDHUS a,b,c``
``uw1 __MAND (uw1, uw1)``             ``c = __MAND (a, b)``      ``MAND a,b,c``
``void __MASACCS (acc, acc)``         ``__MASACCS (b, a)``       ``MASACCS a,b``
``uw1 __MAVEH (uw1, uw1)``            ``c = __MAVEH (a, b)``     ``MAVEH a,b,c``
``uw2 __MBTOH (uw1)``                 ``b = __MBTOH (a)``        ``MBTOH a,b``
``void __MBTOHE (uw1 *, uw1)``        ``__MBTOHE (&b, a)``       ``MBTOHE a,b``
``void __MCLRACC (acc)``              ``__MCLRACC (a)``          ``MCLRACC a``
``void __MCLRACCA (void)``            ``__MCLRACCA ()``          ``MCLRACCA``
``uw1 __Mcop1 (uw1, uw1)``            ``c = __Mcop1 (a, b)``     ``Mcop1 a,b,c``
``uw1 __Mcop2 (uw1, uw1)``            ``c = __Mcop2 (a, b)``     ``Mcop2 a,b,c``
``uw1 __MCPLHI (uw2, const)``         ``c = __MCPLHI (a, b)``    ``MCPLHI a,#b,c``
``uw1 __MCPLI (uw2, const)``          ``c = __MCPLI (a, b)``     ``MCPLI a,#b,c``
``void __MCPXIS (acc, sw1, sw1)``     ``__MCPXIS (c, a, b)``     ``MCPXIS a,b,c``
``void __MCPXIU (acc, uw1, uw1)``     ``__MCPXIU (c, a, b)``     ``MCPXIU a,b,c``
``void __MCPXRS (acc, sw1, sw1)``     ``__MCPXRS (c, a, b)``     ``MCPXRS a,b,c``
``void __MCPXRU (acc, uw1, uw1)``     ``__MCPXRU (c, a, b)``     ``MCPXRU a,b,c``
``uw1 __MCUT (acc, uw1)``             ``c = __MCUT (a, b)``      ``MCUT a,b,c``
``uw1 __MCUTSS (acc, sw1)``           ``c = __MCUTSS (a, b)``    ``MCUTSS a,b,c``
``void __MDADDACCS (acc, acc)``       ``__MDADDACCS (b, a)``     ``MDADDACCS a,b``
``void __MDASACCS (acc, acc)``        ``__MDASACCS (b, a)``      ``MDASACCS a,b``
``uw2 __MDCUTSSI (acc, const)``       ``c = __MDCUTSSI (a, b)``  ``MDCUTSSI a,#b,c``
``uw2 __MDPACKH (uw2, uw2)``          ``c = __MDPACKH (a, b)``   ``MDPACKH a,b,c``
``uw2 __MDROTLI (uw2, const)``        ``c = __MDROTLI (a, b)``   ``MDROTLI a,#b,c``
``void __MDSUBACCS (acc, acc)``       ``__MDSUBACCS (b, a)``     ``MDSUBACCS a,b``
``void __MDUNPACKH (uw1 *, uw2)``     ``__MDUNPACKH (&b, a)``    ``MDUNPACKH a,b``
``uw2 __MEXPDHD (uw1, const)``        ``c = __MEXPDHD (a, b)``   ``MEXPDHD a,#b,c``
``uw1 __MEXPDHW (uw1, const)``        ``c = __MEXPDHW (a, b)``   ``MEXPDHW a,#b,c``
``uw1 __MHDSETH (uw1, const)``        ``c = __MHDSETH (a, b)``   ``MHDSETH a,#b,c``
``sw1 __MHDSETS (const)``             ``b = __MHDSETS (a)``      ``MHDSETS #a,b``
``uw1 __MHSETHIH (uw1, const)``       ``b = __MHSETHIH (b, a)``  ``MHSETHIH #a,b``
``sw1 __MHSETHIS (sw1, const)``       ``b = __MHSETHIS (b, a)``  ``MHSETHIS #a,b``
``uw1 __MHSETLOH (uw1, const)``       ``b = __MHSETLOH (b, a)``  ``MHSETLOH #a,b``
``sw1 __MHSETLOS (sw1, const)``       ``b = __MHSETLOS (b, a)``  ``MHSETLOS #a,b``
``uw1 __MHTOB (uw2)``                 ``b = __MHTOB (a)``        ``MHTOB a,b``
``void __MMACHS (acc, sw1, sw1)``     ``__MMACHS (c, a, b)``     ``MMACHS a,b,c``
``void __MMACHU (acc, uw1, uw1)``     ``__MMACHU (c, a, b)``     ``MMACHU a,b,c``
``void __MMRDHS (acc, sw1, sw1)``     ``__MMRDHS (c, a, b)``     ``MMRDHS a,b,c``
``void __MMRDHU (acc, uw1, uw1)``     ``__MMRDHU (c, a, b)``     ``MMRDHU a,b,c``
``void __MMULHS (acc, sw1, sw1)``     ``__MMULHS (c, a, b)``     ``MMULHS a,b,c``
``void __MMULHU (acc, uw1, uw1)``     ``__MMULHU (c, a, b)``     ``MMULHU a,b,c``
``void __MMULXHS (acc, sw1, sw1)``    ``__MMULXHS (c, a, b)``    ``MMULXHS a,b,c``
``void __MMULXHU (acc, uw1, uw1)``    ``__MMULXHU (c, a, b)``    ``MMULXHU a,b,c``
``uw1 __MNOT (uw1)``                  ``b = __MNOT (a)``         ``MNOT a,b``
``uw1 __MOR (uw1, uw1)``              ``c = __MOR (a, b)``       ``MOR a,b,c``
``uw1 __MPACKH (uh, uh)``             ``c = __MPACKH (a, b)``    ``MPACKH a,b,c``
``sw2 __MQADDHSS (sw2, sw2)``         ``c = __MQADDHSS (a, b)``  ``MQADDHSS a,b,c``
``uw2 __MQADDHUS (uw2, uw2)``         ``c = __MQADDHUS (a, b)``  ``MQADDHUS a,b,c``
``void __MQCPXIS (acc, sw2, sw2)``    ``__MQCPXIS (c, a, b)``    ``MQCPXIS a,b,c``
``void __MQCPXIU (acc, uw2, uw2)``    ``__MQCPXIU (c, a, b)``    ``MQCPXIU a,b,c``
``void __MQCPXRS (acc, sw2, sw2)``    ``__MQCPXRS (c, a, b)``    ``MQCPXRS a,b,c``
``void __MQCPXRU (acc, uw2, uw2)``    ``__MQCPXRU (c, a, b)``    ``MQCPXRU a,b,c``
``sw2 __MQLCLRHS (sw2, sw2)``         ``c = __MQLCLRHS (a, b)``  ``MQLCLRHS a,b,c``
``sw2 __MQLMTHS (sw2, sw2)``          ``c = __MQLMTHS (a, b)``   ``MQLMTHS a,b,c``
``void __MQMACHS (acc, sw2, sw2)``    ``__MQMACHS (c, a, b)``    ``MQMACHS a,b,c``
``void __MQMACHU (acc, uw2, uw2)``    ``__MQMACHU (c, a, b)``    ``MQMACHU a,b,c``
``void __MQMACXHS (acc, sw2, sw2)``   ``__MQMACXHS (c, a, b)``   ``MQMACXHS a,b,c``
``void __MQMULHS (acc, sw2, sw2)``    ``__MQMULHS (c, a, b)``    ``MQMULHS a,b,c``
``void __MQMULHU (acc, uw2, uw2)``    ``__MQMULHU (c, a, b)``    ``MQMULHU a,b,c``
``void __MQMULXHS (acc, sw2, sw2)``   ``__MQMULXHS (c, a, b)``   ``MQMULXHS a,b,c``
``void __MQMULXHU (acc, uw2, uw2)``   ``__MQMULXHU (c, a, b)``   ``MQMULXHU a,b,c``
``sw2 __MQSATHS (sw2, sw2)``          ``c = __MQSATHS (a, b)``   ``MQSATHS a,b,c``
``uw2 __MQSLLHI (uw2, int)``          ``c = __MQSLLHI (a, b)``   ``MQSLLHI a,b,c``
``sw2 __MQSRAHI (sw2, int)``          ``c = __MQSRAHI (a, b)``   ``MQSRAHI a,b,c``
``sw2 __MQSUBHSS (sw2, sw2)``         ``c = __MQSUBHSS (a, b)``  ``MQSUBHSS a,b,c``
``uw2 __MQSUBHUS (uw2, uw2)``         ``c = __MQSUBHUS (a, b)``  ``MQSUBHUS a,b,c``
``void __MQXMACHS (acc, sw2, sw2)``   ``__MQXMACHS (c, a, b)``   ``MQXMACHS a,b,c``
``void __MQXMACXHS (acc, sw2, sw2)``  ``__MQXMACXHS (c, a, b)``  ``MQXMACXHS a,b,c``
``uw1 __MRDACC (acc)``                ``b = __MRDACC (a)``       ``MRDACC a,b``
``uw1 __MRDACCG (acc)``               ``b = __MRDACCG (a)``      ``MRDACCG a,b``
``uw1 __MROTLI (uw1, const)``         ``c = __MROTLI (a, b)``    ``MROTLI a,#b,c``
``uw1 __MROTRI (uw1, const)``         ``c = __MROTRI (a, b)``    ``MROTRI a,#b,c``
``sw1 __MSATHS (sw1, sw1)``           ``c = __MSATHS (a, b)``    ``MSATHS a,b,c``
``uw1 __MSATHU (uw1, uw1)``           ``c = __MSATHU (a, b)``    ``MSATHU a,b,c``
``uw1 __MSLLHI (uw1, const)``         ``c = __MSLLHI (a, b)``    ``MSLLHI a,#b,c``
``sw1 __MSRAHI (sw1, const)``         ``c = __MSRAHI (a, b)``    ``MSRAHI a,#b,c``
``uw1 __MSRLHI (uw1, const)``         ``c = __MSRLHI (a, b)``    ``MSRLHI a,#b,c``
``void __MSUBACCS (acc, acc)``        ``__MSUBACCS (b, a)``      ``MSUBACCS a,b``
``sw1 __MSUBHSS (sw1, sw1)``          ``c = __MSUBHSS (a, b)``   ``MSUBHSS a,b,c``
``uw1 __MSUBHUS (uw1, uw1)``          ``c = __MSUBHUS (a, b)``   ``MSUBHUS a,b,c``
``void __MTRAP (void)``               ``__MTRAP ()``             ``MTRAP``
``uw2 __MUNPACKH (uw1)``              ``b = __MUNPACKH (a)``     ``MUNPACKH a,b``
``uw1 __MWCUT (uw2, uw1)``            ``c = __MWCUT (a, b)``     ``MWCUT a,b,c``
``void __MWTACC (acc, uw1)``          ``__MWTACC (b, a)``        ``MWTACC a,b``
``void __MWTACCG (acc, uw1)``         ``__MWTACCG (b, a)``       ``MWTACCG a,b``
``uw1 __MXOR (uw1, uw1)``             ``c = __MXOR (a, b)``      ``MXOR a,b,c``
====================================  =========================  ===================
.. _raw-read-write-functions:

Raw Read/Write Functions
~~~~~~~~~~~~~~~~~~~~~~~~

This sections describes built-in functions related to read and write
instructions to access memory.  These functions generate
``membar`` instructions to flush the I/O load and stores where
appropriate, as described in Fujitsu's manual described above.

:samp:`unsigned char __builtin_read8 (void *{data})`:samp:`unsigned short __builtin_read16 (void *{data})`:samp:`unsigned long __builtin_read32 (void *{data})`
:samp:`unsigned long long __builtin_read64 (void *{data})`
  :samp:`void __builtin_write8 (void *{data}, unsigned char {datum})`:samp:`void __builtin_write16 (void *{data}, unsigned short {datum})`:samp:`void __builtin_write32 (void *{data}, unsigned long {datum})`:samp:`void __builtin_write64 (void *{data}, unsigned long long {datum})`.. _other-built-in-functions:

Other Built-in Functions
~~~~~~~~~~~~~~~~~~~~~~~~

This section describes built-in functions that are not named after
a specific FR-V instruction.

:samp:`sw2 __IACCreadll (iacc {reg})`
  Return the full 64-bit value of IACC0.  The :samp:`{reg}` argument is reserved
  for future expansion and must be 0.

:samp:`sw1 __IACCreadl (iacc {reg})`
  Return the value of IACC0H if :samp:`{reg}` is 0 and IACC0L if :samp:`{reg}` is 1.
  Other values of :samp:`{reg}` are rejected as invalid.

:samp:`void __IACCsetll (iacc {reg}, sw2 {x})`
  Set the full 64-bit value of IACC0 to :samp:`{x}`.  The :samp:`{reg}` argument
  is reserved for future expansion and must be 0.

:samp:`void __IACCsetl (iacc {reg}, sw1 {x})`
  Set IACC0H to :samp:`{x}` if :samp:`{reg}` is 0 and IACC0L to :samp:`{x}` if :samp:`{reg}`
  is 1.  Other values of :samp:`{reg}` are rejected as invalid.

:samp:`void __data_prefetch0 (const void *{x})`
  Use the ``dcpl`` instruction to load the contents of address :samp:`{x}`
  into the data cache.

:samp:`void __data_prefetch (const void *{x})`
  Use the ``nldub`` instruction to load the contents of address :samp:`{x}`
  into the data cache.  The instruction is issued in slot I1.

  .. _mips-dsp-built-in-functions:

MIPS DSP Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The MIPS DSP Application-Specific Extension (ASE) includes new
instructions that are designed to improve the performance of DSP and
media applications.  It provides instructions that operate on packed
8-bit/16-bit integer data, Q7, Q15 and Q31 fractional data.

GCC supports MIPS DSP operations using both the generic
vector extensions (see :ref:`vector-extensions`) and a collection of
MIPS-specific built-in functions.  Both kinds of support are
enabled by the :option:`-mdsp` command-line option.

Revision 2 of the ASE was introduced in the second half of 2006.
This revision adds extra instructions to the original ASE, but is
otherwise backwards-compatible with it.  You can select revision 2
using the command-line option :option:`-mdspr2` ; this option implies
:option:`-mdsp`.

The SCOUNT and POS bits of the DSP control register are global.  The
WRDSP, EXTPDP, EXTPDPV and MTHLIP instructions modify the SCOUNT and
POS bits.  During optimization, the compiler does not delete these
instructions and it does not delete calls to functions containing
these instructions.

At present, GCC only provides support for operations on 32-bit
vectors.  The vector type associated with 8-bit integer data is
usually called ``v4i8``, the vector type associated with Q7
is usually called ``v4q7``, the vector type associated with 16-bit
integer data is usually called ``v2i16``, and the vector type
associated with Q15 is usually called ``v2q15``.  They can be
defined in C as follows:

.. code-block:: c++

  typedef signed char v4i8 __attribute__ ((vector_size(4)));
  typedef signed char v4q7 __attribute__ ((vector_size(4)));
  typedef short v2i16 __attribute__ ((vector_size(4)));
  typedef short v2q15 __attribute__ ((vector_size(4)));

``v4i8``, ``v4q7``, ``v2i16`` and ``v2q15`` values are
initialized in the same way as aggregates.  For example:

.. code-block:: c++

  v4i8 a = {1, 2, 3, 4};
  v4i8 b;
  b = (v4i8) {5, 6, 7, 8};

  v2q15 c = {0x0fcb, 0x3a75};
  v2q15 d;
  d = (v2q15) {0.1234 * 0x1.0p15, 0.4567 * 0x1.0p15};

*Note:* The CPU's endianness determines the order in which values
are packed.  On little-endian targets, the first value is the least
significant and the last value is the most significant.  The opposite
order applies to big-endian targets.  For example, the code above
sets the lowest byte of ``a`` to ``1`` on little-endian targets
and ``4`` on big-endian targets.

*Note:* Q7, Q15 and Q31 values must be initialized with their integer
representation.  As shown in this example, the integer representation
of a Q7 value can be obtained by multiplying the fractional value by
``0x1.0p7``.  The equivalent for Q15 values is to multiply by
``0x1.0p15``.  The equivalent for Q31 values is to multiply by
``0x1.0p31``.

The table below lists the ``v4i8`` and ``v2q15`` operations for which
hardware support exists.  ``a`` and ``b`` are ``v4i8`` values,
and ``c`` and ``d`` are ``v2q15`` values.

=========  ================
C code     MIPS instruction
=========  ================
``a + b``  ``addu.qb``
``c + d``  ``addq.ph``
``a - b``  ``subu.qb``
``c - d``  ``subq.ph``
=========  ================
The table below lists the ``v2i16`` operation for which
hardware support exists for the DSP ASE REV 2.  ``e`` and ``f`` are
``v2i16`` values.

=========  ================
C code     MIPS instruction
=========  ================
``e * f``  ``mul.ph``
=========  ================
It is easier to describe the DSP built-in functions if we first define
the following types:

.. code-block:: c++

  typedef int q31;
  typedef int i32;
  typedef unsigned int ui32;
  typedef long long a64;

``q31`` and ``i32`` are actually the same as ``int``, but we
use ``q31`` to indicate a Q31 fractional value and ``i32`` to
indicate a 32-bit integer value.  Similarly, ``a64`` is the same as
``long long``, but we use ``a64`` to indicate values that are
placed in one of the four DSP accumulators (``$ac0``,
``$ac1``, ``$ac2`` or ``$ac3``).

Also, some built-in functions prefer or require immediate numbers as
parameters, because the corresponding DSP instructions accept both immediate
numbers and register operands, or accept immediate numbers only.  The
immediate parameters are listed as follows.

.. code-block:: c++

  imm0_3: 0 to 3.
  imm0_7: 0 to 7.
  imm0_15: 0 to 15.
  imm0_31: 0 to 31.
  imm0_63: 0 to 63.
  imm0_255: 0 to 255.
  imm_n32_31: -32 to 31.
  imm_n512_511: -512 to 511.

The following built-in functions map directly to a particular MIPS DSP
instruction.  Please refer to the architecture specification
for details on what each instruction does.

.. code-block:: c++

  v2q15 __builtin_mips_addq_ph (v2q15, v2q15)
  v2q15 __builtin_mips_addq_s_ph (v2q15, v2q15)
  q31 __builtin_mips_addq_s_w (q31, q31)
  v4i8 __builtin_mips_addu_qb (v4i8, v4i8)
  v4i8 __builtin_mips_addu_s_qb (v4i8, v4i8)
  v2q15 __builtin_mips_subq_ph (v2q15, v2q15)
  v2q15 __builtin_mips_subq_s_ph (v2q15, v2q15)
  q31 __builtin_mips_subq_s_w (q31, q31)
  v4i8 __builtin_mips_subu_qb (v4i8, v4i8)
  v4i8 __builtin_mips_subu_s_qb (v4i8, v4i8)
  i32 __builtin_mips_addsc (i32, i32)
  i32 __builtin_mips_addwc (i32, i32)
  i32 __builtin_mips_modsub (i32, i32)
  i32 __builtin_mips_raddu_w_qb (v4i8)
  v2q15 __builtin_mips_absq_s_ph (v2q15)
  q31 __builtin_mips_absq_s_w (q31)
  v4i8 __builtin_mips_precrq_qb_ph (v2q15, v2q15)
  v2q15 __builtin_mips_precrq_ph_w (q31, q31)
  v2q15 __builtin_mips_precrq_rs_ph_w (q31, q31)
  v4i8 __builtin_mips_precrqu_s_qb_ph (v2q15, v2q15)
  q31 __builtin_mips_preceq_w_phl (v2q15)
  q31 __builtin_mips_preceq_w_phr (v2q15)
  v2q15 __builtin_mips_precequ_ph_qbl (v4i8)
  v2q15 __builtin_mips_precequ_ph_qbr (v4i8)
  v2q15 __builtin_mips_precequ_ph_qbla (v4i8)
  v2q15 __builtin_mips_precequ_ph_qbra (v4i8)
  v2q15 __builtin_mips_preceu_ph_qbl (v4i8)
  v2q15 __builtin_mips_preceu_ph_qbr (v4i8)
  v2q15 __builtin_mips_preceu_ph_qbla (v4i8)
  v2q15 __builtin_mips_preceu_ph_qbra (v4i8)
  v4i8 __builtin_mips_shll_qb (v4i8, imm0_7)
  v4i8 __builtin_mips_shll_qb (v4i8, i32)
  v2q15 __builtin_mips_shll_ph (v2q15, imm0_15)
  v2q15 __builtin_mips_shll_ph (v2q15, i32)
  v2q15 __builtin_mips_shll_s_ph (v2q15, imm0_15)
  v2q15 __builtin_mips_shll_s_ph (v2q15, i32)
  q31 __builtin_mips_shll_s_w (q31, imm0_31)
  q31 __builtin_mips_shll_s_w (q31, i32)
  v4i8 __builtin_mips_shrl_qb (v4i8, imm0_7)
  v4i8 __builtin_mips_shrl_qb (v4i8, i32)
  v2q15 __builtin_mips_shra_ph (v2q15, imm0_15)
  v2q15 __builtin_mips_shra_ph (v2q15, i32)
  v2q15 __builtin_mips_shra_r_ph (v2q15, imm0_15)
  v2q15 __builtin_mips_shra_r_ph (v2q15, i32)
  q31 __builtin_mips_shra_r_w (q31, imm0_31)
  q31 __builtin_mips_shra_r_w (q31, i32)
  v2q15 __builtin_mips_muleu_s_ph_qbl (v4i8, v2q15)
  v2q15 __builtin_mips_muleu_s_ph_qbr (v4i8, v2q15)
  v2q15 __builtin_mips_mulq_rs_ph (v2q15, v2q15)
  q31 __builtin_mips_muleq_s_w_phl (v2q15, v2q15)
  q31 __builtin_mips_muleq_s_w_phr (v2q15, v2q15)
  a64 __builtin_mips_dpau_h_qbl (a64, v4i8, v4i8)
  a64 __builtin_mips_dpau_h_qbr (a64, v4i8, v4i8)
  a64 __builtin_mips_dpsu_h_qbl (a64, v4i8, v4i8)
  a64 __builtin_mips_dpsu_h_qbr (a64, v4i8, v4i8)
  a64 __builtin_mips_dpaq_s_w_ph (a64, v2q15, v2q15)
  a64 __builtin_mips_dpaq_sa_l_w (a64, q31, q31)
  a64 __builtin_mips_dpsq_s_w_ph (a64, v2q15, v2q15)
  a64 __builtin_mips_dpsq_sa_l_w (a64, q31, q31)
  a64 __builtin_mips_mulsaq_s_w_ph (a64, v2q15, v2q15)
  a64 __builtin_mips_maq_s_w_phl (a64, v2q15, v2q15)
  a64 __builtin_mips_maq_s_w_phr (a64, v2q15, v2q15)
  a64 __builtin_mips_maq_sa_w_phl (a64, v2q15, v2q15)
  a64 __builtin_mips_maq_sa_w_phr (a64, v2q15, v2q15)
  i32 __builtin_mips_bitrev (i32)
  i32 __builtin_mips_insv (i32, i32)
  v4i8 __builtin_mips_repl_qb (imm0_255)
  v4i8 __builtin_mips_repl_qb (i32)
  v2q15 __builtin_mips_repl_ph (imm_n512_511)
  v2q15 __builtin_mips_repl_ph (i32)
  void __builtin_mips_cmpu_eq_qb (v4i8, v4i8)
  void __builtin_mips_cmpu_lt_qb (v4i8, v4i8)
  void __builtin_mips_cmpu_le_qb (v4i8, v4i8)
  i32 __builtin_mips_cmpgu_eq_qb (v4i8, v4i8)
  i32 __builtin_mips_cmpgu_lt_qb (v4i8, v4i8)
  i32 __builtin_mips_cmpgu_le_qb (v4i8, v4i8)
  void __builtin_mips_cmp_eq_ph (v2q15, v2q15)
  void __builtin_mips_cmp_lt_ph (v2q15, v2q15)
  void __builtin_mips_cmp_le_ph (v2q15, v2q15)
  v4i8 __builtin_mips_pick_qb (v4i8, v4i8)
  v2q15 __builtin_mips_pick_ph (v2q15, v2q15)
  v2q15 __builtin_mips_packrl_ph (v2q15, v2q15)
  i32 __builtin_mips_extr_w (a64, imm0_31)
  i32 __builtin_mips_extr_w (a64, i32)
  i32 __builtin_mips_extr_r_w (a64, imm0_31)
  i32 __builtin_mips_extr_s_h (a64, i32)
  i32 __builtin_mips_extr_rs_w (a64, imm0_31)
  i32 __builtin_mips_extr_rs_w (a64, i32)
  i32 __builtin_mips_extr_s_h (a64, imm0_31)
  i32 __builtin_mips_extr_r_w (a64, i32)
  i32 __builtin_mips_extp (a64, imm0_31)
  i32 __builtin_mips_extp (a64, i32)
  i32 __builtin_mips_extpdp (a64, imm0_31)
  i32 __builtin_mips_extpdp (a64, i32)
  a64 __builtin_mips_shilo (a64, imm_n32_31)
  a64 __builtin_mips_shilo (a64, i32)
  a64 __builtin_mips_mthlip (a64, i32)
  void __builtin_mips_wrdsp (i32, imm0_63)
  i32 __builtin_mips_rddsp (imm0_63)
  i32 __builtin_mips_lbux (void *, i32)
  i32 __builtin_mips_lhx (void *, i32)
  i32 __builtin_mips_lwx (void *, i32)
  a64 __builtin_mips_ldx (void *, i32) [MIPS64 only]
  i32 __builtin_mips_bposge32 (void)
  a64 __builtin_mips_madd (a64, i32, i32);
  a64 __builtin_mips_maddu (a64, ui32, ui32);
  a64 __builtin_mips_msub (a64, i32, i32);
  a64 __builtin_mips_msubu (a64, ui32, ui32);
  a64 __builtin_mips_mult (i32, i32);
  a64 __builtin_mips_multu (ui32, ui32);

The following built-in functions map directly to a particular MIPS DSP REV 2
instruction.  Please refer to the architecture specification
for details on what each instruction does.

.. code-block:: c++

  v4q7 __builtin_mips_absq_s_qb (v4q7);
  v2i16 __builtin_mips_addu_ph (v2i16, v2i16);
  v2i16 __builtin_mips_addu_s_ph (v2i16, v2i16);
  v4i8 __builtin_mips_adduh_qb (v4i8, v4i8);
  v4i8 __builtin_mips_adduh_r_qb (v4i8, v4i8);
  i32 __builtin_mips_append (i32, i32, imm0_31);
  i32 __builtin_mips_balign (i32, i32, imm0_3);
  i32 __builtin_mips_cmpgdu_eq_qb (v4i8, v4i8);
  i32 __builtin_mips_cmpgdu_lt_qb (v4i8, v4i8);
  i32 __builtin_mips_cmpgdu_le_qb (v4i8, v4i8);
  a64 __builtin_mips_dpa_w_ph (a64, v2i16, v2i16);
  a64 __builtin_mips_dps_w_ph (a64, v2i16, v2i16);
  v2i16 __builtin_mips_mul_ph (v2i16, v2i16);
  v2i16 __builtin_mips_mul_s_ph (v2i16, v2i16);
  q31 __builtin_mips_mulq_rs_w (q31, q31);
  v2q15 __builtin_mips_mulq_s_ph (v2q15, v2q15);
  q31 __builtin_mips_mulq_s_w (q31, q31);
  a64 __builtin_mips_mulsa_w_ph (a64, v2i16, v2i16);
  v4i8 __builtin_mips_precr_qb_ph (v2i16, v2i16);
  v2i16 __builtin_mips_precr_sra_ph_w (i32, i32, imm0_31);
  v2i16 __builtin_mips_precr_sra_r_ph_w (i32, i32, imm0_31);
  i32 __builtin_mips_prepend (i32, i32, imm0_31);
  v4i8 __builtin_mips_shra_qb (v4i8, imm0_7);
  v4i8 __builtin_mips_shra_r_qb (v4i8, imm0_7);
  v4i8 __builtin_mips_shra_qb (v4i8, i32);
  v4i8 __builtin_mips_shra_r_qb (v4i8, i32);
  v2i16 __builtin_mips_shrl_ph (v2i16, imm0_15);
  v2i16 __builtin_mips_shrl_ph (v2i16, i32);
  v2i16 __builtin_mips_subu_ph (v2i16, v2i16);
  v2i16 __builtin_mips_subu_s_ph (v2i16, v2i16);
  v4i8 __builtin_mips_subuh_qb (v4i8, v4i8);
  v4i8 __builtin_mips_subuh_r_qb (v4i8, v4i8);
  v2q15 __builtin_mips_addqh_ph (v2q15, v2q15);
  v2q15 __builtin_mips_addqh_r_ph (v2q15, v2q15);
  q31 __builtin_mips_addqh_w (q31, q31);
  q31 __builtin_mips_addqh_r_w (q31, q31);
  v2q15 __builtin_mips_subqh_ph (v2q15, v2q15);
  v2q15 __builtin_mips_subqh_r_ph (v2q15, v2q15);
  q31 __builtin_mips_subqh_w (q31, q31);
  q31 __builtin_mips_subqh_r_w (q31, q31);
  a64 __builtin_mips_dpax_w_ph (a64, v2i16, v2i16);
  a64 __builtin_mips_dpsx_w_ph (a64, v2i16, v2i16);
  a64 __builtin_mips_dpaqx_s_w_ph (a64, v2q15, v2q15);
  a64 __builtin_mips_dpaqx_sa_w_ph (a64, v2q15, v2q15);
  a64 __builtin_mips_dpsqx_s_w_ph (a64, v2q15, v2q15);
  a64 __builtin_mips_dpsqx_sa_w_ph (a64, v2q15, v2q15);

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

.. _mips-loongson-built-in-functions:

MIPS Loongson Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides intrinsics to access the SIMD instructions provided by the
ST Microelectronics Loongson-2E and -2F processors.  These intrinsics,
available after inclusion of the ``loongson.h`` header file,
operate on the following 64-bit vector types:

** ``uint8x8_t``, a vector of eight unsigned 8-bit integers;

* ``uint16x4_t``, a vector of four unsigned 16-bit integers;

* ``uint32x2_t``, a vector of two unsigned 32-bit integers;

* ``int8x8_t``, a vector of eight signed 8-bit integers;

* ``int16x4_t``, a vector of four signed 16-bit integers;

* ``int32x2_t``, a vector of two signed 32-bit integers.

The intrinsics provided are listed below; each is named after the
machine instruction to which it corresponds, with suffixes added as
appropriate to distinguish intrinsics that expand to the same machine
instruction yet have different argument types.  Refer to the architecture
documentation for a description of the functionality of each
instruction.

.. code-block:: c++

  int16x4_t packsswh (int32x2_t s, int32x2_t t);
  int8x8_t packsshb (int16x4_t s, int16x4_t t);
  uint8x8_t packushb (uint16x4_t s, uint16x4_t t);
  uint32x2_t paddw_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t paddh_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t paddb_u (uint8x8_t s, uint8x8_t t);
  int32x2_t paddw_s (int32x2_t s, int32x2_t t);
  int16x4_t paddh_s (int16x4_t s, int16x4_t t);
  int8x8_t paddb_s (int8x8_t s, int8x8_t t);
  uint64_t paddd_u (uint64_t s, uint64_t t);
  int64_t paddd_s (int64_t s, int64_t t);
  int16x4_t paddsh (int16x4_t s, int16x4_t t);
  int8x8_t paddsb (int8x8_t s, int8x8_t t);
  uint16x4_t paddush (uint16x4_t s, uint16x4_t t);
  uint8x8_t paddusb (uint8x8_t s, uint8x8_t t);
  uint64_t pandn_ud (uint64_t s, uint64_t t);
  uint32x2_t pandn_uw (uint32x2_t s, uint32x2_t t);
  uint16x4_t pandn_uh (uint16x4_t s, uint16x4_t t);
  uint8x8_t pandn_ub (uint8x8_t s, uint8x8_t t);
  int64_t pandn_sd (int64_t s, int64_t t);
  int32x2_t pandn_sw (int32x2_t s, int32x2_t t);
  int16x4_t pandn_sh (int16x4_t s, int16x4_t t);
  int8x8_t pandn_sb (int8x8_t s, int8x8_t t);
  uint16x4_t pavgh (uint16x4_t s, uint16x4_t t);
  uint8x8_t pavgb (uint8x8_t s, uint8x8_t t);
  uint32x2_t pcmpeqw_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t pcmpeqh_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t pcmpeqb_u (uint8x8_t s, uint8x8_t t);
  int32x2_t pcmpeqw_s (int32x2_t s, int32x2_t t);
  int16x4_t pcmpeqh_s (int16x4_t s, int16x4_t t);
  int8x8_t pcmpeqb_s (int8x8_t s, int8x8_t t);
  uint32x2_t pcmpgtw_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t pcmpgth_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t pcmpgtb_u (uint8x8_t s, uint8x8_t t);
  int32x2_t pcmpgtw_s (int32x2_t s, int32x2_t t);
  int16x4_t pcmpgth_s (int16x4_t s, int16x4_t t);
  int8x8_t pcmpgtb_s (int8x8_t s, int8x8_t t);
  uint16x4_t pextrh_u (uint16x4_t s, int field);
  int16x4_t pextrh_s (int16x4_t s, int field);
  uint16x4_t pinsrh_0_u (uint16x4_t s, uint16x4_t t);
  uint16x4_t pinsrh_1_u (uint16x4_t s, uint16x4_t t);
  uint16x4_t pinsrh_2_u (uint16x4_t s, uint16x4_t t);
  uint16x4_t pinsrh_3_u (uint16x4_t s, uint16x4_t t);
  int16x4_t pinsrh_0_s (int16x4_t s, int16x4_t t);
  int16x4_t pinsrh_1_s (int16x4_t s, int16x4_t t);
  int16x4_t pinsrh_2_s (int16x4_t s, int16x4_t t);
  int16x4_t pinsrh_3_s (int16x4_t s, int16x4_t t);
  int32x2_t pmaddhw (int16x4_t s, int16x4_t t);
  int16x4_t pmaxsh (int16x4_t s, int16x4_t t);
  uint8x8_t pmaxub (uint8x8_t s, uint8x8_t t);
  int16x4_t pminsh (int16x4_t s, int16x4_t t);
  uint8x8_t pminub (uint8x8_t s, uint8x8_t t);
  uint8x8_t pmovmskb_u (uint8x8_t s);
  int8x8_t pmovmskb_s (int8x8_t s);
  uint16x4_t pmulhuh (uint16x4_t s, uint16x4_t t);
  int16x4_t pmulhh (int16x4_t s, int16x4_t t);
  int16x4_t pmullh (int16x4_t s, int16x4_t t);
  int64_t pmuluw (uint32x2_t s, uint32x2_t t);
  uint8x8_t pasubub (uint8x8_t s, uint8x8_t t);
  uint16x4_t biadd (uint8x8_t s);
  uint16x4_t psadbh (uint8x8_t s, uint8x8_t t);
  uint16x4_t pshufh_u (uint16x4_t dest, uint16x4_t s, uint8_t order);
  int16x4_t pshufh_s (int16x4_t dest, int16x4_t s, uint8_t order);
  uint16x4_t psllh_u (uint16x4_t s, uint8_t amount);
  int16x4_t psllh_s (int16x4_t s, uint8_t amount);
  uint32x2_t psllw_u (uint32x2_t s, uint8_t amount);
  int32x2_t psllw_s (int32x2_t s, uint8_t amount);
  uint16x4_t psrlh_u (uint16x4_t s, uint8_t amount);
  int16x4_t psrlh_s (int16x4_t s, uint8_t amount);
  uint32x2_t psrlw_u (uint32x2_t s, uint8_t amount);
  int32x2_t psrlw_s (int32x2_t s, uint8_t amount);
  uint16x4_t psrah_u (uint16x4_t s, uint8_t amount);
  int16x4_t psrah_s (int16x4_t s, uint8_t amount);
  uint32x2_t psraw_u (uint32x2_t s, uint8_t amount);
  int32x2_t psraw_s (int32x2_t s, uint8_t amount);
  uint32x2_t psubw_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t psubh_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t psubb_u (uint8x8_t s, uint8x8_t t);
  int32x2_t psubw_s (int32x2_t s, int32x2_t t);
  int16x4_t psubh_s (int16x4_t s, int16x4_t t);
  int8x8_t psubb_s (int8x8_t s, int8x8_t t);
  uint64_t psubd_u (uint64_t s, uint64_t t);
  int64_t psubd_s (int64_t s, int64_t t);
  int16x4_t psubsh (int16x4_t s, int16x4_t t);
  int8x8_t psubsb (int8x8_t s, int8x8_t t);
  uint16x4_t psubush (uint16x4_t s, uint16x4_t t);
  uint8x8_t psubusb (uint8x8_t s, uint8x8_t t);
  uint32x2_t punpckhwd_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t punpckhhw_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t punpckhbh_u (uint8x8_t s, uint8x8_t t);
  int32x2_t punpckhwd_s (int32x2_t s, int32x2_t t);
  int16x4_t punpckhhw_s (int16x4_t s, int16x4_t t);
  int8x8_t punpckhbh_s (int8x8_t s, int8x8_t t);
  uint32x2_t punpcklwd_u (uint32x2_t s, uint32x2_t t);
  uint16x4_t punpcklhw_u (uint16x4_t s, uint16x4_t t);
  uint8x8_t punpcklbh_u (uint8x8_t s, uint8x8_t t);
  int32x2_t punpcklwd_s (int32x2_t s, int32x2_t t);
  int16x4_t punpcklhw_s (int16x4_t s, int16x4_t t);
  int8x8_t punpcklbh_s (int8x8_t s, int8x8_t t);

.. toctree::

   <paired-single-arithmetic>
   <paired-single-built-in-functions>
   <mips-3d-built-in-functions>

.. _paired-single-arithmetic:

Paired-Single Arithmetic
~~~~~~~~~~~~~~~~~~~~~~~~

The table below lists the ``v2sf`` operations for which hardware
support exists.  ``a``, ``b`` and ``c`` are ``v2sf``
values and ``x`` is an integral value.

================  =======================
C code            MIPS instruction
================  =======================
``a + b``         ``add.ps``
``a - b``         ``sub.ps``
``-a``            ``neg.ps``
``a * b``         ``mul.ps``
``a * b + c``     ``madd.ps``
``a * b - c``     ``msub.ps``
``-(a * b + c)``  ``nmadd.ps``
``-(a * b - c)``  ``nmsub.ps``
``x ? a : b``     ``movn.ps``/``movz.ps``
================  =======================
Note that the multiply-accumulate instructions can be disabled
using the command-line option ``-mno-fused-madd``.

.. _paired-single-built-in-functions:

Paired-Single Built-in Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following paired-single functions map directly to a particular
MIPS instruction.  Please refer to the architecture specification
for details on what each instruction does.

``v2sf __builtin_mips_pll_ps (v2sf, v2sf)``
  Pair lower lower (``pll.ps``).

``v2sf __builtin_mips_pul_ps (v2sf, v2sf)``
  Pair upper lower (``pul.ps``).

``v2sf __builtin_mips_plu_ps (v2sf, v2sf)``
  Pair lower upper (``plu.ps``).

``v2sf __builtin_mips_puu_ps (v2sf, v2sf)``
  Pair upper upper (``puu.ps``).

``v2sf __builtin_mips_cvt_ps_s (float, float)``
  Convert pair to paired single (``cvt.ps.s``).

``float __builtin_mips_cvt_s_pl (v2sf)``
  Convert pair lower to single (``cvt.s.pl``).

``float __builtin_mips_cvt_s_pu (v2sf)``
  Convert pair upper to single (``cvt.s.pu``).

``v2sf __builtin_mips_abs_ps (v2sf)``
  Absolute value (``abs.ps``).

``v2sf __builtin_mips_alnv_ps (v2sf, v2sf, int)``
  Align variable (``alnv.ps``).

  *Note:* The value of the third parameter must be 0 or 4
  modulo 8, otherwise the result is unpredictable.  Please read the
  instruction description for details.

  The following multi-instruction functions are also available.
In each case, :samp:`{cond}` can be any of the 16 floating-point conditions:
``f``, ``un``, ``eq``, ``ueq``, ``olt``, ``ult``,
``ole``, ``ule``, ``sf``, ``ngle``, ``seq``, ``ngl``,
``lt``, ``nge``, ``le`` or ``ngt``.

:samp:`v2sf __builtin_mips_movt_c_{cond}_ps (v2sf {a}, v2sf {b}, v2sf {c}, v2sf {d})` :samp:`v2sf __builtin_mips_movf_c_{cond}_ps (v2sf {a}, v2sf {b}, v2sf {c}, v2sf {d})`
  Conditional move based on floating-point comparison (``c.cond.ps``,
  ``movt.ps``/``movf.ps``).

  The ``movt`` functions return the value :samp:`{x}` computed by:

  .. code-block:: c++

    c.cond.ps cc,a,b
    mov.ps x,c
    movt.ps x,d,cc

  The ``movf`` functions are similar but use ``movf.ps`` instead
  of ``movt.ps``.

:samp:`int __builtin_mips_upper_c_{cond}_ps (v2sf {a}, v2sf {b})` :samp:`int __builtin_mips_lower_c_{cond}_ps (v2sf {a}, v2sf {b})`
  Comparison of two paired-single values (``c.cond.ps``,
  ``bc1t``/``bc1f``).

  These functions compare :samp:`{a}` and :samp:`{b}` using ``c.cond.ps``
  and return either the upper or lower half of the result.  For example:

  .. code-block:: c++

    v2sf a, b;
    if (__builtin_mips_upper_c_eq_ps (a, b))
      upper_halves_are_equal ();
    else
      upper_halves_are_unequal ();

    if (__builtin_mips_lower_c_eq_ps (a, b))
      lower_halves_are_equal ();
    else
      lower_halves_are_unequal ();

  .. _mips-3d-built-in-functions:

MIPS-3D Built-in Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~

The MIPS-3D Application-Specific Extension (ASE) includes additional
paired-single instructions that are designed to improve the performance
of 3D graphics operations.  Support for these instructions is controlled
by the :option:`-mips3d` command-line option.

The functions listed below map directly to a particular MIPS-3D
instruction.  Please refer to the architecture specification for
more details on what each instruction does.

``v2sf __builtin_mips_addr_ps (v2sf, v2sf)``
  Reduction add (``addr.ps``).

``v2sf __builtin_mips_mulr_ps (v2sf, v2sf)``
  Reduction multiply (``mulr.ps``).

``v2sf __builtin_mips_cvt_pw_ps (v2sf)``
  Convert paired single to paired word (``cvt.pw.ps``).

``v2sf __builtin_mips_cvt_ps_pw (v2sf)``
  Convert paired word to paired single (``cvt.ps.pw``).

``float __builtin_mips_recip1_s (float)`` ``double __builtin_mips_recip1_d (double)`` ``v2sf __builtin_mips_recip1_ps (v2sf)``
  Reduced-precision reciprocal (sequence step 1) (``recip1.fmt``).

``float __builtin_mips_recip2_s (float, float)`` ``double __builtin_mips_recip2_d (double, double)`` ``v2sf __builtin_mips_recip2_ps (v2sf, v2sf)``
  Reduced-precision reciprocal (sequence step 2) (``recip2.fmt``).

``float __builtin_mips_rsqrt1_s (float)`` ``double __builtin_mips_rsqrt1_d (double)`` ``v2sf __builtin_mips_rsqrt1_ps (v2sf)``
  Reduced-precision reciprocal square root (sequence step 1)
  (``rsqrt1.fmt``).

``float __builtin_mips_rsqrt2_s (float, float)`` ``double __builtin_mips_rsqrt2_d (double, double)`` ``v2sf __builtin_mips_rsqrt2_ps (v2sf, v2sf)``
  Reduced-precision reciprocal square root (sequence step 2)
  (``rsqrt2.fmt``).

  The following multi-instruction functions are also available.
In each case, :samp:`{cond}` can be any of the 16 floating-point conditions:
``f``, ``un``, ``eq``, ``ueq``, ``olt``, ``ult``,
``ole``, ``ule``, ``sf``, ``ngle``, ``seq``,
``ngl``, ``lt``, ``nge``, ``le`` or ``ngt``.

:samp:`int __builtin_mips_cabs_{cond}_s (float {a}, float {b})` :samp:`int __builtin_mips_cabs_{cond}_d (double {a}, double {b})`
  Absolute comparison of two scalar values (``cabs.cond.fmt``,
  ``bc1t``/``bc1f``).

  These functions compare :samp:`{a}` and :samp:`{b}` using ``cabs.cond.s``
  or ``cabs.cond.d`` and return the result as a boolean value.
  For example:

  .. code-block:: c++

    float a, b;
    if (__builtin_mips_cabs_eq_s (a, b))
      true ();
    else
      false ();

:samp:`int __builtin_mips_upper_cabs_{cond}_ps (v2sf {a}, v2sf {b})` :samp:`int __builtin_mips_lower_cabs_{cond}_ps (v2sf {a}, v2sf {b})`
  Absolute comparison of two paired-single values (``cabs.cond.ps``,
  ``bc1t``/``bc1f``).

  These functions compare :samp:`{a}` and :samp:`{b}` using ``cabs.cond.ps``
  and return either the upper or lower half of the result.  For example:

  .. code-block:: c++

    v2sf a, b;
    if (__builtin_mips_upper_cabs_eq_ps (a, b))
      upper_halves_are_equal ();
    else
      upper_halves_are_unequal ();

    if (__builtin_mips_lower_cabs_eq_ps (a, b))
      lower_halves_are_equal ();
    else
      lower_halves_are_unequal ();

:samp:`v2sf __builtin_mips_movt_cabs_{cond}_ps (v2sf {a}, v2sf {b}, v2sf {c}, v2sf {d})` :samp:`v2sf __builtin_mips_movf_cabs_{cond}_ps (v2sf {a}, v2sf {b}, v2sf {c}, v2sf {d})`
  Conditional move based on absolute comparison (``cabs.cond.ps``,
  ``movt.ps``/``movf.ps``).

  The ``movt`` functions return the value :samp:`{x}` computed by:

  .. code-block:: c++

    cabs.cond.ps cc,a,b
    mov.ps x,c
    movt.ps x,d,cc

  The ``movf`` functions are similar but use ``movf.ps`` instead
  of ``movt.ps``.

:samp:`int __builtin_mips_any_c_{cond}_ps (v2sf {a}, v2sf {b})` :samp:`int __builtin_mips_all_c_{cond}_ps (v2sf {a}, v2sf {b})` :samp:`int __builtin_mips_any_cabs_{cond}_ps (v2sf {a}, v2sf {b})` :samp:`int __builtin_mips_all_cabs_{cond}_ps (v2sf {a}, v2sf {b})`
  Comparison of two paired-single values
  (``c.cond.ps``/``cabs.cond.ps``,
  ``bc1any2t``/``bc1any2f``).

  These functions compare :samp:`{a}` and :samp:`{b}` using ``c.cond.ps``
  or ``cabs.cond.ps``.  The ``any`` forms return ``true`` if either
  result is ``true`` and the ``all`` forms return ``true`` if both results are ``true``.
  For example:

  .. code-block:: c++

    v2sf a, b;
    if (__builtin_mips_any_c_eq_ps (a, b))
      one_is_true ();
    else
      both_are_false ();

    if (__builtin_mips_all_c_eq_ps (a, b))
      both_are_true ();
    else
      one_is_false ();

:samp:`int __builtin_mips_any_c_{cond}_4s (v2sf {a}, v2sf {b}, v2sf {c}, v2sf {d})` :samp:`int __builtin_mips_all_c_{cond}_4s (v2sf {a}, v2sf {b}, v2sf {c}, v2sf {d})` :samp:`int __builtin_mips_any_cabs_{cond}_4s (v2sf {a}, v2sf {b}, v2sf {c}, v2sf {d})` :samp:`int __builtin_mips_all_cabs_{cond}_4s (v2sf {a}, v2sf {b}, v2sf {c}, v2sf {d})`
  Comparison of four paired-single values
  (``c.cond.ps``/``cabs.cond.ps``,
  ``bc1any4t``/``bc1any4f``).

  These functions use ``c.cond.ps`` or ``cabs.cond.ps``
  to compare :samp:`{a}` with :samp:`{b}` and to compare :samp:`{c}` with :samp:`{d}`.
  The ``any`` forms return ``true`` if any of the four results are ``true``
  and the ``all`` forms return ``true`` if all four results are ``true``.
  For example:

  .. code-block:: c++

    v2sf a, b, c, d;
    if (__builtin_mips_any_c_eq_4s (a, b, c, d))
      some_are_true ();
    else
      all_are_false ();

    if (__builtin_mips_all_c_eq_4s (a, b, c, d))
      all_are_true ();
    else
      some_are_false ();

  .. _mips-simd-architecture-(msa)-support:

MIPS SIMD Architecture (MSA) Support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. toctree::

   <mips-simd-architecture-built-in-functions>

GCC provides intrinsics to access the SIMD instructions provided by the
MSA MIPS SIMD Architecture.  The interface is made available by including
``<msa.h>`` and using :option:`-mmsa -mhard-float -mfp64 -mnan=2008`.
For each ``__builtin_msa_*``, there is a shortened name of the intrinsic,
``__msa_*``.

MSA implements 128-bit wide vector registers, operating on 8-, 16-, 32- and
64-bit integer, 16- and 32-bit fixed-point, or 32- and 64-bit floating point
data elements.  The following vectors typedefs are included in ``msa.h``:

** ``v16i8``, a vector of sixteen signed 8-bit integers;

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

** ``imm0_1``, an integer literal in range 0 to 1;

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

.. _other-mips-built-in-functions:

Other MIPS Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides other MIPS-specific built-in functions:

:samp:`void __builtin_mips_cache (int {op}, const volatile void *{addr})`
  Insert a :samp:`cache` instruction with operands :samp:`{op}` and :samp:`{addr}`.
  GCC defines the preprocessor macro ``___GCC_HAVE_BUILTIN_MIPS_CACHE``
  when this function is available.

``unsigned int __builtin_mips_get_fcsr (void)`` :samp:`void __builtin_mips_set_fcsr (unsigned int {value})`
  Get and set the contents of the floating-point control and status register
  (FPU control register 31).  These functions are only available in hard-float
  code but can be called in both MIPS16 and non-MIPS16 contexts.

  ``__builtin_mips_set_fcsr`` can be used to change any bit of the
  register except the condition codes, which GCC assumes are preserved.

  .. _msp430-built-in-functions:

MSP430 Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides a couple of special builtin functions to aid in the
writing of interrupt handlers in C.

:samp:`__bic_SR_register_on_exit (int {mask})`
  This clears the indicated bits in the saved copy of the status register
  currently residing on the stack.  This only works inside interrupt
  handlers and the changes to the status register will only take affect
  once the handler returns.

:samp:`__bis_SR_register_on_exit (int {mask})`
  This sets the indicated bits in the saved copy of the status register
  currently residing on the stack.  This only works inside interrupt
  handlers and the changes to the status register will only take affect
  once the handler returns.

:samp:`__delay_cycles (long long {cycles})`
  This inserts an instruction sequence that takes exactly :samp:`{cycles}`
  cycles (between 0 and about 17E9) to complete.  The inserted sequence
  may use jumps, loops, or no-ops, and does not interfere with any other
  instructions.  Note that :samp:`{cycles}` must be a compile-time constant
  integer - that is, you must pass a number, not a variable that may be
  optimized to a constant later.  The number of cycles delayed by this
  builtin is exact.

  .. _nds32-built-in-functions:

NDS32 Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the NDS32 target:

.. function:: void __builtin_nds32_isync(int *addr)

  Insert an ISYNC instruction into the instruction stream where
  :samp:`{addr}` is an instruction address for serialization.

.. function:: void __builtin_nds32_isb(void )

  Insert an ISB instruction into the instruction stream.

.. function:: int __builtin_nds32_mfsr(int sr)

  Return the content of a system register which is mapped by :samp:`{sr}`.

.. function:: int __builtin_nds32_mfusr(int usr)

  Return the content of a user space register which is mapped by :samp:`{usr}`.

.. function:: void __builtin_nds32_mtsr(int value,int sr)

  Move the :samp:`{value}` to a system register which is mapped by :samp:`{sr}`.

.. function:: void __builtin_nds32_mtusr(int value,int usr)

  Move the :samp:`{value}` to a user space register which is mapped by :samp:`{usr}`.

.. function:: void __builtin_nds32_setgie_en(void )

  Enable global interrupt.

.. function:: void __builtin_nds32_setgie_dis(void )

  Disable global interrupt.

.. _picochip-built-in-functions:

picoChip Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides an interface to selected machine instructions from the
picoChip instruction set.

:samp:`int __builtin_sbc (int {value})`
  Sign bit count.  Return the number of consecutive bits in :samp:`{value}`
  that have the same value as the sign bit.  The result is the number of
  leading sign bits minus one, giving the number of redundant sign bits in
  :samp:`{value}`.

:samp:`int __builtin_byteswap (int {value})`
  Byte swap.  Return the result of swapping the upper and lower bytes of
  :samp:`{value}`.

:samp:`int __builtin_brev (int {value})`
  Bit reversal.  Return the result of reversing the bits in
  :samp:`{value}`.  Bit 15 is swapped with bit 0, bit 14 is swapped with bit 1,
  and so on.

:samp:`int __builtin_adds (int {x}, int {y})`
  Saturating addition.  Return the result of adding :samp:`{x}` and :samp:`{y}` ,
  storing the value 32767 if the result overflows.

:samp:`int __builtin_subs (int {x}, int {y})`
  Saturating subtraction.  Return the result of subtracting :samp:`{y}` from
  :samp:`{x}` , storing the value -32768 if the result overflows.

``void __builtin_halt (void)``
  Halt.  The processor stops execution.  This built-in is useful for
  implementing assertions.

  .. _basic-powerpc-built-in-functions:

Basic PowerPC Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. toctree::

   <basic-powerpc-built-in-functions-available-on-all-configurations>
   <basic-powerpc-built-in-functions-available-on-isa-2.05>
   <basic-powerpc-built-in-functions-available-on-isa-2.06>
   <basic-powerpc-built-in-functions-available-on-isa-2.07>
   <basic-powerpc-built-in-functions-available-on-isa-3.0>
   <basic-powerpc-built-in-functions-available-on-isa-3.1>

This section describes PowerPC built-in functions that do not require
the inclusion of any special header files to declare prototypes or
provide macro definitions.  The sections that follow describe
additional PowerPC built-in functions.

.. _basic-powerpc-built-in-functions-available-on-all-configurations:

Basic PowerPC Built-in Functions Available on all Configurations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. function:: void __builtin_cpu_init(void )

  This function is a ``nop`` on the PowerPC platform and is included solely
  to maintain API compatibility with the x86 builtins.

.. function:: int __builtin_cpu_is(const char* cpuname)

  This function returns a value of ``1`` if the run-time CPU is of type
  :samp:`{cpuname}` and returns ``0`` otherwise

  The ``__builtin_cpu_is`` function requires GLIBC 2.23 or newer
  which exports the hardware capability bits.  GCC defines the macro
  ``__BUILTIN_CPU_SUPPORTS__`` if the ``__builtin_cpu_supports``
  built-in function is fully supported.

  If GCC was configured to use a GLIBC before 2.23, the built-in
  function ``__builtin_cpu_is`` always returns a 0 and the compiler
  issues a warning.

  The following CPU names can be detected:

  :samp:`power10`
    IBM POWER10 Server CPU.

  :samp:`power9`
    IBM POWER9 Server CPU.

  :samp:`power8`
    IBM POWER8 Server CPU.

  :samp:`power7`
    IBM POWER7 Server CPU.

  :samp:`power6x`
    IBM POWER6 Server CPU (RAW mode).

  :samp:`power6`
    IBM POWER6 Server CPU (Architected mode).

  :samp:`power5+`
    IBM POWER5+ Server CPU.

  :samp:`power5`
    IBM POWER5 Server CPU.

  :samp:`ppc970`
    IBM 970 Server CPU (ie, Apple G5).

  :samp:`power4`
    IBM POWER4 Server CPU.

  :samp:`ppca2`
    IBM A2 64-bit Embedded CPU

  :samp:`ppc476`
    IBM PowerPC 476FP 32-bit Embedded CPU.

  :samp:`ppc464`
    IBM PowerPC 464 32-bit Embedded CPU.

  :samp:`ppc440`
    PowerPC 440 32-bit Embedded CPU.

  :samp:`ppc405`
    PowerPC 405 32-bit Embedded CPU.

  :samp:`ppc-cell-be`
    IBM PowerPC Cell Broadband Engine Architecture CPU.

    Here is an example:

  .. code-block:: c++

    #ifdef __BUILTIN_CPU_SUPPORTS__
      if (__builtin_cpu_is ("power8"))
        {
           do_power8 (); // POWER8 specific implementation.
        }
      else
    #endif
        {
           do_generic (); // Generic implementation.
        }

.. function:: int __builtin_cpu_supports(const char* feature)

  This function returns a value of ``1`` if the run-time CPU supports the HWCAP
  feature :samp:`{feature}` and returns ``0`` otherwise.

  The ``__builtin_cpu_supports`` function requires GLIBC 2.23 or
  newer which exports the hardware capability bits.  GCC defines the
  macro ``__BUILTIN_CPU_SUPPORTS__`` if the
  ``__builtin_cpu_supports`` built-in function is fully supported.

  If GCC was configured to use a GLIBC before 2.23, the built-in
  function ``__builtin_cpu_suports`` always returns a 0 and the
  compiler issues a warning.

  The following features can be
  detected:

  :samp:`4xxmac`
    4xx CPU has a Multiply Accumulator.

  :samp:`altivec`
    CPU has a SIMD/Vector Unit.

  :samp:`arch_2_05`
    CPU supports ISA 2.05 (eg, POWER6)

  :samp:`arch_2_06`
    CPU supports ISA 2.06 (eg, POWER7)

  :samp:`arch_2_07`
    CPU supports ISA 2.07 (eg, POWER8)

  :samp:`arch_3_00`
    CPU supports ISA 3.0 (eg, POWER9)

  :samp:`arch_3_1`
    CPU supports ISA 3.1 (eg, POWER10)

  :samp:`archpmu`
    CPU supports the set of compatible performance monitoring events.

  :samp:`booke`
    CPU supports the Embedded ISA category.

  :samp:`cellbe`
    CPU has a CELL broadband engine.

  :samp:`darn`
    CPU supports the ``darn`` (deliver a random number) instruction.

  :samp:`dfp`
    CPU has a decimal floating point unit.

  :samp:`dscr`
    CPU supports the data stream control register.

  :samp:`ebb`
    CPU supports event base branching.

  :samp:`efpdouble`
    CPU has a SPE double precision floating point unit.

  :samp:`efpsingle`
    CPU has a SPE single precision floating point unit.

  :samp:`fpu`
    CPU has a floating point unit.

  :samp:`htm`
    CPU has hardware transaction memory instructions.

  :samp:`htm-nosc`
    Kernel aborts hardware transactions when a syscall is made.

  :samp:`htm-no-suspend`
    CPU supports hardware transaction memory but does not support the
    ``tsuspend.`` instruction.

  :samp:`ic_snoop`
    CPU supports icache snooping capabilities.

  :samp:`ieee128`
    CPU supports 128-bit IEEE binary floating point instructions.

  :samp:`isel`
    CPU supports the integer select instruction.

  :samp:`mma`
    CPU supports the matrix-multiply assist instructions.

  :samp:`mmu`
    CPU has a memory management unit.

  :samp:`notb`
    CPU does not have a timebase (eg, 601 and 403gx).

  :samp:`pa6t`
    CPU supports the PA Semi 6T CORE ISA.

  :samp:`power4`
    CPU supports ISA 2.00 (eg, POWER4)

  :samp:`power5`
    CPU supports ISA 2.02 (eg, POWER5)

  :samp:`power5+`
    CPU supports ISA 2.03 (eg, POWER5+)

  :samp:`power6x`
    CPU supports ISA 2.05 (eg, POWER6) extended opcodes mffgpr and mftgpr.

  :samp:`ppc32`
    CPU supports 32-bit mode execution.

  :samp:`ppc601`
    CPU supports the old POWER ISA (eg, 601)

  :samp:`ppc64`
    CPU supports 64-bit mode execution.

  :samp:`ppcle`
    CPU supports a little-endian mode that uses address swizzling.

  :samp:`scv`
    Kernel supports system call vectored.

  :samp:`smt`
    CPU support simultaneous multi-threading.

  :samp:`spe`
    CPU has a signal processing extension unit.

  :samp:`tar`
    CPU supports the target address register.

  :samp:`true_le`
    CPU supports true little-endian mode.

  :samp:`ucache`
    CPU has unified I/D cache.

  :samp:`vcrypto`
    CPU supports the vector cryptography instructions.

  :samp:`vsx`
    CPU supports the vector-scalar extension.

    Here is an example:

  .. code-block:: c++

    #ifdef __BUILTIN_CPU_SUPPORTS__
      if (__builtin_cpu_supports ("fpu"))
        {
           asm("fadd %0,%1,%2" : "=d"(dst) : "d"(src1), "d"(src2));
        }
      else
    #endif
        {
           dst = __fadd (src1, src2); // Software FP addition function.
        }

The following built-in functions are also available on all PowerPC
processors:

.. code-block:: c++

  uint64_t __builtin_ppc_get_timebase ();
  unsigned long __builtin_ppc_mftb ();
  double __builtin_unpack_ibm128 (__ibm128, int);
  __ibm128 __builtin_pack_ibm128 (double, double);
  double __builtin_mffs (void);
  void __builtin_mtfsf (const int, double);
  void __builtin_mtfsb0 (const int);
  void __builtin_mtfsb1 (const int);
  void __builtin_set_fpscr_rn (int);

The ``__builtin_ppc_get_timebase`` and ``__builtin_ppc_mftb``
functions generate instructions to read the Time Base Register.  The
``__builtin_ppc_get_timebase`` function may generate multiple
instructions and always returns the 64 bits of the Time Base Register.
The ``__builtin_ppc_mftb`` function always generates one instruction and
returns the Time Base Register value as an unsigned long, throwing away
the most significant word on 32-bit environments.  The ``__builtin_mffs``
return the value of the FPSCR register.  Note, ISA 3.0 supports the
``__builtin_mffsl()`` which permits software to read the control and
non-sticky status bits in the FSPCR without the higher latency associated with
accessing the sticky status bits.  The ``__builtin_mtfsf`` takes a constant
8-bit integer field mask and a double precision floating point argument
and generates the ``mtfsf`` (extended mnemonic) instruction to write new
values to selected fields of the FPSCR.  The
``__builtin_mtfsb0`` and ``__builtin_mtfsb1`` take the bit to change
as an argument.  The valid bit range is between 0 and 31.  The builtins map to
the ``mtfsb0`` and ``mtfsb1`` instructions which take the argument and
add 32.  Hence these instructions only modify the FPSCR[32:63] bits by
changing the specified bit to a zero or one respectively.  The
``__builtin_set_fpscr_rn`` builtin allows changing both of the floating
point rounding mode bits.  The argument is a 2-bit value.  The argument can
either be a ``const int`` or stored in a variable. The builtin uses
the ISA 3.0
instruction ``mffscrn`` if available, otherwise it reads the FPSCR, masks
the current rounding mode bits out and OR's in the new value.

.. _basic-powerpc-built-in-functions-available-on-isa-2.05:

Basic PowerPC Built-in Functions Available on ISA 2.05
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 2.05
or later.  Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power6` has the effect of
enabling the :option:`-mpowerpc64` , :option:`-mpowerpc-gpopt` ,
:option:`-mpowerpc-gfxopt` , :option:`-mmfcrf` , :option:`-mpopcntb` ,
:option:`-mfprnd` , :option:`-mcmpb` , :option:`-mhard-dfp` , and
:option:`-mrecip-precision` options.  Specify the
:option:`-maltivec` option explicitly in
combination with the above options if desired.

The following functions require option :option:`-mcmpb`.

.. code-block:: c++

  unsigned long long __builtin_cmpb (unsigned long long int, unsigned long long int);
  unsigned int __builtin_cmpb (unsigned int, unsigned int);

The ``__builtin_cmpb`` function
performs a byte-wise compare on the contents of its two arguments,
returning the result of the byte-wise comparison as the returned
value.  For each byte comparison, the corresponding byte of the return
value holds 0xff if the input bytes are equal and 0 if the input bytes
are not equal.  If either of the arguments to this built-in function
is wider than 32 bits, the function call expands into the form that
expects ``unsigned long long int`` arguments
which is only available on 64-bit targets.

The following built-in functions are available
when hardware decimal floating point
( :option:`-mhard-dfp` ) is available:

.. code-block:: c++

  void __builtin_set_fpscr_drn(int);
  _Decimal64 __builtin_ddedpd (int, _Decimal64);
  _Decimal128 __builtin_ddedpdq (int, _Decimal128);
  _Decimal64 __builtin_denbcd (int, _Decimal64);
  _Decimal128 __builtin_denbcdq (int, _Decimal128);
  _Decimal64 __builtin_diex (long long, _Decimal64);
  _Decimal128 _builtin_diexq (long long, _Decimal128);
  _Decimal64 __builtin_dscli (_Decimal64, int);
  _Decimal128 __builtin_dscliq (_Decimal128, int);
  _Decimal64 __builtin_dscri (_Decimal64, int);
  _Decimal128 __builtin_dscriq (_Decimal128, int);
  long long __builtin_dxex (_Decimal64);
  long long __builtin_dxexq (_Decimal128);
  _Decimal128 __builtin_pack_dec128 (unsigned long long, unsigned long long);
  unsigned long long __builtin_unpack_dec128 (_Decimal128, int);

  The __builtin_set_fpscr_drn builtin allows changing the three decimal
  floating point rounding mode bits.  The argument is a 3-bit value.  The
  argument can either be a const int or the value can be stored in
  a variable.
  The builtin uses the ISA 3.0 instruction mffscdrn if available.
  Otherwise the builtin reads the FPSCR, masks the current decimal rounding
  mode bits out and OR's in the new value.

The following functions require :option:`-mhard-float` ,
:option:`-mpowerpc-gfxopt` , and :option:`-mpopcntb` options.

.. code-block:: c++

  double __builtin_recipdiv (double, double);
  float __builtin_recipdivf (float, float);
  double __builtin_rsqrt (double);
  float __builtin_rsqrtf (float);

The ``vec_rsqrt``, ``__builtin_rsqrt``, and
``__builtin_rsqrtf`` functions generate multiple instructions to
implement the reciprocal sqrt functionality using reciprocal sqrt
estimate instructions.

The ``__builtin_recipdiv``, and ``__builtin_recipdivf``
functions generate multiple instructions to implement division using
the reciprocal estimate instructions.

The following functions require :option:`-mhard-float` and
:option:`-mmultiple` options.

The ``__builtin_unpack_longdouble`` function takes a
``long double`` argument and a compile time constant of 0 or 1.  If
the constant is 0, the first ``double`` within the
``long double`` is returned, otherwise the second ``double``
is returned.  The ``__builtin_unpack_longdouble`` function is only
available if ``long double`` uses the IBM extended double
representation.

The ``__builtin_pack_longdouble`` function takes two ``double``
arguments and returns a ``long double`` value that combines the two
arguments.  The ``__builtin_pack_longdouble`` function is only
available if ``long double`` uses the IBM extended double
representation.

The ``__builtin_unpack_ibm128`` function takes a ``__ibm128``
argument and a compile time constant of 0 or 1.  If the constant is 0,
the first ``double`` within the ``__ibm128`` is returned,
otherwise the second ``double`` is returned.

The ``__builtin_pack_ibm128`` function takes two ``double``
arguments and returns a ``__ibm128`` value that combines the two
arguments.

Additional built-in functions are available for the 64-bit PowerPC
family of processors, for efficient use of 128-bit floating point
(``__float128``) values.

.. _basic-powerpc-built-in-functions-available-on-isa-2.06:

Basic PowerPC Built-in Functions Available on ISA 2.06
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 2.05
or later.  Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power7` has the effect of
enabling all the same options as for :option:`-mcpu=power6` in
addition to the :option:`-maltivec` , :option:`-mpopcntd` , and
:option:`-mvsx` options.

The following basic built-in functions require :option:`-mpopcntd` :

.. code-block:: c++

  unsigned int __builtin_addg6s (unsigned int, unsigned int);
  long long __builtin_bpermd (long long, long long);
  unsigned int __builtin_cbcdtd (unsigned int);
  unsigned int __builtin_cdtbcd (unsigned int);
  long long __builtin_divde (long long, long long);
  unsigned long long __builtin_divdeu (unsigned long long, unsigned long long);
  int __builtin_divwe (int, int);
  unsigned int __builtin_divweu (unsigned int, unsigned int);
  vector __int128 __builtin_pack_vector_int128 (long long, long long);
  void __builtin_rs6000_speculation_barrier (void);
  long long __builtin_unpack_vector_int128 (vector __int128, signed char);

Of these, the ``__builtin_divde`` and ``__builtin_divdeu`` functions
require a 64-bit environment.

The following basic built-in functions, which are also supported on
x86 targets, require :option:`-mfloat128`.

.. code-block:: c++

  __float128 __builtin_fabsq (__float128);
  __float128 __builtin_copysignq (__float128, __float128);
  __float128 __builtin_infq (void);
  __float128 __builtin_huge_valq (void);
  __float128 __builtin_nanq (void);
  __float128 __builtin_nansq (void);

  __float128 __builtin_sqrtf128 (__float128);
  __float128 __builtin_fmaf128 (__float128, __float128, __float128);

.. _basic-powerpc-built-in-functions-available-on-isa-2.07:

Basic PowerPC Built-in Functions Available on ISA 2.07
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 2.07
or later.  Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power8` has the effect of
enabling all the same options as for :option:`-mcpu=power7` in
addition to the :option:`-mpower8-fusion` , :option:`-mpower8-vector` ,
:option:`-mcrypto` , :option:`-mhtm` , :option:`-mquad-memory` , and
:option:`-mquad-memory-atomic` options.

This section intentionally empty.

.. _basic-powerpc-built-in-functions-available-on-isa-3.0:

Basic PowerPC Built-in Functions Available on ISA 3.0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 3.0
or later.  Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power9` has the effect of
enabling all the same options as for :option:`-mcpu=power8` in
addition to the :option:`-misel` option.

The following built-in functions are available on Linux 64-bit systems
that use the ISA 3.0 instruction set ( :option:`-mcpu=power9` ):

``__float128 __builtin_addf128_round_to_odd (__float128, __float128)``
  Perform a 128-bit IEEE floating point add using round to odd as the
  rounding mode.

  .. index:: __builtin_addf128_round_to_odd

``__float128 __builtin_subf128_round_to_odd (__float128, __float128)``
  Perform a 128-bit IEEE floating point subtract using round to odd as
  the rounding mode.

  .. index:: __builtin_subf128_round_to_odd

``__float128 __builtin_mulf128_round_to_odd (__float128, __float128)``
  Perform a 128-bit IEEE floating point multiply using round to odd as
  the rounding mode.

  .. index:: __builtin_mulf128_round_to_odd

``__float128 __builtin_divf128_round_to_odd (__float128, __float128)``
  Perform a 128-bit IEEE floating point divide using round to odd as
  the rounding mode.

  .. index:: __builtin_divf128_round_to_odd

``__float128 __builtin_sqrtf128_round_to_odd (__float128)``
  Perform a 128-bit IEEE floating point square root using round to odd
  as the rounding mode.

  .. index:: __builtin_sqrtf128_round_to_odd

``__float128 __builtin_fmaf128_round_to_odd (__float128, __float128, __float128)``
  Perform a 128-bit IEEE floating point fused multiply and add operation
  using round to odd as the rounding mode.

  .. index:: __builtin_fmaf128_round_to_odd

``double __builtin_truncf128_round_to_odd (__float128)``
  Convert a 128-bit IEEE floating point value to ``double`` using
  round to odd as the rounding mode.

  .. index:: __builtin_truncf128_round_to_odd

  The following additional built-in functions are also available for the
PowerPC family of processors, starting with ISA 3.0 or later:

.. code-block:: c++

  long long __builtin_darn (void);
  long long __builtin_darn_raw (void);
  int __builtin_darn_32 (void);

The ``__builtin_darn`` and ``__builtin_darn_raw``
functions require a
64-bit environment supporting ISA 3.0 or later.
The ``__builtin_darn`` function provides a 64-bit conditioned
random number.  The ``__builtin_darn_raw`` function provides a
64-bit raw random number.  The ``__builtin_darn_32`` function
provides a 32-bit conditioned random number.

The following additional built-in functions are also available for the
PowerPC family of processors, starting with ISA 3.0 or later:

.. code-block:: c++

  int __builtin_byte_in_set (unsigned char u, unsigned long long set);
  int __builtin_byte_in_range (unsigned char u, unsigned int range);
  int __builtin_byte_in_either_range (unsigned char u, unsigned int ranges);

  int __builtin_dfp_dtstsfi_lt (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_lt (unsigned int comparison, _Decimal128 value);
  int __builtin_dfp_dtstsfi_lt_dd (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_lt_td (unsigned int comparison, _Decimal128 value);

  int __builtin_dfp_dtstsfi_gt (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_gt (unsigned int comparison, _Decimal128 value);
  int __builtin_dfp_dtstsfi_gt_dd (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_gt_td (unsigned int comparison, _Decimal128 value);

  int __builtin_dfp_dtstsfi_eq (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_eq (unsigned int comparison, _Decimal128 value);
  int __builtin_dfp_dtstsfi_eq_dd (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_eq_td (unsigned int comparison, _Decimal128 value);

  int __builtin_dfp_dtstsfi_ov (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_ov (unsigned int comparison, _Decimal128 value);
  int __builtin_dfp_dtstsfi_ov_dd (unsigned int comparison, _Decimal64 value);
  int __builtin_dfp_dtstsfi_ov_td (unsigned int comparison, _Decimal128 value);

  double __builtin_mffsl(void);

The ``__builtin_byte_in_set`` function requires a
64-bit environment supporting ISA 3.0 or later.  This function returns
a non-zero value if and only if its ``u`` argument exactly equals one of
the eight bytes contained within its 64-bit ``set`` argument.

The ``__builtin_byte_in_range`` and
``__builtin_byte_in_either_range`` require an environment
supporting ISA 3.0 or later.  For these two functions, the
``range`` argument is encoded as 4 bytes, organized as
``hi_1:lo_1:hi_2:lo_2``.
The ``__builtin_byte_in_range`` function returns a
non-zero value if and only if its ``u`` argument is within the
range bounded between ``lo_2`` and ``hi_2`` inclusive.
The ``__builtin_byte_in_either_range`` function returns non-zero if
and only if its ``u`` argument is within either the range bounded
between ``lo_1`` and ``hi_1`` inclusive or the range bounded
between ``lo_2`` and ``hi_2`` inclusive.

The ``__builtin_dfp_dtstsfi_lt`` function returns a non-zero value
if and only if the number of signficant digits of its ``value`` argument
is less than its ``comparison`` argument.  The
``__builtin_dfp_dtstsfi_lt_dd`` and
``__builtin_dfp_dtstsfi_lt_td`` functions behave similarly, but
require that the type of the ``value`` argument be
``__Decimal64`` and ``__Decimal128`` respectively.

The ``__builtin_dfp_dtstsfi_gt`` function returns a non-zero value
if and only if the number of signficant digits of its ``value`` argument
is greater than its ``comparison`` argument.  The
``__builtin_dfp_dtstsfi_gt_dd`` and
``__builtin_dfp_dtstsfi_gt_td`` functions behave similarly, but
require that the type of the ``value`` argument be
``__Decimal64`` and ``__Decimal128`` respectively.

The ``__builtin_dfp_dtstsfi_eq`` function returns a non-zero value
if and only if the number of signficant digits of its ``value`` argument
equals its ``comparison`` argument.  The
``__builtin_dfp_dtstsfi_eq_dd`` and
``__builtin_dfp_dtstsfi_eq_td`` functions behave similarly, but
require that the type of the ``value`` argument be
``__Decimal64`` and ``__Decimal128`` respectively.

The ``__builtin_dfp_dtstsfi_ov`` function returns a non-zero value
if and only if its ``value`` argument has an undefined number of
significant digits, such as when ``value`` is an encoding of ``NaN``.
The ``__builtin_dfp_dtstsfi_ov_dd`` and
``__builtin_dfp_dtstsfi_ov_td`` functions behave similarly, but
require that the type of the ``value`` argument be
``__Decimal64`` and ``__Decimal128`` respectively.

The ``__builtin_mffsl`` uses the ISA 3.0 ``mffsl`` instruction to read
the FPSCR.  The instruction is a lower latency version of the ``mffs``
instruction.  If the ``mffsl`` instruction is not available, then the
builtin uses the older ``mffs`` instruction to read the FPSCR.

.. _basic-powerpc-built-in-functions-available-on-isa-3.1:

Basic PowerPC Built-in Functions Available on ISA 3.1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 3.1.
Unless specific options are explicitly disabled on the
command line, specifying option :option:`-mcpu=power10` has the effect of
enabling all the same options as for :option:`-mcpu=power9`.

The following built-in functions are available on Linux 64-bit systems
that use a future architecture instruction set ( :option:`-mcpu=power10` ):

unsigned long long int__builtin_cfuged (unsigned long long int, unsigned long long int)Perform a 64-bit centrifuge operation, as if implemented by the
``cfuged`` instruction.

.. index:: __builtin_cfuged

unsigned long long int__builtin_cntlzdm (unsigned long long int, unsigned long long int)Perform a 64-bit count leading zeros operation under mask, as if
implemented by the ``cntlzdm`` instruction.

.. index:: __builtin_cntlzdm

unsigned long long int__builtin_cnttzdm (unsigned long long int, unsigned long long int)Perform a 64-bit count trailing zeros operation under mask, as if
implemented by the ``cnttzdm`` instruction.

.. index:: __builtin_cnttzdm

unsigned long long int__builtin_pdepd (unsigned long long int, unsigned long long int)Perform a 64-bit parallel bits deposit operation, as if implemented by the
``pdepd`` instruction.

.. index:: __builtin_pdepd

unsigned long long int__builtin_pextd (unsigned long long int, unsigned long long int)Perform a 64-bit parallel bits extract operation, as if implemented by the
``pextd`` instruction.

.. index:: __builtin_pextd

vector signed __int128 vsx_xl_sext (signed long long, signed char *);vector signed __int128 vsx_xl_sext (signed long long, signed short *);vector signed __int128 vsx_xl_sext (signed long long, signed int *);vector signed __int128 vsx_xl_sext (signed long long, signed long long *);vector unsigned __int128 vsx_xl_zext (signed long long, unsigned char *);vector unsigned __int128 vsx_xl_zext (signed long long, unsigned short *);vector unsigned __int128 vsx_xl_zext (signed long long, unsigned int *);vector unsigned __int128 vsx_xl_zext (signed long long, unsigned long long *);Load (and sign extend) to an __int128 vector, as if implemented by the ISA 3.1
``lxvrbx`` ``lxvrhx`` ``lxvrwx`` ``lxvrdx`` instructions.

.. index:: vsx_xl_sext

.. index:: vsx_xl_zext

void vec_xst_trunc (vector signed __int128, signed long long, signed char *);void vec_xst_trunc (vector signed __int128, signed long long, signed short *);void vec_xst_trunc (vector signed __int128, signed long long, signed int *);void vec_xst_trunc (vector signed __int128, signed long long, signed long long *);void vec_xst_trunc (vector unsigned __int128, signed long long, unsigned char *);void vec_xst_trunc (vector unsigned __int128, signed long long, unsigned short *);void vec_xst_trunc (vector unsigned __int128, signed long long, unsigned int *);void vec_xst_trunc (vector unsigned __int128, signed long long, unsigned long long *);Truncate and store the rightmost element of a vector, as if implemented by the
ISA 3.1 ``stxvrbx`` ``stxvrhx`` ``stxvrwx`` ``stxvrdx`` instructions.

.. index:: vec_xst_trunc

.. _powerpc-altivec-vsx-built-in-functions:

PowerPC AltiVec/VSX Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides an interface for the PowerPC family of processors to access
the AltiVec operations described in Motorola's AltiVec Programming
Interface Manual.  The interface is made available by including
``<altivec.h>`` and using :option:`-maltivec` and
:option:`-mabi=altivec`.  The interface supports the following vector
types.

.. code-block:: c++

  vector unsigned char
  vector signed char
  vector bool char

  vector unsigned short
  vector signed short
  vector bool short
  vector pixel

  vector unsigned int
  vector signed int
  vector bool int
  vector float

GCC's implementation of the high-level language interface available from
C and C++ code differs from Motorola's documentation in several ways.

* A vector constant is a list of constant expressions within curly braces.

* A vector initializer requires no cast if the vector constant is of the
  same type as the variable it is initializing.

* If ``signed`` or ``unsigned`` is omitted, the signedness of the
  vector type is the default signedness of the base type.  The default
  varies depending on the operating system, so a portable program should
  always specify the signedness.

* Compiling with :option:`-maltivec` adds keywords ``__vector``,
  ``vector``, ``__pixel``, ``pixel``, ``__bool`` and
  ``bool``.  When compiling ISO C, the context-sensitive substitution
  of the keywords ``vector``, ``pixel`` and ``bool`` is
  disabled.  To use them, you must include ``<altivec.h>`` instead.

* GCC allows using a ``typedef`` name as the type specifier for a
  vector type, but only under the following circumstances:

  * When using ``__vector`` instead of ``vector``; for example,

    .. code-block:: c++

      typedef signed short int16;
      __vector int16 data;

  * When using ``vector`` in keyword-and-predefine mode; for example,

    .. code-block:: c++

      typedef signed short int16;
      vector int16 data;

    Note that keyword-and-predefine mode is enabled by disabling GNU
    extensions (e.g., by using ``-std=c11``) and including
    ``<altivec.h>``.

* For C, overloaded functions are implemented with macros so the following
  does not work:

  .. code-block:: c++

      vec_add ((vector signed int){1, 2, 3, 4}, foo);

  Since ``vec_add`` is a macro, the vector constant in the example
  is treated as four separate arguments.  Wrap the entire argument in
  parentheses for this to work.

*Note:* Only the ``<altivec.h>`` interface is supported.
Internally, GCC uses built-in functions to achieve the functionality in
the aforementioned header file, but they are not supported and are
subject to change without notice.

GCC complies with the Power Vector Intrinsic Programming Reference (PVIPR),
which may be found at
https://openpowerfoundation.org/?resource_lib=power-vector-intrinsic-programming-reference.
Chapter 4 of this document fully documents the vector API interfaces
that must be
provided by compliant compilers.  Programmers should preferentially use
the interfaces described therein.  However, historically GCC has provided
additional interfaces for access to vector instructions.  These are
briefly described below.  Where the PVIPR provides a portable interface,
other functions in GCC that provide the same capabilities should be
considered deprecated.

The PVIPR documents the following overloaded functions:

================================  ================================  ===================================
``vec_abs``                       ``vec_absd``                      ``vec_abss``
================================  ================================  ===================================
``vec_add``                       ``vec_addc``                      ``vec_adde``
``vec_addec``                     ``vec_adds``                      ``vec_all_eq``
``vec_all_ge``                    ``vec_all_gt``                    ``vec_all_in``
``vec_all_le``                    ``vec_all_lt``                    ``vec_all_nan``
``vec_all_ne``                    ``vec_all_nge``                   ``vec_all_ngt``
``vec_all_nle``                   ``vec_all_nlt``                   ``vec_all_numeric``
``vec_and``                       ``vec_andc``                      ``vec_any_eq``
``vec_any_ge``                    ``vec_any_gt``                    ``vec_any_le``
``vec_any_lt``                    ``vec_any_nan``                   ``vec_any_ne``
``vec_any_nge``                   ``vec_any_ngt``                   ``vec_any_nle``
``vec_any_nlt``                   ``vec_any_numeric``               ``vec_any_out``
``vec_avg``                       ``vec_bperm``                     ``vec_ceil``
``vec_cipher_be``                 ``vec_cipherlast_be``             ``vec_cmpb``
``vec_cmpeq``                     ``vec_cmpge``                     ``vec_cmpgt``
``vec_cmple``                     ``vec_cmplt``                     ``vec_cmpne``
``vec_cmpnez``                    ``vec_cntlz``                     ``vec_cntlz_lsbb``
``vec_cnttz``                     ``vec_cnttz_lsbb``                ``vec_cpsgn``
``vec_ctf``                       ``vec_cts``                       ``vec_ctu``
``vec_div``                       ``vec_double``                    ``vec_doublee``
``vec_doubleh``                   ``vec_doublel``                   ``vec_doubleo``
``vec_eqv``                       ``vec_expte``                     ``vec_extract``
``vec_extract_exp``               ``vec_extract_fp32_from_shorth``  ``vec_extract_fp32_from_shortl``
``vec_extract_sig``               ``vec_extract_4b``                ``vec_first_match_index``
``vec_first_match_or_eos_index``  ``vec_first_mismatch_index``      ``vec_first_mismatch_or_eos_index``
``vec_float``                     ``vec_float2``                    ``vec_floate``
``vec_floato``                    ``vec_floor``                     ``vec_gb``
``vec_insert``                    ``vec_insert_exp``                ``vec_insert4b``
``vec_ld``                        ``vec_lde``                       ``vec_ldl``
``vec_loge``                      ``vec_madd``                      ``vec_madds``
``vec_max``                       ``vec_mergee``                    ``vec_mergeh``
``vec_mergel``                    ``vec_mergeo``                    ``vec_mfvscr``
``vec_min``                       ``vec_mradds``                    ``vec_msub``
``vec_msum``                      ``vec_msums``                     ``vec_mtvscr``
``vec_mul``                       ``vec_mule``                      ``vec_mulo``
``vec_nabs``                      ``vec_nand``                      ``vec_ncipher_be``
``vec_ncipherlast_be``            ``vec_nearbyint``                 ``vec_neg``
``vec_nmadd``                     ``vec_nmsub``                     ``vec_nor``
``vec_or``                        ``vec_orc``                       ``vec_pack``
``vec_pack_to_short_fp32``        ``vec_packpx``                    ``vec_packs``
``vec_packsu``                    ``vec_parity_lsbb``               ``vec_perm``
``vec_permxor``                   ``vec_pmsum_be``                  ``vec_popcnt``
``vec_re``                        ``vec_recipdiv``                  ``vec_revb``
``vec_reve``                      ``vec_rint``                      ``vec_rl``
``vec_rlmi``                      ``vec_rlnm``                      ``vec_round``
``vec_rsqrt``                     ``vec_rsqrte``                    ``vec_sbox_be``
``vec_sel``                       ``vec_shasigma_be``               ``vec_signed``
``vec_signed2``                   ``vec_signede``                   ``vec_signedo``
``vec_sl``                        ``vec_sld``                       ``vec_sldw``
``vec_sll``                       ``vec_slo``                       ``vec_slv``
``vec_splat``                     ``vec_splat_s8``                  ``vec_splat_s16``
``vec_splat_s32``                 ``vec_splat_u8``                  ``vec_splat_u16``
``vec_splat_u32``                 ``vec_splats``                    ``vec_sqrt``
``vec_sr``                        ``vec_sra``                       ``vec_srl``
``vec_sro``                       ``vec_srv``                       ``vec_st``
``vec_ste``                       ``vec_stl``                       ``vec_sub``
``vec_subc``                      ``vec_sube``                      ``vec_subec``
``vec_subs``                      ``vec_sum2s``                     ``vec_sum4s``
``vec_sums``                      ``vec_test_data_class``           ``vec_trunc``
``vec_unpackh``                   ``vec_unpackl``                   ``vec_unsigned``
``vec_unsigned2``                 ``vec_unsignede``                 ``vec_unsignedo``
``vec_xl``                        ``vec_xl_be``                     ``vec_xl_len``
``vec_xl_len_r``                  ``vec_xor``                       ``vec_xst``
``vec_xst_be``                    ``vec_xst_len``                   ``vec_xst_len_r``
================================  ================================  ===================================

.. toctree::

   <powerpc-altivec-built-in-functions-on-isa-2.05>
   <powerpc-altivec-built-in-functions-available-on-isa-2.06>
   <powerpc-altivec-built-in-functions-available-on-isa-2.07>
   <powerpc-altivec-built-in-functions-available-on-isa-3.0>
   <powerpc-altivec-built-in-functions-available-on-isa-3.1>

.. _powerpc-altivec-built-in-functions-on-isa-2.05:

PowerPC AltiVec Built-in Functions on ISA 2.05
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following interfaces are supported for the generic and specific
AltiVec operations and the AltiVec predicates.  In cases where there
is a direct mapping between generic and specific operations, only the
generic names are shown here, although the specific operations can also
be used.

Arguments that are documented as ``const int`` require literal
integral values within the range required for that operation.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  void vec_dss (const int);

  void vec_dssall (void);

  void vec_dst (const vector unsigned char *, int, const int);
  void vec_dst (const vector signed char *, int, const int);
  void vec_dst (const vector bool char *, int, const int);
  void vec_dst (const vector unsigned short *, int, const int);
  void vec_dst (const vector signed short *, int, const int);
  void vec_dst (const vector bool short *, int, const int);
  void vec_dst (const vector pixel *, int, const int);
  void vec_dst (const vector unsigned int *, int, const int);
  void vec_dst (const vector signed int *, int, const int);
  void vec_dst (const vector bool int *, int, const int);
  void vec_dst (const vector float *, int, const int);
  void vec_dst (const unsigned char *, int, const int);
  void vec_dst (const signed char *, int, const int);
  void vec_dst (const unsigned short *, int, const int);
  void vec_dst (const short *, int, const int);
  void vec_dst (const unsigned int *, int, const int);
  void vec_dst (const int *, int, const int);
  void vec_dst (const float *, int, const int);

  void vec_dstst (const vector unsigned char *, int, const int);
  void vec_dstst (const vector signed char *, int, const int);
  void vec_dstst (const vector bool char *, int, const int);
  void vec_dstst (const vector unsigned short *, int, const int);
  void vec_dstst (const vector signed short *, int, const int);
  void vec_dstst (const vector bool short *, int, const int);
  void vec_dstst (const vector pixel *, int, const int);
  void vec_dstst (const vector unsigned int *, int, const int);
  void vec_dstst (const vector signed int *, int, const int);
  void vec_dstst (const vector bool int *, int, const int);
  void vec_dstst (const vector float *, int, const int);
  void vec_dstst (const unsigned char *, int, const int);
  void vec_dstst (const signed char *, int, const int);
  void vec_dstst (const unsigned short *, int, const int);
  void vec_dstst (const short *, int, const int);
  void vec_dstst (const unsigned int *, int, const int);
  void vec_dstst (const int *, int, const int);
  void vec_dstst (const unsigned long *, int, const int);
  void vec_dstst (const long *, int, const int);
  void vec_dstst (const float *, int, const int);

  void vec_dststt (const vector unsigned char *, int, const int);
  void vec_dststt (const vector signed char *, int, const int);
  void vec_dststt (const vector bool char *, int, const int);
  void vec_dststt (const vector unsigned short *, int, const int);
  void vec_dststt (const vector signed short *, int, const int);
  void vec_dststt (const vector bool short *, int, const int);
  void vec_dststt (const vector pixel *, int, const int);
  void vec_dststt (const vector unsigned int *, int, const int);
  void vec_dststt (const vector signed int *, int, const int);
  void vec_dststt (const vector bool int *, int, const int);
  void vec_dststt (const vector float *, int, const int);
  void vec_dststt (const unsigned char *, int, const int);
  void vec_dststt (const signed char *, int, const int);
  void vec_dststt (const unsigned short *, int, const int);
  void vec_dststt (const short *, int, const int);
  void vec_dststt (const unsigned int *, int, const int);
  void vec_dststt (const int *, int, const int);
  void vec_dststt (const float *, int, const int);

  void vec_dstt (const vector unsigned char *, int, const int);
  void vec_dstt (const vector signed char *, int, const int);
  void vec_dstt (const vector bool char *, int, const int);
  void vec_dstt (const vector unsigned short *, int, const int);
  void vec_dstt (const vector signed short *, int, const int);
  void vec_dstt (const vector bool short *, int, const int);
  void vec_dstt (const vector pixel *, int, const int);
  void vec_dstt (const vector unsigned int *, int, const int);
  void vec_dstt (const vector signed int *, int, const int);
  void vec_dstt (const vector bool int *, int, const int);
  void vec_dstt (const vector float *, int, const int);
  void vec_dstt (const unsigned char *, int, const int);
  void vec_dstt (const signed char *, int, const int);
  void vec_dstt (const unsigned short *, int, const int);
  void vec_dstt (const short *, int, const int);
  void vec_dstt (const unsigned int *, int, const int);
  void vec_dstt (const int *, int, const int);
  void vec_dstt (const float *, int, const int);

  vector signed char vec_lvebx (int, char *);
  vector unsigned char vec_lvebx (int, unsigned char *);

  vector signed short vec_lvehx (int, short *);
  vector unsigned short vec_lvehx (int, unsigned short *);

  vector float vec_lvewx (int, float *);
  vector signed int vec_lvewx (int, int *);
  vector unsigned int vec_lvewx (int, unsigned int *);

  vector unsigned char vec_lvsl (int, const unsigned char *);
  vector unsigned char vec_lvsl (int, const signed char *);
  vector unsigned char vec_lvsl (int, const unsigned short *);
  vector unsigned char vec_lvsl (int, const short *);
  vector unsigned char vec_lvsl (int, const unsigned int *);
  vector unsigned char vec_lvsl (int, const int *);
  vector unsigned char vec_lvsl (int, const float *);

  vector unsigned char vec_lvsr (int, const unsigned char *);
  vector unsigned char vec_lvsr (int, const signed char *);
  vector unsigned char vec_lvsr (int, const unsigned short *);
  vector unsigned char vec_lvsr (int, const short *);
  vector unsigned char vec_lvsr (int, const unsigned int *);
  vector unsigned char vec_lvsr (int, const int *);
  vector unsigned char vec_lvsr (int, const float *);

  void vec_stvebx (vector signed char, int, signed char *);
  void vec_stvebx (vector unsigned char, int, unsigned char *);
  void vec_stvebx (vector bool char, int, signed char *);
  void vec_stvebx (vector bool char, int, unsigned char *);

  void vec_stvehx (vector signed short, int, short *);
  void vec_stvehx (vector unsigned short, int, unsigned short *);
  void vec_stvehx (vector bool short, int, short *);
  void vec_stvehx (vector bool short, int, unsigned short *);

  void vec_stvewx (vector float, int, float *);
  void vec_stvewx (vector signed int, int, int *);
  void vec_stvewx (vector unsigned int, int, unsigned int *);
  void vec_stvewx (vector bool int, int, int *);
  void vec_stvewx (vector bool int, int, unsigned int *);

  vector float vec_vaddfp (vector float, vector float);

  vector signed char vec_vaddsbs (vector bool char, vector signed char);
  vector signed char vec_vaddsbs (vector signed char, vector bool char);
  vector signed char vec_vaddsbs (vector signed char, vector signed char);

  vector signed short vec_vaddshs (vector bool short, vector signed short);
  vector signed short vec_vaddshs (vector signed short, vector bool short);
  vector signed short vec_vaddshs (vector signed short, vector signed short);

  vector signed int vec_vaddsws (vector bool int, vector signed int);
  vector signed int vec_vaddsws (vector signed int, vector bool int);
  vector signed int vec_vaddsws (vector signed int, vector signed int);

  vector signed char vec_vaddubm (vector bool char, vector signed char);
  vector signed char vec_vaddubm (vector signed char, vector bool char);
  vector signed char vec_vaddubm (vector signed char, vector signed char);
  vector unsigned char vec_vaddubm (vector bool char, vector unsigned char);
  vector unsigned char vec_vaddubm (vector unsigned char, vector bool char);
  vector unsigned char vec_vaddubm (vector unsigned char, vector unsigned char);

  vector unsigned char vec_vaddubs (vector bool char, vector unsigned char);
  vector unsigned char vec_vaddubs (vector unsigned char, vector bool char);
  vector unsigned char vec_vaddubs (vector unsigned char, vector unsigned char);

  vector signed short vec_vadduhm (vector bool short, vector signed short);
  vector signed short vec_vadduhm (vector signed short, vector bool short);
  vector signed short vec_vadduhm (vector signed short, vector signed short);
  vector unsigned short vec_vadduhm (vector bool short, vector unsigned short);
  vector unsigned short vec_vadduhm (vector unsigned short, vector bool short);
  vector unsigned short vec_vadduhm (vector unsigned short, vector unsigned short);

  vector unsigned short vec_vadduhs (vector bool short, vector unsigned short);
  vector unsigned short vec_vadduhs (vector unsigned short, vector bool short);
  vector unsigned short vec_vadduhs (vector unsigned short, vector unsigned short);

  vector signed int vec_vadduwm (vector bool int, vector signed int);
  vector signed int vec_vadduwm (vector signed int, vector bool int);
  vector signed int vec_vadduwm (vector signed int, vector signed int);
  vector unsigned int vec_vadduwm (vector bool int, vector unsigned int);
  vector unsigned int vec_vadduwm (vector unsigned int, vector bool int);
  vector unsigned int vec_vadduwm (vector unsigned int, vector unsigned int);

  vector unsigned int vec_vadduws (vector bool int, vector unsigned int);
  vector unsigned int vec_vadduws (vector unsigned int, vector bool int);
  vector unsigned int vec_vadduws (vector unsigned int, vector unsigned int);

  vector signed char vec_vavgsb (vector signed char, vector signed char);

  vector signed short vec_vavgsh (vector signed short, vector signed short);

  vector signed int vec_vavgsw (vector signed int, vector signed int);

  vector unsigned char vec_vavgub (vector unsigned char, vector unsigned char);

  vector unsigned short vec_vavguh (vector unsigned short, vector unsigned short);

  vector unsigned int vec_vavguw (vector unsigned int, vector unsigned int);

  vector float vec_vcfsx (vector signed int, const int);

  vector float vec_vcfux (vector unsigned int, const int);

  vector bool int vec_vcmpeqfp (vector float, vector float);

  vector bool char vec_vcmpequb (vector signed char, vector signed char);
  vector bool char vec_vcmpequb (vector unsigned char, vector unsigned char);

  vector bool short vec_vcmpequh (vector signed short, vector signed short);
  vector bool short vec_vcmpequh (vector unsigned short, vector unsigned short);

  vector bool int vec_vcmpequw (vector signed int, vector signed int);
  vector bool int vec_vcmpequw (vector unsigned int, vector unsigned int);

  vector bool int vec_vcmpgtfp (vector float, vector float);

  vector bool char vec_vcmpgtsb (vector signed char, vector signed char);

  vector bool short vec_vcmpgtsh (vector signed short, vector signed short);

  vector bool int vec_vcmpgtsw (vector signed int, vector signed int);

  vector bool char vec_vcmpgtub (vector unsigned char, vector unsigned char);

  vector bool short vec_vcmpgtuh (vector unsigned short, vector unsigned short);

  vector bool int vec_vcmpgtuw (vector unsigned int, vector unsigned int);

  vector float vec_vmaxfp (vector float, vector float);

  vector signed char vec_vmaxsb (vector bool char, vector signed char);
  vector signed char vec_vmaxsb (vector signed char, vector bool char);
  vector signed char vec_vmaxsb (vector signed char, vector signed char);

  vector signed short vec_vmaxsh (vector bool short, vector signed short);
  vector signed short vec_vmaxsh (vector signed short, vector bool short);
  vector signed short vec_vmaxsh (vector signed short, vector signed short);

  vector signed int vec_vmaxsw (vector bool int, vector signed int);
  vector signed int vec_vmaxsw (vector signed int, vector bool int);
  vector signed int vec_vmaxsw (vector signed int, vector signed int);

  vector unsigned char vec_vmaxub (vector bool char, vector unsigned char);
  vector unsigned char vec_vmaxub (vector unsigned char, vector bool char);
  vector unsigned char vec_vmaxub (vector unsigned char, vector unsigned char);

  vector unsigned short vec_vmaxuh (vector bool short, vector unsigned short);
  vector unsigned short vec_vmaxuh (vector unsigned short, vector bool short);
  vector unsigned short vec_vmaxuh (vector unsigned short, vector unsigned short);

  vector unsigned int vec_vmaxuw (vector bool int, vector unsigned int);
  vector unsigned int vec_vmaxuw (vector unsigned int, vector bool int);
  vector unsigned int vec_vmaxuw (vector unsigned int, vector unsigned int);

  vector float vec_vminfp (vector float, vector float);

  vector signed char vec_vminsb (vector bool char, vector signed char);
  vector signed char vec_vminsb (vector signed char, vector bool char);
  vector signed char vec_vminsb (vector signed char, vector signed char);

  vector signed short vec_vminsh (vector bool short, vector signed short);
  vector signed short vec_vminsh (vector signed short, vector bool short);
  vector signed short vec_vminsh (vector signed short, vector signed short);

  vector signed int vec_vminsw (vector bool int, vector signed int);
  vector signed int vec_vminsw (vector signed int, vector bool int);
  vector signed int vec_vminsw (vector signed int, vector signed int);

  vector unsigned char vec_vminub (vector bool char, vector unsigned char);
  vector unsigned char vec_vminub (vector unsigned char, vector bool char);
  vector unsigned char vec_vminub (vector unsigned char, vector unsigned char);

  vector unsigned short vec_vminuh (vector bool short, vector unsigned short);
  vector unsigned short vec_vminuh (vector unsigned short, vector bool short);
  vector unsigned short vec_vminuh (vector unsigned short, vector unsigned short);

  vector unsigned int vec_vminuw (vector bool int, vector unsigned int);
  vector unsigned int vec_vminuw (vector unsigned int, vector bool int);
  vector unsigned int vec_vminuw (vector unsigned int, vector unsigned int);

  vector bool char vec_vmrghb (vector bool char, vector bool char);
  vector signed char vec_vmrghb (vector signed char, vector signed char);
  vector unsigned char vec_vmrghb (vector unsigned char, vector unsigned char);

  vector bool short vec_vmrghh (vector bool short, vector bool short);
  vector signed short vec_vmrghh (vector signed short, vector signed short);
  vector unsigned short vec_vmrghh (vector unsigned short, vector unsigned short);
  vector pixel vec_vmrghh (vector pixel, vector pixel);

  vector float vec_vmrghw (vector float, vector float);
  vector bool int vec_vmrghw (vector bool int, vector bool int);
  vector signed int vec_vmrghw (vector signed int, vector signed int);
  vector unsigned int vec_vmrghw (vector unsigned int, vector unsigned int);

  vector bool char vec_vmrglb (vector bool char, vector bool char);
  vector signed char vec_vmrglb (vector signed char, vector signed char);
  vector unsigned char vec_vmrglb (vector unsigned char, vector unsigned char);

  vector bool short vec_vmrglh (vector bool short, vector bool short);
  vector signed short vec_vmrglh (vector signed short, vector signed short);
  vector unsigned short vec_vmrglh (vector unsigned short, vector unsigned short);
  vector pixel vec_vmrglh (vector pixel, vector pixel);

  vector float vec_vmrglw (vector float, vector float);
  vector signed int vec_vmrglw (vector signed int, vector signed int);
  vector unsigned int vec_vmrglw (vector unsigned int, vector unsigned int);
  vector bool int vec_vmrglw (vector bool int, vector bool int);

  vector signed int vec_vmsummbm (vector signed char, vector unsigned char,
                                  vector signed int);

  vector signed int vec_vmsumshm (vector signed short, vector signed short,
                                  vector signed int);

  vector signed int vec_vmsumshs (vector signed short, vector signed short,
                                  vector signed int);

  vector unsigned int vec_vmsumubm (vector unsigned char, vector unsigned char,
                                    vector unsigned int);

  vector unsigned int vec_vmsumuhm (vector unsigned short, vector unsigned short,
                                    vector unsigned int);

  vector unsigned int vec_vmsumuhs (vector unsigned short, vector unsigned short,
                                    vector unsigned int);

  vector signed short vec_vmulesb (vector signed char, vector signed char);

  vector signed int vec_vmulesh (vector signed short, vector signed short);

  vector unsigned short vec_vmuleub (vector unsigned char, vector unsigned char);

  vector unsigned int vec_vmuleuh (vector unsigned short, vector unsigned short);

  vector signed short vec_vmulosb (vector signed char, vector signed char);

  vector signed int vec_vmulosh (vector signed short, vector signed short);

  vector unsigned short vec_vmuloub (vector unsigned char, vector unsigned char);

  vector unsigned int vec_vmulouh (vector unsigned short, vector unsigned short);

  vector signed char vec_vpkshss (vector signed short, vector signed short);

  vector unsigned char vec_vpkshus (vector signed short, vector signed short);

  vector signed short vec_vpkswss (vector signed int, vector signed int);

  vector unsigned short vec_vpkswus (vector signed int, vector signed int);

  vector bool char vec_vpkuhum (vector bool short, vector bool short);
  vector signed char vec_vpkuhum (vector signed short, vector signed short);
  vector unsigned char vec_vpkuhum (vector unsigned short, vector unsigned short);

  vector unsigned char vec_vpkuhus (vector unsigned short, vector unsigned short);

  vector bool short vec_vpkuwum (vector bool int, vector bool int);
  vector signed short vec_vpkuwum (vector signed int, vector signed int);
  vector unsigned short vec_vpkuwum (vector unsigned int, vector unsigned int);

  vector unsigned short vec_vpkuwus (vector unsigned int, vector unsigned int);

  vector signed char vec_vrlb (vector signed char, vector unsigned char);
  vector unsigned char vec_vrlb (vector unsigned char, vector unsigned char);

  vector signed short vec_vrlh (vector signed short, vector unsigned short);
  vector unsigned short vec_vrlh (vector unsigned short, vector unsigned short);

  vector signed int vec_vrlw (vector signed int, vector unsigned int);
  vector unsigned int vec_vrlw (vector unsigned int, vector unsigned int);

  vector signed char vec_vslb (vector signed char, vector unsigned char);
  vector unsigned char vec_vslb (vector unsigned char, vector unsigned char);

  vector signed short vec_vslh (vector signed short, vector unsigned short);
  vector unsigned short vec_vslh (vector unsigned short, vector unsigned short);

  vector signed int vec_vslw (vector signed int, vector unsigned int);
  vector unsigned int vec_vslw (vector unsigned int, vector unsigned int);

  vector signed char vec_vspltb (vector signed char, const int);
  vector unsigned char vec_vspltb (vector unsigned char, const int);
  vector bool char vec_vspltb (vector bool char, const int);

  vector bool short vec_vsplth (vector bool short, const int);
  vector signed short vec_vsplth (vector signed short, const int);
  vector unsigned short vec_vsplth (vector unsigned short, const int);
  vector pixel vec_vsplth (vector pixel, const int);

  vector float vec_vspltw (vector float, const int);
  vector signed int vec_vspltw (vector signed int, const int);
  vector unsigned int vec_vspltw (vector unsigned int, const int);
  vector bool int vec_vspltw (vector bool int, const int);

  vector signed char vec_vsrab (vector signed char, vector unsigned char);
  vector unsigned char vec_vsrab (vector unsigned char, vector unsigned char);

  vector signed short vec_vsrah (vector signed short, vector unsigned short);
  vector unsigned short vec_vsrah (vector unsigned short, vector unsigned short);

  vector signed int vec_vsraw (vector signed int, vector unsigned int);
  vector unsigned int vec_vsraw (vector unsigned int, vector unsigned int);

  vector signed char vec_vsrb (vector signed char, vector unsigned char);
  vector unsigned char vec_vsrb (vector unsigned char, vector unsigned char);

  vector signed short vec_vsrh (vector signed short, vector unsigned short);
  vector unsigned short vec_vsrh (vector unsigned short, vector unsigned short);

  vector signed int vec_vsrw (vector signed int, vector unsigned int);
  vector unsigned int vec_vsrw (vector unsigned int, vector unsigned int);

  vector float vec_vsubfp (vector float, vector float);

  vector signed char vec_vsubsbs (vector bool char, vector signed char);
  vector signed char vec_vsubsbs (vector signed char, vector bool char);
  vector signed char vec_vsubsbs (vector signed char, vector signed char);

  vector signed short vec_vsubshs (vector bool short, vector signed short);
  vector signed short vec_vsubshs (vector signed short, vector bool short);
  vector signed short vec_vsubshs (vector signed short, vector signed short);

  vector signed int vec_vsubsws (vector bool int, vector signed int);
  vector signed int vec_vsubsws (vector signed int, vector bool int);
  vector signed int vec_vsubsws (vector signed int, vector signed int);

  vector signed char vec_vsububm (vector bool char, vector signed char);
  vector signed char vec_vsububm (vector signed char, vector bool char);
  vector signed char vec_vsububm (vector signed char, vector signed char);
  vector unsigned char vec_vsububm (vector bool char, vector unsigned char);
  vector unsigned char vec_vsububm (vector unsigned char, vector bool char);
  vector unsigned char vec_vsububm (vector unsigned char, vector unsigned char);

  vector unsigned char vec_vsububs (vector bool char, vector unsigned char);
  vector unsigned char vec_vsububs (vector unsigned char, vector bool char);
  vector unsigned char vec_vsububs (vector unsigned char, vector unsigned char);

  vector signed short vec_vsubuhm (vector bool short, vector signed short);
  vector signed short vec_vsubuhm (vector signed short, vector bool short);
  vector signed short vec_vsubuhm (vector signed short, vector signed short);
  vector unsigned short vec_vsubuhm (vector bool short, vector unsigned short);
  vector unsigned short vec_vsubuhm (vector unsigned short, vector bool short);
  vector unsigned short vec_vsubuhm (vector unsigned short, vector unsigned short);

  vector unsigned short vec_vsubuhs (vector bool short, vector unsigned short);
  vector unsigned short vec_vsubuhs (vector unsigned short, vector bool short);
  vector unsigned short vec_vsubuhs (vector unsigned short, vector unsigned short);

  vector signed int vec_vsubuwm (vector bool int, vector signed int);
  vector signed int vec_vsubuwm (vector signed int, vector bool int);
  vector signed int vec_vsubuwm (vector signed int, vector signed int);
  vector unsigned int vec_vsubuwm (vector bool int, vector unsigned int);
  vector unsigned int vec_vsubuwm (vector unsigned int, vector bool int);
  vector unsigned int vec_vsubuwm (vector unsigned int, vector unsigned int);

  vector unsigned int vec_vsubuws (vector bool int, vector unsigned int);
  vector unsigned int vec_vsubuws (vector unsigned int, vector bool int);
  vector unsigned int vec_vsubuws (vector unsigned int, vector unsigned int);

  vector signed int vec_vsum4sbs (vector signed char, vector signed int);

  vector signed int vec_vsum4shs (vector signed short, vector signed int);

  vector unsigned int vec_vsum4ubs (vector unsigned char, vector unsigned int);

  vector unsigned int vec_vupkhpx (vector pixel);

  vector bool short vec_vupkhsb (vector bool char);
  vector signed short vec_vupkhsb (vector signed char);

  vector bool int vec_vupkhsh (vector bool short);
  vector signed int vec_vupkhsh (vector signed short);

  vector unsigned int vec_vupklpx (vector pixel);

  vector bool short vec_vupklsb (vector bool char);
  vector signed short vec_vupklsb (vector signed char);

  vector bool int vec_vupklsh (vector bool short);
  vector signed int vec_vupklsh (vector signed short);

.. _powerpc-altivec-built-in-functions-available-on-isa-2.06:

PowerPC AltiVec Built-in Functions Available on ISA 2.06
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The AltiVec built-in functions described in this section are
available on the PowerPC family of processors starting with ISA 2.06
or later.  These are normally enabled by adding :option:`-mvsx` to the
command line.

When :option:`-mvsx` is used, the following additional vector types are
implemented.

.. code-block:: c++

  vector unsigned __int128
  vector signed __int128
  vector unsigned long long int
  vector signed long long int
  vector double

The long long types are only implemented for 64-bit code generation.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  void vec_dst (const unsigned long *, int, const int);
  void vec_dst (const long *, int, const int);

  void vec_dststt (const unsigned long *, int, const int);
  void vec_dststt (const long *, int, const int);

  void vec_dstt (const unsigned long *, int, const int);
  void vec_dstt (const long *, int, const int);

  vector unsigned char vec_lvsl (int, const unsigned long *);
  vector unsigned char vec_lvsl (int, const long *);

  vector unsigned char vec_lvsr (int, const unsigned long *);
  vector unsigned char vec_lvsr (int, const long *);

  vector unsigned char vec_lvsl (int, const double *);
  vector unsigned char vec_lvsr (int, const double *);

  vector double vec_vsx_ld (int, const vector double *);
  vector double vec_vsx_ld (int, const double *);
  vector float vec_vsx_ld (int, const vector float *);
  vector float vec_vsx_ld (int, const float *);
  vector bool int vec_vsx_ld (int, const vector bool int *);
  vector signed int vec_vsx_ld (int, const vector signed int *);
  vector signed int vec_vsx_ld (int, const int *);
  vector signed int vec_vsx_ld (int, const long *);
  vector unsigned int vec_vsx_ld (int, const vector unsigned int *);
  vector unsigned int vec_vsx_ld (int, const unsigned int *);
  vector unsigned int vec_vsx_ld (int, const unsigned long *);
  vector bool short vec_vsx_ld (int, const vector bool short *);
  vector pixel vec_vsx_ld (int, const vector pixel *);
  vector signed short vec_vsx_ld (int, const vector signed short *);
  vector signed short vec_vsx_ld (int, const short *);
  vector unsigned short vec_vsx_ld (int, const vector unsigned short *);
  vector unsigned short vec_vsx_ld (int, const unsigned short *);
  vector bool char vec_vsx_ld (int, const vector bool char *);
  vector signed char vec_vsx_ld (int, const vector signed char *);
  vector signed char vec_vsx_ld (int, const signed char *);
  vector unsigned char vec_vsx_ld (int, const vector unsigned char *);
  vector unsigned char vec_vsx_ld (int, const unsigned char *);

  void vec_vsx_st (vector double, int, vector double *);
  void vec_vsx_st (vector double, int, double *);
  void vec_vsx_st (vector float, int, vector float *);
  void vec_vsx_st (vector float, int, float *);
  void vec_vsx_st (vector signed int, int, vector signed int *);
  void vec_vsx_st (vector signed int, int, int *);
  void vec_vsx_st (vector unsigned int, int, vector unsigned int *);
  void vec_vsx_st (vector unsigned int, int, unsigned int *);
  void vec_vsx_st (vector bool int, int, vector bool int *);
  void vec_vsx_st (vector bool int, int, unsigned int *);
  void vec_vsx_st (vector bool int, int, int *);
  void vec_vsx_st (vector signed short, int, vector signed short *);
  void vec_vsx_st (vector signed short, int, short *);
  void vec_vsx_st (vector unsigned short, int, vector unsigned short *);
  void vec_vsx_st (vector unsigned short, int, unsigned short *);
  void vec_vsx_st (vector bool short, int, vector bool short *);
  void vec_vsx_st (vector bool short, int, unsigned short *);
  void vec_vsx_st (vector pixel, int, vector pixel *);
  void vec_vsx_st (vector pixel, int, unsigned short *);
  void vec_vsx_st (vector pixel, int, short *);
  void vec_vsx_st (vector bool short, int, short *);
  void vec_vsx_st (vector signed char, int, vector signed char *);
  void vec_vsx_st (vector signed char, int, signed char *);
  void vec_vsx_st (vector unsigned char, int, vector unsigned char *);
  void vec_vsx_st (vector unsigned char, int, unsigned char *);
  void vec_vsx_st (vector bool char, int, vector bool char *);
  void vec_vsx_st (vector bool char, int, unsigned char *);
  void vec_vsx_st (vector bool char, int, signed char *);

  vector double vec_xxpermdi (vector double, vector double, const int);
  vector float vec_xxpermdi (vector float, vector float, const int);
  vector long long vec_xxpermdi (vector long long, vector long long, const int);
  vector unsigned long long vec_xxpermdi (vector unsigned long long,
                                          vector unsigned long long, const int);
  vector int vec_xxpermdi (vector int, vector int, const int);
  vector unsigned int vec_xxpermdi (vector unsigned int,
                                    vector unsigned int, const int);
  vector short vec_xxpermdi (vector short, vector short, const int);
  vector unsigned short vec_xxpermdi (vector unsigned short,
                                      vector unsigned short, const int);
  vector signed char vec_xxpermdi (vector signed char, vector signed char,
                                   const int);
  vector unsigned char vec_xxpermdi (vector unsigned char,
                                     vector unsigned char, const int);

  vector double vec_xxsldi (vector double, vector double, int);
  vector float vec_xxsldi (vector float, vector float, int);
  vector long long vec_xxsldi (vector long long, vector long long, int);
  vector unsigned long long vec_xxsldi (vector unsigned long long,
                                        vector unsigned long long, int);
  vector int vec_xxsldi (vector int, vector int, int);
  vector unsigned int vec_xxsldi (vector unsigned int, vector unsigned int, int);
  vector short vec_xxsldi (vector short, vector short, int);
  vector unsigned short vec_xxsldi (vector unsigned short,
                                    vector unsigned short, int);
  vector signed char vec_xxsldi (vector signed char, vector signed char, int);
  vector unsigned char vec_xxsldi (vector unsigned char,
                                   vector unsigned char, int);

Note that the :samp:`vec_ld` and :samp:`vec_st` built-in functions always
generate the AltiVec :samp:`LVX` and :samp:`STVX` instructions even
if the VSX instruction set is available.  The :samp:`vec_vsx_ld` and
:samp:`vec_vsx_st` built-in functions always generate the VSX :samp:`LXVD2X`,
:samp:`LXVW4X`, :samp:`STXVD2X`, and :samp:`STXVW4X` instructions.

.. _powerpc-altivec-built-in-functions-available-on-isa-2.07:

PowerPC AltiVec Built-in Functions Available on ISA 2.07
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the ISA 2.07 additions to the vector/scalar (power8-vector)
instruction set are available, the following additional functions are
available for both 32-bit and 64-bit targets.  For 64-bit targets, you
can use :samp:`{vector long}` instead of :samp:`{vector long long}` ,
:samp:`{vector bool long}` instead of :samp:`{vector bool long long}` , and
:samp:`{vector unsigned long}` instead of :samp:`{vector unsigned long long}`.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  vector long long vec_vaddudm (vector long long, vector long long);
  vector long long vec_vaddudm (vector bool long long, vector long long);
  vector long long vec_vaddudm (vector long long, vector bool long long);
  vector unsigned long long vec_vaddudm (vector unsigned long long,
                                         vector unsigned long long);
  vector unsigned long long vec_vaddudm (vector bool unsigned long long,
                                         vector unsigned long long);
  vector unsigned long long vec_vaddudm (vector unsigned long long,
                                         vector bool unsigned long long);

  vector long long vec_vclz (vector long long);
  vector unsigned long long vec_vclz (vector unsigned long long);
  vector int vec_vclz (vector int);
  vector unsigned int vec_vclz (vector int);
  vector short vec_vclz (vector short);
  vector unsigned short vec_vclz (vector unsigned short);
  vector signed char vec_vclz (vector signed char);
  vector unsigned char vec_vclz (vector unsigned char);

  vector signed char vec_vclzb (vector signed char);
  vector unsigned char vec_vclzb (vector unsigned char);

  vector long long vec_vclzd (vector long long);
  vector unsigned long long vec_vclzd (vector unsigned long long);

  vector short vec_vclzh (vector short);
  vector unsigned short vec_vclzh (vector unsigned short);

  vector int vec_vclzw (vector int);
  vector unsigned int vec_vclzw (vector int);

  vector signed char vec_vgbbd (vector signed char);
  vector unsigned char vec_vgbbd (vector unsigned char);

  vector long long vec_vmaxsd (vector long long, vector long long);

  vector unsigned long long vec_vmaxud (vector unsigned long long,
                                        unsigned vector long long);

  vector long long vec_vminsd (vector long long, vector long long);

  vector unsigned long long vec_vminud (vector long long, vector long long);

  vector int vec_vpksdss (vector long long, vector long long);
  vector unsigned int vec_vpksdss (vector long long, vector long long);

  vector unsigned int vec_vpkudus (vector unsigned long long,
                                   vector unsigned long long);

  vector int vec_vpkudum (vector long long, vector long long);
  vector unsigned int vec_vpkudum (vector unsigned long long,
                                   vector unsigned long long);
  vector bool int vec_vpkudum (vector bool long long, vector bool long long);

  vector long long vec_vpopcnt (vector long long);
  vector unsigned long long vec_vpopcnt (vector unsigned long long);
  vector int vec_vpopcnt (vector int);
  vector unsigned int vec_vpopcnt (vector int);
  vector short vec_vpopcnt (vector short);
  vector unsigned short vec_vpopcnt (vector unsigned short);
  vector signed char vec_vpopcnt (vector signed char);
  vector unsigned char vec_vpopcnt (vector unsigned char);

  vector signed char vec_vpopcntb (vector signed char);
  vector unsigned char vec_vpopcntb (vector unsigned char);

  vector long long vec_vpopcntd (vector long long);
  vector unsigned long long vec_vpopcntd (vector unsigned long long);

  vector short vec_vpopcnth (vector short);
  vector unsigned short vec_vpopcnth (vector unsigned short);

  vector int vec_vpopcntw (vector int);
  vector unsigned int vec_vpopcntw (vector int);

  vector long long vec_vrld (vector long long, vector unsigned long long);
  vector unsigned long long vec_vrld (vector unsigned long long,
                                      vector unsigned long long);

  vector long long vec_vsld (vector long long, vector unsigned long long);
  vector long long vec_vsld (vector unsigned long long,
                             vector unsigned long long);

  vector long long vec_vsrad (vector long long, vector unsigned long long);
  vector unsigned long long vec_vsrad (vector unsigned long long,
                                       vector unsigned long long);

  vector long long vec_vsrd (vector long long, vector unsigned long long);
  vector unsigned long long char vec_vsrd (vector unsigned long long,
                                           vector unsigned long long);

  vector long long vec_vsubudm (vector long long, vector long long);
  vector long long vec_vsubudm (vector bool long long, vector long long);
  vector long long vec_vsubudm (vector long long, vector bool long long);
  vector unsigned long long vec_vsubudm (vector unsigned long long,
                                         vector unsigned long long);
  vector unsigned long long vec_vsubudm (vector bool long long,
                                         vector unsigned long long);
  vector unsigned long long vec_vsubudm (vector unsigned long long,
                                         vector bool long long);

  vector long long vec_vupkhsw (vector int);
  vector unsigned long long vec_vupkhsw (vector unsigned int);

  vector long long vec_vupklsw (vector int);
  vector unsigned long long vec_vupklsw (vector int);

If the ISA 2.07 additions to the vector/scalar (power8-vector)
instruction set are available, the following additional functions are
available for 64-bit targets.  New vector types
( :samp:`{vector __int128}` and :samp:`{vector __uint128}` ) are available
to hold the :samp:`{__int128}` and :samp:`{__uint128}` types to use these
builtins.

The normal vector extract, and set operations work on
:samp:`{vector __int128}` and :samp:`{vector __uint128}` types,
but the index value must be 0.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  vector __int128 vec_vaddcuq (vector __int128, vector __int128);
  vector __uint128 vec_vaddcuq (vector __uint128, vector __uint128);

  vector __int128 vec_vadduqm (vector __int128, vector __int128);
  vector __uint128 vec_vadduqm (vector __uint128, vector __uint128);

  vector __int128 vec_vaddecuq (vector __int128, vector __int128,
                                  vector __int128);
  vector __uint128 vec_vaddecuq (vector __uint128, vector __uint128,
                                   vector __uint128);

  vector __int128 vec_vaddeuqm (vector __int128, vector __int128,
                                  vector __int128);
  vector __uint128 vec_vaddeuqm (vector __uint128, vector __uint128,
                                   vector __uint128);

  vector __int128 vec_vsubecuq (vector __int128, vector __int128,
                                  vector __int128);
  vector __uint128 vec_vsubecuq (vector __uint128, vector __uint128,
                                   vector __uint128);

  vector __int128 vec_vsubeuqm (vector __int128, vector __int128,
                                  vector __int128);
  vector __uint128 vec_vsubeuqm (vector __uint128, vector __uint128,
                                   vector __uint128);

  vector __int128 vec_vsubcuq (vector __int128, vector __int128);
  vector __uint128 vec_vsubcuq (vector __uint128, vector __uint128);

  __int128 vec_vsubuqm (__int128, __int128);
  __uint128 vec_vsubuqm (__uint128, __uint128);

  vector __int128 __builtin_bcdadd (vector __int128, vector __int128, const int);
  vector unsigned char __builtin_bcdadd (vector unsigned char, vector unsigned char,
                                         const int);
  int __builtin_bcdadd_lt (vector __int128, vector __int128, const int);
  int __builtin_bcdadd_lt (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdadd_eq (vector __int128, vector __int128, const int);
  int __builtin_bcdadd_eq (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdadd_gt (vector __int128, vector __int128, const int);
  int __builtin_bcdadd_gt (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdadd_ov (vector __int128, vector __int128, const int);
  int __builtin_bcdadd_ov (vector unsigned char, vector unsigned char, const int);

  vector __int128 __builtin_bcdsub (vector __int128, vector __int128, const int);
  vector unsigned char __builtin_bcdsub (vector unsigned char, vector unsigned char,
                                         const int);
  int __builtin_bcdsub_lt (vector __int128, vector __int128, const int);
  int __builtin_bcdsub_lt (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdsub_eq (vector __int128, vector __int128, const int);
  int __builtin_bcdsub_eq (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdsub_gt (vector __int128, vector __int128, const int);
  int __builtin_bcdsub_gt (vector unsigned char, vector unsigned char, const int);
  int __builtin_bcdsub_ov (vector __int128, vector __int128, const int);
  int __builtin_bcdsub_ov (vector unsigned char, vector unsigned char, const int);

.. _powerpc-altivec-built-in-functions-available-on-isa-3.0:

PowerPC AltiVec Built-in Functions Available on ISA 3.0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following additional built-in functions are also available for the
PowerPC family of processors, starting with ISA 3.0
( :option:`-mcpu=power9` ) or later.

Only instructions excluded from the PVIPR are listed here.

.. code-block:: c++

  unsigned int scalar_extract_exp (double source);
  unsigned long long int scalar_extract_exp (__ieee128 source);

  unsigned long long int scalar_extract_sig (double source);
  unsigned __int128 scalar_extract_sig (__ieee128 source);

  double scalar_insert_exp (unsigned long long int significand,
                            unsigned long long int exponent);
  double scalar_insert_exp (double significand, unsigned long long int exponent);

  ieee_128 scalar_insert_exp (unsigned __int128 significand,
                              unsigned long long int exponent);
  ieee_128 scalar_insert_exp (ieee_128 significand, unsigned long long int exponent);

  int scalar_cmp_exp_gt (double arg1, double arg2);
  int scalar_cmp_exp_lt (double arg1, double arg2);
  int scalar_cmp_exp_eq (double arg1, double arg2);
  int scalar_cmp_exp_unordered (double arg1, double arg2);

  bool scalar_test_data_class (float source, const int condition);
  bool scalar_test_data_class (double source, const int condition);
  bool scalar_test_data_class (__ieee128 source, const int condition);

  bool scalar_test_neg (float source);
  bool scalar_test_neg (double source);
  bool scalar_test_neg (__ieee128 source);

The ``scalar_extract_exp`` and ``scalar_extract_sig``
functions require a 64-bit environment supporting ISA 3.0 or later.
The ``scalar_extract_exp`` and ``scalar_extract_sig`` built-in
functions return the significand and the biased exponent value
respectively of their ``source`` arguments.
When supplied with a 64-bit ``source`` argument, the
result returned by ``scalar_extract_sig`` has
the ``0x0010000000000000`` bit set if the
function's ``source`` argument is in normalized form.
Otherwise, this bit is set to 0.
When supplied with a 128-bit ``source`` argument, the
``0x00010000000000000000000000000000`` bit of the result is
treated similarly.
Note that the sign of the significand is not represented in the result
returned from the ``scalar_extract_sig`` function.  Use the
``scalar_test_neg`` function to test the sign of its ``double``
argument.

The ``scalar_insert_exp``
functions require a 64-bit environment supporting ISA 3.0 or later.
When supplied with a 64-bit first argument, the
``scalar_insert_exp`` built-in function returns a double-precision
floating point value that is constructed by assembling the values of its
``significand`` and ``exponent`` arguments.  The sign of the
result is copied from the most significant bit of the
``significand`` argument.  The significand and exponent components
of the result are composed of the least significant 11 bits of the
``exponent`` argument and the least significant 52 bits of the
``significand`` argument respectively.

When supplied with a 128-bit first argument, the
``scalar_insert_exp`` built-in function returns a quad-precision
ieee floating point value.  The sign bit of the result is copied from
the most significant bit of the ``significand`` argument.
The significand and exponent components of the result are composed of
the least significant 15 bits of the ``exponent`` argument and the
least significant 112 bits of the ``significand`` argument respectively.

The ``scalar_cmp_exp_gt``, ``scalar_cmp_exp_lt``,
``scalar_cmp_exp_eq``, and ``scalar_cmp_exp_unordered`` built-in
functions return a non-zero value if ``arg1`` is greater than, less
than, equal to, or not comparable to ``arg2`` respectively.  The
arguments are not comparable if one or the other equals NaN (not a
number). 

The ``scalar_test_data_class`` built-in function returns 1
if any of the condition tests enabled by the value of the
``condition`` variable are true, and 0 otherwise.  The
``condition`` argument must be a compile-time constant integer with
value not exceeding 127.  The
``condition`` argument is encoded as a bitmask with each bit
enabling the testing of a different condition, as characterized by the
following:

.. code-block:: c++

  0x40    Test for NaN
  0x20    Test for +Infinity
  0x10    Test for -Infinity
  0x08    Test for +Zero
  0x04    Test for -Zero
  0x02    Test for +Denormal
  0x01    Test for -Denormal

The ``scalar_test_neg`` built-in function returns 1 if its
``source`` argument holds a negative value, 0 otherwise.

The following built-in functions are also available for the PowerPC family
of processors, starting with ISA 3.0 or later
( :option:`-mcpu=power9` ).  These string functions are described
separately in order to group the descriptions closer to the function
prototypes.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  int vec_all_nez (vector signed char, vector signed char);
  int vec_all_nez (vector unsigned char, vector unsigned char);
  int vec_all_nez (vector signed short, vector signed short);
  int vec_all_nez (vector unsigned short, vector unsigned short);
  int vec_all_nez (vector signed int, vector signed int);
  int vec_all_nez (vector unsigned int, vector unsigned int);

  int vec_any_eqz (vector signed char, vector signed char);
  int vec_any_eqz (vector unsigned char, vector unsigned char);
  int vec_any_eqz (vector signed short, vector signed short);
  int vec_any_eqz (vector unsigned short, vector unsigned short);
  int vec_any_eqz (vector signed int, vector signed int);
  int vec_any_eqz (vector unsigned int, vector unsigned int);

  signed char vec_xlx (unsigned int index, vector signed char data);
  unsigned char vec_xlx (unsigned int index, vector unsigned char data);
  signed short vec_xlx (unsigned int index, vector signed short data);
  unsigned short vec_xlx (unsigned int index, vector unsigned short data);
  signed int vec_xlx (unsigned int index, vector signed int data);
  unsigned int vec_xlx (unsigned int index, vector unsigned int data);
  float vec_xlx (unsigned int index, vector float data);

  signed char vec_xrx (unsigned int index, vector signed char data);
  unsigned char vec_xrx (unsigned int index, vector unsigned char data);
  signed short vec_xrx (unsigned int index, vector signed short data);
  unsigned short vec_xrx (unsigned int index, vector unsigned short data);
  signed int vec_xrx (unsigned int index, vector signed int data);
  unsigned int vec_xrx (unsigned int index, vector unsigned int data);
  float vec_xrx (unsigned int index, vector float data);

The ``vec_all_nez``, ``vec_any_eqz``, and ``vec_cmpnez``
perform pairwise comparisons between the elements at the same
positions within their two vector arguments.
The ``vec_all_nez`` function returns a
non-zero value if and only if all pairwise comparisons are not
equal and no element of either vector argument contains a zero.
The ``vec_any_eqz`` function returns a
non-zero value if and only if at least one pairwise comparison is equal
or if at least one element of either vector argument contains a zero.
The ``vec_cmpnez`` function returns a vector of the same type as
its two arguments, within which each element consists of all ones to
denote that either the corresponding elements of the incoming arguments are
not equal or that at least one of the corresponding elements contains
zero.  Otherwise, the element of the returned vector contains all zeros.

The ``vec_xlx`` and ``vec_xrx`` functions extract the single
element selected by the ``index`` argument from the vector
represented by the ``data`` argument.  The ``index`` argument
always specifies a byte offset, regardless of the size of the vector
element.  With ``vec_xlx``, ``index`` is the offset of the first
byte of the element to be extracted.  With ``vec_xrx``, ``index``
represents the last byte of the element to be extracted, measured
from the right end of the vector.  In other words, the last byte of
the element to be extracted is found at position ``(15 - index)``.
There is no requirement that ``index`` be a multiple of the vector
element size.  However, if the size of the vector element added to
``index`` is greater than 15, the content of the returned value is
undefined.

The following functions are also available if the ISA 3.0 instruction
set additions ( :option:`-mcpu=power9` ) are available.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  vector long long vec_vctz (vector long long);
  vector unsigned long long vec_vctz (vector unsigned long long);
  vector int vec_vctz (vector int);
  vector unsigned int vec_vctz (vector int);
  vector short vec_vctz (vector short);
  vector unsigned short vec_vctz (vector unsigned short);
  vector signed char vec_vctz (vector signed char);
  vector unsigned char vec_vctz (vector unsigned char);

  vector signed char vec_vctzb (vector signed char);
  vector unsigned char vec_vctzb (vector unsigned char);

  vector long long vec_vctzd (vector long long);
  vector unsigned long long vec_vctzd (vector unsigned long long);

  vector short vec_vctzh (vector short);
  vector unsigned short vec_vctzh (vector unsigned short);

  vector int vec_vctzw (vector int);
  vector unsigned int vec_vctzw (vector int);

  vector int vec_vprtyb (vector int);
  vector unsigned int vec_vprtyb (vector unsigned int);
  vector long long vec_vprtyb (vector long long);
  vector unsigned long long vec_vprtyb (vector unsigned long long);

  vector int vec_vprtybw (vector int);
  vector unsigned int vec_vprtybw (vector unsigned int);

  vector long long vec_vprtybd (vector long long);
  vector unsigned long long vec_vprtybd (vector unsigned long long);

On 64-bit targets, if the ISA 3.0 additions ( :option:`-mcpu=power9` )
are available:

.. code-block:: c++

  vector long vec_vprtyb (vector long);
  vector unsigned long vec_vprtyb (vector unsigned long);
  vector __int128 vec_vprtyb (vector __int128);
  vector __uint128 vec_vprtyb (vector __uint128);

  vector long vec_vprtybd (vector long);
  vector unsigned long vec_vprtybd (vector unsigned long);

  vector __int128 vec_vprtybq (vector __int128);
  vector __uint128 vec_vprtybd (vector __uint128);

The following built-in functions are available for the PowerPC family
of processors, starting with ISA 3.0 or later ( :option:`-mcpu=power9` ).

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  __vector unsigned char
  vec_absdb (__vector unsigned char arg1, __vector unsigned char arg2);
  __vector unsigned short
  vec_absdh (__vector unsigned short arg1, __vector unsigned short arg2);
  __vector unsigned int
  vec_absdw (__vector unsigned int arg1, __vector unsigned int arg2);

The ``vec_absd``, ``vec_absdb``, ``vec_absdh``, and
``vec_absdw`` built-in functions each computes the absolute
differences of the pairs of vector elements supplied in its two vector
arguments, placing the absolute differences into the corresponding
elements of the vector result.

The following built-in functions are available for the PowerPC family
of processors, starting with ISA 3.0 or later ( :option:`-mcpu=power9` ):

.. code-block:: c++

  vector unsigned int vec_vrlnm (vector unsigned int, vector unsigned int);
  vector unsigned long long vec_vrlnm (vector unsigned long long,
                                       vector unsigned long long);

The result of ``vec_vrlnm`` is obtained by rotating each element
of the first argument vector left and ANDing it with a mask.  The
second argument vector contains the mask  beginning in bits 11:15,
the mask end in bits 19:23, and the shift count in bits 27:31,
of each element.

If the cryptographic instructions are enabled ( :option:`-mcrypto` or
:option:`-mcpu=power8` ), the following builtins are enabled.

Only functions excluded from the PVIPR are listed here.

.. code-block:: c++

  vector unsigned long long __builtin_crypto_vsbox (vector unsigned long long);

  vector unsigned long long __builtin_crypto_vcipher (vector unsigned long long,
                                                      vector unsigned long long);

  vector unsigned long long __builtin_crypto_vcipherlast
                                       (vector unsigned long long,
                                        vector unsigned long long);

  vector unsigned long long __builtin_crypto_vncipher (vector unsigned long long,
                                                       vector unsigned long long);

  vector unsigned long long __builtin_crypto_vncipherlast (vector unsigned long long,
                                                           vector unsigned long long);

  vector unsigned char __builtin_crypto_vpermxor (vector unsigned char,
                                                  vector unsigned char,
                                                  vector unsigned char);

  vector unsigned short __builtin_crypto_vpermxor (vector unsigned short,
                                                   vector unsigned short,
                                                   vector unsigned short);

  vector unsigned int __builtin_crypto_vpermxor (vector unsigned int,
                                                 vector unsigned int,
                                                 vector unsigned int);

  vector unsigned long long __builtin_crypto_vpermxor (vector unsigned long long,
                                                       vector unsigned long long,
                                                       vector unsigned long long);

  vector unsigned char __builtin_crypto_vpmsumb (vector unsigned char,
                                                 vector unsigned char);

  vector unsigned short __builtin_crypto_vpmsumh (vector unsigned short,
                                                  vector unsigned short);

  vector unsigned int __builtin_crypto_vpmsumw (vector unsigned int,
                                                vector unsigned int);

  vector unsigned long long __builtin_crypto_vpmsumd (vector unsigned long long,
                                                      vector unsigned long long);

  vector unsigned long long __builtin_crypto_vshasigmad (vector unsigned long long,
                                                         int, int);

  vector unsigned int __builtin_crypto_vshasigmaw (vector unsigned int, int, int);

The second argument to :samp:`{__builtin_crypto_vshasigmad}` and
:samp:`{__builtin_crypto_vshasigmaw}` must be a constant
integer that is 0 or 1.  The third argument to these built-in functions
must be a constant integer in the range of 0 to 15.

.. _powerpc-altivec-built-in-functions-available-on-isa-3.1:

PowerPC AltiVec Built-in Functions Available on ISA 3.1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following additional built-in functions are also available for the
PowerPC family of processors, starting with ISA 3.1 ( :option:`-mcpu=power10` ):

vector unsigned long long intvec_cfuge (vector unsigned long long int, vector unsigned long long int)Perform a vector centrifuge operation, as if implemented by the
``vcfuged`` instruction.

.. index:: vec_cfuge

vector unsigned long long intvec_cntlzm (vector unsigned long long int, vector unsigned long long int)Perform a vector count leading zeros under bit mask operation, as if
implemented by the ``vclzdm`` instruction.

.. index:: vec_cntlzm

vector unsigned long long intvec_cnttzm (vector unsigned long long int, vector unsigned long long int)Perform a vector count trailing zeros under bit mask operation, as if
implemented by the ``vctzdm`` instruction.

.. index:: vec_cnttzm

vector signed charvec_clrl (vector signed char a, unsigned int n)vector unsigned charvec_clrl (vector unsigned char a, unsigned int n)Clear the left-most ``(16 - n)`` bytes of vector argument ``a``, as if
implemented by the ``vclrlb`` instruction on a big-endian target
and by the ``vclrrb`` instruction on a little-endian target.  A
value of ``n`` that is greater than 16 is treated as if it equaled 16.

.. index:: vec_clrl

vector signed charvec_clrr (vector signed char a, unsigned int n)vector unsigned charvec_clrr (vector unsigned char a, unsigned int n)Clear the right-most ``(16 - n)`` bytes of vector argument ``a``, as if
implemented by the ``vclrrb`` instruction on a big-endian target
and by the ``vclrlb`` instruction on a little-endian target.  A
value of ``n`` that is greater than 16 is treated as if it equaled 16.

.. index:: vec_clrr

vector unsigned long long intvec_gnb (vector unsigned __int128, const unsigned char)Perform a 128-bit vector gather  operation, as if implemented by the
``vgnb`` instruction.  The second argument must be a literal
integer value between 2 and 7 inclusive.

.. index:: vec_gnb

Vector Extract

vector unsigned long long intvec_extractl (vector unsigned char, vector unsigned char, unsigned int)vector unsigned long long intvec_extractl (vector unsigned short, vector unsigned short, unsigned int)vector unsigned long long intvec_extractl (vector unsigned int, vector unsigned int, unsigned int)vector unsigned long long intvec_extractl (vector unsigned long long, vector unsigned long long, unsigned int)Extract an element from two concatenated vectors starting at the given byte index
in natural-endian order, and place it zero-extended in doubleword 1 of the result
according to natural element order.  If the byte index is out of range for the
data type, the intrinsic will be rejected.
For little-endian, this output will match the placement by the hardware
instruction, i.e., dword[0] in RTL notation.  For big-endian, an additional
instruction is needed to move it from the "left" doubleword to the  "right" one.
For little-endian, semantics matching the ``vextdubvrx``,
``vextduhvrx``, ``vextduwvrx`` instruction will be generated, while for
big-endian, semantics matching the ``vextdubvlx``, ``vextduhvlx``,
``vextduwvlx`` instructions
will be generated.  Note that some fairly anomalous results can be generated if
the byte index is not aligned on an element boundary for the element being
extracted.  This is a limitation of the bi-endian vector programming model is
consistent with the limitation on ``vec_perm``.

.. index:: vec_extractl

.. code-block:: c++

  vector unsigned long long intvec_extracth (vector unsigned char, vector unsigned char, unsigned int)vector unsigned long long intvec_extracth (vector unsigned short, vector unsigned short,unsigned int)
  vector unsigned long long intvec_extracth (vector unsigned int, vector unsigned int, unsigned int)vector unsigned long long intvec_extracth (vector unsigned long long, vector unsigned long long,unsigned int)

Extract an element from two concatenated vectors starting at the given byte
index.  The index is based on big endian order for a little endian system.
Similarly, the index is based on little endian order for a big endian system.
The extraced elements are zero-extended and put in doubleword 1
according to natural element order.  If the byte index is out of range for the
data type, the intrinsic will be rejected.  For little-endian, this output
will match the placement by the hardware instruction (vextdubvrx, vextduhvrx,
vextduwvrx, vextddvrx) i.e., dword[0] in RTL
notation.  For big-endian, an additional instruction is needed to move it
from the "left" doubleword to the "right" one.  For little-endian, semantics
matching the ``vextdubvlx``, ``vextduhvlx``, ``vextduwvlx``
instructions will be generated, while for big-endian, semantics matching the
``vextdubvrx``, ``vextduhvrx``, ``vextduwvrx`` instructions will
be generated.  Note that some fairly anomalous
results can be generated if the byte index is not aligned on the
element boundary for the element being extracted.  This is a
limitation of the bi-endian vector programming model consistent with the
limitation on ``vec_perm``.

.. index:: vec_extracth

vector unsigned long long intvec_pdep (vector unsigned long long int, vector unsigned long long int)Perform a vector parallel bits deposit operation, as if implemented by
the ``vpdepd`` instruction.

.. index:: vec_pdep

Vector Insert

.. code-block:: c++

  vector unsigned charvec_insertl (unsigned char, vector unsigned char, unsigned int);vector unsigned shortvec_insertl (unsigned short, vector unsigned short, unsigned int);vector unsigned intvec_insertl (unsigned int, vector unsigned int, unsigned int);vector unsigned long longvec_insertl (unsigned long long, vector unsigned long long,unsigned int);
  vector unsigned charvec_insertl (vector unsigned char, vector unsigned char, unsigned int;vector unsigned shortvec_insertl (vector unsigned short, vector unsigned short,unsigned int);
  vector unsigned intvec_insertl (vector unsigned int, vector unsigned int, unsigned int);

Let src be the first argument, when the first argument is a scalar, or the
rightmost element of the left doubleword of the first argument, when the first
argument is a vector.  Insert the source into the destination at the position
given by the third argument, using natural element order in the second
argument.  The rest of the second argument is unchanged.  If the byte
index is greater than 14 for halfwords, greater than 12 for words, or
greater than 8 for doublewords the result is undefined.   For little-endian,
the generated code will be semantically equivalent to ``vins[bhwd]rx``
instructions.  Similarly for big-endian it will be semantically equivalent
to ``vins[bhwd]lx``.  Note that some fairly anomalous results can be
generated if the byte index is not aligned on an element boundary for the
type of element being inserted.

.. index:: vec_insertl

.. code-block:: c++

  vector unsigned charvec_inserth (unsigned char, vector unsigned char, unsigned int);vector unsigned shortvec_inserth (unsigned short, vector unsigned short, unsigned int);vector unsigned intvec_inserth (unsigned int, vector unsigned int, unsigned int);vector unsigned long longvec_inserth (unsigned long long, vector unsigned long long,unsigned int);
  vector unsigned charvec_inserth (vector unsigned char, vector unsigned char, unsigned int);vector unsigned shortvec_inserth (vector unsigned short, vector unsigned short,unsigned int);
  vector unsigned intvec_inserth (vector unsigned int, vector unsigned int, unsigned int);

Let src be the first argument, when the first argument is a scalar, or the
rightmost element of the first argument, when the first argument is a vector.
Insert src into the second argument at the position identified by the third
argument, using opposite element order in the second argument, and leaving the
rest of the second argument unchanged.  If the byte index is greater than 14
for halfwords, 12 for words, or 8 for doublewords, the intrinsic will be
rejected. Note that the underlying hardware instruction uses the same register
for the second argument and the result.
For little-endian, the code generation will be semantically equivalent to
``vins[bhwd]lx``, while for big-endian it will be semantically equivalent to
``vins[bhwd]rx``.
Note that some fairly anomalous results can be generated if the byte index is
not aligned on an element boundary for the sort of element being inserted.

.. index:: vec_inserth

Vector Replace Element

.. code-block:: c++

  vector signed int vec_replace_elt (vector signed int, signed int,const int);
  vector unsigned int vec_replace_elt (vector unsigned int,unsigned int, const int);
  vector float vec_replace_elt (vector float, float, const int);vector signed long long vec_replace_elt (vector signed long long,signed long long, const int);
  vector unsigned long long vec_replace_elt (vector unsigned long long,unsigned long long, const int);
  vector double rec_replace_elt (vector double, double, const int);

The third argument (constrained to [0,3]) identifies the natural-endian
element number of the first argument that will be replaced by the second
argument to produce the result.  The other elements of the first argument will
remain unchanged in the result.

If it's desirable to insert a word at an unaligned position, use
vec_replace_unaligned instead.

.. index:: vec_replace_element

Vector Replace Unaligned

.. code-block:: c++

  vector unsigned char vec_replace_unaligned (vector unsigned char,signed int, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,unsigned int, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,float, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,signed long long, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,unsigned long long, const int);
  vector unsigned char vec_replace_unaligned (vector unsigned char,double, const int);

The second argument replaces a portion of the first argument to produce the
result, with the rest of the first argument unchanged in the result.  The
third argument identifies the byte index (using left-to-right, or big-endian
order) where the high-order byte of the second argument will be placed, with
the remaining bytes of the second argument placed naturally "to the right"
of the high-order byte.

The programmer is responsible for understanding the endianness issues involved
with the first argument and the result.

.. index:: vec_replace_unaligned

Vector Shift Left Double Bit Immediate

.. code-block:: c++

  vector signed char vec_sldb (vector signed char, vector signed char,const unsigned int);
  vector unsigned char vec_sldb (vector unsigned char,vector unsigned char, const unsigned int);
  vector signed short vec_sldb (vector signed short, vector signed short,const unsigned int);
  vector unsigned short vec_sldb (vector unsigned short,vector unsigned short, const unsigned int);
  vector signed int vec_sldb (vector signed int, vector signed int,const unsigned int);
  vector unsigned int vec_sldb (vector unsigned int, vector unsigned int,const unsigned int);
  vector signed long long vec_sldb (vector signed long long,vector signed long long, const unsigned int);
  vector unsigned long long vec_sldb (vector unsigned long long,vector unsigned long long, const unsigned int);

Shift the combined input vectors left by the amount specified by the low-order
three bits of the third argument, and return the leftmost remaining 128 bits.
Code using this instruction must be endian-aware.

.. index:: vec_sldb

Vector Shift Right Double Bit Immediate

.. code-block:: c++

  vector signed char vec_srdb (vector signed char, vector signed char,const unsigned int);
  vector unsigned char vec_srdb (vector unsigned char, vector unsigned char,const unsigned int);
  vector signed short vec_srdb (vector signed short, vector signed short,const unsigned int);
  vector unsigned short vec_srdb (vector unsigned short, vector unsigned short,const unsigned int);
  vector signed int vec_srdb (vector signed int, vector signed int,const unsigned int);
  vector unsigned int vec_srdb (vector unsigned int, vector unsigned int,const unsigned int);
  vector signed long long vec_srdb (vector signed long long,vector signed long long, const unsigned int);
  vector unsigned long long vec_srdb (vector unsigned long long,vector unsigned long long, const unsigned int);

Shift the combined input vectors right by the amount specified by the low-order
three bits of the third argument, and return the remaining 128 bits.  Code
using this built-in must be endian-aware.

.. index:: vec_srdb

Vector Splat

vector signed int vec_splati (const signed int);vector float vec_splati (const float);Splat a 32-bit immediate into a vector of words.

.. index:: vec_splati

vector double vec_splatid (const float);Convert a single precision floating-point value to double-precision and splat
the result to a vector of double-precision floats.

.. index:: vec_splatid

.. code-block:: c++

  vector signed int vec_splati_ins (vector signed int,const unsigned int, const signed int);
  vector unsigned int vec_splati_ins (vector unsigned int,const unsigned int, const unsigned int);
  vector float vec_splati_ins (vector float, const unsigned int,const float);

Argument 2 must be either 0 or 1.  Splat the value of argument 3 into the word
identified by argument 2 of each doubleword of argument 1 and return the
result.  The other words of argument 1 are unchanged.

.. index:: vec_splati_ins

Vector Blend Variable

.. code-block:: c++

  vector signed char vec_blendv (vector signed char, vector signed char,vector unsigned char);
  vector unsigned char vec_blendv (vector unsigned char,vector unsigned char, vector unsigned char);
  vector signed short vec_blendv (vector signed short,vector signed short, vector unsigned short);
  vector unsigned short vec_blendv (vector unsigned short,vector unsigned short, vector unsigned short);
  vector signed int vec_blendv (vector signed int, vector signed int,vector unsigned int);
  vector unsigned int vec_blendv (vector unsigned int,vector unsigned int, vector unsigned int);
  vector signed long long vec_blendv (vector signed long long,vector signed long long, vector unsigned long long);
  vector unsigned long long vec_blendv (vector unsigned long long,vector unsigned long long, vector unsigned long long);
  vector float vec_blendv (vector float, vector float,vector unsigned int);
  vector double vec_blendv (vector double, vector double,vector unsigned long long);

Blend the first and second argument vectors according to the sign bits of the
corresponding elements of the third argument vector.  This is similar to the
``vsel`` and ``xxsel`` instructions but for bigger elements.

.. index:: vec_blendv

Vector Permute Extended

.. code-block:: c++

  vector signed char vec_permx (vector signed char, vector signed char,vector unsigned char, const int);
  vector unsigned char vec_permx (vector unsigned char,vector unsigned char, vector unsigned char, const int);
  vector signed short vec_permx (vector signed short,vector signed short, vector unsigned char, const int);
  vector unsigned short vec_permx (vector unsigned short,vector unsigned short, vector unsigned char, const int);
  vector signed int vec_permx (vector signed int, vector signed int,vector unsigned char, const int);
  vector unsigned int vec_permx (vector unsigned int,vector unsigned int, vector unsigned char, const int);
  vector signed long long vec_permx (vector signed long long,vector signed long long, vector unsigned char, const int);
  vector unsigned long long vec_permx (vector unsigned long long,vector unsigned long long, vector unsigned char, const int);
  vector float (vector float, vector float, vector unsigned char,const int);
  vector double (vector double, vector double, vector unsigned char,const int);

Perform a partial permute of the first two arguments, which form a 32-byte
section of an emulated vector up to 256 bytes wide, using the partial permute
control vector in the third argument.  The fourth argument (constrained to
values of 0-7) identifies which 32-byte section of the emulated vector is
contained in the first two arguments.

.. index:: vec_permx

vector unsigned long long intvec_pext (vector unsigned long long int, vector unsigned long long int)Perform a vector parallel bit extract operation, as if implemented by
the ``vpextd`` instruction.

.. index:: vec_pext

vector unsigned char vec_stril (vector unsigned char)vector signed char vec_stril (vector signed char)vector unsigned short vec_stril (vector unsigned short)vector signed short vec_stril (vector signed short)Isolate the left-most non-zero elements of the incoming vector argument,
replacing all elements to the right of the left-most zero element
found within the argument with zero.  The typical implementation uses
the ``vstribl`` or ``vstrihl`` instruction on big-endian targets
and uses the ``vstribr`` or ``vstrihr`` instruction on
little-endian targets.

.. index:: vec_stril

int vec_stril_p (vector unsigned char)int vec_stril_p (vector signed char)int short vec_stril_p (vector unsigned short)int vec_stril_p (vector signed short)Return a non-zero value if and only if the argument contains a zero
element.  The typical implementation uses
the ``vstribl.`` or ``vstrihl.`` instruction on big-endian targets
and uses the ``vstribr.`` or ``vstrihr.`` instruction on
little-endian targets.  Choose this built-in to check for presence of
zero element if the same argument is also passed to ``vec_stril``.

.. index:: vec_stril_p

vector unsigned char vec_strir (vector unsigned char)vector signed char vec_strir (vector signed char)vector unsigned short vec_strir (vector unsigned short)vector signed short vec_strir (vector signed short)Isolate the right-most non-zero elements of the incoming vector argument,
replacing all elements to the left of the right-most zero element
found within the argument with zero.  The typical implementation uses
the ``vstribr`` or ``vstrihr`` instruction on big-endian targets
and uses the ``vstribl`` or ``vstrihl`` instruction on
little-endian targets.

.. index:: vec_strir

int vec_strir_p (vector unsigned char)int vec_strir_p (vector signed char)int short vec_strir_p (vector unsigned short)int vec_strir_p (vector signed short)Return a non-zero value if and only if the argument contains a zero
element.  The typical implementation uses
the ``vstribr.`` or ``vstrihr.`` instruction on big-endian targets
and uses the ``vstribl.`` or ``vstrihl.`` instruction on
little-endian targets.  Choose this built-in to check for presence of
zero element if the same argument is also passed to ``vec_strir``.

.. index:: vec_strir_p

.. code-block:: c++

  vector unsigned charvec_ternarylogic (vector unsigned char, vector unsigned char,            vector unsigned char, const unsigned int)
  vector unsigned shortvec_ternarylogic (vector unsigned short, vector unsigned short,            vector unsigned short, const unsigned int)
  vector unsigned intvec_ternarylogic (vector unsigned int, vector unsigned int,            vector unsigned int, const unsigned int)
  vector unsigned long long intvec_ternarylogic (vector unsigned long long int, vector unsigned long long int,            vector unsigned long long int, const unsigned int)
  vector unsigned __int128vec_ternarylogic (vector unsigned __int128, vector unsigned __int128,            vector unsigned __int128, const unsigned int)

Perform a 128-bit vector evaluate operation, as if implemented by the
``xxeval`` instruction.  The fourth argument must be a literal
integer value between 0 and 255 inclusive.

.. index:: vec_ternarylogic

.. code-block:: c++

  vector unsigned char vec_genpcvm (vector unsigned char, const int)vector unsigned short vec_genpcvm (vector unsigned short, const int)vector unsigned int vec_genpcvm (vector unsigned int, const int)vector unsigned int vec_genpcvm (vector unsigned long long int,                                         const int)

Vector Integer Multiply/Divide/Modulo

vector signed intvec_mulh (vector signed int a, vector signed int b)vector unsigned intvec_mulh (vector unsigned int a, vector unsigned int b)For each integer value ``i`` from 0 to 3, do the following. The integer
value in word element ``i`` of a is multiplied by the integer value in word
element ``i`` of b. The high-order 32 bits of the 64-bit product are placed
into word element ``i`` of the vector returned.

vector signed long longvec_mulh (vector signed long long a, vector signed long long b)vector unsigned long longvec_mulh (vector unsigned long long a, vector unsigned long long b)For each integer value ``i`` from 0 to 1, do the following. The integer
value in doubleword element ``i`` of a is multiplied by the integer value in
doubleword element ``i`` of b. The high-order 64 bits of the 128-bit product
are placed into doubleword element ``i`` of the vector returned.

vector unsigned long longvec_mul (vector unsigned long long a, vector unsigned long long b)vector signed long longvec_mul (vector signed long long a, vector signed long long b)For each integer value ``i`` from 0 to 1, do the following. The integer
value in doubleword element ``i`` of a is multiplied by the integer value in
doubleword element ``i`` of b. The low-order 64 bits of the 128-bit product
are placed into doubleword element ``i`` of the vector returned.

vector signed intvec_div (vector signed int a, vector signed int b)vector unsigned intvec_div (vector unsigned int a, vector unsigned int b)For each integer value ``i`` from 0 to 3, do the following. The integer in
word element ``i`` of a is divided by the integer in word element ``i``
of b. The unique integer quotient is placed into the word element ``i`` of
the vector returned. If an attempt is made to perform any of the divisions
<anything> ÷ 0 then the quotient is undefined.

vector signed long longvec_div (vector signed long long a, vector signed long long b)vector unsigned long longvec_div (vector unsigned long long a, vector unsigned long long b)For each integer value ``i`` from 0 to 1, do the following. The integer in
doubleword element ``i`` of a is divided by the integer in doubleword
element ``i`` of b. The unique integer quotient is placed into the
doubleword element ``i`` of the vector returned. If an attempt is made to
perform any of the divisions 0x8000_0000_0000_0000 ÷ -1 or <anything> ÷ 0 then
the quotient is undefined.

vector signed intvec_dive (vector signed int a, vector signed int b)vector unsigned intvec_dive (vector unsigned int a, vector unsigned int b)For each integer value ``i`` from 0 to 3, do the following. The integer in
word element ``i`` of a is shifted left by 32 bits, then divided by the
integer in word element ``i`` of b. The unique integer quotient is placed
into the word element ``i`` of the vector returned. If the quotient cannot
be represented in 32 bits, or if an attempt is made to perform any of the
divisions <anything> ÷ 0 then the quotient is undefined.

vector signed long longvec_dive (vector signed long long a, vector signed long long b)vector unsigned long longvec_dive (vector unsigned long long a, vector unsigned long long b)For each integer value ``i`` from 0 to 1, do the following. The integer in
doubleword element ``i`` of a is shifted left by 64 bits, then divided by
the integer in doubleword element ``i`` of b. The unique integer quotient is
placed into the doubleword element ``i`` of the vector returned. If the
quotient cannot be represented in 64 bits, or if an attempt is made to perform
<anything> ÷ 0 then the quotient is undefined.

vector signed intvec_mod (vector signed int a, vector signed int b)vector unsigned intvec_mod (vector unsigned int a, vector unsigned int b)For each integer value ``i`` from 0 to 3, do the following. The integer in
word element ``i`` of a is divided by the integer in word element ``i``
of b. The unique integer remainder is placed into the word element ``i`` of
the vector returned.  If an attempt is made to perform any of the divisions
0x8000_0000 ÷ -1 or <anything> ÷ 0 then the remainder is undefined.

vector signed long longvec_mod (vector signed long long a, vector signed long long b)vector unsigned long longvec_mod (vector unsigned long long a, vector unsigned long long b)For each integer value ``i`` from 0 to 1, do the following. The integer in
doubleword element ``i`` of a is divided by the integer in doubleword
element ``i`` of b. The unique integer remainder is placed into the
doubleword element ``i`` of the vector returned. If an attempt is made to
perform <anything> ÷ 0 then the remainder is undefined.

Generate PCV from specified Mask size, as if implemented by the
``xxgenpcvbm``, ``xxgenpcvhm``, ``xxgenpcvwm`` instructions, where
immediate value is either 0, 1, 2 or 3.

.. index:: vec_genpcvm

.. _powerpc-hardware-transactional-memory-built-in-functions:

PowerPC Hardware Transactional Memory Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides two interfaces for accessing the Hardware Transactional
Memory (HTM) instructions available on some of the PowerPC family
of processors (eg, POWER8).  The two interfaces come in a low level
interface, consisting of built-in functions specific to PowerPC and a
higher level interface consisting of inline functions that are common
between PowerPC and S/390.

PowerPC HTM Low Level Built-in Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following low level built-in functions are available with
:option:`-mhtm` or :option:`-mcpu=CPU` where CPU is 'power8' or later.
They all generate the machine instruction that is part of the name.

The HTM builtins (with the exception of ``__builtin_tbegin``) return
the full 4-bit condition register value set by their associated hardware
instruction.  The header file ``htmintrin.h`` defines some macros that can
be used to decipher the return value.  The ``__builtin_tbegin`` builtin
returns a simple ``true`` or ``false`` value depending on whether a transaction was
successfully started or not.  The arguments of the builtins match exactly the
type and order of the associated hardware instruction's operands, except for
the ``__builtin_tcheck`` builtin, which does not take any input arguments.
Refer to the ISA manual for a description of each instruction's operands.

.. code-block:: c++

  unsigned int __builtin_tbegin (unsigned int)
  unsigned int __builtin_tend (unsigned int)

  unsigned int __builtin_tabort (unsigned int)
  unsigned int __builtin_tabortdc (unsigned int, unsigned int, unsigned int)
  unsigned int __builtin_tabortdci (unsigned int, unsigned int, int)
  unsigned int __builtin_tabortwc (unsigned int, unsigned int, unsigned int)
  unsigned int __builtin_tabortwci (unsigned int, unsigned int, int)

  unsigned int __builtin_tcheck (void)
  unsigned int __builtin_treclaim (unsigned int)
  unsigned int __builtin_trechkpt (void)
  unsigned int __builtin_tsr (unsigned int)

In addition to the above HTM built-ins, we have added built-ins for
some common extended mnemonics of the HTM instructions:

.. code-block:: c++

  unsigned int __builtin_tendall (void)
  unsigned int __builtin_tresume (void)
  unsigned int __builtin_tsuspend (void)

Note that the semantics of the above HTM builtins are required to mimic
the locking semantics used for critical sections.  Builtins that are used
to create a new transaction or restart a suspended transaction must have
lock acquisition like semantics while those builtins that end or suspend a
transaction must have lock release like semantics.  Specifically, this must
mimic lock semantics as specified by C++11, for example: Lock acquisition is
as-if an execution of __atomic_exchange_n(&globallock,1,__ATOMIC_ACQUIRE)
that returns 0, and lock release is as-if an execution of
__atomic_store(&globallock,0,__ATOMIC_RELEASE), with globallock being an
implicit implementation-defined lock used for all transactions.  The HTM
instructions associated with with the builtins inherently provide the
correct acquisition and release hardware barriers required.  However,
the compiler must also be prohibited from moving loads and stores across
the builtins in a way that would violate their semantics.  This has been
accomplished by adding memory barriers to the associated HTM instructions
(which is a conservative approach to provide acquire and release semantics).
Earlier versions of the compiler did not treat the HTM instructions as
memory barriers.  A ``__TM_FENCE__`` macro has been added, which can
be used to determine whether the current compiler treats HTM instructions
as memory barriers or not.  This allows the user to explicitly add memory
barriers to their code when using an older version of the compiler.

The following set of built-in functions are available to gain access
to the HTM specific special purpose registers.

.. code-block:: c++

  unsigned long __builtin_get_texasr (void)
  unsigned long __builtin_get_texasru (void)
  unsigned long __builtin_get_tfhar (void)
  unsigned long __builtin_get_tfiar (void)

  void __builtin_set_texasr (unsigned long);
  void __builtin_set_texasru (unsigned long);
  void __builtin_set_tfhar (unsigned long);
  void __builtin_set_tfiar (unsigned long);

Example usage of these low level built-in functions may look like:

.. code-block:: c++

  #include <htmintrin.h>

  int num_retries = 10;

  while (1)
    {
      if (__builtin_tbegin (0))
        {
          /* Transaction State Initiated.  */
          if (is_locked (lock))
            __builtin_tabort (0);
          ... transaction code...
          __builtin_tend (0);
          break;
        }
      else
        {
          /* Transaction State Failed.  Use locks if the transaction
             failure is "persistent" or we've tried too many times.  */
          if (num_retries-- <= 0
              || _TEXASRU_FAILURE_PERSISTENT (__builtin_get_texasru ()))
            {
              acquire_lock (lock);
              ... non transactional fallback path...
              release_lock (lock);
              break;
            }
        }
    }

One final built-in function has been added that returns the value of
the 2-bit Transaction State field of the Machine Status Register (MSR)
as stored in ``CR0``.

.. code-block:: c++

  unsigned long __builtin_ttest (void)

This built-in can be used to determine the current transaction state
using the following code example:

.. code-block:: c++

  #include <htmintrin.h>

  unsigned char tx_state = _HTM_STATE (__builtin_ttest ());

  if (tx_state == _HTM_TRANSACTIONAL)
    {
      /* Code to use in transactional state.  */
    }
  else if (tx_state == _HTM_NONTRANSACTIONAL)
    {
      /* Code to use in non-transactional state.  */
    }
  else if (tx_state == _HTM_SUSPENDED)
    {
      /* Code to use in transaction suspended state.  */
    }

PowerPC HTM High Level Inline Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following high level HTM interface is made available by including
``<htmxlintrin.h>`` and using :option:`-mhtm` or :option:`-mcpu=CPU`
where CPU is 'power8' or later.  This interface is common between PowerPC
and S/390, allowing users to write one HTM source implementation that
can be compiled and executed on either system.

.. code-block:: c++

  long __TM_simple_begin (void)
  long __TM_begin (void* const TM_buff)
  long __TM_end (void)
  void __TM_abort (void)
  void __TM_named_abort (unsigned char const code)
  void __TM_resume (void)
  void __TM_suspend (void)

  long __TM_is_user_abort (void* const TM_buff)
  long __TM_is_named_user_abort (void* const TM_buff, unsigned char *code)
  long __TM_is_illegal (void* const TM_buff)
  long __TM_is_footprint_exceeded (void* const TM_buff)
  long __TM_nesting_depth (void* const TM_buff)
  long __TM_is_nested_too_deep(void* const TM_buff)
  long __TM_is_conflict(void* const TM_buff)
  long __TM_is_failure_persistent(void* const TM_buff)
  long __TM_failure_address(void* const TM_buff)
  long long __TM_failure_code(void* const TM_buff)

Using these common set of HTM inline functions, we can create
a more portable version of the HTM example in the previous
section that will work on either PowerPC or S/390:

.. code-block:: c++

  #include <htmxlintrin.h>

  int num_retries = 10;
  TM_buff_type TM_buff;

  while (1)
    {
      if (__TM_begin (TM_buff) == _HTM_TBEGIN_STARTED)
        {
          /* Transaction State Initiated.  */
          if (is_locked (lock))
            __TM_abort ();
          ... transaction code...
          __TM_end ();
          break;
        }
      else
        {
          /* Transaction State Failed.  Use locks if the transaction
             failure is "persistent" or we've tried too many times.  */
          if (num_retries-- <= 0
              || __TM_is_failure_persistent (TM_buff))
            {
              acquire_lock (lock);
              ... non transactional fallback path...
              release_lock (lock);
              break;
            }
        }
    }

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

.. _powerpc-matrix-multiply-assist-built-in-functions:

PowerPC Matrix-Multiply Assist Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

ISA 3.1 of the PowerPC added new Matrix-Multiply Assist (MMA) instructions.
GCC provides support for these instructions through the following built-in
functions which are enabled with the ``-mmma`` option.  The vec_t type
below is defined to be a normal vector unsigned char type.  The uint2, uint4
and uint8 parameters are 2-bit, 4-bit and 8-bit unsigned integer constants
respectively.  The compiler will verify that they are constants and that
their values are within range.

The built-in functions supported are:

.. code-block:: c++

  void __builtin_mma_xvi4ger8 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi8ger4 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi16ger2 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi16ger2s (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2 (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32ger (__vector_quad *, vec_t, vec_t);

  void __builtin_mma_xvi4ger8pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi8ger4pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi8ger4spp(__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi16ger2pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvi16ger2spp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2pn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2np (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf16ger2nn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2pp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2pn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2np (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvbf16ger2nn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32gerpp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32gerpn (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32gernp (__vector_quad *, vec_t, vec_t);
  void __builtin_mma_xvf32gernn (__vector_quad *, vec_t, vec_t);

  void __builtin_mma_pmxvi4ger8 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint8);
  void __builtin_mma_pmxvi4ger8pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint8);

  void __builtin_mma_pmxvi8ger4 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint4);
  void __builtin_mma_pmxvi8ger4pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint4);
  void __builtin_mma_pmxvi8ger4spp(__vector_quad *, vec_t, vec_t, uint4, uint4, uint4);

  void __builtin_mma_pmxvi16ger2 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvi16ger2s (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2 (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);

  void __builtin_mma_pmxvi16ger2pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvi16ger2spp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2pn (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2np (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvf16ger2nn (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2pp (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2pn (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2np (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);
  void __builtin_mma_pmxvbf16ger2nn (__vector_quad *, vec_t, vec_t, uint4, uint4, uint2);

  void __builtin_mma_pmxvf32ger (__vector_quad *, vec_t, vec_t, uint4, uint4);
  void __builtin_mma_pmxvf32gerpp (__vector_quad *, vec_t, vec_t, uint4, uint4);
  void __builtin_mma_pmxvf32gerpn (__vector_quad *, vec_t, vec_t, uint4, uint4);
  void __builtin_mma_pmxvf32gernp (__vector_quad *, vec_t, vec_t, uint4, uint4);
  void __builtin_mma_pmxvf32gernn (__vector_quad *, vec_t, vec_t, uint4, uint4);

  void __builtin_mma_xvf64ger (__vector_quad *, __vector_pair, vec_t);
  void __builtin_mma_xvf64gerpp (__vector_quad *, __vector_pair, vec_t);
  void __builtin_mma_xvf64gerpn (__vector_quad *, __vector_pair, vec_t);
  void __builtin_mma_xvf64gernp (__vector_quad *, __vector_pair, vec_t);
  void __builtin_mma_xvf64gernn (__vector_quad *, __vector_pair, vec_t);

  void __builtin_mma_pmxvf64ger (__vector_quad *, __vector_pair, vec_t, uint4, uint2);
  void __builtin_mma_pmxvf64gerpp (__vector_quad *, __vector_pair, vec_t, uint4, uint2);
  void __builtin_mma_pmxvf64gerpn (__vector_quad *, __vector_pair, vec_t, uint4, uint2);
  void __builtin_mma_pmxvf64gernp (__vector_quad *, __vector_pair, vec_t, uint4, uint2);
  void __builtin_mma_pmxvf64gernn (__vector_quad *, __vector_pair, vec_t, uint4, uint2);

  void __builtin_mma_xxmtacc (__vector_quad *);
  void __builtin_mma_xxmfacc (__vector_quad *);
  void __builtin_mma_xxsetaccz (__vector_quad *);

  void __builtin_mma_assemble_acc (__vector_quad *, vec_t, vec_t, vec_t, vec_t);
  void __builtin_mma_disassemble_acc (void *, __vector_quad *);

  void __builtin_vsx_assemble_pair (__vector_pair *, vec_t, vec_t);
  void __builtin_vsx_disassemble_pair (void *, __vector_pair *);

  vec_t __builtin_vsx_xvcvspbf16 (vec_t);
  vec_t __builtin_vsx_xvcvbf16spn (vec_t);

.. _pru-built-in-functions:

PRU Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

GCC provides a couple of special builtin functions to aid in utilizing
special PRU instructions.

The built-in functions supported are:

:samp:`__delay_cycles (long long {cycles})`
  This inserts an instruction sequence that takes exactly :samp:`{cycles}`
  cycles (between 0 and 0xffffffff) to complete.  The inserted sequence
  may use jumps, loops, or no-ops, and does not interfere with any other
  instructions.  Note that :samp:`{cycles}` must be a compile-time constant
  integer - that is, you must pass a number, not a variable that may be
  optimized to a constant later.  The number of cycles delayed by this
  builtin is exact.

``__halt (void)``
  This inserts a HALT instruction to stop processor execution.

:samp:`unsigned int __lmbd (unsigned int {wordval}, unsigned int {bitval})`
  This inserts LMBD instruction to calculate the left-most bit with value
  :samp:`{bitval}` in value :samp:`{wordval}`.  Only the least significant bit
  of :samp:`{bitval}` is taken into account.

  .. _risc-v-built-in-functions:

RISC-V Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the RISC-V family of
processors.

.. function:: void * __builtin_thread_pointer(void )

  Returns the value that is currently set in the :samp:`tp` register.

.. _rx-built-in-functions:

RX Built-in Functions
^^^^^^^^^^^^^^^^^^^^^

GCC supports some of the RX instructions which cannot be expressed in
the C programming language via the use of built-in functions.  The
following functions are supported:

.. function:: void __builtin_rx_brk(void )

  Generates the ``brk`` machine instruction.

.. function:: void __builtin_rx_clrpsw(int )

  Generates the ``clrpsw`` machine instruction to clear the specified
  bit in the processor status word.

.. function:: void __builtin_rx_int(int )

  Generates the ``int`` machine instruction to generate an interrupt
  with the specified value.

.. function:: void __builtin_rx_machi(int ,int )

  Generates the ``machi`` machine instruction to add the result of
  multiplying the top 16 bits of the two arguments into the
  accumulator.

.. function:: void __builtin_rx_maclo(int ,int )

  Generates the ``maclo`` machine instruction to add the result of
  multiplying the bottom 16 bits of the two arguments into the
  accumulator.

.. function:: void __builtin_rx_mulhi(int ,int )

  Generates the ``mulhi`` machine instruction to place the result of
  multiplying the top 16 bits of the two arguments into the
  accumulator.

.. function:: void __builtin_rx_mullo(int ,int )

  Generates the ``mullo`` machine instruction to place the result of
  multiplying the bottom 16 bits of the two arguments into the
  accumulator.

.. function:: int __builtin_rx_mvfachi(void )

  Generates the ``mvfachi`` machine instruction to read the top
  32 bits of the accumulator.

.. function:: int __builtin_rx_mvfacmi(void )

  Generates the ``mvfacmi`` machine instruction to read the middle
  32 bits of the accumulator.

.. function:: int __builtin_rx_mvfc(int )

  Generates the ``mvfc`` machine instruction which reads the control
  register specified in its argument and returns its value.

.. function:: void __builtin_rx_mvtachi(int )

  Generates the ``mvtachi`` machine instruction to set the top
  32 bits of the accumulator.

.. function:: void __builtin_rx_mvtaclo(int )

  Generates the ``mvtaclo`` machine instruction to set the bottom
  32 bits of the accumulator.

.. function:: void __builtin_rx_mvtc(int reg,int val)

  Generates the ``mvtc`` machine instruction which sets control
  register number ``reg`` to ``val``.

.. function:: void __builtin_rx_mvtipl(int )

  Generates the ``mvtipl`` machine instruction set the interrupt
  priority level.

.. function:: void __builtin_rx_racw(int )

  Generates the ``racw`` machine instruction to round the accumulator
  according to the specified mode.

.. function:: int __builtin_rx_revw(int )

  Generates the ``revw`` machine instruction which swaps the bytes in
  the argument so that bits 0-7 now occupy bits 8-15 and vice versa,
  and also bits 16-23 occupy bits 24-31 and vice versa.

.. function:: void __builtin_rx_rmpa(void )

  Generates the ``rmpa`` machine instruction which initiates a
  repeated multiply and accumulate sequence.

.. function:: void __builtin_rx_round(float )

  Generates the ``round`` machine instruction which returns the
  floating-point argument rounded according to the current rounding mode
  set in the floating-point status word register.

.. function:: int __builtin_rx_sat(int )

  Generates the ``sat`` machine instruction which returns the
  saturated value of the argument.

.. function:: void __builtin_rx_setpsw(int )

  Generates the ``setpsw`` machine instruction to set the specified
  bit in the processor status word.

.. function:: void __builtin_rx_wait(void )

  Generates the ``wait`` machine instruction.

.. _s-390-system-z-built-in-functions:

S/390 System z Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. function:: int __builtin_tbegin(void* )

  Generates the ``tbegin`` machine instruction starting a
  non-constrained hardware transaction.  If the parameter is non-NULL the
  memory area is used to store the transaction diagnostic buffer and
  will be passed as first operand to ``tbegin``.  This buffer can be
  defined using the ``struct __htm_tdb`` C struct defined in
  ``htmintrin.h`` and must reside on a double-word boundary.  The
  second tbegin operand is set to ``0xff0c``. This enables
  save/restore of all GPRs and disables aborts for FPR and AR
  manipulations inside the transaction body.  The condition code set by
  the tbegin instruction is returned as integer value.  The tbegin
  instruction by definition overwrites the content of all FPRs.  The
  compiler will generate code which saves and restores the FPRs.  For
  soft-float code it is recommended to used the ``*_nofloat``
  variant.  In order to prevent a TDB from being written it is required
  to pass a constant zero value as parameter.  Passing a zero value
  through a variable is not sufficient.  Although modifications of
  access registers inside the transaction will not trigger an
  transaction abort it is not supported to actually modify them.  Access
  registers do not get saved when entering a transaction. They will have
  undefined state when reaching the abort code.

Macros for the possible return codes of tbegin are defined in the
``htmintrin.h`` header file:

``_HTM_TBEGIN_STARTED``
  ``tbegin`` has been executed as part of normal processing.  The
  transaction body is supposed to be executed.

``_HTM_TBEGIN_INDETERMINATE``
  The transaction was aborted due to an indeterminate condition which
  might be persistent.

``_HTM_TBEGIN_TRANSIENT``
  The transaction aborted due to a transient failure.  The transaction
  should be re-executed in that case.

``_HTM_TBEGIN_PERSISTENT``
  The transaction aborted due to a persistent failure.  Re-execution
  under same circumstances will not be productive.

.. index:: _HTM_FIRST_USER_ABORT_CODE

Macro_HTM_FIRST_USER_ABORT_CODEThe ``_HTM_FIRST_USER_ABORT_CODE`` defined in ``htmintrin.h``
specifies the first abort code which can be used for
``__builtin_tabort``.  Values below this threshold are reserved for
machine use.

.. index:: struct __htm_tdb

Data typestruct __htm_tdbThe ``struct __htm_tdb`` defined in ``htmintrin.h`` describes
the structure of the transaction diagnostic block as specified in the
Principles of Operation manual chapter 5-91.

.. function:: int __builtin_tbegin_nofloat(void* )

  Same as ``__builtin_tbegin`` but without FPR saves and restores.
  Using this variant in code making use of FPRs will leave the FPRs in
  undefined state when entering the transaction abort handler code.

.. function:: int __builtin_tbegin_retry(void* ,int )

  In addition to ``__builtin_tbegin`` a loop for transient failures
  is generated.  If tbegin returns a condition code of 2 the transaction
  will be retried as often as specified in the second argument.  The
  perform processor assist instruction is used to tell the CPU about the
  number of fails so far.

.. function:: int __builtin_tbegin_retry_nofloat(void* ,int )

  Same as ``__builtin_tbegin_retry`` but without FPR saves and
  restores.  Using this variant in code making use of FPRs will leave
  the FPRs in undefined state when entering the transaction abort
  handler code.

.. function:: void __builtin_tbeginc(void )

  Generates the ``tbeginc`` machine instruction starting a constrained
  hardware transaction.  The second operand is set to ``0xff08``.

.. function:: int __builtin_tend(void )

  Generates the ``tend`` machine instruction finishing a transaction
  and making the changes visible to other threads.  The condition code
  generated by tend is returned as integer value.

.. function:: void __builtin_tabort(int )

  Generates the ``tabort`` machine instruction with the specified
  abort code.  Abort codes from 0 through 255 are reserved and will
  result in an error message.

.. function:: void __builtin_tx_assist(int )

  Generates the ``ppa rX,rY,1`` machine instruction.  Where the
  integer parameter is loaded into rX and a value of zero is loaded into
  rY.  The integer parameter specifies the number of times the
  transaction repeatedly aborted.

.. function:: int __builtin_tx_nesting_depth(void )

  Generates the ``etnd`` machine instruction.  The current nesting
  depth is returned as integer value.  For a nesting depth of 0 the code
  is not executed as part of an transaction.

.. function:: void __builtin_non_tx_store(uint64_t *,uint64_t )

  Generates the ``ntstg`` machine instruction.  The second argument
  is written to the first arguments location.  The store operation will
  not be rolled-back in case of an transaction abort.

.. _sh-built-in-functions:

SH Built-in Functions
^^^^^^^^^^^^^^^^^^^^^

The following built-in functions are supported on the SH1, SH2, SH3 and SH4
families of processors:

.. function:: void __builtin_set_thread_pointer(void *ptr)

  Sets the :samp:`GBR` register to the specified value :samp:`{ptr}`.  This is usually
  used by system code that manages threads and execution contexts.  The compiler
  normally does not generate code that modifies the contents of :samp:`GBR` and
  thus the value is preserved across function calls.  Changing the :samp:`GBR`
  value in user code must be done with caution, since the compiler might use
  :samp:`GBR` in order to access thread local variables.

.. function:: void * __builtin_thread_pointer(void )

  Returns the value that is currently set in the :samp:`GBR` register.
  Memory loads and stores that use the thread pointer as a base address are
  turned into :samp:`GBR` based displacement loads and stores, if possible.
  For example:

  .. code-block:: c++

    struct my_tcb
    {
       int a, b, c, d, e;
    };

    int get_tcb_value (void)
    {
      // Generate mov.l @(8,gbr),r0 instruction
      return ((my_tcb*)__builtin_thread_pointer ())->c;
    }

.. function:: unsigned int __builtin_sh_get_fpscr(void )

  Returns the value that is currently set in the :samp:`FPSCR` register.

.. function:: void __builtin_sh_set_fpscr(unsigned intval)

  Sets the :samp:`FPSCR` register to the specified value :samp:`{val}` , while
  preserving the current values of the FR, SZ and PR bits.

.. _sparc-vis-built-in-functions:

SPARC VIS Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC supports SIMD operations on the SPARC using both the generic vector
extensions (see :ref:`vector-extensions`) as well as built-in functions for
the SPARC Visual Instruction Set (VIS).  When you use the :option:`-mvis`
switch, the VIS extension is exposed as the following built-in functions:

.. code-block:: c++

  typedef int v1si __attribute__ ((vector_size (4)));
  typedef int v2si __attribute__ ((vector_size (8)));
  typedef short v4hi __attribute__ ((vector_size (8)));
  typedef short v2hi __attribute__ ((vector_size (4)));
  typedef unsigned char v8qi __attribute__ ((vector_size (8)));
  typedef unsigned char v4qi __attribute__ ((vector_size (4)));

  void __builtin_vis_write_gsr (int64_t);
  int64_t __builtin_vis_read_gsr (void);

  void * __builtin_vis_alignaddr (void *, long);
  void * __builtin_vis_alignaddrl (void *, long);
  int64_t __builtin_vis_faligndatadi (int64_t, int64_t);
  v2si __builtin_vis_faligndatav2si (v2si, v2si);
  v4hi __builtin_vis_faligndatav4hi (v4si, v4si);
  v8qi __builtin_vis_faligndatav8qi (v8qi, v8qi);

  v4hi __builtin_vis_fexpand (v4qi);

  v4hi __builtin_vis_fmul8x16 (v4qi, v4hi);
  v4hi __builtin_vis_fmul8x16au (v4qi, v2hi);
  v4hi __builtin_vis_fmul8x16al (v4qi, v2hi);
  v4hi __builtin_vis_fmul8sux16 (v8qi, v4hi);
  v4hi __builtin_vis_fmul8ulx16 (v8qi, v4hi);
  v2si __builtin_vis_fmuld8sux16 (v4qi, v2hi);
  v2si __builtin_vis_fmuld8ulx16 (v4qi, v2hi);

  v4qi __builtin_vis_fpack16 (v4hi);
  v8qi __builtin_vis_fpack32 (v2si, v8qi);
  v2hi __builtin_vis_fpackfix (v2si);
  v8qi __builtin_vis_fpmerge (v4qi, v4qi);

  int64_t __builtin_vis_pdist (v8qi, v8qi, int64_t);

  long __builtin_vis_edge8 (void *, void *);
  long __builtin_vis_edge8l (void *, void *);
  long __builtin_vis_edge16 (void *, void *);
  long __builtin_vis_edge16l (void *, void *);
  long __builtin_vis_edge32 (void *, void *);
  long __builtin_vis_edge32l (void *, void *);

  long __builtin_vis_fcmple16 (v4hi, v4hi);
  long __builtin_vis_fcmple32 (v2si, v2si);
  long __builtin_vis_fcmpne16 (v4hi, v4hi);
  long __builtin_vis_fcmpne32 (v2si, v2si);
  long __builtin_vis_fcmpgt16 (v4hi, v4hi);
  long __builtin_vis_fcmpgt32 (v2si, v2si);
  long __builtin_vis_fcmpeq16 (v4hi, v4hi);
  long __builtin_vis_fcmpeq32 (v2si, v2si);

  v4hi __builtin_vis_fpadd16 (v4hi, v4hi);
  v2hi __builtin_vis_fpadd16s (v2hi, v2hi);
  v2si __builtin_vis_fpadd32 (v2si, v2si);
  v1si __builtin_vis_fpadd32s (v1si, v1si);
  v4hi __builtin_vis_fpsub16 (v4hi, v4hi);
  v2hi __builtin_vis_fpsub16s (v2hi, v2hi);
  v2si __builtin_vis_fpsub32 (v2si, v2si);
  v1si __builtin_vis_fpsub32s (v1si, v1si);

  long __builtin_vis_array8 (long, long);
  long __builtin_vis_array16 (long, long);
  long __builtin_vis_array32 (long, long);

When you use the :option:`-mvis2` switch, the VIS version 2.0 built-in
functions also become available:

.. code-block:: c++

  long __builtin_vis_bmask (long, long);
  int64_t __builtin_vis_bshuffledi (int64_t, int64_t);
  v2si __builtin_vis_bshufflev2si (v2si, v2si);
  v4hi __builtin_vis_bshufflev2si (v4hi, v4hi);
  v8qi __builtin_vis_bshufflev2si (v8qi, v8qi);

  long __builtin_vis_edge8n (void *, void *);
  long __builtin_vis_edge8ln (void *, void *);
  long __builtin_vis_edge16n (void *, void *);
  long __builtin_vis_edge16ln (void *, void *);
  long __builtin_vis_edge32n (void *, void *);
  long __builtin_vis_edge32ln (void *, void *);

When you use the :option:`-mvis3` switch, the VIS version 3.0 built-in
functions also become available:

.. code-block:: c++

  void __builtin_vis_cmask8 (long);
  void __builtin_vis_cmask16 (long);
  void __builtin_vis_cmask32 (long);

  v4hi __builtin_vis_fchksm16 (v4hi, v4hi);

  v4hi __builtin_vis_fsll16 (v4hi, v4hi);
  v4hi __builtin_vis_fslas16 (v4hi, v4hi);
  v4hi __builtin_vis_fsrl16 (v4hi, v4hi);
  v4hi __builtin_vis_fsra16 (v4hi, v4hi);
  v2si __builtin_vis_fsll16 (v2si, v2si);
  v2si __builtin_vis_fslas16 (v2si, v2si);
  v2si __builtin_vis_fsrl16 (v2si, v2si);
  v2si __builtin_vis_fsra16 (v2si, v2si);

  long __builtin_vis_pdistn (v8qi, v8qi);

  v4hi __builtin_vis_fmean16 (v4hi, v4hi);

  int64_t __builtin_vis_fpadd64 (int64_t, int64_t);
  int64_t __builtin_vis_fpsub64 (int64_t, int64_t);

  v4hi __builtin_vis_fpadds16 (v4hi, v4hi);
  v2hi __builtin_vis_fpadds16s (v2hi, v2hi);
  v4hi __builtin_vis_fpsubs16 (v4hi, v4hi);
  v2hi __builtin_vis_fpsubs16s (v2hi, v2hi);
  v2si __builtin_vis_fpadds32 (v2si, v2si);
  v1si __builtin_vis_fpadds32s (v1si, v1si);
  v2si __builtin_vis_fpsubs32 (v2si, v2si);
  v1si __builtin_vis_fpsubs32s (v1si, v1si);

  long __builtin_vis_fucmple8 (v8qi, v8qi);
  long __builtin_vis_fucmpne8 (v8qi, v8qi);
  long __builtin_vis_fucmpgt8 (v8qi, v8qi);
  long __builtin_vis_fucmpeq8 (v8qi, v8qi);

  float __builtin_vis_fhadds (float, float);
  double __builtin_vis_fhaddd (double, double);
  float __builtin_vis_fhsubs (float, float);
  double __builtin_vis_fhsubd (double, double);
  float __builtin_vis_fnhadds (float, float);
  double __builtin_vis_fnhaddd (double, double);

  int64_t __builtin_vis_umulxhi (int64_t, int64_t);
  int64_t __builtin_vis_xmulx (int64_t, int64_t);
  int64_t __builtin_vis_xmulxhi (int64_t, int64_t);

When you use the :option:`-mvis4` switch, the VIS version 4.0 built-in
functions also become available:

.. code-block:: c++

  v8qi __builtin_vis_fpadd8 (v8qi, v8qi);
  v8qi __builtin_vis_fpadds8 (v8qi, v8qi);
  v8qi __builtin_vis_fpaddus8 (v8qi, v8qi);
  v4hi __builtin_vis_fpaddus16 (v4hi, v4hi);

  v8qi __builtin_vis_fpsub8 (v8qi, v8qi);
  v8qi __builtin_vis_fpsubs8 (v8qi, v8qi);
  v8qi __builtin_vis_fpsubus8 (v8qi, v8qi);
  v4hi __builtin_vis_fpsubus16 (v4hi, v4hi);

  long __builtin_vis_fpcmple8 (v8qi, v8qi);
  long __builtin_vis_fpcmpgt8 (v8qi, v8qi);
  long __builtin_vis_fpcmpule16 (v4hi, v4hi);
  long __builtin_vis_fpcmpugt16 (v4hi, v4hi);
  long __builtin_vis_fpcmpule32 (v2si, v2si);
  long __builtin_vis_fpcmpugt32 (v2si, v2si);

  v8qi __builtin_vis_fpmax8 (v8qi, v8qi);
  v4hi __builtin_vis_fpmax16 (v4hi, v4hi);
  v2si __builtin_vis_fpmax32 (v2si, v2si);

  v8qi __builtin_vis_fpmaxu8 (v8qi, v8qi);
  v4hi __builtin_vis_fpmaxu16 (v4hi, v4hi);
  v2si __builtin_vis_fpmaxu32 (v2si, v2si);

  v8qi __builtin_vis_fpmin8 (v8qi, v8qi);
  v4hi __builtin_vis_fpmin16 (v4hi, v4hi);
  v2si __builtin_vis_fpmin32 (v2si, v2si);

  v8qi __builtin_vis_fpminu8 (v8qi, v8qi);
  v4hi __builtin_vis_fpminu16 (v4hi, v4hi);
  v2si __builtin_vis_fpminu32 (v2si, v2si);

When you use the :option:`-mvis4b` switch, the VIS version 4.0B
built-in functions also become available:

.. code-block:: c++

  v8qi __builtin_vis_dictunpack8 (double, int);
  v4hi __builtin_vis_dictunpack16 (double, int);
  v2si __builtin_vis_dictunpack32 (double, int);

  long __builtin_vis_fpcmple8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpgt8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpeq8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpne8shl (v8qi, v8qi, int);

  long __builtin_vis_fpcmple16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpgt16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpeq16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpne16shl (v4hi, v4hi, int);

  long __builtin_vis_fpcmple32shl (v2si, v2si, int);
  long __builtin_vis_fpcmpgt32shl (v2si, v2si, int);
  long __builtin_vis_fpcmpeq32shl (v2si, v2si, int);
  long __builtin_vis_fpcmpne32shl (v2si, v2si, int);

  long __builtin_vis_fpcmpule8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpugt8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpule16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpugt16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpule32shl (v2si, v2si, int);
  long __builtin_vis_fpcmpugt32shl (v2si, v2si, int);

  long __builtin_vis_fpcmpde8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpde16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpde32shl (v2si, v2si, int);

  long __builtin_vis_fpcmpur8shl (v8qi, v8qi, int);
  long __builtin_vis_fpcmpur16shl (v4hi, v4hi, int);
  long __builtin_vis_fpcmpur32shl (v2si, v2si, int);

.. _ti-c6x-built-in-functions:

TI C6X Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides intrinsics to access certain instructions of the TI C6X
processors.  These intrinsics, listed below, are available after
inclusion of the ``c6x_intrinsics.h`` header file.  They map directly
to C6X instructions.

.. code-block:: c++

  int _sadd (int, int)
  int _ssub (int, int)
  int _sadd2 (int, int)
  int _ssub2 (int, int)
  long long _mpy2 (int, int)
  long long _smpy2 (int, int)
  int _add4 (int, int)
  int _sub4 (int, int)
  int _saddu4 (int, int)

  int _smpy (int, int)
  int _smpyh (int, int)
  int _smpyhl (int, int)
  int _smpylh (int, int)

  int _sshl (int, int)
  int _subc (int, int)

  int _avg2 (int, int)
  int _avgu4 (int, int)

  int _clrr (int, int)
  int _extr (int, int)
  int _extru (int, int)
  int _abs (int)
  int _abs2 (int)

.. _tile-gx-built-in-functions:

TILE-Gx Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides intrinsics to access every instruction of the TILE-Gx
processor.  The intrinsics are of the form:

.. code-block:: c++

  unsigned long long __insn_op (...)

Where :samp:`{op}` is the name of the instruction.  Refer to the ISA manual
for the complete list of instructions.

GCC also provides intrinsics to directly access the network registers.
The intrinsics are:

.. code-block:: c++

  unsigned long long __tile_idn0_receive (void)
  unsigned long long __tile_idn1_receive (void)
  unsigned long long __tile_udn0_receive (void)
  unsigned long long __tile_udn1_receive (void)
  unsigned long long __tile_udn2_receive (void)
  unsigned long long __tile_udn3_receive (void)
  void __tile_idn_send (unsigned long long)
  void __tile_udn_send (unsigned long long)

The intrinsic ``void __tile_network_barrier (void)`` is used to
guarantee that no network operations before it are reordered with
those after it.

.. _tilepro-built-in-functions:

TILEPro Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC provides intrinsics to access every instruction of the TILEPro
processor.  The intrinsics are of the form:

.. code-block:: c++

  unsigned __insn_op (...)

where :samp:`{op}` is the name of the instruction.  Refer to the ISA manual
for the complete list of instructions.

GCC also provides intrinsics to directly access the network registers.
The intrinsics are:

.. code-block:: c++

  unsigned __tile_idn0_receive (void)
  unsigned __tile_idn1_receive (void)
  unsigned __tile_sn_receive (void)
  unsigned __tile_udn0_receive (void)
  unsigned __tile_udn1_receive (void)
  unsigned __tile_udn2_receive (void)
  unsigned __tile_udn3_receive (void)
  void __tile_idn_send (unsigned)
  void __tile_sn_send (unsigned)
  void __tile_udn_send (unsigned)

The intrinsic ``void __tile_network_barrier (void)`` is used to
guarantee that no network operations before it are reordered with
those after it.

.. _x86-built-in-functions:

x86 Built-in Functions
^^^^^^^^^^^^^^^^^^^^^^

These built-in functions are available for the x86-32 and x86-64 family
of computers, depending on the command-line switches used.

If you specify command-line switches such as :option:`-msse` ,
the compiler could use the extended instruction sets even if the built-ins
are not used explicitly in the program.  For this reason, applications
that perform run-time CPU detection must compile separate files for each
supported architecture, using the appropriate flags.  In particular,
the file containing the CPU detection code should be compiled without
these options.

The following machine modes are available for use with MMX built-in functions
(see :ref:`vector-extensions`): ``V2SI`` for a vector of two 32-bit integers,
``V4HI`` for a vector of four 16-bit integers, and ``V8QI`` for a
vector of eight 8-bit integers.  Some of the built-in functions operate on
MMX registers as a whole 64-bit entity, these use ``V1DI`` as their mode.

If 3DNow! extensions are enabled, ``V2SF`` is used as a mode for a vector
of two 32-bit floating-point values.

If SSE extensions are enabled, ``V4SF`` is used for a vector of four 32-bit
floating-point values.  Some instructions use a vector of four 32-bit
integers, these use ``V4SI``.  Finally, some instructions operate on an
entire vector register, interpreting it as a 128-bit integer, these use mode
``TI``.

The x86-32 and x86-64 family of processors use additional built-in
functions for efficient use of ``TF`` (``__float128``) 128-bit
floating point and ``TC`` 128-bit complex floating-point values.

The following floating-point built-in functions are always available.  All
of them implement the function that is part of the name.

.. code-block:: c++

  __float128 __builtin_fabsq (__float128)
  __float128 __builtin_copysignq (__float128, __float128)

The following built-in functions are always available.

``__float128 __builtin_infq (void)``
  Similar to ``__builtin_inf``, except the return type is ``__float128``.

  .. index:: __builtin_infq

``__float128 __builtin_huge_valq (void)``
  Similar to ``__builtin_huge_val``, except the return type is ``__float128``.

  .. index:: __builtin_huge_valq

``__float128 __builtin_nanq (void)``
  Similar to ``__builtin_nan``, except the return type is ``__float128``.

  .. index:: __builtin_nanq

``__float128 __builtin_nansq (void)``
  Similar to ``__builtin_nans``, except the return type is ``__float128``.

  .. index:: __builtin_nansq

  The following built-in function is always available.

``void __builtin_ia32_pause (void)``
  Generates the ``pause`` machine instruction with a compiler memory
  barrier.

  The following built-in functions are always available and can be used to
check the target platform type.

.. function:: void __builtin_cpu_init(void )

  This function runs the CPU detection code to check the type of CPU and the
  features supported.  This built-in function needs to be invoked along with the built-in functions
  to check CPU type and features, ``__builtin_cpu_is`` and
  ``__builtin_cpu_supports``, only when used in a function that is
  executed before any constructors are called.  The CPU detection code is
  automatically executed in a very high priority constructor.

  For example, this function has to be used in ``ifunc`` resolvers that
  check for CPU type using the built-in functions ``__builtin_cpu_is``
  and ``__builtin_cpu_supports``, or in constructors on targets that
  don't support constructor priority.

  .. code-block:: c++

    static void (*resolve_memcpy (void)) (void)
    {
      // ifunc resolvers fire before constructors, explicitly call the init
      // function.
      __builtin_cpu_init ();
      if (__builtin_cpu_supports ("ssse3"))
        return ssse3_memcpy; // super fast memcpy with ssse3 instructions.
      else
        return default_memcpy;
    }

    void *memcpy (void *, const void *, size_t)
         __attribute__ ((ifunc ("resolve_memcpy")));

.. function:: int __builtin_cpu_is(const char* cpuname)

  This function returns a positive integer if the run-time CPU
  is of type :samp:`{cpuname}`
  and returns ``0`` otherwise. The following CPU names can be detected:

  :samp:`amd`
    AMD CPU.

  :samp:`intel`
    Intel CPU.

  :samp:`atom`
    Intel Atom CPU.

  :samp:`slm`
    Intel Silvermont CPU.

  :samp:`core2`
    Intel Core 2 CPU.

  :samp:`corei7`
    Intel Core i7 CPU.

  :samp:`nehalem`
    Intel Core i7 Nehalem CPU.

  :samp:`westmere`
    Intel Core i7 Westmere CPU.

  :samp:`sandybridge`
    Intel Core i7 Sandy Bridge CPU.

  :samp:`ivybridge`
    Intel Core i7 Ivy Bridge CPU.

  :samp:`haswell`
    Intel Core i7 Haswell CPU.

  :samp:`broadwell`
    Intel Core i7 Broadwell CPU.

  :samp:`skylake`
    Intel Core i7 Skylake CPU.

  :samp:`skylake-avx512`
    Intel Core i7 Skylake AVX512 CPU.

  :samp:`cannonlake`
    Intel Core i7 Cannon Lake CPU.

  :samp:`icelake-client`
    Intel Core i7 Ice Lake Client CPU.

  :samp:`icelake-server`
    Intel Core i7 Ice Lake Server CPU.

  :samp:`cascadelake`
    Intel Core i7 Cascadelake CPU.

  :samp:`tigerlake`
    Intel Core i7 Tigerlake CPU.

  :samp:`cooperlake`
    Intel Core i7 Cooperlake CPU.

  :samp:`sapphirerapids`
    Intel Core i7 sapphirerapids CPU.

  :samp:`alderlake`
    Intel Core i7 Alderlake CPU.

  :samp:`rocketlake`
    Intel Core i7 Rocketlake CPU.

  :samp:`bonnell`
    Intel Atom Bonnell CPU.

  :samp:`silvermont`
    Intel Atom Silvermont CPU.

  :samp:`goldmont`
    Intel Atom Goldmont CPU.

  :samp:`goldmont-plus`
    Intel Atom Goldmont Plus CPU.

  :samp:`tremont`
    Intel Atom Tremont CPU.

  :samp:`knl`
    Intel Knights Landing CPU.

  :samp:`knm`
    Intel Knights Mill CPU.

  :samp:`amdfam10h`
    AMD Family 10h CPU.

  :samp:`barcelona`
    AMD Family 10h Barcelona CPU.

  :samp:`shanghai`
    AMD Family 10h Shanghai CPU.

  :samp:`istanbul`
    AMD Family 10h Istanbul CPU.

  :samp:`btver1`
    AMD Family 14h CPU.

  :samp:`amdfam15h`
    AMD Family 15h CPU.

  :samp:`bdver1`
    AMD Family 15h Bulldozer version 1.

  :samp:`bdver2`
    AMD Family 15h Bulldozer version 2.

  :samp:`bdver3`
    AMD Family 15h Bulldozer version 3.

  :samp:`bdver4`
    AMD Family 15h Bulldozer version 4.

  :samp:`btver2`
    AMD Family 16h CPU.

  :samp:`amdfam17h`
    AMD Family 17h CPU.

  :samp:`znver1`
    AMD Family 17h Zen version 1.

  :samp:`znver2`
    AMD Family 17h Zen version 2.

  :samp:`amdfam19h`
    AMD Family 19h CPU.

  :samp:`znver3`
    AMD Family 19h Zen version 3.

    Here is an example:

  .. code-block:: c++

    if (__builtin_cpu_is ("corei7"))
      {
         do_corei7 (); // Core i7 specific implementation.
      }
    else
      {
         do_generic (); // Generic implementation.
      }

.. function:: int __builtin_cpu_supports(const char* feature)

  This function returns a positive integer if the run-time CPU
  supports :samp:`{feature}`
  and returns ``0`` otherwise. The following features can be detected:

  :samp:`cmov`
    CMOV instruction.

  :samp:`mmx`
    MMX instructions.

  :samp:`popcnt`
    POPCNT instruction.

  :samp:`sse`
    SSE instructions.

  :samp:`sse2`
    SSE2 instructions.

  :samp:`sse3`
    SSE3 instructions.

  :samp:`ssse3`
    SSSE3 instructions.

  :samp:`sse4.1`
    SSE4.1 instructions.

  :samp:`sse4.2`
    SSE4.2 instructions.

  :samp:`avx`
    AVX instructions.

  :samp:`avx2`
    AVX2 instructions.

  :samp:`sse4a`
    SSE4A instructions.

  :samp:`fma4`
    FMA4 instructions.

  :samp:`xop`
    XOP instructions.

  :samp:`fma`
    FMA instructions.

  :samp:`avx512f`
    AVX512F instructions.

  :samp:`bmi`
    BMI instructions.

  :samp:`bmi2`
    BMI2 instructions.

  :samp:`aes`
    AES instructions.

  :samp:`pclmul`
    PCLMUL instructions.

  :samp:`avx512vl`
    AVX512VL instructions.

  :samp:`avx512bw`
    AVX512BW instructions.

  :samp:`avx512dq`
    AVX512DQ instructions.

  :samp:`avx512cd`
    AVX512CD instructions.

  :samp:`avx512er`
    AVX512ER instructions.

  :samp:`avx512pf`
    AVX512PF instructions.

  :samp:`avx512vbmi`
    AVX512VBMI instructions.

  :samp:`avx512ifma`
    AVX512IFMA instructions.

  :samp:`avx5124vnniw`
    AVX5124VNNIW instructions.

  :samp:`avx5124fmaps`
    AVX5124FMAPS instructions.

  :samp:`avx512vpopcntdq`
    AVX512VPOPCNTDQ instructions.

  :samp:`avx512vbmi2`
    AVX512VBMI2 instructions.

  :samp:`gfni`
    GFNI instructions.

  :samp:`vpclmulqdq`
    VPCLMULQDQ instructions.

  :samp:`avx512vnni`
    AVX512VNNI instructions.

  :samp:`avx512bitalg`
    AVX512BITALG instructions.

    Here is an example:

  .. code-block:: c++

    if (__builtin_cpu_supports ("popcnt"))
      {
         asm("popcnt %1,%0" : "=r"(count) : "rm"(n) : "cc");
      }
    else
      {
         count = generic_countbits (n); //generic implementation.
      }

The following built-in functions are made available by :option:`-mmmx`.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v8qi __builtin_ia32_paddb (v8qi, v8qi)
  v4hi __builtin_ia32_paddw (v4hi, v4hi)
  v2si __builtin_ia32_paddd (v2si, v2si)
  v8qi __builtin_ia32_psubb (v8qi, v8qi)
  v4hi __builtin_ia32_psubw (v4hi, v4hi)
  v2si __builtin_ia32_psubd (v2si, v2si)
  v8qi __builtin_ia32_paddsb (v8qi, v8qi)
  v4hi __builtin_ia32_paddsw (v4hi, v4hi)
  v8qi __builtin_ia32_psubsb (v8qi, v8qi)
  v4hi __builtin_ia32_psubsw (v4hi, v4hi)
  v8qi __builtin_ia32_paddusb (v8qi, v8qi)
  v4hi __builtin_ia32_paddusw (v4hi, v4hi)
  v8qi __builtin_ia32_psubusb (v8qi, v8qi)
  v4hi __builtin_ia32_psubusw (v4hi, v4hi)
  v4hi __builtin_ia32_pmullw (v4hi, v4hi)
  v4hi __builtin_ia32_pmulhw (v4hi, v4hi)
  di __builtin_ia32_pand (di, di)
  di __builtin_ia32_pandn (di,di)
  di __builtin_ia32_por (di, di)
  di __builtin_ia32_pxor (di, di)
  v8qi __builtin_ia32_pcmpeqb (v8qi, v8qi)
  v4hi __builtin_ia32_pcmpeqw (v4hi, v4hi)
  v2si __builtin_ia32_pcmpeqd (v2si, v2si)
  v8qi __builtin_ia32_pcmpgtb (v8qi, v8qi)
  v4hi __builtin_ia32_pcmpgtw (v4hi, v4hi)
  v2si __builtin_ia32_pcmpgtd (v2si, v2si)
  v8qi __builtin_ia32_punpckhbw (v8qi, v8qi)
  v4hi __builtin_ia32_punpckhwd (v4hi, v4hi)
  v2si __builtin_ia32_punpckhdq (v2si, v2si)
  v8qi __builtin_ia32_punpcklbw (v8qi, v8qi)
  v4hi __builtin_ia32_punpcklwd (v4hi, v4hi)
  v2si __builtin_ia32_punpckldq (v2si, v2si)
  v8qi __builtin_ia32_packsswb (v4hi, v4hi)
  v4hi __builtin_ia32_packssdw (v2si, v2si)
  v8qi __builtin_ia32_packuswb (v4hi, v4hi)

  v4hi __builtin_ia32_psllw (v4hi, v4hi)
  v2si __builtin_ia32_pslld (v2si, v2si)
  v1di __builtin_ia32_psllq (v1di, v1di)
  v4hi __builtin_ia32_psrlw (v4hi, v4hi)
  v2si __builtin_ia32_psrld (v2si, v2si)
  v1di __builtin_ia32_psrlq (v1di, v1di)
  v4hi __builtin_ia32_psraw (v4hi, v4hi)
  v2si __builtin_ia32_psrad (v2si, v2si)
  v4hi __builtin_ia32_psllwi (v4hi, int)
  v2si __builtin_ia32_pslldi (v2si, int)
  v1di __builtin_ia32_psllqi (v1di, int)
  v4hi __builtin_ia32_psrlwi (v4hi, int)
  v2si __builtin_ia32_psrldi (v2si, int)
  v1di __builtin_ia32_psrlqi (v1di, int)
  v4hi __builtin_ia32_psrawi (v4hi, int)
  v2si __builtin_ia32_psradi (v2si, int)

The following built-in functions are made available either with
:option:`-msse` , or with :option:`-m3dnowa`.  All of them generate
the machine instruction that is part of the name.

.. code-block:: c++

  v4hi __builtin_ia32_pmulhuw (v4hi, v4hi)
  v8qi __builtin_ia32_pavgb (v8qi, v8qi)
  v4hi __builtin_ia32_pavgw (v4hi, v4hi)
  v1di __builtin_ia32_psadbw (v8qi, v8qi)
  v8qi __builtin_ia32_pmaxub (v8qi, v8qi)
  v4hi __builtin_ia32_pmaxsw (v4hi, v4hi)
  v8qi __builtin_ia32_pminub (v8qi, v8qi)
  v4hi __builtin_ia32_pminsw (v4hi, v4hi)
  int __builtin_ia32_pmovmskb (v8qi)
  void __builtin_ia32_maskmovq (v8qi, v8qi, char *)
  void __builtin_ia32_movntq (di *, di)
  void __builtin_ia32_sfence (void)

The following built-in functions are available when :option:`-msse` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  int __builtin_ia32_comieq (v4sf, v4sf)
  int __builtin_ia32_comineq (v4sf, v4sf)
  int __builtin_ia32_comilt (v4sf, v4sf)
  int __builtin_ia32_comile (v4sf, v4sf)
  int __builtin_ia32_comigt (v4sf, v4sf)
  int __builtin_ia32_comige (v4sf, v4sf)
  int __builtin_ia32_ucomieq (v4sf, v4sf)
  int __builtin_ia32_ucomineq (v4sf, v4sf)
  int __builtin_ia32_ucomilt (v4sf, v4sf)
  int __builtin_ia32_ucomile (v4sf, v4sf)
  int __builtin_ia32_ucomigt (v4sf, v4sf)
  int __builtin_ia32_ucomige (v4sf, v4sf)
  v4sf __builtin_ia32_addps (v4sf, v4sf)
  v4sf __builtin_ia32_subps (v4sf, v4sf)
  v4sf __builtin_ia32_mulps (v4sf, v4sf)
  v4sf __builtin_ia32_divps (v4sf, v4sf)
  v4sf __builtin_ia32_addss (v4sf, v4sf)
  v4sf __builtin_ia32_subss (v4sf, v4sf)
  v4sf __builtin_ia32_mulss (v4sf, v4sf)
  v4sf __builtin_ia32_divss (v4sf, v4sf)
  v4sf __builtin_ia32_cmpeqps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpltps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpleps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpgtps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpgeps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpunordps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpneqps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpnltps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpnleps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpngtps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpngeps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpordps (v4sf, v4sf)
  v4sf __builtin_ia32_cmpeqss (v4sf, v4sf)
  v4sf __builtin_ia32_cmpltss (v4sf, v4sf)
  v4sf __builtin_ia32_cmpless (v4sf, v4sf)
  v4sf __builtin_ia32_cmpunordss (v4sf, v4sf)
  v4sf __builtin_ia32_cmpneqss (v4sf, v4sf)
  v4sf __builtin_ia32_cmpnltss (v4sf, v4sf)
  v4sf __builtin_ia32_cmpnless (v4sf, v4sf)
  v4sf __builtin_ia32_cmpordss (v4sf, v4sf)
  v4sf __builtin_ia32_maxps (v4sf, v4sf)
  v4sf __builtin_ia32_maxss (v4sf, v4sf)
  v4sf __builtin_ia32_minps (v4sf, v4sf)
  v4sf __builtin_ia32_minss (v4sf, v4sf)
  v4sf __builtin_ia32_andps (v4sf, v4sf)
  v4sf __builtin_ia32_andnps (v4sf, v4sf)
  v4sf __builtin_ia32_orps (v4sf, v4sf)
  v4sf __builtin_ia32_xorps (v4sf, v4sf)
  v4sf __builtin_ia32_movss (v4sf, v4sf)
  v4sf __builtin_ia32_movhlps (v4sf, v4sf)
  v4sf __builtin_ia32_movlhps (v4sf, v4sf)
  v4sf __builtin_ia32_unpckhps (v4sf, v4sf)
  v4sf __builtin_ia32_unpcklps (v4sf, v4sf)
  v4sf __builtin_ia32_cvtpi2ps (v4sf, v2si)
  v4sf __builtin_ia32_cvtsi2ss (v4sf, int)
  v2si __builtin_ia32_cvtps2pi (v4sf)
  int __builtin_ia32_cvtss2si (v4sf)
  v2si __builtin_ia32_cvttps2pi (v4sf)
  int __builtin_ia32_cvttss2si (v4sf)
  v4sf __builtin_ia32_rcpps (v4sf)
  v4sf __builtin_ia32_rsqrtps (v4sf)
  v4sf __builtin_ia32_sqrtps (v4sf)
  v4sf __builtin_ia32_rcpss (v4sf)
  v4sf __builtin_ia32_rsqrtss (v4sf)
  v4sf __builtin_ia32_sqrtss (v4sf)
  v4sf __builtin_ia32_shufps (v4sf, v4sf, int)
  void __builtin_ia32_movntps (float *, v4sf)
  int __builtin_ia32_movmskps (v4sf)

The following built-in functions are available when :option:`-msse` is used.

``v4sf __builtin_ia32_loadups (float *)``
  Generates the ``movups`` machine instruction as a load from memory.

``void __builtin_ia32_storeups (float *, v4sf)``
  Generates the ``movups`` machine instruction as a store to memory.

``v4sf __builtin_ia32_loadss (float *)``
  Generates the ``movss`` machine instruction as a load from memory.

``v4sf __builtin_ia32_loadhps (v4sf, const v2sf *)``
  Generates the ``movhps`` machine instruction as a load from memory.

``v4sf __builtin_ia32_loadlps (v4sf, const v2sf *)``
  Generates the ``movlps`` machine instruction as a load from memory

``void __builtin_ia32_storehps (v2sf *, v4sf)``
  Generates the ``movhps`` machine instruction as a store to memory.

``void __builtin_ia32_storelps (v2sf *, v4sf)``
  Generates the ``movlps`` machine instruction as a store to memory.

  The following built-in functions are available when :option:`-msse2` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  int __builtin_ia32_comisdeq (v2df, v2df)
  int __builtin_ia32_comisdlt (v2df, v2df)
  int __builtin_ia32_comisdle (v2df, v2df)
  int __builtin_ia32_comisdgt (v2df, v2df)
  int __builtin_ia32_comisdge (v2df, v2df)
  int __builtin_ia32_comisdneq (v2df, v2df)
  int __builtin_ia32_ucomisdeq (v2df, v2df)
  int __builtin_ia32_ucomisdlt (v2df, v2df)
  int __builtin_ia32_ucomisdle (v2df, v2df)
  int __builtin_ia32_ucomisdgt (v2df, v2df)
  int __builtin_ia32_ucomisdge (v2df, v2df)
  int __builtin_ia32_ucomisdneq (v2df, v2df)
  v2df __builtin_ia32_cmpeqpd (v2df, v2df)
  v2df __builtin_ia32_cmpltpd (v2df, v2df)
  v2df __builtin_ia32_cmplepd (v2df, v2df)
  v2df __builtin_ia32_cmpgtpd (v2df, v2df)
  v2df __builtin_ia32_cmpgepd (v2df, v2df)
  v2df __builtin_ia32_cmpunordpd (v2df, v2df)
  v2df __builtin_ia32_cmpneqpd (v2df, v2df)
  v2df __builtin_ia32_cmpnltpd (v2df, v2df)
  v2df __builtin_ia32_cmpnlepd (v2df, v2df)
  v2df __builtin_ia32_cmpngtpd (v2df, v2df)
  v2df __builtin_ia32_cmpngepd (v2df, v2df)
  v2df __builtin_ia32_cmpordpd (v2df, v2df)
  v2df __builtin_ia32_cmpeqsd (v2df, v2df)
  v2df __builtin_ia32_cmpltsd (v2df, v2df)
  v2df __builtin_ia32_cmplesd (v2df, v2df)
  v2df __builtin_ia32_cmpunordsd (v2df, v2df)
  v2df __builtin_ia32_cmpneqsd (v2df, v2df)
  v2df __builtin_ia32_cmpnltsd (v2df, v2df)
  v2df __builtin_ia32_cmpnlesd (v2df, v2df)
  v2df __builtin_ia32_cmpordsd (v2df, v2df)
  v2di __builtin_ia32_paddq (v2di, v2di)
  v2di __builtin_ia32_psubq (v2di, v2di)
  v2df __builtin_ia32_addpd (v2df, v2df)
  v2df __builtin_ia32_subpd (v2df, v2df)
  v2df __builtin_ia32_mulpd (v2df, v2df)
  v2df __builtin_ia32_divpd (v2df, v2df)
  v2df __builtin_ia32_addsd (v2df, v2df)
  v2df __builtin_ia32_subsd (v2df, v2df)
  v2df __builtin_ia32_mulsd (v2df, v2df)
  v2df __builtin_ia32_divsd (v2df, v2df)
  v2df __builtin_ia32_minpd (v2df, v2df)
  v2df __builtin_ia32_maxpd (v2df, v2df)
  v2df __builtin_ia32_minsd (v2df, v2df)
  v2df __builtin_ia32_maxsd (v2df, v2df)
  v2df __builtin_ia32_andpd (v2df, v2df)
  v2df __builtin_ia32_andnpd (v2df, v2df)
  v2df __builtin_ia32_orpd (v2df, v2df)
  v2df __builtin_ia32_xorpd (v2df, v2df)
  v2df __builtin_ia32_movsd (v2df, v2df)
  v2df __builtin_ia32_unpckhpd (v2df, v2df)
  v2df __builtin_ia32_unpcklpd (v2df, v2df)
  v16qi __builtin_ia32_paddb128 (v16qi, v16qi)
  v8hi __builtin_ia32_paddw128 (v8hi, v8hi)
  v4si __builtin_ia32_paddd128 (v4si, v4si)
  v2di __builtin_ia32_paddq128 (v2di, v2di)
  v16qi __builtin_ia32_psubb128 (v16qi, v16qi)
  v8hi __builtin_ia32_psubw128 (v8hi, v8hi)
  v4si __builtin_ia32_psubd128 (v4si, v4si)
  v2di __builtin_ia32_psubq128 (v2di, v2di)
  v8hi __builtin_ia32_pmullw128 (v8hi, v8hi)
  v8hi __builtin_ia32_pmulhw128 (v8hi, v8hi)
  v2di __builtin_ia32_pand128 (v2di, v2di)
  v2di __builtin_ia32_pandn128 (v2di, v2di)
  v2di __builtin_ia32_por128 (v2di, v2di)
  v2di __builtin_ia32_pxor128 (v2di, v2di)
  v16qi __builtin_ia32_pavgb128 (v16qi, v16qi)
  v8hi __builtin_ia32_pavgw128 (v8hi, v8hi)
  v16qi __builtin_ia32_pcmpeqb128 (v16qi, v16qi)
  v8hi __builtin_ia32_pcmpeqw128 (v8hi, v8hi)
  v4si __builtin_ia32_pcmpeqd128 (v4si, v4si)
  v16qi __builtin_ia32_pcmpgtb128 (v16qi, v16qi)
  v8hi __builtin_ia32_pcmpgtw128 (v8hi, v8hi)
  v4si __builtin_ia32_pcmpgtd128 (v4si, v4si)
  v16qi __builtin_ia32_pmaxub128 (v16qi, v16qi)
  v8hi __builtin_ia32_pmaxsw128 (v8hi, v8hi)
  v16qi __builtin_ia32_pminub128 (v16qi, v16qi)
  v8hi __builtin_ia32_pminsw128 (v8hi, v8hi)
  v16qi __builtin_ia32_punpckhbw128 (v16qi, v16qi)
  v8hi __builtin_ia32_punpckhwd128 (v8hi, v8hi)
  v4si __builtin_ia32_punpckhdq128 (v4si, v4si)
  v2di __builtin_ia32_punpckhqdq128 (v2di, v2di)
  v16qi __builtin_ia32_punpcklbw128 (v16qi, v16qi)
  v8hi __builtin_ia32_punpcklwd128 (v8hi, v8hi)
  v4si __builtin_ia32_punpckldq128 (v4si, v4si)
  v2di __builtin_ia32_punpcklqdq128 (v2di, v2di)
  v16qi __builtin_ia32_packsswb128 (v8hi, v8hi)
  v8hi __builtin_ia32_packssdw128 (v4si, v4si)
  v16qi __builtin_ia32_packuswb128 (v8hi, v8hi)
  v8hi __builtin_ia32_pmulhuw128 (v8hi, v8hi)
  void __builtin_ia32_maskmovdqu (v16qi, v16qi)
  v2df __builtin_ia32_loadupd (double *)
  void __builtin_ia32_storeupd (double *, v2df)
  v2df __builtin_ia32_loadhpd (v2df, double const *)
  v2df __builtin_ia32_loadlpd (v2df, double const *)
  int __builtin_ia32_movmskpd (v2df)
  int __builtin_ia32_pmovmskb128 (v16qi)
  void __builtin_ia32_movnti (int *, int)
  void __builtin_ia32_movnti64 (long long int *, long long int)
  void __builtin_ia32_movntpd (double *, v2df)
  void __builtin_ia32_movntdq (v2df *, v2df)
  v4si __builtin_ia32_pshufd (v4si, int)
  v8hi __builtin_ia32_pshuflw (v8hi, int)
  v8hi __builtin_ia32_pshufhw (v8hi, int)
  v2di __builtin_ia32_psadbw128 (v16qi, v16qi)
  v2df __builtin_ia32_sqrtpd (v2df)
  v2df __builtin_ia32_sqrtsd (v2df)
  v2df __builtin_ia32_shufpd (v2df, v2df, int)
  v2df __builtin_ia32_cvtdq2pd (v4si)
  v4sf __builtin_ia32_cvtdq2ps (v4si)
  v4si __builtin_ia32_cvtpd2dq (v2df)
  v2si __builtin_ia32_cvtpd2pi (v2df)
  v4sf __builtin_ia32_cvtpd2ps (v2df)
  v4si __builtin_ia32_cvttpd2dq (v2df)
  v2si __builtin_ia32_cvttpd2pi (v2df)
  v2df __builtin_ia32_cvtpi2pd (v2si)
  int __builtin_ia32_cvtsd2si (v2df)
  int __builtin_ia32_cvttsd2si (v2df)
  long long __builtin_ia32_cvtsd2si64 (v2df)
  long long __builtin_ia32_cvttsd2si64 (v2df)
  v4si __builtin_ia32_cvtps2dq (v4sf)
  v2df __builtin_ia32_cvtps2pd (v4sf)
  v4si __builtin_ia32_cvttps2dq (v4sf)
  v2df __builtin_ia32_cvtsi2sd (v2df, int)
  v2df __builtin_ia32_cvtsi642sd (v2df, long long)
  v4sf __builtin_ia32_cvtsd2ss (v4sf, v2df)
  v2df __builtin_ia32_cvtss2sd (v2df, v4sf)
  void __builtin_ia32_clflush (const void *)
  void __builtin_ia32_lfence (void)
  void __builtin_ia32_mfence (void)
  v16qi __builtin_ia32_loaddqu (const char *)
  void __builtin_ia32_storedqu (char *, v16qi)
  v1di __builtin_ia32_pmuludq (v2si, v2si)
  v2di __builtin_ia32_pmuludq128 (v4si, v4si)
  v8hi __builtin_ia32_psllw128 (v8hi, v8hi)
  v4si __builtin_ia32_pslld128 (v4si, v4si)
  v2di __builtin_ia32_psllq128 (v2di, v2di)
  v8hi __builtin_ia32_psrlw128 (v8hi, v8hi)
  v4si __builtin_ia32_psrld128 (v4si, v4si)
  v2di __builtin_ia32_psrlq128 (v2di, v2di)
  v8hi __builtin_ia32_psraw128 (v8hi, v8hi)
  v4si __builtin_ia32_psrad128 (v4si, v4si)
  v2di __builtin_ia32_pslldqi128 (v2di, int)
  v8hi __builtin_ia32_psllwi128 (v8hi, int)
  v4si __builtin_ia32_pslldi128 (v4si, int)
  v2di __builtin_ia32_psllqi128 (v2di, int)
  v2di __builtin_ia32_psrldqi128 (v2di, int)
  v8hi __builtin_ia32_psrlwi128 (v8hi, int)
  v4si __builtin_ia32_psrldi128 (v4si, int)
  v2di __builtin_ia32_psrlqi128 (v2di, int)
  v8hi __builtin_ia32_psrawi128 (v8hi, int)
  v4si __builtin_ia32_psradi128 (v4si, int)
  v4si __builtin_ia32_pmaddwd128 (v8hi, v8hi)
  v2di __builtin_ia32_movq128 (v2di)

The following built-in functions are available when :option:`-msse3` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v2df __builtin_ia32_addsubpd (v2df, v2df)
  v4sf __builtin_ia32_addsubps (v4sf, v4sf)
  v2df __builtin_ia32_haddpd (v2df, v2df)
  v4sf __builtin_ia32_haddps (v4sf, v4sf)
  v2df __builtin_ia32_hsubpd (v2df, v2df)
  v4sf __builtin_ia32_hsubps (v4sf, v4sf)
  v16qi __builtin_ia32_lddqu (char const *)
  void __builtin_ia32_monitor (void *, unsigned int, unsigned int)
  v4sf __builtin_ia32_movshdup (v4sf)
  v4sf __builtin_ia32_movsldup (v4sf)
  void __builtin_ia32_mwait (unsigned int, unsigned int)

The following built-in functions are available when :option:`-mssse3` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v2si __builtin_ia32_phaddd (v2si, v2si)
  v4hi __builtin_ia32_phaddw (v4hi, v4hi)
  v4hi __builtin_ia32_phaddsw (v4hi, v4hi)
  v2si __builtin_ia32_phsubd (v2si, v2si)
  v4hi __builtin_ia32_phsubw (v4hi, v4hi)
  v4hi __builtin_ia32_phsubsw (v4hi, v4hi)
  v4hi __builtin_ia32_pmaddubsw (v8qi, v8qi)
  v4hi __builtin_ia32_pmulhrsw (v4hi, v4hi)
  v8qi __builtin_ia32_pshufb (v8qi, v8qi)
  v8qi __builtin_ia32_psignb (v8qi, v8qi)
  v2si __builtin_ia32_psignd (v2si, v2si)
  v4hi __builtin_ia32_psignw (v4hi, v4hi)
  v1di __builtin_ia32_palignr (v1di, v1di, int)
  v8qi __builtin_ia32_pabsb (v8qi)
  v2si __builtin_ia32_pabsd (v2si)
  v4hi __builtin_ia32_pabsw (v4hi)

The following built-in functions are available when :option:`-mssse3` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v4si __builtin_ia32_phaddd128 (v4si, v4si)
  v8hi __builtin_ia32_phaddw128 (v8hi, v8hi)
  v8hi __builtin_ia32_phaddsw128 (v8hi, v8hi)
  v4si __builtin_ia32_phsubd128 (v4si, v4si)
  v8hi __builtin_ia32_phsubw128 (v8hi, v8hi)
  v8hi __builtin_ia32_phsubsw128 (v8hi, v8hi)
  v8hi __builtin_ia32_pmaddubsw128 (v16qi, v16qi)
  v8hi __builtin_ia32_pmulhrsw128 (v8hi, v8hi)
  v16qi __builtin_ia32_pshufb128 (v16qi, v16qi)
  v16qi __builtin_ia32_psignb128 (v16qi, v16qi)
  v4si __builtin_ia32_psignd128 (v4si, v4si)
  v8hi __builtin_ia32_psignw128 (v8hi, v8hi)
  v2di __builtin_ia32_palignr128 (v2di, v2di, int)
  v16qi __builtin_ia32_pabsb128 (v16qi)
  v4si __builtin_ia32_pabsd128 (v4si)
  v8hi __builtin_ia32_pabsw128 (v8hi)

The following built-in functions are available when :option:`-msse4.1` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v2df __builtin_ia32_blendpd (v2df, v2df, const int)
  v4sf __builtin_ia32_blendps (v4sf, v4sf, const int)
  v2df __builtin_ia32_blendvpd (v2df, v2df, v2df)
  v4sf __builtin_ia32_blendvps (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_dppd (v2df, v2df, const int)
  v4sf __builtin_ia32_dpps (v4sf, v4sf, const int)
  v4sf __builtin_ia32_insertps128 (v4sf, v4sf, const int)
  v2di __builtin_ia32_movntdqa (v2di *);
  v16qi __builtin_ia32_mpsadbw128 (v16qi, v16qi, const int)
  v8hi __builtin_ia32_packusdw128 (v4si, v4si)
  v16qi __builtin_ia32_pblendvb128 (v16qi, v16qi, v16qi)
  v8hi __builtin_ia32_pblendw128 (v8hi, v8hi, const int)
  v2di __builtin_ia32_pcmpeqq (v2di, v2di)
  v8hi __builtin_ia32_phminposuw128 (v8hi)
  v16qi __builtin_ia32_pmaxsb128 (v16qi, v16qi)
  v4si __builtin_ia32_pmaxsd128 (v4si, v4si)
  v4si __builtin_ia32_pmaxud128 (v4si, v4si)
  v8hi __builtin_ia32_pmaxuw128 (v8hi, v8hi)
  v16qi __builtin_ia32_pminsb128 (v16qi, v16qi)
  v4si __builtin_ia32_pminsd128 (v4si, v4si)
  v4si __builtin_ia32_pminud128 (v4si, v4si)
  v8hi __builtin_ia32_pminuw128 (v8hi, v8hi)
  v4si __builtin_ia32_pmovsxbd128 (v16qi)
  v2di __builtin_ia32_pmovsxbq128 (v16qi)
  v8hi __builtin_ia32_pmovsxbw128 (v16qi)
  v2di __builtin_ia32_pmovsxdq128 (v4si)
  v4si __builtin_ia32_pmovsxwd128 (v8hi)
  v2di __builtin_ia32_pmovsxwq128 (v8hi)
  v4si __builtin_ia32_pmovzxbd128 (v16qi)
  v2di __builtin_ia32_pmovzxbq128 (v16qi)
  v8hi __builtin_ia32_pmovzxbw128 (v16qi)
  v2di __builtin_ia32_pmovzxdq128 (v4si)
  v4si __builtin_ia32_pmovzxwd128 (v8hi)
  v2di __builtin_ia32_pmovzxwq128 (v8hi)
  v2di __builtin_ia32_pmuldq128 (v4si, v4si)
  v4si __builtin_ia32_pmulld128 (v4si, v4si)
  int __builtin_ia32_ptestc128 (v2di, v2di)
  int __builtin_ia32_ptestnzc128 (v2di, v2di)
  int __builtin_ia32_ptestz128 (v2di, v2di)
  v2df __builtin_ia32_roundpd (v2df, const int)
  v4sf __builtin_ia32_roundps (v4sf, const int)
  v2df __builtin_ia32_roundsd (v2df, v2df, const int)
  v4sf __builtin_ia32_roundss (v4sf, v4sf, const int)

The following built-in functions are available when :option:`-msse4.1` is
used.

``v4sf __builtin_ia32_vec_set_v4sf (v4sf, float, const int)``
  Generates the ``insertps`` machine instruction.

``int __builtin_ia32_vec_ext_v16qi (v16qi, const int)``
  Generates the ``pextrb`` machine instruction.

``v16qi __builtin_ia32_vec_set_v16qi (v16qi, int, const int)``
  Generates the ``pinsrb`` machine instruction.

``v4si __builtin_ia32_vec_set_v4si (v4si, int, const int)``
  Generates the ``pinsrd`` machine instruction.

``v2di __builtin_ia32_vec_set_v2di (v2di, long long, const int)``
  Generates the ``pinsrq`` machine instruction in 64bit mode.

  The following built-in functions are changed to generate new SSE4.1
instructions when :option:`-msse4.1` is used.

``float __builtin_ia32_vec_ext_v4sf (v4sf, const int)``
  Generates the ``extractps`` machine instruction.

``int __builtin_ia32_vec_ext_v4si (v4si, const int)``
  Generates the ``pextrd`` machine instruction.

``long long __builtin_ia32_vec_ext_v2di (v2di, const int)``
  Generates the ``pextrq`` machine instruction in 64bit mode.

  The following built-in functions are available when :option:`-msse4.2` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v16qi __builtin_ia32_pcmpestrm128 (v16qi, int, v16qi, int, const int)
  int __builtin_ia32_pcmpestri128 (v16qi, int, v16qi, int, const int)
  int __builtin_ia32_pcmpestria128 (v16qi, int, v16qi, int, const int)
  int __builtin_ia32_pcmpestric128 (v16qi, int, v16qi, int, const int)
  int __builtin_ia32_pcmpestrio128 (v16qi, int, v16qi, int, const int)
  int __builtin_ia32_pcmpestris128 (v16qi, int, v16qi, int, const int)
  int __builtin_ia32_pcmpestriz128 (v16qi, int, v16qi, int, const int)
  v16qi __builtin_ia32_pcmpistrm128 (v16qi, v16qi, const int)
  int __builtin_ia32_pcmpistri128 (v16qi, v16qi, const int)
  int __builtin_ia32_pcmpistria128 (v16qi, v16qi, const int)
  int __builtin_ia32_pcmpistric128 (v16qi, v16qi, const int)
  int __builtin_ia32_pcmpistrio128 (v16qi, v16qi, const int)
  int __builtin_ia32_pcmpistris128 (v16qi, v16qi, const int)
  int __builtin_ia32_pcmpistriz128 (v16qi, v16qi, const int)
  v2di __builtin_ia32_pcmpgtq (v2di, v2di)

The following built-in functions are available when :option:`-msse4.2` is
used.

``unsigned int __builtin_ia32_crc32qi (unsigned int, unsigned char)``
  Generates the ``crc32b`` machine instruction.

``unsigned int __builtin_ia32_crc32hi (unsigned int, unsigned short)``
  Generates the ``crc32w`` machine instruction.

``unsigned int __builtin_ia32_crc32si (unsigned int, unsigned int)``
  Generates the ``crc32l`` machine instruction.

``unsigned long long __builtin_ia32_crc32di (unsigned long long, unsigned long long)``
  Generates the ``crc32q`` machine instruction.

  The following built-in functions are changed to generate new SSE4.2
instructions when :option:`-msse4.2` is used.

``int __builtin_popcount (unsigned int)``
  Generates the ``popcntl`` machine instruction.

``int __builtin_popcountl (unsigned long)``
  Generates the ``popcntl`` or ``popcntq`` machine instruction,
  depending on the size of ``unsigned long``.

``int __builtin_popcountll (unsigned long long)``
  Generates the ``popcntq`` machine instruction.

  The following built-in functions are available when :option:`-mavx` is
used. All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v4df __builtin_ia32_addpd256 (v4df,v4df)
  v8sf __builtin_ia32_addps256 (v8sf,v8sf)
  v4df __builtin_ia32_addsubpd256 (v4df,v4df)
  v8sf __builtin_ia32_addsubps256 (v8sf,v8sf)
  v4df __builtin_ia32_andnpd256 (v4df,v4df)
  v8sf __builtin_ia32_andnps256 (v8sf,v8sf)
  v4df __builtin_ia32_andpd256 (v4df,v4df)
  v8sf __builtin_ia32_andps256 (v8sf,v8sf)
  v4df __builtin_ia32_blendpd256 (v4df,v4df,int)
  v8sf __builtin_ia32_blendps256 (v8sf,v8sf,int)
  v4df __builtin_ia32_blendvpd256 (v4df,v4df,v4df)
  v8sf __builtin_ia32_blendvps256 (v8sf,v8sf,v8sf)
  v2df __builtin_ia32_cmppd (v2df,v2df,int)
  v4df __builtin_ia32_cmppd256 (v4df,v4df,int)
  v4sf __builtin_ia32_cmpps (v4sf,v4sf,int)
  v8sf __builtin_ia32_cmpps256 (v8sf,v8sf,int)
  v2df __builtin_ia32_cmpsd (v2df,v2df,int)
  v4sf __builtin_ia32_cmpss (v4sf,v4sf,int)
  v4df __builtin_ia32_cvtdq2pd256 (v4si)
  v8sf __builtin_ia32_cvtdq2ps256 (v8si)
  v4si __builtin_ia32_cvtpd2dq256 (v4df)
  v4sf __builtin_ia32_cvtpd2ps256 (v4df)
  v8si __builtin_ia32_cvtps2dq256 (v8sf)
  v4df __builtin_ia32_cvtps2pd256 (v4sf)
  v4si __builtin_ia32_cvttpd2dq256 (v4df)
  v8si __builtin_ia32_cvttps2dq256 (v8sf)
  v4df __builtin_ia32_divpd256 (v4df,v4df)
  v8sf __builtin_ia32_divps256 (v8sf,v8sf)
  v8sf __builtin_ia32_dpps256 (v8sf,v8sf,int)
  v4df __builtin_ia32_haddpd256 (v4df,v4df)
  v8sf __builtin_ia32_haddps256 (v8sf,v8sf)
  v4df __builtin_ia32_hsubpd256 (v4df,v4df)
  v8sf __builtin_ia32_hsubps256 (v8sf,v8sf)
  v32qi __builtin_ia32_lddqu256 (pcchar)
  v32qi __builtin_ia32_loaddqu256 (pcchar)
  v4df __builtin_ia32_loadupd256 (pcdouble)
  v8sf __builtin_ia32_loadups256 (pcfloat)
  v2df __builtin_ia32_maskloadpd (pcv2df,v2df)
  v4df __builtin_ia32_maskloadpd256 (pcv4df,v4df)
  v4sf __builtin_ia32_maskloadps (pcv4sf,v4sf)
  v8sf __builtin_ia32_maskloadps256 (pcv8sf,v8sf)
  void __builtin_ia32_maskstorepd (pv2df,v2df,v2df)
  void __builtin_ia32_maskstorepd256 (pv4df,v4df,v4df)
  void __builtin_ia32_maskstoreps (pv4sf,v4sf,v4sf)
  void __builtin_ia32_maskstoreps256 (pv8sf,v8sf,v8sf)
  v4df __builtin_ia32_maxpd256 (v4df,v4df)
  v8sf __builtin_ia32_maxps256 (v8sf,v8sf)
  v4df __builtin_ia32_minpd256 (v4df,v4df)
  v8sf __builtin_ia32_minps256 (v8sf,v8sf)
  v4df __builtin_ia32_movddup256 (v4df)
  int __builtin_ia32_movmskpd256 (v4df)
  int __builtin_ia32_movmskps256 (v8sf)
  v8sf __builtin_ia32_movshdup256 (v8sf)
  v8sf __builtin_ia32_movsldup256 (v8sf)
  v4df __builtin_ia32_mulpd256 (v4df,v4df)
  v8sf __builtin_ia32_mulps256 (v8sf,v8sf)
  v4df __builtin_ia32_orpd256 (v4df,v4df)
  v8sf __builtin_ia32_orps256 (v8sf,v8sf)
  v2df __builtin_ia32_pd_pd256 (v4df)
  v4df __builtin_ia32_pd256_pd (v2df)
  v4sf __builtin_ia32_ps_ps256 (v8sf)
  v8sf __builtin_ia32_ps256_ps (v4sf)
  int __builtin_ia32_ptestc256 (v4di,v4di,ptest)
  int __builtin_ia32_ptestnzc256 (v4di,v4di,ptest)
  int __builtin_ia32_ptestz256 (v4di,v4di,ptest)
  v8sf __builtin_ia32_rcpps256 (v8sf)
  v4df __builtin_ia32_roundpd256 (v4df,int)
  v8sf __builtin_ia32_roundps256 (v8sf,int)
  v8sf __builtin_ia32_rsqrtps_nr256 (v8sf)
  v8sf __builtin_ia32_rsqrtps256 (v8sf)
  v4df __builtin_ia32_shufpd256 (v4df,v4df,int)
  v8sf __builtin_ia32_shufps256 (v8sf,v8sf,int)
  v4si __builtin_ia32_si_si256 (v8si)
  v8si __builtin_ia32_si256_si (v4si)
  v4df __builtin_ia32_sqrtpd256 (v4df)
  v8sf __builtin_ia32_sqrtps_nr256 (v8sf)
  v8sf __builtin_ia32_sqrtps256 (v8sf)
  void __builtin_ia32_storedqu256 (pchar,v32qi)
  void __builtin_ia32_storeupd256 (pdouble,v4df)
  void __builtin_ia32_storeups256 (pfloat,v8sf)
  v4df __builtin_ia32_subpd256 (v4df,v4df)
  v8sf __builtin_ia32_subps256 (v8sf,v8sf)
  v4df __builtin_ia32_unpckhpd256 (v4df,v4df)
  v8sf __builtin_ia32_unpckhps256 (v8sf,v8sf)
  v4df __builtin_ia32_unpcklpd256 (v4df,v4df)
  v8sf __builtin_ia32_unpcklps256 (v8sf,v8sf)
  v4df __builtin_ia32_vbroadcastf128_pd256 (pcv2df)
  v8sf __builtin_ia32_vbroadcastf128_ps256 (pcv4sf)
  v4df __builtin_ia32_vbroadcastsd256 (pcdouble)
  v4sf __builtin_ia32_vbroadcastss (pcfloat)
  v8sf __builtin_ia32_vbroadcastss256 (pcfloat)
  v2df __builtin_ia32_vextractf128_pd256 (v4df,int)
  v4sf __builtin_ia32_vextractf128_ps256 (v8sf,int)
  v4si __builtin_ia32_vextractf128_si256 (v8si,int)
  v4df __builtin_ia32_vinsertf128_pd256 (v4df,v2df,int)
  v8sf __builtin_ia32_vinsertf128_ps256 (v8sf,v4sf,int)
  v8si __builtin_ia32_vinsertf128_si256 (v8si,v4si,int)
  v4df __builtin_ia32_vperm2f128_pd256 (v4df,v4df,int)
  v8sf __builtin_ia32_vperm2f128_ps256 (v8sf,v8sf,int)
  v8si __builtin_ia32_vperm2f128_si256 (v8si,v8si,int)
  v2df __builtin_ia32_vpermil2pd (v2df,v2df,v2di,int)
  v4df __builtin_ia32_vpermil2pd256 (v4df,v4df,v4di,int)
  v4sf __builtin_ia32_vpermil2ps (v4sf,v4sf,v4si,int)
  v8sf __builtin_ia32_vpermil2ps256 (v8sf,v8sf,v8si,int)
  v2df __builtin_ia32_vpermilpd (v2df,int)
  v4df __builtin_ia32_vpermilpd256 (v4df,int)
  v4sf __builtin_ia32_vpermilps (v4sf,int)
  v8sf __builtin_ia32_vpermilps256 (v8sf,int)
  v2df __builtin_ia32_vpermilvarpd (v2df,v2di)
  v4df __builtin_ia32_vpermilvarpd256 (v4df,v4di)
  v4sf __builtin_ia32_vpermilvarps (v4sf,v4si)
  v8sf __builtin_ia32_vpermilvarps256 (v8sf,v8si)
  int __builtin_ia32_vtestcpd (v2df,v2df,ptest)
  int __builtin_ia32_vtestcpd256 (v4df,v4df,ptest)
  int __builtin_ia32_vtestcps (v4sf,v4sf,ptest)
  int __builtin_ia32_vtestcps256 (v8sf,v8sf,ptest)
  int __builtin_ia32_vtestnzcpd (v2df,v2df,ptest)
  int __builtin_ia32_vtestnzcpd256 (v4df,v4df,ptest)
  int __builtin_ia32_vtestnzcps (v4sf,v4sf,ptest)
  int __builtin_ia32_vtestnzcps256 (v8sf,v8sf,ptest)
  int __builtin_ia32_vtestzpd (v2df,v2df,ptest)
  int __builtin_ia32_vtestzpd256 (v4df,v4df,ptest)
  int __builtin_ia32_vtestzps (v4sf,v4sf,ptest)
  int __builtin_ia32_vtestzps256 (v8sf,v8sf,ptest)
  void __builtin_ia32_vzeroall (void)
  void __builtin_ia32_vzeroupper (void)
  v4df __builtin_ia32_xorpd256 (v4df,v4df)
  v8sf __builtin_ia32_xorps256 (v8sf,v8sf)

The following built-in functions are available when :option:`-mavx2` is
used. All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v32qi __builtin_ia32_mpsadbw256 (v32qi,v32qi,int)
  v32qi __builtin_ia32_pabsb256 (v32qi)
  v16hi __builtin_ia32_pabsw256 (v16hi)
  v8si __builtin_ia32_pabsd256 (v8si)
  v16hi __builtin_ia32_packssdw256 (v8si,v8si)
  v32qi __builtin_ia32_packsswb256 (v16hi,v16hi)
  v16hi __builtin_ia32_packusdw256 (v8si,v8si)
  v32qi __builtin_ia32_packuswb256 (v16hi,v16hi)
  v32qi __builtin_ia32_paddb256 (v32qi,v32qi)
  v16hi __builtin_ia32_paddw256 (v16hi,v16hi)
  v8si __builtin_ia32_paddd256 (v8si,v8si)
  v4di __builtin_ia32_paddq256 (v4di,v4di)
  v32qi __builtin_ia32_paddsb256 (v32qi,v32qi)
  v16hi __builtin_ia32_paddsw256 (v16hi,v16hi)
  v32qi __builtin_ia32_paddusb256 (v32qi,v32qi)
  v16hi __builtin_ia32_paddusw256 (v16hi,v16hi)
  v4di __builtin_ia32_palignr256 (v4di,v4di,int)
  v4di __builtin_ia32_andsi256 (v4di,v4di)
  v4di __builtin_ia32_andnotsi256 (v4di,v4di)
  v32qi __builtin_ia32_pavgb256 (v32qi,v32qi)
  v16hi __builtin_ia32_pavgw256 (v16hi,v16hi)
  v32qi __builtin_ia32_pblendvb256 (v32qi,v32qi,v32qi)
  v16hi __builtin_ia32_pblendw256 (v16hi,v16hi,int)
  v32qi __builtin_ia32_pcmpeqb256 (v32qi,v32qi)
  v16hi __builtin_ia32_pcmpeqw256 (v16hi,v16hi)
  v8si __builtin_ia32_pcmpeqd256 (c8si,v8si)
  v4di __builtin_ia32_pcmpeqq256 (v4di,v4di)
  v32qi __builtin_ia32_pcmpgtb256 (v32qi,v32qi)
  v16hi __builtin_ia32_pcmpgtw256 (16hi,v16hi)
  v8si __builtin_ia32_pcmpgtd256 (v8si,v8si)
  v4di __builtin_ia32_pcmpgtq256 (v4di,v4di)
  v16hi __builtin_ia32_phaddw256 (v16hi,v16hi)
  v8si __builtin_ia32_phaddd256 (v8si,v8si)
  v16hi __builtin_ia32_phaddsw256 (v16hi,v16hi)
  v16hi __builtin_ia32_phsubw256 (v16hi,v16hi)
  v8si __builtin_ia32_phsubd256 (v8si,v8si)
  v16hi __builtin_ia32_phsubsw256 (v16hi,v16hi)
  v32qi __builtin_ia32_pmaddubsw256 (v32qi,v32qi)
  v16hi __builtin_ia32_pmaddwd256 (v16hi,v16hi)
  v32qi __builtin_ia32_pmaxsb256 (v32qi,v32qi)
  v16hi __builtin_ia32_pmaxsw256 (v16hi,v16hi)
  v8si __builtin_ia32_pmaxsd256 (v8si,v8si)
  v32qi __builtin_ia32_pmaxub256 (v32qi,v32qi)
  v16hi __builtin_ia32_pmaxuw256 (v16hi,v16hi)
  v8si __builtin_ia32_pmaxud256 (v8si,v8si)
  v32qi __builtin_ia32_pminsb256 (v32qi,v32qi)
  v16hi __builtin_ia32_pminsw256 (v16hi,v16hi)
  v8si __builtin_ia32_pminsd256 (v8si,v8si)
  v32qi __builtin_ia32_pminub256 (v32qi,v32qi)
  v16hi __builtin_ia32_pminuw256 (v16hi,v16hi)
  v8si __builtin_ia32_pminud256 (v8si,v8si)
  int __builtin_ia32_pmovmskb256 (v32qi)
  v16hi __builtin_ia32_pmovsxbw256 (v16qi)
  v8si __builtin_ia32_pmovsxbd256 (v16qi)
  v4di __builtin_ia32_pmovsxbq256 (v16qi)
  v8si __builtin_ia32_pmovsxwd256 (v8hi)
  v4di __builtin_ia32_pmovsxwq256 (v8hi)
  v4di __builtin_ia32_pmovsxdq256 (v4si)
  v16hi __builtin_ia32_pmovzxbw256 (v16qi)
  v8si __builtin_ia32_pmovzxbd256 (v16qi)
  v4di __builtin_ia32_pmovzxbq256 (v16qi)
  v8si __builtin_ia32_pmovzxwd256 (v8hi)
  v4di __builtin_ia32_pmovzxwq256 (v8hi)
  v4di __builtin_ia32_pmovzxdq256 (v4si)
  v4di __builtin_ia32_pmuldq256 (v8si,v8si)
  v16hi __builtin_ia32_pmulhrsw256 (v16hi, v16hi)
  v16hi __builtin_ia32_pmulhuw256 (v16hi,v16hi)
  v16hi __builtin_ia32_pmulhw256 (v16hi,v16hi)
  v16hi __builtin_ia32_pmullw256 (v16hi,v16hi)
  v8si __builtin_ia32_pmulld256 (v8si,v8si)
  v4di __builtin_ia32_pmuludq256 (v8si,v8si)
  v4di __builtin_ia32_por256 (v4di,v4di)
  v16hi __builtin_ia32_psadbw256 (v32qi,v32qi)
  v32qi __builtin_ia32_pshufb256 (v32qi,v32qi)
  v8si __builtin_ia32_pshufd256 (v8si,int)
  v16hi __builtin_ia32_pshufhw256 (v16hi,int)
  v16hi __builtin_ia32_pshuflw256 (v16hi,int)
  v32qi __builtin_ia32_psignb256 (v32qi,v32qi)
  v16hi __builtin_ia32_psignw256 (v16hi,v16hi)
  v8si __builtin_ia32_psignd256 (v8si,v8si)
  v4di __builtin_ia32_pslldqi256 (v4di,int)
  v16hi __builtin_ia32_psllwi256 (16hi,int)
  v16hi __builtin_ia32_psllw256(v16hi,v8hi)
  v8si __builtin_ia32_pslldi256 (v8si,int)
  v8si __builtin_ia32_pslld256(v8si,v4si)
  v4di __builtin_ia32_psllqi256 (v4di,int)
  v4di __builtin_ia32_psllq256(v4di,v2di)
  v16hi __builtin_ia32_psrawi256 (v16hi,int)
  v16hi __builtin_ia32_psraw256 (v16hi,v8hi)
  v8si __builtin_ia32_psradi256 (v8si,int)
  v8si __builtin_ia32_psrad256 (v8si,v4si)
  v4di __builtin_ia32_psrldqi256 (v4di, int)
  v16hi __builtin_ia32_psrlwi256 (v16hi,int)
  v16hi __builtin_ia32_psrlw256 (v16hi,v8hi)
  v8si __builtin_ia32_psrldi256 (v8si,int)
  v8si __builtin_ia32_psrld256 (v8si,v4si)
  v4di __builtin_ia32_psrlqi256 (v4di,int)
  v4di __builtin_ia32_psrlq256(v4di,v2di)
  v32qi __builtin_ia32_psubb256 (v32qi,v32qi)
  v32hi __builtin_ia32_psubw256 (v16hi,v16hi)
  v8si __builtin_ia32_psubd256 (v8si,v8si)
  v4di __builtin_ia32_psubq256 (v4di,v4di)
  v32qi __builtin_ia32_psubsb256 (v32qi,v32qi)
  v16hi __builtin_ia32_psubsw256 (v16hi,v16hi)
  v32qi __builtin_ia32_psubusb256 (v32qi,v32qi)
  v16hi __builtin_ia32_psubusw256 (v16hi,v16hi)
  v32qi __builtin_ia32_punpckhbw256 (v32qi,v32qi)
  v16hi __builtin_ia32_punpckhwd256 (v16hi,v16hi)
  v8si __builtin_ia32_punpckhdq256 (v8si,v8si)
  v4di __builtin_ia32_punpckhqdq256 (v4di,v4di)
  v32qi __builtin_ia32_punpcklbw256 (v32qi,v32qi)
  v16hi __builtin_ia32_punpcklwd256 (v16hi,v16hi)
  v8si __builtin_ia32_punpckldq256 (v8si,v8si)
  v4di __builtin_ia32_punpcklqdq256 (v4di,v4di)
  v4di __builtin_ia32_pxor256 (v4di,v4di)
  v4di __builtin_ia32_movntdqa256 (pv4di)
  v4sf __builtin_ia32_vbroadcastss_ps (v4sf)
  v8sf __builtin_ia32_vbroadcastss_ps256 (v4sf)
  v4df __builtin_ia32_vbroadcastsd_pd256 (v2df)
  v4di __builtin_ia32_vbroadcastsi256 (v2di)
  v4si __builtin_ia32_pblendd128 (v4si,v4si)
  v8si __builtin_ia32_pblendd256 (v8si,v8si)
  v32qi __builtin_ia32_pbroadcastb256 (v16qi)
  v16hi __builtin_ia32_pbroadcastw256 (v8hi)
  v8si __builtin_ia32_pbroadcastd256 (v4si)
  v4di __builtin_ia32_pbroadcastq256 (v2di)
  v16qi __builtin_ia32_pbroadcastb128 (v16qi)
  v8hi __builtin_ia32_pbroadcastw128 (v8hi)
  v4si __builtin_ia32_pbroadcastd128 (v4si)
  v2di __builtin_ia32_pbroadcastq128 (v2di)
  v8si __builtin_ia32_permvarsi256 (v8si,v8si)
  v4df __builtin_ia32_permdf256 (v4df,int)
  v8sf __builtin_ia32_permvarsf256 (v8sf,v8sf)
  v4di __builtin_ia32_permdi256 (v4di,int)
  v4di __builtin_ia32_permti256 (v4di,v4di,int)
  v4di __builtin_ia32_extract128i256 (v4di,int)
  v4di __builtin_ia32_insert128i256 (v4di,v2di,int)
  v8si __builtin_ia32_maskloadd256 (pcv8si,v8si)
  v4di __builtin_ia32_maskloadq256 (pcv4di,v4di)
  v4si __builtin_ia32_maskloadd (pcv4si,v4si)
  v2di __builtin_ia32_maskloadq (pcv2di,v2di)
  void __builtin_ia32_maskstored256 (pv8si,v8si,v8si)
  void __builtin_ia32_maskstoreq256 (pv4di,v4di,v4di)
  void __builtin_ia32_maskstored (pv4si,v4si,v4si)
  void __builtin_ia32_maskstoreq (pv2di,v2di,v2di)
  v8si __builtin_ia32_psllv8si (v8si,v8si)
  v4si __builtin_ia32_psllv4si (v4si,v4si)
  v4di __builtin_ia32_psllv4di (v4di,v4di)
  v2di __builtin_ia32_psllv2di (v2di,v2di)
  v8si __builtin_ia32_psrav8si (v8si,v8si)
  v4si __builtin_ia32_psrav4si (v4si,v4si)
  v8si __builtin_ia32_psrlv8si (v8si,v8si)
  v4si __builtin_ia32_psrlv4si (v4si,v4si)
  v4di __builtin_ia32_psrlv4di (v4di,v4di)
  v2di __builtin_ia32_psrlv2di (v2di,v2di)
  v2df __builtin_ia32_gathersiv2df (v2df, pcdouble,v4si,v2df,int)
  v4df __builtin_ia32_gathersiv4df (v4df, pcdouble,v4si,v4df,int)
  v2df __builtin_ia32_gatherdiv2df (v2df, pcdouble,v2di,v2df,int)
  v4df __builtin_ia32_gatherdiv4df (v4df, pcdouble,v4di,v4df,int)
  v4sf __builtin_ia32_gathersiv4sf (v4sf, pcfloat,v4si,v4sf,int)
  v8sf __builtin_ia32_gathersiv8sf (v8sf, pcfloat,v8si,v8sf,int)
  v4sf __builtin_ia32_gatherdiv4sf (v4sf, pcfloat,v2di,v4sf,int)
  v4sf __builtin_ia32_gatherdiv4sf256 (v4sf, pcfloat,v4di,v4sf,int)
  v2di __builtin_ia32_gathersiv2di (v2di, pcint64,v4si,v2di,int)
  v4di __builtin_ia32_gathersiv4di (v4di, pcint64,v4si,v4di,int)
  v2di __builtin_ia32_gatherdiv2di (v2di, pcint64,v2di,v2di,int)
  v4di __builtin_ia32_gatherdiv4di (v4di, pcint64,v4di,v4di,int)
  v4si __builtin_ia32_gathersiv4si (v4si, pcint,v4si,v4si,int)
  v8si __builtin_ia32_gathersiv8si (v8si, pcint,v8si,v8si,int)
  v4si __builtin_ia32_gatherdiv4si (v4si, pcint,v2di,v4si,int)
  v4si __builtin_ia32_gatherdiv4si256 (v4si, pcint,v4di,v4si,int)

The following built-in functions are available when :option:`-maes` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  v2di __builtin_ia32_aesenc128 (v2di, v2di)
  v2di __builtin_ia32_aesenclast128 (v2di, v2di)
  v2di __builtin_ia32_aesdec128 (v2di, v2di)
  v2di __builtin_ia32_aesdeclast128 (v2di, v2di)
  v2di __builtin_ia32_aeskeygenassist128 (v2di, const int)
  v2di __builtin_ia32_aesimc128 (v2di)

The following built-in function is available when :option:`-mpclmul` is
used.

``v2di __builtin_ia32_pclmulqdq128 (v2di, v2di, const int)``
  Generates the ``pclmulqdq`` machine instruction.

  The following built-in function is available when :option:`-mfsgsbase` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  unsigned int __builtin_ia32_rdfsbase32 (void)
  unsigned long long __builtin_ia32_rdfsbase64 (void)
  unsigned int __builtin_ia32_rdgsbase32 (void)
  unsigned long long __builtin_ia32_rdgsbase64 (void)
  void _writefsbase_u32 (unsigned int)
  void _writefsbase_u64 (unsigned long long)
  void _writegsbase_u32 (unsigned int)
  void _writegsbase_u64 (unsigned long long)

The following built-in function is available when :option:`-mrdrnd` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  unsigned int __builtin_ia32_rdrand16_step (unsigned short *)
  unsigned int __builtin_ia32_rdrand32_step (unsigned int *)
  unsigned int __builtin_ia32_rdrand64_step (unsigned long long *)

The following built-in function is available when :option:`-mptwrite` is
used.  All of them generate the machine instruction that is part of the
name.

.. code-block:: c++

  void __builtin_ia32_ptwrite32 (unsigned)
  void __builtin_ia32_ptwrite64 (unsigned long long)

The following built-in functions are available when :option:`-msse4a` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_movntsd (double *, v2df)
  void __builtin_ia32_movntss (float *, v4sf)
  v2di __builtin_ia32_extrq  (v2di, v16qi)
  v2di __builtin_ia32_extrqi (v2di, const unsigned int, const unsigned int)
  v2di __builtin_ia32_insertq (v2di, v2di)
  v2di __builtin_ia32_insertqi (v2di, v2di, const unsigned int, const unsigned int)

The following built-in functions are available when :option:`-mxop` is used.

.. code-block:: c++

  v2df __builtin_ia32_vfrczpd (v2df)
  v4sf __builtin_ia32_vfrczps (v4sf)
  v2df __builtin_ia32_vfrczsd (v2df)
  v4sf __builtin_ia32_vfrczss (v4sf)
  v4df __builtin_ia32_vfrczpd256 (v4df)
  v8sf __builtin_ia32_vfrczps256 (v8sf)
  v2di __builtin_ia32_vpcmov (v2di, v2di, v2di)
  v2di __builtin_ia32_vpcmov_v2di (v2di, v2di, v2di)
  v4si __builtin_ia32_vpcmov_v4si (v4si, v4si, v4si)
  v8hi __builtin_ia32_vpcmov_v8hi (v8hi, v8hi, v8hi)
  v16qi __builtin_ia32_vpcmov_v16qi (v16qi, v16qi, v16qi)
  v2df __builtin_ia32_vpcmov_v2df (v2df, v2df, v2df)
  v4sf __builtin_ia32_vpcmov_v4sf (v4sf, v4sf, v4sf)
  v4di __builtin_ia32_vpcmov_v4di256 (v4di, v4di, v4di)
  v8si __builtin_ia32_vpcmov_v8si256 (v8si, v8si, v8si)
  v16hi __builtin_ia32_vpcmov_v16hi256 (v16hi, v16hi, v16hi)
  v32qi __builtin_ia32_vpcmov_v32qi256 (v32qi, v32qi, v32qi)
  v4df __builtin_ia32_vpcmov_v4df256 (v4df, v4df, v4df)
  v8sf __builtin_ia32_vpcmov_v8sf256 (v8sf, v8sf, v8sf)
  v16qi __builtin_ia32_vpcomeqb (v16qi, v16qi)
  v8hi __builtin_ia32_vpcomeqw (v8hi, v8hi)
  v4si __builtin_ia32_vpcomeqd (v4si, v4si)
  v2di __builtin_ia32_vpcomeqq (v2di, v2di)
  v16qi __builtin_ia32_vpcomequb (v16qi, v16qi)
  v4si __builtin_ia32_vpcomequd (v4si, v4si)
  v2di __builtin_ia32_vpcomequq (v2di, v2di)
  v8hi __builtin_ia32_vpcomequw (v8hi, v8hi)
  v8hi __builtin_ia32_vpcomeqw (v8hi, v8hi)
  v16qi __builtin_ia32_vpcomfalseb (v16qi, v16qi)
  v4si __builtin_ia32_vpcomfalsed (v4si, v4si)
  v2di __builtin_ia32_vpcomfalseq (v2di, v2di)
  v16qi __builtin_ia32_vpcomfalseub (v16qi, v16qi)
  v4si __builtin_ia32_vpcomfalseud (v4si, v4si)
  v2di __builtin_ia32_vpcomfalseuq (v2di, v2di)
  v8hi __builtin_ia32_vpcomfalseuw (v8hi, v8hi)
  v8hi __builtin_ia32_vpcomfalsew (v8hi, v8hi)
  v16qi __builtin_ia32_vpcomgeb (v16qi, v16qi)
  v4si __builtin_ia32_vpcomged (v4si, v4si)
  v2di __builtin_ia32_vpcomgeq (v2di, v2di)
  v16qi __builtin_ia32_vpcomgeub (v16qi, v16qi)
  v4si __builtin_ia32_vpcomgeud (v4si, v4si)
  v2di __builtin_ia32_vpcomgeuq (v2di, v2di)
  v8hi __builtin_ia32_vpcomgeuw (v8hi, v8hi)
  v8hi __builtin_ia32_vpcomgew (v8hi, v8hi)
  v16qi __builtin_ia32_vpcomgtb (v16qi, v16qi)
  v4si __builtin_ia32_vpcomgtd (v4si, v4si)
  v2di __builtin_ia32_vpcomgtq (v2di, v2di)
  v16qi __builtin_ia32_vpcomgtub (v16qi, v16qi)
  v4si __builtin_ia32_vpcomgtud (v4si, v4si)
  v2di __builtin_ia32_vpcomgtuq (v2di, v2di)
  v8hi __builtin_ia32_vpcomgtuw (v8hi, v8hi)
  v8hi __builtin_ia32_vpcomgtw (v8hi, v8hi)
  v16qi __builtin_ia32_vpcomleb (v16qi, v16qi)
  v4si __builtin_ia32_vpcomled (v4si, v4si)
  v2di __builtin_ia32_vpcomleq (v2di, v2di)
  v16qi __builtin_ia32_vpcomleub (v16qi, v16qi)
  v4si __builtin_ia32_vpcomleud (v4si, v4si)
  v2di __builtin_ia32_vpcomleuq (v2di, v2di)
  v8hi __builtin_ia32_vpcomleuw (v8hi, v8hi)
  v8hi __builtin_ia32_vpcomlew (v8hi, v8hi)
  v16qi __builtin_ia32_vpcomltb (v16qi, v16qi)
  v4si __builtin_ia32_vpcomltd (v4si, v4si)
  v2di __builtin_ia32_vpcomltq (v2di, v2di)
  v16qi __builtin_ia32_vpcomltub (v16qi, v16qi)
  v4si __builtin_ia32_vpcomltud (v4si, v4si)
  v2di __builtin_ia32_vpcomltuq (v2di, v2di)
  v8hi __builtin_ia32_vpcomltuw (v8hi, v8hi)
  v8hi __builtin_ia32_vpcomltw (v8hi, v8hi)
  v16qi __builtin_ia32_vpcomneb (v16qi, v16qi)
  v4si __builtin_ia32_vpcomned (v4si, v4si)
  v2di __builtin_ia32_vpcomneq (v2di, v2di)
  v16qi __builtin_ia32_vpcomneub (v16qi, v16qi)
  v4si __builtin_ia32_vpcomneud (v4si, v4si)
  v2di __builtin_ia32_vpcomneuq (v2di, v2di)
  v8hi __builtin_ia32_vpcomneuw (v8hi, v8hi)
  v8hi __builtin_ia32_vpcomnew (v8hi, v8hi)
  v16qi __builtin_ia32_vpcomtrueb (v16qi, v16qi)
  v4si __builtin_ia32_vpcomtrued (v4si, v4si)
  v2di __builtin_ia32_vpcomtrueq (v2di, v2di)
  v16qi __builtin_ia32_vpcomtrueub (v16qi, v16qi)
  v4si __builtin_ia32_vpcomtrueud (v4si, v4si)
  v2di __builtin_ia32_vpcomtrueuq (v2di, v2di)
  v8hi __builtin_ia32_vpcomtrueuw (v8hi, v8hi)
  v8hi __builtin_ia32_vpcomtruew (v8hi, v8hi)
  v4si __builtin_ia32_vphaddbd (v16qi)
  v2di __builtin_ia32_vphaddbq (v16qi)
  v8hi __builtin_ia32_vphaddbw (v16qi)
  v2di __builtin_ia32_vphadddq (v4si)
  v4si __builtin_ia32_vphaddubd (v16qi)
  v2di __builtin_ia32_vphaddubq (v16qi)
  v8hi __builtin_ia32_vphaddubw (v16qi)
  v2di __builtin_ia32_vphaddudq (v4si)
  v4si __builtin_ia32_vphadduwd (v8hi)
  v2di __builtin_ia32_vphadduwq (v8hi)
  v4si __builtin_ia32_vphaddwd (v8hi)
  v2di __builtin_ia32_vphaddwq (v8hi)
  v8hi __builtin_ia32_vphsubbw (v16qi)
  v2di __builtin_ia32_vphsubdq (v4si)
  v4si __builtin_ia32_vphsubwd (v8hi)
  v4si __builtin_ia32_vpmacsdd (v4si, v4si, v4si)
  v2di __builtin_ia32_vpmacsdqh (v4si, v4si, v2di)
  v2di __builtin_ia32_vpmacsdql (v4si, v4si, v2di)
  v4si __builtin_ia32_vpmacssdd (v4si, v4si, v4si)
  v2di __builtin_ia32_vpmacssdqh (v4si, v4si, v2di)
  v2di __builtin_ia32_vpmacssdql (v4si, v4si, v2di)
  v4si __builtin_ia32_vpmacsswd (v8hi, v8hi, v4si)
  v8hi __builtin_ia32_vpmacssww (v8hi, v8hi, v8hi)
  v4si __builtin_ia32_vpmacswd (v8hi, v8hi, v4si)
  v8hi __builtin_ia32_vpmacsww (v8hi, v8hi, v8hi)
  v4si __builtin_ia32_vpmadcsswd (v8hi, v8hi, v4si)
  v4si __builtin_ia32_vpmadcswd (v8hi, v8hi, v4si)
  v16qi __builtin_ia32_vpperm (v16qi, v16qi, v16qi)
  v16qi __builtin_ia32_vprotb (v16qi, v16qi)
  v4si __builtin_ia32_vprotd (v4si, v4si)
  v2di __builtin_ia32_vprotq (v2di, v2di)
  v8hi __builtin_ia32_vprotw (v8hi, v8hi)
  v16qi __builtin_ia32_vpshab (v16qi, v16qi)
  v4si __builtin_ia32_vpshad (v4si, v4si)
  v2di __builtin_ia32_vpshaq (v2di, v2di)
  v8hi __builtin_ia32_vpshaw (v8hi, v8hi)
  v16qi __builtin_ia32_vpshlb (v16qi, v16qi)
  v4si __builtin_ia32_vpshld (v4si, v4si)
  v2di __builtin_ia32_vpshlq (v2di, v2di)
  v8hi __builtin_ia32_vpshlw (v8hi, v8hi)

The following built-in functions are available when :option:`-mfma4` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v2df __builtin_ia32_vfmaddpd (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfmaddps (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_vfmaddsd (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfmaddss (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_vfmsubpd (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfmsubps (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_vfmsubsd (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfmsubss (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_vfnmaddpd (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfnmaddps (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_vfnmaddsd (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfnmaddss (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_vfnmsubpd (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfnmsubps (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_vfnmsubsd (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfnmsubss (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_vfmaddsubpd  (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfmaddsubps  (v4sf, v4sf, v4sf)
  v2df __builtin_ia32_vfmsubaddpd  (v2df, v2df, v2df)
  v4sf __builtin_ia32_vfmsubaddps  (v4sf, v4sf, v4sf)
  v4df __builtin_ia32_vfmaddpd256 (v4df, v4df, v4df)
  v8sf __builtin_ia32_vfmaddps256 (v8sf, v8sf, v8sf)
  v4df __builtin_ia32_vfmsubpd256 (v4df, v4df, v4df)
  v8sf __builtin_ia32_vfmsubps256 (v8sf, v8sf, v8sf)
  v4df __builtin_ia32_vfnmaddpd256 (v4df, v4df, v4df)
  v8sf __builtin_ia32_vfnmaddps256 (v8sf, v8sf, v8sf)
  v4df __builtin_ia32_vfnmsubpd256 (v4df, v4df, v4df)
  v8sf __builtin_ia32_vfnmsubps256 (v8sf, v8sf, v8sf)
  v4df __builtin_ia32_vfmaddsubpd256 (v4df, v4df, v4df)
  v8sf __builtin_ia32_vfmaddsubps256 (v8sf, v8sf, v8sf)
  v4df __builtin_ia32_vfmsubaddpd256 (v4df, v4df, v4df)
  v8sf __builtin_ia32_vfmsubaddps256 (v8sf, v8sf, v8sf)

The following built-in functions are available when :option:`-mlwp` is used.

.. code-block:: c++

  void __builtin_ia32_llwpcb16 (void *);
  void __builtin_ia32_llwpcb32 (void *);
  void __builtin_ia32_llwpcb64 (void *);
  void * __builtin_ia32_llwpcb16 (void);
  void * __builtin_ia32_llwpcb32 (void);
  void * __builtin_ia32_llwpcb64 (void);
  void __builtin_ia32_lwpval16 (unsigned short, unsigned int, unsigned short)
  void __builtin_ia32_lwpval32 (unsigned int, unsigned int, unsigned int)
  void __builtin_ia32_lwpval64 (unsigned __int64, unsigned int, unsigned int)
  unsigned char __builtin_ia32_lwpins16 (unsigned short, unsigned int, unsigned short)
  unsigned char __builtin_ia32_lwpins32 (unsigned int, unsigned int, unsigned int)
  unsigned char __builtin_ia32_lwpins64 (unsigned __int64, unsigned int, unsigned int)

The following built-in functions are available when :option:`-mbmi` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  unsigned int __builtin_ia32_bextr_u32(unsigned int, unsigned int);
  unsigned long long __builtin_ia32_bextr_u64 (unsigned long long, unsigned long long);

The following built-in functions are available when :option:`-mbmi2` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  unsigned int _bzhi_u32 (unsigned int, unsigned int)
  unsigned int _pdep_u32 (unsigned int, unsigned int)
  unsigned int _pext_u32 (unsigned int, unsigned int)
  unsigned long long _bzhi_u64 (unsigned long long, unsigned long long)
  unsigned long long _pdep_u64 (unsigned long long, unsigned long long)
  unsigned long long _pext_u64 (unsigned long long, unsigned long long)

The following built-in functions are available when :option:`-mlzcnt` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  unsigned short __builtin_ia32_lzcnt_u16(unsigned short);
  unsigned int __builtin_ia32_lzcnt_u32(unsigned int);
  unsigned long long __builtin_ia32_lzcnt_u64 (unsigned long long);

The following built-in functions are available when :option:`-mfxsr` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_fxsave (void *)
  void __builtin_ia32_fxrstor (void *)
  void __builtin_ia32_fxsave64 (void *)
  void __builtin_ia32_fxrstor64 (void *)

The following built-in functions are available when :option:`-mxsave` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_xsave (void *, long long)
  void __builtin_ia32_xrstor (void *, long long)
  void __builtin_ia32_xsave64 (void *, long long)
  void __builtin_ia32_xrstor64 (void *, long long)

The following built-in functions are available when :option:`-mxsaveopt` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_xsaveopt (void *, long long)
  void __builtin_ia32_xsaveopt64 (void *, long long)

The following built-in functions are available when :option:`-mtbm` is used.
Both of them generate the immediate form of the bextr machine instruction.

.. code-block:: c++

  unsigned int __builtin_ia32_bextri_u32 (unsigned int,
                                          const unsigned int);
  unsigned long long __builtin_ia32_bextri_u64 (unsigned long long,
                                                const unsigned long long);

The following built-in functions are available when :option:`-m3dnow` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_femms (void)
  v8qi __builtin_ia32_pavgusb (v8qi, v8qi)
  v2si __builtin_ia32_pf2id (v2sf)
  v2sf __builtin_ia32_pfacc (v2sf, v2sf)
  v2sf __builtin_ia32_pfadd (v2sf, v2sf)
  v2si __builtin_ia32_pfcmpeq (v2sf, v2sf)
  v2si __builtin_ia32_pfcmpge (v2sf, v2sf)
  v2si __builtin_ia32_pfcmpgt (v2sf, v2sf)
  v2sf __builtin_ia32_pfmax (v2sf, v2sf)
  v2sf __builtin_ia32_pfmin (v2sf, v2sf)
  v2sf __builtin_ia32_pfmul (v2sf, v2sf)
  v2sf __builtin_ia32_pfrcp (v2sf)
  v2sf __builtin_ia32_pfrcpit1 (v2sf, v2sf)
  v2sf __builtin_ia32_pfrcpit2 (v2sf, v2sf)
  v2sf __builtin_ia32_pfrsqrt (v2sf)
  v2sf __builtin_ia32_pfsub (v2sf, v2sf)
  v2sf __builtin_ia32_pfsubr (v2sf, v2sf)
  v2sf __builtin_ia32_pi2fd (v2si)
  v4hi __builtin_ia32_pmulhrw (v4hi, v4hi)

The following built-in functions are available when :option:`-m3dnowa` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  v2si __builtin_ia32_pf2iw (v2sf)
  v2sf __builtin_ia32_pfnacc (v2sf, v2sf)
  v2sf __builtin_ia32_pfpnacc (v2sf, v2sf)
  v2sf __builtin_ia32_pi2fw (v2si)
  v2sf __builtin_ia32_pswapdsf (v2sf)
  v2si __builtin_ia32_pswapdsi (v2si)

The following built-in functions are available when :option:`-mrtm` is used
They are used for restricted transactional memory. These are the internal
low level functions. Normally the functions in 
x86 transactional memory intrinsics should be used instead.

.. code-block:: c++

  int __builtin_ia32_xbegin ()
  void __builtin_ia32_xend ()
  void __builtin_ia32_xabort (status)
  int __builtin_ia32_xtest ()

The following built-in functions are available when :option:`-mmwaitx` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_ia32_monitorx (void *, unsigned int, unsigned int)
  void __builtin_ia32_mwaitx (unsigned int, unsigned int, unsigned int)

The following built-in functions are available when :option:`-mclzero` is used.
All of them generate the machine instruction that is part of the name.

.. code-block:: c++

  void __builtin_i32_clzero (void *)

The following built-in functions are available when :option:`-mpku` is used.
They generate reads and writes to PKRU.

.. code-block:: c++

  void __builtin_ia32_wrpkru (unsigned int)
  unsigned int __builtin_ia32_rdpkru ()

The following built-in functions are available when
:option:`-mshstk` option is used.  They support shadow stack
machine instructions from Intel Control-flow Enforcement Technology (CET).
Each built-in function generates the  machine instruction that is part
of the function's name.  These are the internal low-level functions.
Normally the functions in x86 control-flow protection intrinsics
should be used instead.

.. code-block:: c++

  unsigned int __builtin_ia32_rdsspd (void)
  unsigned long long __builtin_ia32_rdsspq (void)
  void __builtin_ia32_incsspd (unsigned int)
  void __builtin_ia32_incsspq (unsigned long long)
  void __builtin_ia32_saveprevssp(void);
  void __builtin_ia32_rstorssp(void *);
  void __builtin_ia32_wrssd(unsigned int, void *);
  void __builtin_ia32_wrssq(unsigned long long, void *);
  void __builtin_ia32_wrussd(unsigned int, void *);
  void __builtin_ia32_wrussq(unsigned long long, void *);
  void __builtin_ia32_setssbsy(void);
  void __builtin_ia32_clrssbsy(void *);

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

.. function:: unsigned _xbegin()

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

.. function:: void _xend()

  Commit the current transaction. When no transaction is active this faults.
  All memory side effects of the transaction become visible
  to other threads in an atomic manner.

.. function:: int _xtest()

  Return a nonzero value if a transaction is currently active, otherwise 0.

.. function:: void _xabort(status )

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

.. function:: ret_type _get_ssp(void )

  Get the current value of shadow stack pointer if shadow stack support
  from Intel CET is enabled in the hardware or ``0`` otherwise.
  The ``ret_type`` is ``unsigned long long`` for 64-bit targets 
  and ``unsigned int`` for 32-bit targets.

.. function:: void _inc_ssp(unsigned int)

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

