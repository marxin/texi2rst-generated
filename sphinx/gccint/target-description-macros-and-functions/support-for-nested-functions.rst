..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _trampolines:

Support for Nested Functions
****************************

.. index:: support for nested functions

.. index:: trampolines for nested functions

.. index:: descriptors for nested functions

.. index:: nested functions, support for

Taking the address of a nested function requires special compiler
handling to ensure that the static chain register is loaded when
the function is invoked via an indirect call.

GCC has traditionally supported nested functions by creating an
executable :dfn:`trampoline` at run time when the address of a nested
function is taken.  This is a small piece of code which normally
resides on the stack, in the stack frame of the containing function.
The trampoline loads the static chain register and then jumps to the
real address of the nested function.

The use of trampolines requires an executable stack, which is a
security risk.  To avoid this problem, GCC also supports another
strategy: using descriptors for nested functions.  Under this model,
taking the address of a nested function results in a pointer to a
non-executable function descriptor object.  Initializing the static chain
from the descriptor is handled at indirect call sites.

On some targets, including HPPA and IA-64, function descriptors may be
mandated by the ABI or be otherwise handled in a target-specific way
by the back end in its code generation strategy for indirect calls.
GCC also provides its own generic descriptor implementation to support the
:option:`-fno-trampolines` option.  In this case runtime detection of
function descriptors at indirect call sites relies on descriptor
pointers being tagged with a bit that is never set in bare function
addresses.  Since GCC's generic function descriptors are
not ABI-compliant, this option is typically used only on a
per-language basis (notably by Ada) or when it can otherwise be
applied to the whole program.

For languages other than Ada, the ``-ftrampolines`` and
``-fno-trampolines`` options currently have no effect, and
trampolines are always generated on platforms that need them
for nested functions.

Define the following hook if your backend either implements ABI-specified
descriptor support, or can use GCC's generic descriptor implementation
for nested functions.

.. c:var:: int TARGET_CUSTOM_FUNCTION_DESCRIPTORS

  If the target can use GCC's generic descriptor mechanism for nested
  functions, define this hook to a power of 2 representing an unused bit
  in function pointers which can be used to differentiate descriptors at
  run time.  This value gives the number of bytes by which descriptor
  pointers are misaligned compared to function pointers.  For example, on
  targets that require functions to be aligned to a 4-byte boundary, a
  value of either 1 or 2 is appropriate unless the architecture already
  reserves the bit for another purpose, such as on ARM.

  Define this hook to 0 if the target implements ABI support for
  function descriptors in its standard calling sequence, like for example
  HPPA or IA-64.

  Using descriptors for nested functions
  eliminates the need for trampolines that reside on the stack and require
  it to be made executable.

The following macros tell GCC how to generate code to allocate and
initialize an executable trampoline.  You can also use this interface
if your back end needs to create ABI-specified non-executable descriptors; in
this case the "trampoline" created is the descriptor containing data only.

The instructions in an executable trampoline must do two things: load
a constant address into the static chain register, and jump to the real
address of the nested function.  On CISC machines such as the m68k,
this requires two instructions, a move immediate and a jump.  Then the
two addresses exist in the trampoline as word-long immediate operands.
On RISC machines, it is often necessary to load each address into a
register in two parts.  Then pieces of each address form separate
immediate operands.

The code generated to initialize the trampoline must store the variable
parts---the static chain value and the function address---into the
immediate operands of the instructions.  On a CISC machine, this is
simply a matter of copying each address to a memory reference at the
proper offset from the start of the trampoline.  On a RISC machine, it
may be necessary to take out pieces of the address and store them
separately.

.. function:: void TARGET_ASM_TRAMPOLINE_TEMPLATE (FILE *f)

  This hook is called by ``assemble_trampoline_template`` to output,
  on the stream :samp:`{f}`, assembler code for a block of data that contains
  the constant parts of a trampoline.  This code should not include a
  label---the label is taken care of automatically.

  If you do not define this hook, it means no template is needed
  for the target.  Do not define this hook on systems where the block move
  code to copy the trampoline into place would be larger than the code
  to generate it on the spot.

.. c:macro:: TRAMPOLINE_SECTION

  Return the section into which the trampoline template is to be placed
  (see :ref:`sections`).  The default value is ``readonly_data_section``.

.. c:macro:: TRAMPOLINE_SIZE

  A C expression for the size in bytes of the trampoline, as an integer.

.. c:macro:: TRAMPOLINE_ALIGNMENT

  Alignment required for trampolines, in bits.

  If you don't define this macro, the value of ``FUNCTION_ALIGNMENT``
  is used for aligning trampolines.

.. function:: void TARGET_TRAMPOLINE_INIT (rtx m_tramp, tree fndecl, rtx static_chain)

  This hook is called to initialize a trampoline.
  :samp:`{m_tramp}` is an RTX for the memory block for the trampoline; :samp:`{fndecl}`
  is the ``FUNCTION_DECL`` for the nested function; :samp:`{static_chain}` is an
  RTX for the static chain value that should be passed to the function
  when it is called.

  If the target defines ``TARGET_ASM_TRAMPOLINE_TEMPLATE``, then the
  first thing this hook should do is emit a block move into :samp:`{m_tramp}`
  from the memory block returned by ``assemble_trampoline_template``.
  Note that the block move need only cover the constant parts of the
  trampoline.  If the target isolates the variable parts of the trampoline
  to the end, not all ``TRAMPOLINE_SIZE`` bytes need be copied.

  If the target requires any other actions, such as flushing caches
  (possibly calling function maybe_emit_call_builtin___clear_cache) or
  enabling stack execution, these actions should be performed after
  initializing the trampoline proper.

.. function:: void TARGET_EMIT_CALL_BUILTIN___CLEAR_CACHE (rtx begin, rtx end)

  On targets that do not define a ``clear_cache`` insn expander,
  but that define the ``CLEAR_CACHE_INSN`` macro,
  maybe_emit_call_builtin___clear_cache relies on this target hook
  to clear an address range in the instruction cache.

  The default implementation calls the ``__clear_cache`` builtin,
  taking the assembler name from the builtin declaration.  Overriding
  definitions may call alternate functions, with alternate calling
  conventions, or emit alternate RTX to perform the job.

.. function:: rtx TARGET_TRAMPOLINE_ADJUST_ADDRESS (rtx addr)

  This hook should perform any machine-specific adjustment in
  the address of the trampoline.  Its argument contains the address of the
  memory block that was passed to ``TARGET_TRAMPOLINE_INIT``.  In case
  the address to be used for a function call should be different from the
  address at which the template was stored, the different address should
  be returned; otherwise :samp:`{addr}` should be returned unchanged.
  If this hook is not defined, :samp:`{addr}` will be used for function calls.

Implementing trampolines is difficult on many machines because they have
separate instruction and data caches.  Writing into a stack location
fails to clear the memory in the instruction cache, so when the program
jumps to that location, it executes the old contents.

Here are two possible solutions.  One is to clear the relevant parts of
the instruction cache whenever a trampoline is set up.  The other is to
make all trampolines identical, by having them jump to a standard
subroutine.  The former technique makes trampoline execution faster; the
latter makes initialization faster.

To clear the instruction cache when a trampoline is initialized, define
the following macro.

.. c:macro:: CLEAR_INSN_CACHE (beg, end)

  If defined, expands to a C expression clearing the *instruction
  cache* in the specified interval.  The definition of this macro would
  typically be a series of ``asm`` statements.  Both :samp:`{beg}` and
  :samp:`{end}` are pointer expressions.

To use a standard subroutine, define the following macro.  In addition,
you must make sure that the instructions in a trampoline fill an entire
cache line with identical instructions, or else ensure that the
beginning of the trampoline code is always aligned at the same point in
its cache line.  Look in m68k.h as a guide.

.. c:macro:: TRANSFER_FROM_TRAMPOLINE

  Define this macro if trampolines need a special subroutine to do their
  work.  The macro should expand to a series of ``asm`` statements
  which will be compiled with GCC.  They go in a library function named
  ``__transfer_from_trampoline``.

  If you need to avoid executing the ordinary prologue code of a compiled
  C function when you jump to the subroutine, you can do so by placing a
  special label of your own in the assembler code.  Use one ``asm``
  statement to generate an assembler label, and another to make the label
  global.  Then trampolines can use that label to jump directly to your
  special assembler code.

