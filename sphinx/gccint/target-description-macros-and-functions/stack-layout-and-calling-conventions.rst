.. _stack-and-calling:

Stack Layout and Calling Conventions
************************************

.. index:: calling conventions

.. prevent bad page break with this line

This describes the stack layout and calling conventions.

.. toctree::
  :maxdepth: 2

  stack-layout-and-calling-conventions/frame-layout
  stack-layout-and-calling-conventions/stack-checking
  stack-layout-and-calling-conventions/frame-registers
  stack-layout-and-calling-conventions/elimination
  stack-layout-and-calling-conventions/stack-arguments
  stack-layout-and-calling-conventions/register-arguments
  stack-layout-and-calling-conventions/scalar-return
  stack-layout-and-calling-conventions/aggregate-return
  stack-layout-and-calling-conventions/caller-saves
  stack-layout-and-calling-conventions/function-entry
  stack-layout-and-calling-conventions/profiling
  stack-layout-and-calling-conventions/tail-calls
  stack-layout-and-calling-conventions/shrink-wrapping-separate-components
  stack-layout-and-calling-conventions/stack-smashing-protection
  stack-layout-and-calling-conventions/miscellaneous-register-hooks

.. _frame-layout:

Basic Stack Layout
^^^^^^^^^^^^^^^^^^

.. index:: stack frame layout

.. index:: frame layout

.. prevent bad page break with this line

Here is the basic stack layout.

.. c:macro:: STACK_GROWS_DOWNWARD

  Define this macro to be true if pushing a word onto the stack moves the stack
  pointer to a smaller address, and false otherwise.

.. c:macro:: STACK_PUSH_CODE

  This macro defines the operation used when something is pushed
  on the stack.  In RTL, a push operation will be
  ``(set (mem (STACK_PUSH_CODE (reg sp))) ...)``

  The choices are ``PRE_DEC``, ``POST_DEC``, ``PRE_INC``,
  and ``POST_INC``.  Which of these is correct depends on
  the stack direction and on whether the stack pointer points
  to the last item on the stack or whether it points to the
  space for the next item on the stack.

  The default is ``PRE_DEC`` when ``STACK_GROWS_DOWNWARD`` is
  true, which is almost always right, and ``PRE_INC`` otherwise,
  which is often wrong.

.. c:macro:: FRAME_GROWS_DOWNWARD

  Define this macro to nonzero value if the addresses of local variable slots
  are at negative offsets from the frame pointer.

.. c:macro:: ARGS_GROW_DOWNWARD

  Define this macro if successive arguments to a function occupy decreasing
  addresses on the stack.

.. function:: HOST_WIDE_INT TARGET_STARTING_FRAME_OFFSET (void)

  This hook returns the offset from the frame pointer to the first local
  variable slot to be allocated.  If ``FRAME_GROWS_DOWNWARD``, it is the
  offset to *end* of the first slot allocated, otherwise it is the
  offset to *beginning* of the first slot allocated.  The default
  implementation returns 0.

.. c:macro:: STACK_ALIGNMENT_NEEDED

  Define to zero to disable final alignment of the stack during reload.
  The nonzero default for this macro is suitable for most ports.

  On ports where ``TARGET_STARTING_FRAME_OFFSET`` is nonzero or where there
  is a register save block following the local block that doesn't require
  alignment to ``STACK_BOUNDARY``, it may be beneficial to disable
  stack alignment and do it in the backend.

.. c:macro:: STACK_POINTER_OFFSET

  Offset from the stack pointer register to the first location at which
  outgoing arguments are placed.  If not specified, the default value of
  zero is used.  This is the proper value for most machines.

  If ``ARGS_GROW_DOWNWARD``, this is the offset to the location above
  the first location at which outgoing arguments are placed.

.. c:macro:: FIRST_PARM_OFFSET (fundecl)

  Offset from the argument pointer register to the first argument's
  address.  On some machines it may depend on the data type of the
  function.

  If ``ARGS_GROW_DOWNWARD``, this is the offset to the location above
  the first argument's address.

.. c:macro:: STACK_DYNAMIC_OFFSET (fundecl)

  Offset from the stack pointer register to an item dynamically allocated
  on the stack, e.g., by ``alloca``.

  The default value for this macro is ``STACK_POINTER_OFFSET`` plus the
  length of the outgoing arguments.  The default is correct for most
  machines.  See function.c for details.

.. c:macro:: INITIAL_FRAME_ADDRESS_RTX

  A C expression whose value is RTL representing the address of the initial
  stack frame. This address is passed to ``RETURN_ADDR_RTX`` and
  ``DYNAMIC_CHAIN_ADDRESS``.  If you don't define this macro, a reasonable
  default value will be used.  Define this macro in order to make frame pointer
  elimination work in the presence of ``__builtin_frame_address (count)`` and
  ``__builtin_return_address (count)`` for ``count`` not equal to zero.

.. c:macro:: DYNAMIC_CHAIN_ADDRESS (frameaddr)

  A C expression whose value is RTL representing the address in a stack
  frame where the pointer to the caller's frame is stored.  Assume that
  :samp:`{frameaddr}` is an RTL expression for the address of the stack frame
  itself.

  If you don't define this macro, the default is to return the value
  of :samp:`{frameaddr}` ---that is, the stack frame address is also the
  address of the stack word that points to the previous frame.

.. c:macro:: SETUP_FRAME_ADDRESSES

  A C expression that produces the machine-specific code to
  setup the stack so that arbitrary frames can be accessed.  For example,
  on the SPARC, we must flush all of the register windows to the stack
  before we can access arbitrary stack frames.  You will seldom need to
  define this macro.  The default is to do nothing.

.. function:: rtx TARGET_BUILTIN_SETJMP_FRAME_VALUE (void)

  This target hook should return an rtx that is used to store
  the address of the current frame into the built in ``setjmp`` buffer.
  The default value, ``virtual_stack_vars_rtx``, is correct for most
  machines.  One reason you may need to define this target hook is if
  ``hard_frame_pointer_rtx`` is the appropriate value on your machine.

.. c:macro:: FRAME_ADDR_RTX (frameaddr)

  A C expression whose value is RTL representing the value of the frame
  address for the current frame.  :samp:`{frameaddr}` is the frame pointer
  of the current frame.  This is used for __builtin_frame_address.
  You need only define this macro if the frame address is not the same
  as the frame pointer.  Most machines do not need to define it.

.. c:macro:: RETURN_ADDR_RTX (count, frameaddr)

  A C expression whose value is RTL representing the value of the return
  address for the frame :samp:`{count}` steps up from the current frame, after
  the prologue.  :samp:`{frameaddr}` is the frame pointer of the :samp:`{count}`
  frame, or the frame pointer of the :samp:`{count}` - 1 frame if
  ``RETURN_ADDR_IN_PREVIOUS_FRAME`` is nonzero.

  The value of the expression must always be the correct address when
  :samp:`{count}` is zero, but may be ``NULL_RTX`` if there is no way to
  determine the return address of other frames.

.. c:macro:: RETURN_ADDR_IN_PREVIOUS_FRAME

  Define this macro to nonzero value if the return address of a particular
  stack frame is accessed from the frame pointer of the previous stack
  frame.  The zero default for this macro is suitable for most ports.

.. c:macro:: INCOMING_RETURN_ADDR_RTX

  A C expression whose value is RTL representing the location of the
  incoming return address at the beginning of any function, before the
  prologue.  This RTL is either a ``REG``, indicating that the return
  value is saved in :samp:`REG`, or a ``MEM`` representing a location in
  the stack.

  You only need to define this macro if you want to support call frame
  debugging information like that provided by DWARF 2.

  If this RTL is a ``REG``, you should also define
  ``DWARF_FRAME_RETURN_COLUMN`` to ``DWARF_FRAME_REGNUM (REGNO)``.

.. c:macro:: DWARF_ALT_FRAME_RETURN_COLUMN

  A C expression whose value is an integer giving a DWARF 2 column
  number that may be used as an alternative return column.  The column
  must not correspond to any gcc hard register (that is, it must not
  be in the range of ``DWARF_FRAME_REGNUM`` ).

  This macro can be useful if ``DWARF_FRAME_RETURN_COLUMN`` is set to a
  general register, but an alternative column needs to be used for signal
  frames.  Some targets have also used different frame return columns
  over time.

.. c:macro:: DWARF_ZERO_REG

  A C expression whose value is an integer giving a DWARF 2 register
  number that is considered to always have the value zero.  This should
  only be defined if the target has an architected zero register, and
  someone decided it was a good idea to use that register number to
  terminate the stack backtrace.  New ports should avoid this.

.. function:: void TARGET_DWARF_HANDLE_FRAME_UNSPEC (const char *label, rtx pattern, int index)

  This target hook allows the backend to emit frame-related insns that
  contain UNSPECs or UNSPEC_VOLATILEs.  The DWARF 2 call frame debugging
  info engine will invoke it on insns of the form

  .. code-block:: c++

    (set (reg) (unspec [...] UNSPEC_INDEX))

  and

  .. code-block:: c++

    (set (reg) (unspec_volatile [...] UNSPECV_INDEX)).

  to let the backend emit the call frame instructions.  :samp:`{label}` is
  the CFI label attached to the insn, :samp:`{pattern}` is the pattern of
  the insn and :samp:`{index}` is ``UNSPEC_INDEX`` or ``UNSPECV_INDEX``.

.. function:: unsigned int TARGET_DWARF_POLY_INDETERMINATE_VALUE (unsigned int i, unsigned int *factor, int *offset)

  Express the value of ``poly_int`` indeterminate :samp:`{i}` as a DWARF
  expression, with :samp:`{i}` counting from 1.  Return the number of a DWARF
  register :samp:`{R}` and set :samp:`*{factor}` and :samp:`*{offset}` such
  that the value of the indeterminate is:

  .. code-block:: c++

    value_of(R) / factor - offset

  A target only needs to define this hook if it sets
  :samp:`NUM_POLY_INT_COEFFS` to a value greater than 1.

.. c:macro:: INCOMING_FRAME_SP_OFFSET

  A C expression whose value is an integer giving the offset, in bytes,
  from the value of the stack pointer register to the top of the stack
  frame at the beginning of any function, before the prologue.  The top of
  the frame is defined to be the value of the stack pointer in the
  previous frame, just before the call instruction.

  You only need to define this macro if you want to support call frame
  debugging information like that provided by DWARF 2.

.. c:macro:: DEFAULT_INCOMING_FRAME_SP_OFFSET

  Like ``INCOMING_FRAME_SP_OFFSET``, but must be the same for all
  functions of the same ABI, and when using GAS ``.cfi_*`` directives
  must also agree with the default CFI GAS emits.  Define this macro
  only if ``INCOMING_FRAME_SP_OFFSET`` can have different values
  between different functions of the same ABI or when
  ``INCOMING_FRAME_SP_OFFSET`` does not agree with GAS default CFI.

.. c:macro:: ARG_POINTER_CFA_OFFSET (fundecl)

  A C expression whose value is an integer giving the offset, in bytes,
  from the argument pointer to the canonical frame address (cfa).  The
  final value should coincide with that calculated by
  ``INCOMING_FRAME_SP_OFFSET``.  Which is unfortunately not usable
  during virtual register instantiation.

  The default value for this macro is
  ``FIRST_PARM_OFFSET (fundecl) + crtl->args.pretend_args_size``,
  which is correct for most machines; in general, the arguments are found
  immediately before the stack frame.  Note that this is not the case on
  some targets that save registers into the caller's frame, such as SPARC
  and rs6000, and so such targets need to define this macro.

  You only need to define this macro if the default is incorrect, and you
  want to support call frame debugging information like that provided by
  DWARF 2.

.. c:macro:: FRAME_POINTER_CFA_OFFSET (fundecl)

  If defined, a C expression whose value is an integer giving the offset
  in bytes from the frame pointer to the canonical frame address (cfa).
  The final value should coincide with that calculated by
  ``INCOMING_FRAME_SP_OFFSET``.

  Normally the CFA is calculated as an offset from the argument pointer,
  via ``ARG_POINTER_CFA_OFFSET``, but if the argument pointer is
  variable due to the ABI, this may not be possible.  If this macro is
  defined, it implies that the virtual register instantiation should be
  based on the frame pointer instead of the argument pointer.  Only one
  of ``FRAME_POINTER_CFA_OFFSET`` and ``ARG_POINTER_CFA_OFFSET``
  should be defined.

.. c:macro:: CFA_FRAME_BASE_OFFSET (fundecl)

  If defined, a C expression whose value is an integer giving the offset
  in bytes from the canonical frame address (cfa) to the frame base used
  in DWARF 2 debug information.  The default is zero.  A different value
  may reduce the size of debug information on some ports.

.. _exception-handling:

Exception Handling Support
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: exception handling

.. c:macro:: EH_RETURN_DATA_REGNO (N)

  A C expression whose value is the :samp:`{N}` th register number used for
  data by exception handlers, or ``INVALID_REGNUM`` if fewer than
  :samp:`{N}` registers are usable.

  The exception handling library routines communicate with the exception
  handlers via a set of agreed upon registers.  Ideally these registers
  should be call-clobbered; it is possible to use call-saved registers,
  but may negatively impact code size.  The target must support at least
  2 data registers, but should define 4 if there are enough free registers.

  You must define this macro if you want to support call frame exception
  handling like that provided by DWARF 2.

.. c:macro:: EH_RETURN_STACKADJ_RTX

  A C expression whose value is RTL representing a location in which
  to store a stack adjustment to be applied before function return.
  This is used to unwind the stack to an exception handler's call frame.
  It will be assigned zero on code paths that return normally.

  Typically this is a call-clobbered hard register that is otherwise
  untouched by the epilogue, but could also be a stack slot.

  Do not define this macro if the stack pointer is saved and restored
  by the regular prolog and epilog code in the call frame itself; in
  this case, the exception handling library routines will update the
  stack location to be restored in place.  Otherwise, you must define
  this macro if you want to support call frame exception handling like
  that provided by DWARF 2.

.. c:macro:: EH_RETURN_HANDLER_RTX

  A C expression whose value is RTL representing a location in which
  to store the address of an exception handler to which we should
  return.  It will not be assigned on code paths that return normally.

  Typically this is the location in the call frame at which the normal
  return address is stored.  For targets that return by popping an
  address off the stack, this might be a memory address just below
  the *target* call frame rather than inside the current call
  frame.  If defined, ``EH_RETURN_STACKADJ_RTX`` will have already
  been assigned, so it may be used to calculate the location of the
  target call frame.

  Some targets have more complex requirements than storing to an
  address calculable during initial code generation.  In that case
  the ``eh_return`` instruction pattern should be used instead.

  If you want to support call frame exception handling, you must
  define either this macro or the ``eh_return`` instruction pattern.

.. c:macro:: RETURN_ADDR_OFFSET

  If defined, an integer-valued C expression for which rtl will be generated
  to add it to the exception handler address before it is searched in the
  exception handling tables, and to subtract it again from the address before
  using it to return to the exception handler.

.. c:macro:: ASM_PREFERRED_EH_DATA_FORMAT (code, global)

  This macro chooses the encoding of pointers embedded in the exception
  handling sections.  If at all possible, this should be defined such
  that the exception handling section will not require dynamic relocations,
  and so may be read-only.

  :samp:`{code}` is 0 for data, 1 for code labels, 2 for function pointers.
  :samp:`{global}` is true if the symbol may be affected by dynamic relocations.
  The macro should return a combination of the ``DW_EH_PE_*`` defines
  as found in dwarf2.h.

  If this macro is not defined, pointers will not be encoded but
  represented directly.

.. c:macro:: ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX (file, encoding, size, addr, done)

  This macro allows the target to emit whatever special magic is required
  to represent the encoding chosen by ``ASM_PREFERRED_EH_DATA_FORMAT``.
  Generic code takes care of pc-relative and indirect encodings; this must
  be defined if the target uses text-relative or data-relative encodings.

  This is a C statement that branches to :samp:`{done}` if the format was
  handled.  :samp:`{encoding}` is the format chosen, :samp:`{size}` is the number
  of bytes that the format occupies, :samp:`{addr}` is the ``SYMBOL_REF``
  to be emitted.

.. c:macro:: MD_FALLBACK_FRAME_STATE_FOR (context, fs)

  This macro allows the target to add CPU and operating system specific
  code to the call-frame unwinder for use when there is no unwind data
  available.  The most common reason to implement this macro is to unwind
  through signal frames.

  This macro is called from ``uw_frame_state_for`` in
  unwind-dw2.c, unwind-dw2-xtensa.c and
  unwind-ia64.c.  :samp:`{context}` is an ``_Unwind_Context`` ;
  :samp:`{fs}` is an ``_Unwind_FrameState``.  Examine ``context->ra``
  for the address of the code being executed and ``context->cfa`` for
  the stack pointer value.  If the frame can be decoded, the register
  save addresses should be updated in :samp:`{fs}` and the macro should
  evaluate to ``_URC_NO_REASON``.  If the frame cannot be decoded,
  the macro should evaluate to ``_URC_END_OF_STACK``.

  For proper signal handling in Java this macro is accompanied by
  ``MAKE_THROW_FRAME``, defined in libjava/include/*-signal.h headers.

.. c:macro:: MD_HANDLE_UNWABI (context, fs)

  This macro allows the target to add operating system specific code to the
  call-frame unwinder to handle the IA-64 ``.unwabi`` unwinding directive,
  usually used for signal or interrupt frames.

  This macro is called from ``uw_update_context`` in libgcc's
  unwind-ia64.c.  :samp:`{context}` is an ``_Unwind_Context`` ;
  :samp:`{fs}` is an ``_Unwind_FrameState``.  Examine ``fs->unwabi``
  for the abi and context in the ``.unwabi`` directive.  If the
  ``.unwabi`` directive can be handled, the register save addresses should
  be updated in :samp:`{fs}`.

.. c:macro:: TARGET_USES_WEAK_UNWIND_INFO

  A C expression that evaluates to true if the target requires unwind
  info to be given comdat linkage.  Define it to be ``1`` if comdat
  linkage is necessary.  The default is ``0``.

.. _stack-checking:

Specifying How Stack Checking is Done
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC will check that stack references are within the boundaries of the
stack, if the option :option:`-fstack-check` is specified, in one of
three ways:

* If the value of the ``STACK_CHECK_BUILTIN`` macro is nonzero, GCC
  will assume that you have arranged for full stack checking to be done
  at appropriate places in the configuration files.  GCC will not do
  other special processing.

* If ``STACK_CHECK_BUILTIN`` is zero and the value of the
  ``STACK_CHECK_STATIC_BUILTIN`` macro is nonzero, GCC will assume
  that you have arranged for static stack checking (checking of the
  static stack frame of functions) to be done at appropriate places
  in the configuration files.  GCC will only emit code to do dynamic
  stack checking (checking on dynamic stack allocations) using the third
  approach below.

* If neither of the above are true, GCC will generate code to periodically
  'probe' the stack pointer using the values of the macros defined below.

If neither STACK_CHECK_BUILTIN nor STACK_CHECK_STATIC_BUILTIN is defined,
GCC will change its allocation strategy for large objects if the option
:option:`-fstack-check` is specified: they will always be allocated
dynamically if their size exceeds ``STACK_CHECK_MAX_VAR_SIZE`` bytes.

.. c:macro:: STACK_CHECK_BUILTIN

  A nonzero value if stack checking is done by the configuration files in a
  machine-dependent manner.  You should define this macro if stack checking
  is required by the ABI of your machine or if you would like to do stack
  checking in some more efficient way than the generic approach.  The default
  value of this macro is zero.

.. c:macro:: STACK_CHECK_STATIC_BUILTIN

  A nonzero value if static stack checking is done by the configuration files
  in a machine-dependent manner.  You should define this macro if you would
  like to do static stack checking in some more efficient way than the generic
  approach.  The default value of this macro is zero.

.. c:macro:: STACK_CHECK_PROBE_INTERVAL_EXP

  An integer specifying the interval at which GCC must generate stack probe
  instructions, defined as 2 raised to this integer.  You will normally
  define this macro so that the interval be no larger than the size of
  the 'guard pages' at the end of a stack area.  The default value
  of 12 (4096-byte interval) is suitable for most systems.

.. c:macro:: STACK_CHECK_MOVING_SP

  An integer which is nonzero if GCC should move the stack pointer page by page
  when doing probes.  This can be necessary on systems where the stack pointer
  contains the bottom address of the memory area accessible to the executing
  thread at any point in time.  In this situation an alternate signal stack
  is required in order to be able to recover from a stack overflow.  The
  default value of this macro is zero.

.. c:macro:: STACK_CHECK_PROTECT

  The number of bytes of stack needed to recover from a stack overflow, for
  languages where such a recovery is supported.  The default value of 4KB/8KB
  with the ``setjmp`` / ``longjmp`` -based exception handling mechanism and
  8KB/12KB with other exception handling mechanisms should be adequate for most
  architectures and operating systems.

The following macros are relevant only if neither STACK_CHECK_BUILTIN
nor STACK_CHECK_STATIC_BUILTIN is defined; you can omit them altogether
in the opposite case.

.. c:macro:: STACK_CHECK_MAX_FRAME_SIZE

  The maximum size of a stack frame, in bytes.  GCC will generate probe
  instructions in non-leaf functions to ensure at least this many bytes of
  stack are available.  If a stack frame is larger than this size, stack
  checking will not be reliable and GCC will issue a warning.  The
  default is chosen so that GCC only generates one instruction on most
  systems.  You should normally not change the default value of this macro.

.. c:macro:: STACK_CHECK_FIXED_FRAME_SIZE

  GCC uses this value to generate the above warning message.  It
  represents the amount of fixed frame used by a function, not including
  space for any callee-saved registers, temporaries and user variables.
  You need only specify an upper bound for this amount and will normally
  use the default of four words.

.. c:macro:: STACK_CHECK_MAX_VAR_SIZE

  The maximum size, in bytes, of an object that GCC will place in the
  fixed area of the stack frame when the user specifies
  :option:`-fstack-check`.
  GCC computed the default from the values of the above macros and you will
  normally not need to override that default.

.. function:: HOST_WIDE_INT TARGET_STACK_CLASH_PROTECTION_ALLOCA_PROBE_RANGE (void)

  Some targets have an ABI defined interval for which no probing needs to be done.
  When a probe does need to be done this same interval is used as the probe distance
  up when doing stack clash protection for alloca.
  On such targets this value can be set to override the default probing up interval.
  Define this variable to return nonzero if such a probe range is required or zero otherwise.
  Defining this hook also requires your functions which make use of alloca to have at least 8 byes
  of outgoing arguments.  If this is not the case the stack will be corrupted.
  You need not define this macro if it would always have the value zero.

.. _frame-registers:

Registers That Address the Stack Frame
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This discusses registers that address the stack frame.

.. c:macro:: STACK_POINTER_REGNUM

  The register number of the stack pointer register, which must also be a
  fixed register according to ``FIXED_REGISTERS``.  On most machines,
  the hardware determines which register this is.

.. c:macro:: FRAME_POINTER_REGNUM

  The register number of the frame pointer register, which is used to
  access automatic variables in the stack frame.  On some machines, the
  hardware determines which register this is.  On other machines, you can
  choose any register you wish for this purpose.

.. c:macro:: HARD_FRAME_POINTER_REGNUM

  On some machines the offset between the frame pointer and starting
  offset of the automatic variables is not known until after register
  allocation has been done (for example, because the saved registers are
  between these two locations).  On those machines, define
  ``FRAME_POINTER_REGNUM`` the number of a special, fixed register to
  be used internally until the offset is known, and define
  ``HARD_FRAME_POINTER_REGNUM`` to be the actual hard register number
  used for the frame pointer.

  You should define this macro only in the very rare circumstances when it
  is not possible to calculate the offset between the frame pointer and
  the automatic variables until after register allocation has been
  completed.  When this macro is defined, you must also indicate in your
  definition of ``ELIMINABLE_REGS`` how to eliminate
  ``FRAME_POINTER_REGNUM`` into either ``HARD_FRAME_POINTER_REGNUM``
  or ``STACK_POINTER_REGNUM``.

  Do not define this macro if it would be the same as
  ``FRAME_POINTER_REGNUM``.

.. c:macro:: ARG_POINTER_REGNUM

  The register number of the arg pointer register, which is used to access
  the function's argument list.  On some machines, this is the same as the
  frame pointer register.  On some machines, the hardware determines which
  register this is.  On other machines, you can choose any register you
  wish for this purpose.  If this is not the same register as the frame
  pointer register, then you must mark it as a fixed register according to
  ``FIXED_REGISTERS``, or arrange to be able to eliminate it
  (see :ref:`elimination`).

.. c:macro:: HARD_FRAME_POINTER_IS_FRAME_POINTER

  Define this to a preprocessor constant that is nonzero if
  ``hard_frame_pointer_rtx`` and ``frame_pointer_rtx`` should be
  the same.  The default definition is :samp:`(HARD_FRAME_POINTER_REGNUM
  == FRAME_POINTER_REGNUM)`; you only need to define this macro if that
  definition is not suitable for use in preprocessor conditionals.

.. c:macro:: HARD_FRAME_POINTER_IS_ARG_POINTER

  Define this to a preprocessor constant that is nonzero if
  ``hard_frame_pointer_rtx`` and ``arg_pointer_rtx`` should be the
  same.  The default definition is :samp:`(HARD_FRAME_POINTER_REGNUM ==
  ARG_POINTER_REGNUM)`; you only need to define this macro if that
  definition is not suitable for use in preprocessor conditionals.

.. c:macro:: RETURN_ADDRESS_POINTER_REGNUM

  The register number of the return address pointer register, which is used to
  access the current function's return address from the stack.  On some
  machines, the return address is not at a fixed offset from the frame
  pointer or stack pointer or argument pointer.  This register can be defined
  to point to the return address on the stack, and then be converted by
  ``ELIMINABLE_REGS`` into either the frame pointer or stack pointer.

  Do not define this macro unless there is no other way to get the return
  address from the stack.

.. c:macro:: STATIC_CHAIN_REGNUM

  Register numbers used for passing a function's static chain pointer.  If
  register windows are used, the register number as seen by the called
  function is ``STATIC_CHAIN_INCOMING_REGNUM``, while the register
  number as seen by the calling function is ``STATIC_CHAIN_REGNUM``.  If
  these registers are the same, ``STATIC_CHAIN_INCOMING_REGNUM`` need
  not be defined.

  The static chain register need not be a fixed register.

  If the static chain is passed in memory, these macros should not be
  defined; instead, the ``TARGET_STATIC_CHAIN`` hook should be used.

.. function:: rtx TARGET_STATIC_CHAIN (const_tree fndecl_or_type, bool incoming_p)

  This hook replaces the use of ``STATIC_CHAIN_REGNUM`` et al for
  targets that may use different static chain locations for different
  nested functions.  This may be required if the target has function
  attributes that affect the calling conventions of the function and
  those calling conventions use different static chain locations.

  The default version of this hook uses ``STATIC_CHAIN_REGNUM`` et al.

  If the static chain is passed in memory, this hook should be used to
  provide rtx giving ``mem`` expressions that denote where they are stored.
  Often the ``mem`` expression as seen by the caller will be at an offset
  from the stack pointer and the ``mem`` expression as seen by the callee
  will be at an offset from the frame pointer.

  .. index:: stack_pointer_rtx

  .. index:: frame_pointer_rtx

  .. index:: arg_pointer_rtx

  The variables ``stack_pointer_rtx``, ``frame_pointer_rtx``, and
  ``arg_pointer_rtx`` will have been initialized and should be used
  to refer to those items.

.. c:macro:: DWARF_FRAME_REGISTERS

  This macro specifies the maximum number of hard registers that can be
  saved in a call frame.  This is used to size data structures used in
  DWARF2 exception handling.

  Prior to GCC 3.0, this macro was needed in order to establish a stable
  exception handling ABI in the face of adding new hard registers for ISA
  extensions.  In GCC 3.0 and later, the EH ABI is insulated from changes
  in the number of hard registers.  Nevertheless, this macro can still be
  used to reduce the runtime memory requirements of the exception handling
  routines, which can be substantial if the ISA contains a lot of
  registers that are not call-saved.

  If this macro is not defined, it defaults to
  ``FIRST_PSEUDO_REGISTER``.

.. c:macro:: PRE_GCC3_DWARF_FRAME_REGISTERS

  This macro is similar to ``DWARF_FRAME_REGISTERS``, but is provided
  for backward compatibility in pre GCC 3.0 compiled code.

  If this macro is not defined, it defaults to
  ``DWARF_FRAME_REGISTERS``.

.. c:macro:: DWARF_REG_TO_UNWIND_COLUMN (regno)

  Define this macro if the target's representation for dwarf registers
  is different than the internal representation for unwind column.
  Given a dwarf register, this macro should return the internal unwind
  column number to use instead.

.. c:macro:: DWARF_FRAME_REGNUM (regno)

  Define this macro if the target's representation for dwarf registers
  used in .eh_frame or .debug_frame is different from that used in other
  debug info sections.  Given a GCC hard register number, this macro
  should return the .eh_frame register number.  The default is
  ``DBX_REGISTER_NUMBER (regno)``.

.. c:macro:: DWARF2_FRAME_REG_OUT (regno, for_eh)

  Define this macro to map register numbers held in the call frame info
  that GCC has collected using ``DWARF_FRAME_REGNUM`` to those that
  should be output in .debug_frame ( ``for_eh`` is zero) and
  .eh_frame ( ``for_eh`` is nonzero).  The default is to
  return ``regno``.

.. c:macro:: REG_VALUE_IN_UNWIND_CONTEXT

  Define this macro if the target stores register values as
  ``_Unwind_Word`` type in unwind context.  It should be defined if
  target register size is larger than the size of ``void *``.  The
  default is to store register values as ``void *`` type.

.. c:macro:: ASSUME_EXTENDED_UNWIND_CONTEXT

  Define this macro to be 1 if the target always uses extended unwind
  context with version, args_size and by_value fields.  If it is undefined,
  it will be defined to 1 when ``REG_VALUE_IN_UNWIND_CONTEXT`` is
  defined and 0 otherwise.

.. c:macro:: DWARF_LAZY_REGISTER_VALUE (regno, value)

  Define this macro if the target has pseudo DWARF registers whose
  values need to be computed lazily on demand by the unwinder (such as when
  referenced in a CFA expression).  The macro returns true if :samp:`{regno}`
  is such a register and stores its value in :samp:`*{value}` if so.

.. _elimination:

Eliminating Frame Pointer and Arg Pointer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This is about eliminating the frame pointer and arg pointer.

.. function:: bool TARGET_FRAME_POINTER_REQUIRED (void)

  This target hook should return ``true`` if a function must have and use
  a frame pointer.  This target hook is called in the reload pass.  If its return
  value is ``true`` the function will have a frame pointer.

  This target hook can in principle examine the current function and decide
  according to the facts, but on most machines the constant ``false`` or the
  constant ``true`` suffices.  Use ``false`` when the machine allows code
  to be generated with no frame pointer, and doing so saves some time or space.
  Use ``true`` when there is no possible advantage to avoiding a frame
  pointer.

  In certain cases, the compiler does not know how to produce valid code
  without a frame pointer.  The compiler recognizes those cases and
  automatically gives the function a frame pointer regardless of what
  ``targetm.frame_pointer_required`` returns.  You don't need to worry about
  them.

  In a function that does not require a frame pointer, the frame pointer
  register can be allocated for ordinary usage, unless you mark it as a
  fixed register.  See ``FIXED_REGISTERS`` for more information.

  Default return value is ``false``.

.. c:macro:: ELIMINABLE_REGS

  This macro specifies a table of register pairs used to eliminate
  unneeded registers that point into the stack frame.

  The definition of this macro is a list of structure initializations, each
  of which specifies an original and replacement register.

  On some machines, the position of the argument pointer is not known until
  the compilation is completed.  In such a case, a separate hard register
  must be used for the argument pointer.  This register can be eliminated by
  replacing it with either the frame pointer or the argument pointer,
  depending on whether or not the frame pointer has been eliminated.

  In this case, you might specify:

  .. code-block:: c++

    #define ELIMINABLE_REGS  \
    {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
     {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
     {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

  Note that the elimination of the argument pointer with the stack pointer is
  specified first since that is the preferred elimination.

.. function:: bool TARGET_CAN_ELIMINATE (const int from_reg, const int to_reg)

  This target hook should return ``true`` if the compiler is allowed to
  try to replace register number :samp:`{from_reg}` with register number
  :samp:`{to_reg}`.  This target hook will usually be ``true``, since most of the
  cases preventing register elimination are things that the compiler already
  knows about.

  Default return value is ``true``.

.. c:macro:: INITIAL_ELIMINATION_OFFSET (from-reg, to-reg, offset-var)

  This macro returns the initial difference between the specified pair
  of registers.  The value would be computed from information
  such as the result of ``get_frame_size ()`` and the tables of
  registers ``df_regs_ever_live_p`` and ``call_used_regs``.

.. function:: void TARGET_COMPUTE_FRAME_LAYOUT (void)

  This target hook is called once each time the frame layout needs to be
  recalculated.  The calculations can be cached by the target and can then
  be used by ``INITIAL_ELIMINATION_OFFSET`` instead of re-computing the
  layout on every invocation of that hook.  This is particularly useful
  for targets that have an expensive frame layout function.  Implementing
  this callback is optional.

.. _stack-arguments:

Passing Function Arguments on the Stack
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: arguments on stack

.. index:: stack arguments

The macros in this section control how arguments are passed
on the stack.  See the following section for other macros that
control passing certain arguments in registers.

.. function:: bool TARGET_PROMOTE_PROTOTYPES (const_tree fntype)

  This target hook returns ``true`` if an argument declared in a
  prototype as an integral type smaller than ``int`` should actually be
  passed as an ``int``.  In addition to avoiding errors in certain
  cases of mismatch, it also makes for better code on certain machines.
  The default is to not promote prototypes.

.. c:macro:: PUSH_ARGS

  A C expression.  If nonzero, push insns will be used to pass
  outgoing arguments.
  If the target machine does not have a push instruction, set it to zero.
  That directs GCC to use an alternate strategy: to
  allocate the entire argument block and then store the arguments into
  it.  When ``PUSH_ARGS`` is nonzero, ``PUSH_ROUNDING`` must be defined too.

.. c:macro:: PUSH_ARGS_REVERSED

  A C expression.  If nonzero, function arguments will be evaluated from
  last to first, rather than from first to last.  If this macro is not
  defined, it defaults to ``PUSH_ARGS`` on targets where the stack
  and args grow in opposite directions, and 0 otherwise.

.. c:macro:: PUSH_ROUNDING (npushed)

  A C expression that is the number of bytes actually pushed onto the
  stack when an instruction attempts to push :samp:`{npushed}` bytes.

  On some machines, the definition

  .. code-block:: c++

    #define PUSH_ROUNDING(BYTES) (BYTES)

  will suffice.  But on other machines, instructions that appear
  to push one byte actually push two bytes in an attempt to maintain
  alignment.  Then the definition should be

  .. code-block:: c++

    #define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)

  If the value of this macro has a type, it should be an unsigned type.

.. index:: outgoing_args_size

.. index:: crtl->outgoing_args_size

.. c:macro:: ACCUMULATE_OUTGOING_ARGS

  A C expression.  If nonzero, the maximum amount of space required for outgoing arguments
  will be computed and placed into
  ``crtl->outgoing_args_size``.  No space will be pushed
  onto the stack for each call; instead, the function prologue should
  increase the stack frame size by this amount.

  Setting both ``PUSH_ARGS`` and ``ACCUMULATE_OUTGOING_ARGS``
  is not proper.

.. c:macro:: REG_PARM_STACK_SPACE (fndecl)

  Define this macro if functions should assume that stack space has been
  allocated for arguments even when their values are passed in
  registers.

  The value of this macro is the size, in bytes, of the area reserved for
  arguments passed in registers for the function represented by :samp:`{fndecl}`,
  which can be zero if GCC is calling a library function.
  The argument :samp:`{fndecl}` can be the FUNCTION_DECL, or the type itself
  of the function.

  This space can be allocated by the caller, or be a part of the
  machine-dependent stack frame: ``OUTGOING_REG_PARM_STACK_SPACE`` says
  which.

.. above is overfull.  not sure what to do.  -mew 5feb93  did
   something, not sure if it looks good.  -mew 10feb93

.. c:macro:: INCOMING_REG_PARM_STACK_SPACE (fndecl)

  Like ``REG_PARM_STACK_SPACE``, but for incoming register arguments.
  Define this macro if space guaranteed when compiling a function body
  is different to space required when making a call, a situation that
  can arise with K&R style function definitions.

.. c:macro:: OUTGOING_REG_PARM_STACK_SPACE (fntype)

  Define this to a nonzero value if it is the responsibility of the
  caller to allocate the area reserved for arguments passed in registers
  when calling a function of :samp:`{fntype}`.  :samp:`{fntype}` may be NULL
  if the function called is a library function.

  If ``ACCUMULATE_OUTGOING_ARGS`` is defined, this macro controls
  whether the space for these arguments counts in the value of
  ``crtl->outgoing_args_size``.

.. c:macro:: STACK_PARMS_IN_REG_PARM_AREA

  Define this macro if ``REG_PARM_STACK_SPACE`` is defined, but the
  stack parameters don't skip the area specified by it.

  .. i changed this, makes more sens and it should have taken care of the

  .. overfull.. not as specific, tho.  -mew 5feb93

  Normally, when a parameter is not passed in registers, it is placed on the
  stack beyond the ``REG_PARM_STACK_SPACE`` area.  Defining this macro
  suppresses this behavior and causes the parameter to be passed on the
  stack in its natural location.

.. function:: poly_int64 TARGET_RETURN_POPS_ARGS (tree fundecl, tree funtype, poly_int64 size)

  This target hook returns the number of bytes of its own arguments that
  a function pops on returning, or 0 if the function pops no arguments
  and the caller must therefore pop them all after the function returns.

  :samp:`{fundecl}` is a C variable whose value is a tree node that describes
  the function in question.  Normally it is a node of type
  ``FUNCTION_DECL`` that describes the declaration of the function.
  From this you can obtain the ``DECL_ATTRIBUTES`` of the function.

  :samp:`{funtype}` is a C variable whose value is a tree node that
  describes the function in question.  Normally it is a node of type
  ``FUNCTION_TYPE`` that describes the data type of the function.
  From this it is possible to obtain the data types of the value and
  arguments (if known).

  When a call to a library function is being considered, :samp:`{fundecl}`
  will contain an identifier node for the library function.  Thus, if
  you need to distinguish among various library functions, you can do so
  by their names.  Note that 'library function' in this context means
  a function used to perform arithmetic, whose name is known specially
  in the compiler and was not mentioned in the C code being compiled.

  :samp:`{size}` is the number of bytes of arguments passed on the
  stack.  If a variable number of bytes is passed, it is zero, and
  argument popping will always be the responsibility of the calling function.

  On the VAX, all functions always pop their arguments, so the definition
  of this macro is :samp:`{size}`.  On the 68000, using the standard
  calling convention, no functions pop their arguments, so the value of
  the macro is always 0 in this case.  But an alternative calling
  convention is available in which functions that take a fixed number of
  arguments pop them but other functions (such as ``printf`` ) pop
  nothing (the caller pops all).  When this convention is in use,
  :samp:`{funtype}` is examined to determine whether a function takes a fixed
  number of arguments.

.. c:macro:: CALL_POPS_ARGS (cum)

  A C expression that should indicate the number of bytes a call sequence
  pops off the stack.  It is added to the value of ``RETURN_POPS_ARGS``
  when compiling a function call.

  :samp:`{cum}` is the variable in which all arguments to the called function
  have been accumulated.

  On certain architectures, such as the SH5, a call trampoline is used
  that pops certain registers off the stack, depending on the arguments
  that have been passed to the function.  Since this is a property of the
  call site, not of the called function, ``RETURN_POPS_ARGS`` is not
  appropriate.

.. _register-arguments:

Passing Arguments in Registers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: arguments in registers

.. index:: registers arguments

This section describes the macros which let you control how various
types of arguments are passed in registers or how they are arranged in
the stack.

.. function:: rtx TARGET_FUNCTION_ARG (cumulative_args_t ca, const function_arg_info &arg)

  Return an RTX indicating whether function argument :samp:`{arg}` is passed
  in a register and if so, which register.  Argument :samp:`{ca}` summarizes all
  the previous arguments.

  The return value is usually either a ``reg`` RTX for the hard
  register in which to pass the argument, or zero to pass the argument
  on the stack.

  The return value can be a ``const_int`` which means argument is
  passed in a target specific slot with specified number.  Target hooks
  should be used to store or load argument in such case.  See
  ``TARGET_STORE_BOUNDS_FOR_ARG`` and ``TARGET_LOAD_BOUNDS_FOR_ARG``
  for more information.

  The value of the expression can also be a ``parallel`` RTX.  This is
  used when an argument is passed in multiple locations.  The mode of the
  ``parallel`` should be the mode of the entire argument.  The
  ``parallel`` holds any number of ``expr_list`` pairs; each one
  describes where part of the argument is passed.  In each
  ``expr_list`` the first operand must be a ``reg`` RTX for the hard
  register in which to pass this part of the argument, and the mode of the
  register RTX indicates how large this part of the argument is.  The
  second operand of the ``expr_list`` is a ``const_int`` which gives
  the offset in bytes into the entire argument of where this part starts.
  As a special exception the first ``expr_list`` in the ``parallel``
  RTX may have a first operand of zero.  This indicates that the entire
  argument is also stored on the stack.

  The last time this hook is called, it is called with ``MODE ==
  VOIDmode``, and its result is passed to the ``call`` or ``call_value``
  pattern as operands 2 and 3 respectively.

  .. index:: stdarg.h and register arguments

  The usual way to make the ISO library stdarg.h work on a
  machine where some arguments are usually passed in registers, is to
  cause nameless arguments to be passed on the stack instead.  This is
  done by making ``TARGET_FUNCTION_ARG`` return 0 whenever
  :samp:`{named}` is ``false``.

  .. index:: TARGET_MUST_PASS_IN_STACK, and TARGET_FUNCTION_ARG

  .. index:: REG_PARM_STACK_SPACE, and TARGET_FUNCTION_ARG

  You may use the hook ``targetm.calls.must_pass_in_stack``
  in the definition of this macro to determine if this argument is of a
  type that must be passed in the stack.  If ``REG_PARM_STACK_SPACE``
  is not defined and ``TARGET_FUNCTION_ARG`` returns nonzero for such an
  argument, the compiler will abort.  If ``REG_PARM_STACK_SPACE`` is
  defined, the argument will be computed in the stack and then loaded into
  a register.

.. function:: bool TARGET_MUST_PASS_IN_STACK (const function_arg_info &arg)

  This target hook should return ``true`` if we should not pass :samp:`{arg}`
  solely in registers.  The file expr.h defines a
  definition that is usually appropriate, refer to expr.h for additional
  documentation.

.. function:: rtx TARGET_FUNCTION_INCOMING_ARG (cumulative_args_t ca, const function_arg_info &arg)

  Define this hook if the caller and callee on the target have different
  views of where arguments are passed.  Also define this hook if there are
  functions that are never directly called, but are invoked by the hardware
  and which have nonstandard calling conventions.

  In this case ``TARGET_FUNCTION_ARG`` computes the register in
  which the caller passes the value, and
  ``TARGET_FUNCTION_INCOMING_ARG`` should be defined in a similar
  fashion to tell the function being called where the arguments will
  arrive.

  ``TARGET_FUNCTION_INCOMING_ARG`` can also return arbitrary address
  computation using hard register, which can be forced into a register,
  so that it can be used to pass special arguments.

  If ``TARGET_FUNCTION_INCOMING_ARG`` is not defined,
  ``TARGET_FUNCTION_ARG`` serves both purposes.

.. function:: bool TARGET_USE_PSEUDO_PIC_REG (void)

  This hook should return 1 in case pseudo register should be created
  for pic_offset_table_rtx during function expand.

.. function:: void TARGET_INIT_PIC_REG (void)

  Perform a target dependent initialization of pic_offset_table_rtx.
  This hook is called at the start of register allocation.

.. function:: int TARGET_ARG_PARTIAL_BYTES (cumulative_args_t cum, const function_arg_info &arg)

  This target hook returns the number of bytes at the beginning of an
  argument that must be put in registers.  The value must be zero for
  arguments that are passed entirely in registers or that are entirely
  pushed on the stack.

  On some machines, certain arguments must be passed partially in
  registers and partially in memory.  On these machines, typically the
  first few words of arguments are passed in registers, and the rest
  on the stack.  If a multi-word argument (a ``double`` or a
  structure) crosses that boundary, its first few words must be passed
  in registers and the rest must be pushed.  This macro tells the
  compiler when this occurs, and how many bytes should go in registers.

  ``TARGET_FUNCTION_ARG`` for these arguments should return the first
  register to be used by the caller for this argument; likewise
  ``TARGET_FUNCTION_INCOMING_ARG``, for the called function.

.. function:: bool TARGET_PASS_BY_REFERENCE (cumulative_args_t cum, const function_arg_info &arg)

  This target hook should return ``true`` if argument :samp:`{arg}` at the
  position indicated by :samp:`{cum}` should be passed by reference.  This
  predicate is queried after target independent reasons for being
  passed by reference, such as ``TREE_ADDRESSABLE (arg.type)``.

  If the hook returns true, a copy of that argument is made in memory and a
  pointer to the argument is passed instead of the argument itself.
  The pointer is passed in whatever way is appropriate for passing a pointer
  to that type.

.. function:: bool TARGET_CALLEE_COPIES (cumulative_args_t cum, const function_arg_info &arg)

  The function argument described by the parameters to this hook is
  known to be passed by reference.  The hook should return true if the
  function argument should be copied by the callee instead of copied
  by the caller.

  For any argument for which the hook returns true, if it can be
  determined that the argument is not modified, then a copy need
  not be generated.

  The default version of this hook always returns false.

.. c:macro:: CUMULATIVE_ARGS

  A C type for declaring a variable that is used as the first argument
  of ``TARGET_FUNCTION_ARG`` and other related values.  For some
  target machines, the type ``int`` suffices and can hold the number
  of bytes of argument so far.

  There is no need to record in ``CUMULATIVE_ARGS`` anything about the
  arguments that have been passed on the stack.  The compiler has other
  variables to keep track of that.  For target machines on which all
  arguments are passed on the stack, there is no need to store anything in
  ``CUMULATIVE_ARGS`` ; however, the data structure must exist and
  should not be empty, so use ``int``.

.. c:macro:: OVERRIDE_ABI_FORMAT (fndecl)

  If defined, this macro is called before generating any code for a
  function, but after the :samp:`{cfun}` descriptor for the function has been
  created.  The back end may use this macro to update :samp:`{cfun}` to
  reflect an ABI other than that which would normally be used by default.
  If the compiler is generating code for a compiler-generated function,
  :samp:`{fndecl}` may be ``NULL``.

.. c:macro:: INIT_CUMULATIVE_ARGS (cum, fntype, libname, fndecl, n_named_args)

  A C statement (sans semicolon) for initializing the variable
  :samp:`{cum}` for the state at the beginning of the argument list.  The
  variable has type ``CUMULATIVE_ARGS``.  The value of :samp:`{fntype}`
  is the tree node for the data type of the function which will receive
  the args, or 0 if the args are to a compiler support library function.
  For direct calls that are not libcalls, :samp:`{fndecl}` contain the
  declaration node of the function.  :samp:`{fndecl}` is also set when
  ``INIT_CUMULATIVE_ARGS`` is used to find arguments for the function
  being compiled.  :samp:`{n_named_args}` is set to the number of named
  arguments, including a structure return address if it is passed as a
  parameter, when making a call.  When processing incoming arguments,
  :samp:`{n_named_args}` is set to -1.

  When processing a call to a compiler support library function,
  :samp:`{libname}` identifies which one.  It is a ``symbol_ref`` rtx which
  contains the name of the function, as a string.  :samp:`{libname}` is 0 when
  an ordinary C function call is being processed.  Thus, each time this
  macro is called, either :samp:`{libname}` or :samp:`{fntype}` is nonzero, but
  never both of them at once.

.. c:macro:: INIT_CUMULATIVE_LIBCALL_ARGS (cum, mode, libname)

  Like ``INIT_CUMULATIVE_ARGS`` but only used for outgoing libcalls,
  it gets a ``MODE`` argument instead of :samp:`{fntype}`, that would be
  ``NULL``.  :samp:`{indirect}` would always be zero, too.  If this macro
  is not defined, ``INIT_CUMULATIVE_ARGS (cum, NULL_RTX, libname,
  0)`` is used instead.

.. c:macro:: INIT_CUMULATIVE_INCOMING_ARGS (cum, fntype, libname)

  Like ``INIT_CUMULATIVE_ARGS`` but overrides it for the purposes of
  finding the arguments for the function being compiled.  If this macro is
  undefined, ``INIT_CUMULATIVE_ARGS`` is used instead.

  The value passed for :samp:`{libname}` is always 0, since library routines
  with special calling conventions are never compiled with GCC.  The
  argument :samp:`{libname}` exists for symmetry with
  ``INIT_CUMULATIVE_ARGS``.

  .. could use "this macro" in place of @code{INIT_CUMULATIVE_ARGS}, maybe.

  .. -mew 5feb93   i switched the order of the sentences.  -mew 10feb93

.. function:: void TARGET_FUNCTION_ARG_ADVANCE (cumulative_args_t ca, const function_arg_info &arg)

  This hook updates the summarizer variable pointed to by :samp:`{ca}` to
  advance past argument :samp:`{arg}` in the argument list.  Once this is done,
  the variable :samp:`{cum}` is suitable for analyzing the *following*
  argument with ``TARGET_FUNCTION_ARG``, etc.

  This hook need not do anything if the argument in question was passed
  on the stack.  The compiler knows how to track the amount of stack space
  used for arguments without any special help.

.. function:: HOST_WIDE_INT TARGET_FUNCTION_ARG_OFFSET (machine_mode mode, const_tree type)

  This hook returns the number of bytes to add to the offset of an
  argument of type :samp:`{type}` and mode :samp:`{mode}` when passed in memory.
  This is needed for the SPU, which passes ``char`` and ``short``
  arguments in the preferred slot that is in the middle of the quad word
  instead of starting at the top.  The default implementation returns 0.

.. function:: pad_direction TARGET_FUNCTION_ARG_PADDING (machine_mode mode, const_tree type)

  This hook determines whether, and in which direction, to pad out
  an argument of mode :samp:`{mode}` and type :samp:`{type}`.  It returns
  ``PAD_UPWARD`` to insert padding above the argument, ``PAD_DOWNWARD``
  to insert padding below the argument, or ``PAD_NONE`` to inhibit padding.

  The *amount* of padding is not controlled by this hook, but by
  ``TARGET_FUNCTION_ARG_ROUND_BOUNDARY``.  It is always just enough
  to reach the next multiple of that boundary.

  This hook has a default definition that is right for most systems.
  For little-endian machines, the default is to pad upward.  For
  big-endian machines, the default is to pad downward for an argument of
  constant size shorter than an ``int``, and upward otherwise.

.. c:macro:: PAD_VARARGS_DOWN

  If defined, a C expression which determines whether the default
  implementation of va_arg will attempt to pad down before reading the
  next argument, if that argument is smaller than its aligned space as
  controlled by ``PARM_BOUNDARY``.  If this macro is not defined, all such
  arguments are padded down if ``BYTES_BIG_ENDIAN`` is true.

.. c:macro:: BLOCK_REG_PADDING (mode, type, first)

  Specify padding for the last element of a block move between registers and
  memory.  :samp:`{first}` is nonzero if this is the only element.  Defining this
  macro allows better control of register function parameters on big-endian
  machines, without using ``PARALLEL`` rtl.  In particular,
  ``MUST_PASS_IN_STACK`` need not test padding and mode of types in
  registers, as there is no longer a "wrong" part of a register;  For example,
  a three byte aggregate may be passed in the high part of a register if so
  required.

.. function:: unsigned int TARGET_FUNCTION_ARG_BOUNDARY (machine_mode mode, const_tree type)

  This hook returns the alignment boundary, in bits, of an argument
  with the specified mode and type.  The default hook returns
  ``PARM_BOUNDARY`` for all arguments.

.. function:: unsigned int TARGET_FUNCTION_ARG_ROUND_BOUNDARY (machine_mode mode, const_tree type)

  Normally, the size of an argument is rounded up to ``PARM_BOUNDARY``,
  which is the default value for this hook.  You can define this hook to
  return a different value if an argument size must be rounded to a larger
  value.

.. c:macro:: FUNCTION_ARG_REGNO_P (regno)

  A C expression that is nonzero if :samp:`{regno}` is the number of a hard
  register in which function arguments are sometimes passed.  This does
  *not* include implicit arguments such as the static chain and
  the structure-value address.  On many machines, no registers can be
  used for this purpose since all function arguments are pushed on the
  stack.

.. function:: bool TARGET_SPLIT_COMPLEX_ARG (const_tree type)

  This hook should return true if parameter of type :samp:`{type}` are passed
  as two scalar parameters.  By default, GCC will attempt to pack complex
  arguments into the target's word size.  Some ABIs require complex arguments
  to be split and treated as their individual components.  For example, on
  AIX64, complex floats should be passed in a pair of floating point
  registers, even though a complex float would fit in one 64-bit floating
  point register.

  The default value of this hook is ``NULL``, which is treated as always
  false.

.. function:: tree TARGET_BUILD_BUILTIN_VA_LIST (void)

  This hook returns a type node for ``va_list`` for the target.
  The default version of the hook returns ``void*``.

.. function:: int TARGET_ENUM_VA_LIST_P (int idx, const char **pname, tree *ptree)

  This target hook is used in function ``c_common_nodes_and_builtins``
  to iterate through the target specific builtin types for va_list. The
  variable :samp:`{idx}` is used as iterator. :samp:`{pname}` has to be a pointer
  to a ``const char *`` and :samp:`{ptree}` a pointer to a ``tree`` typed
  variable.
  The arguments :samp:`{pname}` and :samp:`{ptree}` are used to store the result of
  this macro and are set to the name of the va_list builtin type and its
  internal type.
  If the return value of this macro is zero, then there is no more element.
  Otherwise the :samp:`{IDX}` should be increased for the next call of this
  macro to iterate through all types.

.. function:: tree TARGET_FN_ABI_VA_LIST (tree fndecl)

  This hook returns the va_list type of the calling convention specified by
  :samp:`{fndecl}`.
  The default version of this hook returns ``va_list_type_node``.

.. function:: tree TARGET_CANONICAL_VA_LIST_TYPE (tree type)

  This hook returns the va_list type of the calling convention specified by the
  type of :samp:`{type}`. If :samp:`{type}` is not a valid va_list type, it returns
  ``NULL_TREE``.

.. function:: tree TARGET_GIMPLIFY_VA_ARG_EXPR (tree valist, tree type, gimple_seq *pre_p, gimple_seq *post_p)

  This hook performs target-specific gimplification of
  ``VA_ARG_EXPR``.  The first two parameters correspond to the
  arguments to ``va_arg`` ; the latter two are as in
  ``gimplify.c:gimplify_expr``.

.. function:: bool TARGET_VALID_POINTER_MODE (scalar_int_mode mode)

  Define this to return nonzero if the port can handle pointers
  with machine mode :samp:`{mode}`.  The default version of this
  hook returns true for both ``ptr_mode`` and ``Pmode``.

.. function:: bool TARGET_REF_MAY_ALIAS_ERRNO (ao_ref *ref)

  Define this to return nonzero if the memory reference :samp:`{ref}`
  may alias with the system C library errno location.  The default
  version of this hook assumes the system C library errno location
  is either a declaration of type int or accessed by dereferencing
  a pointer to int.

.. function:: machine_mode TARGET_TRANSLATE_MODE_ATTRIBUTE (machine_mode mode)

  Define this hook if during mode attribute processing, the port should
  translate machine_mode :samp:`{mode}` to another mode.  For example, rs6000's
  ``KFmode``, when it is the same as ``TFmode``.

  The default version of the hook returns that mode that was passed in.

.. function:: bool TARGET_SCALAR_MODE_SUPPORTED_P (scalar_mode mode)

  Define this to return nonzero if the port is prepared to handle
  insns involving scalar mode :samp:`{mode}`.  For a scalar mode to be
  considered supported, all the basic arithmetic and comparisons
  must work.

  The default version of this hook returns true for any mode
  required to handle the basic C types (as defined by the port).
  Included here are the double-word arithmetic supported by the
  code in optabs.c.

.. function:: bool TARGET_VECTOR_MODE_SUPPORTED_P (machine_mode mode)

  Define this to return nonzero if the port is prepared to handle
  insns involving vector mode :samp:`{mode}`.  At the very least, it
  must have move patterns for this mode.

.. function:: bool TARGET_COMPATIBLE_VECTOR_TYPES_P (const_tree type1, const_tree type2)

  Return true if there is no target-specific reason for treating
  vector types :samp:`{type1}` and :samp:`{type2}` as distinct types.  The caller
  has already checked for target-independent reasons, meaning that the
  types are known to have the same mode, to have the same number of elements,
  and to have what the caller considers to be compatible element types.

  The main reason for defining this hook is to reject pairs of types
  that are handled differently by the target's calling convention.
  For example, when a new :samp:`{N}` -bit vector architecture is added
  to a target, the target may want to handle normal :samp:`{N}` -bit
  ``VECTOR_TYPE`` arguments and return values in the same way as
  before, to maintain backwards compatibility.  However, it may also
  provide new, architecture-specific ``VECTOR_TYPE`` s that are passed
  and returned in a more efficient way.  It is then important to maintain
  a distinction between the 'normal' ``VECTOR_TYPE`` s and the new
  architecture-specific ones.

  The default implementation returns true, which is correct for most targets.

.. function:: opt_machine_mode TARGET_ARRAY_MODE (machine_mode mode, unsigned HOST_WIDE_INT nelems)

  Return the mode that GCC should use for an array that has
  :samp:`{nelems}` elements, with each element having mode :samp:`{mode}`.
  Return no mode if the target has no special requirements.  In the
  latter case, GCC looks for an integer mode of the appropriate size
  if available and uses BLKmode otherwise.  Usually the search for the
  integer mode is limited to ``MAX_FIXED_MODE_SIZE``, but the
  ``TARGET_ARRAY_MODE_SUPPORTED_P`` hook allows a larger mode to be
  used in specific cases.

  The main use of this hook is to specify that an array of vectors should
  also have a vector mode.  The default implementation returns no mode.

.. function:: bool TARGET_ARRAY_MODE_SUPPORTED_P (machine_mode mode, unsigned HOST_WIDE_INT nelems)

  Return true if GCC should try to use a scalar mode to store an array
  of :samp:`{nelems}` elements, given that each element has mode :samp:`{mode}`.
  Returning true here overrides the usual ``MAX_FIXED_MODE`` limit
  and allows GCC to use any defined integer mode.

  One use of this hook is to support vector load and store operations
  that operate on several homogeneous vectors.  For example, ARM NEON
  has operations like:

  .. code-block:: c++

    int8x8x3_t vld3_s8 (const int8_t *)

  where the return type is defined as:

  .. code-block:: c++

    typedef struct int8x8x3_t
    {
      int8x8_t val[3];
    } int8x8x3_t;

  If this hook allows ``val`` to have a scalar mode, then
  ``int8x8x3_t`` can have the same mode.  GCC can then store
  ``int8x8x3_t`` s in registers rather than forcing them onto the stack.

.. function:: bool TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P (scalar_float_mode mode)

  Define this to return nonzero if libgcc provides support for the 
  floating-point mode :samp:`{mode}`, which is known to pass 
  ``TARGET_SCALAR_MODE_SUPPORTED_P``.  The default version of this 
  hook returns true for all of ``SFmode``, ``DFmode``, 
  ``XFmode`` and ``TFmode``, if such modes exist.

.. function:: opt_scalar_float_mode TARGET_FLOATN_MODE (int n, bool extended)

  Define this to return the machine mode to use for the type 
  ``_Floatn``, if :samp:`{extended}` is false, or the type 
  ``_Floatnx``, if :samp:`{extended}` is true.  If such a type is not
  supported, return ``opt_scalar_float_mode ()``.  The default version of
  this hook returns ``SFmode`` for ``_Float32``, ``DFmode`` for
  ``_Float64`` and ``_Float32x`` and ``TFmode`` for 
  ``_Float128``, if those modes exist and satisfy the requirements for 
  those types and pass ``TARGET_SCALAR_MODE_SUPPORTED_P`` and 
  ``TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P`` ; for ``_Float64x``, it 
  returns the first of ``XFmode`` and ``TFmode`` that exists and 
  satisfies the same requirements; for other types, it returns 
  ``opt_scalar_float_mode ()``.  The hook is only called for values
  of :samp:`{n}` and :samp:`{extended}` that are valid according to
  ISO/IEC TS 18661-3:2015; that is, :samp:`{n}` is one of 32, 64, 128, or,
  if :samp:`{extended}` is false, 16 or greater than 128 and a multiple of 32.

.. function:: bool TARGET_FLOATN_BUILTIN_P (int func)

  Define this to return true if the ``_Floatn`` and
  ``_Floatnx`` built-in functions should implicitly enable the
  built-in function without the ``__builtin_`` prefix in addition to the
  normal built-in function with the ``__builtin_`` prefix.  The default is
  to only enable built-in functions without the ``__builtin_`` prefix for
  the GNU C langauge.  In strict ANSI/ISO mode, the built-in function without
  the ``__builtin_`` prefix is not enabled.  The argument ``FUNC`` is the
  ``enum built_in_function`` id of the function to be enabled.

.. function:: bool TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P (machine_mode mode)

  Define this to return nonzero for machine modes for which the port has
  small register classes.  If this target hook returns nonzero for a given
  :samp:`{mode}`, the compiler will try to minimize the lifetime of registers
  in :samp:`{mode}`.  The hook may be called with ``VOIDmode`` as argument.
  In this case, the hook is expected to return nonzero if it returns nonzero
  for any mode.

  On some machines, it is risky to let hard registers live across arbitrary
  insns.  Typically, these machines have instructions that require values
  to be in specific registers (like an accumulator), and reload will fail
  if the required hard register is used for another purpose across such an
  insn.

  Passes before reload do not know which hard registers will be used
  in an instruction, but the machine modes of the registers set or used in
  the instruction are already known.  And for some machines, register
  classes are small for, say, integer registers but not for floating point
  registers.  For example, the AMD x86-64 architecture requires specific
  registers for the legacy x86 integer instructions, but there are many
  SSE registers for floating point operations.  On such targets, a good
  strategy may be to return nonzero from this hook for ``INTEGRAL_MODE_P``
  machine modes but zero for the SSE register classes.

  The default version of this hook returns false for any mode.  It is always
  safe to redefine this hook to return with a nonzero value.  But if you
  unnecessarily define it, you will reduce the amount of optimizations
  that can be performed in some cases.  If you do not define this hook
  to return a nonzero value when it is required, the compiler will run out
  of spill registers and print a fatal error message.

.. _scalar-return:

How Scalar Function Values Are Returned
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: return values in registers

.. index:: values, returned by functions

.. index:: scalars, returned as values

This section discusses the macros that control returning scalars as
values---values that can fit in registers.

.. function:: rtx TARGET_FUNCTION_VALUE (const_tree ret_type, const_tree fn_decl_or_type, bool outgoing)

  Define this to return an RTX representing the place where a function
  returns or receives a value of data type :samp:`{ret_type}`, a tree node
  representing a data type.  :samp:`{fn_decl_or_type}` is a tree node
  representing ``FUNCTION_DECL`` or ``FUNCTION_TYPE`` of a
  function being called.  If :samp:`{outgoing}` is false, the hook should
  compute the register in which the caller will see the return value.
  Otherwise, the hook should return an RTX representing the place where
  a function returns a value.

  On many machines, only ``TYPE_MODE (ret_type)`` is relevant.
  (Actually, on most machines, scalar values are returned in the same
  place regardless of mode.)  The value of the expression is usually a
  ``reg`` RTX for the hard register where the return value is stored.
  The value can also be a ``parallel`` RTX, if the return value is in
  multiple places.  See ``TARGET_FUNCTION_ARG`` for an explanation of the
  ``parallel`` form.   Note that the callee will populate every
  location specified in the ``parallel``, but if the first element of
  the ``parallel`` contains the whole return value, callers will use
  that element as the canonical location and ignore the others.  The m68k
  port uses this type of ``parallel`` to return pointers in both
  :samp:`%a0` (the canonical location) and :samp:`%d0`.

  If ``TARGET_PROMOTE_FUNCTION_RETURN`` returns true, you must apply
  the same promotion rules specified in ``PROMOTE_MODE`` if
  :samp:`{valtype}` is a scalar type.

  If the precise function being called is known, :samp:`{func}` is a tree
  node ( ``FUNCTION_DECL`` ) for it; otherwise, :samp:`{func}` is a null
  pointer.  This makes it possible to use a different value-returning
  convention for specific functions when all their calls are
  known.

  Some target machines have 'register windows' so that the register in
  which a function returns its value is not the same as the one in which
  the caller sees the value.  For such machines, you should return
  different RTX depending on :samp:`{outgoing}`.

  ``TARGET_FUNCTION_VALUE`` is not used for return values with
  aggregate data types, because these are returned in another way.  See
  ``TARGET_STRUCT_VALUE_RTX`` and related macros, below.

.. c:macro:: FUNCTION_VALUE (valtype, func)

  This macro has been deprecated.  Use ``TARGET_FUNCTION_VALUE`` for
  a new target instead.

.. c:macro:: LIBCALL_VALUE (mode)

  A C expression to create an RTX representing the place where a library
  function returns a value of mode :samp:`{mode}`.

  Note that 'library function' in this context means a compiler
  support routine, used to perform arithmetic, whose name is known
  specially by the compiler and was not mentioned in the C code being
  compiled.

.. function:: rtx TARGET_LIBCALL_VALUE (machine_mode mode, const_rtx fun)

  Define this hook if the back-end needs to know the name of the libcall
  function in order to determine where the result should be returned.

  The mode of the result is given by :samp:`{mode}` and the name of the called
  library function is given by :samp:`{fun}`.  The hook should return an RTX
  representing the place where the library function result will be returned.

  If this hook is not defined, then LIBCALL_VALUE will be used.

.. c:macro:: FUNCTION_VALUE_REGNO_P (regno)

  A C expression that is nonzero if :samp:`{regno}` is the number of a hard
  register in which the values of called function may come back.

  A register whose use for returning values is limited to serving as the
  second of a pair (for a value of type ``double``, say) need not be
  recognized by this macro.  So for most machines, this definition
  suffices:

  .. code-block:: c++

    #define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

  If the machine has register windows, so that the caller and the called
  function use different registers for the return value, this macro
  should recognize only the caller's register numbers.

  This macro has been deprecated.  Use ``TARGET_FUNCTION_VALUE_REGNO_P``
  for a new target instead.

.. function:: bool TARGET_FUNCTION_VALUE_REGNO_P (const unsigned int regno)

  A target hook that return ``true`` if :samp:`{regno}` is the number of a hard
  register in which the values of called function may come back.

  A register whose use for returning values is limited to serving as the
  second of a pair (for a value of type ``double``, say) need not be
  recognized by this target hook.

  If the machine has register windows, so that the caller and the called
  function use different registers for the return value, this target hook
  should recognize only the caller's register numbers.

  If this hook is not defined, then FUNCTION_VALUE_REGNO_P will be used.

.. c:macro:: APPLY_RESULT_SIZE

  Define this macro if :samp:`untyped_call` and :samp:`untyped_return`
  need more space than is implied by ``FUNCTION_VALUE_REGNO_P`` for
  saving and restoring an arbitrary return value.

.. c:var:: bool TARGET_OMIT_STRUCT_RETURN_REG

  Normally, when a function returns a structure by memory, the address
  is passed as an invisible pointer argument, but the compiler also
  arranges to return the address from the function like it would a normal
  pointer return value.  Define this to true if that behavior is
  undesirable on your target.

.. function:: bool TARGET_RETURN_IN_MSB (const_tree type)

  This hook should return true if values of type :samp:`{type}` are returned
  at the most significant end of a register (in other words, if they are
  padded at the least significant end).  You can assume that :samp:`{type}`
  is returned in a register; the caller is required to check this.

  Note that the register provided by ``TARGET_FUNCTION_VALUE`` must
  be able to hold the complete return value.  For example, if a 1-, 2-
  or 3-byte structure is returned at the most significant end of a
  4-byte register, ``TARGET_FUNCTION_VALUE`` should provide an
  ``SImode`` rtx.

.. _aggregate-return:

How Large Values Are Returned
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: aggregates as return values

.. index:: large return values

.. index:: returning aggregate values

.. index:: structure value address

When a function value's mode is ``BLKmode`` (and in some other
cases), the value is not returned according to
``TARGET_FUNCTION_VALUE`` (see :ref:`scalar-return`).  Instead, the
caller passes the address of a block of memory in which the value
should be stored.  This address is called the :dfn:`structure value
address`.

This section describes how to control returning structure values in
memory.

.. function:: bool TARGET_RETURN_IN_MEMORY (const_tree type, const_tree fntype)

  This target hook should return a nonzero value to say to return the
  function value in memory, just as large structures are always returned.
  Here :samp:`{type}` will be the data type of the value, and :samp:`{fntype}`
  will be the type of the function doing the returning, or ``NULL`` for
  libcalls.

  Note that values of mode ``BLKmode`` must be explicitly handled
  by this function.  Also, the option :option:`-fpcc-struct-return`
  takes effect regardless of this macro.  On most systems, it is
  possible to leave the hook undefined; this causes a default
  definition to be used, whose value is the constant 1 for ``BLKmode``
  values, and 0 otherwise.

  Do not use this hook to indicate that structures and unions should always
  be returned in memory.  You should instead use ``DEFAULT_PCC_STRUCT_RETURN``
  to indicate this.

.. c:macro:: DEFAULT_PCC_STRUCT_RETURN

  Define this macro to be 1 if all structure and union return values must be
  in memory.  Since this results in slower code, this should be defined
  only if needed for compatibility with other compilers or with an ABI.
  If you define this macro to be 0, then the conventions used for structure
  and union return values are decided by the ``TARGET_RETURN_IN_MEMORY``
  target hook.

  If not defined, this defaults to the value 1.

.. function:: rtx TARGET_STRUCT_VALUE_RTX (tree fndecl, int incoming)

  This target hook should return the location of the structure value
  address (normally a ``mem`` or ``reg`` ), or 0 if the address is
  passed as an 'invisible' first argument.  Note that :samp:`{fndecl}` may
  be ``NULL``, for libcalls.  You do not need to define this target
  hook if the address is always passed as an 'invisible' first
  argument.

  On some architectures the place where the structure value address
  is found by the called function is not the same place that the
  caller put it.  This can be due to register windows, or it could
  be because the function prologue moves it to a different place.
  :samp:`{incoming}` is ``1`` or ``2`` when the location is needed in
  the context of the called function, and ``0`` in the context of
  the caller.

  If :samp:`{incoming}` is nonzero and the address is to be found on the
  stack, return a ``mem`` which refers to the frame pointer. If
  :samp:`{incoming}` is ``2``, the result is being used to fetch the
  structure value address at the beginning of a function.  If you need
  to emit adjusting code, you should do it at this point.

.. c:macro:: PCC_STATIC_STRUCT_RETURN

  Define this macro if the usual system convention on the target machine
  for returning structures and unions is for the called function to return
  the address of a static variable containing the value.

  Do not define this if the usual system convention is for the caller to
  pass an address to the subroutine.

  This macro has effect in :option:`-fpcc-struct-return` mode, but it does
  nothing when you use :option:`-freg-struct-return` mode.

.. function:: fixed_size_mode TARGET_GET_RAW_RESULT_MODE (int regno)

  This target hook returns the mode to be used when accessing raw return
  registers in ``__builtin_return``.  Define this macro if the value
  in :samp:`{reg_raw_mode}` is not correct.

.. function:: fixed_size_mode TARGET_GET_RAW_ARG_MODE (int regno)

  This target hook returns the mode to be used when accessing raw argument
  registers in ``__builtin_apply_args``.  Define this macro if the value
  in :samp:`{reg_raw_mode}` is not correct.

.. function:: bool TARGET_EMPTY_RECORD_P (const_tree type)

  This target hook returns true if the type is an empty record.  The default
  is to return ``false``.

.. function:: void TARGET_WARN_PARAMETER_PASSING_ABI (cumulative_args_t ca, tree type)

  This target hook warns about the change in empty class parameter passing
  ABI.

.. _caller-saves:

Caller-Saves Register Allocation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you enable it, GCC can save registers around function calls.  This
makes it possible to use call-clobbered registers to hold variables that
must live across calls.

.. c:macro:: HARD_REGNO_CALLER_SAVE_MODE (regno, nregs)

  A C expression specifying which mode is required for saving :samp:`{nregs}`
  of a pseudo-register in call-clobbered hard register :samp:`{regno}`.  If
  :samp:`{regno}` is unsuitable for caller save, ``VOIDmode`` should be
  returned.  For most machines this macro need not be defined since GCC
  will select the smallest suitable mode.

.. _function-entry:

Function Entry and Exit
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: function entry and exit

.. index:: prologue

.. index:: epilogue

This section describes the macros that output function entry
(:dfn:`prologue`) and exit (:dfn:`epilogue`) code.

.. function:: void TARGET_ASM_PRINT_PATCHABLE_FUNCTION_ENTRY (FILE *file, unsigned HOST_WIDE_INT patch_area_size, bool record_p)

  Generate a patchable area at the function start, consisting of
  :samp:`{patch_area_size}` NOP instructions.  If the target supports named
  sections and if :samp:`{record_p}` is true, insert a pointer to the current
  location in the table of patchable functions.  The default implementation
  of the hook places the table of pointers in the special section named
  ``__patchable_function_entries``.

.. function:: void TARGET_ASM_FUNCTION_PROLOGUE (FILE *file)

  If defined, a function that outputs the assembler code for entry to a
  function.  The prologue is responsible for setting up the stack frame,
  initializing the frame pointer register, saving registers that must be
  saved, and allocating :samp:`{size}` additional bytes of storage for the
  local variables.  :samp:`{file}` is a stdio stream to which the assembler
  code should be output.

  The label for the beginning of the function need not be output by this
  macro.  That has already been done when the macro is run.

  .. index:: regs_ever_live

  To determine which registers to save, the macro can refer to the array
  ``regs_ever_live`` : element :samp:`{r}` is nonzero if hard register
  :samp:`{r}` is used anywhere within the function.  This implies the function
  prologue should save register :samp:`{r}`, provided it is not one of the
  call-used registers.  ( ``TARGET_ASM_FUNCTION_EPILOGUE`` must likewise use
  ``regs_ever_live``.)

  On machines that have 'register windows', the function entry code does
  not save on the stack the registers that are in the windows, even if
  they are supposed to be preserved by function calls; instead it takes
  appropriate steps to 'push' the register stack, if any non-call-used
  registers are used in the function.

  .. index:: frame_pointer_needed

  On machines where functions may or may not have frame-pointers, the
  function entry code must vary accordingly; it must set up the frame
  pointer if one is wanted, and not otherwise.  To determine whether a
  frame pointer is in wanted, the macro can refer to the variable
  ``frame_pointer_needed``.  The variable's value will be 1 at run
  time in a function that needs a frame pointer.  See :ref:`elimination`.

  The function entry code is responsible for allocating any stack space
  required for the function.  This stack space consists of the regions
  listed below.  In most cases, these regions are allocated in the
  order listed, with the last listed region closest to the top of the
  stack (the lowest address if ``STACK_GROWS_DOWNWARD`` is defined, and
  the highest address if it is not defined).  You can use a different order
  for a machine if doing so is more convenient or required for
  compatibility reasons.  Except in cases where required by standard
  or by a debugger, there is no reason why the stack layout used by GCC
  need agree with that used by other compilers for a machine.

.. function:: void TARGET_ASM_FUNCTION_END_PROLOGUE (FILE *file)

  If defined, a function that outputs assembler code at the end of a
  prologue.  This should be used when the function prologue is being
  emitted as RTL, and you have some extra assembler that needs to be
  emitted.  See :ref:`prologue-instruction-pattern`.

.. function:: void TARGET_ASM_FUNCTION_BEGIN_EPILOGUE (FILE *file)

  If defined, a function that outputs assembler code at the start of an
  epilogue.  This should be used when the function epilogue is being
  emitted as RTL, and you have some extra assembler that needs to be
  emitted.  See :ref:`epilogue-instruction-pattern`.

.. function:: void TARGET_ASM_FUNCTION_EPILOGUE (FILE *file)

  If defined, a function that outputs the assembler code for exit from a
  function.  The epilogue is responsible for restoring the saved
  registers and stack pointer to their values when the function was
  called, and returning control to the caller.  This macro takes the
  same argument as the macro ``TARGET_ASM_FUNCTION_PROLOGUE``, and the
  registers to restore are determined from ``regs_ever_live`` and
  ``CALL_USED_REGISTERS`` in the same way.

  On some machines, there is a single instruction that does all the work
  of returning from the function.  On these machines, give that
  instruction the name :samp:`return` and do not define the macro
  ``TARGET_ASM_FUNCTION_EPILOGUE`` at all.

  Do not define a pattern named :samp:`return` if you want the
  ``TARGET_ASM_FUNCTION_EPILOGUE`` to be used.  If you want the target
  switches to control whether return instructions or epilogues are used,
  define a :samp:`return` pattern with a validity condition that tests the
  target switches appropriately.  If the :samp:`return` pattern's validity
  condition is false, epilogues will be used.

  On machines where functions may or may not have frame-pointers, the
  function exit code must vary accordingly.  Sometimes the code for these
  two cases is completely different.  To determine whether a frame pointer
  is wanted, the macro can refer to the variable
  ``frame_pointer_needed``.  The variable's value will be 1 when compiling
  a function that needs a frame pointer.

  Normally, ``TARGET_ASM_FUNCTION_PROLOGUE`` and
  ``TARGET_ASM_FUNCTION_EPILOGUE`` must treat leaf functions specially.
  The C variable ``current_function_is_leaf`` is nonzero for such a
  function.  See :ref:`leaf-functions`.

  On some machines, some functions pop their arguments on exit while
  others leave that for the caller to do.  For example, the 68020 when
  given :option:`-mrtd` pops arguments in functions that take a fixed
  number of arguments.

  .. index:: pops_args

  .. index:: crtl->args.pops_args

  Your definition of the macro ``RETURN_POPS_ARGS`` decides which
  functions pop their own arguments.  ``TARGET_ASM_FUNCTION_EPILOGUE``
  needs to know what was decided.  The number of bytes of the current
  function's arguments that this function should pop is available in
  ``crtl->args.pops_args``.  See :ref:`scalar-return`.

* 
  .. index:: pretend_args_size

  .. index:: crtl->args.pretend_args_size

  A region of ``crtl->args.pretend_args_size`` bytes of
  uninitialized space just underneath the first argument arriving on the
  stack.  (This may not be at the very start of the allocated stack region
  if the calling sequence has pushed anything else since pushing the stack
  arguments.  But usually, on such machines, nothing else has been pushed
  yet, because the function prologue itself does all the pushing.)  This
  region is used on machines where an argument may be passed partly in
  registers and partly in memory, and, in some cases to support the
  features in ``<stdarg.h>``.

* An area of memory used to save certain registers used by the function.
  The size of this area, which may also include space for such things as
  the return address and pointers to previous stack frames, is
  machine-specific and usually depends on which registers have been used
  in the function.  Machines with register windows often do not require
  a save area.

* A region of at least :samp:`{size}` bytes, possibly rounded up to an allocation
  boundary, to contain the local variables of the function.  On some machines,
  this region and the save area may occur in the opposite order, with the
  save area closer to the top of the stack.

* 
  .. index:: ACCUMULATE_OUTGOING_ARGS and stack frames

  Optionally, when ``ACCUMULATE_OUTGOING_ARGS`` is defined, a region of
  ``crtl->outgoing_args_size`` bytes to be used for outgoing
  argument lists of the function.  See :ref:`stack-arguments`.

.. c:macro:: EXIT_IGNORE_STACK

  Define this macro as a C expression that is nonzero if the return
  instruction or the function epilogue ignores the value of the stack
  pointer; in other words, if it is safe to delete an instruction to
  adjust the stack pointer before a return from the function.  The
  default is 0.

  Note that this macro's value is relevant only for functions for which
  frame pointers are maintained.  It is never safe to delete a final
  stack adjustment in a function that has no frame pointer, and the
  compiler knows this regardless of ``EXIT_IGNORE_STACK``.

.. c:macro:: EPILOGUE_USES (regno)

  Define this macro as a C expression that is nonzero for registers that are
  used by the epilogue or the :samp:`return` pattern.  The stack and frame
  pointer registers are already assumed to be used as needed.

.. c:macro:: EH_USES (regno)

  Define this macro as a C expression that is nonzero for registers that are
  used by the exception handling mechanism, and so should be considered live
  on entry to an exception edge.

.. function:: void TARGET_ASM_OUTPUT_MI_THUNK (FILE *file, tree thunk_fndecl, HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset, tree function)

  A function that outputs the assembler code for a thunk
  function, used to implement C++ virtual function calls with multiple
  inheritance.  The thunk acts as a wrapper around a virtual function,
  adjusting the implicit object parameter before handing control off to
  the real function.

  First, emit code to add the integer :samp:`{delta}` to the location that
  contains the incoming first argument.  Assume that this argument
  contains a pointer, and is the one used to pass the ``this`` pointer
  in C++.  This is the incoming argument *before* the function prologue,
  e.g. :samp:`%o0` on a sparc.  The addition must preserve the values of
  all other incoming arguments.

  Then, if :samp:`{vcall_offset}` is nonzero, an additional adjustment should be
  made after adding ``delta``.  In particular, if :samp:`{p}` is the
  adjusted pointer, the following adjustment should be made:

  .. code-block:: c++

    p += (*((ptrdiff_t **)p))[vcall_offset/sizeof(ptrdiff_t)]

  After the additions, emit code to jump to :samp:`{function}`, which is a
  ``FUNCTION_DECL``.  This is a direct pure jump, not a call, and does
  not touch the return address.  Hence returning from :samp:`{FUNCTION}` will
  return to whoever called the current :samp:`thunk`.

  The effect must be as if :samp:`{function}` had been called directly with
  the adjusted first argument.  This macro is responsible for emitting all
  of the code for a thunk function; ``TARGET_ASM_FUNCTION_PROLOGUE``
  and ``TARGET_ASM_FUNCTION_EPILOGUE`` are not invoked.

  The :samp:`{thunk_fndecl}` is redundant.  ( :samp:`{delta}` and :samp:`{function}`
  have already been extracted from it.)  It might possibly be useful on
  some targets, but probably not.

  If you do not define this macro, the target-independent code in the C++
  front end will generate a less efficient heavyweight thunk that calls
  :samp:`{function}` instead of jumping to it.  The generic approach does
  not support varargs.

.. function:: bool TARGET_ASM_CAN_OUTPUT_MI_THUNK (const_tree thunk_fndecl, HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset, const_tree function)

  A function that returns true if TARGET_ASM_OUTPUT_MI_THUNK would be able
  to output the assembler code for the thunk function specified by the
  arguments it is passed, and false otherwise.  In the latter case, the
  generic approach will be used by the C++ front end, with the limitations
  previously exposed.

.. _profiling:

Generating Code for Profiling
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: profiling, code generation

These macros will help you generate code for profiling.

.. c:macro:: FUNCTION_PROFILER (file, labelno)

  A C statement or compound statement to output to :samp:`{file}` some
  assembler code to call the profiling subroutine ``mcount``.

  .. index:: mcount

  The details of how ``mcount`` expects to be called are determined by
  your operating system environment, not by GCC.  To figure them out,
  compile a small program for profiling using the system's installed C
  compiler and look at the assembler code that results.

  Older implementations of ``mcount`` expect the address of a counter
  variable to be loaded into some register.  The name of this variable is
  :samp:`LP` followed by the number :samp:`{labelno}`, so you would generate
  the name using :samp:`LP%d` in a ``fprintf``.

.. c:macro:: PROFILE_HOOK

  A C statement or compound statement to output to :samp:`{file}` some assembly
  code to call the profiling subroutine ``mcount`` even the target does
  not support profiling.

.. c:macro:: NO_PROFILE_COUNTERS

  Define this macro to be an expression with a nonzero value if the
  ``mcount`` subroutine on your system does not need a counter variable
  allocated for each function.  This is true for almost all modern
  implementations.  If you define this macro, you must not use the
  :samp:`{labelno}` argument to ``FUNCTION_PROFILER``.

.. c:macro:: PROFILE_BEFORE_PROLOGUE

  Define this macro if the code for function profiling should come before
  the function prologue.  Normally, the profiling code comes after.

.. function:: bool TARGET_KEEP_LEAF_WHEN_PROFILED (void)

  This target hook returns true if the target wants the leaf flag for
  the current function to stay true even if it calls mcount.  This might
  make sense for targets using the leaf flag only to determine whether a
  stack frame needs to be generated or not and for which the call to
  mcount is generated before the function prologue.

.. _tail-calls:

Permitting tail calls
^^^^^^^^^^^^^^^^^^^^^

.. index:: tail calls

.. function:: bool TARGET_FUNCTION_OK_FOR_SIBCALL (tree decl, tree exp)

  True if it is OK to do sibling call optimization for the specified
  call expression :samp:`{exp}`.  :samp:`{decl}` will be the called function,
  or ``NULL`` if this is an indirect call.

  It is not uncommon for limitations of calling conventions to prevent
  tail calls to functions outside the current unit of translation, or
  during PIC compilation.  The hook is used to enforce these restrictions,
  as the ``sibcall`` md pattern cannot fail, or fall over to a
  'normal' call.  The criteria for successful sibling call optimization
  may vary greatly between different architectures.

.. function:: void TARGET_EXTRA_LIVE_ON_ENTRY (bitmap regs)

  Add any hard registers to :samp:`{regs}` that are live on entry to the
  function.  This hook only needs to be defined to provide registers that
  cannot be found by examination of FUNCTION_ARG_REGNO_P, the callee saved
  registers, STATIC_CHAIN_INCOMING_REGNUM, STATIC_CHAIN_REGNUM,
  TARGET_STRUCT_VALUE_RTX, FRAME_POINTER_REGNUM, EH_USES,
  FRAME_POINTER_REGNUM, ARG_POINTER_REGNUM, and the PIC_OFFSET_TABLE_REGNUM.

.. function:: void TARGET_SET_UP_BY_PROLOGUE (struct hard_reg_set_container *)

  This hook should add additional registers that are computed by the prologue
  to the hard regset for shrink-wrapping optimization purposes.

.. function:: bool TARGET_WARN_FUNC_RETURN (tree)

  True if a function's return statements should be checked for matching
  the function's return type.  This includes checking for falling off the end
  of a non-void function.  Return false if no such check should be made.

.. _shrink-wrapping-separate-components:

Shrink-wrapping separate components
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: shrink-wrapping separate components

The prologue may perform a variety of target dependent tasks such as
saving callee-saved registers, saving the return address, aligning the
stack, creating a stack frame, initializing the PIC register, setting
up the static chain, etc.

On some targets some of these tasks may be independent of others and
thus may be shrink-wrapped separately.  These independent tasks are
referred to as components and are handled generically by the target
independent parts of GCC.

Using the following hooks those prologue or epilogue components can be
shrink-wrapped separately, so that the initialization (and possibly
teardown) those components do is not done as frequently on execution
paths where this would unnecessary.

What exactly those components are is up to the target code; the generic
code treats them abstractly, as a bit in an ``sbitmap``.  These
``sbitmap`` s are allocated by the ``shrink_wrap.get_separate_components``
and ``shrink_wrap.components_for_bb`` hooks, and deallocated by the
generic code.

.. function:: sbitmap TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS (void)

  This hook should return an ``sbitmap`` with the bits set for those
  components that can be separately shrink-wrapped in the current function.
  Return ``NULL`` if the current function should not get any separate
  shrink-wrapping.
  Don't define this hook if it would always return ``NULL``.
  If it is defined, the other hooks in this group have to be defined as well.

.. function:: sbitmap TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB (basic_block)

  This hook should return an ``sbitmap`` with the bits set for those
  components where either the prologue component has to be executed before
  the ``basic_block``, or the epilogue component after it, or both.

.. function:: void TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS (sbitmap components, edge e, sbitmap edge_components, bool is_prologue)

  This hook should clear the bits in the :samp:`{components}` bitmap for those
  components in :samp:`{edge_components}` that the target cannot handle on edge
  :samp:`{e}`, where :samp:`{is_prologue}` says if this is for a prologue or an
  epilogue instead.

.. function:: void TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS (sbitmap)

  Emit prologue insns for the components indicated by the parameter.

.. function:: void TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS (sbitmap)

  Emit epilogue insns for the components indicated by the parameter.

.. function:: void TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS (sbitmap)

  Mark the components in the parameter as handled, so that the
  ``prologue`` and ``epilogue`` named patterns know to ignore those
  components.  The target code should not hang on to the ``sbitmap``, it
  will be deleted after this call.

.. _stack-smashing-protection:

Stack smashing protection
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: stack smashing protection

.. function:: tree TARGET_STACK_PROTECT_GUARD (void)

  This hook returns a ``DECL`` node for the external variable to use
  for the stack protection guard.  This variable is initialized by the
  runtime to some random value and is used to initialize the guard value
  that is placed at the top of the local stack frame.  The type of this
  variable must be ``ptr_type_node``.

  The default version of this hook creates a variable called
  :samp:`__stack_chk_guard`, which is normally defined in libgcc2.c.

.. function:: tree TARGET_STACK_PROTECT_FAIL (void)

  This hook returns a ``CALL_EXPR`` that alerts the runtime that the
  stack protect guard variable has been modified.  This expression should
  involve a call to a ``noreturn`` function.

  The default version of this hook invokes a function called
  :samp:`__stack_chk_fail`, taking no arguments.  This function is
  normally defined in libgcc2.c.

.. function:: bool TARGET_STACK_PROTECT_RUNTIME_ENABLED_P (void)

  Returns true if the target wants GCC's default stack protect runtime support,
  otherwise return false.  The default implementation always returns true.

.. function:: bool TARGET_SUPPORTS_SPLIT_STACK (bool report, struct gcc_options *opts)

  Whether this target supports splitting the stack when the options
  described in :samp:`{opts}` have been passed.  This is called
  after options have been parsed, so the target may reject splitting
  the stack in some configurations.  The default version of this hook
  returns false.  If :samp:`{report}` is true, this function may issue a warning
  or error; if :samp:`{report}` is false, it must simply return a value

.. function:: vec<const char *> TARGET_GET_VALID_OPTION_VALUES (int option_code, const char *prefix)

  The hook is used for options that have a non-trivial list of
  possible option values.  OPTION_CODE is option code of opt_code
  enum type.  PREFIX is used for bash completion and allows an implementation
  to return more specific completion based on the prefix.  All string values
  should be allocated from heap memory and consumers should release them.
  The result will be pruned to cases with PREFIX if not NULL.

.. _miscellaneous-register-hooks:

Miscellaneous register hooks
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: miscellaneous register hooks

.. c:var:: bool TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS

  Set to true if each call that binds to a local definition explicitly
  clobbers or sets all non-fixed registers modified by performing the call.
  That is, by the call pattern itself, or by code that might be inserted by the
  linker (e.g. stubs, veneers, branch islands), but not including those
  modifiable by the callee.  The affected registers may be mentioned explicitly
  in the call pattern, or included as clobbers in CALL_INSN_FUNCTION_USAGE.
  The default version of this hook is set to false.  The purpose of this hook
  is to enable the fipa-ra optimization.

