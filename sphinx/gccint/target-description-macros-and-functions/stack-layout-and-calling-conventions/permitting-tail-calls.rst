..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

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

