.. _tuple-specific-accessors:

Tuple specific accessors
************************

.. index:: Tuple specific accessors

.. toctree::

   <gimple_asm>
   <gimple_assign>
   <gimple_bind>
   <gimple_call>
   <gimple_catch>
   <gimple_cond>
   <gimple_debug>
   <gimple_eh_filter>
   <gimple_label>
   <gimple_goto>
   <gimple_nop>
   <gimple_omp_atomic_load>
   <gimple_omp_atomic_store>
   <gimple_omp_continue>
   <gimple_omp_critical>
   <gimple_omp_for>
   <gimple_omp_master>
   <gimple_omp_ordered>
   <gimple_omp_parallel>
   <gimple_omp_return>
   <gimple_omp_section>
   <gimple_omp_sections>
   <gimple_omp_single>
   <gimple_phi>
   <gimple_resx>
   <gimple_return>
   <gimple_switch>
   <gimple_try>
   <gimple_with_cleanup_expr>

``GIMPLE_ASM``
^^^^^^^^^^^^^^

.. index:: GIMPLE_ASM

.. function:: gasm *gimple_build_asm_vec(const char*string ,vec<tree ,va_gc> *inputs,vec<tree ,va_gc> *outputs,vec<tree ,va_gc> *clobbers,vec<tree ,va_gc> *labels)

  Build a ``GIMPLE_ASM`` statement.  This statement is used for
  building in-line assembly constructs.  ``STRING`` is the assembly
  code.  ``INPUTS``, ``OUTPUTS``, ``CLOBBERS``  and ``LABELS``
  are the inputs, outputs, clobbered registers and labels.

.. function:: unsigned gimple_asm_ninputs(const gasm*g )

  Return the number of input operands for ``GIMPLE_ASM`` ``G``.

.. function:: unsigned gimple_asm_noutputs(const gasm*g )

  Return the number of output operands for ``GIMPLE_ASM`` ``G``.

.. function:: unsigned gimple_asm_nclobbers(const gasm*g )

  Return the number of clobber operands for ``GIMPLE_ASM`` ``G``.

.. function:: tree gimple_asm_input_op(const gasm*g ,unsigned index)

  Return input operand ``INDEX`` of ``GIMPLE_ASM`` ``G``.

.. function:: void gimple_asm_set_input_op(gasm *g,unsigned index,tree in_op)

  Set ``IN_OP`` to be input operand ``INDEX`` in ``GIMPLE_ASM`` ``G``.

.. function:: tree gimple_asm_output_op(const gasm*g ,unsigned index)

  Return output operand ``INDEX`` of ``GIMPLE_ASM`` ``G``.

.. function:: void gimple_asm_set_output_op(gasm *g,unsigned index,tree out_op)

  Set ``OUT_OP`` to be output operand ``INDEX`` in ``GIMPLE_ASM`` ``G``.

.. function:: tree gimple_asm_clobber_op(const gasm*g ,unsigned index)

  Return clobber operand ``INDEX`` of ``GIMPLE_ASM`` ``G``.

.. function:: void gimple_asm_set_clobber_op(gasm *g,unsigned index,tree clobber_op)

  Set ``CLOBBER_OP`` to be clobber operand ``INDEX`` in ``GIMPLE_ASM`` ``G``.

.. function:: const char * gimple_asm_string(const gasm*g )

  Return the string representing the assembly instruction in
  ``GIMPLE_ASM`` ``G``.

.. function:: bool gimple_asm_volatile_p(const gasm*g )

  Return true if ``G`` is an asm statement marked volatile.

.. function:: void gimple_asm_set_volatile(gasm *g,bool volatile_p)

  Mark asm statement ``G`` as volatile or non-volatile based on
  ``VOLATILE_P``.

``GIMPLE_ASSIGN``
^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_ASSIGN

.. function:: gassign *gimple_build_assign(tree lhs,tree rhs)

  Build a ``GIMPLE_ASSIGN`` statement.  The left-hand side is an lvalue
  passed in lhs.  The right-hand side can be either a unary or
  binary tree expression.  The expression tree rhs will be
  flattened and its operands assigned to the corresponding operand
  slots in the new statement.  This function is useful when you
  already have a tree expression that you want to convert into a
  tuple.  However, try to avoid building expression trees for the
  sole purpose of calling this function.  If you already have the
  operands in separate trees, it is better to use
  ``gimple_build_assign`` with ``enum tree_code`` argument and separate
  arguments for each operand.

.. function:: gassign *gimple_build_assign(tree lhs,enum tree_codesubcode ,tree op1,tree op2,tree op3)

  This function is similar to two operand ``gimple_build_assign``,
  but is used to build a ``GIMPLE_ASSIGN`` statement when the operands of the
  right-hand side of the assignment are already split into
  different operands.

  The left-hand side is an lvalue passed in lhs.  Subcode is the
  ``tree_code`` for the right-hand side of the assignment.  Op1, op2 and op3
  are the operands.

.. function:: gassign *gimple_build_assign(tree lhs,enum tree_codesubcode ,tree op1,tree op2)

  Like the above 5 operand ``gimple_build_assign``, but with the last
  argument ``NULL`` - this overload should not be used for
  ``GIMPLE_TERNARY_RHS`` assignments.

.. function:: gassign *gimple_build_assign(tree lhs,enum tree_codesubcode ,tree op1)

  Like the above 4 operand ``gimple_build_assign``, but with the last
  argument ``NULL`` - this overload should be used only for
  ``GIMPLE_UNARY_RHS`` and ``GIMPLE_SINGLE_RHS`` assignments.

.. function:: gimple gimplify_assign(tree dst,tree src,gimple_seq *seq_p)

  Build a new ``GIMPLE_ASSIGN`` tuple and append it to the end of
  ``*SEQ_P``.

``DST`` / ``SRC`` are the destination and source respectively.  You can
pass ungimplified trees in ``DST`` or ``SRC``, in which
case they will be converted to a gimple operand if necessary.

This function returns the newly created ``GIMPLE_ASSIGN`` tuple.

.. function:: enum tree_code gimple_assign_rhs_code(gimple g)

  Return the code of the expression computed on the ``RHS`` of
  assignment statement ``G``.

.. function:: enum gimple_rhs_class gimple_assign_rhs_class(gimple g)

  Return the gimple rhs class of the code for the expression
  computed on the rhs of assignment statement ``G``.  This will never
  return ``GIMPLE_INVALID_RHS``.

.. function:: tree gimple_assign_lhs(gimple g)

  Return the ``LHS`` of assignment statement ``G``.

.. function:: tree * gimple_assign_lhs_ptr(gimple g)

  Return a pointer to the ``LHS`` of assignment statement ``G``.

.. function:: tree gimple_assign_rhs1(gimple g)

  Return the first operand on the ``RHS`` of assignment statement ``G``.

.. function:: tree * gimple_assign_rhs1_ptr(gimple g)

  Return the address of the first operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: tree gimple_assign_rhs2(gimple g)

  Return the second operand on the ``RHS`` of assignment statement ``G``.

.. function:: tree * gimple_assign_rhs2_ptr(gimple g)

  Return the address of the second operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: tree gimple_assign_rhs3(gimple g)

  Return the third operand on the ``RHS`` of assignment statement ``G``.

.. function:: tree * gimple_assign_rhs3_ptr(gimple g)

  Return the address of the third operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: void gimple_assign_set_lhs(gimple g,tree lhs)

  Set ``LHS`` to be the ``LHS`` operand of assignment statement ``G``.

.. function:: void gimple_assign_set_rhs1(gimple g,tree rhs)

  Set ``RHS`` to be the first operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: void gimple_assign_set_rhs2(gimple g,tree rhs)

  Set ``RHS`` to be the second operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: void gimple_assign_set_rhs3(gimple g,tree rhs)

  Set ``RHS`` to be the third operand on the ``RHS`` of assignment
  statement ``G``.

.. function:: bool gimple_assign_cast_p(const_gimple s)

  Return true if ``S`` is a type-cast assignment.

``GIMPLE_BIND``
^^^^^^^^^^^^^^^

.. index:: GIMPLE_BIND

.. function:: gbind *gimple_build_bind(tree vars,gimple_seq body)

  Build a ``GIMPLE_BIND`` statement with a list of variables in ``VARS``
  and a body of statements in sequence ``BODY``.

.. function:: tree gimple_bind_vars(const gbind*g )

  Return the variables declared in the ``GIMPLE_BIND`` statement ``G``.

.. function:: void gimple_bind_set_vars(gbind *g,tree vars)

  Set ``VARS`` to be the set of variables declared in the ``GIMPLE_BIND``
  statement ``G``.

.. function:: void gimple_bind_append_vars(gbind *g,tree vars)

  Append ``VARS`` to the set of variables declared in the ``GIMPLE_BIND``
  statement ``G``.

.. function:: gimple_seq gimple_bind_body(gbind *g)

  Return the GIMPLE sequence contained in the ``GIMPLE_BIND`` statement
  ``G``.

.. function:: void gimple_bind_set_body(gbind *g,gimple_seq seq)

  Set ``SEQ`` to be sequence contained in the ``GIMPLE_BIND`` statement ``G``.

.. function:: void gimple_bind_add_stmt(gbind *gs,gimple stmt)

  Append a statement to the end of a ``GIMPLE_BIND`` 's body.

.. function:: void gimple_bind_add_seq(gbind *gs,gimple_seq seq)

  Append a sequence of statements to the end of a ``GIMPLE_BIND`` 's
  body.

.. function:: tree gimple_bind_block(const gbind*g )

  Return the ``TREE_BLOCK`` node associated with ``GIMPLE_BIND`` statement
  ``G``. This is analogous to the ``BIND_EXPR_BLOCK`` field in trees.

.. function:: void gimple_bind_set_block(gbind *g,tree block)

  Set ``BLOCK`` to be the ``TREE_BLOCK`` node associated with ``GIMPLE_BIND``
  statement ``G``.

``GIMPLE_CALL``
^^^^^^^^^^^^^^^

.. index:: GIMPLE_CALL

.. function:: gcall *gimple_build_call(tree fn,unsigned nargs,... )

  Build a ``GIMPLE_CALL`` statement to function ``FN``.  The argument ``FN``
  must be either a ``FUNCTION_DECL`` or a gimple call address as
  determined by ``is_gimple_call_addr``.  ``NARGS`` are the number of
  arguments.  The rest of the arguments follow the argument ``NARGS``,
  and must be trees that are valid as rvalues in gimple (i.e., each
  operand is validated with ``is_gimple_operand`` ).

.. function:: gcall *gimple_build_call_from_tree(tree call_expr,tree fnptrtype)

  Build a ``GIMPLE_CALL`` from a ``CALL_EXPR`` node.  The arguments
  and the function are taken from the expression directly.  The type of the
  ``GIMPLE_CALL`` is set from the second parameter passed by a caller.
  This routine assumes that ``call_expr`` is already in GIMPLE form.
  That is, its operands are GIMPLE values and the function call needs no further
  simplification.  All the call flags in ``call_expr`` are copied over
  to the new ``GIMPLE_CALL``.

.. function:: gcall *gimple_build_call_vec(tree fn,vec<tree> args)

  Identical to ``gimple_build_call`` but the arguments are stored in a
  ``vec<tree>``.

.. function:: tree gimple_call_lhs(gimple g)

  Return the ``LHS`` of call statement ``G``.

.. function:: tree * gimple_call_lhs_ptr(gimple g)

  Return a pointer to the ``LHS`` of call statement ``G``.

.. function:: void gimple_call_set_lhs(gimple g,tree lhs)

  Set ``LHS`` to be the ``LHS`` operand of call statement ``G``.

.. function:: tree gimple_call_fn(gimple g)

  Return the tree node representing the function called by call
  statement ``G``.

.. function:: void gimple_call_set_fn(gcall *g,tree fn)

  Set ``FN`` to be the function called by call statement ``G``.  This has
  to be a gimple value specifying the address of the called
  function.

.. function:: tree gimple_call_fndecl(gimple g)

  If a given ``GIMPLE_CALL`` 's callee is a ``FUNCTION_DECL``, return it.
  Otherwise return ``NULL``.  This function is analogous to
  ``get_callee_fndecl`` in ``GENERIC``.

.. function:: tree gimple_call_set_fndecl(gimple g,tree fndecl)

  Set the called function to ``FNDECL``.

.. function:: tree gimple_call_return_type(const gcall*g )

  Return the type returned by call statement ``G``.

.. function:: tree gimple_call_chain(gimple g)

  Return the static chain for call statement ``G``.

.. function:: void gimple_call_set_chain(gcall *g,tree chain)

  Set ``CHAIN`` to be the static chain for call statement ``G``.

.. function:: unsigned gimple_call_num_args(gimple g)

  Return the number of arguments used by call statement ``G``.

.. function:: tree gimple_call_arg(gimple g,unsigned index)

  Return the argument at position ``INDEX`` for call statement ``G``.  The
  first argument is 0.

.. function:: tree * gimple_call_arg_ptr(gimple g,unsigned index)

  Return a pointer to the argument at position ``INDEX`` for call
  statement ``G``.

.. function:: void gimple_call_set_arg(gimple g,unsigned index,tree arg)

  Set ``ARG`` to be the argument at position ``INDEX`` for call statement
  ``G``.

.. function:: void gimple_call_set_tail(gcall *s)

  Mark call statement ``S`` as being a tail call (i.e., a call just
  before the exit of a function). These calls are candidate for
  tail call optimization.

.. function:: bool gimple_call_tail_p(gcall *s)

  Return true if ``GIMPLE_CALL`` ``S`` is marked as a tail call.

.. function:: bool gimple_call_noreturn_p(gimple s)

  Return true if ``S`` is a noreturn call.

.. function:: gimple gimple_call_copy_skip_args(gcall *stmt,bitmap args_to_skip)

  Build a ``GIMPLE_CALL`` identical to ``STMT`` but skipping the arguments
  in the positions marked by the set ``ARGS_TO_SKIP``.

``GIMPLE_CATCH``
^^^^^^^^^^^^^^^^

.. index:: GIMPLE_CATCH

.. function:: gcatch *gimple_build_catch(tree types,gimple_seq handler)

  Build a ``GIMPLE_CATCH`` statement.  ``TYPES`` are the tree types this
  catch handles.  ``HANDLER`` is a sequence of statements with the code
  for the handler.

.. function:: tree gimple_catch_types(const gcatch*g )

  Return the types handled by ``GIMPLE_CATCH`` statement ``G``.

.. function:: tree * gimple_catch_types_ptr(gcatch *g)

  Return a pointer to the types handled by ``GIMPLE_CATCH`` statement
  ``G``.

.. function:: gimple_seq gimple_catch_handler(gcatch *g)

  Return the GIMPLE sequence representing the body of the handler
  of ``GIMPLE_CATCH`` statement ``G``.

.. function:: void gimple_catch_set_types(gcatch *g,tree t)

  Set ``T`` to be the set of types handled by ``GIMPLE_CATCH`` ``G``.

.. function:: void gimple_catch_set_handler(gcatch *g,gimple_seq handler)

  Set ``HANDLER`` to be the body of ``GIMPLE_CATCH`` ``G``.

``GIMPLE_COND``
^^^^^^^^^^^^^^^

.. index:: GIMPLE_COND

.. function:: gcond *gimple_build_cond(enum tree_codepred_code ,tree lhs,tree rhs,tree t_label,tree f_label)

  Build a ``GIMPLE_COND`` statement.  ``A`` ``GIMPLE_COND`` statement compares
  ``LHS`` and ``RHS`` and if the condition in ``PRED_CODE`` is true, jump to
  the label in ``t_label``, otherwise jump to the label in ``f_label``.
  ``PRED_CODE`` are relational operator tree codes like ``EQ_EXPR``,
  ``LT_EXPR``, ``LE_EXPR``, ``NE_EXPR``, etc.

.. function:: gcond *gimple_build_cond_from_tree(tree cond,tree t_label,tree f_label)

  Build a ``GIMPLE_COND`` statement from the conditional expression
  tree ``COND``.  ``T_LABEL`` and ``F_LABEL`` are as in ``gimple_build_cond``.

.. function:: enum tree_code gimple_cond_code(gimple g)

  Return the code of the predicate computed by conditional
  statement ``G``.

.. function:: void gimple_cond_set_code(gcond *g,enum tree_codecode )

  Set ``CODE`` to be the predicate code for the conditional statement
  ``G``.

.. function:: tree gimple_cond_lhs(gimple g)

  Return the ``LHS`` of the predicate computed by conditional statement
  ``G``.

.. function:: void gimple_cond_set_lhs(gcond *g,tree lhs)

  Set ``LHS`` to be the ``LHS`` operand of the predicate computed by
  conditional statement ``G``.

.. function:: tree gimple_cond_rhs(gimple g)

  Return the ``RHS`` operand of the predicate computed by conditional
  ``G``.

.. function:: void gimple_cond_set_rhs(gcond *g,tree rhs)

  Set ``RHS`` to be the ``RHS`` operand of the predicate computed by
  conditional statement ``G``.

.. function:: tree gimple_cond_true_label(const gcond*g )

  Return the label used by conditional statement ``G`` when its
  predicate evaluates to true.

.. function:: void gimple_cond_set_true_label(gcond *g,tree label)

  Set ``LABEL`` to be the label used by conditional statement ``G`` when
  its predicate evaluates to true.

.. function:: void gimple_cond_set_false_label(gcond *g,tree label)

  Set ``LABEL`` to be the label used by conditional statement ``G`` when
  its predicate evaluates to false.

.. function:: tree gimple_cond_false_label(const gcond*g )

  Return the label used by conditional statement ``G`` when its
  predicate evaluates to false.

.. function:: void gimple_cond_make_false(gcond *g)

  Set the conditional ``COND_STMT`` to be of the form 'if (1 == 0)'.

.. function:: void gimple_cond_make_true(gcond *g)

  Set the conditional ``COND_STMT`` to be of the form 'if (1 == 1)'.

``GIMPLE_DEBUG``
^^^^^^^^^^^^^^^^

.. index:: GIMPLE_DEBUG

.. index:: GIMPLE_DEBUG_BIND

.. index:: GIMPLE_DEBUG_BEGIN_STMT

.. index:: GIMPLE_DEBUG_INLINE_ENTRY

.. function:: gdebug *gimple_build_debug_bind(tree var,tree value,gimple stmt)

  Build a ``GIMPLE_DEBUG`` statement with ``GIMPLE_DEBUG_BIND``
  ``subcode``.  The effect of this statement is to tell debug
  information generation machinery that the value of user variable
  ``var`` is given by ``value`` at that point, and to remain with
  that value until ``var`` runs out of scope, a
  dynamically-subsequent debug bind statement overrides the binding, or
  conflicting values reach a control flow merge point.  Even if
  components of the ``value`` expression change afterwards, the
  variable is supposed to retain the same value, though not necessarily
  the same location.

  It is expected that ``var`` be most often a tree for automatic user
  variables ( ``VAR_DECL`` or ``PARM_DECL`` ) that satisfy the
  requirements for gimple registers, but it may also be a tree for a
  scalarized component of a user variable ( ``ARRAY_REF``,
  ``COMPONENT_REF`` ), or a debug temporary ( ``DEBUG_EXPR_DECL`` ).

  As for ``value``, it can be an arbitrary tree expression, but it is
  recommended that it be in a suitable form for a gimple assignment
  ``RHS``.  It is not expected that user variables that could appear
  as ``var`` ever appear in ``value``, because in the latter we'd
  have their ``SSA_NAME`` s instead, but even if they were not in SSA
  form, user variables appearing in ``value`` are to be regarded as
  part of the executable code space, whereas those in ``var`` are to
  be regarded as part of the source code space.  There is no way to
  refer to the value bound to a user variable within a ``value``
  expression.

  If ``value`` is ``GIMPLE_DEBUG_BIND_NOVALUE``, debug information
  generation machinery is informed that the variable ``var`` is
  unbound, i.e., that its value is indeterminate, which sometimes means
  it is really unavailable, and other times that the compiler could not
  keep track of it.

  Block and location information for the newly-created stmt are
  taken from ``stmt``, if given.

.. function:: tree gimple_debug_bind_get_var(gimple stmt)

  Return the user variable :samp:`{var}` that is bound at ``stmt``.

.. function:: tree gimple_debug_bind_get_value(gimple stmt)

  Return the value expression that is bound to a user variable at
  ``stmt``.

.. function:: tree * gimple_debug_bind_get_value_ptr(gimple stmt)

  Return a pointer to the value expression that is bound to a user
  variable at ``stmt``.

.. function:: void gimple_debug_bind_set_var(gimple stmt,tree var)

  Modify the user variable bound at ``stmt`` to :samp:`{var}`.

.. function:: void gimple_debug_bind_set_value(gimple stmt,tree var)

  Modify the value bound to the user variable bound at ``stmt`` to
  :samp:`{value}`.

.. function:: void gimple_debug_bind_reset_value(gimple stmt)

  Modify the value bound to the user variable bound at ``stmt`` so
  that the variable becomes unbound.

.. function:: bool gimple_debug_bind_has_value_p(gimple stmt)

  Return ``TRUE`` if ``stmt`` binds a user variable to a value,
  and ``FALSE`` if it unbinds the variable.

.. function:: gimple gimple_build_debug_begin_stmt(tree block,location_t location)

  Build a ``GIMPLE_DEBUG`` statement with
  ``GIMPLE_DEBUG_BEGIN_STMT`` ``subcode``.  The effect of this
  statement is to tell debug information generation machinery that the
  user statement at the given ``location`` and ``block`` starts at
  the point at which the statement is inserted.  The intent is that side
  effects (e.g. variable bindings) of all prior user statements are
  observable, and that none of the side effects of subsequent user
  statements are.

.. function:: gimple gimple_build_debug_inline_entry(tree block,location_t location)

  Build a ``GIMPLE_DEBUG`` statement with
  ``GIMPLE_DEBUG_INLINE_ENTRY`` ``subcode``.  The effect of this
  statement is to tell debug information generation machinery that a
  function call at ``location`` underwent inline substitution, that
  ``block`` is the enclosing lexical block created for the
  substitution, and that at the point of the program in which the stmt is
  inserted, all parameters for the inlined function are bound to the
  respective arguments, and none of the side effects of its stmts are
  observable.

``GIMPLE_EH_FILTER``
^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_EH_FILTER

.. function:: geh_filter *gimple_build_eh_filter(tree types,gimple_seq failure)

  Build a ``GIMPLE_EH_FILTER`` statement.  ``TYPES`` are the filter's
  types.  ``FAILURE`` is a sequence with the filter's failure action.

.. function:: tree gimple_eh_filter_types(gimple g)

  Return the types handled by ``GIMPLE_EH_FILTER`` statement ``G``.

.. function:: tree * gimple_eh_filter_types_ptr(gimple g)

  Return a pointer to the types handled by ``GIMPLE_EH_FILTER``
  statement ``G``.

.. function:: gimple_seq gimple_eh_filter_failure(gimple g)

  Return the sequence of statement to execute when ``GIMPLE_EH_FILTER``
  statement fails.

.. function:: void gimple_eh_filter_set_types(geh_filter *g,tree types)

  Set ``TYPES`` to be the set of types handled by ``GIMPLE_EH_FILTER`` ``G``.

.. function:: void gimple_eh_filter_set_failure(geh_filter *g,gimple_seq failure)

  Set ``FAILURE`` to be the sequence of statements to execute on
  failure for ``GIMPLE_EH_FILTER`` ``G``.

.. function:: tree gimple_eh_must_not_throw_fndecl(geh_mnt *eh_mnt_stmt)

  Get the function decl to be called by the MUST_NOT_THROW region.

.. function:: void gimple_eh_must_not_throw_set_fndecl(geh_mnt *eh_mnt_stmt,tree decl)

  Set the function decl to be called by GS to DECL.

``GIMPLE_LABEL``
^^^^^^^^^^^^^^^^

.. index:: GIMPLE_LABEL

.. function:: glabel *gimple_build_label(tree label)

  Build a ``GIMPLE_LABEL`` statement with corresponding to the tree
  label, ``LABEL``.

.. function:: tree gimple_label_label(const glabel*g )

  Return the ``LABEL_DECL`` node used by ``GIMPLE_LABEL`` statement ``G``.

.. function:: void gimple_label_set_label(glabel *g,tree label)

  Set ``LABEL`` to be the ``LABEL_DECL`` node used by ``GIMPLE_LABEL``
  statement ``G``.

``GIMPLE_GOTO``
^^^^^^^^^^^^^^^

.. index:: GIMPLE_GOTO

.. function:: ggoto *gimple_build_goto(tree dest)

  Build a ``GIMPLE_GOTO`` statement to label ``DEST``.

.. function:: tree gimple_goto_dest(gimple g)

  Return the destination of the unconditional jump ``G``.

.. function:: void gimple_goto_set_dest(ggoto *g,tree dest)

  Set ``DEST`` to be the destination of the unconditional jump ``G``.

``GIMPLE_NOP``
^^^^^^^^^^^^^^

.. index:: GIMPLE_NOP

.. function:: gimple gimple_build_nop(void )

  Build a ``GIMPLE_NOP`` statement.

.. function:: bool gimple_nop_p(gimple g)

  Returns ``TRUE`` if statement ``G`` is a ``GIMPLE_NOP``.

``GIMPLE_OMP_ATOMIC_LOAD``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_ATOMIC_LOAD

.. function:: gomp_atomic_load *gimple_build_omp_atomic_load(tree lhs,tree rhs)

  Build a ``GIMPLE_OMP_ATOMIC_LOAD`` statement.  ``LHS`` is the left-hand
  side of the assignment.  ``RHS`` is the right-hand side of the
  assignment.

.. function:: void gimple_omp_atomic_load_set_lhs(gomp_atomic_load *g,tree lhs)

  Set the ``LHS`` of an atomic load.

.. function:: tree gimple_omp_atomic_load_lhs(const gomp_atomic_load*g )

  Get the ``LHS`` of an atomic load.

.. function:: void gimple_omp_atomic_load_set_rhs(gomp_atomic_load *g,tree rhs)

  Set the ``RHS`` of an atomic set.

.. function:: tree gimple_omp_atomic_load_rhs(const gomp_atomic_load*g )

  Get the ``RHS`` of an atomic set.

``GIMPLE_OMP_ATOMIC_STORE``
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_ATOMIC_STORE

.. function:: gomp_atomic_store *gimple_build_omp_atomic_store(tree val)

  Build a ``GIMPLE_OMP_ATOMIC_STORE`` statement. ``VAL`` is the value to be
  stored.

.. function:: void gimple_omp_atomic_store_set_val(gomp_atomic_store *g,tree val)

  Set the value being stored in an atomic store.

.. function:: tree gimple_omp_atomic_store_val(const gomp_atomic_store*g )

  Return the value being stored in an atomic store.

``GIMPLE_OMP_CONTINUE``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_CONTINUE

.. function:: gomp_continue *gimple_build_omp_continue(tree control_def,tree control_use)

  Build a ``GIMPLE_OMP_CONTINUE`` statement.  ``CONTROL_DEF`` is the
  definition of the control variable.  ``CONTROL_USE`` is the use of
  the control variable.

.. function:: tree gimple_omp_continue_control_def(const gomp_continue*s )

  Return the definition of the control variable on a
  ``GIMPLE_OMP_CONTINUE`` in ``S``.

.. function:: tree gimple_omp_continue_control_def_ptr(gomp_continue *s)

  Same as above, but return the pointer.

.. function:: tree gimple_omp_continue_set_control_def(gomp_continue *s)

  Set the control variable definition for a ``GIMPLE_OMP_CONTINUE``
  statement in ``S``.

.. function:: tree gimple_omp_continue_control_use(const gomp_continue*s )

  Return the use of the control variable on a ``GIMPLE_OMP_CONTINUE``
  in ``S``.

.. function:: tree gimple_omp_continue_control_use_ptr(gomp_continue *s)

  Same as above, but return the pointer.

.. function:: tree gimple_omp_continue_set_control_use(gomp_continue *s)

  Set the control variable use for a ``GIMPLE_OMP_CONTINUE`` statement
  in ``S``.

``GIMPLE_OMP_CRITICAL``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_CRITICAL

.. function:: gomp_critical *gimple_build_omp_critical(gimple_seq body,tree name)

  Build a ``GIMPLE_OMP_CRITICAL`` statement. ``BODY`` is the sequence of
  statements for which only one thread can execute.  ``NAME`` is an
  optional identifier for this critical block.

.. function:: tree gimple_omp_critical_name(const gomp_critical*g )

  Return the name associated with ``OMP_CRITICAL`` statement ``G``.

.. function:: tree * gimple_omp_critical_name_ptr(gomp_critical *g)

  Return a pointer to the name associated with ``OMP`` critical
  statement ``G``.

.. function:: void gimple_omp_critical_set_name(gomp_critical *g,tree name)

  Set ``NAME`` to be the name associated with ``OMP`` critical statement ``G``.

``GIMPLE_OMP_FOR``
^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_FOR

.. function:: gomp_for *gimple_build_omp_for(gimple_seq body,tree clauses,tree index,tree initial,tree final,tree incr,gimple_seq pre_body,enum tree_codeomp_for_cond )

  Build a ``GIMPLE_OMP_FOR`` statement. ``BODY`` is sequence of statements
  inside the for loop.  ``CLAUSES``, are any of the loop
  construct's clauses.  ``PRE_BODY`` is the
  sequence of statements that are loop invariant.  ``INDEX`` is the
  index variable.  ``INITIAL`` is the initial value of ``INDEX``.  ``FINAL`` is
  final value of ``INDEX``.  OMP_FOR_COND is the predicate used to
  compare ``INDEX`` and ``FINAL``.  ``INCR`` is the increment expression.

.. function:: tree gimple_omp_for_clauses(gimple g)

  Return the clauses associated with ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_clauses_ptr(gimple g)

  Return a pointer to the ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_clauses(gimple g,tree clauses)

  Set ``CLAUSES`` to be the list of clauses associated with ``OMP_FOR`` ``G``.

.. function:: tree gimple_omp_for_index(gimple g)

  Return the index variable for ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_index_ptr(gimple g)

  Return a pointer to the index variable for ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_index(gimple g,tree index)

  Set ``INDEX`` to be the index variable for ``OMP_FOR`` ``G``.

.. function:: tree gimple_omp_for_initial(gimple g)

  Return the initial value for ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_initial_ptr(gimple g)

  Return a pointer to the initial value for ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_initial(gimple g,tree initial)

  Set ``INITIAL`` to be the initial value for ``OMP_FOR`` ``G``.

.. function:: tree gimple_omp_for_final(gimple g)

  Return the final value for ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_final_ptr(gimple g)

  turn a pointer to the final value for ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_final(gimple g,tree final)

  Set ``FINAL`` to be the final value for ``OMP_FOR`` ``G``.

.. function:: tree gimple_omp_for_incr(gimple g)

  Return the increment value for ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_incr_ptr(gimple g)

  Return a pointer to the increment value for ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_incr(gimple g,tree incr)

  Set ``INCR`` to be the increment value for ``OMP_FOR`` ``G``.

.. function:: gimple_seq gimple_omp_for_pre_body(gimple g)

  Return the sequence of statements to execute before the ``OMP_FOR``
  statement ``G`` starts.

.. function:: void gimple_omp_for_set_pre_body(gimple g,gimple_seq pre_body)

  Set ``PRE_BODY`` to be the sequence of statements to execute before
  the ``OMP_FOR`` statement ``G`` starts.

.. function:: void gimple_omp_for_set_cond(gimple g,enum tree_codecond )

  Set ``COND`` to be the condition code for ``OMP_FOR`` ``G``.

.. function:: enum tree_code gimple_omp_for_cond(gimple g)

  Return the condition code associated with ``OMP_FOR`` ``G``.

``GIMPLE_OMP_MASTER``
^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_MASTER

.. function:: gimple gimple_build_omp_master(gimple_seq body)

  Build a ``GIMPLE_OMP_MASTER`` statement. ``BODY`` is the sequence of
  statements to be executed by just the master.

``GIMPLE_OMP_ORDERED``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_ORDERED

.. function:: gimple gimple_build_omp_ordered(gimple_seq body)

  Build a ``GIMPLE_OMP_ORDERED`` statement.

``BODY`` is the sequence of statements inside a loop that will
executed in sequence.

``GIMPLE_OMP_PARALLEL``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_PARALLEL

.. function:: gomp_parallel *gimple_build_omp_parallel(gimple_seq body,tree clauses,tree child_fn,tree data_arg)

  Build a ``GIMPLE_OMP_PARALLEL`` statement.

``BODY`` is sequence of statements which are executed in parallel.
``CLAUSES``, are the ``OMP`` parallel construct's clauses.  ``CHILD_FN`` is
the function created for the parallel threads to execute.
``DATA_ARG`` are the shared data argument(s).

.. function:: bool gimple_omp_parallel_combined_p(gimple g)

  Return true if ``OMP`` parallel statement ``G`` has the
  ``GF_OMP_PARALLEL_COMBINED`` flag set.

.. function:: void gimple_omp_parallel_set_combined_p(gimple g)

  Set the ``GF_OMP_PARALLEL_COMBINED`` field in ``OMP`` parallel statement
  ``G``.

.. function:: gimple_seq gimple_omp_body(gimple g)

  Return the body for the ``OMP`` statement ``G``.

.. function:: void gimple_omp_set_body(gimple g,gimple_seq body)

  Set ``BODY`` to be the body for the ``OMP`` statement ``G``.

.. function:: tree gimple_omp_parallel_clauses(gimple g)

  Return the clauses associated with ``OMP_PARALLEL`` ``G``.

.. function:: tree * gimple_omp_parallel_clauses_ptr(gomp_parallel *g)

  Return a pointer to the clauses associated with ``OMP_PARALLEL`` ``G``.

.. function:: void gimple_omp_parallel_set_clauses(gomp_parallel *g,tree clauses)

  Set ``CLAUSES`` to be the list of clauses associated with
  ``OMP_PARALLEL`` ``G``.

.. function:: tree gimple_omp_parallel_child_fn(const gomp_parallel*g )

  Return the child function used to hold the body of ``OMP_PARALLEL``
  ``G``.

.. function:: tree * gimple_omp_parallel_child_fn_ptr(gomp_parallel *g)

  Return a pointer to the child function used to hold the body of
  ``OMP_PARALLEL`` ``G``.

.. function:: void gimple_omp_parallel_set_child_fn(gomp_parallel *g,tree child_fn)

  Set ``CHILD_FN`` to be the child function for ``OMP_PARALLEL`` ``G``.

.. function:: tree gimple_omp_parallel_data_arg(const gomp_parallel*g )

  Return the artificial argument used to send variables and values
  from the parent to the children threads in ``OMP_PARALLEL`` ``G``.

.. function:: tree * gimple_omp_parallel_data_arg_ptr(gomp_parallel *g)

  Return a pointer to the data argument for ``OMP_PARALLEL`` ``G``.

.. function:: void gimple_omp_parallel_set_data_arg(gomp_parallel *g,tree data_arg)

  Set ``DATA_ARG`` to be the data argument for ``OMP_PARALLEL`` ``G``.

``GIMPLE_OMP_RETURN``
^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_RETURN

.. function:: gimple gimple_build_omp_return(bool wait_p)

  Build a ``GIMPLE_OMP_RETURN`` statement. ``WAIT_P`` is true if this is a
  non-waiting return.

.. function:: void gimple_omp_return_set_nowait(gimple s)

  Set the nowait flag on ``GIMPLE_OMP_RETURN`` statement ``S``.

.. function:: bool gimple_omp_return_nowait_p(gimple g)

  Return true if ``OMP`` return statement ``G`` has the
  ``GF_OMP_RETURN_NOWAIT`` flag set.

``GIMPLE_OMP_SECTION``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_SECTION

.. function:: gimple gimple_build_omp_section(gimple_seq body)

  Build a ``GIMPLE_OMP_SECTION`` statement for a sections statement.

``BODY`` is the sequence of statements in the section.

.. function:: bool gimple_omp_section_last_p(gimple g)

  Return true if ``OMP`` section statement ``G`` has the
  ``GF_OMP_SECTION_LAST`` flag set.

.. function:: void gimple_omp_section_set_last(gimple g)

  Set the ``GF_OMP_SECTION_LAST`` flag on ``G``.

``GIMPLE_OMP_SECTIONS``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_SECTIONS

.. function:: gomp_sections *gimple_build_omp_sections(gimple_seq body,tree clauses)

  Build a ``GIMPLE_OMP_SECTIONS`` statement. ``BODY`` is a sequence of
  section statements.  ``CLAUSES`` are any of the ``OMP`` sections
  construct's clauses: private, firstprivate, lastprivate,
  reduction, and nowait.

.. function:: gimple gimple_build_omp_sections_switch(void )

  Build a ``GIMPLE_OMP_SECTIONS_SWITCH`` statement.

.. function:: tree gimple_omp_sections_control(gimple g)

  Return the control variable associated with the
  ``GIMPLE_OMP_SECTIONS`` in ``G``.

.. function:: tree * gimple_omp_sections_control_ptr(gimple g)

  Return a pointer to the clauses associated with the
  ``GIMPLE_OMP_SECTIONS`` in ``G``.

.. function:: void gimple_omp_sections_set_control(gimple g,tree control)

  Set ``CONTROL`` to be the set of clauses associated with the
  ``GIMPLE_OMP_SECTIONS`` in ``G``.

.. function:: tree gimple_omp_sections_clauses(gimple g)

  Return the clauses associated with ``OMP_SECTIONS`` ``G``.

.. function:: tree * gimple_omp_sections_clauses_ptr(gimple g)

  Return a pointer to the clauses associated with ``OMP_SECTIONS`` ``G``.

.. function:: void gimple_omp_sections_set_clauses(gimple g,tree clauses)

  Set ``CLAUSES`` to be the set of clauses associated with ``OMP_SECTIONS``
  ``G``.

``GIMPLE_OMP_SINGLE``
^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_OMP_SINGLE

.. function:: gomp_single *gimple_build_omp_single(gimple_seq body,tree clauses)

  Build a ``GIMPLE_OMP_SINGLE`` statement. ``BODY`` is the sequence of
  statements that will be executed once.  ``CLAUSES`` are any of the
  ``OMP`` single construct's clauses: private, firstprivate,
  copyprivate, nowait.

.. function:: tree gimple_omp_single_clauses(gimple g)

  Return the clauses associated with ``OMP_SINGLE`` ``G``.

.. function:: tree * gimple_omp_single_clauses_ptr(gimple g)

  Return a pointer to the clauses associated with ``OMP_SINGLE`` ``G``.

.. function:: void gimple_omp_single_set_clauses(gomp_single *g,tree clauses)

  Set ``CLAUSES`` to be the clauses associated with ``OMP_SINGLE`` ``G``.

``GIMPLE_PHI``
^^^^^^^^^^^^^^

.. index:: GIMPLE_PHI

.. function:: unsigned gimple_phi_capacity(gimple g)

  Return the maximum number of arguments supported by ``GIMPLE_PHI`` ``G``.

.. function:: unsigned gimple_phi_num_args(gimple g)

  Return the number of arguments in ``GIMPLE_PHI`` ``G``. This must always
  be exactly the number of incoming edges for the basic block
  holding ``G``.

.. function:: tree gimple_phi_result(gimple g)

  Return the ``SSA`` name created by ``GIMPLE_PHI`` ``G``.

.. function:: tree * gimple_phi_result_ptr(gimple g)

  Return a pointer to the ``SSA`` name created by ``GIMPLE_PHI`` ``G``.

.. function:: void gimple_phi_set_result(gphi *g,tree result)

  Set ``RESULT`` to be the ``SSA`` name created by ``GIMPLE_PHI`` ``G``.

.. function:: struct phi_arg_d * gimple_phi_arg(gimple g,index )

  Return the ``PHI`` argument corresponding to incoming edge ``INDEX`` for
  ``GIMPLE_PHI`` ``G``.

.. function:: void gimple_phi_set_arg(gphi *g,index ,struct phi_arg_d* phiarg)

  Set ``PHIARG`` to be the argument corresponding to incoming edge
  ``INDEX`` for ``GIMPLE_PHI`` ``G``.

``GIMPLE_RESX``
^^^^^^^^^^^^^^^

.. index:: GIMPLE_RESX

.. function:: gresx *gimple_build_resx(int region)

  Build a ``GIMPLE_RESX`` statement which is a statement.  This
  statement is a placeholder for _Unwind_Resume before we know if a
  function call or a branch is needed.  ``REGION`` is the exception
  region from which control is flowing.

.. function:: int gimple_resx_region(const gresx*g )

  Return the region number for ``GIMPLE_RESX`` ``G``.

.. function:: void gimple_resx_set_region(gresx *g,int region)

  Set ``REGION`` to be the region number for ``GIMPLE_RESX`` ``G``.

``GIMPLE_RETURN``
^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_RETURN

.. function:: greturn *gimple_build_return(tree retval)

  Build a ``GIMPLE_RETURN`` statement whose return value is retval.

.. function:: tree gimple_return_retval(const greturn*g )

  Return the return value for ``GIMPLE_RETURN`` ``G``.

.. function:: void gimple_return_set_retval(greturn *g,tree retval)

  Set ``RETVAL`` to be the return value for ``GIMPLE_RETURN`` ``G``.

``GIMPLE_SWITCH``
^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_SWITCH

.. function:: gswitch *gimple_build_switch(tree index,tree default_label,vec <tree>*args )

  Build a ``GIMPLE_SWITCH`` statement.  ``INDEX`` is the index variable
  to switch on, and ``DEFAULT_LABEL`` represents the default label.
  ``ARGS`` is a vector of ``CASE_LABEL_EXPR`` trees that contain the
  non-default case labels.  Each label is a tree of code ``CASE_LABEL_EXPR``.

.. function:: unsigned gimple_switch_num_labels(const gswitch*g )

  Return the number of labels associated with the switch statement
  ``G``.

.. function:: void gimple_switch_set_num_labels(gswitch *g,unsigned nlabels)

  Set ``NLABELS`` to be the number of labels for the switch statement
  ``G``.

.. function:: tree gimple_switch_index(const gswitch*g )

  Return the index variable used by the switch statement ``G``.

.. function:: void gimple_switch_set_index(gswitch *g,tree index)

  Set ``INDEX`` to be the index variable for switch statement ``G``.

.. function:: tree gimple_switch_label(const gswitch*g ,unsigned index)

  Return the label numbered ``INDEX``. The default label is 0, followed
  by any labels in a switch statement.

.. function:: void gimple_switch_set_label(gswitch *g,unsigned index,tree label)

  Set the label number ``INDEX`` to ``LABEL``. 0 is always the default
  label.

.. function:: tree gimple_switch_default_label(const gswitch*g )

  Return the default label for a switch statement.

.. function:: void gimple_switch_set_default_label(gswitch *g,tree label)

  Set the default label for a switch statement.

``GIMPLE_TRY``
^^^^^^^^^^^^^^

.. index:: GIMPLE_TRY

.. function:: gtry *gimple_build_try(gimple_seq eval,gimple_seq cleanup,unsigned intkind )

  Build a ``GIMPLE_TRY`` statement.  ``EVAL`` is a sequence with the
  expression to evaluate.  ``CLEANUP`` is a sequence of statements to
  run at clean-up time.  ``KIND`` is the enumeration value
  ``GIMPLE_TRY_CATCH`` if this statement denotes a try/catch construct
  or ``GIMPLE_TRY_FINALLY`` if this statement denotes a try/finally
  construct.

.. function:: enum gimple_try_flags gimple_try_kind(gimple g)

  Return the kind of try block represented by ``GIMPLE_TRY`` ``G``. This is
  either ``GIMPLE_TRY_CATCH`` or ``GIMPLE_TRY_FINALLY``.

.. function:: bool gimple_try_catch_is_cleanup(gimple g)

  Return the ``GIMPLE_TRY_CATCH_IS_CLEANUP`` flag.

.. function:: gimple_seq gimple_try_eval(gimple g)

  Return the sequence of statements used as the body for ``GIMPLE_TRY``
  ``G``.

.. function:: gimple_seq gimple_try_cleanup(gimple g)

  Return the sequence of statements used as the cleanup body for
  ``GIMPLE_TRY`` ``G``.

.. function:: void gimple_try_set_catch_is_cleanup(gimple g,bool catch_is_cleanup)

  Set the ``GIMPLE_TRY_CATCH_IS_CLEANUP`` flag.

.. function:: void gimple_try_set_eval(gtry *g,gimple_seq eval)

  Set ``EVAL`` to be the sequence of statements to use as the body for
  ``GIMPLE_TRY`` ``G``.

.. function:: void gimple_try_set_cleanup(gtry *g,gimple_seq cleanup)

  Set ``CLEANUP`` to be the sequence of statements to use as the
  cleanup body for ``GIMPLE_TRY`` ``G``.

``GIMPLE_WITH_CLEANUP_EXPR``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: GIMPLE_WITH_CLEANUP_EXPR

.. function:: gimple gimple_build_wce(gimple_seq cleanup)

  Build a ``GIMPLE_WITH_CLEANUP_EXPR`` statement.  ``CLEANUP`` is the
  clean-up expression.

.. function:: gimple_seq gimple_wce_cleanup(gimple g)

  Return the cleanup sequence for cleanup statement ``G``.

.. function:: void gimple_wce_set_cleanup(gimple g,gimple_seq cleanup)

  Set ``CLEANUP`` to be the cleanup sequence for ``G``.

.. function:: bool gimple_wce_cleanup_eh_only(gimple g)

  Return the ``CLEANUP_EH_ONLY`` flag for a ``WCE`` tuple.

.. function:: void gimple_wce_set_cleanup_eh_only(gimple g,bool eh_only_p)

  Set the ``CLEANUP_EH_ONLY`` flag for a ``WCE`` tuple.

