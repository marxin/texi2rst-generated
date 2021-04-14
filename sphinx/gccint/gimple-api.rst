.. _gimple-api:

GIMPLE API
**********

.. index:: GIMPLE API

.. function:: tree gimple_simplify(enum tree_code,tree ,tree ,gimple_seq *,tree (* )(tree ))

  The main GIMPLE API entry to the expression simplifications mimicing
  that of the GENERIC fold_{unary,binary,ternary} functions.

thus providing n-ary overloads for operation or function.  The
additional arguments are a gimple_seq where built statements are
inserted on (if ``NULL`` then simplifications requiring new statements
are not performed) and a valueization hook that can be used to
tie simplifications to a SSA lattice.

In addition to those APIs ``fold_stmt`` is overloaded with
a valueization hook:

.. function:: fold_stmt (gimple_stmt_iterator* ,tree (* )(tree )); 

Ontop of these a ``fold_buildN``-like API for GIMPLE is introduced:

.. function:: tree gimple_build(gimple_seq *,location_t ,enum tree_code,tree ,tree ,tree (*valueize )(tree )= NULL); 

which is supposed to replace ``force_gimple_operand (fold_buildN (...), ...)``
and calls to ``fold_convert``.  Overloads without the ``location_t``
argument exist.  Built statements are inserted on the provided sequence
and simplification is performed using the optional valueization hook.

