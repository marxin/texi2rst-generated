.. _annotations:

Annotations
***********

.. index:: annotations

The optimizers need to associate attributes with variables during the
optimization process.  For instance, we need to know whether a
variable has aliases.  All these attributes are stored in data
structures called annotations which are then linked to the field
``ann`` in ``struct tree_common``.

