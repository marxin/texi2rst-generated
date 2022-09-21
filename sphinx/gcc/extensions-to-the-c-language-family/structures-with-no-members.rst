..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _empty-structures:

.. index:: empty structures

.. index:: zero-size structures

Structures with No Members
**************************

GCC permits a C structure to have no members:

.. code-block:: c++

  struct empty {
  };

The structure has size zero.  In C++, empty structures are part
of the language.  G++ treats empty structures as if they had a single
member of type ``char``.

