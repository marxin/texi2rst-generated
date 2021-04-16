.. _type-information:

Memory Management and Type Information
--------------------------------------

.. index:: GGC

.. index:: GTY

GCC uses some fairly sophisticated memory management techniques, which
involve determining information about GCC's data structures from GCC's
source code and using this information to perform garbage collection and
implement precompiled headers.

A full C++ parser would be too complicated for this task, so a limited
subset of C++ is interpreted and special markers are used to determine
what parts of the source to look at.  All ``struct``, ``union``
and ``template`` structure declarations that define data structures
that are allocated under control of the garbage collector must be
marked.  All global variables that hold pointers to garbage-collected
memory must also be marked.  Finally, all global variables that need
to be saved and restored by a precompiled header must be marked.  (The
precompiled header mechanism can only save static variables if they're
scalar. Complex data structures must be allocated in garbage-collected
memory to be saved in a precompiled header.)

The full format of a marker is

.. code-block:: c++

  GTY (([option] [(param)], [option] [(param)] ...))

but in most cases no options are needed.  The outer double parentheses
are still necessary, though: ``GTY(())``.  Markers can appear:

* In a structure definition, before the open brace;

* In a global variable declaration, after the keyword ``static`` or
  ``extern``; and

* In a structure field definition, before the name of the field.

Here are some examples of marking simple data structures and globals.

.. code-block:: c++

  struct GTY(()) tag
  {
    fields...
  };

  typedef struct GTY(()) tag
  {
    fields...
  } *typename;

  static GTY(()) struct tag *list;   /* points to GC memory */
  static GTY(()) int counter;        /* save counter in a PCH */

The parser understands simple typedefs such as
``typedef struct tag *name;`` and
``typedef int name;``.
These don't need to be marked.

Since ``gengtype``'s understanding of C++ is limited, there are
several constructs and declarations that are not supported inside
classes/structures marked for automatic GC code generation.  The
following C++ constructs produce a ``gengtype`` error on
structures/classes marked for automatic GC code generation:

* Type definitions inside classes/structures are not supported.

* Enumerations inside classes/structures are not supported.

If you have a class or structure using any of the above constructs,
you need to mark that class as ``GTY ((user))`` and provide your
own marking routines (see section User GC for details).

It is always valid to include function definitions inside classes.
Those are always ignored by ``gengtype``, as it only cares about
data members.

.. toctree::

  What goes inside a ``GTY(())``. <gty-options>
  Adding GTY to a class hierarchy. <inheritance-and-gty>
  Adding user-provided GC marking routines. <user-gc>
  Making global variables GGC roots. <ggc-roots>
  How the generated files work. <files>
  How to invoke the garbage collector. <invoking-the-garbage-collector>
  When something does not work as expected. <troubleshooting>

.. toctree::

  the-inside-of-a-gty
  support-for-inheritance
  support-for-user-provided-gc-marking-routines
  marking-roots-for-the-garbage-collector
  source-files-containing-type-information
  how-to-invoke-the-garbage-collector
  troubleshooting-the-garbage-collector

