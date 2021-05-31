.. _c++-extensions:

Extensions to the C++ Language
------------------------------

.. index:: extensions, C++ language

.. index:: C++ language extensions

The GNU compiler provides these extensions to the C++ language (and you
can also use most of the C language extensions in your C++ programs).  If you
want to write code that checks whether these features are available, you can
test for the GNU compiler the same way as for C programs: check for a
predefined macro ``__GNUC__``.  You can also use ``__GNUG__`` to
test specifically for GNU C++ (see :ref:`Predefined Macros <common-predefined-macros>`).

.. toctree::
  :maxdepth: 2

  c++-volatiles
  restricted-pointers
  vague-linkage
  c++-interface
  template-instantiation
  bound-member-functions
  c++-attributes
  function-multiversioning
  type-traits
  c++-concepts
  deprecated-features
  backwards-compatibility
  when-is-a-volatile-c++-object-accessed
  restricting-pointer-aliasing
  c++-interface-and-implementation-pragmas
  wheres-the-template
  extracting-the-function-pointer-from-a-bound-pointer-to-member-function
  c++-specific-variable-function-and-type-attributes

