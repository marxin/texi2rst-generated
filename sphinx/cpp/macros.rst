.. _macros:

Macros
------

A :dfn:`macro` is a fragment of code which has been given a name.
Whenever the name is used, it is replaced by the contents of the macro.
There are two kinds of macros.  They differ mostly in what they look
like when they are used.  :dfn:`Object-like` macros resemble data objects
when used, :dfn:`function-like` macros resemble function calls.

You may define any valid identifier as a macro, even if it is a C
keyword.  The preprocessor does not know anything about keywords.  This
can be useful if you wish to hide a keyword such as ``const`` from an
older compiler that does not understand it.  However, the preprocessor
operator ``defined`` (see :ref:`defined`) can never be defined as a
macro, and C++'s named operators (see :ref:`c++-named-operators`) cannot be
macros when you are compiling C++.

.. toctree::
  :maxdepth: 2

  object-like-macros
  function-like-macros
  macro-arguments
  stringizing
  concatenation
  variadic-macros
  predefined-macros
  undefining-and-redefining-macros
  directives-within-macro-arguments
  macro-pitfalls

