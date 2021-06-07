..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _function-like-macros:

Function-like Macros
********************

.. index:: function-like macros

You can also define macros whose use looks like a function call.  These
are called :dfn:`function-like macros`.  To define a function-like macro,
you use the same :samp:`#define` directive, but you put a pair of
parentheses immediately after the macro name.  For example,

.. code-block:: c++

  #define lang_init()  c_init()
  lang_init()
       → c_init()

A function-like macro is only expanded if its name appears with a pair
of parentheses after it.  If you write just the name, it is left alone.
This can be useful when you have a function and a macro of the same
name, and you wish to use the function sometimes.

.. code-block:: c++

  extern void foo(void);
  #define foo() /* optimized inline version */
  ...
    foo();
    funcptr = foo;

Here the call to ``foo()`` will use the macro, but the function
pointer will get the address of the real function.  If the macro were to
be expanded, it would cause a syntax error.

If you put spaces between the macro name and the parentheses in the
macro definition, that does not define a function-like macro, it defines
an object-like macro whose expansion happens to begin with a pair of
parentheses.

.. code-block:: c++

  #define lang_init ()    c_init()
  lang_init()
       → () c_init()()

The first two pairs of parentheses in this expansion come from the
macro.  The third is the pair that was originally after the macro
invocation.  Since ``lang_init`` is an object-like macro, it does not
consume those parentheses.

