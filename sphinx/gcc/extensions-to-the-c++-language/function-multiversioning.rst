.. _function-multiversioning:

Function Multiversioning
************************

.. index:: function versions

With the GNU C++ front end, for x86 targets, you may specify multiple
versions of a function, where each function is specialized for a
specific target feature.  At runtime, the appropriate version of the
function is automatically executed depending on the characteristics of
the execution platform.  Here is an example.

.. code-block:: c++

  __attribute__ ((target ("default")))
  int foo ()
  {
    // The default version of foo.
    return 0;
  }

  __attribute__ ((target ("sse4.2")))
  int foo ()
  {
    // foo version for SSE4.2
    return 1;
  }

  __attribute__ ((target ("arch=atom")))
  int foo ()
  {
    // foo version for the Intel ATOM processor
    return 2;
  }

  __attribute__ ((target ("arch=amdfam10")))
  int foo ()
  {
    // foo version for the AMD Family 0x10 processors.
    return 3;
  }

  int main ()
  {
    int (*p)() = &foo;
    assert ((*p) () == foo ());
    return 0;
  }

In the above example, four versions of function foo are created. The
first version of foo with the target attribute "default" is the default
version.  This version gets executed when no other target specific
version qualifies for execution on a particular platform. A new version
of foo is created by using the same function signature but with a
different target string.  Function foo is called or a pointer to it is
taken just like a regular function.  GCC takes care of doing the
dispatching to call the right version at runtime.  Refer to the
`GCC wiki on
Function Multiversioning <http://gcc.gnu.org/wiki/FunctionMultiVersioning>`_ for more details.
