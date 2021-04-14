.. _implementing-atomic-construct:

Implementing ATOMIC construct
*****************************

The target should implement the ``__sync`` builtins.

Failing that we could add

.. code-block:: c++

    void GOMP_atomic_enter (void)
    void GOMP_atomic_exit (void)

which reuses the regular lock code, but with yet another lock
object private to the library.

