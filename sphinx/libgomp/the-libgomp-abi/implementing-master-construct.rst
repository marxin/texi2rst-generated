.. _implementing-master-construct:

Implementing MASTER construct
*****************************

.. code-block:: c++

  if (omp_get_thread_num () == 0)
    block

Alternately, we generate two copies of the parallel subfunction
and only include this in the version run by the master thread.
Surely this is not worthwhile though...

