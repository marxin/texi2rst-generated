.. _kind-type-parameters:

KIND Type Parameters
********************

.. index:: kind

The ``KIND`` type parameters supported by GNU Fortran for the primitive
data types are:

.. envvar:: INTEGER

  1, 2, 4, 8*, 16*, default: 4**

.. envvar:: LOGICAL

  1, 2, 4, 8*, 16*, default: 4**

.. envvar:: REAL

  4, 8, 10*, 16*, default: 4***

.. envvar:: COMPLEX

  4, 8, 10*, 16*, default: 4***

``DOUBLE PRECISION``
  4, 8, 10*, 16*, default: 8***

.. envvar:: CHARACTER

  1, 4, default: 1

* not available on all systems 

** unless :option:`-fdefault-integer-8` is used 

*** unless :option:`-fdefault-real-8` is used (see Fortran Dialect Options)

The ``KIND`` value matches the storage size in bytes, except for
``COMPLEX`` where the storage size is twice as much (or both real and
imaginary part are a real value of the given size).  It is recommended to use
the SELECTED_CHAR_KIND, SELECTED_INT_KIND and
SELECTED_REAL_KIND intrinsics or the ``INT8``, ``INT16``,
``INT32``, ``INT64``, ``REAL32``, ``REAL64``, and ``REAL128``
parameters of the ``ISO_FORTRAN_ENV`` module instead of the concrete values.
The available kind parameters can be found in the constant arrays
``CHARACTER_KINDS``, ``INTEGER_KINDS``, ``LOGICAL_KINDS`` and
``REAL_KINDS`` in the ISO_FORTRAN_ENV module.  For C interoperability,
the kind parameters of the ISO_C_BINDING module should be used.
