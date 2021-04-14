  .. _c-interoperability:

C Interoperability
------------------

When using :command:`gccgo` there is limited interoperability with C,
or with C++ code compiled using ``extern "C"``.

This information is provided largely for documentation purposes.  For
ordinary use it is best to build programs with the go tool and then
use ``import "C"``, as described at
http://golang.org/cmd/cgo.

.. toctree::

  How C and Go types match up. <c-type-interoperability>
  How Go functions are named. <function-names>

.. toctree::

  c-type-interoperability
  function-names

