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
  :maxdepth: 2

  c-type-interoperability
  function-names

