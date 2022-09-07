  .. _environment-variables:

GNU Modula-2 related environment variables
******************************************

This section descibes the environment variables used by GNU Modula-2 and
how they can be used to switch between releases of the compiler.  Other
environment variables can be set to modify the default library path.
Initially we will consider environment variables most likely used by
the end user.  These two environment variables are ``GM2IPATH``
and ``GM2OPATH``.

For example suppose a compile and link on the command line looks like
this:

.. code-block:: modula2

  $ gm2 -g -c -I. -I../project -I../project/unix foo.mod
  $ gm2 -fonlylink -g -I. -I../project -I../project/unix \
    -fobject-path=../project/obj -Iobject-path=../project/unix/obj \
    -I. foo.mod

they can be simplified by utilising two environment variables to do
exactly the same compile and link.

.. code-block:: modula2

  $ export GM2IPATH=../project:../project/unix
  $ export GM2OPATH=../project/obj:../project/unix/obj
  $ gm2 -g -I. foo.mod

It is important to note that the two environment variables
``GM2IPATH`` and ``GM2OPATH`` have a lower priority than any
``-I`` or ``-fobject-path=`` command line option.  The search
order for compiling and linking is: command line switches followed by
environment variable paths followed by default runtime libraries or
Modula-2 dialect libraries.  If in doubt include the ``-v`` option
to see the search path used between the compiler subcomponents.

