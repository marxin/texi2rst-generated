.. _system-v-options:

Options for System V
^^^^^^^^^^^^^^^^^^^^

These additional options are available on System V Release 4 for
compatibility with other compilers on those systems:

.. option:: -G

  Create a shared object.
  It is recommended that :option:`-symbolic` or :option:`-shared` be used instead.

.. option:: -Qy

  Identify the versions of each tool used by the compiler, in a
  ``.ident`` assembler directive in the output.

.. option:: -Qn

  Refrain from adding ``.ident`` directives to the output file (this is
  the default).

.. option:: -YP,dirs, -YP

  Search the directories :samp:`{dirs}`, and no others, for libraries
  specified with :option:`-l`.

.. option:: -Ym,dir, -Ym

  Look in the directory :samp:`{dir}` to find the M4 preprocessor.
  The assembler uses this option.

  .. This is supposed to go with a -Yd for predefined M4 macro files, but

  .. the generic assembler that comes with Solaris takes just -Ym.
