.. _including-patterns:

Including Patterns in Machine Descriptions.
*******************************************

.. index:: insn includes

.. index:: include

The ``include`` pattern tells the compiler tools where to
look for patterns that are in files other than in the file
.md.  This is used only at build time and there is no preprocessing allowed.

It looks like:

.. code-block:: c++

  (include
    pathname)

For example:

.. code-block:: c++

  (include "filestuff")

Where :samp:`{pathname}` is a string that specifies the location of the file,
specifies the include file to be in gcc/config/target/filestuff.  The
directory gcc/config/target is regarded as the default directory.

Machine descriptions may be split up into smaller more manageable subsections
and placed into subdirectories.

By specifying:

.. code-block:: c++

  (include "BOGUS/filestuff")

the include file is specified to be in gcc/config/ :samp:`{target}` /BOGUS/filestuff.

Specifying an absolute path for the include file such as;

.. code-block:: c++

  (include "/u2/BOGUS/filestuff")

is permitted but is not encouraged.

RTL Generation Tool Options for Directory Search
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: directory options .md

.. index:: options, directory search

.. index:: search options

The :option:`-I`:samp:`{dir}` option specifies directories to search for machine descriptions.
For example:

.. code-block:: c++

  genrecog -I/p1/abc/proc1 -I/p2/abcd/pro2 target.md

Add the directory :samp:`{dir}` to the head of the list of directories to be
searched for header files.  This can be used to override a system machine definition
file, substituting your own version, since these directories are
searched before the default machine description file directories.  If you use more than
one :option:`-I` option, the directories are scanned in left-to-right
order; the standard default directory come after.
