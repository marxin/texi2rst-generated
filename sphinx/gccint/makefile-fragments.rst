.. _fragments:

Makefile Fragments
------------------

.. index:: makefile fragment

When you configure GCC using the configure script, it will
construct the file Makefile from the template file
Makefile.in.  When it does this, it can incorporate makefile
fragments from the config directory.  These are used to set
Makefile parameters that are not amenable to being calculated by
autoconf.  The list of fragments to incorporate is set by
config.gcc (and occasionally config.build
and config.host); See :ref:`system-config`.

Fragments are named either t- :samp:`{target}` or x- :samp:`{host}`,
depending on whether they are relevant to configuring GCC to produce
code for a particular target, or to configuring GCC to run on a
particular host.  Here :samp:`{target}` and :samp:`{host}` are mnemonics
which usually have some relationship to the canonical system name, but
no formal connection.

If these files do not exist, it means nothing needs to be added for a
given target or host.  Most targets need a few t- :samp:`{target}`
fragments, but needing x- :samp:`{host}` fragments is rare.

.. toctree::

  target-fragment
  host-fragment
  target-makefile-fragments
  host-makefile-fragments

