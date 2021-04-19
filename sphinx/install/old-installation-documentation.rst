
Old installation documentation
------------------------------

Note most of this information is out of date and superseded by the
previous chapters of this manual.  It is provided for historical
reference only, because of a lack of volunteers to merge it into the
main manual.

.. toctree::

  Configurations Supported by GCC. <configurations>

Here is the procedure for installing GCC on a GNU or Unix system.

* If you have chosen a configuration for GCC which requires other GNU
  tools (such as GAS or the GNU linker) instead of the standard system
  tools, install the required tools in the build directory under the names
  as, ld or whatever is appropriate.

  Alternatively, you can do subsequent compilation using a value of the
  ``PATH`` environment variable such that the necessary GNU tools come
  before the standard system tools.

* Specify the host, build and target machine configurations.  You do this
  when you run the configure script.

  The :dfn:`build` machine is the system which you are using, the
  :dfn:`host` machine is the system where you want to run the resulting
  compiler (normally the build machine), and the :dfn:`target` machine is
  the system for which you want the compiler to generate code.

  If you are building a compiler to produce code for the machine it runs
  on (a native compiler), you normally do not need to specify any operands
  to configure; it will try to guess the type of machine you are on
  and use that as the build, host and target machines.  So you don't need
  to specify a configuration when building a native compiler unless
  configure cannot figure out what your configuration is or guesses
  wrong.

  In those cases, specify the build machine's :dfn:`configuration name`
  with the :option:`--host` option; the host and target will default to be
  the same as the host machine.

  Here is an example:

  .. code-block:: c++

    ./configure --host=sparc-sun-sunos4.1

  A configuration name may be canonical or it may be more or less
  abbreviated.

  A canonical configuration name has three parts, separated by dashes.
  It looks like this: :samp:`{cpu}-{company}-{system}`.
  (The three parts may themselves contain dashes; configure
  can figure out which dashes serve which purpose.)  For example,
  :samp:`m68k-sun-sunos4.1` specifies a Sun 3.

  You can also replace parts of the configuration by nicknames or aliases.
  For example, :samp:`sun3` stands for :samp:`m68k-sun`, so
  :samp:`sun3-sunos4.1` is another way to specify a Sun 3.

  You can specify a version number after any of the system types, and some
  of the CPU types.  In most cases, the version is irrelevant, and will be
  ignored.  So you might as well specify the version if you know it.

  See Configurations, for a list of supported configuration names and
  notes on many of the configurations.  You should check the notes in that
  section before proceeding any further with the installation of GCC.

.. toctree::

  configurations-supported-by-gcc

