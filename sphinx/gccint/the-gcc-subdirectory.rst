.. _gcc-directory:

The gcc Subdirectory
********************

The gcc directory contains many files that are part of the C
sources of GCC, other files used as part of the configuration and
build process, and subdirectories including documentation and a
testsuite.  The files that are sources of GCC are documented in a
separate chapter.  See :ref:`Passes and Files of the Compiler <passes>`.

.. toctree::

  Subdirectories of gcc. <subdirectories>
  The configuration process, and the files it uses. <configuration>
  The build system in the gcc directory. <build>
  Targets in gcc/Makefile. <makefile>
  Library source files and headers under gcc/. <library-files>
  Headers installed by GCC. <headers>
  Building documentation in GCC. <documentation>
  Anatomy of a language front end. <front-end>
  Anatomy of a target back end. <back-end>

.. _subdirectories:

Subdirectories of gcc
^^^^^^^^^^^^^^^^^^^^^

The gcc directory contains the following subdirectories:

language
  Subdirectories for various languages.  Directories containing a file
  config-lang.in are language subdirectories.  The contents of
  the subdirectories c (for C), cp (for C++),
  objc (for Objective-C), objcp (for Objective-C++),
  and lto (for LTO) are documented in this
  manual (see :ref:`Passes and Files of the Compiler <passes>`);
  those for other languages are not.  See :ref:`Anatomy of a Language Front End <front-end>`, for details of the files in these
  directories.

common
  Source files shared between the compiler drivers (such as
  :command:`gcc`) and the compilers proper (such as cc1).  If an
  architecture defines target hooks shared between those places, it also
  has a subdirectory in common/config.  See :ref:`target-structure`.

config
  Configuration files for supported architectures and operating
  systems.  See :ref:`Anatomy of a Target Back End <back-end>`, for
  details of the files in this directory.

doc
  Texinfo documentation for GCC, together with automatically generated
  man pages and support for converting the installation manual to
  HTML.  See :ref:`documentation`.

ginclude
  System headers installed by GCC, mainly those required by the C
  standard of freestanding implementations.  See :ref:`Headers
  Installed by GCC <headers>`, for details of when these and other headers are
  installed.

po
  Message catalogs with translations of messages produced by GCC into
  various languages, :samp:`{language}`.po.  This directory also
  contains gcc.pot, the template for these message catalogues,
  exgettext, a wrapper around :command:`gettext` to extract the
  messages from the GCC sources and create gcc.pot, which is run
  by :samp:`make gcc.pot`, and EXCLUDES, a list of files from
  which messages should not be extracted.

testsuite
  The GCC testsuites (except for those for runtime libraries).
  See :ref:`testsuites`.

  .. _configuration:

Configuration in the gcc Directory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The gcc directory is configured with an Autoconf-generated
script configure.  The configure script is generated
from configure.ac and aclocal.m4.  From the files
configure.ac and acconfig.h, Autoheader generates the
file config.in.  The file cstamp-h.in is used as a
timestamp.

.. toctree::

  Scripts used by configure. <config-fragments>
  The config.build, config.host, and
                           config.gcc files. <system-config>
  Files created by running configure. <configuration-files>

.. _config-fragments:

Scripts Used by configure
~~~~~~~~~~~~~~~~~~~~~~~~~

configure uses some other scripts to help in its work:

* The standard GNU config.sub and config.guess
  files, kept in the top level directory, are used.

* The file config.gcc is used to handle configuration
  specific to the particular target machine.  The file
  config.build is used to handle configuration specific to the
  particular build machine.  The file config.host is used to handle
  configuration specific to the particular host machine.  (In general,
  these should only be used for features that cannot reasonably be tested in
  Autoconf feature tests.)
  See :ref:`system-config`, for details of the contents of these files.

* Each language subdirectory has a file
  :samp:`{language}` /config-lang.in that is used for
  front-end-specific configuration.  See :ref:`front-end-config`, for details of this file.

* A helper script configure.frag is used as part of
  creating the output of configure.

.. _system-config:

The config.build; config.host; and config.gcc Files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The config.build file contains specific rules for particular systems
which GCC is built on.  This should be used as rarely as possible, as the
behavior of the build system can always be detected by autoconf.

The config.host file contains specific rules for particular systems
which GCC will run on.  This is rarely needed.

The config.gcc file contains specific rules for particular systems
which GCC will generate code for.  This is usually needed.

Each file has a list of the shell variables it sets, with descriptions, at the
top of the file.

FIXME: document the contents of these files, and what variables should
be set to control build, host and target configuration.

.. Copyright (C) 1988-2021 Free Software Foundation, Inc.
   This is part of the GCC manual.
   For copying conditions, see the file gcc.texi.

.. _configuration-files:

Files Created by ``configure``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here we spell out what files will be set up by configure in the
gcc directory.  Some other files are created as temporary files
in the configuration process, and are not used in the subsequent
build; these are not documented.

* Makefile is constructed from Makefile.in, together with
  the host and target fragments (see :ref:`Makefile
  Fragments <fragments>`) t- :samp:`{target}` and x- :samp:`{host}` from
  config, if any, and language Makefile fragments
  :samp:`{language}` /Make-lang.in.

* auto-host.h contains information about the host machine
  determined by configure.  If the host machine is different from
  the build machine, then auto-build.h is also created,
  containing such information about the build machine.

* config.status is a script that may be run to recreate the
  current configuration.

* configargs.h is a header containing details of the arguments
  passed to configure to configure GCC, and of the thread model
  used.

* cstamp-h is used as a timestamp.

* If a language config-lang.in file (see :ref:`front-end-config`) sets ``outputs``, then
  the files listed in ``outputs`` there are also generated.

The following configuration headers are created from the Makefile,
using mkconfig.sh, rather than directly by configure.
config.h, bconfig.h and tconfig.h all contain the
xm- :samp:`{machine}`.h header, if any, appropriate to the host,
build and target machines respectively, the configuration headers for
the target, and some definitions; for the host and build machines,
these include the autoconfigured headers generated by
configure.  The other configuration headers are determined by
config.gcc.  They also contain the typedefs for ``rtx``,
``rtvec`` and ``tree``.

* config.h, for use in programs that run on the host machine.

* bconfig.h, for use in programs that run on the build machine.

* tconfig.h, for use in programs and libraries for the target
  machine.

* tm_p.h, which includes the header :samp:`{machine}` -protos.h
  that contains prototypes for functions in the target
  :samp:`{machine}`.c file.  The
  :samp:`{machine}` -protos.h header is included after the rtl.h
  and/or tree.h would have been included.
  The tm_p.h also
  includes the header tm-preds.h which is generated by
  genpreds program during the build to define the declarations
  and inline functions for the predicate functions.

.. _build:

Build System in the gcc Directory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

FIXME: describe the build system, including what is built in what
stages.  Also list the various source files that are used in the build
process but aren't source files of GCC itself and so aren't documented
below (see :ref:`passes`).

.. Copyright (C) 2001-2021 Free Software Foundation, Inc.
   This is part of the GCC manual.
   For copying conditions, see the file gcc.texi.

.. _makefile:

Makefile Targets
^^^^^^^^^^^^^^^^

.. index:: makefile targets

.. index:: targets, makefile

These targets are available from the :samp:`gcc` directory:

``all``
  This is the default target.  Depending on what your build/host/target
  configuration is, it coordinates all the things that need to be built.

``doc``
  Produce info-formatted documentation and man pages.  Essentially it
  calls :samp:`make man` and :samp:`make info`.

``dvi``
  Produce DVI-formatted documentation.

``pdf``
  Produce PDF-formatted documentation.

``html``
  Produce HTML-formatted documentation.

``man``
  Generate man pages.

``info``
  Generate info-formatted pages.

``mostlyclean``
  Delete the files made while building the compiler.

``clean``
  That, and all the other files built by :samp:`make all`.

``distclean``
  That, and all the files created by :command:`configure`.

``maintainer-clean``
  Distclean plus any file that can be generated from other files.  Note
  that additional tools may be required beyond what is normally needed to
  build GCC.

``srcextra``
  Generates files in the source directory that are not version-controlled but
  should go into a release tarball.

``srcinfo`` ``srcman``
  Copies the info-formatted and manpage documentation into the source
  directory usually for the purpose of generating a release tarball.

``install``
  Installs GCC.

``uninstall``
  Deletes installed files, though this is not supported.

``check``
  Run the testsuite.  This creates a testsuite subdirectory that
  has various .sum and .log files containing the results of
  the testing.  You can run subsets with, for example, :samp:`make check-gcc`.
  You can specify specific tests by setting :envvar:`RUNTESTFLAGS` to be the name
  of the .exp file, optionally followed by (for some tests) an equals
  and a file wildcard, like:

  .. code-block:: c++

    make check-gcc RUNTESTFLAGS="execute.exp=19980413-*"

  Note that running the testsuite may require additional tools be
  installed, such as Tcl or DejaGnu.

  The toplevel tree from which you start GCC compilation is not
the GCC directory, but rather a complex Makefile that coordinates
the various steps of the build, including bootstrapping the compiler
and using the new compiler to build target libraries.

When GCC is configured for a native configuration, the default action
for :command:`make` is to do a full three-stage bootstrap.  This means
that GCC is built three times-once with the native compiler, once with
the native-built compiler it just built, and once with the compiler it
built the second time.  In theory, the last two should produce the same
results, which :samp:`make compare` can check.  Each stage is configured
separately and compiled into a separate directory, to minimize problems
due to ABI incompatibilities between the native compiler and GCC.

If you do a change, rebuilding will also start from the first stage
and 'bubble' up the change through the three stages.  Each stage
is taken from its build directory (if it had been built previously),
rebuilt, and copied to its subdirectory.  This will allow you to, for
example, continue a bootstrap after fixing a bug which causes the
stage2 build to crash.  It does not provide as good coverage of the
compiler as bootstrapping from scratch, but it ensures that the new
code is syntactically correct (e.g., that you did not use GCC extensions
by mistake), and avoids spurious bootstrap comparison
failuresExcept if the compiler was buggy and miscompiled
some of the files that were not modified.  In this case, it's best
to use :command:`make restrap`.

.

Other targets available from the top level include:

``bootstrap-lean``
  Like ``bootstrap``, except that the various stages are removed once
  they're no longer needed.  This saves disk space.

``bootstrap2`` ``bootstrap2-lean``
  Performs only the first two stages of bootstrap.  Unlike a three-stage
  bootstrap, this does not perform a comparison to test that the compiler
  is running properly.  Note that the disk space required by a 'lean'
  bootstrap is approximately independent of the number of stages.

:samp:`stage{N}-bubble ({N} = 1...4, profile, feedback)`
  Rebuild all the stages up to :samp:`{N}`, with the appropriate flags,
  'bubbling' the changes as described above.

:samp:`all-stage{N} ({N} = 1...4, profile, feedback)`
  Assuming that stage :samp:`{N}` has already been built, rebuild it with the
  appropriate flags.  This is rarely needed.

``cleanstrap``
  Remove everything (:samp:`make clean`) and rebuilds (:samp:`make bootstrap`).

``compare``
  Compares the results of stages 2 and 3.  This ensures that the compiler
  is running properly, since it should produce the same object files
  regardless of how it itself was compiled.

``profiledbootstrap``
  Builds a compiler with profiling feedback information.  In this case,
  the second and third stages are named :samp:`profile` and :samp:`feedback`,
  respectively.  For more information, see the installation instructions.

``restrap``
  Restart a bootstrap, so that everything that was not built with
  the system compiler is rebuilt.

:samp:`stage{N}-start ({N} = 1...4, profile, feedback)`
  For each package that is bootstrapped, rename directories so that,
  for example, gcc points to the stage :samp:`{N}` GCC, compiled
  with the stage :samp:`{N-1}` GCCCustomarily, the system compiler
  is also termed the stage0 GCC.

  .

  You will invoke this target if you need to test or debug the
  stage :samp:`{N}` GCC.  If you only need to execute GCC (but you need
  not run :samp:`make` either to rebuild it or to run test suites),
  you should be able to work directly in the stage :samp:`{N}` -gcc
  directory.  This makes it easier to debug multiple stages in
  parallel.

``stage``
  For each package that is bootstrapped, relocate its build directory
  to indicate its stage.  For example, if the gcc directory
  points to the stage2 GCC, after invoking this target it will be
  renamed to stage2-gcc.

  If you wish to use non-default GCC flags when compiling the stage2 and
stage3 compilers, set ``BOOT_CFLAGS`` on the command line when doing
:samp:`make`.

Usually, the first stage only builds the languages that the compiler
is written in: typically, C and maybe Ada.  If you are debugging a
miscompilation of a different stage2 front-end (for example, of the
Fortran front-end), you may want to have front-ends for other languages
in the first stage as well.  To do so, set ``STAGE1_LANGUAGES``
on the command line when doing :samp:`make`.

For example, in the aforementioned scenario of debugging a Fortran
front-end miscompilation caused by the stage1 compiler, you may need a
command like

.. code-block:: c++

  make stage2-bubble STAGE1_LANGUAGES=c,fortran

Alternatively, you can use per-language targets to build and test
languages that are not enabled by default in stage1.  For example,
:command:`make f951` will build a Fortran compiler even in the stage1
build directory.

.. _library-files:

Library Source Files and Headers under the gcc Directory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

FIXME: list here, with explanation, all the C source files and headers
under the gcc directory that aren't built into the GCC
executable but rather are part of runtime libraries and object files,
such as crtstuff.c and unwind-dw2.c.  See :ref:`Headers Installed by GCC <headers>`, for more information about the
ginclude directory.

.. _headers:

Headers Installed by GCC
^^^^^^^^^^^^^^^^^^^^^^^^

In general, GCC expects the system C library to provide most of the
headers to be used with it.  However, GCC will fix those headers if
necessary to make them work with GCC, and will install some headers
required of freestanding implementations.  These headers are installed
in :samp:`{libsubdir}` /include.  Headers for non-C runtime
libraries are also installed by GCC; these are not documented here.
(FIXME: document them somewhere.)

Several of the headers GCC installs are in the ginclude
directory.  These headers, iso646.h,
stdarg.h, stdbool.h, and stddef.h,
are installed in :samp:`{libsubdir}` /include,
unless the target Makefile fragment (see :ref:`target-fragment`)
overrides this by setting ``USER_H``.

In addition to these headers and those generated by fixing system
headers to work with GCC, some other headers may also be installed in
:samp:`{libsubdir}` /include.  config.gcc may set
``extra_headers`` ; this specifies additional headers under
config to be installed on some systems.

GCC installs its own version of ``<float.h>``, from ginclude/float.h.
This is done to cope with command-line options that change the
representation of floating point numbers.

GCC also installs its own version of ``<limits.h>`` ; this is generated
from glimits.h, together with limitx.h and
limity.h if the system also has its own version of
``<limits.h>``.  (GCC provides its own header because it is
required of ISO C freestanding implementations, but needs to include
the system header from its own header as well because other standards
such as POSIX specify additional values to be defined in
``<limits.h>``.)  The system's ``<limits.h>`` header is used via
:samp:`{libsubdir}` /include/syslimits.h, which is copied from
gsyslimits.h if it does not need fixing to work with GCC; if it
needs fixing, syslimits.h is the fixed copy.

GCC can also install ``<tgmath.h>``.  It will do this when
config.gcc sets ``use_gcc_tgmath`` to ``yes``.

.. _documentation:

Building Documentation
^^^^^^^^^^^^^^^^^^^^^^

The main GCC documentation is in the form of manuals in Texinfo
format.  These are installed in Info format; DVI versions may be
generated by :samp:`make dvi`, PDF versions by :samp:`make pdf`, and
HTML versions by :samp:`make html`.  In addition, some man pages are
generated from the Texinfo manuals, there are some other text files
with miscellaneous documentation, and runtime libraries have their own
documentation outside the gcc directory.  FIXME: document the
documentation for runtime libraries somewhere.

.. toctree::

  GCC manuals in Texinfo format. <texinfo-manuals>
  Generating man pages from Texinfo manuals. <man-page-generation>
  Miscellaneous text files with documentation. <miscellaneous-docs>

.. _texinfo-manuals:

Texinfo Manuals
~~~~~~~~~~~~~~~

The manuals for GCC as a whole, and the C and C++ front ends, are in
files doc/*.texi.  Other front ends have their own manuals in
files :samp:`{language}` /*.texi.  Common files
doc/include/*.texi are provided which may be included in
multiple manuals; the following files are in doc/include:

fdl.texi
  The GNU Free Documentation License.

funding.texi
  The section 'Funding Free Software'.

gcc-common.texi
  Common definitions for manuals.

gpl_v3.texi
  The GNU General Public License.

texinfo.tex
  A copy of texinfo.tex known to work with the GCC manuals.

  DVI-formatted manuals are generated by :samp:`make dvi`, which uses
:command:`texi2dvi` (via the Makefile macro ``$(TEXI2DVI)`` ).
PDF-formatted manuals are generated by :samp:`make pdf`, which uses
:command:`texi2pdf` (via the Makefile macro ``$(TEXI2PDF)`` ).  HTML
formatted manuals are generated by :samp:`make html`.  Info
manuals are generated by :samp:`make info` (which is run as part of
a bootstrap); this generates the manuals in the source directory,
using :command:`makeinfo` via the Makefile macro ``$(MAKEINFO)``,
and they are included in release distributions.

Manuals are also provided on the GCC web site, in both HTML and
PostScript forms.  This is done via the script
maintainer-scripts/update_web_docs_git.  Each manual to be
provided online must be listed in the definition of ``MANUALS`` in
that file; a file :samp:`{name}`.texi must only appear once in the
source tree, and the output manual must have the same name as the
source file.  (However, other Texinfo files, included in manuals but
not themselves the root files of manuals, may have names that appear
more than once in the source tree.)  The manual file
:samp:`{name}`.texi should only include other files in its own
directory or in doc/include.  HTML manuals will be generated by
:samp:`makeinfo --html`, PostScript manuals by :command:`texi2dvi`
and :command:`dvips`, and PDF manuals by :command:`texi2pdf`.
All Texinfo files that are parts of manuals must
be version-controlled, even if they are generated files, for the
generation of online manuals to work.

The installation manual, doc/install.texi, is also provided on
the GCC web site.  The HTML version is generated by the script
doc/install.texi2html.

.. _man-page-generation:

Man Page Generation
~~~~~~~~~~~~~~~~~~~

Because of user demand, in addition to full Texinfo manuals, man pages
are provided which contain extracts from those manuals.  These man
pages are generated from the Texinfo manuals using
contrib/texi2pod.pl and :command:`pod2man`.  (The man page for
:command:`g++`, cp/g++.1, just contains a :samp:`.so` reference
to gcc.1, but all the other man pages are generated from
Texinfo manuals.)

Because many systems may not have the necessary tools installed to
generate the man pages, they are only generated if the
configure script detects that recent enough tools are
installed, and the Makefiles allow generating man pages to fail
without aborting the build.  Man pages are also included in release
distributions.  They are generated in the source directory.

Magic comments in Texinfo files starting :samp:`@c man` control what
parts of a Texinfo file go into a man page.  Only a subset of Texinfo
is supported by texi2pod.pl, and it may be necessary to add
support for more Texinfo features to this script when generating new
man pages.  To improve the man page output, some special Texinfo
macros are provided in doc/include/gcc-common.texi which
texi2pod.pl understands:

``@gcctabopt``
  Use in the form :samp:`@table @gcctabopt` for tables of options,
  where for printed output the effect of :samp:`@code` is better than
  that of :samp:`@option` but for man page output a different effect is
  wanted.

``@gccoptlist``
  Use for summary lists of options in manuals.

``@gol``
  Use at the end of each line inside :samp:`@gccoptlist`.  This is
  necessary to avoid problems with differences in how the
  :samp:`@gccoptlist` macro is handled by different Texinfo formatters.

  FIXME: describe the texi2pod.pl input language and magic
comments in more detail.

.. _miscellaneous-docs:

Miscellaneous Documentation
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to the formal documentation that is installed by GCC,
there are several other text files in the gcc subdirectory
with miscellaneous documentation:

ABOUT-GCC-NLS
  Notes on GCC's Native Language Support.  FIXME: this should be part of
  this manual rather than a separate file.

ABOUT-NLS
  Notes on the Free Translation Project.

.. envvar:: COPYING

  The GNU General Public License, Versions 2 and 3.

COPYING.LIB COPYING3.LIB
  The GNU Lesser General Public License, Versions 2.1 and 3.

*ChangeLog* */ChangeLog*
  Change log files for various parts of GCC.

.. envvar:: LANGUAGES

  Details of a few changes to the GCC front-end interface.  FIXME: the
  information in this file should be part of general documentation of
  the front-end interface in this manual.

.. envvar:: ONEWS

  Information about new features in old versions of GCC.  (For recent
  versions, the information is on the GCC web site.)

README.Portability
  Information about portability issues when writing code in GCC.  FIXME:
  why isn't this part of this manual or of the GCC Coding Conventions?

  FIXME: document such files in subdirectories, at least config,
c, cp, objc, testsuite.

.. _front-end:

Anatomy of a Language Front End
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A front end for a language in GCC has the following parts:

* A directory :samp:`{language}` under gcc containing source
  files for that front end.  See :ref:`front-end-directory`, for details.

* A mention of the language in the list of supported languages in
  gcc/doc/install.texi.

* A mention of the name under which the language's runtime library is
  recognized by :option:`--enable-shared`:samp:`={package}` in the
  documentation of that option in gcc/doc/install.texi.

* A mention of any special prerequisites for building the front end in
  the documentation of prerequisites in gcc/doc/install.texi.

* Details of contributors to that front end in
  gcc/doc/contrib.texi.  If the details are in that front end's
  own manual then there should be a link to that manual's list in
  contrib.texi.

* Information about support for that language in
  gcc/doc/frontends.texi.

* Information about standards for that language, and the front end's
  support for them, in gcc/doc/standards.texi.  This may be a
  link to such information in the front end's own manual.

* Details of source file suffixes for that language and :option:`-x
  `:samp:`{lang}` options supported, in gcc/doc/invoke.texi.

* Entries in ``default_compilers`` in gcc.c for source file
  suffixes for that language.

* Preferably testsuites, which may be under gcc/testsuite or
  runtime library directories.  FIXME: document somewhere how to write
  testsuite harnesses.

* Probably a runtime library for the language, outside the gcc
  directory.  FIXME: document this further.

* Details of the directories of any runtime libraries in
  gcc/doc/sourcebuild.texi.

* Check targets in Makefile.def for the top-level Makefile
  to check just the compiler or the compiler and runtime library for the
  language.

If the front end is added to the official GCC source repository, the
following are also necessary:

* At least one Bugzilla component for bugs in that front end and runtime
  libraries.  This category needs to be added to the Bugzilla database.

* Normally, one or more maintainers of that front end listed in
  MAINTAINERS.

* Mentions on the GCC web site in index.html and
  frontends.html, with any relevant links on
  readings.html.  (Front ends that are not an official part of
  GCC may also be listed on frontends.html, with relevant links.)

* A news item on index.html, and possibly an announcement on the
  gcc-announce@gcc.gnu.org mailing list.

* The front end's manuals should be mentioned in
  maintainer-scripts/update_web_docs_git (see :ref:`texinfo-manuals`)
  and the online manuals should be linked to from
  onlinedocs/index.html.

* Any old releases or CVS repositories of the front end, before its
  inclusion in GCC, should be made available on the GCC web site at
  https://gcc.gnu.org/pub/gcc/old-releases/.

* The release and snapshot script maintainer-scripts/gcc_release
  should be updated to generate appropriate tarballs for this front end.

* If this front end includes its own version files that include the
  current date, maintainer-scripts/update_version should be
  updated accordingly.

.. toctree::

  The front end language directory. <front-end-directory>
  The front end config-lang.in file. <front-end-config>
  The front end Make-lang.in file. <front-end-makefile>

.. _front-end-directory:

The Front End :samp:`{language}` Directory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A front end :samp:`{language}` directory contains the source files
of that front end (but not of any runtime libraries, which should be
outside the gcc directory).  This includes documentation, and
possibly some subsidiary programs built alongside the front end.
Certain files are special and other parts of the compiler depend on
their names:

config-lang.in
  This file is required in all language subdirectories.  See :ref:`front-end-config`, for details of
  its contents

Make-lang.in
  This file is required in all language subdirectories.  See :ref:`front-end-makefile`, for details of its
  contents.

lang.opt
  This file registers the set of switches that the front end accepts on
  the command line, and their :option:`--help` text.  See :ref:`options`.

lang-specs.h
  This file provides entries for ``default_compilers`` in
  gcc.c which override the default of giving an error that a
  compiler for that language is not installed.

:samp:`{language}-tree.def`
  This file, which need not exist, defines any language-specific tree
  codes.

  .. _front-end-config:

The Front End config-lang.in File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each language subdirectory contains a config-lang.in file.
This file is a shell script that may define some variables describing
the language:

``language``
  This definition must be present, and gives the name of the language
  for some purposes such as arguments to :option:`--enable-languages`.

``lang_requires``
  If defined, this variable lists (space-separated) language front ends
  other than C that this front end requires to be enabled (with the
  names given being their ``language`` settings).  For example, the
  Obj-C++ front end depends on the C++ and ObjC front ends, so sets
  :samp:`lang_requires="objc c++"`.

``subdir_requires``
  If defined, this variable lists (space-separated) front end directories
  other than C that this front end requires to be present.  For example,
  the Objective-C++ front end uses source files from the C++ and
  Objective-C front ends, so sets :samp:`subdir_requires="cp objc"`.

``target_libs``
  If defined, this variable lists (space-separated) targets in the top
  level Makefile to build the runtime libraries for this
  language, such as ``target-libobjc``.

``lang_dirs``
  If defined, this variable lists (space-separated) top level
  directories (parallel to gcc), apart from the runtime libraries,
  that should not be configured if this front end is not built.

``build_by_default``
  If defined to :samp:`no`, this language front end is not built unless
  enabled in a :option:`--enable-languages` argument.  Otherwise, front
  ends are built by default, subject to any special logic in
  configure.ac (as is present to disable the Ada front end if the
  Ada compiler is not already installed).

``boot_language``
  If defined to :samp:`yes`, this front end is built in stage1 of the
  bootstrap.  This is only relevant to front ends written in their own
  languages.

``compilers``
  If defined, a space-separated list of compiler executables that will
  be run by the driver.  The names here will each end
  with :samp:`\$(exeext)`.

``outputs``
  If defined, a space-separated list of files that should be generated
  by configure substituting values in them.  This mechanism can
  be used to create a file :samp:`{language}` /Makefile from
  :samp:`{language}` /Makefile.in, but this is deprecated, building
  everything from the single gcc/Makefile is preferred.

``gtfiles``
  If defined, a space-separated list of files that should be scanned by
  gengtype.c to generate the garbage collection tables and routines for
  this language.  This excludes the files that are common to all front
  ends.  See :ref:`type-information`.

  .. _front-end-makefile:

The Front End Make-lang.in File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each language subdirectory contains a Make-lang.in file.  It contains
targets ``lang.hook`` (where ``lang`` is the
setting of ``language`` in config-lang.in) for the following
values of ``hook``, and any other Makefile rules required to
build those targets (which may if necessary use other Makefiles
specified in ``outputs`` in config-lang.in, although this is
deprecated).  It also adds any testsuite targets that can use the
standard rule in gcc/Makefile.in to the variable
``lang_checks``.

``all.cross`` ``start.encap`` ``rest.encap``
  FIXME: exactly what goes in each of these targets?

``tags``
  Build an :command:`etags` TAGS file in the language subdirectory
  in the source tree.

``info``
  Build info documentation for the front end, in the build directory.
  This target is only called by :samp:`make bootstrap` if a suitable
  version of :command:`makeinfo` is available, so does not need to check
  for this, and should fail if an error occurs.

``dvi``
  Build DVI documentation for the front end, in the build directory.
  This should be done using ``$(TEXI2DVI)``, with appropriate
  :option:`-I` arguments pointing to directories of included files.

``pdf``
  Build PDF documentation for the front end, in the build directory.
  This should be done using ``$(TEXI2PDF)``, with appropriate
  :option:`-I` arguments pointing to directories of included files.

``html``
  Build HTML documentation for the front end, in the build directory.

``man``
  Build generated man pages for the front end from Texinfo manuals
  (see :ref:`man-page-generation`), in the build directory.  This target
  is only called if the necessary tools are available, but should ignore
  errors so as not to stop the build if errors occur; man pages are
  optional and the tools involved may be installed in a broken way.

``install-common``
  Install everything that is part of the front end, apart from the
  compiler executables listed in ``compilers`` in
  config-lang.in.

``install-info``
  Install info documentation for the front end, if it is present in the
  source directory.  This target should have dependencies on info files
  that should be installed.

``install-man``
  Install man pages for the front end.  This target should ignore
  errors.

``install-plugin``
  Install headers needed for plugins.

``srcextra``
  Copies its dependencies into the source directory.  This generally should
  be used for generated files such as Bison output files which are not
  version-controlled, but should be included in any release tarballs.  This
  target will be executed during a bootstrap if
  :samp:`--enable-generated-files-in-srcdir` was specified as a
  configure option.

``srcinfo`` ``srcman``
  Copies its dependencies into the source directory.  These targets will be
  executed during a bootstrap if :samp:`--enable-generated-files-in-srcdir`
  was specified as a configure option.

``uninstall``
  Uninstall files installed by installing the compiler.  This is
  currently documented not to be supported, so the hook need not do
  anything.

``mostlyclean`` ``clean`` ``distclean`` ``maintainer-clean``
  The language parts of the standard GNU
  :samp:`*clean` targets.  See :ref:`Standard Targets for
  Users <standard-targets>`, for details of the standard
  targets.  For GCC, ``maintainer-clean`` should delete
  all generated files in the source directory that are not version-controlled,
  but should not delete anything that is.

  Make-lang.in must also define a variable ``lang_OBJS``
to a list of host object files that are used by that language.

.. _back-end:

Anatomy of a Target Back End
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A back end for a target architecture in GCC has the following parts:

* A directory :samp:`{machine}` under gcc/config, containing a
  machine description :samp:`{machine}`.md file (see :ref:`Machine Descriptions <machine-desc>`), header files :samp:`{machine}`.h and
  :samp:`{machine}` -protos.h and a source file :samp:`{machine}`.c
  (see :ref:`Target Description Macros and Functions <target-macros>`),
  possibly a target Makefile fragment t- :samp:`{machine}`
  (see :ref:`The Target Makefile Fragment <target-fragment>`), and maybe
  some other files.  The names of these files may be changed from the
  defaults given by explicit specifications in config.gcc.

* If necessary, a file :samp:`{machine}` -modes.def in the
  :samp:`{machine}` directory, containing additional machine modes to
  represent condition codes.  See :ref:`condition-code`, for further details.

* An optional :samp:`{machine}`.opt file in the :samp:`{machine}`
  directory, containing a list of target-specific options.  You can also
  add other option files using the ``extra_options`` variable in
  config.gcc.  See :ref:`options`.

* Entries in config.gcc (see :ref:`system-config`) for the systems with this target
  architecture.

* Documentation in gcc/doc/invoke.texi for any command-line
  options supported by this target (see :ref:`Run-time
  Target Specification <run-time-target>`).  This means both entries in the summary table
  of options and details of the individual options.

* Documentation in gcc/doc/extend.texi for any target-specific
  attributes supported (see :ref:`target-attributes`), including where the
  same attribute is already supported on some targets, which are
  enumerated in the manual.

* Documentation in gcc/doc/extend.texi for any target-specific
  pragmas supported.

* Documentation in gcc/doc/extend.texi of any target-specific
  built-in functions supported.

* Documentation in gcc/doc/extend.texi of any target-specific
  format checking styles supported.

* Documentation in gcc/doc/md.texi of any target-specific
  constraint letters (see :ref:`Constraints for
  Particular Machines <machine-constraints>`).

* A note in gcc/doc/contrib.texi under the person or people who
  contributed the target support.

* Entries in gcc/doc/install.texi for all target triplets
  supported with this target architecture, giving details of any special
  notes about installation for this target, or saying that there are no
  special notes if there are none.

* Possibly other support outside the gcc directory for runtime
  libraries.  FIXME: reference docs for this.  The ``libstdc++`` porting
  manual needs to be installed as info for this to work, or to be a
  chapter of this manual.

The :samp:`{machine}`.h header is included very early in GCC's
standard sequence of header files, while :samp:`{machine}` -protos.h
is included late in the sequence.  Thus :samp:`{machine}` -protos.h
can include declarations referencing types that are not defined when
:samp:`{machine}`.h is included, specifically including those from
rtl.h and tree.h.  Since both RTL and tree types may not
be available in every context where :samp:`{machine}` -protos.h is
included, in this file you should guard declarations using these types
inside appropriate ``#ifdef RTX_CODE`` or ``#ifdef TREE_CODE``
conditional code segments.

If the backend uses shared data structures that require ``GTY`` markers 
for garbage collection (see :ref:`type-information`), you must declare those
in :samp:`{machine}`.h rather than :samp:`{machine}` -protos.h.  
Any definitions required for building libgcc must also go in
:samp:`{machine}`.h.

GCC uses the macro ``IN_TARGET_CODE`` to distinguish between
machine-specific .c and .cc files and
machine-independent .c and .cc files.  Machine-specific
files should use the directive:

.. code-block:: c++

  #define IN_TARGET_CODE 1

before including ``config.h``.

If the back end is added to the official GCC source repository, the
following are also necessary:

* An entry for the target architecture in readings.html on the
  GCC web site, with any relevant links.

* Details of the properties of the back end and target architecture in
  backends.html on the GCC web site.

* A news item about the contribution of support for that target
  architecture, in index.html on the GCC web site.

* Normally, one or more maintainers of that target listed in
  MAINTAINERS.  Some existing architectures may be unmaintained,
  but it would be unusual to add support for a target that does not have
  a maintainer when support is added.

* Target triplets covering all config.gcc stanzas for the target,
  in the list in contrib/config-list.mk.

