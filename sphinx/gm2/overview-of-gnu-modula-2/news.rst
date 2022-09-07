.. _news:

News
****

``gm2 release numbers``
  are now superceded by gcc branch numbers as gm2
  is tracking the gcc release model.

``accuracy of error messages``
  has been improved and offending subexpressions are now highlighted.

``new type checker``
  has been implemented which will check
  all data types, including recursive procedure parameter types.

``libraries``
  the gm2 driver program has been enhanced to allow third party
  libraries to be installed alongside gm2 libraries.  For example if the
  user specifies library ``foo`` using ``-flibs=foo`` the driver
  will check the standard GCC install directory for a subdirectory
  ``foo``.

``libpth``
  has been removed from gm2 and the coroutines are now implemented using
  the gcc portable threading library.

``libulm``
  the Ulm libraries have been removed from the gm2 tree as they were not
  GPL3.

``Talk given at The GNU Tools Cauldron 2018``
  here is a talk on GNU Modula-2 given at
  https://gcc.gnu.org/wiki/cauldron2018The GNU Tools Cauldron,
  Manchester on 8th September 2018.
  The title is, 'GNU Modula-2 update, catching semantic errors post
  code optimisation and improved debugging'
  [http://floppsie.comp.glam.ac.uk/Papers/paper23/gaius-mulley-gnu-m2.pdfslides
  and https://www.youtube.com/watch?v=6jf6weRuHjkvideo].

``gm2 1.8.2``
  was released on Aug 30th 2018.  gm2-1.8.2 grafts onto gcc-8.2.0
  and contains integer overflow detection for addition, subtraction,
  negation and multiplication.  It also detects and traps when
  a floating point nan occurs.  This is the first release with the
  new semantic checking plugin which checks whether any exception
  will occur post optimization (see -fsoft-check-all).  The compiler
  also works well with the automake tools.

``gm2 1.2.0``
  was released on May 11th 2017.  gm2-1.2.0 grafts onto gcc-5.2.0 and
  supports much better line number accuracy in debugging output.
  Source to code relationship can be further improved by the new option
  -fm2-g.  -fm2-whole-program also provides whole program optimization.

``Talk given at The GNU Tools Cauldron 2016``
  here is a talk on GNU Modula-2 given at
  https://gcc.gnu.org/wiki/cauldron2016The GNU Tools Cauldron,
  Hebden Bridge on 9th September 2016.
  The title is, 'GNU Modula-2 status, whole
  program optimisation and language interoperability'
  [http://floppsie.comp.glam.ac.uk/Papers/paper22/gaius-gcc-cauldron-2016.pdfslides
  and https://www.youtube.com/watch?v=8GMyxwHdr1Evideo].

``gm2 1.1.6``
  was released on February 22nd 2016.  gm2-1.1.6 grafts onto gcc-4.7.4.

``gm2 1.1.5``
  was released on September 3 2015, passes all regression tests and has
  many bug fixes applied.  Arrays and Records can be assigned to and from
  WORD, LOC, BYTE providing sizes permit.  Also a small number of fixes
  to the library module MemStream.mod.  Fixed a number of bugs shown by
  valgrind.

``gm2 1.1.3``
  was released on April 15 2015.  gm2-1.1.3 passes all regression
  tests on Debian Wheezy (x86_64) and (i686).  Also passes all regression tests
  under Debian Jessie (x86_64).  It also builds on armv7l Ubuntu Trusty Tahr.

``gm2 1.1.1``
  was released on January 26 2015.  gm2-1.1.1 passes all regression
  tests on Debian Wheezy (x86_64) and (i686).  Also passes all regression tests
  under Debian Jessie (x86_64).

``gm2 1.1.0``
  was released on January 02 2015.  gm2-1.1.0 passes all regression
  tests on Debian Wheezy (x86_64) and (i686).

``gm2 1.0.9``
  Beta was released on September 23 2014, all regressions passed on
  x86_64 Debian Wheezy.

``gm2 1.0.4``
  was released on September 30 2011.  This is a bug fix release.

``gm2 1.0``
  was released on December 11 2010.

