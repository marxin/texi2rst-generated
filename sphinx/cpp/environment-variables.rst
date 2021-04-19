.. _environment-variables:

Environment Variables
---------------------

.. index:: environment variables

.. man begin ENVIRONMENT

This section describes the environment variables that affect how CPP
operates.  You can use them to specify directories or prefixes to use
when searching for include files, or to control dependency output.

Note that you can also specify places to search using options such as
:option:`-I` , and control dependency output with options like
:option:`-M` (see :ref:`invocation`).  These take precedence over
environment variables, which in turn take precedence over the
configuration of GCC.

.. Copyright (C) 1999-2021 Free Software Foundation, Inc.
   This is part of the CPP and GCC manuals.
   For copying conditions, see the file gcc.texi.
   -
   Environment variables affecting the preprocessor
   -
   If this file is included with the flag ``cppmanual'' set, it is
   formatted for inclusion in the CPP manual; otherwise the main GCC manual.

.. envvar:: CPATHCPATH

  .. Commented out until ObjC++ is part of GCC:
     @itemx OBJCPLUS_INCLUDE_PATH

  Each variable's value is a list of directories separated by a special
  character, much like :envvar:`PATH`, in which to look for header files.
  The special character, ``PATH_SEPARATOR``, is target-dependent and
  determined at GCC build time.  For Microsoft Windows-based targets it is a
  semicolon, and for almost all other targets it is a colon.

  :envvar:`CPATH` specifies a list of directories to be searched as if
  specified with :option:`-I` , but after any paths given with :option:`-I`
  options on the command line.  This environment variable is used
  regardless of which language is being preprocessed.

  The remaining environment variables apply only when preprocessing the
  particular language indicated.  Each specifies a list of directories
  to be searched as if specified with :option:`-isystem` , but after any
  paths given with :option:`-isystem` options on the command line.

  In all these variables, an empty element instructs the compiler to
  search its current working directory.  Empty elements can appear at the
  beginning or end of a path.  For instance, if the value of
  :envvar:`CPATH` is ``:/special/include``, that has the same
  effect as :samp:`-I. -I/special/include`.

  .. man end

  See also Search Path.

  .. man begin ENVIRONMENT

.. envvar:: DEPENDENCIES_OUTPUTDEPENDENCIES_OUTPUT

  .. index:: dependencies for make as output

  If this variable is set, its value specifies how to output
  dependencies for Make based on the non-system header files processed
  by the compiler.  System header files are ignored in the dependency
  output.

  The value of :envvar:`DEPENDENCIES_OUTPUT` can be just a file name, in
  which case the Make rules are written to that file, guessing the target
  name from the source file name.  Or the value can have the form
  :samp:`{file}{target}`, in which case the rules are written to
  file :samp:`{file}` using :samp:`{target}` as the target name.

  In other words, this environment variable is equivalent to combining
  the options :option:`-MM` and :option:`-MF`
  (see :ref:`invocation`),
  with an optional :option:`-MT` switch too.

.. envvar:: SUNPRO_DEPENDENCIESSUNPRO_DEPENDENCIES

  .. index:: dependencies for make as output

  This variable is the same as :envvar:`DEPENDENCIES_OUTPUT` (see above),
  except that system header files are not ignored, so it implies
  :option:`-M` rather than :option:`-MM`.  However, the dependence on the
  main input file is omitted.
  See :ref:`invocation`.

.. envvar:: SOURCE_DATE_EPOCHSOURCE_DATE_EPOCH

  If this variable is set, its value specifies a UNIX timestamp to be
  used in replacement of the current date and time in the ``__DATE__``
  and ``__TIME__`` macros, so that the embedded timestamps become
  reproducible.

  The value of :envvar:`SOURCE_DATE_EPOCH` must be a UNIX timestamp,
  defined as the number of seconds (excluding leap seconds) since
  01 Jan 1970 00:00:00 represented in ASCII; identical to the output of
  ``date +%s`` on GNU/Linux and other systems that support the
  ``%s`` extension in the ``date`` command.

  The value should be a known timestamp such as the last modification
  time of the source or package and it should be set by the build
  process.

.. man end

@c Set file name and title for man page.
@setfilename gfdl
@settitle GNU Free Documentation License
@c man begin SEEALSO
gpl(7), fsf-funding(7).
@c man end
@c man begin COPYRIGHT
Copyright @copyright{} 2000, 2001, 2002, 2007, 2008 Free Software Foundation, Inc.
@uref{http://fsf.org/}

Everyone is permitted to copy and distribute verbatim copies
of this license document, but changing it is not allowed.
@c This file is intended to be included within another document,
@c hence no sectioning command or @node.
@c man end

.. Special handling for inclusion in the install manual.
   man begin DESCRIPTION
    comment For some cases, this default @node/@unnumbered is not applicable and
    comment causes warnings.  In those cases, the including file can set
    comment nodefaultgnufreedocumentationlicensenode and provide it's own version.
    comment F.i., when this file is included in an @raisesections context, the
    comment including file can use an @unnumberedsec.

