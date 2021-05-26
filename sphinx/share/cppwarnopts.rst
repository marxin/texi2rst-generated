.. option:: -Wcomment, -Wcomments

  Warn whenever a comment-start sequence :samp:`/*` appears in a :samp:`/*`
  comment, or whenever a backslash-newline appears in a :samp:`//` comment.
  This warning is enabled by :option:`-Wall`.

.. option:: -Wtrigraphs

  .. _wtrigraphs:
  Warn if any trigraphs are encountered that might change the meaning of
  the program.  Trigraphs within comments are not warned about,
  except those that would form escaped newlines.

  This option is implied by :option:`-Wall`.  If :option:`-Wall` is not
  given, this option is still enabled unless trigraphs are enabled.  To
  get trigraph conversion without warnings, but get the other
  :option:`-Wall` warnings, use :samp:`-trigraphs -Wall -Wno-trigraphs`.

.. option:: -Wundef, -Wno-undef

  Warn if an undefined identifier is evaluated in an ``#if`` directive.
  Such identifiers are replaced with zero.

.. option:: -Wexpansion-to-defined

  Warn whenever :samp:`defined` is encountered in the expansion of a macro
  (including the case where the macro is expanded by an :samp:`#if` directive).
  Such usage is not portable.
  This warning is also enabled by :option:`-Wpedantic` and :option:`-Wextra`.

.. option:: -Wunused-macros

  Warn about macros defined in the main file that are unused.  A macro
  is :dfn:`used` if it is expanded or tested for existence at least once.
  The preprocessor also warns if the macro has not been used at the
  time it is redefined or undefined.

  Built-in macros, macros defined on the command line, and macros
  defined in include files are not warned about.

  *Note:* If a macro is actually used, but only used in skipped
  conditional blocks, then the preprocessor reports it as unused.  To avoid the
  warning in such a case, you might improve the scope of the macro's
  definition by, for example, moving it into the first skipped block.
  Alternatively, you could provide a dummy use with something like:

  .. code-block:: c++

    #if defined the_macro_causing_the_warning
    #endif

.. option:: -Wno-endif-labels, -Wendif-labels

  Do not warn whenever an ``#else`` or an ``#endif`` are followed by text.
  This sometimes happens in older programs with code of the form

  .. code-block:: c++

    #if FOO
    ...
    #else FOO
    ...
    #endif FOO

  The second and third ``FOO`` should be in comments.
