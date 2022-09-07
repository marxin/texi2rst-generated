.. _features:

GNU Modula-2 Features
*********************

* the compiler currently complies with Programming in Modula-2 Edition
  2, 3, 4 and ISO Modula-2.  Users can switch on specific language
  features by using: :samp:`-fpim`, :samp:`-fpim2`, :samp:`-fpim3`,
  :samp:`-fpim4` or :samp:`-fiso`.

* the option :samp:`-fswig` will automatically create a swig interface
  file which corresponds to the definition module of the file being
  compiled.

* exception handling is compatible with C++ and swig.  Modula-2 code can
  be used with C or C++ code.

* Python can call GNU Modula-2 modules via swig.

* shared libraries can be built.

* fixed sized types are now available from :samp:`SYSTEM`.

* support for dynamic ``ARRAY`` s has been added into :samp:`gdb`.

* variables can be declared at addresses.

* much better dwarf-2 debugging support and when used with
  :samp:`gdb` the programmer can display ``RECORD`` s,
  ``ARRAY`` s, ``SET`` s, subranges and constant char literals
  in Modula-2 syntax.

* supports sets of any ordinal size (memory permitting).

* easy interface to C, and varargs can be passed to C routines.

* many Logitech libraries have been implemented and can be accessed via:
  :samp:`-flibs=m2log,m2pim,m2iso`.

* coroutines have been implemented in the PIM style and these are
  accessible from SYSTEM. A number of supporting libraries (executive
  and file descriptor mapping to interrupt vector libraries are
  available through the :samp:`-flibs=m2iso,m2pim` switch).

* can be built as a cross compiler (for embedded microprocessors
  such as the AVR and the ARM).

