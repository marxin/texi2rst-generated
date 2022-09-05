..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

References for Other Languages
******************************

See :ref:`gnat_rm:top`, for information on standard
conformance and compatibility of the Ada compiler.

See :ref:`gfortran:standards`, for details
of standards supported by GNU Fortran.

Synopsis
^^^^^^^^

gcc [ :option:`-c` | :option:`-S` | :option:`-E` ] [ :option:`-std`:samp:`={standard}` ]
    [ :option:`-g` ] [ :option:`-pg` ] [ :option:`-O`:samp:`{level}` ]
    [ :option:`-W`:samp:`{warn}`...] [ :option:`-Wpedantic` ]
    [ :option:`-I`:samp:`{dir}`...] [ :option:`-L`:samp:`{dir}`...]
    [ :option:`-D`:samp:`{macro}` [= :samp:`{defn}` ]...] [ :option:`-U`:samp:`{macro}` ]
    [ :option:`-f`:samp:`{option}`...] [ :option:`-m`:samp:`{machine-option}`...]
    [ :option:`-o` :samp:`{outfile}` ] [@ :samp:`{file}` ] :samp:`{infile}`...

Only the most useful options are listed here; see below for the
remainder.  :command:`g++` accepts mostly the same options as :command:`gcc`.
For instructions on reporting bugs, see
|bugurl|.

See the Info entry for :command:`gcc`, or
https://gcc.gnu.org/onlinedocs/gcc/Contributors.html,
for contributors to GCC.

