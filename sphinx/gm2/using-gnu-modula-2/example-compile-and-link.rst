.. _example-usage:

Example compile and link
************************

@c man begin SYNOPSIS gm2
gm2 [@option{-c}|@option{-S}] [@option{-g}] [@option{-pg}]
    [@option{-O}@var{level}] [@option{-W}@var{warn}...]
    [@option{-I}@var{dir}...] [@option{-L}@var{dir}...]
    [@option{-f}@var{option}...] [@option{-m}@var{machine-option}...]
    [@option{-o} @var{outfile}] [@@@var{file}] @var{infile}...

Only the most useful options are listed here; see below for the
remainder.
@c man end
@c man begin SEEALSO
gpl(7), gfdl(7), fsf-funding(7), gcc(1)
and the Info entries for @file{gm2} and @file{gcc}.
@c man end
The :command:`gm2` command is the GNU compiler for the Modula-2 language and
supports many of the same options as :command:`gcc`.  See :ref:`gcc:option-summary`.
This manual only documents the options specific to :command:`gm2`.

This section describes how to compile and link a simple hello world
program.  It provides a few examples of using the different options
mentioned in see :ref:`gm2:compiler-options`.  Assuming that you have
a file called :samp:`hello.mod` in your current directory which
contains:

.. code-block:: modula2

  MODULE hello ;

  FROM StrIO IMPORT WriteString, WriteLn ;

  BEGIN
     WriteString('hello world') ; WriteLn
  END hello.

You can compile and link it by: :samp:`gm2 -g hello.mod`.
The result will be an :samp:`a.out` file created in your directory.

You can split this command into two steps if you prefer.  The compile
step can be achieved by: :samp:`gm2 -g -c -fscaffold-main hello.mod`
and the link via: :samp:`gm2 -g hello.o`.

To see all the compile actions taken by :samp:`gm2` users can also
add the :samp:`-v` flag at the command line, for example:

:samp:`gm2 -v -g -I. hello.mod`

This displays the subprocesses initiated by :samp:`gm2` which can be useful
when trouble shooting.

