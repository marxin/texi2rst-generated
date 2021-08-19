Introduction
============

I'm a demo documentation page.

.. option:: -Wno-shift-overflow, -Wshift-overflow=n, -Wshift-overflow

  .. note::

    Default value is :option:`-Wno-shift-overflow`, :option:`-Wshift-overflow` is enabled by :option:`-Wall`.

  These options control warnings about left shift overflows.

  ``-Wshift-overflow=1``
    This is the warning level of :option:`-Wshift-overflow` and ...


.. option:: -Wno-shift-overflow2, -Wshift-overflow2=n (non-default), -Wshift-overflow2 (non-default)

  These options control warnings about left shift overflows.

  ``-Wshift-overflow2=1``
    This is the warning level of :option:`-Wshift-overflow2` and ...

.. option:: -Wno-shift-overflow3

  Default option value for :option:`-Wshift-overflow3`.

.. option:: -Wshift-overflow3=n, -Wshift-overflow3

  These options control warnings about left shift overflows.

  ``-Wshift-overflow3=1``
    This is the warning level of :option:`-Wshift-overflow3` and ...

  Enabled by :option:`-Wall`.


.. option:: -Wall

Enable it all :)

I am a :command:`super-command`.
I am **strong** and I am *emphasis*.

Show :samp:`Samp with a {variable}.`

Show it::

  gcc a.c
  ./a.out

Code block:

.. code-block::

  gcc a.c
  ./a.out

Note1: ([#]_)
Note2: ([#]_)

.. [#] Future versions of GCC may zero-extend, or use a target-defined ``ptr_extend`` pattern.  Do not rely on sign extension.
.. [#] I am note 2.