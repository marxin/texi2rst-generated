  .. _umask:

UMASK --- Set the file creation mask
************************************

.. index:: UMASK

.. index:: file system, file creation mask

:samp:`{Description}:`
  Sets the file creation mask to :samp:`{MASK}`. If called as a function, it
  returns the old value. If called as a subroutine and argument :samp:`{OLD}`
  if it is supplied, it is set to the old value. See ``umask(2)``.

:samp:`{Standard}:`
  GNU extension

:samp:`{Class}:`
  Subroutine, function

:samp:`{Syntax}:`
  ============================
  ``CALL UMASK(MASK [, OLD])``
  ``OLD = UMASK(MASK)``
  ============================

:samp:`{Arguments}:`
  ==============  ======================================
  :samp:`{MASK}`  Shall be a scalar of type ``INTEGER``.
  :samp:`{OLD}`   (Optional) Shall be a scalar of type
                  ``INTEGER``.
  ==============  ======================================
