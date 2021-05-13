.. _submodel-options:

Machine-Dependent Options
*************************

.. index:: submodel options

.. index:: specifying hardware config

.. index:: hardware models and configurations, specifying

.. index:: target-dependent options

.. index:: machine-dependent options

Each target machine supported by GCC can have its own options-for
example, to allow you to compile for a particular processor variant or
ABI, or to control optimizations specific to that machine.  By
convention, the names of machine-specific options start with
:samp:`-m`.

Some configurations of the compiler also support additional target-specific
options, usually for compatibility with other compilers on the same
platform.

.. This list is ordered alphanumerically by subsection name.
   It should be the same order and spelling as these options are listed
   in Machine Dependent Options

.. toctree::

   <aarch64-options>
   <adapteva-epiphany-options>
   <amd-gcn-options>
   <arc-options>
   <arm-options>
   <avr-options>
   <blackfin-options>
   <c6x-options>
   <cris-options>
   <cr16-options>
   <c-sky-options>
   <darwin-options>
   <dec-alpha-options>
   <ebpf-options>
   <fr30-options>
   <ft32-options>
   <frv-options>
   <gnu-linux-options>
   <h8-300-options>
   <hppa-options>
   <ia-64-options>
   <lm32-options>
   <m32c-options>
   <m32r-d-options>
   <m680x0-options>
   <mcore-options>
   <mep-options>
   <microblaze-options>
   <mips-options>
   <mmix-options>
   <mn10300-options>
   <moxie-options>
   <msp430-options>
   <nds32-options>
   <nios-ii-options>
   <nvidia-ptx-options>
   <openrisc-options>
   <pdp-11-options>
   <picochip-options>
   <powerpc-options>
   <pru-options>
   <risc-v-options>
   <rl78-options>
   <rs-6000-and-powerpc-options>
   <rx-options>
   <s-390-and-zseries-options>
   <score-options>
   <sh-options>
   <solaris-2-options>
   <sparc-options>
   <system-v-options>
   <tile-gx-options>
   <tilepro-options>
   <v850-options>
   <vax-options>
   <visium-options>
   <vms-options>
   <vxworks-options>
   <x86-options>
   <x86-windows-options>
   <xstormy16-options>
   <xtensa-options>
   <zseries-options>

.. toctree::

  aarch64-options
  adapteva-epiphany-options
  amd-gcn-options
  arc-options
  arm-options
  avr-options
  blackfin-options
  c6x-options
  cris-options
  cr16-options
  c-sky-options
  darwin-options
  dec-alpha-options
  ebpf-options
  fr30-options
  ft32-options
  frv-options
  gnu-linux-options
  h8-300-options
  hppa-options
  ia-64-options
  lm32-options
  m32c-options
  m32r-d-options
  m680x0-options
  mcore-options
  mep-options
  microblaze-options
  mips-options
  mmix-options
  mn10300-options
  moxie-options
  msp430-options
  nds32-options
  nios-ii-options
  nvidia-ptx-options
  openrisc-options
  pdp-11-options
  picochip-options
  powerpc-options
  pru-options
  risc-v-options
  rl78-options
  ibm-rs-6000-and-powerpc-options
  rx-options
  s-390-and-zseries-options
  score-options
  sh-options
  solaris-2-options
  sparc-options
  tile-gx-options
  tilepro-options
  v850-options
  vax-options
  visium-options
  vms-options
  vxworks-options
  x86-options
  x86-windows-options
  xstormy16-options
  xtensa-options
  zseries-options

.. _system-v-options:

Options for System V
^^^^^^^^^^^^^^^^^^^^

These additional options are available on System V Release 4 for
compatibility with other compilers on those systems:

.. option:: -G

  Create a shared object.
  It is recommended that :option:`-symbolic` or :option:`-shared` be used instead.

.. option:: -Qy

  Identify the versions of each tool used by the compiler, in a
  ``.ident`` assembler directive in the output.

.. option:: -Qn

  Refrain from adding ``.ident`` directives to the output file (this is
  the default).

.. option:: -YP,dirs, -YP

  Search the directories :samp:`{dirs}`, and no others, for libraries
  specified with :option:`-l`.

.. option:: -Ym,dir, -Ym

  Look in the directory :samp:`{dir}` to find the M4 preprocessor.
  The assembler uses this option.

  .. This is supposed to go with a -Yd for predefined M4 macro files, but

  .. the generic assembler that comes with Solaris takes just -Ym.

