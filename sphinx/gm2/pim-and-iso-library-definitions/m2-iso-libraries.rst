.. _m2-iso-libraries:

M2 ISO Libraries
****************

.. README.texi describes the ISO libraries.
    c

This directory contains the ISO definition modules and some
corresponding implementation modules.  The definition files:
:samp:`ChanConsts.def`, :samp:`CharClass.def`, :samp:`ComplexMath.def`,
:samp:`ConvStringLong.def`, :samp:`ConvStringReal.def`,
:samp:`ConvTypes.def`, :samp:`COROUTINES.def`, :samp:`EXCEPTIONS.def`,
:samp:`GeneralUserExceptions.def`, :samp:`IOChan.def`,
:samp:`IOConsts.def`, :samp:`IOLink.def`, :samp:`IOLink.def`,
:samp:`IOResult.def`, :samp:`LongComplexMath.def`, :samp:`LongConv.def`,
:samp:`LongIO.def`, :samp:`LongMath.def`, :samp:`LongStr.def`,
:samp:`LowLong.def`, :samp:`LowReal.def`, :samp:`M2EXCEPTION.def`,
:samp:`Processes.def`, :samp:`ProgramArgs.def`, :samp:`RawIO.def`,
:samp:`RealConv.def`, :samp:`RealIO.def`, :samp:`RealMath.def`,
:samp:`RealStr.def`, :samp:`RndFile.def`, :samp:`Semaphores.def`,
:samp:`SeqFile.def`, :samp:`SIOResult.def`, :samp:`SLongIO.def`,
:samp:`SRawIO.def`, :samp:`SRealIO.def`, :samp:`StdChans.def`,
:samp:`STextIO.def`, :samp:`Storage.def`, :samp:`StreamFile.def`,
:samp:`Strings.def`, :samp:`SWholeIO.def`, :samp:`SysClock.def`,
:samp:`SYSTEM.def`, :samp:`TERMINATION.def`, :samp:`TextIO.def`,
:samp:`WholeConv.def`, :samp:`WholeIO.def` and :samp:`WholeStr.def`
were defined by the International Standard
Information technology - programming languages BS ISO/IEC
10514-1:1996E Part 1: Modula-2, Base Language.

The Copyright to the definition files :samp:`ChanConsts.def`,
:samp:`CharClass.def`, :samp:`ComplexMath.def`,
:samp:`ConvStringLong.def`, :samp:`ConvStringReal.def`,
:samp:`ConvTypes.def`, :samp:`COROUTINES.def`, :samp:`EXCEPTIONS.def`,
:samp:`GeneralUserExceptions.def`, :samp:`IOChan.def`,
:samp:`IOConsts.def`, :samp:`IOLink.def`, :samp:`IOLink.def`,
:samp:`IOResult.def`, :samp:`LongComplexMath.def`, :samp:`LongConv.def`,
:samp:`LongIO.def`, :samp:`LongMath.def`, :samp:`LongStr.def`,
:samp:`LowLong.def`, :samp:`LowReal.def`, :samp:`M2EXCEPTION.def`,
:samp:`Processes.def`, :samp:`ProgramArgs.def`, :samp:`RawIO.def`,
:samp:`RealConv.def`, :samp:`RealIO.def`, :samp:`RealMath.def`,
:samp:`RealStr.def`, :samp:`RndFile.def`, :samp:`Semaphores.def`,
:samp:`SeqFile.def`, :samp:`SIOResult.def`, :samp:`SLongIO.def`,
:samp:`SRawIO.def`, :samp:`SRealIO.def`, :samp:`StdChans.def`,
:samp:`STextIO.def`, :samp:`Storage.def`, :samp:`StreamFile.def`,
:samp:`Strings.def`, :samp:`SWholeIO.def`, :samp:`SysClock.def`,
:samp:`SYSTEM.def`, :samp:`TERMINATION.def`, :samp:`TextIO.def`,
:samp:`WholeConv.def`, :samp:`WholeIO.def` and :samp:`WholeStr.def`
belong to ISO/IEC (International Organization for Standardization and
International Electrotechnical Commission).  The licence allows them
to be distributed with the compiler (as described on page
707 of the Information technology - Programming languages Part 1:
Modula-2, Base Language.  BS ISO/IEC 10514-1:1996).

All implementation modules and :samp:`ClientSocket.def`,
:samp:`LongWholeIO.def`, :samp:`M2RTS.def`, :samp:`MemStream.def`,
:samp:`pth.def`, :samp:`RandomNumber.def`, :samp:`RTdata.def`,
:samp:`RTentity.def`, :samp:`RTfio.def`, :samp:`RTio.def`,
:samp:`ShortComplexMath.def`, :samp:`ShortIO.def`,
:samp:`ShortWholeIO.def`, :samp:`SimpleCipher.def`,
:samp:`SLongWholeIO.def`, :samp:`SShortIO.def`,
:samp:`SShortWholeIO.def`, :samp:`StringChan.def` and
:samp:`wraptime.def` are Copyright of the FSF and are held under the
GPLv3 with runtime exceptions.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
http://www.gnu.org/licenses/.

Notice that GNU Modula-2 contains additional libraries for
input/output of ``SHORTREAL``, ``SHORTCARD``, ``SHORTINT``,
``LONGCARD``, ``LONGINT`` data types.  It also provides a
``RandomNumber``, ``SimpleCipher`` and ``ClientSocket``
modules as well as low level modules which allow the IO libraries to
coexist with their PIM counterparts.

.. toctree::
  :maxdepth: 2

  m2-iso-libraries/gm2-libs-iso-coroutines
  m2-iso-libraries/gm2-libs-iso-chanconsts
  m2-iso-libraries/gm2-libs-iso-charclass
  m2-iso-libraries/gm2-libs-iso-clientsocket
  m2-iso-libraries/gm2-libs-iso-complexmath
  m2-iso-libraries/gm2-libs-iso-convstringlong
  m2-iso-libraries/gm2-libs-iso-convstringreal
  m2-iso-libraries/gm2-libs-iso-convtypes
  m2-iso-libraries/gm2-libs-iso-exceptions
  m2-iso-libraries/gm2-libs-iso-errnocategory
  m2-iso-libraries/gm2-libs-iso-generaluserexceptions
  m2-iso-libraries/gm2-libs-iso-iochan
  m2-iso-libraries/gm2-libs-iso-ioconsts
  m2-iso-libraries/gm2-libs-iso-iolink
  m2-iso-libraries/gm2-libs-iso-ioresult
  m2-iso-libraries/gm2-libs-iso-longcomplexmath
  m2-iso-libraries/gm2-libs-iso-longconv
  m2-iso-libraries/gm2-libs-iso-longio
  m2-iso-libraries/gm2-libs-iso-longmath
  m2-iso-libraries/gm2-libs-iso-longstr
  m2-iso-libraries/gm2-libs-iso-longwholeio
  m2-iso-libraries/gm2-libs-iso-lowlong
  m2-iso-libraries/gm2-libs-iso-lowreal
  m2-iso-libraries/gm2-libs-iso-lowshort
  m2-iso-libraries/gm2-libs-iso-m2exception
  m2-iso-libraries/gm2-libs-iso-m2rts
  m2-iso-libraries/gm2-libs-iso-memstream
  m2-iso-libraries/gm2-libs-iso-preemptive
  m2-iso-libraries/gm2-libs-iso-processes
  m2-iso-libraries/gm2-libs-iso-programargs
  m2-iso-libraries/gm2-libs-iso-rtco
  m2-iso-libraries/gm2-libs-iso-rtdata
  m2-iso-libraries/gm2-libs-iso-rtentity
  m2-iso-libraries/gm2-libs-iso-rtfio
  m2-iso-libraries/gm2-libs-iso-rtgen
  m2-iso-libraries/gm2-libs-iso-rtgenif
  m2-iso-libraries/gm2-libs-iso-rtio
  m2-iso-libraries/gm2-libs-iso-randomnumber
  m2-iso-libraries/gm2-libs-iso-rawio
  m2-iso-libraries/gm2-libs-iso-realconv
  m2-iso-libraries/gm2-libs-iso-realio
  m2-iso-libraries/gm2-libs-iso-realmath
  m2-iso-libraries/gm2-libs-iso-realstr
  m2-iso-libraries/gm2-libs-iso-rndfile
  m2-iso-libraries/gm2-libs-iso-sioresult
  m2-iso-libraries/gm2-libs-iso-slongio
  m2-iso-libraries/gm2-libs-iso-slongwholeio
  m2-iso-libraries/gm2-libs-iso-srawio
  m2-iso-libraries/gm2-libs-iso-srealio
  m2-iso-libraries/gm2-libs-iso-sshortio
  m2-iso-libraries/gm2-libs-iso-sshortwholeio
  m2-iso-libraries/gm2-libs-iso-stextio
  m2-iso-libraries/gm2-libs-iso-swholeio
  m2-iso-libraries/gm2-libs-iso-system
  m2-iso-libraries/gm2-libs-iso-semaphores
  m2-iso-libraries/gm2-libs-iso-seqfile
  m2-iso-libraries/gm2-libs-iso-serversocket
  m2-iso-libraries/gm2-libs-iso-shortcomplexmath
  m2-iso-libraries/gm2-libs-iso-shortio
  m2-iso-libraries/gm2-libs-iso-shortwholeio
  m2-iso-libraries/gm2-libs-iso-simplecipher
  m2-iso-libraries/gm2-libs-iso-stdchans
  m2-iso-libraries/gm2-libs-iso-storage
  m2-iso-libraries/gm2-libs-iso-streamfile
  m2-iso-libraries/gm2-libs-iso-stringchan
  m2-iso-libraries/gm2-libs-iso-strings
  m2-iso-libraries/gm2-libs-iso-sysclock
  m2-iso-libraries/gm2-libs-iso-termination
  m2-iso-libraries/gm2-libs-iso-termfile
  m2-iso-libraries/gm2-libs-iso-textio
  m2-iso-libraries/gm2-libs-iso-wholeconv
  m2-iso-libraries/gm2-libs-iso-wholeio
  m2-iso-libraries/gm2-libs-iso-wholestr
  m2-iso-libraries/gm2-libs-iso-wrapsock
  m2-iso-libraries/gm2-libs-iso-wraptime

