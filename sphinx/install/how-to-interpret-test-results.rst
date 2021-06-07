..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

How to interpret test results
*****************************

The result of running the testsuite are various *.sum and *.log
files in the testsuite subdirectories.  The *.log files contain a
detailed log of the compiler invocations and the corresponding
results, the *.sum files summarize the results.  These summaries
contain status codes for all tests:

* PASS: the test passed as expected

* XPASS: the test unexpectedly passed

* FAIL: the test unexpectedly failed

* XFAIL: the test failed as expected

* UNSUPPORTED: the test is not supported on this platform

* ERROR: the testsuite detected an error

* WARNING: the testsuite detected a possible problem

It is normal for some tests to report unexpected failures.  At the
current time the testing harness does not allow fine grained control
over whether or not a test is expected to fail.  This problem should
be fixed in future releases.

