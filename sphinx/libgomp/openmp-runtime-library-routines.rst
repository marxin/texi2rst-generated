..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _runtime-library-routines:

OpenMP Runtime Library Routines
-------------------------------

The runtime routines described here are defined by Section 3 of the OpenMP
specification in version 4.5.  The routines are structured in following
three parts:

.. toctree::
  :maxdepth: 2

  Control threads, processors and the parallel environment.  They have C
  linkage, and do not throw exceptions.

  openmp-runtime-library-routines/omp_get_active_level
  openmp-runtime-library-routines/omp_get_ancestor_thread_num
  openmp-runtime-library-routines/omp_get_cancellation
  openmp-runtime-library-routines/omp_get_default_device
  openmp-runtime-library-routines/omp_get_device_num
  openmp-runtime-library-routines/omp_get_dynamic
  openmp-runtime-library-routines/omp_get_initial_device
  openmp-runtime-library-routines/omp_get_level
  openmp-runtime-library-routines/omp_get_max_active_levels
  openmp-runtime-library-routines/omp_get_max_task_priority
  openmp-runtime-library-routines/omp_get_max_teams
  openmp-runtime-library-routines/omp_get_max_threads
  openmp-runtime-library-routines/omp_get_nested
  openmp-runtime-library-routines/omp_get_num_devices
  openmp-runtime-library-routines/omp_get_num_procs
  openmp-runtime-library-routines/omp_get_num_teams
  openmp-runtime-library-routines/omp_get_num_threads
  openmp-runtime-library-routines/omp_get_proc_bind
  openmp-runtime-library-routines/omp_get_schedule
  openmp-runtime-library-routines/omp_get_supported_active_levels
  openmp-runtime-library-routines/omp_get_team_num
  openmp-runtime-library-routines/omp_get_team_size
  openmp-runtime-library-routines/omp_get_teams_thread_limit
  openmp-runtime-library-routines/omp_get_thread_limit
  openmp-runtime-library-routines/omp_get_thread_num
  openmp-runtime-library-routines/omp_in_parallel
  openmp-runtime-library-routines/omp_in_final
  openmp-runtime-library-routines/omp_is_initial_device
  openmp-runtime-library-routines/omp_set_default_device
  openmp-runtime-library-routines/omp_set_dynamic
  openmp-runtime-library-routines/omp_set_max_active_levels
  openmp-runtime-library-routines/omp_set_nested
  openmp-runtime-library-routines/omp_set_num_teams
  openmp-runtime-library-routines/omp_set_num_threads
  openmp-runtime-library-routines/omp_set_schedule
  openmp-runtime-library-routines/omp_set_teams_thread_limit

  Initialize, set, test, unset and destroy simple and nested locks.

  openmp-runtime-library-routines/omp_init_lock
  openmp-runtime-library-routines/omp_set_lock
  openmp-runtime-library-routines/omp_test_lock
  openmp-runtime-library-routines/omp_unset_lock
  openmp-runtime-library-routines/omp_destroy_lock
  openmp-runtime-library-routines/omp_init_nest_lock
  openmp-runtime-library-routines/omp_set_nest_lock
  openmp-runtime-library-routines/omp_test_nest_lock
  openmp-runtime-library-routines/omp_unset_nest_lock
  openmp-runtime-library-routines/omp_destroy_nest_lock

  Portable, thread-based, wall clock timer.

  openmp-runtime-library-routines/omp_get_wtick
  openmp-runtime-library-routines/omp_get_wtime

  Support for event objects.

  openmp-runtime-library-routines/omp_fulfill_event
  openmp-runtime-library-routines/ompgetactivelevel
  openmp-runtime-library-routines/ompgetancestorthreadnum
  openmp-runtime-library-routines/ompgetcancellation
  openmp-runtime-library-routines/ompgetdefaultdevice
  openmp-runtime-library-routines/ompgetdevicenum
  openmp-runtime-library-routines/ompgetdynamic
  openmp-runtime-library-routines/ompgetinitialdevice
  openmp-runtime-library-routines/ompgetlevel
  openmp-runtime-library-routines/ompgetmaxactivelevels
  openmp-runtime-library-routines/ompgetmaxtaskpriority
  openmp-runtime-library-routines/ompgetmaxteams
  openmp-runtime-library-routines/ompgetmaxthreads
  openmp-runtime-library-routines/ompgetnested
  openmp-runtime-library-routines/ompgetnumdevices
  openmp-runtime-library-routines/ompgetnumprocs
  openmp-runtime-library-routines/ompgetnumteams
  openmp-runtime-library-routines/ompgetnumthreads
  openmp-runtime-library-routines/ompgetprocbind
  openmp-runtime-library-routines/ompgetschedule
  openmp-runtime-library-routines/ompgetsupportedactivelevels
  openmp-runtime-library-routines/ompgetteamnum
  openmp-runtime-library-routines/ompgetteamsize
  openmp-runtime-library-routines/ompgetteamsthreadlimit
  openmp-runtime-library-routines/ompgetthreadlimit
  openmp-runtime-library-routines/ompgetthreadnum
  openmp-runtime-library-routines/ompinparallel
  openmp-runtime-library-routines/ompinfinal
  openmp-runtime-library-routines/ompisinitialdevice
  openmp-runtime-library-routines/ompsetdefaultdevice
  openmp-runtime-library-routines/ompsetdynamic
  openmp-runtime-library-routines/ompsetmaxactivelevels
  openmp-runtime-library-routines/ompsetnested
  openmp-runtime-library-routines/ompsetnumteams
  openmp-runtime-library-routines/ompsetnumthreads
  openmp-runtime-library-routines/ompsetschedule
  openmp-runtime-library-routines/ompsetteamsthreadlimit
  openmp-runtime-library-routines/ompinitlock
  openmp-runtime-library-routines/ompsetlock
  openmp-runtime-library-routines/omptestlock
  openmp-runtime-library-routines/ompunsetlock
  openmp-runtime-library-routines/ompdestroylock
  openmp-runtime-library-routines/ompinitnestlock
  openmp-runtime-library-routines/ompsetnestlock
  openmp-runtime-library-routines/omptestnestlock
  openmp-runtime-library-routines/ompunsetnestlock
  openmp-runtime-library-routines/ompdestroynestlock
  openmp-runtime-library-routines/ompgetwtick
  openmp-runtime-library-routines/ompgetwtime
  openmp-runtime-library-routines/ompfulfillevent

