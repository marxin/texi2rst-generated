.. _runtime-library-routines:

OpenMP Runtime Library Routines
-------------------------------

The runtime routines described here are defined by Section 3 of the OpenMP
specification in version 4.5.  The routines are structured in following
three parts:

.. toctree::

  Control threads, processors and the parallel environment.  They have C
  linkage, and do not throw exceptions.

  omp_get_active_level
  omp_get_ancestor_thread_num
  omp_get_cancellation
  omp_get_default_device
  omp_get_dynamic
  omp_get_initial_device
  omp_get_level
  omp_get_max_active_levels
  omp_get_max_task_priority
  omp_get_max_threads
  omp_get_nested
  omp_get_num_devices
  omp_get_num_procs
  omp_get_num_teams
  omp_get_num_threads
  omp_get_proc_bind
  omp_get_schedule
  omp_get_supported_active_levels
  omp_get_team_num
  omp_get_team_size
  omp_get_thread_limit
  omp_get_thread_num
  omp_in_parallel
  omp_in_final
  omp_is_initial_device
  omp_set_default_device
  omp_set_dynamic
  omp_set_max_active_levels
  omp_set_nested
  omp_set_num_threads
  omp_set_schedule

  Initialize, set, test, unset and destroy simple and nested locks.

  omp_init_lock
  omp_set_lock
  omp_test_lock
  omp_unset_lock
  omp_destroy_lock
  omp_init_nest_lock
  omp_set_nest_lock
  omp_test_nest_lock
  omp_unset_nest_lock
  omp_destroy_nest_lock

  Portable, thread-based, wall clock timer.

  omp_get_wtick
  omp_get_wtime

  Support for event objects.

  omp_fulfill_event
  ompgetactivelevel---number-of-parallel-regions
  ompgetancestorthreadnum---ancestor-thread-id
  ompgetcancellation---whether-cancellation-support-is-enabled
  ompgetdefaultdevice---get-the-default-device-for-target-regions
  ompgetdynamic---dynamic-teams-setting
  ompgetinitialdevice---return-device-number-of-initial-device
  ompgetlevel---obtain-the-current-nesting-level
  ompgetmaxactivelevels---current-maximum-number-of-active-regions
  ompgetmaxtaskpriority---maximum-priority-value
  ompgetmaxthreads---maximum-number-of-threads-of-parallel-region
  ompgetnested---nested-parallel-regions
  ompgetnumdevices---number-of-target-devices
  ompgetnumprocs---number-of-processors-online
  ompgetnumteams---number-of-teams
  ompgetnumthreads---size-of-the-active-team
  ompgetprocbind---whether-theads-may-be-moved-between-cpus
  ompgetschedule---obtain-the-runtime-scheduling-method
  ompgetsupportedactivelevels---maximum-number-of-active-regions-supported
  ompgetteamnum---get-team-number
  ompgetteamsize---number-of-threads-in-a-team
  ompgetthreadlimit---maximum-number-of-threads
  ompgetthreadnum---current-thread-id
  ompinparallel---whether-a-parallel-region-is-active
  ompinfinal---whether-in-final-or-included-task-region
  ompisinitialdevice---whether-executing-on-the-host-device
  ompsetdefaultdevice---set-the-default-device-for-target-regions
  ompsetdynamic---enable-disable-dynamic-teams
  ompsetmaxactivelevels---limits-the-number-of-active-parallel-regions
  ompsetnested---enable-disable-nested-parallel-regions
  ompsetnumthreads---set-upper-team-size-limit
  ompsetschedule---set-the-runtime-scheduling-method
  ompinitlock---initialize-simple-lock
  ompsetlock---wait-for-and-set-simple-lock
  omptestlock---test-and-set-simple-lock-if-available
  ompunsetlock---unset-simple-lock
  ompdestroylock---destroy-simple-lock
  ompinitnestlock---initialize-nested-lock
  ompsetnestlock---wait-for-and-set-nested-lock
  omptestnestlock---test-and-set-nested-lock-if-available
  ompunsetnestlock---unset-nested-lock
  ompdestroynestlock---destroy-nested-lock
  ompgetwtick---get-timer-precision
  ompgetwtime---elapsed-wall-clock-time
  ompfulfillevent---fulfill-and-destroy-an-openmp-event

