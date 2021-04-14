.. _runtime-library-routines:

OpenMP Runtime Library Routines
-------------------------------

The runtime routines described here are defined by Section 3 of the OpenMP
specification in version 4.5.  The routines are structured in following
three parts:

.. toctree::

  Control threads, processors and the parallel environment.  They have C
  linkage, and do not throw exceptions.

  Number of active parallel regions <omp_get_active_level>
  Ancestor thread ID <omp_get_ancestor_thread_num>
  Whether cancellation support is enabled <omp_get_cancellation>
  Get the default device for target regions <omp_get_default_device>
  Dynamic teams setting <omp_get_dynamic>
  Device number of host device <omp_get_initial_device>
  Number of parallel regions <omp_get_level>
  Current maximum number of active regions <omp_get_max_active_levels>
  Maximum task priority value that can be set <omp_get_max_task_priority>
  Maximum number of threads of parallel region <omp_get_max_threads>
  Nested parallel regions <omp_get_nested>
  Number of target devices <omp_get_num_devices>
  Number of processors online <omp_get_num_procs>
  Number of teams <omp_get_num_teams>
  Size of the active team <omp_get_num_threads>
  Whether theads may be moved between CPUs <omp_get_proc_bind>
  Obtain the runtime scheduling method <omp_get_schedule>
  Maximum number of active regions supported <omp_get_supported_active_levels>
  Get team number <omp_get_team_num>
  Number of threads in a team <omp_get_team_size>
  Maximum number of threads <omp_get_thread_limit>
  Current thread ID <omp_get_thread_num>
  Whether a parallel region is active <omp_in_parallel>
  Whether in final or included task region <omp_in_final>
  Whether executing on the host device <omp_is_initial_device>
  Set the default device for target regions <omp_set_default_device>
  Enable/disable dynamic teams <omp_set_dynamic>
  Limits the number of active parallel regions <omp_set_max_active_levels>
  Enable/disable nested parallel regions <omp_set_nested>
  Set upper team size limit <omp_set_num_threads>
  Set the runtime scheduling method <omp_set_schedule>

  Initialize, set, test, unset and destroy simple and nested locks.

  Initialize simple lock <omp_init_lock>
  Wait for and set simple lock <omp_set_lock>
  Test and set simple lock if available <omp_test_lock>
  Unset simple lock <omp_unset_lock>
  Destroy simple lock <omp_destroy_lock>
  Initialize nested lock <omp_init_nest_lock>
  Wait for and set simple lock <omp_set_nest_lock>
  Test and set nested lock if available <omp_test_nest_lock>
  Unset nested lock <omp_unset_nest_lock>
  Destroy nested lock <omp_destroy_nest_lock>

  Portable, thread-based, wall clock timer.

  Get timer precision. <omp_get_wtick>
  Elapsed wall clock time. <omp_get_wtime>

  Support for event objects.

  Fulfill and destroy an OpenMP event. <omp_fulfill_event>

.. toctree::

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

