.. _environment-variables:

OpenMP Environment Variables
----------------------------

The environment variables which beginning with :envvar:`OMP_` are defined by
section 4 of the OpenMP specification in version 4.5, while those
beginning with :envvar:`GOMP_` are GNU extensions.

.. toctree::

  Set whether cancellation is activated <omp_cancellation>
  Show OpenMP version and environment variables <omp_display_env>
  Set the device used in target regions <omp_default_device>
  Dynamic adjustment of threads <omp_dynamic>
  Set the maximum number of nested parallel regions <omp_max_active_levels>
  Set the maximum task priority value <omp_max_task_priority>
  Nested parallel regions <omp_nested>
  Specifies the number of threads to use <omp_num_threads>
  Whether theads may be moved between CPUs <omp_proc_bind>
  Specifies on which CPUs the theads should be placed <omp_places>
  Set default thread stack size <omp_stacksize>
  How threads are scheduled <omp_schedule>
  Controls offloading behaviour <omp_target_offload>
  Set the maximum number of threads <omp_thread_limit>
  How waiting threads are handled <omp_wait_policy>
  Bind threads to specific CPUs <gomp_cpu_affinity>
  Enable debugging output <gomp_debug>
  Set default thread stack size <gomp_stacksize>
  Set the busy-wait spin count <gomp_spincount>
  Set the RTEMS specific thread pools <gomp_rtems_thread_pools>

.. toctree::

  ompcancellation---set-whether-cancellation-is-activated
  ompdisplayenv---show-openmp-version-and-environment-variables
  ompdefaultdevice---set-the-device-used-in-target-regions
  ompdynamic---dynamic-adjustment-of-threads
  ompmaxactivelevels---set-the-maximum-number-of-nested-parallel-regions
  ompmaxtaskpriority---set-the-maximum-priority
  ompnested---nested-parallel-regions
  ompnumthreads---specifies-the-number-of-threads-to-use
  ompprocbind---whether-theads-may-be-moved-between-cpus
  ompplaces---specifies-on-which-cpus-the-theads-should-be-placed
  ompstacksize---set-default-thread-stack-size
  ompschedule---how-threads-are-scheduled
  omptargetoffload---controls-offloading-behaviour
  ompthreadlimit---set-the-maximum-number-of-threads
  ompwaitpolicy---how-waiting-threads-are-handled
  gompcpuaffinity---bind-threads-to-specific-cpus
  gompdebug---enable-debugging-output
  gompstacksize---set-default-thread-stack-size
  gompspincount---set-the-busy-wait-spin-count
  gomprtemsthreadpools---set-the-rtems-specific-thread-pools

