.. _openmp-modules-omp_lib-and-omp_lib_kinds:

OpenMP Modules OMP_LIB and OMP_LIB_KINDS
****************************************

:samp:`{Standard}:`
  OpenMP Application Program Interface v4.5 and
  OpenMP Application Program Interface v5.0 (partially supported).

  The OpenMP Fortran runtime library routines are provided both in
a form of two Fortran modules, named ``OMP_LIB`` and
``OMP_LIB_KINDS``, and in a form of a Fortran ``include`` file named
omp_lib.h. The procedures provided by ``OMP_LIB`` can be found
in the TopIntroductionlibgompGNU Offloading and Multi
Processing Runtime Library manual,
the named constants defined in the modules are listed
below.

For details refer to the actual
http://www.openmp.org/wp-content/uploads/openmp-4.5.pdfOpenMP Application Program Interface v4.5 and
https://www.openmp.org/wp-content/uploads/OpenMP-API-Specification-5.0.pdfOpenMP Application Program Interface v5.0.

``OMP_LIB_KINDS`` provides the following scalar default-integer
named constants:

omp_allocator_handle_kindomp_alloctrait_key_kindomp_alloctrait_val_kindomp_depend_kindomp_lock_kindomp_lock_hint_kindomp_nest_lock_kindomp_pause_resource_kindomp_memspace_handle_kindomp_proc_bind_kindomp_sched_kindomp_sync_hint_kind``OMP_LIB`` provides the scalar default-integer
named constant ``openmp_version`` with a value of the form
:samp:`{yyyymm}`, where ``yyyy`` is the year and :samp:`{mm}` the month
of the OpenMP version; for OpenMP v4.5 the value is ``201511``.

The following derived type:

omp_alloctraitThe following scalar integer named constants of the
kind ``omp_sched_kind`` :

omp_sched_staticomp_sched_dynamicomp_sched_guidedomp_sched_autoAnd the following scalar integer named constants of the
kind ``omp_proc_bind_kind`` :

omp_proc_bind_falseomp_proc_bind_trueomp_proc_bind_masteromp_proc_bind_closeomp_proc_bind_spreadThe following scalar integer named constants are of the
kind ``omp_lock_hint_kind`` :

omp_lock_hint_noneomp_lock_hint_uncontendedomp_lock_hint_contendedomp_lock_hint_nonspeculativeomp_lock_hint_speculativeomp_sync_hint_noneomp_sync_hint_uncontendedomp_sync_hint_contendedomp_sync_hint_nonspeculativeomp_sync_hint_speculativeAnd the following two scalar integer named constants are of the
kind ``omp_pause_resource_kind`` :

omp_pause_softomp_pause_hardThe following scalar integer named constants are of the kind
``omp_alloctrait_key_kind`` :

omp_atk_sync_hintomp_atk_alignmentomp_atk_accessomp_atk_pool_sizeomp_atk_fallbackomp_atk_fb_dataomp_atk_pinnedomp_atk_partitionThe following scalar integer named constants are of the kind
``omp_alloctrait_val_kind`` :

``omp_alloctrait_key_kind`` :

omp_atv_defaultomp_atv_falseomp_atv_trueomp_atv_contendedomp_atv_uncontendedomp_atv_serializedomp_atv_sequentialomp_atv_privateomp_atv_allomp_atv_threadomp_atv_pteamomp_atv_cgroupomp_atv_default_mem_fbomp_atv_null_fbomp_atv_abort_fbomp_atv_allocator_fbomp_atv_environmentomp_atv_nearestomp_atv_blockedThe following scalar integer named constants are of the kind
``omp_allocator_handle_kind`` :

omp_null_allocatoromp_default_mem_allocomp_large_cap_mem_allocomp_const_mem_allocomp_high_bw_mem_allocomp_low_lat_mem_allocomp_cgroup_mem_allocomp_pteam_mem_allocomp_thread_mem_allocThe following scalar integer named constants are of the kind
``omp_memspace_handle_kind`` :

