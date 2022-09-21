..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _openacc-runtime-library-routines:

OpenACC Runtime Library Routines
--------------------------------

The runtime routines described here are defined by section 3 of the OpenACC
specifications in version 2.6.
They have C linkage, and do not throw exceptions.
Generally, they are available only for the host, with the exception of
``acc_on_device``, which is available for both the host and the
acceleration device.

.. toctree::
  :maxdepth: 2

  openacc-runtime-library-routines/acc_get_num_devices
  openacc-runtime-library-routines/acc_set_device_type
  openacc-runtime-library-routines/acc_get_device_type
  openacc-runtime-library-routines/acc_set_device_num
  openacc-runtime-library-routines/acc_get_device_num
  openacc-runtime-library-routines/acc_get_property
  openacc-runtime-library-routines/acc_async_test
  openacc-runtime-library-routines/acc_async_test_all
  openacc-runtime-library-routines/acc_wait
  openacc-runtime-library-routines/acc_wait_all
  openacc-runtime-library-routines/acc_wait_all_async
  openacc-runtime-library-routines/acc_wait_async
  openacc-runtime-library-routines/acc_init
  openacc-runtime-library-routines/acc_shutdown
  openacc-runtime-library-routines/acc_on_device
  openacc-runtime-library-routines/acc_malloc
  openacc-runtime-library-routines/acc_free
  openacc-runtime-library-routines/acc_copyin
  openacc-runtime-library-routines/acc_present_or_copyin
  openacc-runtime-library-routines/acc_create
  openacc-runtime-library-routines/acc_present_or_create
  openacc-runtime-library-routines/acc_copyout
  openacc-runtime-library-routines/acc_delete
  openacc-runtime-library-routines/acc_update_device
  openacc-runtime-library-routines/acc_update_self
  openacc-runtime-library-routines/acc_map_data
  openacc-runtime-library-routines/acc_unmap_data
  openacc-runtime-library-routines/acc_deviceptr
  openacc-runtime-library-routines/acc_hostptr
  openacc-runtime-library-routines/acc_is_present
  openacc-runtime-library-routines/acc_memcpy_to_device
  openacc-runtime-library-routines/acc_memcpy_from_device
  openacc-runtime-library-routines/acc_attach
  openacc-runtime-library-routines/acc_detach

  API routines for target platforms.

  openacc-runtime-library-routines/acc_get_current_cuda_device
  openacc-runtime-library-routines/acc_get_current_cuda_context
  openacc-runtime-library-routines/acc_get_cuda_stream
  openacc-runtime-library-routines/acc_set_cuda_stream

  API routines for the OpenACC Profiling Interface.

  openacc-runtime-library-routines/acc_prof_register
  openacc-runtime-library-routines/acc_prof_unregister
  openacc-runtime-library-routines/acc_prof_lookup
  openacc-runtime-library-routines/acc_register_library
  openacc-runtime-library-routines/accgetnumdevices
  openacc-runtime-library-routines/accsetdevicetype
  openacc-runtime-library-routines/accgetdevicetype
  openacc-runtime-library-routines/accsetdevicenum
  openacc-runtime-library-routines/accgetdevicenum
  openacc-runtime-library-routines/accgetproperty
  openacc-runtime-library-routines/accasynctest
  openacc-runtime-library-routines/accasynctestall
  openacc-runtime-library-routines/accwait
  openacc-runtime-library-routines/accwaitall
  openacc-runtime-library-routines/accwaitallasync
  openacc-runtime-library-routines/accwaitasync
  openacc-runtime-library-routines/accinit
  openacc-runtime-library-routines/accshutdown
  openacc-runtime-library-routines/accondevice
  openacc-runtime-library-routines/accmalloc
  openacc-runtime-library-routines/accfree
  openacc-runtime-library-routines/acccopyin
  openacc-runtime-library-routines/accpresentorcopyin
  openacc-runtime-library-routines/acccreate
  openacc-runtime-library-routines/accpresentorcreate
  openacc-runtime-library-routines/acccopyout
  openacc-runtime-library-routines/accdelete
  openacc-runtime-library-routines/accupdatedevice
  openacc-runtime-library-routines/accupdateself
  openacc-runtime-library-routines/accmapdata
  openacc-runtime-library-routines/accunmapdata
  openacc-runtime-library-routines/accdeviceptr
  openacc-runtime-library-routines/acchostptr
  openacc-runtime-library-routines/accispresent
  openacc-runtime-library-routines/accmemcpytodevice
  openacc-runtime-library-routines/accmemcpyfromdevice
  openacc-runtime-library-routines/accattach
  openacc-runtime-library-routines/accdetach
  openacc-runtime-library-routines/accgetcurrentcudadevice
  openacc-runtime-library-routines/accgetcurrentcudacontext
  openacc-runtime-library-routines/accgetcudastream
  openacc-runtime-library-routines/accsetcudastream
  openacc-runtime-library-routines/accprofregister
  openacc-runtime-library-routines/accprofunregister
  openacc-runtime-library-routines/accproflookup
  openacc-runtime-library-routines/accregisterlibrary

