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
  openacc-runtime-library-routines/accgetnumdevices-get-number-of-devices-for-given-device-type
  openacc-runtime-library-routines/accsetdevicetype-set-type-of-device-accelerator-to-use
  openacc-runtime-library-routines/accgetdevicetype-get-type-of-device-accelerator-to-be-used
  openacc-runtime-library-routines/accsetdevicenum-set-device-number-to-use
  openacc-runtime-library-routines/accgetdevicenum-get-device-number-to-be-used
  openacc-runtime-library-routines/accgetproperty-get-device-property
  openacc-runtime-library-routines/accasynctest-test-for-completion-of-a-specific-asynchronous-operation
  openacc-runtime-library-routines/accasynctestall-tests-for-completion-of-all-asynchronous-operations
  openacc-runtime-library-routines/accwait-wait-for-completion-of-a-specific-asynchronous-operation
  openacc-runtime-library-routines/accwaitall-waits-for-completion-of-all-asynchronous-operations
  openacc-runtime-library-routines/accwaitallasync-wait-for-completion-of-all-asynchronous-operations
  openacc-runtime-library-routines/accwaitasync-wait-for-completion-of-asynchronous-operations
  openacc-runtime-library-routines/accinit-initialize-runtime-for-a-specific-device-type
  openacc-runtime-library-routines/accshutdown-shuts-down-the-runtime-for-a-specific-device-type
  openacc-runtime-library-routines/accondevice-whether-executing-on-a-particular-device
  openacc-runtime-library-routines/accmalloc-allocate-device-memory
  openacc-runtime-library-routines/accfree-free-device-memory
  openacc-runtime-library-routines/acccopyin-allocate-device-memory-and-copy-host-memory-to-it
  openacc-runtime-library-routines/accpresentorcopyin-if-the-data-is-not-present-on-the-device-allocate-device-memory-and-copy-from-host-memory
  openacc-runtime-library-routines/acccreate-allocate-device-memory-and-map-it-to-host-memory
  openacc-runtime-library-routines/accpresentorcreate-if-the-data-is-not-present-on-the-device-allocate-device-memory-and-map-it-to-host-memory
  openacc-runtime-library-routines/acccopyout-copy-device-memory-to-host-memory
  openacc-runtime-library-routines/accdelete-free-device-memory
  openacc-runtime-library-routines/accupdatedevice-update-device-memory-from-mapped-host-memory
  openacc-runtime-library-routines/accupdateself-update-host-memory-from-mapped-device-memory
  openacc-runtime-library-routines/accmapdata-map-previously-allocated-device-memory-to-host-memory
  openacc-runtime-library-routines/accunmapdata-unmap-device-memory-from-host-memory
  openacc-runtime-library-routines/accdeviceptr-get-device-pointer-associated-with-specific-host-address
  openacc-runtime-library-routines/acchostptr-get-host-pointer-associated-with-specific-device-address
  openacc-runtime-library-routines/accispresent-indicate-whether-host-variable-array-is-present-on-device
  openacc-runtime-library-routines/accmemcpytodevice-copy-host-memory-to-device-memory
  openacc-runtime-library-routines/accmemcpyfromdevice-copy-device-memory-to-host-memory
  openacc-runtime-library-routines/accattach-let-device-pointer-point-to-device-pointer-target
  openacc-runtime-library-routines/accdetach-let-device-pointer-point-to-host-pointer-target
  openacc-runtime-library-routines/accgetcurrentcudadevice-get-cuda-device-handle
  openacc-runtime-library-routines/accgetcurrentcudacontext-get-cuda-context-handle
  openacc-runtime-library-routines/accgetcudastream-get-cuda-stream-handle
  openacc-runtime-library-routines/accsetcudastream-set-cuda-stream-handle
  openacc-runtime-library-routines/accprofregister-register-callbacks
  openacc-runtime-library-routines/accprofunregister-unregister-callbacks
  openacc-runtime-library-routines/accproflookup-obtain-inquiry-functions
  openacc-runtime-library-routines/accregisterlibrary-library-registration

