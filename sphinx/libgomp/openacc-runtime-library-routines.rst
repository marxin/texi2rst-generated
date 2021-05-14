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

  acc_get_num_devices
  acc_set_device_type
  acc_get_device_type
  acc_set_device_num
  acc_get_device_num
  acc_get_property
  acc_async_test
  acc_async_test_all
  acc_wait
  acc_wait_all
  acc_wait_all_async
  acc_wait_async
  acc_init
  acc_shutdown
  acc_on_device
  acc_malloc
  acc_free
  acc_copyin
  acc_present_or_copyin
  acc_create
  acc_present_or_create
  acc_copyout
  acc_delete
  acc_update_device
  acc_update_self
  acc_map_data
  acc_unmap_data
  acc_deviceptr
  acc_hostptr
  acc_is_present
  acc_memcpy_to_device
  acc_memcpy_from_device
  acc_attach
  acc_detach

  API routines for target platforms.

  acc_get_current_cuda_device
  acc_get_current_cuda_context
  acc_get_cuda_stream
  acc_set_cuda_stream

  API routines for the OpenACC Profiling Interface.

  acc_prof_register
  acc_prof_unregister
  acc_prof_lookup
  acc_register_library
  accgetnumdevices---get-number-of-devices-for-given-device-type
  accsetdevicetype---set-type-of-device-accelerator-to-use
  accgetdevicetype---get-type-of-device-accelerator-to-be-used
  accsetdevicenum---set-device-number-to-use
  accgetdevicenum---get-device-number-to-be-used
  accgetproperty---get-device-property
  accasynctest---test-for-completion-of-a-specific-asynchronous-operation
  accasynctestall---tests-for-completion-of-all-asynchronous-operations
  accwait---wait-for-completion-of-a-specific-asynchronous-operation
  accwaitall---waits-for-completion-of-all-asynchronous-operations
  accwaitallasync---wait-for-completion-of-all-asynchronous-operations
  accwaitasync---wait-for-completion-of-asynchronous-operations
  accinit---initialize-runtime-for-a-specific-device-type
  accshutdown---shuts-down-the-runtime-for-a-specific-device-type
  accondevice---whether-executing-on-a-particular-device
  accmalloc---allocate-device-memory
  accfree---free-device-memory
  acccopyin---allocate-device-memory-and-copy-host-memory-to-it
  accpresentorcopyin---if-the-data-is-not-present-on-the-device-allocate-device-memory-and-copy-from-host-memory
  acccreate---allocate-device-memory-and-map-it-to-host-memory
  accpresentorcreate---if-the-data-is-not-present-on-the-device-allocate-device-memory-and-map-it-to-host-memory
  acccopyout---copy-device-memory-to-host-memory
  accdelete---free-device-memory
  accupdatedevice---update-device-memory-from-mapped-host-memory
  accupdateself---update-host-memory-from-mapped-device-memory
  accmapdata---map-previously-allocated-device-memory-to-host-memory
  accunmapdata---unmap-device-memory-from-host-memory
  accdeviceptr---get-device-pointer-associated-with-specific-host-address
  acchostptr---get-host-pointer-associated-with-specific-device-address
  accispresent---indicate-whether-host-variable---array-is-present-on-device
  accmemcpytodevice---copy-host-memory-to-device-memory
  accmemcpyfromdevice---copy-device-memory-to-host-memory
  accattach---let-device-pointer-point-to-device-pointer-target
  accdetach---let-device-pointer-point-to-host-pointer-target
  accgetcurrentcudadevice---get-cuda-device-handle
  accgetcurrentcudacontext---get-cuda-context-handle
  accgetcudastream---get-cuda-stream-handle
  accsetcudastream---set-cuda-stream-handle
  accprofregister---register-callbacks
  accprofunregister---unregister-callbacks
  accproflookup---obtain-inquiry-functions
  accregisterlibrary---library-registration

