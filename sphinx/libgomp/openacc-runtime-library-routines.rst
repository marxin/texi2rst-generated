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

  Get number of devices for the given device
                                  type. <acc_get_num_devices>
  Set type of device accelerator to use. <acc_set_device_type>
  Get type of device accelerator to be used. <acc_get_device_type>
  Set device number to use. <acc_set_device_num>
  Get device number to be used. <acc_get_device_num>
  Get device property. <acc_get_property>
  Tests for completion of a specific asynchronous
                                  operation. <acc_async_test>
  Tests for completion of all asynchronous
                                  operations. <acc_async_test_all>
  Wait for completion of a specific asynchronous
                                  operation. <acc_wait>
  Waits for completion of all asynchronous
                                  operations. <acc_wait_all>
  Wait for completion of all asynchronous
                                  operations. <acc_wait_all_async>
  Wait for completion of asynchronous operations. <acc_wait_async>
  Initialize runtime for a specific device type. <acc_init>
  Shuts down the runtime for a specific device
                                  type. <acc_shutdown>
  Whether executing on a particular device <acc_on_device>
  Allocate device memory. <acc_malloc>
  Free device memory. <acc_free>
  Allocate device memory and copy host memory to
                                  it. <acc_copyin>
  If the data is not present on the device,
                                  allocate device memory and copy from host
                                  memory. <acc_present_or_copyin>
  Allocate device memory and map it to host
                                  memory. <acc_create>
  If the data is not present on the device,
                                  allocate device memory and map it to host
                                  memory. <acc_present_or_create>
  Copy device memory to host memory. <acc_copyout>
  Free device memory. <acc_delete>
  Update device memory from mapped host memory. <acc_update_device>
  Update host memory from mapped device memory. <acc_update_self>
  Map previously allocated device memory to host
                                  memory. <acc_map_data>
  Unmap device memory from host memory. <acc_unmap_data>
  Get device pointer associated with specific
                                  host address. <acc_deviceptr>
  Get host pointer associated with specific
                                  device address. <acc_hostptr>
  Indicate whether host variable / array is
                                  present on device. <acc_is_present>
  Copy host memory to device memory. <acc_memcpy_to_device>
  Copy device memory to host memory. <acc_memcpy_from_device>
  Let device pointer point to device-pointer target. <acc_attach>
  Let device pointer point to host-pointer target. <acc_detach>

  API routines for target platforms.

  Get CUDA device handle. <acc_get_current_cuda_device>
  Get CUDA context handle. <acc_get_current_cuda_context>
  Get CUDA stream handle. <acc_get_cuda_stream>
  Set CUDA stream handle. <acc_set_cuda_stream>

  API routines for the OpenACC Profiling Interface.

  Register callbacks. <acc_prof_register>
  Unregister callbacks. <acc_prof_unregister>
  Obtain inquiry functions. <acc_prof_lookup>
  Library registration. <acc_register_library>

.. toctree::

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

