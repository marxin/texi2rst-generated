.. _gm2-libs-iso-rtdata:

gm2-libs-iso/RTdata
^^^^^^^^^^^^^^^^^^^

.. code-block:: modula2

  DEFINITION MODULE RTdata ;

  (*
      Description: provides a mechanism whereby devices can store
                   data attached to a device.
  *)

  FROM SYSTEM IMPORT ADDRESS ;
  FROM IOLink IMPORT DeviceTablePtr ;

  TYPE
  ModuleId (type)
     ModuleId ;
  FreeProcedure (type)
     FreeProcedure = PROCEDURE (ADDRESS) ;

  (*
     MakeModuleId - creates a unique module Id.
  *)

  MakeModuleId
  PROCEDURE MakeModuleId (VAR m: ModuleId) ;

  (*
     InitData - adds, datum, to the device, d.  The datum
                is associated with ModuleID, m.
  *)

  InitData
  PROCEDURE InitData (d: DeviceTablePtr; m: ModuleId;
                      datum: ADDRESS; f: FreeProcedure) ;

  (*
     GetData - returns the datum assocated with ModuleId, m.
  *)

  GetData
  PROCEDURE GetData (d: DeviceTablePtr; m: ModuleId) : ADDRESS ;

  (*
     KillData - destroys the datum associated with ModuleId, m,
                in device, d.  It invokes the free procedure
                given during InitData.
  *)

  KillData
  PROCEDURE KillData (d: DeviceTablePtr; m: ModuleId) ;

  END RTdata.

