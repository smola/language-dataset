with System;
with Comm.TL;       use Comm.TL;
with Comm.Mbox;     use Comm.Mbox;
with STM32.IPCC;    use STM32.IPCC;
with Memory;        use Memory;
with Log;           use Log;

package body Comm.Shci is

   Ble_Init_Cmd_Packet : SHCI_C2_Ble_Init_Cmd_Packet_T :=
     (Header => (others => 0),
      Param => (PBleBufferAddress          => 0,
                BleBufferSize              => 0,
                NumAttrRecord              => CFG_BLE_NUM_GATT_ATTRIBUTES,
                NumAttrServ                => CFG_BLE_NUM_GATT_SERVICES,
                AttrValueArrSize           => CFG_BLE_ATT_VALUE_ARRAY_SIZE,
                NumOfLinks                 => CFG_BLE_NUM_LINK,
                ExtendedPacketLengthEnable => CFG_BLE_DATA_LENGTH_EXTENSION,
                PrWriteListSize            => CFG_BLE_PREPARE_WRITE_LIST_SIZE,
                MblockCount                => CFG_BLE_MBLOCK_COUNT,
                AttMtu                     => CFG_BLE_MAX_ATT_MTU,
                SlaveSca                   => CFG_BLE_SLAVE_SCA,
                MasterSca                  => CFG_BLE_MASTER_SCA,
                LsSource                   => CFG_BLE_LSE_SOURCE,
                MaxConnEventLength         => CFG_BLE_MAX_CONN_EVENT_LENGTH,
                HsStartupTime              => CFG_BLE_HSE_STARTUP_TIME,
                ViterbiEnable              => CFG_BLE_VITERBI_MODE,
                LlOnly                     => CFG_BLE_LL_ONLY,
                HwVersion                  => 0));

   function SHCI_C2_BLE_Init return UInt8
   is
      Code  : Opcode_Field;
      Param : aliased SHCI_C2_Ble_Init_Cmd_Param_T := Ble_Init_Cmd_Packet.Param;
      Sz  : Integer := Param'Size / 8;
      Buff    : UInt8_Array (1 .. Sz);
      for Buff'Address use Param'Address;
      Syscmd : TL_CmdPacket_T
        with Volatile, Address => System'To_Address (16#2003_06e8#);
      EvtPacket : TL_EvtPacket_T;
      SyscmdRsp : DataBuffT
        with Volatile, Address => System'To_Address (16#2003_06e8#);
      for EvtPacket'Address use SyscmdRsp'Address;
      for EvtPacket'Alignment use 1;
      CcEvt : TL_CcEvt_T;
      for CcEvt'Address use SyscmdRsp (1 + (EvtPacket'Size / 8))'Address;
      for CcEvt'Alignment use 1;
   begin
      Code.Result := SHCI_OPCODE_C2_BLE_INIT;
      Syscmd.Cmdserial.Cmd.Cmdcode := Code.Val;
      Syscmd.Cmdserial.Cmd.Plen := UInt8 (Sz);
      for I in 1 .. Sz loop
         Syscmd.Cmdserial.Cmd.Payload (I) := Buff (I);
      end loop;
      Syscmd.Cmdserial.Type_Code := TL_SYSCMD_PKT_TYPE;

      IPCC_Cpu1_EnableReceiveChannel (HW_IPCC_SYSTEM_CMD_RSP_CHANNEL);

      IPCC_Cpu1_SetFlag (HW_IPCC_SYSTEM_CMD_RSP_CHANNEL);

      Suspend_Until_True (IPCC_SYSTEM_EVENT_SO);

      return SyscmdRsp (1 + (EvtPacket'Size / 8) + (CcEvt'Size / 8));

   end SHCI_C2_BLE_Init;

   procedure Initialize_Shci
   is
   begin
      if SHCI_C2_BLE_Init /= 0 then
         raise Program_Error with "SHCI_C2_BLE_Init";
      end if;
      Enable_Log;
   end Initialize_Shci;

end Comm.Shci;
