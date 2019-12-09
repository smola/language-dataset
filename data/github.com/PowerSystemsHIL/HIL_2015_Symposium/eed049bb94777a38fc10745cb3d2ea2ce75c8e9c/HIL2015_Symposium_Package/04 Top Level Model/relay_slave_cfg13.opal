OPAL-1.0 Object
MODBUS::Block {
  name=Modbus_device
  device=modbus_slv
}
MODBUS::Slave {
  name=modbus_slv
  proto_name=TCP
  ip=192.168.10.30
  port=1036
  slave_id=17
  verbose=0
  nb_bits=1
  nb_input_bits=1
  nb_registers=2002
  nb_input_registers=25
  pinout {
		incoming_bits {
			pins {
				item {
					addr=0
				}
			}
	   }
	   	incoming_input_bits {
			pins {
				item {
					addr=0
				}
			}
	   }
	   	incoming_registers {
			pins {
				item {
					addr=0
				}
			}
	   }
	   	incoming_input_registers {
			pins {
				item {
					addr=0
				}
				item {
					addr=1
				}
				item {
					addr=2
				}
				item {
					addr=3
				}
				item {
					addr=4
				}
				item {
					addr=5
				}
				item {
					addr=6
				}
				item {
					addr=7
				}
				item {
					addr=8
				}
				item {
					addr=9
				}
				item {
					addr=10
				}
				item {
					addr=11
				}
				item {
					addr=12
				}
				item {
					addr=13
				}
				item {
					addr=14
				}
				item {
					addr=15
				}
				item {
					addr=16
				}
				item {
					addr=17
				}
				item {
					addr=18
				}
				item {
					addr=19
				}
				item {
					addr=20
				}
				item {
					addr=21
				}
				item {
					addr=22
				}
				item {
					addr=23
				}
				item {
					addr=24
				}
			}
	   }
	   	outgoing_bits {
			pins {
				item {
					addr=0
				}
			}
	   }
	   	outgoing_registers{
			pins {
				item {
					addr=2000
				}
				item {
					addr=2001
				}
			}
	   }
  }
  tasks {
    MODBUS::Task {
      type=mailbox
      deadline=100
    }
  }
}