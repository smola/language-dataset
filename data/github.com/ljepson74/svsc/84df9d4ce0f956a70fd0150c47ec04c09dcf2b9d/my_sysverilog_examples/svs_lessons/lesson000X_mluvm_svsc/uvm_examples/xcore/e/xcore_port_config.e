/*------------------------------------------------------------------------- 
File name   : xcore_port_config.e
Title       : Deault binding of ports. Used in RTL abstraction level 
Project     : XCore eVC
Developers  : 
Created     : 2008
Description : This file binds the signal ports to external
Notes       : 
--------------------------------------------------------------------------- 
Copyright (c) 2008-2010 Cadence Design Systems,Inc.
  All rights reserved worldwide.

-------------------------------------------------------------------------*/ 

  
<'
package cdn_xcore;

extend xcore_env_u  {
    keep bind(sig_reset, external);
    keep soft sig_reset.hdl_path()            == "xbus_reset"; 
};

extend xcore_monitor_u {
    keep bind(sig_reset, external);
    keep bind(sig_base_addr, external);
    keep bind(sig_flow, external);
    keep bind(sig_halt_int, external);
    keep bind(sig_item_count, external);

    keep soft sig_reset.hdl_path()            == "xbus_reset"; 
    keep soft sig_base_addr.hdl_path()  == "base_addr";  
    keep soft sig_flow.hdl_path()       == "flow"; 
    keep soft sig_halt_int.hdl_path()   == "in_chan_inst/flow_halt_int";  
    keep soft sig_item_count.hdl_path() == "in_chan_inst/item_count";  
}; 
'>

Configuration of xbus ports:

<'
extend xbus_synchronizer_u {
    keep sig_clock.hdl_path() == "xbus_clock";
    keep sig_reset.hdl_path() == "xbus_reset";
};

extend xbus_signal_map_u {
    keep soft sig_start.hdl_path() == "xbus_start"; 
    keep soft sig_addr.hdl_path()  == "xbus_addr"; 
    keep soft sig_size.hdl_path()  == "xbus_size"; 
    keep soft sig_read.hdl_path()  == "xbus_read";
    keep soft sig_write.hdl_path() == "xbus_write"; 
    keep soft sig_bip.hdl_path()   == "xbus_bip"; 
    keep soft sig_wait.hdl_path()  == "xbus_wait"; 
    keep soft sig_error.hdl_path() == "xbus_error"; 
    keep soft sig_data.hdl_path()  == "xbus_data"; 
};

extend xbus_master_signal_map_u {
    keep soft sig_request.hdl_path() == "xbus_request";
    keep soft sig_grant.hdl_path()   == "xbus_grant";
};

'>
