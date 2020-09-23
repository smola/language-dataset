function fakeBattCap(){
    
    if(typeof usleep == "undefined")
        usleep = @encode(void(int))(dlsym(-1,"usleep"))
        
    if(typeof dispatch_async == "undefined")
        dispatch_async = @encode(void (void*,void (^dispatch_block_t)(void)))(dlsym(-1,"dispatch_async"))
        
    if(typeof dispatch_queue_create == "undefined")
        dispatch_queue_create = @encode(void* (char*,void*))(dlsym(-1,"dispatch_queue_create"))
    
    var battMenu = choose(BatteryViewInMenu)[0];
    var currCap = battMenu->_batteryInfo[@"BatteryInfo"][0]->_psDescriptionDictionary[@"Current Capacity"].integerValue
    dispatch_async(dispatch_queue_create("t", 0), ^void(){
        for (var i=currCap;i<=100;i++){
            [battMenu _updateBatteryTextCellWithString:[NSString stringWithFormat:@"%@%%",[NSNumber numberWithInt:i]]];
            [battMenu setNeedsDisplay:YES];
            usleep(40000);
        }
    });
}

fakeBattCap()
