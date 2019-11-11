MODULE_NAME='mSim2Projector'(DEV vdvControl, DEV dvRS232)

INCLUDE 'CustomFunctions'

DEFINE_CONSTANT
LONG TLID_POWER_UP = 1

DEFINE_VARIABLE 
LONG TLT_POWER_UP[] = {1000,5000,5000,5000,45000}
CHAR _INPUT

DEFINE_EVENT DATA_EVENT[dvRS232]{
	 ONLINE: SEND_COMMAND dvRS232, 'SET BAUD 19200 N 8 1 485 DISABLE'
}

DEFINE_EVENT DATA_EVENT[vdvControl]{
	 COMMAND:{
		  SWITCH(fnStripCharsRight(REMOVE_STRING(DATA.TEXT,'-',1),1)){
				CASE 'POWER':{
					SWITCH(DATA.TEXT){
						CASE 'OFF':{
							SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$94,$F7,$9D,$01,$00,$00,$00,$00"
							IF(TIMELINE_ACTIVE(TLID_POWER_UP)){
								TIMELINE_KILL(TLID_POWER_UP)
							}
						}
						CASE 'ON':  SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$6B,$E6,$52,$01,$00,$00,$00,$00"
					}
				}
				CASE 'INPUT':{
					_INPUT = DATA.TEXT[1]
					IF(TIMELINE_ACTIVE(TLID_POWER_UP)){
						TIMELINE_KILL(TLID_POWER_UP)
					}
					TIMELINE_CREATE(TLID_POWER_UP,TLT_POWER_UP,LENGTH_ARRAY(TLT_POWER_UP),TIMELINE_ABSOLUTE,TIMELINE_ONCE)
				}
		  }
	 }
}

DEFINE_EVENT TIMELINE_EVENT[TLID_POWER_UP]{
	SWITCH(TIMELINE.SEQUENCE){
		CASE 1:fnSwitchInput(_INPUT)
		CASE 2:SEND_COMMAND vdvControl, 'POWER-ON'
		CASE 3:SEND_COMMAND vdvControl, 'POWER-ON'
		CASE 4:SEND_COMMAND vdvControl, 'POWER-ON'
		CASE 5:fnSwitchInput(_INPUT)
	}
}

DEFINE_FUNCTION fnSwitchInput(CHAR _i){
	SWITCH(_i){
		CASE '1':	SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$80,$E5,$49,$01,$00,$00,$00,$00"
		CASE '2':	SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$B3,$E5,$4A,$01,$00,$00,$00,$00"
		CASE '3':	SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$62,$E4,$4B,$01,$00,$00,$00,$00"
		CASE '4':	SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$D5,$E5,$4C,$01,$00,$00,$00,$00"
		CASE '5':	SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$04,$E4,$4D,$01,$00,$00,$00,$00"
		CASE '6':	SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$37,$E4,$4E,$01,$00,$00,$00,$00"
		CASE '7':	SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$E6,$E5,$4F,$01,$00,$00,$00,$00"
		CASE '8':	SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$89,$E7,$50,$01,$00,$00,$00,$00"
		CASE '9':	SEND_STRING dvRS232, "$BE,$EF,$02,$06,$00,$58,$E6,$51,$01,$00,$00,$00,$00"
	}
}