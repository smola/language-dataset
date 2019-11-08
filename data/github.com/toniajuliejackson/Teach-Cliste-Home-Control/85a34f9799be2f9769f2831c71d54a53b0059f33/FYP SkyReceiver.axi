PROGRAM_NAME='FYP SkyReceiver'
(***********************************************************)
(*               VARIABLE DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_VARIABLE
NON_VOLATILE INTEGER nSkyStatus
(***********************************************************)
(*                  THE EVENTS GO BELOW                    *)
(***********************************************************)
DEFINE_EVENT
/*
 * Button event for sky user interface controls
 */
BUTTON_EVENT[dvPanel, 0]{
    PUSH: {
	SWITCH(button.input.channel){
	    CASE cSKY_Keypad_1_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_1]
		appendToFile('log.txt','Sky _01 button pressed')
	    }
	    CASE cSKY_Keypad_2_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_2]
		appendToFile('log.txt','Sky _02 button pressed')
	    }
	    CASE cSKY_Keypad_3_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_3]
		appendToFile('log.txt','Sky _03 button pressed')
	    }
	    CASE cSKY_Keypad_4_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_4]
		appendToFile('log.txt','Sky _04 button pressed')
	    }
	    CASE cSKY_Keypad_5_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_5]
		appendToFile('log.txt','Sky _05 button pressed')
	    }
	    CASE cSKY_Keypad_6_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_6]
		appendToFile('log.txt','Sky _06 button pressed')
	    }
	    CASE cSKY_Keypad_7_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_7]
		appendToFile('log.txt','Sky _07 button pressed')
	    }
	    CASE cSKY_Keypad_8_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_8]
		appendToFile('log.txt','Sky _08 button pressed')
	    }
	    CASE cSKY_Keypad_9_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_9]
		appendToFile('log.txt','Sky _09 button pressed')
	    }
	    CASE cSKY_Keypad_0_Btn: {
		PULSE[dvSKY_IR, cSky_Keypad_0]
		appendToFile('log.txt','Sky _0 button pressed')
	    }
	    CASE cSKY_Channel_Up_Btn: {
		TO[dvSKY_IR, cSky_Channel_Up]
		appendToFile('log.txt','Sky ch. up button pressed')
	    }
	    CASE cSKY_Channel_Down_Btn: {
		TO[dvSKY_IR, cSky_Channel_Down]
		appendToFile('log.txt','Sky ch. down button pressed')
	    }
	    CASE cSKY_Nav_Up_Btn: {
		TO[dvSKY_IR, cSky_Nav_Up]
		appendToFile('log.txt','Sky cursor up button pressed')
	    }
	    CASE cSKY_Nav_Down_Btn: {
		TO[dvSKY_IR, cSky_Nav_Down]
		appendToFile('log.txt','Sky cursor down button pressed')
	    }
	    CASE cSKY_Nav_Left_Btn: {
		TO[dvSKY_IR, cSky_Nav_Left]
		appendToFile('log.txt','Sky cursor left button pressed')
	    }
	    CASE cSKY_Nav_Right_Btn: {
		PULSE[dvSKY_IR, cSky_Nav_Right]
		appendToFile('log.txt','Sky cursor right button pressed')
	    }
	    CASE cSKY_Select_Btn: {
		PULSE[dvSKY_IR, cSky_Select]
		appendToFile('log.txt','Sky cursor select button pressed')
	    }
	    CASE cSKY_Play_Btn: {
		IF (nSkyStatus = PAUSE){
		    PULSE[dvSKY_IR, PLAY]
		    nSkyStatus = PLAY
		    appendToFile('log.txt','Sky play/pause button pressed')
		}
		ELSE {
		    PULSE[dvSKY_IR, PAUSE]
		    nSkyStatus = PAUSE
		    appendToFile('log.txt','Sky play/pause button pressed')
		}
	    }
	    CASE cSKY_Stop_Btn: {
		PULSE[dvSKY_IR, STOP]
		appendToFile('log.txt','Sky stop button pressed')
	    }
	    CASE cSKY_Ffwd_Btn: { 
		TO[dvSKY_IR, cSky_Ffwd]
		appendToFile('log.txt','Sky fwd button pressed')
	    }
	    CASE cSKY_Rewind_Btn: {
		TO[dvSKY_IR,cSky_Rewind]
		appendToFile('log.txt','Sky rewind button pressed')
	    }
	    CASE cSKY_Red_Btn: {
		PULSE[dvSKY_IR, cSky_Red]
		appendToFile('log.txt','Sky red button pressed')
	    }
	    CASE cSKY_Green_Btn: {
		PULSE[dvSKY_IR, cSky_Green]
		appendToFile('log.txt','Sky green button pressed')
	    }
	    CASE cSKY_Yellow_Btn: {
		PULSE[dvSKY_IR, cSky_Yellow]
		appendToFile('log.txt','Sky yellow button pressed')
	    }
	    CASE cSKY_Blue_Btn: {
		PULSE[dvSKY_IR, cSky_Blue]
		appendToFile('log.txt','Sky blue button pressed')
	    }
	    CASE cSKY_Guide_Btn: {
		PULSE[dvSKY_IR, cSky_Guide]
		appendToFile('log.txt','Sky guide button pressed')
	    }
	    CASE cSKY_Help_Btn:{
		PULSE[dvSky_IR, cSky_Services]
		appendToFile('log.txt','Sky help button pressed')
	    }
	    CASE cSKY_Info_Btn:{
		PULSE[dvSky_IR, cSky_Info]
		appendToFile('log.txt','Sky info button pressed')
	    }
	    CASE cSKY_Back_Btn:{
		PULSE[dvSky_IR, cSky_Backup]
		appendToFile('log.txt','Sky back button pressed')
	    }
	}
    }
}
    
/*
 * Data event for the sky to set mode to IR and carrier frequency
 */
DATA_EVENT[dvSky_IR]{
    ONLINE: {
	SEND_COMMAND dvSky_IR, "'SET MODE IR'"
	SEND_COMMAND dvSky_IR, "'CARON'"
	SEND_STRING 0, "'Online'"
    }
    ONERROR: {
	SWITCH(DATA.TEXT){
	    CASE 'NO DEVICE':
		appendToFile('log.txt','Warning: Wiring problem on IR Port #15')
		SEND_COMMAND dvSky_IR, "'^TXT-50,0, Warning: Wiring problem on IR Port #', dvSky_IR.port"
	}
    }
}
