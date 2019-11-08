MODULE_NAME='GlobalMacros' (DEV TpPressEvent[],DEV TpDoPushEng[],DEV TpDoPushLight[], integer macrosButtons[])

include 'EngineeringChannels'
include 'Keypad_Port_3'

DEFINE_DEVICE

(***********************************************************)
(*               CONSTANT DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_CONSTANT

(***********************************************************)
(*              DATA TYPE DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_TYPE

(***********************************************************)
(*               VARIABLE DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_VARIABLE

(***********************************************************)
(*               LATCHING DEFINITIONS GO BELOW             *)
(***********************************************************)
DEFINE_LATCHING

(***********************************************************)
(*       MUTUALLY EXCLUSIVE DEFINITIONS GO BELOW           *)
(***********************************************************)
DEFINE_MUTUALLY_EXCLUSIVE

(***********************************************************)
(*        SUBROUTINE/FUNCTION DEFINITIONS GO BELOW         *)
(***********************************************************)
(* EXAMPLE: DEFINE_FUNCTION <RETURN_TYPE> <NAME> (<PARAMETERS>) *)
(* EXAMPLE: DEFINE_CALL '<NAME>' (<PARAMETERS>) *)

(***********************************************************)
(*                STARTUP CODE GOES BELOW                  *)
(***********************************************************)
DEFINE_START

(***********************************************************)
(*                THE EVENTS GO BELOW                      *)
(***********************************************************)
DEFINE_EVENT

BUTTON_EVENT[TpPressEvent,macrosButtons[1]]	// Off all system
{    PUSH:{
	do_push(TpDoPushEng[1],macros_AllSystems[1][2])	// air OFF
	do_push(TpDoPushEng[1],macros_AllSystems[2][2])	// floor OFF
	do_push(TpDoPushEng[1],macros_AllSystems[3][2])	// radiator OFF
	do_push(TpDoPushEng[1],macros_AllSystems[4][2])	// windows OFF
	
	do_push(TpDoPushLight[1],PANEL18_BUTTONS[5])	// light OFF
	
}
RELEASE:{}}

DEFINE_PROGRAM

(*****************************************************************)
(*                       END OF PROGRAM                          *)
(*                                                               *)
(*         !!!  DO NOT PUT ANY CODE BELOW THIS COMMENT  !!!      *)
(*                                                               *)
(*****************************************************************)
