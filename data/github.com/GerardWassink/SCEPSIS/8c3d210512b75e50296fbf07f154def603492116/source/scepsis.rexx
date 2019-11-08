#!/usr/bin/rexx
/* Rexx */

/* -------------------------------------------------------------------------- */
/* Program name:    scepsis.rexx                                              */
/* Author:          Gerard Wassink                                            */
/* Date:            July 2019                                                 */
	versionString = "1.3.6a"
/* Purpose:         Teach peeople about simple CPU's and microcode            */
/*                                                                            */
/* History:                                                                   */
/*   v1.0     First release                                                   */
/*   v1.1     Added saving and reloading memory contents                      */
/*   v1.1.1   Correction in mem load, added hidden index feature              */
/*            plus some cleaning and housekeeping                             */
/*   v1.1.2   Added code for ALU operations and some flags                    */
/*   v1.1.4   Fun with flags, finished work on setting the flags              */
/*   v1.1.5   Implemented Control signals for conditioanal jumps              */
/*   v1.1.6   Implemented examples of conditioanal jumps in langdef file      */
/*   v1.1.7   Code for the 'I' command to exceute one instruction             */
/*   v1.1.8   Code for running the program until HLT condition reached        */
/*   v1.1.9   Fixed various little bugs                                       */
/*            - fixed error: running program went past end of memory          */
/*            - ENTER now works as the "S" command in the control panel       */
/*   v1.2     Wrapped everything up to be a working program including all     */
/*            functionality above.                                            */
/*   v1.3     Suppress animation of steps when Running                        */
/*            Correction of bug in flag setting                               */
/*            Built in animate level R(un), I(nstruction), S(tep)             */
/*   v1.3.1   Built in possibility to (P)rocess control signals               */
/*   v1.3.2   Solved issues:                                                  */
/*            #22 - Superfluous LH command                                    */
/*            #23 - LC output                                                 */
/*            #24 - LS command check                                          */
/*   v1.3.3   make memory 64K (65536 bytes)                                   */
/*   v1.3.6   improve the animate function                                    */
/*   v1.3.6a  fixed Issue #35, restored compatibility with oorexx             */
/*                                                                            */
/* -------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------- */
/* ----- Initialize screen control and color Control values ----------------- */
/* -------------------------------------------------------------------------- */
Globals:
	color.black = 30; color.red     = 31; color.green = 32; color.yellow = 33
	color.blue  = 34; color.magenta = 35; color.cyan  = 36; color.white  = 37
	color.brightblack  = 90; color.brightred    = 91; color.brightgreen   = 92
	color.brightyellow = 93; color.brightblue   = 94; color.brightmagenta = 95
	color.brightcyan   = 96; color.brightwhite  = 97
	color.reset = 0
	ESC = d2c(27) || "["
	LF = d2c(10)


/* -------------------------------------------------------------------------- */
/* ----- Control the machine functions --------------------- controlPanel --- */
/* -------------------------------------------------------------------------- */
Main:
	Call Initialize

	choice = ""
	errorMsg = ""
	Do Until choice = "X"
		Call controlPanelDisplay(errorMsg)
		choice = Strip(Upper(linein()))				/* get next command ----- */
		Parse Var choice command value
		If (choice =="") Then choice = "S"
		errorMsg = ""
		Select

			/* -------------------------------------------------------------- */
			/* ----- Component commands ------------------------------------- */
			/* -------------------------------------------------------------- */
			When Wordpos(command, Components) > 0 Then Do	/* exists? ------ */
				If isHex(value) Then Do						/* hex value? --- */
					value = x2d(value)						/* make decimal - */
					If (value <= 65535) Then Do				/* value OK ? --- */
						Interpret "comp_" || command "=" value	/* SET it --- */
					End; Else Do
						errorMsg = "Value for " || command || " too large"
					End
				End; Else Do 
					errorMsg = "Value for " || command || " not hexadecimal"
				End
			End

			/* -------------------------------------------------------------- */
			/* ----- Control Signal commands -------------------------------- */
			/* -------------------------------------------------------------- */
			When Wordpos(command, ctlSignals) > 0 Then Do	/* Exists? ------ */
				If isBin(value) Then Do						/* Binary? ------ */
					If (value == 0 | value == 1) Then Do	/* value OK ? --- */
						Interpret "cs_" || command "=" value	/* SET it --- */
					End; Else Do
						errorMsg = "Value for " || command || " not a binary digit"
					End
				End; Else Do 
					errorMsg = "Value for " || command || " not binary"
				End
			End
			
			/* -------------------------------------------------------------- */
			/* ----- Emulator commands -------------------------------------- */
			/* -------------------------------------------------------------- */
			When choice == "P"  Then	Call processCtlSignals
			When choice == "S"  Then	Call emulateStep
			When choice == "I"  Then	Call emulateInstruction
			When choice == "R"  Then 	Call emulateRun
			
			When choice == "C"  Then	Call emulatorReset
			
			/* -------------------------------------------------------------- */
			/* ----- Memory commands ---------------------------------------- */
			/* -------------------------------------------------------------- */
			When choice == "MEM" Then Do
				Call handleMemory
			End
			When choice == "INIT"  Then	Do
				Call initMemory
				errorMsg = "Memory initialized"
			End
			When choice == "SAVE"  Then	Do
				Call saveMemory
				errorMsg = "Memory saved"
			End
			When choice == "LOAD"  Then	Do
				Call loadMemory
				errorMsg = "Memory loaded"
			End

			
			
			/* -------------------------------------------------------------- */
			/* ----- Miscelaneous commands ---------------------------------- */
			/* -------------------------------------------------------------- */
			When choice == "LC"  Then	Call listComponents
			When choice == "LS"  Then	Call listControlSignals
			When choice == "INS" Then	Call handleInstructions
			When choice == "?"   Then	Call CPhelpInfo
			When choice == "IX"  Then	Call indexMe	/* hidden feature --- */
			When choice == "X"   Then	Do
				choice = ""
				Leave
			End
			Otherwise Do
				errorMsg = "Invalid choice: " || choice 
			End
		End
	End

	Call endProgram

Exit



/* -------------------------------------------------------------------------- */
/* ----- Display the machine functions -------------- controlPanelDisplay --- */
/* -------------------------------------------------------------------------- */
controlPanelDisplay:
	Parse Arg message
	
	hdr =  "SCEPSIS - Simple CPU Emulator Program (Student Instruction System)"
	Call screenHeader hdr
	
	Call Display  4  3 color.brightwhite "Components --------------"
	Call Display  5  3 color.brightwhite "PCT"
	Call Display  5  8 color.brightcyan  Right("0000"||D2X(comp_PCT),4)
	Call Display  5 13 color.brightwhite "INR"
	Call Display  5 17 color.brightcyan  Right("0000"||D2X(comp_INR),4)
	Call Display  5 22 color.brightwhite "STC"
	Call Display  5 27 color.brightcyan  Right("0000"||D2X(comp_STC),4)
	
	Call Display  6  3 color.brightwhite "MAR"
	Call Display  6  8 color.brightcyan  Right("0000"||D2X(comp_MAR),4)
	Call Display  6 13 color.brightwhite "INP"
	Call Display  6 17 color.brightcyan  Right("0000"||D2X(comp_INP),4)
	Call Display  6 22 color.brightwhite "OUT"
	Call Display  6 27 color.brightcyan  Right("0000"||D2X(comp_OUT),4)
	
	Call Display  7  3 color.brightwhite "REGA"
	Call Display  7  8 color.brightcyan  Right("0000"||D2X(comp_REGA),4)
	Call Display  7 13 color.brightwhite "SP"
	Call Display  7 17 color.brightcyan  Right("0000"||D2X(comp_SP),4)
	Call Display  7 22 color.brightwhite "AOPR"
	Call Display  7 27 color.brightcyan  Right("0000"||D2X(comp_AOPR),4)
	
	Call Display  8  3 color.brightwhite "REGB"
	Call Display  8  8 color.brightcyan  Right("0000"||D2X(comp_REGB),4)
	
	Call Display  9  3 color.brightwhite "REGC"
	Call Display  9  8 color.brightcyan  Right("0000"||D2X(comp_REGC),4)
	
	Call Display 10  3 color.brightwhite "Flag-CZELG"
	flgs = C_flag||Z_flag||EQ_flag||LT_flag||GT_flag
	Call Display 11  8 color.brightcyan  flgs
	
	Call Display  4 33 color.brightwhite "Control Signals -----------------"
	Call Display  5 33 color.brightwhite "CE"
	Call Display  5 38 color.brightcyan  cs_CE
	Call Display  5 42 color.brightwhite "CE2"
	Call Display  5 47 color.brightcyan  cs_CE2
	Call Display  5 51 color.brightwhite "INPO"
	Call Display  5 56 color.brightcyan  cs_INPO
	Call Display  5 60 color.brightwhite "OUTI"
	Call Display  5 65 color.brightcyan  cs_OUTI

	Call Display  6 33 color.brightwhite "INRI"
	Call Display  6 38 color.brightcyan  cs_INRI
	Call Display  6 42 color.brightwhite "INRO"
	Call Display  6 47 color.brightcyan  cs_INRO
	Call Display  6 51 color.brightwhite "MARI"
	Call Display  6 56 color.brightcyan  cs_MARI
	Call Display  6 60 color.brightwhite "MARO"
	Call Display  6 65 color.brightcyan  cs_MARO

	Call Display  7 33 color.brightwhite "PCTI"
	Call Display  7 38 color.brightcyan  cs_PCTI
	Call Display  7 42 color.brightwhite "PCTO"
	Call Display  7 47 color.brightcyan  cs_PCTO
	Call Display  7 51 color.brightwhite "MEMI"
	Call Display  7 56 color.brightcyan  cs_MEMI
	Call Display  7 60 color.brightwhite "MEMO"
	Call Display  7 65 color.brightcyan  cs_MEMO

	Call Display  8 33 color.brightwhite "RGAI"
	Call Display  8 38 color.brightcyan  cs_RGAI
	Call Display  8 42 color.brightwhite "RGAO"
	Call Display  8 47 color.brightcyan  cs_RGAO
	Call Display  8 51 color.brightwhite "RGBI"
	Call Display  8 56 color.brightcyan  cs_RGBI
	Call Display  8 60 color.brightwhite "RGBO"
	Call Display  8 65 color.brightcyan  cs_RGBO

	Call Display  9 33 color.brightwhite "RGCI"
	Call Display  9 38 color.brightcyan  cs_RGCI
	Call Display  9 42 color.brightwhite "RGCO"
	Call Display  9 47 color.brightcyan  cs_RGCO
	Call Display  9 51 color.brightwhite "SPI"
	Call Display  9 56 color.brightcyan  cs_SPI
	Call Display  9 60 color.brightwhite "SPD"
	Call Display  9 65 color.brightcyan  cs_SPD

	Call Display 10 33 color.brightwhite "STKI"
	Call Display 10 38 color.brightcyan  cs_STKI
	Call Display 10 42 color.brightwhite "STKO"
	Call Display 10 47 color.brightcyan  cs_STKO
	Call Display 10 51 color.brightwhite "ALUI"
	Call Display 10 56 color.brightcyan  cs_ALUI
	Call Display 10 60 color.brightwhite "EXC"
	Call Display 10 65 color.brightcyan  cs_EXC

	Call Display 11 33 color.brightwhite "SPCC"
	Call Display 11 38 color.brightcyan  cs_SPCC
	Call Display 11 42 color.brightwhite "SPCZ"
	Call Display 11 47 color.brightcyan  cs_SPCZ
	Call Display 11 51 color.brightwhite "SPCE"
	Call Display 11 56 color.brightcyan  cs_SPCE
	Call Display 11 60 color.brightwhite "HLT"
	Call Display 11 65 color.brightcyan  cs_HLT
	
	Call Display 12 33 color.brightwhite "SPCL"
	Call Display 12 38 color.brightcyan  cs_SPCL
	Call Display 12 42 color.brightwhite "SPCG"
	Call Display 12 47 color.brightcyan  cs_SPCG


	Call Display 13  3 color.brightwhite "Commands ---------------"
	Call Display 14  3 color.brightwhite "LC"
	Call Display 14  7 color.brightcyan  "List components"
	Call Display 15  3 color.brightwhite "LS"
	Call Display 15  7 color.brightcyan  "List Control Signals"

	Call Display 13 33 color.brightwhite "------------------------"
	Call Display 14 33 color.brightwhite "MEM"
	Call Display 14 37 color.brightcyan  "Handle Memory"
	Call Display 15 33 color.brightwhite "INS"
	Call Display 15 37 color.brightcyan  "Handle Instructions"
	Call Display 16 33 color.brightwhite "INIT SAVE LOAD"
	Call Display 16 48 color.brightcyan  "Memory"

/* -------------------------------------------------------------------------- */
/* ----- Display info about the next microcode step to be excuted ----------- */
/* -------------------------------------------------------------------------- */
	oPtr = findOpcd(Right("00"||D2X(comp_INR),2))	/* get INR instruction -- */
	If (oPtr > 0) Then Do 							/* does it exist? ------- */
		Oc   = instr.oPtr.1							/* get opcode ----------- */
		Mn   = instr.oPtr.2							/* get mnemonic --------- */
		Ag   = instr.oPtr.2.1						/* get argument --------- */
		Ao   = c2x(BitAnd(x2c(Oc), x2c("07")))		/* get ALU operation ---- */
		Signals = ""		/* walk through and display every control Signal  */
		Do csC = 1 to instr.oPtr.3.comp_STC.0
			cS = instr.oPtr.3.comp_STC.csC
			Signals = Signals || " " || cS
		End
		Call Display 19 11 color.brightred "next: x"||Oc Mn Ag "(ALU opr:"||Ao||") step" comp_STC || ":" Signals "                                                                    "
	End; Else Do
		message = "Unknown instruction" Right("00"||D2X(comp_INR),2) "at PCT address"
	End
	
	Call Display 18  3 color.brightwhite "X"
	Call Display 18  5 color.brightcyan  "Exit"
	Call Display 18 11 color.brightwhite "P"
	Call Display 18 13 color.brightcyan  "CtlSig"
	Call Display 18 21 color.brightwhite "S"
	Call Display 18 23 color.brightcyan  "Step"
	Call Display 18 29 color.brightwhite "I"
	Call Display 18 31 color.brightcyan  "Instr"
	Call Display 18 38 color.brightwhite "R"
	Call Display 18 40 color.brightcyan  "Run"
	Call Display 18 45 color.brightwhite "C"
	Call Display 18 47 color.brightcyan  "Reset"
	Call Display 18 67 color.brightwhite "?"
	Call Display 18 69 color.brightcyan  "Help info"
	
	If Strip(message) <> "" Then Do
		Call Display 21 1 color.brightwhite "===>" message
	End
	Call Display  2 6 color.brightwhite
Return


/* -------------------------------------------------------------------------- */
/* ----- Run the program until HLT --------------------------- emulateRun --- */
/* -------------------------------------------------------------------------- */
emulateRun:
	Do Until (cs_HLT == 1)
		Call emulateInstruction
	End
											/*   Display control panel ------ */
	If (Animate) == "R" Then Call controlPanelDisplay("Step")
	
Return


/* -------------------------------------------------------------------------- */
/* ----- Emulate one instruction --------------------- emulateInstruction --- */
/* -------------------------------------------------------------------------- */
emulateInstruction:
	If (cs_HLT == 1) Then Do
		errorMsg = "System in HLT condition; stopped"
		Return
	End
	
	If (comp_PCT > (memSize - 1)) Then Do			/* Check memory bounds -- */
		errorMsg = "Program running past end of memory, HLT raised"
		cs_HLT = 1									/* Raise HLT condition -- */
		Return
	End
	
							/* get instruction from memory at PCT location -- */
	If (comp_STC == 1) Then Do
		comp_INR = MEM.comp_PCT						/* fill INR ------------- */
	End
										/* search opcode in instruction table */
	OpcodePtr = findOpcd(Right("00"||D2X(comp_INR),2))
	If (OpcodePtr == 0) Then Do 					/* Does it exist? ------- */
		errorMsg = "operation exception, FOUND" Right("00"||D2X(comp_INR),2) 
		errorMsg = errorMsg "at location" Right("00"||D2X(comp_PCT),2)
	End; Else Do
		Opcode   = instr.OpcodePtr.1				/* get opcode ----------- */
		Mnemonic = instr.OpcodePtr.2				/* get mnemonic --------- */
		OpALUopr = C2D(BitAnd(x2c(Opcode), x2c("07"))) /* get ALU operation - */
		
					/* walk through all microcode steps for this instruction  */
		Do mcsCtr = 1 to instr.OpcodePtr.3.0
			Call clrCtlSignals				/* Clear previous control signals */
					/* walk through and set all control Signals for this step */
			Do csCtr = 1 to instr.OpcodePtr.3.mcsCtr.0
				ctrlSignal = instr.OpcodePtr.3.mcsCtr.csCtr
				Interpret "cs_"||ctrlSignal "=" 1
			End
											/* The heart of the machine ----- */
											/*   process the ctrl signals --- */
			Call processCtlSignals
											/*   Display control panel ------ */
			If Animate == "S" Then Call controlPanelDisplay(errorMsg)
											/* reset step counter ----------- */
			If (comp_STC > instr.OpcodePtr.3.0) Then comp_STC = 1
		End
	End
											/*   Display control panel ------ */
	If (Animate) == "I" Then Call controlPanelDisplay(errorMsg)
	
Return


/* -------------------------------------------------------------------------- */
/* ----- Emulate one microcode Step ------------------------- emulateStep --- */
/* -------------------------------------------------------------------------- */
emulateStep:
	If (cs_HLT == 1) Then Do
		errorMsg = "System in HLT condition; stopped"
		Return
	End

	Call clrCtlSignals						/* Clear previous control signals */
							/* get instruction from memory at PCT location -- */
	If (comp_STC == 1) Then Do
		comp_INR = MEM.comp_PCT						/* fill INR ------------- */
	End
										/* search opcode in instruction table */
	OpcodePtr = findOpcd(Right("00"||D2X(comp_INR),2))
	If (OpcodePtr == 0) Then Do 					/* Does it exist? ------- */
		errorMsg = "operation exception, FOUND" Right("00"||D2X(comp_INR),2) 
		errorMsg = errorMsg "at location" Right("00"||D2X(comp_PCT),2)
	End; Else Do
		Opcode   = instr.OpcodePtr.1				/* get opcode ----------- */
		Mnemonic = instr.OpcodePtr.2				/* get mnemonic --------- */
		OpALUopr = C2D(BitAnd(x2c(Opcode), x2c("07"))) /* get ALU operation - */
		
					/* walk through and set all control Signals for this step */
		Do csCtr = 1 to instr.OpcodePtr.3.comp_STC.0
			ctrlSignal = instr.OpcodePtr.3.comp_STC.csCtr
			Interpret "cs_"||ctrlSignal "=" 1
		End
											/* The heart of the machine ----- */
											/*   process the ctrl signals --- */
		Call processCtlSignals
											/* reset step counter ----------- */
		If (comp_STC > instr.OpcodePtr.3.0) Then comp_STC = 1
	End
											/*   Display control panel ------ */
	If (Animate) == "S" Then Call controlPanelDisplay("Step")
	
Return


/* -------------------------------------------------------------------------- */
/* ----- Process the Control Signals ------------------ ProcessCtlSignals --- */
/* -------------------------------------------------------------------------- */
ProcessCtlSignals:
											/* output to DAB processed first  */
	If (cs_INPO == 1)	Then DAB = comp_INP
	If (cs_INRO == 1)	Then DAB = comp_INR
	If (cs_MARO == 1)	Then DAB = comp_MAR
	If (cs_PCTO == 1)	Then DAB = comp_PCT
	If (cs_RGAO == 1)	Then DAB = comp_REGA
	If (cs_RGBO == 1)	Then DAB = comp_REGB
	If (cs_RGCO == 1)	Then DAB = comp_REGC
	If (cs_FTCH == 1)	Then DAB = MEM.comp_PCT
											/* process two bytes from memory  */
	If (cs_MEMO == 1)	Then Do
		temp_MAR = comp_MAR + 1
		DAB = (256 * MEM.comp_MAR) + (MEM.temp_MAR)
	End
	If (cs_STKO == 1)	Then Do
		temp_SP = comp_SP + 1
		DAB = (256 * MEM.comp_SP) + (MEM.temp_SP)
	End

											/* input from DAB processed 2nd   */
	If (cs_OUTI == 1)	Then Do
		comp_OUT     = DAB
											/*   Display control panel ------ */
		If (Animate == 'R') Then Call controlPanelDisplay("output:")
	End
	If (cs_INRI == 1)	Then comp_INR     = DAB
	If (cs_MARI == 1)	Then comp_MAR     = DAB
	If (cs_PCTI == 1)	Then comp_PCT     = DAB
	If (cs_RGAI == 1)	Then comp_REGA    = DAB
	If (cs_RGBI == 1)	Then comp_REGB    = DAB
	If (cs_RGCI == 1)	Then comp_REGC    = DAB
	If (cs_ALUI == 1)	Then comp_AOPR    = DAB
											/* process two bytes for memory   */
	If (cs_MEMI == 1)	Then Do
		temp_MAR = comp_MAR + 1
		MEM.comp_MAR = Trunc(DAB/256)
		MEM.temp_MAR = DAB - (256 * (MEM.comp_MAR))
	End
	If (cs_STKI == 1)	Then Do
		temp_SP = comp_SP + 1
		MEM.comp_SP = Trunc(DAB / 256)
		MEM.temp_SP = DAB - (256 * (MEM.comp_SP))
	End

													/* Conditional Jumps ---- */
	If (cs_SPCC == 1) & (C_flag == 1) Then comp_PCT = DAB 		/* On Carry - */
	If (cs_SPCZ == 1) & (Z_flag == 1) Then comp_PCT = DAB 		/* On Zero -- */
	If (cs_SPCE == 1) & (EQ_flag == 1) Then comp_PCT = DAB 		/* On Equal - */
	If (cs_SPCL == 1) & (LT_flag == 1) Then comp_PCT = DAB 		/* On Less -- */
	If (cs_SPCG == 1) & (GT_flag == 1) Then comp_PCT = DAB 		/* On Greater */

											/* Stack pointer increment ------ */
	If (cs_SPI == 1)	Then Do
		If (comp_SP < (memSize - 2)) Then Do
			comp_SP      = comp_SP + 2
		End; Else Do
			errorMsg = "S0C4 - SP increment invalid"
		End
	End
											/* Stack pointer decrement ------ */
	If (cs_SPD == 1)	Then Do
		If (comp_SP > (memSize - 256)) Then Do
			comp_SP      = comp_SP - 2
		End; Else Do
			errorMsg = "S0C4 - SP decrement invalid"
		End
	End
											/* Execute ALU operation -------- */
	If (cs_EXC   == 1)	Then Do	
		/* ------------------------------------------------------------------ */
		/* Premise at this time is that in previous microcode steps the ----- */
		/* ALU operand (comp_ALUO) has been filled with the desired value --- */
		/* ------------------------------------------------------------------ */
		Select
			
			When OpALUopr == 0 Then Do								/* ADD -- */
				comp_REGA = comp_REGA + comp_AOPR
				C_flag = 0; Z_flag = 0						/* reset flags -- */
				If comp_REGA > (memsize - 1) Then Do
					comp_REGA = comp_REGA - (memsize - 1)
					C_flag = 1					/* set Carry flag when needed */
				End
				If comp_REGA == 0 Then Z_flag = 1			/* set Zero flag  */
			End
			
			When OpALUopr == 1 Then Do							/* SUBTRACT - */
				comp_REGA = comp_REGA - comp_AOPR
				C_flag = 0; Z_flag = 0; EQ_flag = 0			/* reset flags -- */
				If comp_REGA < 0 Then Do
					comp_REGA = comp_REGA + (memsize - 1)
					C_flag = 1					/* set Carry flag when needed */
				End
				If comp_REGA == 0 Then Do
					Z_flag = 1								/* set Zero flag  */
					EQ_flag = 1								/* set Equal flag */
				End
			End
			
			When OpALUopr == 2 Then Do							/* COMPARE -- */
				GT_flag = 0; LT_flag = 0; EQ_flag = 0		/* reset flags -- */
				If (comp_REGA > comp_AOPR) Then Do
					GT_flag = 1							/* Greater than ----- */
				End; Else Do
					If (comp_REGA < comp_AOPR) Then Do
						LT_flag = 1						/* Less than -------- */
					End; Else Do
						EQ_flag = 1						/* Equal to --------- */
					End
				End
			End
			
			When OpALUopr == 3 Then Do								/* AND -- */
				Z_flag = 0									/* reset flags -- */
				comp_REGA = c2d(BitAnd(d2c(comp_REGA), d2c(comp_AOPR)))
				If comp_REGA == 0 Then Z_flag = 1			/* set Zero flag  */
			End
			
			When OpALUopr == 4 Then Do								/* OR --- */
				Z_flag = 0									/* reset flags -- */
				comp_REGA = c2d(BitOr(d2c(comp_REGA), d2c(comp_AOPR)))
				If comp_REGA == 0 Then Z_flag = 1			/* set Zero flag  */
			End
			
			When OpALUopr == 5 Then Do								/* XOR -- */
				Z_flag = 0									/* reset flags -- */
				comp_REGA = c2d(BitXor(d2c(comp_REGA), d2c(comp_AOPR)))
				If comp_REGA == 0 Then Z_flag = 1			/* set Zero flag  */
			End
			
			Otherwise Do
				errorMsg = "Invalid ALU operation" OpALUopr
			End
		End
	End
											/* Count Enable, bump PCT         */
	If (cs_CE   == 1)	Then comp_PCT = comp_PCT + 1
	If (cs_CE2  == 1)	Then comp_PCT = comp_PCT + 2
	
	comp_STC = comp_STC + 1					/* Next microcode step            */
Return


/* -------------------------------------------------------------------------- */
/* ----- Clear all control signals ------------------------ clrCtlSignals --- */
/* -------------------------------------------------------------------------- */
clrCtlSignals:
	Do i = 1 to Words(ctlSignals)
		c = Word(ctlSignals, i)
		Interpret "cs_"||c "=" 0
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- Handle Memory related stuff ----------------------- handleMemory --- */
/* -------------------------------------------------------------------------- */
handleMemory:
	memChoice = ""
	memMsg = ""
	startOfScreen = 0
	Do Until memChoice = "X"
		memChoice = Upper(strip(listMemory(memMsg)))
		Parse Var memChoice command value
		memMsg = ""

		Select
			/* -------------------------------------------------------------- */
			/* ----- Memory commands ---------------------------------------- */
			/* -------------------------------------------------------------- */
			When command == "D" Then Do			/* display memory from address*/
				If (value == "") 
					Then startOfScreen = startOfScreen + 512
					Else startOfScreen = 512*Trunc(x2d(value)/512)
			End
			
			When command == "M" Then	Do		/* fill memory with values -- */
				Parse Var value adr vals
				If isHex(adr) Then Do
					adr = x2d(adr)
					Do i = 1 To Words(vals)
						Parse Var vals val vals
						val = Strip(val)
						If isHex(val) Then Do
							val = x2d(val)
							If (adr <= memSize) Then Do
								If (val <= 255) Then Do
									MEM.adr = val
									memMsg = "Value(s) entered into memory"
									adr = adr + 1	/* in case there's more   */
								End; Else Do
									memMsg = "Value:" d2x(val) "> 255"
								End
							End; Else Do
								memMsg = "Address" d2x(adr) "> memSize"
							End
						End; Else Do
							memMsg = "Value for MEM value not HEXa-decimal"
						End
					End
				End; Else Do
					memMsg = "Value for MEM address not HEXa-decimal"
				End
			End

			When command == "INIT"  Then	Do
				Call initMemory
				memMsg = "Memory initialized"
			End

			When command == "SAVE"  Then	Do
				Call saveMemory
			End

			When command == "LOAD"  Then	Do
				Call loadMemory
			End

			Otherwise Do
				memMsg = "Invalid choice: " || memChoice 
			End
		End
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- List Memory in hex dump format----------------------- listMemory --- */
/* -------------------------------------------------------------------------- */
listMemory:
	pMem = startOfScreen
	endOfScreen = startOfScreen + 511

	Call screenHeader "SCEPSIS - memory from " || d2x(startofScreen) || " to " || d2x(endOfScreen)
	
							/* --- Display contents of memory in neat ------- */
							/* ---   little groups, 32 bytes per row -------- */
	lnum = 3



	line = Right("0000"||d2x(pMem),4) || ": "
	
	Do pMem = startOfScreen to (endOfScreen)
		If pMem > startOfScreen Then Do
			Select
				When ((pMem // 32) == 0) Then Do
					Call Display lnum 2 color.cyan line
					lnum = lnum + 1
					line = Right("0000"||d2x(pMem),4) || ": "
				End
				When ((pMem // 16) == 0) Then line = line || " - "
				When ((pMem //  8) == 0) Then line = line || " "
				When ((pMem //  4) == 0) Then line = line || " "
				Otherwise Nop
			End
		End
		line = line || Right("00"||d2x(MEM.pMem),2)
	End
	Call Display lnum 2 color.cyan line
	lnum = lnum + 1


	Call Display 20  3 color.brightwhite "X"
	Call Display 20  5 color.brightcyan "return"
	Call Display 20 13 color.brightwhite "D"
	Call Display 20 15 color.brightcyan "{adr}"
	Call Display 20 22 color.brightwhite "M"
	Call Display 20 24 color.brightcyan "{adr} {val ...}"
	Call Display 20 41 color.brightwhite "INIT"
	Call Display 20 46 color.brightwhite "SAVE"
	Call Display 20 51 color.brightwhite "LOAD"
	Call Display 20 56 color.brightcyan  "Memory"
		
	
	If Strip(memMsg) <> "" Then Do
		Call Display 21 1 color.brightwhite "===>" memMsg
	End
	Call Display  2 6 color.brightwhite
	memChoice = Strip(Upper(linein()))
Return memChoice


/* -------------------------------------------------------------------------- */
/* ----- Save Memory in hex format --------------------------- saveMemory --- */
/* -------------------------------------------------------------------------- */
saveMemory:
	lnum = 1; p = 0						/* save from location 0, count lines  */
	line = ""
	memFile = "./scepsis.memory"
	If Stream(memFile, 'C', 'OPEN WRITE') = "READY:" Then Do
		Do p = 0 to memSize - 1
			If p > 0 Then Do
				If ((p // 32) == 0) Then Do
						lc = Lineout(memFile, line, lnum)
						lnum = lnum + 1
						line = ""
				End
			End
			line = line || Right("00"||d2x(MEM.p),2)
		End
		lc = Lineout(memFile, line, lnum)
		lnum = lnum + 1
		line = ""
		memMsg = "Wrote" lnum "lines to" memFile
	End; Else Do
		memMsg = "Error opening file" memFile
		Exit 8
	End

Return


/* -------------------------------------------------------------------------- */
/* ----- Load Memory in hex format --------------------------- loadMemory --- */
/* -------------------------------------------------------------------------- */
loadMemory:
	lnum = 0; p = 0						/* load from location 0, count lines  */
	line = ""
	regel = ""
	memFile = "./scepsis.memory"
	If Stream(memFile, 'C', 'OPEN READ') = "READY:" Then Do
		Do While Lines(memFile)
			line = Strip(Upper(Linein(memFile)))
			lnum = lnum + 1
			llen = Length(line)
			If (llen > 0) Then Do
				Do lp = 1 To llen By 2
					MEM.p = X2D(Substr(line, lp,2))
					p = p + 1
				End
			End
		End
		memMsg = "Loaded" lnum "lines from" memFile
	End; Else Do
		memMsg = "Error opening file" memFile
		Exit 8
	End

Return


/* -------------------------------------------------------------------------- */
/* ----- Check whether a value is bin or not ------------------- checkBin --- */
/* -------------------------------------------------------------------------- */
isBin:
	Parse Arg possibleBin
	rval = 1
	Do h = 1 to Length(possibleBin)
		If Pos(Substr(possibleBin,h,1), "01") == 0 Then Do
			rval = 0
			Leave
		End
	End
Return rval


/* -------------------------------------------------------------------------- */
/* ----- Check whether a value is hex or not ------------------- checkHex --- */
/* -------------------------------------------------------------------------- */
isHex:
	Parse Arg possibleHex
	rval = 1
	Do h = 1 to Length(possibleHex)
		If Pos(Substr(possibleHex,h,1), "0123456789ABCDEF") == 0 Then Do
			rval = 0
			Leave
		End
	End
Return rval


/* -------------------------------------------------------------------------- */
/* --------------------------------------------------- handleInstructions --- */
/* -------------------------------------------------------------------------- */
/* ----- Show Instructions and display individual ones ---------------------- */
/* -------------------------------------------------------------------------- */
handleInstructions:
	lstIChoice = ""
	lstIMsg = ""
	liMsg = ""
	Do Until lstIChoice = "X"
		lstIChoice = Upper(strip(listInstructions(lstIMsg)))
		Parse Var lstIChoice command value
		lstIMsg = ""

		Select
			/* -------------------------------------------------------------- */
			/* ----- Instruction commands ----------------------------------- */
			/* -------------------------------------------------------------- */
			When command == "D" Then Do
				Parse Var value opc .
				opc = Strip(opc)
				optr = findOpcd(Strip(opc))
				If (optr == 0) Then Do
					lstIMsg = "Opcode" opc "does not exist (yet)"
				End; Else Do
					Call listInstruction(optr)
				End
			End
			
			Otherwise Do
				lstImMsg = "Invalid choice: " || lstIChoice 
			End
			
		End
		
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- List all instructions ------------------------- listInstructions --- */
/* -------------------------------------------------------------------------- */
listInstructions:
	Call screenHeader "SCEPSIS - Instructions"

	Call Display  5  3 color.brightwhite "List of opcodes and instructions"
	liLine = ""
	scrLine = 6
	Do i = 1 to instr.0
		liLine = liLine || Left(instr.i.1||"  ",2) || " " || Left(instr.i.2||"    ", 4) || " " || Left(instr.i.2.1||"  ", 2) || "   "
		If ((i //  6) == 0) Then Do
			Call Display scrLine 3 color.cyan liLine
			scrLine = scrLine + 1
			liLine = ""
		End
	End
	If (liLine <> "") Then Call Display scrLine 3 color.cyan liLine
	scrLine = scrLine + 1
	
	Call Display 20  3 color.brightwhite "X"
	Call Display 20  5 color.brightcyan "return"
	Call Display 20 13 color.brightwhite "D"
	Call Display 20 15 color.brightcyan "{opcode}"

	If Strip(lstIMsg) <> "" Then Do
		Call Display 21 1 color.brightwhite "===>" lstIMsg
	End
	Call Display  2 6 color.brightwhite
	lstIChoice = Strip(Upper(linein()))
Return lstIChoice


/* -------------------------------------------------------------------------- */
/* ----- List one instruction --------------------------- listInstruction --- */
/* -------------------------------------------------------------------------- */
listInstruction:
	Parse Arg pointer .
	Call screenHeader "SCEPSIS - breakup of the " || instr.pointer.2 || " Instruction"
	
	Call Display  5  3 color.brightwhite "OPC  Ins Arg Stp  Control Signals"
	liLine = instr.pointer.1 "-" left(instr.pointer.2||"    ",5) || Left(instr.pointer.2.1||"   ",3)
	scrLine = 6
	Call Display scrLine 3 color.brightwhite liLine
	Do step = 1 to instr.pointer.3.0
		liLine2 = step " "
		Do cs = 1 To instr.pointer.3.step.0
			liLine2 = liLine2 instr.pointer.3.step.cs
		End
		Call Display scrLine 17 color.cyan liLine2
		scrLine = scrLine + 1
	End
	scrLine = scrLine + 1
	
	If Strip(liMsg) <> "" Then Do
		Call Display 21 1 color.brightwhite "===>" liMsg
	End
	
	Call Display 21 6 color.brightwhite
	Call enterForMore
Return


/* -------------------------------------------------------------------------- */
/* ----- Help info for control panel ------------------------- CPhelpInfo --- */
/* -------------------------------------------------------------------------- */
CPhelpInfo:
	Call screenHeader "SCEPSIS - Help information for the Control Panel"
	
	Call Display  3  3 color.brightwhite  "Help info for SCEPSIS" versionString
	Call Display  5  3 color.cyan  "Every highlighted word can be used as a command."
	Call Display  6  3 color.cyan  "Where appropriate you can add values:"
	Call Display  7  3 color.cyan  "- for 'components' it's a hexadecimal value from 00 to FF"
	Call Display  8  3 color.cyan  "- for 'control signals' it's a binary bit value (0 or 1)"
	Call Display  9  3 color.cyan  "Commands do not have parameters here"
	
	Call Display 11  3 color.cyan  "X - exit the program"
	Call Display 12  3 color.cyan  "P - process the controlsignals"
	Call Display 13  3 color.cyan  "S - execute one micro-step within an instruction"
	Call Display 14  3 color.cyan  "I - execute one instruction (with all of it's micro-steps)"
	Call Display 15  3 color.cyan  "R - execute the whole program until a HLT condition is reached"
	Call Display 16  3 color.cyan  "C - reset the whole emulator"
	
	Call Display 19  3 color.cyan  "For more info see"
	Call Display 19 21 color.brightred  "https://github.com/GerardWassink/SCEPSIS"

	Call Display 21 1 color.brightwhite " "
	Call enterForMore
	
Return


/* -------------------------------------------------------------------------- */
/* ----- List Components in our model computer------------ listComponents --- */
/* -------------------------------------------------------------------------- */
listComponents:
	Call screenHeader "List of Components"

	Call Display  4  3 color.brightwhite	"----- manually settable components -----"
	Call Display  5  3 color.cyan	"PCT - Program CounTer"
	Call Display  6  3 color.cyan	"INR - INstruction Register"
	Call Display  7  3 color.cyan	"STC - STep CounTer (microsteps per instruction)"
	Call Display  8  3 color.cyan	"MAR - Memory Address Register"
	Call Display  9  3 color.cyan	"INP - INPut Register"
	Call Display 10  3 color.cyan	"OUT - OUTput Register"
	Call Display 11  3 color.cyan	"RGA - ReGister A"
	Call Display 12  3 color.cyan	"RGB - ReGister B"
	Call Display 13  3 color.cyan	"RGB - ReGister B"
	Call Display 14  3 color.cyan	"AOPR- ALU Operand 2 (Operand 1 = RGA)"
	Call Display 16  3 color.brightwhite	"----- non settable components ----------"
	Call Display 17  3 color.cyan	"ALU - Arithmetic Logical Unit"
	Call Display 18  3 color.cyan	"CLK - A CLocK pulse generator"
	Call Display 19  3 color.cyan	"CTU - ConTrol Unit"
	Call Display 20  3 color.cyan	"DAB - Data and Address Bus"
	Call Display 21  3 color.cyan	"FLR - FLags Register"
	
	Say ""
	Say ""
	Call enterForMore
Return


/* -------------------------------------------------------------------------- */
/* ----- List available Control Signals -------------- listControlSignals --- */
/* -------------------------------------------------------------------------- */
listControlSignals:
	Call screenHeader "List of Control Signals"
	Say ""
	Do c = 1 to ctlSig.0
		Call Display  (2+c)  3 color.cyan Left(ctlSig.c||"    ",4) || " - " || ctlSig.c.1
	End
	Say ""
	Call enterForMore
Return


/* -------------------------------------------------------------------------- */
/* ----- Display Screen Header ----------------------------- screenHeader --- */
/* -------------------------------------------------------------------------- */
screenHeader:
	Parse Arg headerLine
	"clear"
	Call Display  1  1 color.brightwhite Copies("-", 74) versionString
	If (Strip(headerLine) == "") 
		Then headerLine = "SCEPSIS - Simple CPU Emulator Program (Student Instruction System)"
	position = (37 - Trunc(Length(headerLine)/2))
	Call Display  1 position color.brightwhite " " || headerLine || " "
	
	Call Display  2  1 color.brightwhite "===> "
	Call Display  2  6 color.brightred Copies("_", 75)

Return



/* -------------------------------------------------------------------------- */
/* ----- Display one thing, encoding attributes and positions --- Display --- */
/* -------------------------------------------------------------------------- */
Display:
	Parse Arg row col atr txt
	lineOut = ""
	If ((row > 0) & (col > 0)) Then			/* Position? If yes, encode it -- */
		lineOut = lineOut || ESC || row || ";" || col || "H"
	If (atr <> "") Then					/* attribute? If yes, encode it	----- */
		lineOut = lineOut || ESC || atr || "m"
	If (txt <> "") Then			/* Do we have text? If yes, encode it ------- */
		lineOut = lineOut || txt
	Do j = 1 To Length(lineOut)		/* Write encoded string to the screen --- */
		call charout ,substr(lineOut,j,1)
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- Reset screen attributes to default ----------------------- Reset --- */
/* -------------------------------------------------------------------------- */
Reset:
  Call Display "23 1" color.reset
Return


/* -------------------------------------------------------------------------- */
/* ----- Press enter for more routine ---------------------- enterForMore --- */
/* -------------------------------------------------------------------------- */
enterForMore:
	Say "----- "
	Say ""
	Say "Enter to continue"
	junk = linein()
Return


/* -------------------------------------------------------------------------- */
/* ----- Initialisation of global variables ------------------ Initialize --- */
/* -------------------------------------------------------------------------- */
Initialize:
	/* ----- Available Control Signals ----- */
	ctlSig.0 = 0
	ctlSignals = ""
	Call addCtlSig("CE Counter Enable, the program counter advances to the next position")
	Call addCtlSig("CE2 Counter Enable 2, the program counter advances two bytes to the next position")
	Call addCtlSig("HLT HALT the processor")
	Call addCtlSig("INPO Set the input register to output, put its on the DAB")
	Call addCtlSig("INRI Set the instruction register to input, to take a value from the DAB")
	Call addCtlSig("INRO Set the instruction register to output, put its on the DAB")
	Call addCtlSig("MARI Set the MAR to input, accept an address from the DAB")
	Call addCtlSig("MARO Set the MAR to output, put it out to the DAB")
	Call addCtlSig("PCTI Set the Program Counter to input, getting a value from the DAB")
	Call addCtlSig("PCTO Set the Program Counter to output, put it's value to the DAB")
	Call addCtlSig("OUTI Set the output register to input, getting a value from the DAB")
	Call addCtlSig("MEMI Set memory, pointed to by MAR to input, getting the value from the DAB")
	Call addCtlSig("MEMO Set memory, pointed to by MAR to output, put value on the DAB")
	Call addCtlSig("FTCH Fetch next instruction, put value on the DAB")
	Call addCtlSig("RGAI Set RGA to input, accept a value from the DAB")
	Call addCtlSig("RGAO Set RGA to output, put its value out to the DAB")
	Call addCtlSig("RGBI Set RGB to input, accept a value from the DAB")
	Call addCtlSig("RGBO Set RGB to output, put its value out to the DAB")
	Call addCtlSig("RGCI Set RGC to input, accept a value from the DAB")
	Call addCtlSig("RGCO Set RGC to output, put its value out to the DAB")
	Call addCtlSig("SPI Increment the stack pointer (after retrieval from the stack)")
	Call addCtlSig("SPD Decrement the stack pointer (before pushing unto the stack)")
	Call addCtlSig("STKI Set Stack for input, accept a value from the DAB")
	Call addCtlSig("STKO Set Stack for ouput, put its value out to the DAB")
	Call addCtlSig("ALUI Set ALU operand for input, accept a value for AOPR from the DAB")
	Call addCtlSig("EXC Execute the ALU operation at hand")
	Call addCtlSig("SPCC Set PCT for input when Carry set and accept a value from the DAB")
	Call addCtlSig("SPCZ Set PCT for input when Zero-flag set and accept a value from the DAB")
	Call addCtlSig("SPCE Set PCT for input when Equal-flag set and accept a value from the DAB")
	Call addCtlSig("SPCL Set PCT for input when LessThan-flag set and accept a value from the DAB")
	Call addCtlSig("SPCG Set PCT for input when GreaterThan-flag set and accept a value from the DAB")
	
	
	/* ----- Set default values for program parameters ----- */
	microCodeSteps	= 16							/* Max number of micro code steps */
	memorySize		= 65536							/* Size of memory in bytes*/
	configFile		= "./config/scepsis.conf"		/* File containing the engine parameters */
	langDefFile		= "./config/scepsis.langdef"	/* File containing the instruction definitions */
	Animate			= "R"
	
	/* ----- Read language definition file ----- */
	instr.	= 0								/* stem to hold instructions */
	instr.0 = 0
	If Stream(langDefFile, 'C', 'OPEN READ') = "READY:" Then Do
		Call processLangDefFile
	End; Else Do
		Say "Error opening file" langDefFile
		Exit 8
	End


	/* ----- Read configuration file ----- */
	If Stream(configFile, 'C', 'OPEN READ') = "READY:" Then Do
		Call processConfigFile
	End; Else Do
		Say "Error opening file" configFile
		Exit 8
	End
	
	If (memorySize > 65536) Then Do
		Say "MemorySize specified in config file too large, maximum is 64K (65536 bytes)"
		Exit 8
	End

	/* ----- Initialize Memory ----- */
	memSize = memorySize
	Call initMemory
	
	/* Initialize the emulator --- */
	Call emulatorReset
	
Return

/* -------------------------------------------------------------------------- */
/* ----- Rest the whole emulator -------------------------- emulatorReset --- */
/* -------------------------------------------------------------------------- */
emulatorReset:
	/* ----- Set default values for Flags ----- */
	Flags = "C Z EQ LT GT"
	Do i = 1 To Words(Flags)
		Interpret Word(Flags,i)||"_flag" "=" 0
	End
	
	/* ----- Set default values for Components ----- */
	Components = "PCT INR STC MAR INP OUT REGA REGB REGC SP AOPR"
	Do i = 1 To Words(Components)
		Interpret "comp_"||Word(Components,i) "=" 0
	End
	
	/* ----- Not all must be zero, correct them here: ----- */
	comp_STC  = 1
	comp_SP   = memSize - 1		/* points to top of memory, stack grows down  */
	
	/* ----- Set default values for Control Signals ----- */
	Do i = 1 To Words(ctlSignals)
		Interpret "cs_"||Word(ctlSignals,i) "=" 0
	End
	
Return

/* -------------------------------------------------------------------------- */
/* ----- Helper routine to add control signals to the table --- addCtlSig --- */
/* -------------------------------------------------------------------------- */
addCtlSig:
	Procedure Expose ctlSig. ctlSignals
	Parse Arg ctl desc
	ctlSig.0 = ctlSig.0 + 1; p = ctlSig.0
	ctlSig.p = ctl
	ctlSig.p.1 = desc
	ctlSignals = ctlSignals || " " || ctl
Return p


/* -------------------------------------------------------------------------- */
/* ----- Process the language definition file -------- processLangDefFile --- */
/* -------------------------------------------------------------------------- */
processLangDefFile:
	lnum = 0
	Do While Lines(langDefFile)
		line = Upper(Linein(langDefFile))
		lnum = lnum + 1
		Select
			When ( Substr(line,1,1) == "#") Then Nop
			When ( Strip(line) == "") Then Nop
			Otherwise Do
				Parse Var line opcd mnem argm '|' mcSteps '#' comment
				opcd = Strip(opcd)
				mnem = Strip(mnem)
				argm = Strip(argm)
				i = findOpcd(opcd)		/* yields position or 0 ------------- */
				If (i == 0) Then Do
					instr.0 = instr.0 + 1
					i = instr.0
				End
				instr.i.1   = opcd			/* store opcode ----------------- */
				instr.i.2   = mnem			/* store mnemonic --------------- */
				instr.i.2.1 = argm			/* store argument --------------- */
				instr.i.3.0 = 0				/* ptr to microde steps --------- */
				Do While Words(mcSteps) > 0	/* store ctl signals per step --- */
					Parse Var mcSteps mcStep '-' mcSteps
					instr.i.3.0 = instr.i.3.0 + 1
					s = instr.i.3.0			/* ptr to microde steps --------- */
					instr.i.3.s.0 = 0				/* ptr to ctl Signals --- */
					Do While Words(mcStep) > 0
						Parse Var mcStep ctlSig mcStep
						instr.i.3.s.0 = instr.i.3.s.0 + 1
						mcp = instr.i.3.s.0
						instr.i.3.s.mcp = Strip(ctlSig)
					End
				End
			End
		End
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- Find opcode in the instruction tabel ------------------ findOpcd --- */
/* -------------------------------------------------------------------------- */
findOpcd:
	Procedure Expose instr.
	Parse Arg oc .
	pos = 0
	Do p = 1 to instr.0
		If (oc == instr.p.1) Then Do
			pos = p
			Leave
		End
	End
Return pos


/* -------------------------------------------------------------------------- */
/* ----- Read configuration file and process values --- processConfigFile --- */
/* -------------------------------------------------------------------------- */
processConfigFile:
	lnum = 0
	Do While Lines(configFile)
		line = Linein(configFile)
		lnum = lnum + 1
		Select
			When ( Substr(line,1,1) == "#") Then Nop
			When ( Strip(line) == "") Then Nop
			Otherwise Do
				Parse Var line keyword "=" rest
				Parse Var rest value "#" comment
				keyword = Strip(keyword)
				value = Strip(value)
				Select
					When keyword = "microCodeSteps"		Then microCodeSteps = value
					When keyword = "memorySize"			Then memorySize = value
					When keyword = "langDefFile"		Then langDefFile = value
					When keyword = "Animate"			Then Animate = value

					/* discard SCEPSASM keywords ---------------------------- */
					When keyword = "SRCfile"			Then NOP
					When keyword = "OBJfile"			Then NOP
					When keyword = "LSTfile"			Then NOP

					Otherwise Do
						Say "Invalid keyword" keyword "in config file line" lnum 
						Exit 8
					End
				End
			End
		End
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- Initialize Memory ----------------------------------- initMemory --- */
/* -------------------------------------------------------------------------- */
initMemory:
	Procedure Expose memSize MEM.
	/* ----- Memory ----- */
	Do p = 0 to (memSize - 1)
		MEM.p = 0
	End
Return


/* -------------------------------------------------------------------------- */
/* ----- End of program -------------------------------------- endProgram --- */
/* -------------------------------------------------------------------------- */
endProgram:
	Call reset
	Say "SCEPSIS signing of - Goodbye"
Return


/* -------------------------------------------------------------------------- */
/* ----- Index of labels in this Rexx file ---------------------- indexMe --- */
/* -------------------------------------------------------------------------- */
indexMe:
	"clear"
	lnum = 0
	longest = 0
	srcFile = "./scepsis.rexx"			/* Read our own source -------------- */
	If Stream(srcFile, 'C', 'OPEN READ') = "READY:" Then Do
		i = 1
		Do While Lines(srcFile)
			line = Strip(Linein(srcFile))
			lnum = lnum + 1
			If (line <> "") Then Do
				w = Word(line,1)
				If (Right(w, 1) == ":") Then Do	/* Do we have a label? ------ */
					If Length(w) > longest Then longest = Length(w)
					index.i.1 = Right("      "||lnum, 6)
					index.i.2 = w
					index.0 = i
					i = i + 1
				End
			End
		End
		status = Stream(srcFile, 'C', 'CLOSE')
		Do i = 1 to index.0
			If (Length(index.i.2)/2) <> Trunc(Length(index.i.2)/2) Then index.i.2 = index.i.2||" "
			Say index.i.2 Copies(" .", Trunc((longest - Length(index.i.2)) / 2 )) index.i.1 
		End
		Say ""
		Call enterForMore
	End; Else Do
		srcMsg = "Error opening file" srcFile
		Exit 8
	End
Return

