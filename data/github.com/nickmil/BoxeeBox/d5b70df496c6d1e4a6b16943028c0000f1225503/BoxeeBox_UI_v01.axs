MODULE_NAME='BoxeeBox_UI_v01' (DEV vdvDev,DEV dvTP)

DEFINE_TYPE
	STRUCTURE _NowPlaying {
		INTEGER nMediaType
		
		CHAR cTimePassed[16]
		CHAR cTimeDuration[16]
		
		CHAR cShowSeason[4]
		CHAR cShowEpisode[4]
		
		CHAR cLinesOfText[6][128]
	}
	STRUCTURE _BBoxUI {
		_NowPlaying WasPlaying
		_NowPlaying NowPlaying
	}


DEFINE_CONSTANT
	AUDIO = 1
	VIDEO = 2


DEFINE_VARIABLE
	VOLATILE _BBoxUI BBoxUI
	VOLATILE CHAR cKeys[2][47][2] = {
		{ {'1'},{'2'},{'3'},{'4'},{'5'},{'6'},{'7'},{'8'},{'9'},{'0'},{'-'},{'='},
			{'q'},{'w'},{'e'},{'r'},{'t'},{'y'},{'u'},{'i'},{'o'},{'p'},{'['},{']'},
			{'\'},{'a'},{'s'},{'d'},{'f'},{'g'},{'h'},{'j'},{'k'},{'l'},{';'},{$27},
			{'z'},{'x'},{'c'},{'v'},{'b'},{'n'},{'m'},{','},{'.'},{'/'}
		},
		{ {'!'},{'@'},{'#'},{'$'},{'%'},{'^'},{'&'},{'*'},{'('},{')'},{'_'},{'+'},
			{'Q'},{'W'},{'E'},{'R'},{'T'},{'Y'},{'U'},{'I'},{'O'},{'P'},{'{'},{'}'},
			{'|'},{'A'},{'S'},{'D'},{'F'},{'G'},{'H'},{'J'},{'K'},{'L'},{':'},{'"'},
			{'Z'},{'X'},{'C'},{'V'},{'B'},{'N'},{'M'},{'<'},{'>'},{'?'}
		}
	}
	VOLATILE INTEGER nKeyboard[] = {
		101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,
		119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,
		137,138,139,140,141,142,143,144,145,146
	}
	VOLATILE INTEGER nShiftLevel
	VOLATILE INTEGER nCapsWait

//-----------------------------------------------------------------------------

DEFINE_FUNCTION ClearNowPlaying() {
	LOCAL_VAR INTEGER i
	FOR(i=11;i<=16;i++) {
		BBoxUI.NowPlaying.cLinesOfText[i-10] = ''
		SEND_COMMAND dvTP,"'^TXT-',ITOA(i),',0, '"
	}
	SEND_LEVEL dvTP,2,0
}


DEFINE_FUNCTION UpdateNowPlaying(_NowPlaying Playing) {
	STACK_VAR INTEGER i
	
	FOR(i=1;i<=6;i++) {
		IF(BBoxUI.NowPlaying.cLinesOfText[i]!=BBoxUI.WasPlaying.cLinesOfText[i]) {
			SEND_COMMAND dvTP,"'^TXT-1',ITOA(i),',0,',BBoxUI.NowPlaying.cLinesOfText[i]"
			BBoxUI.WasPlaying.cLinesOfText[i] = BBoxUI.NowPlaying.cLinesOfText[i]
		}
	}
}

//-----------------------------------------------------------------------------
	
DEFINE_EVENT		//---Transports
	BUTTON_EVENT [dvTP,1] {		//---Play
		PUSH : SEND_COMMAND vdvDev,"'PLAY'"
	}
	BUTTON_EVENT [dvTP,2] {		//---Stop
		PUSH : SEND_COMMAND vdvDev,"'STOP'"
	}
	BUTTON_EVENT [dvTP,3] {		//---Pause
		PUSH : SEND_COMMAND vdvDev,"'PAUSE'"
	}
	BUTTON_EVENT [dvTP,5] {		//---Next
		PUSH : SEND_COMMAND vdvDev,"'NEXT'"
	}
	BUTTON_EVENT [dvTP,6] {		//---Previous
		PUSH : SEND_COMMAND vdvDev,"'PREVIOUS'"
	}
	BUTTON_EVENT [dvTP,7] {		//---Seek Fwd
		PUSH : SEND_COMMAND vdvDev,"'SEEK_PCT-1'"
		HOLD[3,REPEAT] : SEND_COMMAND vdvDev,"'SEEK_PCT-1'"
	}
	BUTTON_EVENT [dvTP,8	] {		//---Seek Rev
		PUSH : SEND_COMMAND vdvDev,"'SEEK_PCT--1'"
		HOLD[3,REPEAT] : SEND_COMMAND vdvDev,"'SEEK_PCT--1'"
	}
	
DEFINE_EVENT		//---Volume
	BUTTON_EVENT [dvTP,24] {	//---Volume Up
		PUSH : SEND_COMMAND vdvDev,"'VOLUME-UP'"
		HOLD[2,REPEAT] : SEND_COMMAND vdvDev,"'VOLUME-UP'"
	}
	BUTTON_EVENT [dvTP,25] {	//---Volume Down
		PUSH : SEND_COMMAND vdvDev,"'VOLUME-DOWN'"
		HOLD[2,REPEAT] : SEND_COMMAND vdvDev,"'VOLUME-DOWN'"
	}
	BUTTON_EVENT [dvTP,26] {	//---Volume Mute
		PUSH : SEND_COMMAND vdvDev,"'MUTE'"
	}
	
DEFINE_EVENT		//---Menu Navigation
	BUTTON_EVENT [dvTP,45] {		//---Up
		PUSH : SEND_COMMAND vdvDev,"'MENU-UP'"
	}
	BUTTON_EVENT [dvTP,46] {		//---Down
		PUSH : SEND_COMMAND vdvDev,"'MENU-DOWN'"
	}
	BUTTON_EVENT [dvTP,47] {		//---Left
		PUSH : SEND_COMMAND vdvDev,"'MENU-LEFT'"
	}
	BUTTON_EVENT [dvTP,48] {		//---Right
		PUSH : SEND_COMMAND vdvDev,"'MENU-RIGHT'"
	}
	BUTTON_EVENT [dvTP,49] {		//---Enter
		PUSH : SEND_COMMAND vdvDev,"'MENU-ENTER'"
	}
	BUTTON_EVENT [dvTP,50] {		//---Menu
		PUSH : SEND_COMMAND vdvDev,"'MENU'"
	}
	BUTTON_EVENT [dvTP,51] {		//---Menu - Exit
		PUSH : SEND_COMMAND vdvDev,"'MENU-EXIT'"
	}
	
DEFINE_EVENT
	BUTTON_EVENT [dvTP,100] {		//---Shift Key
		PUSH : {
			SELECT {
				ACTIVE (nShiftLevel==0) : {
					nShiftLevel = 1
					ON[nCapsWait]
					SEND_LEVEL dvTP,4,1		//---Go to Temp Shift
					WAIT 15 'CapsWait'
						OFF[nCapsWait]
				}
				ACTIVE ((nShiftLevel==1) && (!nCapsWait)) : {
					nShiftLevel = 0
					OFF[nCapsWait]
					SEND_LEVEL dvTP,4,0		//---Go to bottom shift level
				}
				ACTIVE ((nShiftLevel==1) && (nCapsWait)) : {
					nShiftLevel = 2
					CANCEL_WAIT 'CapsWait'
					OFF[nCapsWait]
					SEND_LEVEL dvTP,4,1		//---Go to Caps Lock
				}
				ACTIVE (nShiftLevel==2) : {
					nShiftLevel = 0
					OFF[nCapsWait]
					SEND_LEVEL dvTP,4,0		//---Go to bottom shift Level
				}
			}
			SEND_LEVEL dvTP,3,nShiftLevel
		}
		
	}
	BUTTON_EVENT [dvTP,nKeyboard] {
		PUSH : {
			LOCAL_VAR INTEGER nTempKey
			nTempKey = GET_LAST(nKeyboard)
			SEND_STRING 0,"'nTempKey = ',ITOA(nTempKey)"
			SELECT {
				ACTIVE (nShiftLevel==0) : {
					SEND_LEVEL dvTP,4,2
					SEND_COMMAND vdvDev,"'KEYBOARD-',cKeys[1][nTempKey]"
					SEND_STRING 0,"'cKeys[1][nTK] = ',cKeys[1][nTempKey]"
				}
				ACTIVE ((nShiftLevel==1) || (nShiftLevel==2)) : {
					SEND_LEVEL dvTP,4,3
					SEND_COMMAND vdvDev,"'KEYBOARD-',cKeys[2][nTempKey]"
					SEND_STRING 0,"'cKeys[3][nTK] = ',cKeys[2][nTempKey]"
				}
			}
		}
		RELEASE : {
			SELECT {
				ACTIVE (nShiftLevel==0) : {
					SEND_LEVEL dvTP,4,0
				}
				ACTIVE (nShiftLevel==1) : {
					OFF[nShiftLevel]
					SEND_LEVEL dvTP,3,0
					SEND_LEVEL dvTP,4,0
				}
				ACTIVE (nShiftLevel==2) : {
					SEND_LEVEL dvTP,4,1
				}
			}
		}
	}
	BUTTON_EVENT [dvTP,150] {		//---Space Bar
		PUSH : SEND_COMMAND vdvDev,"'KEYBOARD-SPACE'"
	}
	BUTTON_EVENT [dvTP,151] {		//---Space Delete
		PUSH : SEND_COMMAND vdvDev,"'KEYBOARD-DELETE'"
	}
	
DEFINE_EVENT
	LEVEL_EVENT [vdvDev,1] {
		SEND_LEVEL dvTP,1,LEVEL.VALUE
		SEND_COMMAND dvTP,"'^TXT-1,0,',ITOA(LEVEL.VALUE),'%'"
	}
	LEVEL_EVENT [vdvDev,2] {
		SEND_LEVEL dvTP,2,LEVEL.VALUE
		SEND_COMMAND dvTP,"'^TXT-2,0,',ITOA(LEVEL.VALUE),'%'"
	}
	
//-----------------------------------------------------------------------------

DEFINE_EVENT
	DATA_EVENT [dvTP] {
		ONLINE : {
			ClearNowPlaying()
			SEND_COMMAND vdvDev,"'?VOLUME'"
		}
	}
	
DEFINE_EVENT
	DATA_EVENT [vdvDev] {
		STRING : {
			STACK_VAR CHAR cCmd[16]
			STACK_VAR INTEGER nPara
			STACK_VAR CHAR cPara[8][128]
			
			IF(FIND_STRING(DATA.TEXT,'-',1)) {
				cCmd = REMOVE_STRING(DATA.TEXT,'-',1)
				SET_LENGTH_STRING(cCmd,LENGTH_STRING(cCmd)-1)
				
				nPara=1
				WHILE(FIND_STRING(DATA.TEXT,',',1)) {
					cPara[nPara] = REMOVE_STRING(DATA.TEXT,',',1)
					SET_LENGTH_STRING(cPara[nPara],LENGTH_STRING(cPara[nPara])-1)
					nPara++
				}
				cPara[nPara] = DATA.TEXT
			}
			ELSE {
				cCmd = DATA.TEXT
			}
			
			SELECT {
				ACTIVE ((cCmd=='NOW_PLAYING') && (cPara[1]=='Nothing')) : ClearNowPlaying()
				ACTIVE ((cCmd=='NOW_PLAYING') && (cPara[1]=='Type') && (cPara[2]=='Audio')) : BBoxUI.NowPlaying.nMediaType = AUDIO
				ACTIVE ((cCmd=='NOW_PLAYING') && (cPara[1]=='Type') && (cPara[2]=='Video')) : BBoxUI.NowPlaying.nMediaType = VIDEO
				
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==AUDIO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Title'))				: BBoxUI.NowPlaying.cLinesOfText[1] = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==AUDIO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Artist'))			: BBoxUI.NowPlaying.cLinesOfText[2] = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==AUDIO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Album'))				: BBoxUI.NowPlaying.cLinesOfText[3] = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==AUDIO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Time'))				: BBoxUI.NowPlaying.cTimePassed = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==AUDIO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Duration'))		: BBoxUI.NowPlaying.cTimeDuration = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==AUDIO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Percentage'))	: BBoxUI.NowPlaying.cLinesOfText[4] = "BBoxUI.NowPlaying.cTimePassed,' / ',BBoxUI.NowPlaying.cTimeDuration,' / ',cPara[2],'%'"
				
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==VIDEO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Show Title'))	: BBoxUI.NowPlaying.cLinesOfText[1] = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==VIDEO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Title'))				: BBoxUI.NowPlaying.cLinesOfText[2] = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==VIDEO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Season'))			: BBoxUI.NowPlaying.cShowSeason = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==VIDEO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Episode'))			: BBoxUI.NowPlaying.cLinesOfText[3] = "'Season ',BBoxUI.NowPlaying.cShowSeason,', Episode ',cPara[2]"
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==VIDEO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Time'))				: BBoxUI.NowPlaying.cTimePassed = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==VIDEO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Duration'))		: BBoxUI.NowPlaying.cTimeDuration = cPara[2]
				ACTIVE ((BBoxUI.NowPlaying.nMediaType==VIDEO) && (cCmd=='NOW_PLAYING') && (cPara[1]=='Percentage'))	: BBoxUI.NowPlaying.cLinesOfText[4] = "BBoxUI.NowPlaying.cTimePassed,' / ',BBoxUI.NowPlaying.cTimeDuration,' / ',cPara[2],'%'"
				
				ACTIVE (1) : {}
			}
			UpdateNowPlaying(BBoxUI.NowPlaying)
		}
	}

//-----------------------------------------------------------------------------

DEFINE_PROGRAM
	[dvTP,1] = [vdvDev,1]