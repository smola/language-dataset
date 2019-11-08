MODULE_NAME='M_CommandFusion,iViewer,REV01'(DEV idvTP, 		// The iViewer Physical Device
					    DEV vdvTP[], 	// The Virtual Device Array Use To Have Multiple AMX TP Ports
					    CHAR ciViewerPswd[],// For Security
					    CHAR sFileName[],	// Name Of The CommandFusion GUI File
					    INTEGER nDEBUG)	// Use To Enable Debug Messages


(***********************************************************)
(*  FILE_LAST_MODIFIED_ON: 06/30/2009  AT: 12:08:49        *)
(***********************************************************)

(*
How To Use:
	
YOU MUST UPLOAD YOUR .GUI FILE, GENERATED FROM GUIDESIGNER, ONTO YOUR NELINX MASTER BEFORE LOADING THIS MODULE.
	
When the Netlinx Master reboots, it will read, line by line, the .GUI file to gather the following information:
	
- Project File Name.
- Designer's Name.
- IP Address that the iViewer will be connecting to.
- The TCP Port (This will be use when the master opens that port.)
- Page Name And Join Number.
- Bargraph settings (min and max level) (!!Pargraphs with the same level code have to have the same settings!!)
	
After this is complete, the master will open the TCP Port to allow your iViewer to connect.
	
Note: This parsing can take a couple of minutes in this current version of this module.

*)

(*
AMX Touch Panel Standards:
	
Channel Range: 1 - 4000 button push and Feedback (per address port)
Variable Text range: 1 - 4000 (per address port)
Button States Range: 1 - 256 (0 = All states, for General buttons 1 = Off state and 2 = On state)
Level Range: 1 - 600 (Default level value 0 - 255, can be set up to 1 - 65535)
Address port Range: 1 - 100
*)

(*
CF iViewer Touch Panel Rules:

Channel Range: 1 - 1000 button push and Feedback (per address port)
Variable Text range: 1 - 1000 (per address port)
Button States Range: 0 - 1 (0 = Off state,  1 =  On state)
Level Range: 1 - 65535
Address port Range: 1 - 100
*)

//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////

DEFINE_CONSTANT // IP Server Constants

// SET MAXIMUM COUNTS FOR VIRTUAL DEVICES IN DEV ARRAY IN ONLINE EVENT
nMAX_CHAN_CNT	= 1000
nMAX_LVL_CNT	= 1000
nMAX_PORT_CNT	= 100
  
cnOFF   	= 0		// Constant Value OFF
cnON		= 1		// Constast Value ON

cnRELEASE 	= 0		//
cnPUSH    	= 1		//

cEndOfLine    	= $03
cEquals       	= '='

cHeartBeat    	= 'h'		// Constant 'h' Heartbeat value Prefix
cButton 	= 'd'		// Constant 'd' Digital value Prefix
cLevel 		= 'a'		// Constant 'a' Analog Value Prefix
cString 	= 's'		// Constant 's' Serial String Prefix
cPassword 	= 'p'		// Constant 'p' Password Validation Prefix
cInitialize 	= 'i'		// Constant 'i' Initialization Prefix
cOrientation 	= 'm'		// Constant 'm' Portrait/Landscape orentation 

cPortrait 	= 'portrait'	// Constant value for mode = Portrait
cLandscape 	= 'landscape'	// Constant value for mode = Landscape

cPswdPASS 	= 'ok'		// Constant value for correct password
cPswdFAIL 	= 'bad'		// Constant value for incorrect password

cActive       	= '1'		// Reply With A 1 For The Heartbeat Message (Occurs every 3 seconds)

cnModVer	= 1		//


DEFINE_CONSTANT

cnReadOnly	= 1
cnRWNew       	= 2
cnRWAppend    	= 3

cnOFF 		= 0
cnON  		= 1

cnMaxPages    	= 100
cnMaxPopUp    	= 100

//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////

DEFINE_TYPE

STRUCT _iViewerPrt
  {
  CHAR Address[16];
  CHAR Buffer[255];
  CHAR DatRcv[255];
  }

STRUCT _iVPanel
  {
  CHAR CmdRcv[255];
  }

STRUCT _GUIInfo
  {
  CHAR ProjName[25];
  CHAR Designer[25];
  CHAR IPAddress[25];
  INTEGER nIP_PORT;
  }


//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////

DEFINE_VARIABLE

_iViewerPrt iViewerPrt;
_iVPanel    iVPanel;
_GUIInfo    iGUIInfo;
CHAR        cPageTable[cnMaxPages][20]
CHAR	    cPopUpTable[cnMaxPopUp][20]

DEFINE_VARIABLE

VOLATILE INTEGER nAppVer
VOLATILE INTEGER nVdvBtn
VOLATILE INTEGER nTempNum

VOLATILE INTEGER nPort
VOLATILE INTEGER nButton
VOLATILE INTEGER nBtnMode
VOLATILE INTEGER nLvlNum
VOLATILE INTEGER nTxtNum

VOLATILE CHAR sMODE[30]
VOLATILE CHAR sSTRING[255]
VOLATILE INTEGER nLEVEL
VOLATILE INTEGER nSTATUS

VOLATILE SLONG fileIO
VOLATILE CHAR sTrash[16]

VOLATILE INTEGER nCount
VOLATILE INTEGER IDX
VOLATILE CHAR sPAGE[16]
VOLATILE CHAR sNAME[16]

VOLATILE INTEGER nTCP_PORT

VOLATILE INTEGER nTPAGE

INTEGER nRefreshButtonFeedback[100][1000]	// Set buttonfeedback on initialization [x][y]
						// x=Port
						// y=Channel

INTEGER nBargraphSettings[2][100][1000]		// Saving Bargraph Settings for Compatibily [x][y][z]
						// x=Minimum Level / Maximum Level
						// y=Port
						// z=LevelCode

CHAR cMinLevel[20]				// 
CHAR cMaxLevel[20]				//
CHAR cLevelCode[20]				//

//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////

(************************************************************************)
(* Begin DEFINE_FUNCTION Debug		                     29.06.2009 *)
(************************************************************************)

DEFINE_FUNCTION runDebug(CHAR sDATA[])
  {
  SEND_STRING 0,"'iViewerPrt Comm Mod -- ',sDATA"
  }

(************************************************************************)
(* End DEFINE_FUNCTION Debug		                     29.06.2009 *)
(************************************************************************)

(************************************************************************)
(* Begin DEFINE_FUNCTION Initialisation	                     29.06.2009 *)
(************************************************************************)

DEFINE_FUNCTION runOpenIPPort(Integer nIP_PORT)
  {
  IF(nDEBUG) 
    {
    runDebug("'runOpenIPPort() - Opening Server On TCP Port #',Itoa(iGUIInfo.nIP_PORT)")
    }
  nTCP_PORT = nIP_PORT
  ip_server_open(idvTP.PORT,nTCP_PORT,IP_TCP)
  }

DEFINE_FUNCTION runOpenFile (CHAR ThisFile[], INTEGER nFileMode)
  {
  STACK_VAR CHAR sFileIO[3]
  STACK_VAR SLONG Result
  nCount = 0
  IDX = 0
  FILE_SETDIR('\Test\')
  fileIO = File_Open(ThisFile,nFileMode)
  IF(fileIO > 0)
    {
    runReadLine(fileIO)
    }
  ELSE
    {
    sFileIO = Itoa(fileIO)
    SWITCH(sFileIO)
      {
      CASE '-2' : runDebug("'fileIO Error - Invalid file path or name'")
      CASE '-3' : runDebug("'fileIO Error - Invalid value supplied for IOFlag'")
      CASE '-5' : runDebug("'fileIO Error - Disk I/O error'")
      CASE '-14': runDebug("'fileIO Error - Maximum number of files are already open (max is 10)'")
      CASE '-15': runDebug("'fileIO Error - Invalid file format'")
      }
    Result = FILE_CLOSE(fileIO)
    }
  }

DEFINE_FUNCTION runReadLine (SLONG nFileIO)
  {
  STACK_VAR CHAR sXMLData[1000]
  STACK_VAR SLONG nFileHandle
  STACK_VAR CHAR sFileHandle[3]
  STACK_VAR SLONG Result
  nFileHandle = FILE_READ_LINE(nFileIO,sXMLData,MAX_LENGTH_STRING(sXMLData))
  IF(nFileHandle > 0)
    {
    IF(nDEBUG)
      {
      runDebug("'nFileHandle - The Value Is #',ITOA(nFileHandle)")
      }
    SELECT
      {
      ACTIVE(FIND_STRING(sXMLData,"'<project>'",1)):
	{
	sTrash = REMOVE_STRING(sXMLData,"'<project>'",1)
	sNAME = REMOVE_STRING(sXMLData,"'<'",1)
	SET_LENGTH_STRING(sNAME,(LENGTH_STRING(sNAME) - 1))
	iGUIInfo.ProjName = sNAME
	}
      ACTIVE(FIND_STRING(sXMLData,"'<designer>'",1)):
	{
	sTrash = REMOVE_STRING(sXMLData,"'<designer>'",1)
	sNAME = REMOVE_STRING(sXMLData,"'<'",1)
	SET_LENGTH_STRING(sNAME,(LENGTH_STRING(sNAME) - 1))
	iGUIInfo.Designer = sNAME
	}
      ACTIVE(FIND_STRING(sXMLData,"'<hostname>'",1)):
	{
	sTrash = REMOVE_STRING(sXMLData,"'<hostname>'",1)
	sNAME = REMOVE_STRING(sXMLData,"'<'",1)
	SET_LENGTH_STRING(sNAME,(LENGTH_STRING(sNAME) - 1))
	iGUIInfo.IPAddress = sNAME
	}
      ACTIVE(FIND_STRING(sXMLData,"'<commandPort>'",1)):
	{
	sTrash = REMOVE_STRING(sXMLData,"'<commandPort>'",1)
	sNAME = REMOVE_STRING(sXMLData,"'<'",1)
	iGUIInfo.nIP_PORT = ATOI(sNAME)
	}
      ACTIVE(FIND_STRING(sXMLData,"'page name='",1)):
	{
	sTrash = REMOVE_STRING(sXMLData,"'page name="'",1)
	sPAGE = REMOVE_STRING(sXMLData,"'"'",1)
	SET_LENGTH_STRING(sPAGE,(LENGTH_STRING(sPAGE) - 1))
	sTrash = REMOVE_STRING(sXMLData,"'j="'",1)
	IDX = ATOI(REMOVE_STRING(sXMLData,"'"'",1))
	IF(IDX = 0)
	  {
	  IF(nDEBUG)
	    {
	    runDebug("'ParseLine Error - The Joint Is Set To 0, Will Not Be Added'")
	    }
	  }
	ELSE
	  {
	  cPageTable[IDX] = sPAGE
	  IF(nDEBUG)
	    {
	    runDebug("'Page Added - "',sPAGE,'" Was Added To Array Index #',Itoa(IDX)")
	    }
	  }
	}
      ACTIVE(FIND_STRING(sXMLData,"'<slider j="'",1)):
	{
	sTrash = REMOVE_STRING(sXMLData,"'<slider j="'",1)
	cLevelCode = REMOVE_STRING(sXMLData,"'"'",1)
	SET_LENGTH_STRING(cLevelCode,(LENGTH_STRING(cLevelCode)-1))
	sTrash = REMOVE_STRING(sXMLData,"'min="'",1)
	cMinLevel = REMOVE_STRING(sXMLData,"'"'",1)
	SET_LENGTH_STRING(cMinLevel,(LENGTH_STRING(cMinLevel)-1))
	sTrash = REMOVE_STRING(sXMLData,"'max="'",1)
	cMaxLevel = REMOVE_STRING(sXMLData,"'"'",1)
	SET_LENGTH_STRING(cMaxLevel,(LENGTH_STRING(cMaxLevel)-1))
	nBargraphSettings[1][ATOI(cLevelCode)/1000][ATOI(cLevelCode)%1000]=ATOI(cMinLevel)
	nBargraphSettings[2][ATOI(cLevelCode)/1000][ATOI(cLevelCode)%1000]=ATOI(cMaxLevel)
	IF(nDEBUG)
	  {
	  runDebug("'Slider Added - Port:',ITOA(ATOI(cLevelCode)/1000),',Channel:',ITOA(ATOI(cLevelCode)%1000),',Min level:',cMinLevel,',Max level:',cMaxLevel")
	  }
	}
      }
    nCount++
    IF(nDEBUG) 
      {
      runDebug("'ParseLine Count - The Current Line Count Is #',ITOA(nCount)")
      runDebug("'sXMLData Text - The Data Stored Is ',sXMLData")
      }
    sXMLData = ''
    WAIT 2 runReadLine(nFileIO)
    }
  ELSE
    {
    sFileHandle = ITOA(nFileIO)
    SWITCH(sFileHandle)
      {
      CASE '-1': runDebug("'nFileHandle Error - invalid file handle'")
      CASE '-9': runDebug("'nFileHandle Error - EOF (end-of-file) reached'")
      CASE '-5': runDebug("'nFileHandle Error - disk I/O error'")
      CASE '-6': runDebug("'nFileHandle Error - invalid parameter (buffer length must be greater than zero)'")
      }
    IF(nDEBUG) 
      {
      runDebug("'File_Close - Closing FileIO #',ITOA(nFileIO)")
      }
    Result = FILE_CLOSE(nFileIO)
    runOpenIPPort(iGUIInfo.nIP_PORT)
    }
  }

(************************************************************************)
(* End DEFINE_FUNCTION Initialisation	                     29.06.2009 *)
(************************************************************************)

(************************************************************************)
(* Begin DEFINE_FUNCTION Communication	                     29.06.2009 *)
(************************************************************************)

DEFINE_FUNCTION runParseData(CHAR sDATARCV[])
  {
  STACK_VAR CHAR EventType[1]
  EventType = LEFT_STRING(sDATARCV,1)
  SWITCH(EventType)
    {
    CASE cHeartBeat:
      {
// IF HEARTBEAT ISN'T RECEIVED WITHIN 10 SECONDS FORCE DISCONNECT
      WAIT 100 'HEARTBEAT_CHECK'
	{
	IP_SERVER_CLOSE(idvTP.PORT)
	}		  
// Where are sending a h=1$03 to keep connection active
      SEND_STRING idvTP,"cHeartBeat,cEquals,cActive,cEndOfLine"
      IF(nDEBUG)
	{
	runDebug("'Heartbeat Event - Sending Reply Now'")
	}
      }
    CASE cButton:
      {
      REMOVE_STRING(sDATARCV,cButton,1)
      nButton  = ATOI(REMOVE_STRING(sDATARCV,'=',1))
      nBtnMode = ATOI(LEFT_STRING(sDATARCV,1))
      runBtnEvent(nButton%1000,nBtnMode,nButton/1000)
      IF(nDEBUG)
	{
	runDebug("'Button Event #',ITOA(nButton)")
	}
      }
    CASE cString:
      {
      REMOVE_STRING(sDATARCV,cString,1)
      nTxtNum = ATOI(REMOVE_STRING(sDATARCV,'=',1))
      nPort = nTxtNum/1000
      nTxtNum = nTxtNum%1000
      runStrEvent(nTxtNum,sDATARCV,nPort)
      IF(nDEBUG)
	{
	runDebug("'String Event #',ITOA(nTxtNum)")
	}
      }
    CASE cLevel:
      {
      REMOVE_STRING(sDATARCV,cLevel,1)
      nLvlNum = ATOI(REMOVE_STRING(sDATARCV,'=',1))
      nLEVEL = ATOI(REMOVE_STRING(sDATARCV,"$03",1))
      SEND_LEVEL vdvTP[nLvlNum/1000],nLvlNum%1000,CalculateLevelSend(nLEVEL,nLvlNum/1000,nLvlNum%1000)
      IF(nDEBUG)
	{
	runDebug("'Level Event #',ITOA(nLvlNum)")
	}
      }
    CASE cOrientation:
      {
      REMOVE_STRING(sDATARCV,'m=',1)
      SEND_STRING vdvTP[1],"'Orientation:',sDATARCV"
(* NOTE: Only The First UI Will Handle This Event *)
      }
    CASE cPassword:
      {
      REMOVE_STRING(sDATARCV,'p=',1)
      IF(FIND_STRING(sDATARCV,"ciViewerPswd,$03",1))
	{
	SEND_STRING idvTP,"'p=ok',$03"
	SEND_STRING 0,"'iViewerPrt Comm Mod -- Password Has Been Validated'"
        }
      ELSE
        {
	SEND_STRING idvTP,"'p=bad',$03"
	SEND_STRING 0,"'iViewerPrt Comm Mod -- Password Is Incorrect (Password = ',sDATARCV,')'"
	}
      }
    CASE cInitialize:
      {
      REMOVE_STRING(sDATARCV,'i=',1)
      nSTATUS = ATOI(LEFT_STRING(sDATARCV,1))
      IF(nSTATUS)
	{
	RunInitialisationPort()
	SEND_STRING vdvTP[1],"'Initialize'"
	SEND_STRING 0,"'iViewerPrt Comm Mod -- The iViewer Client Has Sent The Initialize Command'"
	}
      }
    }
  REMOVE_STRING(sDATARCV,"$03",1)
  }

DEFINE_FUNCTION runParseCmd	(CHAR sCMDRCV[], INTEGER IDX)
  {
  SELECT
    {
    ACTIVE(FIND_STRING(sCMDRCV,'PAGE',1)):
      {
      sTrash = REMOVE_STRING(sCMDRCV,"'PAGE-'",1)
      runPageCntl(sCMDRCV)
      }
    ACTIVE(FIND_STRING(sCMDRCV,'TPAGEON',1)):
      {
      nTPAGE = cnON;
      }
    ACTIVE(FIND_STRING(sCMDRCV,'TPAGEOFF',1)):
      {
      nTPAGE = cnOFF;
      }
    ACTIVE(FIND_STRING(sCMDRCV,'!T',1)):
      {
      REMOVE_STRING(sCMDRCV,'!T',1)
      nVdvBtn = ATOI(LEFT_STRING(sCMDRCV,','))
      REMOVE_STRING(sCMDRCV,',',1)
      nVdvBtn = ((nPort* 1000) + nVdvBtn)
      runSndText(nVdvBtn,sCMDRCV)
      }
    ACTIVE(FIND_STRING(sCMDRCV,'^TXT',1)):
      {
      //example string : ^TXT-26,1&2,Mike's Study
      REMOVE_STRING(sCMDRCV,'^TXT-',1)
      nVdvBtn = ATOI(LEFT_STRING(sCMDRCV,','))
      REMOVE_STRING(sCMDRCV,',',1)
      //parse here for seperate on/off states. Generally we send on and off state together (1&2)
      //Currently iViewer only supports 1 state per "serial join" anyways, so this method works
      REMOVE_STRING(sCMDRCV,',',1)  
      nVdvBtn = ((nPort* 1000) + nVdvBtn)
      runSndText(nVdvBtn,sCMDRCV)
      }
    ACTIVE(FIND_STRING(sCMDRCV,'^BMP',1)):
      {
      REMOVE_STRING(sCMDRCV,'^BMP-',1);
      nVdvBtn = ATOI(REMOVE_STRING(sCMDRCV,',',1));
      runSndImage(nVdvBtn*1000+nVdvBtn,sCMDRCV);
      }
    }
  }

(************************************************************************)
(* End DEFINE_FUNCTION Communication	                     29.06.2009 *)
(************************************************************************)

DEFINE_FUNCTION runPageCntl (Char PAGE[])
  {
  STACK_VAR INTEGER IDX;
  STACK_VAR INTEGER MODE;
	
  FOR(IDX = 1; IDX <= cnMaxPages; IDX++)
    {
    IF(cPageTable[IDX] = PAGE)
      {
      MODE = 1;
      SEND_STRING idvTP,"cButton,ITOA(IDX),cEquals,ITOA(MODE),cEndOfLine";
      BREAK;
      }
    }
  }

DEFINE_FUNCTION runBtnEvent	(INTEGER nBTN, INTEGER nMODE, INTEGER IDX)
  {
  SEND_STRING 0,"'iViewerPrt Comm Mod -- Running runBtnEvent() Function: Btn #',ITOA(nBTN),' Mode:',ITOA(nMODE)"
  IF(nMODE = cnPUSH)
    {
    DO_PUSH_TIMED(vdvTP[IDX],nBTN,DO_PUSH_TIMED_INFINITE)
    }
  ELSE IF(nMODE = cnRELEASE)
    {
    DO_RELEASE(vdvTP[IDX],nBTN)
    }
  }

// Calculate the Level from the settings in Command Fusion
DEFINE_FUNCTION SLONG CalculateLevelSend (SLONG slSetLevel,INTEGER nSetLevelPort,INTEGER nSetLevelCode)
  {
  IF(nBargraphSettings[1][nSetLevelPort][nSetLevelCode]>nBargraphSettings[2][nSetLevelPort][nSetLevelCode])
    {
    slSetLevel=(slSetLevel*(nBargraphSettings[1][nSetLevelPort][nSetLevelCode]- // Calculation to get the  	
		nBargraphSettings[2][nSetLevelPort][nSetLevelCode])/65535)+	// configurated level
		nBargraphSettings[2][nSetLevelPort][nSetLevelCode]		//		
    slSetLevel=(slSetLevel-nBargraphSettings[1][nSetLevelPort][nSetLevelCode])*-1
    }
  ELSE
    {
    slSetLevel=(slSetLevel*(nBargraphSettings[2][nSetLevelPort][nSetLevelCode]- // 
		nBargraphSettings[1][nSetLevelPort][nSetLevelCode])/65535)+
		nBargraphSettings[1][nSetLevelPort][nSetLevelCode]
    }
  RETURN slSetLevel
  }

DEFINE_FUNCTION SLONG CalculateLevelReceive (SLONG slGetLevel,INTEGER nGetLevelPort,INTEGER nGetLevelCode)
  {
  LOCAL_VAR SLONG slLevelcalc
  IF(nBargraphSettings[1][nGetLevelPort][nGetLevelCode]>nBargraphSettings[2][nGetLevelPort][nGetLevelCode]) 
    {
    slLevelcalc=(slGetLevel*-1)+nBargraphSettings[1][nGetLevelPort][nGetLevelCode]
    slLevelcalc=((slLevelcalc-nBargraphSettings[2][nGetLevelPort][nGetLevelCode])*
		65535)/(nBargraphSettings[1][nGetLevelPort][nGetLevelCode]-
		nBargraphSettings[2][nGetLevelPort][nGetLevelCode])
    }
  ELSE
    {
    slLevelcalc=((slGetLevel-nBargraphSettings[1][nGetLevelPort][nGetLevelCode])*
		65535)/(nBargraphSettings[2][nGetLevelPort][nGetLevelCode]-
		nBargraphSettings[1][nGetLevelPort][nGetLevelCode])
    }
  RETURN slLevelcalc
  }

DEFINE_FUNCTION runStrEvent	(INTEGER nTXT, CHAR sDATA[], INTEGER IDX)
  {	
  SELECT
    {
    ACTIVE(FIND_STRING(sDATA,'build',1)):
      {
      REMOVE_STRING(sDATA,'v',1)
      nAppVer = ATOI(LEFT_STRING(sDATA,1))
      IF(nAppVer != cnModVer)
	{
	SEND_STRING 0,"'iViewerPrt Comm Mod -- This Module Version Does Not Match The App Version'"
	}
      ELSE
	{
	SEND_STRING 0,"'iViewerPrt Comm Mod -- This Module Version Matches The App Version'"
	}
      }
    }
  SEND_STRING vdvTP[IDX],"'TEXT',ITOA(nTXT),'-',sDATA"
  }

(*----------------------------------------------------------*)

DEFINE_FUNCTION runSndText	(INTEGER nTXT, CHAR sDATA[])
  {
  SEND_STRING idvTP,"cString,Itoa(nTXT),cEquals,sDATA,cEndOfLine"
  SEND_STRING 0,"'iViewerPrt Comm Mod -- Send Text ',sDATA,'To #',ITOA(nTXT)"
  }

DEFINE_FUNCTION runSndImage	(INTEGER nTXT, CHAR sDATA[])
  {
  REMOVE_STRING(sDATA,',',1)
  SEND_STRING idvTP,"cString,ITOA(nTXT),cEquals,sDATA,cEndOfLine"
  SEND_STRING 0,"'iViewerPrt Comm Mod -- Send Image ',sDATA,'To #',ITOA(nTXT)"
  }

DEFINE_FUNCTION runBtnFBck	(INTEGER nBTN, INTEGER nMode)
  {
  SEND_STRING idvTP,"cButton,ITOA(nBTN),cEquals,ITOA(nMode),cEndOfLine"
  IF(nDEBUG)
    {
    runDebug("'Feedback -- Button #',ITOA(nBTN),' Has Been Set To ',ITOA(nMode)")
    }
  }

DEFINE_FUNCTION RunInitialisationPort()		// Set Button Feedback at initialisation
  {
  LOCAL_VAR INTEGER nCksmPort
  LOCAL_VAR INTEGER nCksmCount
  LOCAL_VAR INTEGER nReadPort

  FOR(nReadPort=1;nReadPort<=100;nReadPort++)
    {
    FOR(nCksmCount=1;nCksmCount<=100;nCksmCount++)
      {
      nCksmPort=nCksmPort+nRefreshButtonFeedback[nReadPort][nCksmCount]
      }
    IF(nCksmPort)
      {
      RunInitialisationChannel(nReadPort)	
      }
    }
  }

DEFINE_FUNCTION RunInitialisationChannel(INTEGER nDFRICPort)
  {
  LOCAL_VAR INTEGER nReadChannel

  FOR(nReadChannel=1;nReadChannel<=1000;nReadChannel++)
    {
    IF(nRefreshButtonFeedback[nDFRICPort][nReadChannel])
      {
      runBtnFBck((nDFRICPort*1000+nReadChannel),cnON)
      }
    }
  }

//Define_Function runShwLvl		(Integer nLvlInt, Integer nValue)
//  {
//  Stack_Var Integer nValueLvl
//  nValueLvl = nValue * nLvlOpr
//  Send_String idvTP,"cLevel,Itoa(nLvlInt),cEquals,Itoa(nValue),cEndOfLine"
//  }

//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////

DEFINE_START

CREATE_BUFFER idvTP,iViewerPrt.DatRcv


runOpenFile(sFileName,cnReadOnly)


//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////

DEFINE_EVENT

DATA_EVENT[vdvTP]
  { 
  ONLINE:
    {
    SET_VIRTUAL_CHANNEL_COUNT(vdvTP[GET_LAST(vdvTP)],nMAX_CHAN_CNT)
    SET_VIRTUAL_LEVEL_COUNT(vdvTP[GET_LAST(vdvTP)],nMAX_LVL_CNT)
    SET_VIRTUAL_PORT_COUNT(vdvTP[GET_LAST(vdvTP)],nMAX_PORT_CNT)
    SEND_STRING 0,"'iViewerPrt Comm Mod -- The iViewer vdvTP Is Now Online'"
    }
  OFFLINE:
    {
    SEND_STRING 0,"'iViewerPrt Comm Mod -- The iViewer vdvTP Is Now Offline'"
    }
  COMMAND:
    {
    SEND_STRING 0,"'iViewerPrt Comm Mod -- The iViewer vdvTP Command Received Is: ',data.text"
    iVPanel.CmdRcv = "iVPanel.CmdRcv,data.text";
		
    nPort = GET_LAST(vdvTP)
		
    runParseCmd(iVPanel.CmdRcv,nPort)
    }
  ONERROR:
    {
    
    }
  }

DATA_EVENT[idvTP]
  { 
  ONLINE:
    {
    ON[idvTP,255];
    SEND_STRING 0,"'iViewerPrt Comm Mod -- The iViewer Client Has Connected To The Master'"
    iViewerPrt.Address = DATA.SOURCEIP;
    SEND_STRING 0,"'iViewerPrt Comm Mod -- The User IP Address Is ',iViewerPrt.Address"
    }
  STRING:
    {	
// CANCEL DISCONNECT 
    CANCEL_WAIT 'HEARTBEAT_CHECK'
		  
    SEND_STRING 0,"'iViewerPrt Comm Mod -- The Data Received Is: ',iViewerPrt.DatRcv"
	    
    WHILE(FIND_STRING(iViewerPrt.DatRcv,"$03",1))
      {	
      runParseData(iViewerPrt.DatRcv);
      }
    }
  OFFLINE:
    {
    OFF[idvTP,255];
    SEND_STRING 0,"'iViewerPrt Comm Mod -- The iViewer Client Has Disconnected'"
    WAIT 5 
      {
      IP_SERVER_OPEN(idvTP.PORT,nTCP_PORT,IP_TCP) // Open A New IP Session
      SEND_STRING 0,"'iViewerPrt Comm Mod -- Opening A New IP Session For iViewer Client Connection'"
			
      iViewerPrt.Address = ''
      SEND_STRING 0,"'iViewerPrt Comm Mod -- Clearing Out The Previous Client iViewer IP Address'"
      }
    }
  ONERROR:
    {
    SEND_STRING 0,"'iViewerPrt Comm Mod -- ERROR -- Connection Has Been Lost'"
    }
  }

CHANNEL_EVENT[vdvTP,000]
  {
  ON:
    {
    STACK_VAR INTEGER nPORT
    STACK_VAR INTEGER nCHNL
    nPORT = GET_LAST(vdvTP)
    nCHNL=CHANNEL.CHANNEL
    runBtnFBck((nPORT*1000+nCHNL),cnON)
    nRefreshButtonFeedback[nPORT][nCHNL]=cnON
    IF(nDEBUG)
      {
      runDebug("'Channel Event: ON -> Channel #',ITOA(channel.channel)")
      }
    }
  OFF:
    {
    STACK_VAR INTEGER nPORT
    STACK_VAR INTEGER nCHNL
    nPORT = GET_LAST(vdvTP)
    nCHNL=CHANNEL.CHANNEL
    runBtnFBck((nPORT*1000+nCHNL),cnOFF)
    nRefreshButtonFeedback[nPORT][nCHNL]=cnOFF
    IF(nDEBUG)
      {
      runDebug("'Channel Event: OFF -> Channel #',ITOA(channel.channel)")
      }
    }
  }


LEVEL_EVENT[vdvTP,0]
  {
  LOCAL_VAR INTEGER nLVLCHNL
  LOCAL_VAR SLONG slNEWLEVEL
  LOCAL_VAR INTEGER nLeveltest
  LOCAL_VAR INTEGER nPORT
  nPORT = GET_LAST(vdvTP)
  nLVLCHNL = level.input.level
  nLeveltest = LEVEL.VALUE
  slNEWLEVEL = CalculateLevelReceive(LEVEL.VALUE,nPORT,nLVLCHNL)
  SEND_STRING idvTP,"cLevel,Itoa(nPORT*1000+nLVLCHNL),cEquals,ITOA(slNEWLEVEL),cEndOfLine"
  IF(nDEBUG)
    {
    runDebug("'Level Event - Level Channel Is #',ITOA(nLVLCHNL), ' Value Is #',ITOA(slNEWLEVEL)")
    }
  }


//Button_Event[vdvTP[1],255]
//  {
//  Push:
//    {
//    ip_server_CLOSE(idvTP.PORT)
//    runOpenFile(sFileName,cnReadOnly)
//    }
//  }
(***********************************************************)
(*                     END OF PROGRAM                      *)
(*        DO NOT PUT ANY CODE BELOW THIS COMMENT           *)
(***********************************************************)