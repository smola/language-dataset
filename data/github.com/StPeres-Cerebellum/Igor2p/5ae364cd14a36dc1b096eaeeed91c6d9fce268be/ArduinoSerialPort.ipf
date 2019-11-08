#pragma TextEncoding = "UTF-8"
#pragma rtGlobals=3		// Use modern global access method and strict wave access.

function Arduino_initialize()
	if(datafolderexists("root:packages:Arduino")==0)
		if(datafolderexists("root:packages")==0)
			newdatafolder root:packages
		endif
		newdatafolder root:packages:Arduino
		string/g  root:packages:Arduino:portnum //e.g. "COMX"  look it up (WIndows = Device Manager)
		variable/g root:packages:Arduino:numChannels = 6
		variable/g root:packages:Arduino:numRecordings = 0
		make/n=(1,6) root:packages:Arduino:incomingData
		wave incomingData = root:packages:Arduino:incomingData
		setDimLabel 1,0,timeElapsed,incomingData
		setDimLabel 1,1,wheelPos,incomingData
		setDimLabel 1,2,Motor1Pos,incomingData
		setDimLabel 1,3,Motor2Pos,incomingData
		setDimLabel 1,4,reward,incomingData
		setDimLabel 1,5,sync,incomingData
	endif												
	string newPort
	SVAR portNum = root:packages:Arduino:portnum
	VDTGetPortList2
	prompt newPort, "Available ports", popup, S_VDT
	doPrompt "Arduino is connected to which USB port?", newPort
	portnum = newPort
	VDTOPenPort2 $portNum
	VDTOperationsPort2 $portNum
	VDT2/p=$portnum baud=57600, parity=0, databits=8, stopBits=1, buffer=32
	
	NVAR numChannels = root:packages:Arduino:numChannels
	variable tmpChannels = 6
//	prompt tmpChannels, "How many arduino inputs?"
//	doPrompt "How many Arduino channels to record?", tmpChannels
	numChannels = tmpChannels
end

function ReadArduino(s)
	STRUCT WMBackgroundStruct &s
	wave incomingData = root:packages:Arduino:incomingData
	string arduinoBuffer
	SVAR portNum = root:packages:Arduino:portnum
	VDTOperationsPort2 $portNum
	VDTRead2/Q/O=2/T="\n" arduinoBuffer
	
	//Assumes that there are 6 variable coming in
	variable timeElapsed, wheelPos, Motor1Pos, Motor2Pos, reward, sync
	sscanf arduinoBuffer, "%f;%f;%f;%f;%f;%f", timeElapsed, wheelPos, Motor1Pos, Motor2Pos, reward, sync
	
	insertpoints/m=0/v=(timeElapsed) numpnts(incomingData), 1, incomingData

	incomingData[(dimsize(incomingData,0)-1)][1]=wheelPos
	incomingData[(dimsize(incomingData,0)-1)][2]=Motor1Pos
	incomingData[(dimsize(incomingData,0)-1)][3]=Motor2Pos
	incomingData[(dimsize(incomingData,0)-1)][4]=reward
	incomingData[(dimsize(incomingData,0)-1)][5]=sync
	
	return 0
//	print timeElapsed, wheelPos, Motor1Pos, Motor2Pos, reward, sync
end

function closeArduinoPort()
	SVAR portNum = root:packages:Arduino:portnum
	VDTClosePort2 $portNum
end

function openArduinoPort()
	SVAR portNum = root:packages:Arduino:portnum
	VDTOPenPort2 $portNum
	VDTOperationsPort2 $portNum
	VDT2/p=$portnum baud=57600, parity=0, databits=8, stopBits=1
end

Function startReadingArduino()
	Variable numTicks = 10
	
	SVAR portNum = root:packages:Arduino:portnum
//	VDTOPenPort2 $portNum
	VDTOperationsPort2 $portNum
	VDT2/p=$portnum killio
//	VDT2/p=$portnum killio
	
	CtrlNamedBackground wheelBackground, period=numTicks, proc=ReadArduino
	CtrlNamedBackground wheelBackground, start
End

Function stopReadingArduino()
	SVAR portNum = root:packages:Arduino:portnum
	CtrlNamedBackground wheelBackground, stop
//	VDT2/p=$portnum killio
//	VDTClosePort2 $portNum
	
	wave arduinoTmp = root:packages:Arduino:incomingData
	NVAR numRecordings = root:packages:Arduino:numRecordings
	string saveName = "root:packages:Arduino:SavedData"+num2str(numRecordings)
	movewave arduinoTmp $saveName 
	make/n=(1,6) root:packages:Arduino:incomingData
	wave incomingData = root:packages:Arduino:incomingData
	setDimLabel 1,0,timeElapsed,incomingData
	setDimLabel 1,1,wheelPos,incomingData
	setDimLabel 1,2,Motor1Pos,incomingData
	setDimLabel 1,3,Motor2Pos,incomingData
	setDimLabel 1,4,reward,incomingData
	setDimLabel 1,5,sync,incomingData
	numRecordings += 1
End

function testReadIntoWave()
	SVAR portNum = root:packages:Arduino:portnum
	make/n=1000/o root:packages:Arduino:inputBuffer
	wave inputBuffer = root:packages:Arduino:inputBuffer
	VDTOperationsPort2 $portNum
	VDT2/p=$portnum killio
	VDTReadWave2/Q/O=15/T="\n" inputBuffer
end