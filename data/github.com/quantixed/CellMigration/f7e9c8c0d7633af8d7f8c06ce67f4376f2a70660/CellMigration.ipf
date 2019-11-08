#pragma rtGlobals=3		// Use modern global access method and strict wave access.
#pragma version=1.12		// version number of Migrate()
#include <Waves Average>

// CellMigration will analyse 2D cell migration in IgorPro
// Use ImageJ to track the cells. Outputs from tracking are saved either 
// 1) as sheets in an Excel Workbook, 1 per condition, or
// 2) as direct outputs from Manual Tracking, in csv format
// 
// Select CellMigr > Cell Migration...
//
// Tell the dialog how many conditions you want to load and the magnification and time resolution
// Next, give each condition a label (name) and then tell Igor where to find the data
// Either an Excel workbook (per condition) or a directory of CSVs (per condition)
// There is also the ability to define "offsetting data", in the case of XY drift during the experiment.
//
// For Excel workbooks:
// NOTE no headers in Excel file. Keep data to columns A-H, max of 2000 rows
// columns are
// A - 0 - ImageJ row
// B - 1 - Track No
// C - 2 - Slice No
// D - 3 - x (in px)
// E - 4 - y (in px)
// F - 5 - distance
// G - 6 - velocity (actually speed)
// H - 7 - pixel value

////////////////////////////////////////////////////////////////////////
// Menu items
////////////////////////////////////////////////////////////////////////
Menu "CellMigr"
	"Cell Migration...", /Q, SetUpMigration()
	"Save Reports...", /Q, SaveAllReports()
	"Recolor Everything", /Q, RecolorAllPlots()
	"Rerun Analysis", /Q, RerunAnalysis()
	Submenu "Manual tracking conversion"
		"Excel to Converted CSV", /Q, Excel2CSV()
		"CSV to Converted CSV", /Q, CSV2CSV()
	End
	"About CellMigration", /Q, AboutCellMigr()
End

////////////////////////////////////////////////////////////////////////
// Master functions and wrappers
////////////////////////////////////////////////////////////////////////
Function SetUpMigration()
	SetDataFolder root:
	// kill all windows and waves before we start
	CleanSlate()
	
	Variable cond = 2
	Variable tStep = 20
	Variable pxSize = 0.22698
	Variable segmentLength = 25
	String hstr = "A directed migration experiment is where a chemoattractant has been used.\r"
	hstr += "Segment length is an arbitrary distance the cell can comfortably travel in the experiment."
	
	Prompt cond, "How many conditions?"
	Prompt tStep, "Time interval (min)"
	Prompt pxSize, "Pixel size (\u03BCm)"
	Prompt segmentLength, "Segment length (\u03BCm)"
	DoPrompt/HELP=hstr "Specify", cond, tStep, pxSize, segmentLength
	
	if (V_flag) 
		return -1
	endif
	
	Make/O/N=4 paramWave={cond,tStep,pxSize,segmentLength}
	MakeColorWave(cond)
	myIO_Panel(cond)
End

////////////////////////////////////////////////////////////////////////
// Main functions
////////////////////////////////////////////////////////////////////////
// Loads the data and performs migration analysis
Function Migrate()
	WAVE/Z paramWave = root:paramWave
	if(!WaveExists(paramWave))
		Abort "Setup has failed. Missing paramWave."
	endif
	
	// pick up global values needed
	Variable cond = paramWave[0]
	Variable tStep = paramWave[1]
	Variable pxSize = paramWave[2]
	WAVE/Z colorWave = root:colorWave
	WAVE/T condWave = root:condWave
	// because the user may have used illegal characters in condWave, we make a clean version
	// for use in Igor and a copy called labelWave to use in plots and layouts
	WAVE/T labelWave = CleanUpCondWave(condWave)
	
	
	// make summary plot windows
	String fullList = "cdPlot;ivPlot;ivHPlot;dDPlot;MSDPlot;DAPlot;angleHPlot"
	Variable nPlots = ItemsInList(fullList)
	String name
	Variable i
	
	for(i = 0; i < nPlots; i += 1)
		name = StringFromList(i, fullList)
		KillWindow/Z $name
		Display/N=$name/HIDE=1		
	endfor
	
	String dataFolderName = "root:data"
	NewDataFolder/O $dataFolderName // make root:data: but don't put anything in it yet
	
	String condName,pref
	Variable moviemax1, moviemax2
	
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		
		// make data folder for each condition
		dataFolderName = "root:data:" + condName
		NewDataFolder/O/S $dataFolderName
		// run other procedures
		moviemax1 = LoadMigration(i)
		moviemax2 = CorrectMigration(i)
		if(moviemax1 != moviemax2)
			if(moviemax2 == -1)
				print "No correction applied to", condName
			else
				print "Caution: different number of stationary tracks compared with real tracks."
			endif
		endif
		// for each condition go and make tracks and plot everything out
		MakeTracks(i)
		SetDataFolder root:
	endfor
	// make the image quilt, spakline and joint histogram and then sort out the layouts
	Variable optDur = MakeImageQuilt(10) // this aims for a quilt of 100 = 10^2 tracks
	MakeJointHistogram(optDur)
	TidyCondSpecificLayouts()
	
	KillWindow/Z summaryLayout
	NewLayout/N=summaryLayout
	TidyUpSummaryLayout()

	// when we get to the end, print (pragma) version number
	Print "*** Executed Migrate v", GetProcedureVersion("CellMigration.ipf")
	KillWindow/Z FilePicker
End

// This function will load the tracking data
/// @param	ii	variable containing row number from condWave
Function LoadMigration(ii)
	Variable ii
	
	WAVE/T condWave = root:condWave
	String condName = condWave[ii]
	WAVE/T PathWave1 = root:PathWave1
	String pathString = PathWave1[ii]
	String sheet, prefix, matName, wList
	String fileList
	Variable moviemax,csvOrNot
	Variable i
	
	if(StringMatch(pathString, "*.xls*") == 1)
		// set variable to indicate Excel Workbook
		csvOrNot = 0
		// Works out what sheets are in Excel Workbook and then loads each.
		XLLoadWave/J=1 PathWave1[ii]
		fileList = S_value
	else
		// set variable to indicate csv file
		csvOrNot = 1
		// Work out what files are in directory
		NewPath/O/Q ExpDiskFolder, pathString
		fileList = IndexedFile(expDiskFolder,-1,".csv")
	endif
	fileList = SortList(fileList, ";", 16)
	moviemax = ItemsInList(fileList)
		
	for(i = 0; i < moviemax; i += 1)
		sheet = StringFromList(i, fileList)
		prefix = condName + "_c_" + num2str(i)
		matName = condName + "_" + num2str(i)
		if(csvOrNot == 0)
			XLLoadWave/S=sheet/R=(A1,H2000)/O/K=0/N=$prefix/Q PathWave1[ii]
		else
			LoadWave/A=$prefix/J/K=1/L={0,1,0,0,0}/O/P=expDiskFolder/Q sheet
		endif
		wList = wavelist(prefix + "*",";","")	// make matrix for each sheet
		Concatenate/O/KILL wList, $matName
		// now we need to check that the matrix is OK
		Wave matTrax = $matName
		// check we have a valid matrix with 8 columns
		CheckColumnsOfMatrix(matTrax)
		// make sure 1st point is -1
		matTrax[0][5,6] = -1
		// check distances and speeds are correct
		CheckDistancesAndSpeeds(matTrax)
	endfor	
		
	Print "*** Condition", condName, "was loaded from", pathString
	
	// return moviemax back to calling function for checking
	return moviemax
End

// This was added in v1.12 Dec 2018. At some point in the last year, the output
// of Manual Tracking changed so that there was no longer a first (0) column containing
// the row numbers. Add this column if that is the case.
STATIC Function CheckColumnsOfMatrix(matTrax)
	WAVE matTrax
	Variable numCols = DimSize(matTrax,1)
	if(numCols == 8)
		return 1
	elseif(numCols == 7)
		// insert new 0th column
		InsertPoints/M=1 0,1, MatTrax
		MatTrax[][0] = p+1
		return 0
	else
		Print NameOfWave(matTrax), "does not have 8 columns of data."
		return -1
	endif
End

// The purpose of this function is to work out whether the distances (and speeds) in the
// original data are correct. Currently it just corrects them rather than testing and correcting if needed.
STATIC Function CheckDistancesAndSpeeds(matTrax)
	WAVE matTrax
	
	WAVE/Z paramWave = root:paramWave
	Variable tStep = paramWave[1]
	Variable pxSize = paramWave[2]
	
	// make new distance column
	Duplicate/O/RMD=[][3,4]/FREE matTrax,tempDist // take offset coords
	Differentiate/METH=2 tempDist
	tempDist[][] = (matTrax[p][5] == -1) ? 0 : tempDist[p][q]
	MatrixOp/O/FREE tempNorm = sqrt(sumRows(tempDist * tempDist))
	tempNorm[] *= pxSize // convert to real distance
//	MatrixOp/O/FREE tempReal = sumcols(tempNorm - col(matTrax,5)) // for checking
	matTrax[][5] = (matTrax[p][5] == -1) ? -1 : tempNorm[p] // going to leave first point as -1
	// correct speed column
	matTrax[][6] = (matTrax[p][6] == -1) ? -1 : tempNorm[p] / tStep
	// make sure 1st point is -1
	matTrax[0][5,6] = -1
End

// This function will load the tracking data from an Excel Workbook
///	@param	ii	variable containing row number from condWave
Function CorrectMigration(ii)
	Variable ii
	
	WAVE/T condWave = root:condWave
	String condName = condWave[ii]
	WAVE/T PathWave2 = root:PathWave2
	String pathString = PathWave2[ii]
	Variable len = strlen(pathString)
	if(len == 0)
		return -1
	elseif(numtype(len) == 2)
		return -1
	endif
	
	String sheet, prefix, matName, wList, mName
	String fileList
	Variable moviemax,csvOrNot
	Variable i
	
	if(StringMatch(pathString, "*.xls*") == 1)
		// set variable to indicate Excel Workbook
		csvOrNot = 0
		// Works out what sheets are in Excel Workbook and then loads each.
		XLLoadWave/J=1 PathWave2[ii]
		fileList = S_value
	else
		// set variable to indicate csv file
		csvOrNot = 1
		// Work out what files are in directory
		NewPath/O/Q ExpDiskFolder, pathString
		fileList = IndexedFile(expDiskFolder,-1,".csv")
	endif
	fileList = SortList(fileList, ";", 16)
	moviemax = ItemsInList(fileList)
		
	for(i = 0; i < moviemax; i += 1)
		sheet = StringFromList(i,fileList)
		prefix = "stat_" + "c_" + num2str(i)	// use stat prefix
		matName = "stat_" + num2str(i)
		if(csvOrNot == 0)
			XLLoadWave/S=sheet/R=(A1,H2000)/O/K=0/N=$prefix/Q PathWave2[ii]
		else
			LoadWave/A=$prefix/J/K=1/L={0,1,0,0,0}/O/P=expDiskFolder/Q sheet
		endif
		wList = wavelist(prefix + "*",";","")	// make matrix for each sheet
		Concatenate/O/KILL wList, $matName
		Wave matStat = $matName
		// Find corresponding movie matrix
		mName = ReplaceString("stat_",matname,condName + "_")
		Wave matTrax = $mName
		OffsetAndRecalc(matStat,matTrax)
	endfor
	
	Print "*** Offset data for condition", condName, "was loaded from", pathString

	// return moviemax back to calling function for checking
	return moviemax
End

// This function uses matStat to offset matTrax
Function OffsetAndRecalc(matStat,matTrax)
	Wave matStat,matTrax
	// Work out offset for the stat_* waves
	Variable x0 = matStat[0][3]
	Variable y0 = matStat[0][4]
	matStat[][3] -= x0
	matStat[][4] -= y0
	MatrixOp/O/FREE mStat2 = col(matStat,2)
	Variable maxFrame = WaveMax(mStat2)
	Variable j // because i refers to rows
	
	// offsetting loop
	for(j = 1; j < maxFrame + 1; j += 1)
		FindValue/V=(j) mStat2
		if(V_Value == -1)
			x0 = 0
			y0 = 0
		else
			x0 = matStat[V_Value][3]
			y0 = matStat[V_Value][4]
		endif
		matTrax[][3] = (matTrax[p][2] == j) ? matTrax[p][3] - x0 : matTrax[p][3]
		matTrax[][4] = (matTrax[p][2] == j) ? matTrax[p][4] - y0 : matTrax[p][4]
	endfor
	WAVE/Z paramWave = root:paramWave
	Variable tStep = paramWave[1]
	Variable pxSize = paramWave[2]
	// make new distance column
	Duplicate/O/RMD=[][3,4]/FREE matTrax,tempDist // take offset coords
	Differentiate/METH=2 tempDist
	tempDist[][] = (matTrax[p][5] == -1) ? 0 : tempDist[p][q]
	MatrixOp/O/FREE tempNorm = sqrt(sumRows(tempDist * tempDist))
	tempNorm[] *= pxSize // convert to real distance
	matTrax[][5] = (matTrax[p][5] == -1) ? -1 : tempNorm[p] // going to leave first point as -1
	// correct speed column
	matTrax[][6] = (matTrax[p][6] == -1) ? -1 : tempNorm[p] / tStep
	// put 1st point as -1
	matTrax[0][5,6] = -1
End

// This function will make cumulative distance waves for each cell. They are called cd_*
///	@param	ii	variable containing row number from condWave
Function MakeTracks(ii)
	Variable ii
	
	WAVE/T condWave = root:condWave
	String condName = condWave[ii]
	WAVE/Z paramWave = root:paramWave
	Variable tStep = paramWave[1]
	Variable pxSize = paramWave[2]
	WAVE/Z colorWave = root:colorWave
	
	String wList0 = WaveList(condName + "_*",";","") // find all matrices
	Variable nWaves = ItemsInList(wList0)
	
	Variable nTrack,nTrace=0
	String mName0, newName, plotName, avList, avName, errName
	Variable i, j
	
	String layoutName = condName + "_layout"
	KillWindow/Z $layoutName		// Kill the layout if it exists
	NewLayout/HIDE=1/N=$layoutName	

	// cumulative distance and plot over time	
	plotName = condName + "_cdplot"
	KillWindow/Z $plotName	// set up plot
	Display/N=$plotName/HIDE=1

	for(i = 0; i < nWaves; i += 1)
		mName0 = StringFromList(i,wList0)
		WAVE m0 = $mName0
		Duplicate/O/RMD=[][5,5] m0, $"tDistW"	// distance
		Duplicate/O/RMD=[][1,1] m0, $"tCellW"	// cell number
		WAVE tDistW,tCellW
		Redimension/N=-1 tDistW, tCellW // make 1D
		nTrack = WaveMax(tCellW)	// find maximum track number
		for(j = 1; j < (nTrack+1); j += 1)	// index is 1-based
			newName = "cd_" + mName0 + "_" + num2str(j)
			Duplicate/O tDistW, $newName
			WAVE w2 = $newName
			w2 = (tCellW[p] == j) ? tDistW[p] : NaN
			WaveTransform zapnans w2
			if(numpnts(w2) <= (ceil(60/tstep)))
				KillWaves/Z w2	// get short tracks and any tracks that didn't exist
			else
				w2[0] = 0	// first point in distance trace is -1 so correct this
				Integrate/METH=0 w2	// make cumulative distance
				SetScale/P x 0,tStep, w2
				AppendtoGraph/W=$plotName $newName
				nTrace += 1
			endif
		endfor
		KillWaves/Z tDistW
	endfor
	Variable alphaLevel = DecideOpacity(nTrace)
	ModifyGraph/W=$plotName rgb=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2],alphaLevel)
	avList = Wavelist("cd*",";","WIN:"+ plotName)
	avName = "W_Ave_cd_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	fWaveAverage(avList, "", 3, 1, avName, errName)
	AppendToGraph/W=$plotName $avName
	Label/W=$plotName left "Cumulative distance (\u03BCm)"
	Label/W=$plotName bottom "Time (min)"
	ErrorBars/W=$plotName $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($ErrName,$ErrName)
	ModifyGraph/W=$plotName lsize($avName)=2,rgb($avName)=(0,0,0)
	SetAxis/W=$plotName/A/N=1 left
	
	AppendLayoutObject/W=$layoutName graph $plotName
	
	// instantaneous speed over time	
	plotName = condName + "_ivplot"
	KillWindow/Z $plotName	// set up plot
	Display/N=$plotName/HIDE=1

	for(i = 0; i < nWaves; i += 1)
		mName0 = StringFromList(i,wList0)
		WAVE m0 = $mName0
		Duplicate/O/RMD=[][5,5] m0, $"tDistW"	// distance
		Duplicate/O/RMD=[][1,1] m0, $"tCellW"	// cell number
		WAVE tDistW,tCellW
		Redimension/N=-1 tDistW, tCellW // make 1D
		nTrack = WaveMax(tCellW)	// find maximum track number
		for(j = 1; j < (nTrack+1); j += 1)	// index is 1-based
			newName = "iv_" + mName0 + "_" + num2str(j)
			Duplicate/O tDistW, $newName
			WAVE w2 = $newName
			w2 = (tCellW[p] == j) ? tDistW[p] : NaN
			WaveTransform zapnans w2
			if(numpnts(w2) <= (ceil(60/tstep)))
				KillWaves w2
			else
				w2[0] = 0	// first point in distance trace is -1, so correct this
				w2 /= tStep	// make instantaneous speed (units are µm/min)
				SetScale/P x 0,tStep, w2
				AppendtoGraph/W=$plotName $newName
			endif
		endfor
		KillWaves/Z tDistW
	endfor
	ModifyGraph/W=$plotName rgb=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2],alphaLevel)
	avList = Wavelist("iv*",";","WIN:"+ plotName)
	avName = "W_Ave_iv_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	fWaveAverage(avList, "", 3, 1, avName, errName)
	AppendToGraph/W=$plotName $avName
	Label/W=$plotName left "Instantaneous Speed (\u03BCm/min)"
	Label/W=$plotName bottom "Time (min)"
	ErrorBars/W=$plotName $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($ErrName,$ErrName)
	ModifyGraph/W=$plotName lsize($avName)=2,rgb($avName)=(0,0,0)
	SetAxis/W=$plotName/A/N=1 left
	
	AppendLayoutObject/W=$layoutName graph $plotName
	// print a message to say how many valid tracks we have in this condition
	Print ItemsInList(avList), "valid tracks plotted for", condName
	
	plotName = condName + "_ivHist"
	KillWindow/Z $plotName	//set up plot
	Display/N=$plotName/HIDE=1
	
	Concatenate/O/NP avList, tempwave
	newName = "h_iv_" + condName	// note that this makes a name like h_iv_Ctrl
	Variable bval=ceil(wavemax(tempwave)/(sqrt((3*pxsize)^2)/tStep))
	Make/O/N=(bval) $newName
	Histogram/B={0,(sqrt((3*pxsize)^2)/tStep),bVal} tempwave,$newName
	AppendToGraph/W=$plotName $newName
	ModifyGraph/W=$plotName rgb=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2])
	ModifyGraph/W=$plotName mode=5,hbFill=4
	SetAxis/W=$plotName/A/N=1/E=1 left
	SetAxis/W=$plotName bottom 0,2
	Label/W=$plotName left "Frequency"
	Label/W=$plotName bottom "Instantaneous Speed (\u03BCm/min)"
	KillWaves/Z tempwave
	
	AppendLayoutObject/W=$layoutName graph $plotName
	
	// plot out tracks
	plotName = condName + "_tkplot"
	KillWindow/Z $plotName	//set up plot
	Display/N=$plotName/HIDE=1
	
	Variable off
	
	for(i = 0; i < nWaves; i += 1)
		mName0 = StringFromList(i,wList0)
		WAVE m0 = $mName0
		Duplicate/O/RMD=[][3,3] m0, $"tXW"	//x pos
		Duplicate/O/RMD=[][4,4] m0, $"tYW"	//y pos
		Duplicate/O/RMD=[][1,1] m0, $"tCellW"	//track number
		WAVE tXW,tYW,tCellW
		Redimension/N=-1 tXW,tYW,tCellW		
		nTrack = WaveMax(tCellW)	//find maximum track number
		for(j = 1; j < (nTrack+1); j += 1)	//index is 1-based
			newName = "tk_" + mName0 + "_" + num2str(j)
			Duplicate/O tXW, $"w3"
			WAVE w3
			w3 = (tCellW[p] == j) ? w3[p] : NaN
			WaveTransform zapnans w3
			if(numpnts(w3) <= (ceil(60/tstep)))
				KillWaves w3
			else
				off = w3[0]
				w3 -= off	//set to origin
				w3 *= pxSize
				// do the y wave
				Duplicate/O tYW, $"w4"
				WAVE w4
				w4 = (tCellW[p] == j) ? w4[p] : NaN
				WaveTransform zapnans w4
				off = w4[0]
				w4 -= off
				w4 *= pxSize
				Concatenate/O/KILL {w3,w4}, $newName
				Wave w5 = $newName
				AppendtoGraph/W=$plotName w5[][1] vs w5[][0]
			endif
		endfor
		Killwaves/Z tXW,tYW,tCellW //tidy up
	endfor
	ModifyGraph/W=$plotName rgb=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2],alphaLevel)
	// set these graph limits temporarily
	SetAxis/W=$plotName left -250,250
	SetAxis/W=$plotName bottom -250,250
	ModifyGraph/W=$plotName width={Plan,1,bottom,left}
	ModifyGraph/W=$plotName mirror=1
	ModifyGraph/W=$plotName grid=1
	ModifyGraph/W=$plotName gridRGB=(32767,32767,32767)
	
	AppendLayoutObject/W=$layoutName graph $plotName
	
	// calculate d/D directionality ratio
	plotName = condName + "_dDplot"
	KillWindow/Z $plotName	// setup plot
	Display/N=$plotName/HIDE=1
	
	String wName0, wName1
	Variable len
	wList0 = WaveList("tk_" + condName + "_*", ";","")
	nWaves = ItemsInList(wList0)
	
	for(i = 0; i < nWaves; i += 1)
		wName0 = StringFromList(i,wList0)			// tk wave
		wName1 = ReplaceString("tk",wName0,"cd")	// cd wave
		WAVE w0 = $wName0
		WAVE w1 = $wName1
		newName = ReplaceString("tk",wName0,"dD")
		Duplicate/O w1 $newName
		WAVE w2 = $newName
		len = numpnts(w2)
		w2[] = (w1[p] == 0) ? 1 : sqrt(w0[p][0]^2 + w0[p][1]^2) / w1[p]
		w2[0] = NaN	// d/D at point 0 is not a number
		AppendtoGraph/W=$plotName w2
	endfor
	ModifyGraph/W=$plotName rgb=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2],alphaLevel)
	avList = Wavelist("dD*",";","WIN:"+ plotName)
	avName = "W_Ave_dD_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	fWaveAverage(avList, "", 3, 1, avName, errName)
	AppendToGraph/W=$plotName $avName
	Label/W=$plotName left "Directionality ratio (d/D)"
	Label/W=$plotName bottom "Time (min)"
	ErrorBars/W=$plotName $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($ErrName,$ErrName)
	ModifyGraph/W=$plotName lsize($avName)=2,rgb($avName)=(0,0,0)
	
	AppendLayoutObject/W=$layoutName graph $plotName
	
	// calculate MSD (overlapping method)
	plotName = condName + "_MSDplot"
	KillWindow/Z $plotName	//setup plot
	Display/N=$plotName/HIDE=1
	
	wList0 = WaveList("tk_" + condName + "_*", ";","")
	nWaves = ItemsInList(wList0)
	Variable k
	
	for(i = 0; i < nWaves; i += 1)
		wName0 = StringFromList(i,wList0)	// tk wave
		WAVE w0 = $wName0
		len = DimSize(w0,0)
		newName = ReplaceString("tk",wName0,"MSD")	// for results of MSD per cell
		Make/O/N=(len-1,len-1,2)/FREE tempMat0,tempMat1
		// make 2 3D waves. 0 is end point to measure MSD, 1 is start point
		// layers are x and y
		tempMat0[][][] = (p >= q) ? w0[p+1][r] : 0
		tempMat1[][][] = (p >= q) ? w0[p-q][r] : 0
		MatrixOp/O/FREE tempMat2 = (tempMat0 - tempMat1) * (tempMat0 - tempMat1))
		Make/O/N=(len-1)/FREE countOfMSDPnts = (len-1)-p
		MatrixOp/O $newName = sumcols(sumbeams(tempMat2))^t / countOfMSDPnts
		SetScale/P x 0,tStep, $newName
		AppendtoGraph/W=$plotName $newName
	endfor
	ModifyGraph/W=$plotName rgb=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2],alphaLevel)
	avList = Wavelist("MSD*",";","WIN:"+ plotName)
	avName = "W_Ave_MSD_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	fWaveAverage(avList, "", 3, 1, avName, errName)
	AppendToGraph/W=$plotName $avName
	ModifyGraph/W=$plotName log=1
	SetAxis/W=$plotName/A/N=1 left
	len = numpnts($avName)*tStep
	SetAxis/W=$plotName bottom tStep,(len/2)
	Label/W=$plotName left "MSD"
	Label/W=$plotName bottom "Time (min)"
	ErrorBars/W=$plotName $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($errName,$errName)
	ModifyGraph/W=$plotName lsize($avName)=2,rgb($avName)=(0,0,0)
	
	AppendLayoutObject /W=$layoutName graph $plotName
	
	// calculate direction autocorrelation
	plotName = condName + "_DAplot"
	KillWindow/Z $plotName	// setup plot
	Display/N=$plotName/HIDE=1
	
	for(i = 0; i < nWaves; i += 1)
		wName0 = StringFromList(i,wList0)			// tk wave
		WAVE w0 = $wName0
		len = DimSize(w0,0)	// len is number of frames
		Differentiate/METH=2/DIM=0/EP=1 w0 /D=vWave // make vector wave (vWave). nVectors is len-1
		MatrixOp/O/FREE magWave = sqrt(sumrows(vWave * vWave)) // calculate magnitude of each vector
		vWave[][] /= magWave[p]	// normalise vectors
		newName = ReplaceString("tk",wName0,"DA")	// for results of DA per cell
		Make/O/N=(len-2,len-2,2)/FREE tempDAMat0,tempDAMat1
		tempDAMat0[][][] = (p >= q) ? vWave[p-q][r] : 0
		tempDAMat1[][][] = (p >= q) ? vWave[p+1][r] : 0
		MatrixOp/O/FREE dotWave = (tempDAMat0 * tempDAMat1)
		MatrixOp/O/FREE alphaWave = sumBeams(dotWave)
		// Make average. Previously we did this:
//		Make/O/N=(len-2)/FREE countOfDAPnts = (len-2)-p
//		MatrixOp/O $newName = sumcols(alphaWave)^t / countOfDAPnts
		// Now we need to get rid of NaNs in the alphaWave and count the non-NaN points
		MatrixOp/O/FREE alphaWave = replaceNans(alphaWave,0)
		Make/O/FREE/N=(dimsize(alphaWave,0),dimsize(alphaWave,1)) countOfDAPnts
		countOfDAPnts[][] = (abs(alphaWave[p][q]) > 0) ? 1 : 0
		MatrixOp/O $newName = sumcols(alphaWave)^t / sumCols(countOfDAPnts)^t
		SetScale/P x 0,tStep, $newName
		AppendtoGraph/W=$plotName $newName
	endfor
	Killwaves/Z vWave
	ModifyGraph/W=$plotName rgb=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2],alphaLevel)
	avList = Wavelist("DA*",";","WIN:"+ plotName)
	avName = "W_Ave_DA_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	fWaveAverage(avList, "", 3, 1, avName, errName)
	AppendToGraph/W=$plotName $avName
	SetAxis/W=$plotName left -1,1
	Label/W=$plotName left "Direction autocorrelation"
	Label/W=$plotName bottom "Time (min)"
	ErrorBars/W=$plotName $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($errName,$errName)
	ModifyGraph/W=$plotName lsize($avName)=2,rgb($avName)=(0,0,0)
	
	AppendLayoutObject/W=$layoutName graph $plotName
	
	// calculate the angle distribution
	plotName = condName + "_anglePlot"
	KillWindow/Z $plotName	// setup plot
	Display/N=$plotName/HIDE=1
	wList0 = WaveList(condName + "_*",";","") // find all matrices
	nWaves = ItemsInList(wList0)
	Concatenate/O/NP=0 wList0, allTempW // make long matrix of all tracks
	Variable nSteps = dimsize(allTempW,0)
	Make/O/N=(nSteps)/FREE tempDistThreshW
	Make/O/D/N=(nSteps) angleWave = NaN
	// quality filter so that minimal steps are not analysed
	tempDistThreshW[] = (allTempW[p][5] > 4 * pxSize) ? 1 : 0
	Variable successVar = 0 
	// this will find angles for all tracks even short tracks
	// valid track length has a lower bound of 1 h (v. 1.08)
	// at tStep = 10 this will find max 4 angles in a track that is not found elsewhere 
	
	for(i = 0; i < nSteps; i += 1)
		// find a proper step
		if(tempDistThreshW[i] == 1)
			Make/O/N=(2,2)/FREE matAA,matBB
			// set first vector offset to origin (need to transpose later)
			matAA[][] = allTempW[p + (i-1)][q+3] - allTempW[i-1][q+3]
			for(j = i+1; j < nSteps; j += 1)
				if(allTempW[j][1] != allTempW[i][1])
					// check there is another proper step from the same cell
					successVar = 0
					break
				elseif(tempDistThreshW[j] == 1)
					// find a proper step from same cell and set second vector as this, offset to origin
					successVar = 1
					matBB[][] = allTempW[p + (j-1)][q+3] - allTempW[j-1][q+3]
					break
				else
					successVar = 0
				endif
			endfor
			
			if(successVar == 1)
				// matrices need transposing
				MatrixTranspose matAA
				MatrixTranspose matBB
				// find cos(theta)
				MatrixOp/O/FREE matCC = matAA . matBB / (sqrt(sum(matAA * matAA)) * sqrt(sum(matBB * matBB)))
				// angle in radians
				AngleWave[i] = acos(matCC[0])
			endif
		endif
	endfor
	KillWaves/Z allTempW
	// zapnans on AngleWave so I can count valid angles
	WaveTransform zapnans AngleWave
	newName = "h_angle_" + condName
	Make/N=41/O $newName
	Histogram/B={0,pi/40,41} angleWave,$newName
	AppendToGraph/W=$plotName $newName
	ModifyGraph/W=$plotName rgb=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2])
	ModifyGraph/W=$plotName mode=5, hbFill=4
	SetAxis/W=$plotName/A/N=1/E=1 left
	SetAxis/W=$plotName bottom 0,pi
	Make/O/N=5 axisW = {0,pi/4,pi/2,3*pi/4,pi}
	// vulgar fractions and pi as Unicode
	Make/O/N=5/T axisTW = {"0","\u00BD\u03C0","\u00BD\u03C0","\u00BE\u03C0","\u03C0"}
	ModifyGraph/W=$plotName userticks(bottom)={axisW,axisTW}	
	Label/W=$plotName left "Density"
	Label/W=$plotName bottom "Cell turning"
	
	AppendLayoutObject/W=$layoutName graph $plotName
	// print message about number of angles
	Print numpnts(AngleWave), "valid angles found from all tracks for", condName
	
	// Plot these averages to summary windows at the end
	avName = "W_Ave_cd_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	AppendToGraph/W=cdPlot $avName
	ErrorBars/W=cdPlot $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($errName,$errName)
	ModifyGraph/W=cdPlot lsize($avName)=2,rgb($avName)=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2])
	
	avName = "W_Ave_iv_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	AppendToGraph/W=ivPlot $avName
	ErrorBars/W=ivPlot $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($errName,$errName)
	ModifyGraph/W=ivPlot lsize($avName)=2,rgb($avName)=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2])
	
	newName = "h_iv_" + condName
	AppendToGraph/W=ivHPlot $newName
	ModifyGraph/W=ivHPlot rgb($newName)=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2])
	
	avName = "W_Ave_dD_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	AppendToGraph/W=dDPlot $avName
	ErrorBars/W=dDPlot $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($errName,$errName)
	ModifyGraph/W=dDPlot lsize($avName)=2,rgb($avName)=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2])
			
	avName = "W_Ave_MSD_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	AppendToGraph/W=MSDPlot $avName
	ErrorBars/W=MSDPlot $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($errName,$errName)
	ModifyGraph/W=MSDPlot lsize($avName)=2,rgb($avName)=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2])
	
	avName = "W_Ave_DA_" + condName
	errName = ReplaceString("Ave", avName, "Err")
	AppendToGraph/W=DAPlot $avName
	ErrorBars/W=DAPlot $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($errName,$errName)
	ModifyGraph/W=DAPlot lsize($avName)=2,rgb($avName)=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2])
	
	newName = "h_angle_" + condName
	AppendToGraph/W=angleHPlot $newName
	ModifyGraph/W=angleHPlot rgb($newName)=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2])
End

///	@param	plotString	String such as "*_tkPlot" to determine which plot get rescaled
///	@param	limitVar	Variable containing the furthest point
STATIC Function RescalePlotsToLimits(plotString,limitVar)
	String plotString
	Variable limitVar
	// we set a temporary limit of -250,250 to all tkPlots
	if(limitVar > 250)
		limitVar = ceil(limitVar / 50) * 50
		String plotList = WinList(plotString,";","WIN:1")
		String plotName
		Variable i
		for(i = 0; i < ItemsInList(plotList); i += 1)
			plotName = StringFromList(i,plotList)
			SetAxis/W=$plotName left -limitVar,limitVar
			SetAxis/W=$plotName/Z bottom -limitVar,limitVar
			SetAxis/W=$plotName/Z top -limitVar,limitVar
		endfor
		return 1
	else
		return 0
	endif
End

// This function will make ImageQuilts and sparklines of 2D tracks for all conditions
/// @param qSize	Variable to indicate desired size of image quilt (qSize^2 tracks)
Function MakeImageQuilt(qSize)
	Variable qSize
	
	WAVE/T condWave = root:condWave
	Variable cond = numpnts(condWave)
	Wave paramWave = root:paramWave
	Variable tStep = paramWave[1]
	Wave colorWave = root:colorWave
	String condName, dataFolderName, wName
	Variable longestCond = 0 , mostFrames = 0
	
	Variable i
	
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		dataFolderName = "root:data:" + condName
		SetDataFolder datafolderName
		mostFrames = FindSolution()
		longestCond = max(longestCond,mostFrames)
	endfor
	SetDataFolder root:
	// Now they're all done, cycle again to find optimum quilt size
	Make/O/N=(longestCond,qSize+1,cond)/FREE optiMat
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		wName = "root:data:" + condName + ":solutionWave"
		Wave w0 = $wName
		optiMat[][][i] = (w0[p][0] >= q^2) ? 1 : 0
	endfor
	optiMat /= cond
	// make a 1D wave where row = qSize and value = frames that can be plotted for all cond
	MatrixOp/O/FREE quiltSizeMat = sumcols(floor(sumBeams(optiMat)))^t
	// find optimum
	quiltSizeMat *= p^2
	WaveStats/Q quiltSizeMat
	Variable optQSize = V_maxRowLoc
	Variable optDur = (V_max / V_maxRowLoc^2) - 1 // because 0-based
	Print qSize, "x", qSize, "quilt requested.", optQSize, "x", optQSize, "quilt with", optDur, "frames shown (", optDur * tStep, "min)."
	
	String plotName,sampleWName
	Variable startVar,endVar,xShift,yShift
	Variable spaceVar = 100 // this might need changing
	Variable j,k
	
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		dataFolderName = "root:data:" + condName
		SetDataFolder datafolderName
		// make image quilt for each condition
		plotName = condName + "_quilt"
		KillWindow/Z $plotName
		Display/N=$plotName/HIDE=1
		WAVE segValid, trackDurations
		WAVE/T trackNames
		segValid[] = (trackDurations[p] > optDur * tStep) ? p : NaN
		WaveTransform zapnans segValid
		StatsSample/N=(optQSize^2) segValid
		WAVE/Z W_Sampled
		sampleWName = "segSelected_" + condName
		Duplicate/O W_Sampled, $sampleWName
		Wave segSelected = $sampleWName
		Make/O/N=(optQSize^2)/T segNames
		Make/O/N=(optQSize^2) segLengths
		for(j = 0; j < optQSize^2; j += 1)
			segNames[j] = trackNames[segSelected[j]]
			wName = ReplaceString("tk_",segNames[j],"cd_") // get cum dist wave name
			Wave cdW0 = $wName
			segLengths[j] = cdW0[optDur] // store cum dist at point optDur
		endfor
		Sort segLengths, segLengths, segNames
		// plot segNamed waves out
		Make/O/N=(optQSize^2*(optDur+1),2) quiltBigMat = NaN
		for(j = 0; j < optQSize^2; j += 1)
			wName = segNames[j]
			Wave tkW0 = $wName
			// put each track into the big quilt wave leaving a NaN between each
			startVar = j * optDur + (j * 1)
			endVar = startVar + optDur - 1
			quiltBigMat[startVar,endVar][] = tkW0[p-startVar][q]
			xShift = mod(j,optQSize) * spaceVar
			yShift = floor(j/optQSize) * spaceVar
			quiltBigMat[startVar,endVar][0] += xShift
			quiltBigMat[startVar,endVar][1] += yShift
		endfor
		// Add to plot and then format
		AppendToGraph/W=$plotName quiltBigMat[][1] vs quiltBigMat[][0]
		SetAxis/W=$plotName left (optQsize+1.5) * spaceVar,-2.5*spaceVar
		SetAxis/W=$plotName bottom -2.5*spaceVar,(optQsize+1.5) * spaceVar
		ModifyGraph/W=$plotName width={Aspect,1}
		ModifyGraph/W=$plotName manTick={0,100,0,0},manMinor={0,0}
		ModifyGraph/W=$plotName noLabel=2,mirror=1,standoff=0,tick=3
		ModifyGraph/W=$plotName grid=1,gridRGB=(34952,34952,34952)
		ModifyGraph axRGB=(65535,65535,65535)
		ModifyGraph/W=$plotName rgb=(colorWave[i][0],colorWave[i][1],colorWave[i][2])
		ModifyGraph/W=$plotName margin=14
		// Append to appropriate layout (page 2)
		String layoutName = condName + "_layout"
		LayoutPageAction/W=$layoutName appendPage
		AppendLayoutObject/W=$layoutName/PAGE=(2) graph $plotName
		ModifyLayout/W=$layoutName/PAGE=(2) left($plotName)=21,top($plotName)=291,width($plotName)=261,height($plotName)=261
		// make sparkline graphic
		plotName = condName + "_sprkln"
		KillWindow/Z $plotName
		Display/N=$plotName/HIDE=1
		// plot the diagonal of segNamed waves out laterally
		Make/O/N=(optQSize*(optDur+1),2) sprklnBigMat = NaN
		Variable theta
		k = 0
		for(j = 0; j < optQSize^2; j += optQsize+1)
			wName = segNames[j]
			Wave tkW0 = $wName
			Duplicate/O/FREE tkW0, sprkW0
			theta = (1.5 * pi) - atan2(sprkW0[optDur-1][1],sprkW0[optDur-1][0])
			Make/O/N=(2,2)/FREE rotMat = {{cos(theta),-sin(theta)},{sin(theta),cos(theta)}}
			MatrixMultiply sprkW0, rotMat
			WAVE/Z M_product
			// put each track into the big quilt wave leaving a NaN between each
			startVar = k * optDur + (k * 1)
			endVar = startVar + optDur - 1
			sprklnBigMat[startVar,endVar][] = M_Product[p-startVar][q]
			xShift = mod(k,optQSize) * spaceVar
			sprklnBigMat[startVar,endVar][0] += xShift
			k += 1
		endfor
		KillWaves/Z M_product
		// Add to plot and then format
		AppendToGraph/W=$plotName sprklnBigMat[][1] vs sprklnBigMat[][0]
		SetAxis/W=$plotName left 0.75 * spaceVar,-2.5*spaceVar
		SetAxis/W=$plotName bottom -0.5*spaceVar,(optQsize+0.5) * spaceVar
		ModifyGraph/W=$plotName width={Plan,1,bottom,left}
		ModifyGraph/W=$plotName manTick={0,100,0,0},manMinor={0,0}
		ModifyGraph/W=$plotName noLabel=2,mirror=1,standoff=0,tick=3
		ModifyGraph/W=$plotName grid=1,gridRGB=(34952,34952,34952)
		ModifyGraph axRGB=(65535,65535,65535)
		ModifyGraph/W=$plotName rgb=(colorWave[i][0],colorWave[i][1],colorWave[i][2])
		ModifyGraph/W=$plotName margin=14
		SetDrawLayer/W=$plotName UserBack
		SetDrawEnv/W=$plotName xcoord= prel,ycoord= left,linefgc= (21845,21845,21845),dash= 1
		DrawLine/W=$plotName 0,0,1,0
		SetDrawLayer/W=$plotName UserFront
		// Append to appropriate layout (page 2)
		AppendLayoutObject/W=$layoutName/PAGE=(2) graph $plotName
		ModifyLayout/W=$layoutName/PAGE=(2) left($plotName)=21,top($plotName)=558,width($plotName)=542,height($plotName)=180
	endfor
	SetDataFolder root:
	return optDur
End

Function FindSolution()
	String wList = WaveList("tk_*",";","")
	Variable nWaves = ItemsInList(wList)
	Make/O/N=(nWaves)/T trackNames
	Make/O/N=(nWaves) trackDurations, segValid
	Wave paramWave = root:paramWave
	Variable tStep = paramWave[1]
	String wName
	
	Variable i
	
	for(i = 0; i < nWaves; i += 1)
		wName = StringFromList(i,wList)
		trackNames[i] = wName
		Wave w0 = $wName
		trackDurations[i] = dimsize(w0,0) * tStep
	endfor
	
	// how many are longer than x hrs?
	Variable mostFrames = round(WaveMax(trackDurations) / tStep)
	Make/O/N=(mostFrames,nWaves) solutionMat
	// Find tracks that are longer than a given length of time
	solutionMat[][] = (trackDurations[q] > p * tStep) ? 1 : 0
	MatrixOp/O solutionWave = sumRows(solutionMat)
	return mostFrames
End

// This function will make Joint Histograms of 2D tracks for all conditions
// It uses the segValid calculation from MakeImageQuilts to find the correct tracks to plot out
// Two JH are made. One of the segValid camples for optDur length in their original state
// Second is a bootstrap of 50000 resamples of segValid tracks for optDur randomly oriented
Function MakeJointHistogram(optDur)
	Variable optDur
	
	WAVE/T condWave = root:condWave
	Variable cond = numpnts(condWave)
	Wave paramWave = root:paramWave
	Variable tStep = paramWave[1]
	Wave colorWave = root:colorWave
	String condName, dataFolderName, wName, plotName
	Variable leastValid = 1000
	
	Variable i,j
	
	// find lowest number of valid tracks in all conditions
	// Valid tracks are tracks with a length that could be plotted in the quilt
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		dataFolderName = "root:data:" + condName
		SetDataFolder datafolderName
		WAVE segValid
		leastValid = min(leastValid,numpnts(segValid))
	endfor
	
	String sampleWName
	Variable startVar,endVar
	Variable furthestPoint,theta
	Variable boot = 50000
	// now concatenate leastValid number of tracks from each condition
	for(i = 0; i < cond; i += 1)
		Make/O/N=(2,2)/FREE rotMat
		condName = condWave[i]
		dataFolderName = "root:data:" + condName
		SetDataFolder datafolderName
		WAVE/T trackNames
		WAVE segValid
		
		// take a random sample of leastValid number of tracks from each condition
		StatsSample/N=(leastValid) segValid
		WAVE/Z W_Sampled
		sampleWName = "segJHSelected_" + condName
		Duplicate/O W_Sampled, $sampleWName
		Wave segJHSelected = $sampleWName
		Make/O/N=(leastValid)/T segJHNames
		Make/O/N=(leastValid*optDur,2) bigTk
		for(j = 0; j < leastValid; j += 1)
			wName = trackNames[segJHSelected[j]]
			segJHNames[j] = wName
			Wave tkW0 = $wName
			startVar = j * optDur
			endVar = startVar + optDur - 1
			bigTk[startVar,endVar][] = tkW0[p-startVar][q]
		endfor
		
		// bootstrap leastValid number of tracks from each condition
		StatsResample/N=(boot) segValid
		WAVE/Z W_Resampled
		sampleWName = "segBJHSelected_" + condName
		Duplicate/O W_Resampled, $sampleWName
		Wave segBJHSelected = $sampleWName
		Make/O/N=(boot)/T segBJHNames
		Make/O/N=(boot*optDur,2) bigBTk
		for(j = 0; j < boot; j += 1)
			wName = trackNames[segBJHSelected[j]]
			segBJHNames[j] = wName
			Wave tkW0 = $wName
			startVar = j * optDur
			endVar = startVar + optDur - 1
			Duplicate/O/FREE tkW0,tempM0
			theta = pi*enoise(1)
			rotMat = {{cos(theta),-sin(theta)},{sin(theta),cos(theta)}}
			MatrixMultiply tempM0, rotMat
			WAVE/Z M_Product
			bigBTk[startVar,endVar][] = M_Product[p-startVar][q]
		endfor		
		// find the furthest point in x or y in either direction for all conditions
		furthestPoint = max(furthestPoint,wavemax(bigBTk),abs(wavemin(bigBTk)))
		KillWaves/Z W_Sampled,W_Resampled
	endfor
	
	// set up the color table and bin waves for joint histograms
	SetDataFolder root:
	LoadNiceCTableW()
	Variable binSize = 5
	Variable leftTop = (25 * (ceil(furthestPoint / 25) + 1)) + (binSize / 2)
	Variable nBins = (25 * (((ceil(furthestPoint / 25) + 1) * 2)) / binSize) + 2
	Make/O/N=(nBins)/FREE theBinsWave = (p * binSize) - leftTop
	Variable highestPoint = 0,highestBPoint = 0
	String JHName,BJHName
	
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		dataFolderName = "root:data:" + condName
		SetDataFolder datafolderName
		// do first JH
		WAVE bigTk
		Duplicate/O/RMD=[][0] bigTk,xData
		Duplicate/O/RMD=[][1] bigTk,yData
		WAVE/Z xData, yData
		JointHistogram/XBWV=theBinsWave/YBWV=theBinsWave xData,yData
		plotName = condName + "_JHplot"
		WAVE/Z M_JointHistogram
		JHName = "segJHMat"
		Duplicate/O M_JointHistogram, $JHName
		KillWindow/Z $plotName
		NewImage/N=$plotName/HIDE=1 $JHName
		ModifyImage/W=$plotName $JHName ctab= {1,*,root:Packages:ColorTables:Moreland:SmoothCoolWarm256,1},log=1,minRGB=NaN,maxRGB=0
		ModifyGraph/W=$plotName width={Aspect,1}
		ModifyGraph axRGB=(34952,34952,34952),tlblRGB=(34952,34952,34952),alblRGB=(34952,34952,34952)
		// scale the image
		SetScale/P x -leftTop,binSize,"", $JHName
		SetScale/P y -leftTop,binSize,"", $JHName
		SetAxis/W=$plotName left -250,250
		SetAxis/W=$plotName top -250,250
		// find the max point in z of JHs
		highestPoint = max(highestPoint,WaveMax($JHName))
		// Append 1st JH to layout
		String layoutName = condName + "_layout"
		AppendLayoutObject/W=$layoutName/PAGE=(2) graph $plotName
		ModifyLayout/W=$layoutName/PAGE=(2) left($plotName)=21,top($plotName)=21,width($plotName)=261,height($plotName)=261
		
		// now do BJH
		WAVE bigBTk
		Duplicate/O/RMD=[][0] bigBTk,xData
		Duplicate/O/RMD=[][1] bigBTk,yData
		JointHistogram/XBWV=theBinsWave/YBWV=theBinsWave xData,yData
		plotName = condName + "_BJHplot"
		BJHName = "segBJHMat"
		Duplicate/O M_JointHistogram, $BJHName
		KillWindow/Z $plotName
		NewImage/N=$plotName/HIDE=1 $BJHName
		ModifyImage/W=$plotName $BJHName ctab= {1,*,root:Packages:ColorTables:Moreland:SmoothCoolWarm256,1},log=1,minRGB=NaN,maxRGB=0
		ModifyGraph/W=$plotName width={Aspect,1}
		ModifyGraph axRGB=(34952,34952,34952),tlblRGB=(34952,34952,34952),alblRGB=(34952,34952,34952)
		// scale the image
		SetScale/P x -leftTop,binSize,"", $BJHName
		SetScale/P y -leftTop,binSize,"", $BJHName
		SetAxis/W=$plotName left -250,250
		SetAxis/W=$plotName top -250,250
		highestBPoint = max(highestBPoint,WaveMax($BJHName))
		// now append the BJH
		AppendLayoutObject/W=$layoutName/PAGE=(2) graph $plotName
		ModifyLayout/W=$layoutName/PAGE=(2) left($plotName)=291,top($plotName)=21,width($plotName)=261,height($plotName)=261
		
		KillWaves/Z xData,yData, M_JointHistogram, bigTk,bigBTk
	endfor
	// convert highest points to ceil log10 value
	highestPoint = ceil(log(highestPoint))
	highestBPoint = ceil(log(highestBPoint))
	// now go back around a scale the joint histograms to the same max and add colorscale
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		dataFolderName = "root:data:" + condName
		SetDataFolder datafolderName
		plotName = condName + "_JHplot"
		ModifyImage/W=$plotName $JHName ctab= {1,10^highestPoint,root:Packages:ColorTables:Moreland:SmoothCoolWarm256,0},log=1,minRGB=NaN,maxRGB=0
		ColorScale/W=$plotName/C/N=text0/F=0/B=1/A=RB/X=0.00/Y=1 vert=0,widthPct=50,heightPct=5,frame=0.00,image=$JHName,log=1,tickLen=2.00
		ModifyGraph/W=$plotName gfSize=8

		plotName = condName + "_BJHplot"
		ModifyImage/W=$plotName $BJHName ctab= {1,10^highestBPoint,root:Packages:ColorTables:Moreland:SmoothCoolWarm256,0},log=1,minRGB=NaN,maxRGB=0
		ColorScale/W=$plotName/C/N=text0/F=0/B=1/A=RB/X=0.00/Y=1 vert=0,widthPct=50,heightPct=5,frame=0.00,image=$BJHName,log=1,tickLen=2.00
		ModifyGraph/W=$plotName gfSize=8
	endfor
	// rescale tk, JH and BJH plots
	RescalePlotsToLimits("*_tkplot",furthestPoint)
	RescalePlotsToLimits("*_JHplot",furthestPoint)
	RescalePlotsToLimits("*_BJHplot",furthestPoint)
	SetDataFolder root:
End

// Function to sort out the summary layout
// Traces have been added and colored as we went along, but now to format them
// and add some summary graphs
Function TidyUpSummaryLayout()
	SetDataFolder root:
	WAVE/T condWave = root:condWave
	Variable cond = numpnts(condWave)
	Wave paramWave = root:paramWave
	Variable tStep = paramWave[1]
	Variable segmentLength = paramWave[3]
	Wave colorWave = root:colorWave
	WAVE/T labelWave = root:labelWave

	// Tidy up summary windows
	SetAxis/W=cdPlot/A/N=1 left
	Label/W=cdPlot left "Cumulative distance (\u03BCm)"
	Label/W=cdPlot bottom "Time (min)"
		AppendLayoutObject /W=summaryLayout graph cdPlot
	SetAxis/W=ivPlot/A/N=1 left
	Label/W=ivPlot left "Instantaneous Speed (\u03BCm/min)"
	Label/W=ivPlot bottom "Time (min)"
		AppendLayoutObject /W=summaryLayout graph ivPlot
	SetAxis/W=ivHPlot/A/N=1 left
	SetAxis/W=ivHPlot bottom 0,2
	Label/W=ivHPlot left "Frequency"
	Label/W=ivHPlot bottom "Instantaneous Speed (\u03BCm/min)"
	ModifyGraph/W=ivHPlot mode=6
		AppendLayoutObject /W=summaryLayout graph ivHPlot
	Label/W=dDPlot left "Directionality ratio (d/D)"
	Label/W=dDPlot bottom "Time (min)"
		AppendLayoutObject /W=summaryLayout graph dDPlot
	ModifyGraph/W=MSDPlot log=1
	SetAxis/W=MSDPlot/A/N=1 left
	Wave w = WaveRefIndexed("MSDPlot",0,1)
	SetAxis/W=MSDPlot bottom tStep,((numpnts(w) * tStep)/2)
	Label/W=MSDPlot left "MSD"
	Label/W=MSDPlot bottom "Time (min)"
		AppendLayoutObject /W=summaryLayout graph MSDPlot
	SetAxis/W=DAPlot left 0,1
	Wave w = WaveRefIndexed("DAPlot",0,1)
	SetAxis/W=DAPlot bottom 0,((numpnts(w)*tStep)/2)
	Label/W=DAPlot left "Direction autocorrelation"
	Label/W=DAPlot bottom "Time (min)"
		AppendLayoutObject /W=summaryLayout graph DAPlot
	SetAxis/W=angleHPlot/A/N=1/E=1 left
	SetAxis/W=angleHPlot bottom 0,pi
	ModifyGraph/W=angleHPlot mode=6
	Make/O/N=5 axisW = {0,pi/4,pi/2,3*pi/4,pi}
	Make/O/N=5/T axisTW = {"0","\u00BD\u03C0","\u00BD\u03C0","\u00BE\u03C0","\u03C0"}
	ModifyGraph/W=angleHPlot userticks(bottom)={axisW,axisTW}	
	Label/W=angleHPlot left "Density"
	Label/W=angleHPlot bottom "Cell turning"
		AppendLayoutObject /W=summaryLayout graph angleHPlot
	
	// average the speed data and do strava calc from all conditions
	String wList, speedName, stravaName, wName, condName, datafolderName
	Variable nTracks, last, mostTracks
	Variable i, j
	
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		dataFolderName = "root:data:" + condName
		SetDataFolder $dataFolderName
		wList = WaveList("cd_" + condName + "_*", ";","")
		nTracks = ItemsInList(wList)
		speedName = "sum_Speed_" + condName
		stravaName = "sum_Strava_" + condName
		Make/O/N=(nTracks) $speedName, $stravaName
		Wave speedW = $speedName
		Wave stravaW = $stravaName
		for(j = 0; j < nTracks; j += 1)
			wName = StringFromList(j,wList)
			Wave w1 = $wName
			last = numpnts(w1) - 1	// finds last row (max cumulative distance)
			speedW[j] = w1[last]/(last*tStep)	// calculates speed
			stravaW[j] = StravaCalc(w1,segmentLength)
		endfor
		mostTracks = max(mostTracks,nTracks)
	endfor
	
	KillWindow/Z SpeedPlot
	Display/N=SpeedPlot/HIDE=1
	KillWindow/Z StravaPlot
	Display/N=StravaPlot/HIDE=1
	// now store the values in a blank matrix for category-style box/violinplots
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		dataFolderName = "root:data:" + condName
		SetDataFolder $dataFolderName
		speedName = "sum_Speed_" + condName
		stravaName = "sum_Strava_" + condName
		Wave speedW = $speedName
		Wave stravaW = $stravaName
		Make/O/N=(mostTracks,cond) $(ReplaceString("sum_S",speedName,"sum_MatS"))=NaN
		Make/O/N=(mostTracks,cond) $(ReplaceString("sum_S",stravaName,"sum_MatS"))=NaN
		Wave sum_MatSpeed = $(ReplaceString("sum_S",speedName,"sum_MatS"))
		Wave sum_MatStrava = $(ReplaceString("sum_S",stravaName,"sum_MatS"))
		sum_MatSpeed[0,numpnts(speedW)-1][i] = speedW[p]
		sum_MatStrava[0,numpnts(stravaW)-1][i] = stravaW[p]
		BuildBoxOrViolinPlot(sum_MatSpeed,"SpeedPlot",i)
		BuildBoxOrViolinPlot(sum_MatStrava,"StravaPlot", i)
	endfor
	Label/W=SpeedPlot left "Average speed (\u03BCm/min)"
	SetAxis/A/N=1/E=1/W=SpeedPlot left
	ModifyGraph/W=SpeedPlot toMode=-1
	Label/W=StravaPlot left "Fastest "+ num2str(segmentLength) + " \u03BCm (min)"
	SetAxis/A/N=1/E=1/W=StravaPlot left
	ModifyGraph/W=StravaPlot toMode=-1
	// add both new plots to summary layout
	AppendLayoutObject /W=summaryLayout graph SpeedPlot
	AppendLayoutObject /W=summaryLayout graph StravaPlot
	
	SetDataFolder root:
	// Tidy summary layout
	DoWindow/F summaryLayout
	// in case these are not captured as prefs
	LayoutPageAction size(-1)=(595, 842), margins(-1)=(18, 18, 18, 18)
	ModifyLayout units=0
	ModifyLayout frame=0,trans=1
	Execute /Q "Tile/A=(4,3)/O=1"
End

///	@param	w1	wave for calculation
///	@param	segmentLength	variable passed for speed, rather than looking up
STATIC Function StravaCalc(w1,segmentLength)
	Wave w1
	Variable segmentLength
	Variable nPnts = numpnts(w1)
	Variable maxDist = w1[nPnts-1]
	Variable returnVar = 1000
	Variable i
	
	for(i = 0 ; i < nPnts; i += 1)
		if(w1[i] + segmentLength > maxDist)
			break
		endif
		FindLevel/Q/r=[i] w1, w1[i] + segmentLength
		if(V_flag == 1)
			break
		endif
		returnVar = min(returnVar,V_LevelX)
	endfor
	
	if(returnVar == 1000)
		returnVar = NaN
	endif	
	return returnVar
End

// Saving csv output from Manual Tracking in FIJI is missing the first column
// It can also not be read by Manual Tracking because it is comma-separated and not tab-separated
Function Excel2CSV()
	SetDataFolder root:
	NewDataFolder/O/S root:convert
	
	XLLoadWave/J=1
	NewPath/O/Q path1, S_path
	Variable moviemax = ItemsInList(S_value)
	String sheet,csvFileName,prefix,wList,newName
	
	Variable i
	
	for(i = 0; i < moviemax; i += 1)
		sheet = StringFromList(i,S_Value)
		csvFileName = S_filename + "_" + sheet + ".csv"
		prefix = "tempW_" + num2str(i)
		XLLoadWave/S=sheet/R=(A1,H2000)/COLT="N"/O/K=0/N=$prefix/P=path1/Q S_fileName
		wList = wavelist(prefix + "*",";","")	//make matrices
		newName = "tempM_" + num2str(i)
		Concatenate/O/KILL wList, $newName
		Wave m0 = $newName
		CheckColumnsOfMatrix(m0)
		SetDimLabel 1,0,index,m0
		SetDimLabel 1,1,TrackNo,m0
		SetDimLabel 1,2,SliceNo,m0
		SetDimLabel 1,3,x,m0
		SetDimLabel 1,4,y,m0
		SetDimLabel 1,5,distance,m0
		SetDimLabel 1,6,velocity,m0
		SetDimLabel 1,7,pixelvalue,m0
		Save/J/M="\n"/U={0,0,1,0}/O/P=path1 m0 as csvFileName
		KillWaves/Z m0
	endfor
	SetDataFolder root:
	KillDataFolder/Z root:convert
End

Function CSV2CSV()
	SetDataFolder root:
	NewDataFolder/O/S root:convert
	
	NewPath/O/Q/M="Select directory of original CSVs" ExpDiskFolder
	NewPath/O/Q/M="Choose destination directory for converted CSVs" outputDiskFolder
	String fileList = IndexedFile(expDiskFolder,-1,".csv")
	Variable moviemax = ItemsInList(fileList)
	String sheet,csvFileName,prefix,newName,wList
	
	Variable i
	
	for(i = 0; i < moviemax; i += 1)
		sheet = StringFromList(i, fileList)
		prefix = "tempW_" + num2str(i)
		LoadWave/A=$prefix/J/K=1/L={0,1,0,0,0}/O/P=expDiskFolder/Q sheet
		wList = wavelist(prefix + "*",";","")	// make matrix for each sheet
		newName = "tempM_" + num2str(i)
		Concatenate/O/KILL wList, $newName
		Wave m0 = $newName
		CheckColumnsOfMatrix(m0)
		SetDimLabel 1,0,index,m0
		SetDimLabel 1,1,TrackNo,m0
		SetDimLabel 1,2,SliceNo,m0
		SetDimLabel 1,3,x,m0
		SetDimLabel 1,4,y,m0
		SetDimLabel 1,5,distance,m0
		SetDimLabel 1,6,velocity,m0
		SetDimLabel 1,7,pixelvalue,m0
		Save/J/M="\n"/U={0,0,1,0}/O/P=outputDiskFolder m0 as sheet
		KillWaves/Z m0
	endfor
	SetDataFolder root:
	KillDataFolder/Z root:convert
End


////////////////////////////////////////////////////////////////////////
// Panel functions
///////////////////////////////////////////////////////////////////////
///	@param	cond	number of conditions - determines size of box
Function myIO_Panel(cond)
	Variable cond
	
	Wave/Z colorWave = root:colorWave
	// make global text wave to store paths
	Make/T/O/N=(cond) condWave
	Make/T/O/N=(cond) PathWave1,PathWave2
	DoWindow/K FilePicker
	NewPanel/N=FilePicker/K=1/W=(40,40,840,150+30*cond)
	// labelling of columns
	DrawText/W=FilePicker 10,30,"Name"
	DrawText/W=FilePicker 160,30,"Cell tracking data (directory of CSVs or Excel file)"
	DrawText/W=FilePicker 480,30,"Optional: stationary data"
	DrawText/W=FilePicker 10,100+30*cond,"CellMigration"
	// do it button
	Button DoIt,pos={680,70+30*cond},size={100,20},proc=DoItButtonProc,title="Do It"
	// insert rows
	String buttonName1a,buttonName1b,buttonName2a,buttonName2b,boxName0,boxName1,boxName2
	Variable i
	
	for(i = 0; i < cond; i += 1)
		boxName0 = "box0_" + num2str(i)
		buttonName1a = "dir1_" + num2str(i)
		buttonName1b = "file1_" + num2str(i)
		boxName1 = "box1_" + num2str(i)
		buttonName2a = "dir2_" + num2str(i)
		buttonName2b = "file2_" + num2str(i)
		boxName2 = "box2_" + num2str(i)
		// row label
		DrawText/W=FilePicker 10,68+i*30,num2str(i+1)
		// condition label
		SetVariable $boxName0,pos={30,53+i*30},size={100,14},value= condWave[i], title=" "
		// dir button
		Button $buttonName1a,pos={160,50+i*30},size={38,20},proc=ButtonProc,title="Dir"
		// file button
		Button $buttonName1b,pos={200,50+i*30},size={38,20},proc=ButtonProc,title="File"
		// file or dir box
		SetVariable $boxName1,pos={240,53+i*30},size={220,14},value= PathWave1[i], title=" "
		// stationary dir button
		Button $buttonName2a,pos={480,50+i*30},size={38,20},proc=ButtonProc,title="Dir"
		// stationary button
		Button $buttonName2b,pos={520,50+i*30},size={38,20},proc=ButtonProc,title="File"
		// stationary or dir box
		SetVariable $boxName2,pos={560,53+i*30},size={220,14},value= PathWave2[i], title=" "
		SetDrawEnv fillfgc=(colorWave[i][0],colorWave[i][1],colorWave[i][2])
		DrawOval/W=FilePicker 130,50+i*30,148,68+i*30
	endfor
End

// define buttons
Function ButtonProc(ctrlName) : ButtonControl
	String ctrlName

	Wave/T PathWave1,PathWave2
	Variable refnum, wNum, ii
	String expr, wNumStr, iiStr, stringForTextWave

	if(StringMatch(ctrlName,"file*") == 1)
		expr="file([[:digit:]]+)\\w([[:digit:]]+)"
		SplitString/E=(expr) ctrlName, wNumStr, iiStr
		// get File Path
		Open/D/R/F="*.xls*"/M="Select Excel Workbook" refNum
		stringForTextWave = S_filename
	else
		expr="dir([[:digit:]]+)\\w([[:digit:]]+)"
		SplitString/E=(expr) ctrlName, wNumStr, iiStr
		// set outputfolder
		NewPath/O/Q DirOfCSVs
		PathInfo DirOfCSVs
		stringForTextWave = S_Path
	endif

	if (strlen(stringForTextWave) == 0) // user pressed cancel
		return -1
	endif
	wNum = str2num(wNumStr)
	ii = str2num(iiStr)
	if (wNum == 1)
		PathWave1[ii] = stringForTextWave
	else
		PathWave2[ii] = stringForTextWave
	endif
End

Function DoItButtonProc(ctrlName) : ButtonControl
	String ctrlName
 	
 	WAVE/T CondWave
	WAVE/T PathWave1
	Variable okvar = 0
	
	strswitch(ctrlName)	
		case "DoIt" :
			// check CondWave
			okvar = WaveChecker(CondWave)
			if (okvar == -1)
				Print "Error: Not all conditions have a name."
				break
			endif
			okvar = NameChecker(CondWave)
			if (okvar == -1)
				Print "Error: Two conditions have the same name."
				break
			endif
			okvar = WaveChecker(PathWave1)
			if (okvar == -1)
				Print "Error: Not all conditions have a file to load."
				break
			else
				Migrate()
			endif
	endswitch	
End

STATIC function WaveChecker(TextWaveToCheck)
	Wave/T TextWaveToCheck
	Variable nRows = numpnts(TextWaveToCheck)
	Variable len
	
	Variable i
	
	for(i = 0; i < nRows; i += 1)
		len = strlen(TextWaveToCheck[i])
		if(len == 0)
			return -1
		elseif(numtype(len) == 2)
			return -1
		endif
	endfor
	return 1
End

STATIC function NameChecker(TextWaveToCheck)
	Wave/T TextWaveToCheck
	Variable nRows = numpnts(TextWaveToCheck)
	Variable len
	
	Variable i,j
	
	for(i = 0; i < nRows; i += 1)
		for(j = 0; j < nRows; j += 1)
			if(j > i)
				if(cmpstr(TextWaveToCheck[i], TextWaveToCheck[j], 0) == 0)
					return -1
				endif
			endif
		endfor
	endfor
	return 1
End

////////////////////////////////////////////////////////////////////////
// Utility functions
////////////////////////////////////////////////////////////////////////
// Colours are taken from Paul Tol SRON stylesheet
// Colours updated. Brighter palette for up to 6 colours, then palette of 12 for > 6
// Define colours
StrConstant SRON_1 = "0x4477aa;"
StrConstant SRON_2 = "0x4477aa;0xee6677;"
StrConstant SRON_3 = "0x4477aa;0xccbb44;0xee6677;"
StrConstant SRON_4 = "0x4477aa;0x228833;0xccbb44;0xee6677;"
StrConstant SRON_5 = "0x4477aa;0x66ccee;0x228833;0xccbb44;0xee6677;"
StrConstant SRON_6 = "0x4477aa;0x66ccee;0x228833;0xccbb44;0xee6677;0xaa3377;"
StrConstant SRON_7 = "0x332288;0x88ccee;0x44aa99;0x117733;0xddcc77;0xcc6677;0xaa4499;"
StrConstant SRON_8 = "0x332288;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0xcc6677;0xaa4499;"
StrConstant SRON_9 = "0x332288;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0xcc6677;0x882255;0xaa4499;"
StrConstant SRON_10 = "0x332288;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0x661100;0xcc6677;0x882255;0xaa4499;"
StrConstant SRON_11 = "0x332288;0x6699cc;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0x661100;0xcc6677;0x882255;0xaa4499;"
StrConstant SRON_12 = "0x332288;0x6699cc;0x88ccee;0x44aa99;0x117733;0x999933;0xddcc77;0x661100;0xcc6677;0xaa4466;0x882255;0xaa4499;"

/// @param hex		variable in hexadecimal
Function hexcolor_red(hex)
	Variable hex
	return byte_value(hex, 2) * 2^8
End

/// @param hex		variable in hexadecimal
Function hexcolor_green(hex)
	Variable hex
	return byte_value(hex, 1) * 2^8
End

/// @param hex		variable in hexadecimal
Function hexcolor_blue(hex)
	Variable hex
	return byte_value(hex, 0) * 2^8
End

/// @param data	variable in hexadecimal
/// @param byte	variable to determine R, G or B value
STATIC Function byte_value(data, byte)
	Variable data
	Variable byte
	return (data & (0xFF * (2^(8*byte)))) / (2^(8*byte))
End

/// @param	cond	variable for number of conditions
Function MakeColorWave(cond)
	Variable cond
	
	// Pick colours from SRON palettes
	String pal
	if(cond == 1)
		pal = SRON_1
	elseif(cond == 2)
		pal = SRON_2
	elseif(cond == 3)
		pal = SRON_3
	elseif(cond == 4)
		pal = SRON_4
	elseif(cond == 5)
		pal = SRON_5
	elseif(cond == 6)
		pal = SRON_6
	elseif(cond == 7)
		pal = SRON_7
	elseif(cond == 8)
		pal = SRON_8
	elseif(cond == 9)
		pal = SRON_9
	elseif(cond == 10)
		pal = SRON_10
	elseif(cond == 11)
		pal = SRON_11
	else
		pal = SRON_12
	endif
	
	Variable color
	Make/O/N=(cond,3) root:colorwave
	WAVE colorWave = root:colorWave
	Variable i
	
	for(i = 0; i < cond; i += 1)
		// specify colours
		color = str2num(StringFromList(mod(i, 12),pal))
		colorwave[i][0] = hexcolor_red(color)
		colorwave[i][1] = hexcolor_green(color)
		colorwave[i][2] = hexcolor_blue(color)
	endfor
End

STATIC Function CleanSlate()
	String fullList = WinList("*", ";","WIN:7")
	Variable allItems = ItemsInList(fullList)
	String name
	Variable i
 
	for(i = 0; i < allItems; i += 1)
		name = StringFromList(i, fullList)
		KillWindow/Z $name		
	endfor
	
	// Kill waves in root
	KillWaves/A/Z
	// Look for data folders and kill them
	DFREF dfr = GetDataFolderDFR()
	allItems = CountObjectsDFR(dfr, 4)
	for(i = 0; i < allItems; i += 1)
		name = GetIndexedObjNameDFR(dfr, 4, i)
		KillDataFolder $name		
	endfor
End

Function/WAVE CleanUpCondWave(condWave)
	WAVE/T condWave
	Duplicate/O condWave, root:labelWave
	Variable nRows = numpnts(condWave)
	String nameStr
	Variable i
	
	for(i = 0; i < nRows; i += 1)
		condWave[i] = CleanupName(condWave[i],0)
	endfor
	
	return root:labelWave
End

STATIC Function LoadNiceCTableW()
	NewDataFolder/O root:Packages
	NewDataFolder/O root:Packages:ColorTables
	String/G root:Packages:ColorTables:oldDF = GetDataFolder(1)
	NewDataFolder/O/S root:Packages:ColorTables:Moreland
	LoadWave/H/O/P=Igor ":Color Tables:Moreland:SmoothCoolWarm256.ibw"
	KillStrings/Z/A
	SetDataFolder root:
	KillStrings/Z root:Packages:ColorTables:oldDF
	KillVariables/Z root:Packages:ColorTables:Moreland:V_flag
End

Function TidyCondSpecificLayouts()
	WAVE/T condWave = root:condWave
	String layoutName,condName,boxName
	Variable cond = numpnts(condWave)
	Variable pgMax = 2
	Variable i,j
	
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		layoutName = condName + "_layout"
		DoWindow/F $layoutName
		for(j = 1; j < pgMax + 1; j += 1)
			LayoutPageAction/W=$layoutName page=(j)
			LayoutPageAction/W=$layoutName size(-1)=(595, 842), margins(-1)=(18, 18, 18, 18)
			ModifyLayout/W=$layoutName units=0
			ModifyLayout/W=$layoutName frame=0,trans=1
			if(j == 1)
				Execute /Q "Tile"
			endif
			boxName = "text" + num2str(j)
			TextBox/W=$layoutName/C/N=$boxName/F=0/A=RB/X=0.00/Y=0.00 condName
		endfor
		LayoutPageAction/W=$layoutName page=(1)
	endfor
	DoUpdate
End

Function SaveAllReports()
	DoUpdate
	String layoutList = WinList("*layout", ";", "WIN:4")
	Variable nLayouts = ItemsInList(layoutList)
	if(nLayouts == 0)
		Abort "No reports to save!"
	endif
	
	NewPath/O/Q OutputPath
	String layoutName,fileName
	Variable pgMax = 2
	Variable i,j
	
	for(i = 0; i < nLayouts; i += 1)
		layoutName = StringFromList(i,layoutList)
		pgMax = 2
		// export graphs as PDF (EMF on Windows)
		if(defined(WINDOWS) == 1)
			if(CmpStr(layoutName, "summaryLayout") == 0)
				pgMax = 1
			endif
			// save all pages in a loop
			for(j = 1; j < pgMax +1 ; j += 1)
				fileName = layoutName + num2str(j) + ".emf"
				SavePICT/E=-2/P=OutputPath/WIN=$layoutName/W=(0,0,0,0)/PGR=(j,-1) as fileName
			endfor
		else
			fileName = layoutName + ".pdf"
			SavePICT/E=-2/P=OutputPath/WIN=$layoutName/W=(0,0,0,0)/PGR=(1,-1) as fileName
		endif
	endfor
End

Function RecolorAllPlots()
	SetDataFolder root:
	WAVE/Z colorWave = root:colorWave
	if(!WaveExists(colorWave))
		Abort "3-column colorwave required"
	endif
	Duplicate/O colorWave, colorWave_BKP
	// present dialog to work on recoloring
	//
	WAVE/T condWave = root:condWave
	Variable cond = numpnts(condWave)

	Variable i,j,k
	
	String plotList = "anglePlot;cdPlot;DAplot;dDplot;ivHist;ivPlot;MSDplot;quilt;sprkln;tkplot;"
	// plots with 0.5 alpha
	String halfList = "cdPlot;DAplot;dDplot;ivPlot;MSDplot;tkPlot;"
	plotList = RemoveFromList(halfList, plotList)
	Variable nPlots = ItemsInList(plotList)
	String condName,plotName,traceList,traceName
	Variable nTraces
	
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		for(j = 0; j < nPlots; j += 1)
			plotName = condName + "_" + StringFromList(j,plotList)
			traceList = TraceNameList(plotName, ";", 1)
			nTraces = ItemsInList(traceList)
			for(k = 0; k < nTraces; k += 1)
				traceName = StringFromList(k,traceList)
				if(stringmatch(traceName,"W_*") == 0)
					ModifyGraph/W=$plotName rgb($traceName)=(colorWave[i][0],colorWave[i][1],colorWave[i][2])
				endif
			endfor
		endfor
	endfor
	
	// now recolor traces with 0.5 alpha
	plotList = halfList
	nPlots = ItemsInList(plotList)
	
	for(i = 0; i < cond; i += 1)
		condName = condWave[i]
		for(j = 0; j < nPlots; j += 1)
			plotName = condName + "_" + StringFromList(j,plotList)
			traceList = TraceNameList(plotName, ";", 1)
			nTraces = ItemsInList(traceList)
			for(k = 0; k < nTraces; k += 1)
				traceName = StringFromList(k,traceList)
				if(stringmatch(traceName,"W_*") == 0)
					ModifyGraph/W=$plotName rgb($traceName)=(colorWave[i][0],colorWave[i][1],colorWave[i][2],32767)
				endif
			endfor
		endfor
	endfor
	
	plotlist = "angleHPlot;cdPlot;DAPlot;dDplot;ivHPlot;ivPlot;MSDPlot;"
	nPlots = ItemsInList(plotList)
	
	for(i = 0; i < nPlots; i += 1)
		plotName = StringFromList(i,plotList)
		traceList = TraceNameList(plotName, ";", 1)
		nTraces = ItemsInList(traceList)
		for(j = 0; j < nTraces; j += 1)
			traceName = StringFromList(j,traceList)
			for(k = 0; k < cond; k += 1)
				condName = condWave[k]
				if(stringmatch(traceName,"*"+condName) == 1)
					ModifyGraph/W=$plotName rgb($traceName)=(colorWave[k][0],colorWave[k][1],colorWave[k][2])
				endif
			endfor
		endfor
	endfor
End

Function RerunAnalysis()
	SetDataFolder root:
	String fullList = WinList("*", ";","WIN:7")
	Variable allItems = ItemsInList(fullList)
	String name
	Variable i
 
	for(i = 0; i < allItems; i += 1)
		name = StringFromList(i, fullList)
		KillWindow/Z $name		
	endfor

	KillDataFolder root:data:
	WAVE/Z/T CondWave
	if(!WaveExists(CondWave))
		Abort "Something is wrong. Cannot rerun the analysis."
	endif
	Migrate()
End

Function AboutCellMigr()
	String vStr = "CellMigration\rVersion " + num2str(GetProcedureVersion("CellMigration.ipf"))
	DoAlert 0, vStr
End

// Function from aclight to retrieve #pragma version number
/// @param procedureWinTitleStr	This is the procedure window "LoadMigration.ipf"
Function GetProcedureVersion(procedureWinTitleStr)
	String procedureWinTitleStr
 
	// By default, all procedures are version 1.00 unless
	// otherwise specified.
	Variable version = 1.00
	Variable versionIfError = NaN
 
	String procText = ProcedureText("", 0, procedureWinTitleStr)
	if (strlen(procText) <= 0)
		return versionIfError		// Procedure window doesn't exist.
	endif
 
	String regExp = "(?i)(?:^#pragma|\\r#pragma)(?:[ \\t]+)version(?:[\ \t]*)=(?:[\ \t]*)([\\d.]*)"
 
	String versionFoundStr
	SplitString/E=regExp procText, versionFoundStr
	if (V_flag == 1)
		version = str2num(versionFoundStr)
	endif
	return version	
End

STATIC Function DecideOpacity(nTrace)
	Variable nTrace
	Variable alpha
	if(nTrace < 10)
		alpha = 1
	elseif(nTrace < 50)
		alpha = 0.5
	elseif(nTrace < 100)
		alpha = 0.3
	else
		alpha = 0.2
	endif
	alpha = round(65535 * alpha)
	return alpha
End

// This function will make a "multicolumn" boxplot or violinplot (Igor >8 only) 
///	@param	matA	matrix of points to be appended
///	@param	plotName	string to tell igor which graph window to work on
///	@param	ii	variable to indicate which condition (for coloring)
STATIC Function BuildBoxOrViolinPlot(matA,plotName,ii)
	WAVE matA
	String plotName
	Variable ii
	
	String wName = NameOfWave(matA)
	Wave/T/Z condWave = root:condWave
	Wave/Z colorWave = root:colorWave
	//  This works because all matrices passed to this function have the same dimensions
	Variable nTracks = DimSize(matA,0)
	if(nTracks < 100)
		AppendBoxPlot/W=$plotName matA vs condWave
		ModifyBoxPlot/W=$plotName trace=$wName,markers={19,-1,19},markerSizes={2,2,2}
		ModifyBoxPlot/W=$plotName trace=$wName,whiskerMethod=4
	else
		AppendViolinPlot/W=$plotName matA vs condWave
		ModifyViolinPlot/W=$plotName trace=$wName,ShowMean,MeanMarker=19,CloseOutline
		ModifyViolinPlot/W=$plotName trace=$wName,DataMarker=19
	endif
	Variable alphaLevel = DecideOpacity(nTracks)
	ModifyGraph/W=$plotName rgb($wName)=(colorWave[ii][0],colorWave[ii][1],colorWave[ii][2],alphaLevel)
End

// not currently used
///	@param	matA	2d wave of xy coords offset to origin
STATIC Function DistanceFinder(matA)
	Wave MatA
	MatrixOp/O/FREE tempNorm = sqrt(sumRows(matA * matA))
	return WaveMax(tempNorm)
End