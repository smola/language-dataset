#pragma rtGlobals=3		// Use modern global access method.

Function fGFit(w,x)
	//Lifted from Igor's Multi-peak fitting 1.4 package
	WAVE w; Variable x
	
	Variable r= w[0]
	variable npts= numpnts(w),i=1
	do
		if( i>=npts )
			break
		EndIf
		r += w[i]*2^(-((x-w[i+1])/(w[i+2]/2))^2)
		i+=3
	while(1)
	return r
End

Function fLorFit(w,x)
	WAVE w; Variable x
	
	Variable r= w[0]
	variable npts= numpnts(w),i=1
	do
		if( i>=npts )
			break
		EndIf
		r += w[i]/(((x-w[i+1])/(w[i+2]/2))^2+1)
		i+=3
	while(1)
	return r
End

Function RecreateFits(timepoints,wavenumber)
	WAVE timepoints
	WAVE/T wavenumber
	WAVE Shiftx
	Variable counter, length,numpeaks,i
	String currenttime
	numpeaks=numpnts(wavenumber)
	length = numpnts(timepoints)
	Make/D/O/N=(numpeaks*3+1) Peak_coef_W
	for(counter=0;counter<length;counter+=1)
		If(timepoints[counter]<=0)
			currenttime="fit_m"+num2istr(abs(timepoints[counter]))+"_subg_nb"
		Else
			currenttime="fit_p"+num2istr(timepoints[counter])+"_subg_nb"
		EndIf
		// Make my coefficient WAVE
		Peak_coef_W[0]=0 // setting the baseline to 0
		for(i=0;i<numpeaks;i+=1)
			WAVE fitamp = $("fitamp_"+Wavenumber[i])
			WAVE fitfreq = $("fitfreq_"+wavenumber[i])
			WAVE fitwidth = $("fitwidth_"+wavenumber[i])
			Peak_coef_W[3*i+1]=fitamp[counter]
			Peak_coef_W[3*i+2]=fitfreq[counter]
			Peak_coef_W[3*i+3]=fitwidth[counter]
			If(numtype(Peak_coef_W[3*i+1])!=0)
				Peak_coef_W[3*i+1]=0
				Peak_coef_W[3*i+2]=1
				Peak_coef_W[3*i+3]=1
			EndIf
		EndFor
		Print currenttime
		Make/D/O/N=1340 $currenttime=fGfit(Peak_coef_W,shiftx)
	EndFor
End

Function fitTimeSeries(timepoints, pnt1, pnt2,wavenumber,Coefs,[gnd, Wiggle,Width,Plot,PolyNum,subrangeStart,subrangeEnd,suffix,peaktype])
//modified David Hoffman
	WAVE timepoints
	Variable pnt1, pnt2
	WAVE/T wavenumber
	WAVE Coefs
	WAVE gnd
	WAVE wiggle
	WAVE Width
	String Suffix
	Variable Plot
	Variable PolyNum
	Variable subrangeStart,subrangeEnd
	Variable PeakType
	
	Variable includeGND = !ParamIsDefault(gnd)		//The user has indicated that the ground state should be included in the fitting procedure
	
	if(ParamIsDefault(Plot))
		Plot=0
	EndIf
	
	if(ParamIsDefault(suffix))
		If(includeGND)
			Suffix="_subg"
		Else
			Suffix="_withg"
		EndIf
	EndIf
	
	If(ParamIsDefault(PolyNum))
		PolyNum = 4	//Cubic
	EndIf
	
	If(ParamIsDefault(Wiggle))
		Make/N=(numpnts(wavenumber))/O/D/FREE Vlimit = 20 //Change!
	Else
		Duplicate/O/FREE Wiggle Vlimit
	EndIf
	
	If(ParamIsDefault(Width))
		Make/N=(numpnts(wavenumber))/O/D/FREE MaxWidth = 100 //Change!
	Else
		Duplicate/O/FREE Width MaxWidth
	EndIf
	
	If(ParamIsDefault(PeakType))
		PeakType = 1 //Gaussian
	EndIf
	
	IF(WaveExists(root:shiftx))
		WAVE shiftx=root:shiftx
	Else
		DoAlert/T="fitTimeSeries Failed!" 0, "Why are you trying to run this macro so early!?"
		Return -1
	EndIf
	
	//***************************************************************//
	// time to do some error checking//
	if(numpnts(coefs)!=(numpnts(wavenumber)*3+1)) //Check to see if there are enough
		//coefficients for the number of peaks you will fit to.
		Print "The number of peak coefficients DID NOT match the number of peaks"
		Return 0 // Exit the function
	EndIf
	//***************************************************************//
	
	//This string holds the name of the current time point being fit
	String currenttime
	
	//These variables hold the lengths of the various waves so the don't need to be calculated again.
	Variable lengthT=numpnts(timepoints),lengthW=numpnts(wavenumber)
	//Some index variables for use later
	Variable i,j,k
	
	//Let's see if the user has opted to use a subrange, if not lets set the subrange to the full range
	If(ParamIsDefault(subrangeStart))
		subrangeStart = 0
	EndIf
	
	If(ParamIsDefault(subrangeEnd))
		subrangeEnd = lengthT
	EndIf
	
	//Now that we know we don't have any fundamental errors, let's print out the coefficients,
	// the wavenumbers and the points so that the user can find them later if need be
	
	//Print a row with the wavenumbers
	variable q=1
	If(q)
		Print " "
		Print "Initial guesses:"
		PrintF "Wavenumber:"
		For(i=0;i<lengthW;i+=1)
			PrintF "\t%s", wavenumber[i]
		EndFor
		PrintF "\r"
		//Print a row with the amp guesses
		PrintF "Amp:\t\t"
		For(i=0;i<lengthW;i+=1)
			PrintF "\t%g", Coefs[3*i+1]
		EndFor
		PrintF "\r"
		//Print a row with the center guesses
		PrintF "Center:\t"
		For(i=0;i<lengthW;i+=1)
			PrintF "\t%g", coefs[3*i+2]
		EndFor
		PrintF "\r"
		//Print a row with the width guesses
		PrintF "Width:\t"
		For(i=0;i<lengthW;i+=1)
			PrintF "\t\t%g", coefs[3*i+3]
		EndFor
		PrintF "\r"
		Print " "
		Print "Now in copiable form"
		Print "Make/T/O/N="+num2str(lengthW)+" "+nameofwave(wavenumber)+"; DelayUpdate"
		Print Wavenumber
		Print "Make/D/O/N="+num2str(numpnts(coefs))+" "+nameofwave(coefs)+"; DelayUpdate"
		Print coefs
		Print " "
		Print "Point A is "+num2str(pnt1)+" ("+num2str(shiftx[pnt1])+" cm-1) and Point B is "+num2str(pnt2)+" ("+num2str(shiftx[pnt2])+" cm-1)."
		Print " "
		Print "We'll be fitting the timepoints in between " + num2str(timepoints[subrangeStart]) + " and " + num2str(timepoints[subrangeEnd-1])
		Print " "
	EndIf
	
	Make/T/O/N=(lengthW) fitamp,fitfreq,fitwidth,fitamperror,fitfreqerror,fitwidtherror,fitarea,fitareaerror
	for(i=0;I<lengthW;i+=1)
		fitamp[i]="fitamp_"+wavenumber[i]
		fitfreq[i]="fitfreq_"+wavenumber[i]
		fitwidth[i]="fitwidth_"+wavenumber[i]
		fitamperror[i]="fitamperror_"+wavenumber[i]
		fitfreqerror[i]="fitfreqerror_"+wavenumber[i]
		fitwidtherror[i]="fitwidtherror_"+wavenumber[i]
		fitarea[i]="fitarea_"+wavenumber[i]
		fitareaerror[i]="fitareaerror_"+wavenumber[i]
		Make/D/O/N=(lengthT) $fitamp[i], $fitfreq[i],$fitwidth[i],$fitamperror[i],$fitfreqerror[i],$fitwidtherror[i],$fitarea[i],$fitareaerror[i]
	EndFor
	
	
	//WAVE coef,coeftemp
	string temp
	variable fitsuccess=0,fitfail=0,lengthC=numpnts(coefs)
	
	Variable V_fitOptions = 4	// suppress progress window
	Variable V_FitError = 0
	Variable V_FitMaxIters=2000
	//make the hold string to hold the baseline to zero we will be including one later
	String H_string="1"
	//for(i=1;i<lengthC;i+=3)
	//	H_string+="000"
	//EndFor
	
	//Make the Epsilon WAVE
	Make/D/O/N=(lengthC) epsilonWave = 1e-6
	for(i=1;i<lengthC;i+=3)
		epsilonWave[i] = 1e-8//Amplitude Epsilon
		epsilonWave[i+1] = 1e-6//Center freq epsilon
		epsilonWave[i+2] = 1e-6//Width Epsilon
	EndFor
	
	//make the constrating WAVE
	Make/D/O/T/N=((lengthC-1)*5/3) CTextWave //Because for the frequency and width we need
	//TWO constraints
	
	Variable Alimit=1e-8 //constrain the peaks to be greater than zero
	//Variable Vlimit=wiggle // constrain the possible frequencies (important for closely spaced peaks)
	
	k=0
	for(i=0;i<((lengthC-1)*5/3);i+=5)
		j=i*3/5 //this counter will take care of indexing the coefficient WAVE.
		//Amplitude constraint, it must maintain its polarity, i.e. it cannot go
		//from positive to negative or vice versa.
		If(coefs[j+1]>0)
			CTextWave[i]="K"+num2str(j+1)+">"+num2str(Alimit)
		Else
			CTextWave[i]="K"+num2str(j+1)+"<"+num2str(-Alimit)
		EndIf
		
		//frequency constraint, adjustable by the user, look above
		CTextWave[i+1]="K"+num2str(j+2)+">"+num2str(coefs[j+2]-Vlimit[k])
		CTextWave[i+2]="K"+num2str(j+2)+"<"+num2str(coefs[j+2]+Vlimit[k])
		
		//width constraint, these values seem to work for the red table
		CTextWave[i+3]="K"+num2str(j+3)+">0.1"
		CTextWave[i+4]="K"+num2str(j+3)+"<"+num2str(MaxWidth[k])
		k+=1
	EndFor
	
	If(q)
		Print "This is my constraint WAVE:"
		Print CTextWave
		Print " "
		//Print "H_string is "+H_string
		//Print " "
		//Print Coefs
	EndIf
	Duplicate/O coefs tempPeak_Coefs
	
	String F_String=""
	
	If(PeakType)
		If(q)
			Print  "Peak Type is Gaussian"
		Endif
		F_String="{fGFit, tempPeak_Coefs, hold=\"1\",EPSW=epsilonWave}"
	Else
		If(q)
			Print  "Peak Type is Lorentzian"
		EndIf
		F_String="{fLorFit, tempPeak_Coefs, hold=\"1\",EPSW=epsilonWave}"
	EndIf
	
	If(PolyNum>2)
		If(q)
			Print "Including an order", polynum-1, "polynomial for the baseline"
		EndIf
		Make/D/O/N=(PolyNum) tempBaseln_Coefs=0
		F_String+="{poly_XOffset "+num2str(polynum)+", tempBaseln_Coefs}"
	ElseIf(PolyNum!=0)
		If(q)
			Print "Including line for the baseline"
		EndIf
		Make/D/O/N=2 tempBaseln_Coefs=0
		F_String+="{line, tempBaseln_Coefs}"
		Polynum=2
	Else
		If(q)
			Print "No baseline!"
		Endif
		Make/D/O/N=1 tempBaseln_Coefs=0
	EndIf
	
	If(includeGND)
		//Declare my structure for fitting
		Struct scaledGroundStruct gndStruct
		//Apparently the WAVE keyword is necessary to create a wave reference
		WAVE gndStruct.gnd = gnd
		WAVE gndStruct.shift = shiftx
		
		Make/D/O/N=1 scaleFactor = -0.5
		
		Make/D/O/N=(LengthT) ScaleFactors, sigma_ScaleFactors
		
		F_String += "{scaleGround, scaleFactor,STRC=gndStruct}"
	ElseIf(q)
		Print "No ground included."
	EndIf
	
	if(q)
		Print ""
		Printf " Start fitting: -/"
	EndIf
	
	//String to hold the timepoints which weren't fit properly
	
	String badFits = "Bad Fits:\r"
	
	for(i=subrangeStart;i<subrangeEnd;i+=1)
		currenttime = myTime(timepoints[i])+suffix
		//**********HERE**************
		//duplicate/o coefs tempPeak_Coefs
		//tempBaseln_Coefs = 0
		//currenttime = "gnd_"+num2str(timepoints[i])+"uW"
		//**********HERE**************
		
		//Print F_string
		FuncFit/M=0/W=2/N/Q {string = F_String} $currenttime[pnt1, pnt2] /X=shiftx /D /C=CTextWave
		
		WAVE W_sigma
		
		If(includeGnd)
			ScaleFactors[i] = ScaleFactor[0]
			sigma_scaleFactors[i] = w_sigma[numpnts(w_sigma)-1]
		EndIf
		
		for(k=0;k<lengthW;k+=1)
			//Keeping track of all our parameters and putting them in reasonably
			//named waves!
			WAVE  wfitamp=$fitamp[k]
			WAVE  wfitfreq=$fitfreq[k]
			WAVE  wfitwidth=$fitwidth[k]
			WAVE  wfitarea=$fitarea[k]
			
			WAVE  wfitamperror=$fitamperror[k]
			WAVE  wfitfreqerror=$fitfreqerror[k]
			WAVE  wfitwidtherror=$fitwidtherror[k]
			WAVE  wfitareaerror=$fitareaerror[k]
			
			if(V_FitError!=0)
				wfitamp[i]=NaN
				wfitfreq[i] =NaN
				wfitwidth[i]=NaN
				wfitarea[i]=NaN
				
				wfitamperror[i]=NaN
				wfitfreqerror[i] =NaN
				wfitwidtherror[i]=NaN
				wfitareaerror[i]=NaN
			else
				wfitamp[i]=tempPeak_Coefs[3*k+1]
				wfitamperror[i]=w_sigma[3*k+1]
				
				wfitfreq[i] =tempPeak_Coefs[3*k+2]
				wfitfreqerror[i] =w_sigma[3*k+2]
				
				wfitwidth[i]=abs(tempPeak_Coefs[3*k+3])
				wfitwidtherror[i]=w_sigma[3*k+3]
				
				wfitarea[i]=wfitamp[i]*wfitwidth[i]
				wfitareaerror[i] = sqrt((wfitamp[i]*wfitwidtherror[i])^2+(wfitwidth[i]*wfitamperror[i])^2)
				
				If(PeakType)
					//Gaussian here
					wfitarea[i]*=sqrt(pi/Log(16))
					wfitareaerror[i] *= sqrt(pi/Log(16))
				Else
					//Lorentzian stuff here
					wfitarea[i]*=pi/2
					wfitareaerror[i] *= pi/2
				EndIf
			EndIf
		EndFor
		//tempBaseln_Coefs[0]=temppeak_Coefs[0]
		//tempBaseln_Coefs[1]=0
		//scaleFactor=-0.1
		
		If(V_FitError!=0)
			//Trying to fit a WAVE that doesn't exist shouldn't count against you.
			If(WaveExists($currenttime))
				badFits += currenttime+"\r"
				fitfail+=1
				if(q)
					Printf "X"// X for fail!
				Endif
				tempBaseln_Coefs = 0
			Elseif(q)
				Printf " DNE "//DNE=does not exist!
			EndIf
			//print coef
			duplicate/o coefs tempPeak_Coefs
			tempBaseln_Coefs = 0
		Else
			//Print "Fitting successful!"
			fitsuccess+=1
			if(q)
				Printf "O"//O for OK!
			EndIf
			If(PolyNum!=0)
				//Now I will make the fits and the baseline so that these can be plotted by the user.
				If(peaktype)
					Make/D/O/N=1340 $("fit_"+currenttime+"_nb") = fGFit(tempPeak_Coefs,shiftx)
				Else
					Make/D/O/N=1340 $("fit_"+currenttime+"_nb") = fLorFit(tempPeak_Coefs,shiftx)
				EndIf
				Make/D/O/N=1340 $("fit_"+currenttime+"_bl")
				WAVE tempBaseLineWave = $("fit_"+currenttime+"_bl")
				Duplicate/O $("fit_"+currenttime) $("fit_"+currenttime+"_bl2")
				WAVE tempBaseLineWave2 = $("fit_"+currenttime+"_bl2")
				tempBaseLineWave2=0
				
				//Clear the WAVE in the region in which we're working
				//tempBaseLineWave=tempBaseLineWave*(x<pnt1 && x>pnt2)
				tempBaseLineWave=tempBaseLineWave*(x<pnt1 || x>pnt2)
				//Now fill in only that region
				If(PolyNum>2)
					For(j=0;j<PolyNum;j+=1)
						tempBaseLineWave+=(tempBaseln_Coefs[j]*(shiftx-shiftx[pnt1])^j)*(x>=pnt1 && x<=pnt2)
						tempBaseLineWave2+=tempBaseln_Coefs[j]*(x-shiftx[pnt1])^j
					EndFor
				Else
				//Lines are handled slightly differently
					tempBaseLineWave+=(tempBaseln_Coefs[0]+tempBaseln_Coefs[1]*shiftx)*(x>=pnt1 && x<=pnt2)
					tempBaseLineWave2=tempBaseln_Coefs[0]+tempBaseln_Coefs[1]*x
				EndIf
				//We're doing this so that different regions can be fit separately.
			
				WAVE myWave = $currenttime
				Make/D/O/N=1340 $(currenttime+"_nb")=myWave-tempBaseLineWave
			EndIf
		EndIf
		if(q)
			DoUpdate
		EndIf
		
		V_FitError=0
		
		if (GetKeyState(0) & 32)	// Is Escape key pressed now?
			Printf "User abort: "
			Break
		EndIf
	EndFor
	If(q)
		PrintF "/-\r"
		Print " "
		Print "There were "+num2istr(fitsuccess)+" successful fittings and "+num2istr(fitfail)+" fitting failures in this run"
	EndIf
	
	If(fitfail!=0 && q)
		Print badFits
	EndIf
	
	If(Plot)//Plotting the results!
		for(k=0;k<lengthW;k+=1)
			tabfit(freq=str2num(wavenumber[k]))
		EndFor
		
		Display/N=PlotFit
		for(k=0;k<lengthW;k+=1)
			PlotFitSub("amp",str2num(wavenumber[k]),0)
		EndFor
		for(k=0;k<lengthW;k+=1)
			PlotFitSub("freq",str2num(wavenumber[k]),1)
		EndFor
	EndIf
End

Function tabFit([freq])
//Displays the results of peak fitting
//The user can chose whether they want to plot the amplitude, the frequency, the width or the area
	Variable freq
	WAVE timepoints = root:timepoints
	If(!WaveExists(timepoints))
		Print "Timepoints WAVE does not exist"
		return -1
	EndIf
	
	if(ParamIsDefault(freq))
	//If the user doesn't specify a frequency ask them for one
		freq=0
		Prompt freq,"Mode frequency:  "
		DoPrompt "Input the frequency of the mode you'd like to be tabulated",freq
		if( V_Flag )
			Print "Cancelled"
			return 0	// user canceled
		EndIf
	EndIf
	String sFreq=num2str(freq)
	If(!WaveExists($("fitamp_"+sFreq)))
		Print "Error the waves don't exist"
		return -1
	EndIf
	Edit/N=TabFit timepoints,$("fitamp_"+sFreq) as ("Mode " + sFreq + " Tabulated Data")
	AppendToTable $("fitamperror_"+sFreq),$("fitfreq_"+sFreq),$("fitfreqerror_"+sFreq)
	AppendToTable $("fitwidth_"+sFreq),$("fitwidtherror_"+sFreq),$("fitarea_"+sFreq)
	AppendToTable $("fitareaerror_"+sFreq)
	ModifyTable style(Timepoints)=1,alignment(Timepoints)=1,format(Timepoints)=2
End

Function PlotFit()
//Displays the results of peak fitting
//The user can chose whether they want to plot the amplitude, the frequency, the width or the area
	Variable freq=0
	Variable choice=1
	Variable plotAppend=1
	Prompt freq,"Mode frequency:  "
	Prompt choice,"I want to plot the: ",popup,"Amplitude;Frequency;Width;Area"
	Prompt plotAppend,"Do you want to append the plot to the top graph? ",popup,"No;Yes"
	DoPrompt "Input the frequency of the mode you'd like to be plotted",freq,choice,plotAppend
	if( V_Flag )
		Print "Cancelled"
		return 0	// user canceled
	EndIf
	String sFreq
	switch(choice)
		case 1:
			sFreq="amp"
			break
		case 2:
			sFreq="freq"
			break
		case 3:
			sFreq="width"
			break
		case 4:
			sFreq="area"
			break
		default:
			Print "Error in Switch statement"
			Return 0
	endswitch
	PlotFitSub(sFreq,freq,plotAppend)
	Print "PlotFitSub(\""+sFreq+"\","+num2str(freq)+","+num2str(plotAppend)+")"
End

//Helper function for the above plotFit() but can also be used standalone through the command line
Function PlotFitSub(type,freq,plotAppend)
	String type
	Variable freq, plotAppend
	
	WAVE timepoints = root:timepoints
	If(!WaveExists(timepoints))
		Print "Timepoints WAVE does not exist"
		return -1
	EndIf
	
	String toPlot="fit"+type+"_"+num2str(freq)
	String error="fit"+type+"error_"+num2str(freq)
	if(!WaveExists($toPlot)||!WaveExists($error))
		Print toPlot+" doesn't exist"
		Return 0
	EndIf
	If(plotAppend==1)
		display/N=PlotFit $toPlot vs timepoints as ("Mode "+num2str(freq)+" "+type)
	Else
		AppendToGraph $toPlot vs timepoints
	EndIf
	//ErrorBars $toPlot Y,WAVE=($error,$error)
	//ModifyGraph mode($toPlot)=3,marker($toPlot)=8
	ModifyGraph mirror=2
	ModifyGraph prescaleExp(bottom)=-3
	Label bottom "Delay (ps)"
	SetGraphSizeACS()
	ModifyGraph minor=1
	strswitch(type)
		case "amp":
			Label left "Amplitude (a.u.)"
			break
		case "area":
			Label left "Area (a.u.)"
			break
		case "Width":
			Label left "Width (cm\S-1\M)"
			break
		case "freq":
			Label left "Frequency (cm\S-1\M)"
			ModifyGraph tkLblRot(left)=90
			break
	endswitch
End

Function scaleGround(s) : FitFunc
	//A simple structure fit function that takes a spectrum
	//shifts it and scales it to fit the data
	//useful for removing the solvent artifact
	Struct scaledGroundStruct &s
	
	Return s.coefw[0]*interp(s.x,s.shift,s.gnd)//Interp is necessary only for the automatically generated fit WAVE
End

Structure scaledGroundStruct
	//the structure associated with the above
	//structure fit function
	WAVE coefw
	Variable x
	WAVE gnd
	WAVE shift
EndStructure