//--------------------(DE)CONVOLUTION THROUGH FFT-------------------------------------------------


// - Ken Burke, June/July 2015
// 	--- Code adapted/expanded from Pernia-Andrade et al., 2012 (Biophysical Journal, PI = Peter Jonas)
//
// These functions use deconvolution via division in frequency domain for spontaneous event 
// detection (e.g. miniature EPSC's). Deconvolution improves signal-to-noise and temporal precision 
// of detection of these events significantly over amplitude/first derivative measurements and template-matching algorithms.
//
// Main free parameters (in approximate order of importance) are:
//
//			a) event template function to be used for deconv. kernel 
//					-- (suggested sum of exponentials for EPSCs, or average of several hand-picked events)
//			b) signal-to-noise detection threshold for deconvolved trace,
//					-- This parameter affects the tradeoff between false positives and false negatives, 
//					   critical for mini frequency/amplitude analysis
//			c) low-pass filter for deconvolved trace 
//					-- Relatively unnecessary to adjust, as long as it's strong enough attenuation
//


//				***GUI THINGS***

Function Start_Event_Detection()
	Make_Event_Selection_Window()
	Make_mEPSC_Analysis_Window()
End

// *******EVENT DETECTION WINDOW GUI

Function Make_mEPSC_Analysis_Window()
	SVAR Expt=Expt
	SVAR ydataname = ydataname
	NVAR gRadioVal = gRadioVal
	NVAR mRadioVal = mRadioVal
	
	DoWindow/K mEPSCCont
	NewPanel/W=(1030,170,1430,575) as "mEPSC Analysis Window"
	DoWindow/C mEPSCCont					
	Button PrevWaveMbtn pos={10,10}, size={80,25}, title="Prev Sweep", proc = bMiniPrevWaveProc
	Button NextWaveMbtn pos={110,10}, size={80,25}, title="Next Sweep", proc = bMiniNextWaveProc	
	Button RunAllMinibtn pos={10,46}, size={80,20}, title="Run All", proc = bRunMiniProc
	Button RemoveSubplotbtn pos={95,46}, size={110,20}, title="Remove Subplot", proc = bRemoveSubplotProc

	CheckBox FxnKernelbx pos={225,23}, size={50,20}, title="Use Alpha Function Kernel", value = gRadioVal==1, mode=1, proc = bWhichKernelProc
	CheckBox EmpKernelbx pos={225,46}, size={50,20}, title="Use Empirical Kernel", value= gRadioVal==2, mode=1, proc = bWhichKernelProc

	DrawLine 199, 69, 199, 390
	DrawLine 201, 69, 201, 390
	
	DrawLine 5, 72, 395, 72
	DrawLine 5, 70, 395, 70
	
	SetDrawEnv fsize = 12, fstyle = 2
	Drawtext 5,95, "Event Kernel (Alpha)"
	SetVariable Kern_Amp, size = {130, 20}, pos = {35, 103}, value = Kernel_Amp, proc = bMakeEKerProc, Title = "Event Amplitude"
	SetVariable Kern_TauF, size = {130, 20}, pos = {35, 123}, value = tau_one, proc = bMakeEKerProc, Title = "           Rise Tau"
	SetVariable Kern_TauS, size = {130, 20}, pos = {35, 143}, value = tau_two, proc = bMakeEKerProc, Title = "        Decay Tau"
	Drawtext 170,117, "pA"
	Drawtext 170,138, "ms"
	Drawtext 170,159, "ms"
	
	SetDrawEnv fsize = 12, fstyle = 2
	Drawtext 205,95, "Event Kernel (Empirical)"
	SetDrawEnv fsize = 10, fstyle = 2
	Drawtext 205,105, "**uses checked events"
	Button MakeEmpKerBtn, pos={344,82}, size={45,20}, title="Make", proc = bMakeEmpKerProc
	SetVariable empKerDur, size = {95, 20}, pos = {260, 115}, value = empKerDur, Title = "Duration "
	SetVariable empKerSmoothing, size = {95, 20}, pos = {260, 135}, value = empKerSmooth, Title = "Smoothing "
	SetDrawEnv fsize = 12
	Drawtext 367,130, "ms"
	SetDrawEnv fsize = 12
	Drawtext 361,150, "samp"
	
	DrawLine 5, 168, 395, 168
	DrawLine 5, 170, 395, 170

	SetDrawEnv fsize = 12, fstyle = 2
	Drawtext 5,192, "Deconvolution"
	Button RunDeconvbtn pos={148,175}, size={40,20}, title="Go", proc = bDECProc
	SetVariable Deconv_target_wn, size = {165, 20}, pos = {25, 200}, value = Deconv_Target_Wavename, Title = "Input Wavename"
	SetVariable Deconv_output_wn, size = {165, 20}, pos = {25, 220}, value = Deconv_Output_Wavename, Title = "Output Wavename"   
	DrawLine 5, 244, 395, 244
	DrawLine 5, 246, 395, 246
	
	CheckBox EventMarkersbx pos={210,185}, size={50,20}, title="Event Markers", value = 1, variable = events_on, proc = bEventPeakMarkersProc, mode=0
	CheckBox PeakMarkersbx pos={210,208}, size={50,20}, title="Peak Markers", value= 0, variable = peaks_on, proc = bEventPeakMarkersProc, mode=0	
	CheckBox findMinbx pos={303,185}, size={50,20}, title="Find Minimum", value = mRadioVal==1, mode=1, proc = bMaxOrMinProc
	CheckBox findMaxbx pos={303,208}, size={50,20}, title="Find Maximum", value= mRadioVal==2, mode=1, proc = bMaxOrMinProc
	CheckBox blineSubbx pos={240,226}, size={50,20}, title="Trendline Correction", value= 0, variable = blineSubOn, proc = bBlineSubProc, mode=0	

	SetDrawEnv fsize = 12, fstyle = 2
	Drawtext 5,268, "Detect Events"
	Button RunDetEvbtn pos={158,251}, size={30,20}, title="Go", proc = bDetEvProc
	Button KillMarkersbtn pos={85,251}, size={69,20}, title="Kill Marks", proc = bKillMarksProc
	SetVariable Event_Maximum, size = {137, 20}, pos = {25, 276}, value = Event_Max, Title = "Max, Num. Events "
	SetVariable Event_thresh, size = {137, 20}, pos = {25, 296}, value = Event_Threshold, Title = " Detection Thresh.  "   
	SetVariable Event_spacing, size = {152, 20}, pos = {10, 316}, value = minimum_time_spacing, Title = " Min. Inter-Event Interval"   
	SetVariable Peak_Smoothing, size = {137, 20}, pos = {25, 336}, value = peak_smoothing, Title = " Smoothing Width    "   
	SetVariable Hard_Minimum_Amplitude, size = {137, 20}, pos = {25, 356}, value = hard_min_amp, Title = " Hard Minimum Amp"   
	Drawtext 170,290, "#"
	Drawtext 170,311, "S.D."
	Drawtext 170,332, "ms"
	Drawtext 166,353, "samp"
	Drawtext 166,374, "| pA |"
	SetDrawEnv fsize = 9, fstyle = 2
	Drawtext 206, 372, "<< WARNING, hard minimum overrides manual"
	SetDrawEnv fsize = 9, fstyle = 2
	Drawtext 206, 382, "selections in Event Selection Window"
	
	
	SetDrawEnv fsize = 12, fstyle = 2
	Drawtext 205,268, "Search Range In Sweep"
	SetVariable Detection_Start, size = {125, 20}, pos = {240, 276}, value = detection_start, Title = "Detection Start ", proc = bChangeDetectTime
	SetVariable Detection_End, size = {125, 20}, pos = {240, 296}, value = detection_end, Title = "Detection End ", proc = bChangeDetectTime
	SetDrawEnv fsize = 10
	Drawtext 370,293, "sec"
	SetDrawEnv fsize = 10
	Drawtext 370,313, "sec"

	DrawLine 5, 385, 395, 385
	DrawLine 5, 383, 395, 383
	
	Make_EPSC_Kernel()
	
end

Function bMiniPrevWaveProc(ctrlName)
	string ctrlName
	
	SVAR Expt = Expt
	Find_Previous_Sweep(Expt)
	Read_Sweep(Expt)
	
	bRemoveSubplotProc(ctrlName)
	bRunMiniProc(ctrlName)

end

Function bMiniNextWaveProc(ctrlName)
	string ctrlName
	
	SVAR Expt = Expt
	Find_Next_Sweep(Expt)
	Read_Sweep(Expt)
	
	bRemoveSubplotProc(ctrlName)
	bRunMiniProc(ctrlName)

end

Function bRunMiniProc(ctrlName)
	string ctrlName
	String/G Deconv_Target_Wavename = Deconv_Target_Wavename
	String/G Deconv_Output_Wavename = Deconv_Output_Wavename
	String Output_Wavename = "s_"+Deconv_Output_Wavename			// s_ prefix so you can find subplot waves with WaveList
	NVAR disk_sweep_no = disk_sweep_no
	NVAR blineSubOn = blineSubOn
	
	print "---------------------------Sweep "+num2str(disk_sweep_no)+"---------------------------"
	strswitch(ctrlName)
		case "PrevWaveMbtn":
			break
		case "NextWaveMbtn":
			break
		case "RunAllMinibtn":
			break
		default:
			Make_EPSC_Kernel()
			break
	endswitch
	
	FFT_Deconv($Deconv_Target_Wavename, EPSC_kernel, Output_Wavename)
	
	if (blineSubOn == 1)
		duplicate/O $Output_Wavename outputwave_subbed
		Wave W_coef = W_coef
		CurveFit/Q/NTHR=0/TBOX=0 line  outputwave_subbed /D
		outputwave_subbed -= W_coef[0] + W_coef[1]*x
		duplicate/O outputwave_subbed $Output_Wavename
		killwaves outputwave_subbed
	endif
	
	Detect_Peaks($Output_Wavename)
	print "---------------------------------------------------------------------"
	print "\n"

end

Function bRemoveSubplotProc(ctrlName)
	string ctrlName
	
	Variable killMarks = Remove_Subplot("Sweep_window", "all_waves")
	
	if (killMarks==1)
		bKillMarksProc(ctrlName)
	endif
	
end

Function bKillMarksProc(ctrlName)
	string ctrlName
	
	RemoveFromGraph/W=Sweep_window marker_placeholder
end

Function bDECProc(ctrlName)
	string ctrlName
	String/G Deconv_Target_Wavename = Deconv_Target_Wavename
	String/G Deconv_Output_Wavename = Deconv_Output_wavename
	String Output_Wavename = "s_"+Deconv_Output_Wavename			// s_ prefix so you can find subplot waves with WaveList
	
	FFT_Deconv($Deconv_Target_Wavename, EPSC_kernel, Output_Wavename)
	
end

Function bDetEvProc(ctrlName)
	string ctrlName
	String/G Deconv_Output_Wavename = Deconv_Output_wavename
	String Output_Wavename = "s_"+Deconv_Output_Wavename			// s_ prefix so you can find subplot waves with WaveList
	
	Detect_Peaks($Output_Wavename)
	
end

Function bEventPeakMarkersProc(ctrlName,checked) : CheckBoxControl
	String ctrlName
	Variable checked		// 1 if selected, 0 if not
	
	Display_Event_Detections()
	
end

Function bBlineSubProc(ctrlName,checked) : CheckBoxControl
	String ctrlName
	Variable checked
	
	NVAR blineSubOn = blineSubOn
	
	if (checked==0)
		CheckBox blineSubbx, value=0, win=mEPSCCont
	else
		CheckBox blineSubbx, value=1, win=mEPSCCont
	endif
	
	printf num2str(blineSubOn)
		
end

Function bWhichKernelProc(name,value)
	String name
	Variable value
	
	NVAR gRadioVal= gRadioVal
	
	strswitch (name)
		case "FxnKernelbx":
			gRadioVal= 1
			break
		case "EmpKernelbx":
			gRadioVal = 2
			break
	endswitch
	
	CheckBox FxnKernelbx,value= gRadioVal==1, win=mEPSCCont
	CheckBox EmpKernelbx,value= gRadioVal==2, win=mEPSCCont
	
	Make_EPSC_Kernel()
	
End

Function bMakeEmpKerProc(ctrlName)
	string ctrlName
	
	NVAR gRadioVal = gRadioVal
	
	gRadioVal = 2
	
	CheckBox FxnKernelbx,value= gRadioVal==1, win=mEPSCCont
	CheckBox EmpKernelbx,value= gRadioVal==2, win=mEPSCCont
	
	Make_EPSC_Kernel()
end	


Function bMaxOrMinProc(name,value)
	String name
	Variable value
	
	NVAR mRadioVal= mRadioVal
	NVAR Kernel_Amp = Kernel_Amp
	
	strswitch (name)
		case "findMinbx":
			mRadioVal= 1
			Kernel_Amp = abs(Kernel_Amp)*-1			// sets kernel to be negative
			break
		case "findMaxbx":
			mRadioVal= 2
			Kernel_Amp = abs(Kernel_Amp)			// sets kernel to be positive
			break
	endswitch
	
	CheckBox findMinbx,value= mRadioVal==1, win=mEPSCCont
	CheckBox findMaxbx,value= mRadioVal==2, win=mEPSCCont
	
	Make_EPSC_Kernel()
	
End

Function bChangeDetectTime (ctrlName,varNum,varStr,varName) : SetVariableControl
	String ctrlName
	Variable varNum	// value of variable as number
	String varStr		// value of variable as string
	String varName	// name of variable
	
	bRunMiniProc(ctrlName)
	
End



Function Subplot(window_name, subplot_trace, focus_trace,range_min, range_max,axis_label)
	//assumes they have the same x_range
	
	string window_name		//name of target window
	wave subplot_trace		//trace to be added
	wave focus_trace			//trace on sweep_window already to focus axes on
	variable range_min
	variable range_max
	string axis_label
	
	AppendToGraph/W=$window_name/L=subplot subplot_trace
	ModifyGraph/W=$window_name axisEnab(subplot)={0,0.48}
	ModifyGraph/W=$window_name axisEnab(left)={0.52,1}	
	Wavestats/Q focus_trace
	SetAxis/W=$window_name left (V_min-5), (V_max+5)
	SetAxis/W=$window_name subplot range_min,range_max
	Label/W=$window_name subplot axis_label
	ModifyGraph/W=$window_name lblPos(subplot)=40
	ModifyGraph/W=$window_name freePos(subplot)=0
end

Function Remove_Subplot(window_name, subplot_trace)
	//removes subplots created above
	
	string window_name
	string subplot_trace
	
	if (~cmpstr(subplot_trace,"all_waves"))				//keyword to kill all waves
		String subplot_list = WaveList("s_*", ";", "WIN:"+window_name)
		
		if (cmpstr(subplot_list,"")==0)			//if there's nothing with that prefix, then nothing to remove
			return 0
		endif
		
		String theWave
		Variable i=0
		do								// Remove all waves found in wavelist
			theWave = StringFromList(i, subplot_list)
			if (strlen(theWave) == 0)
				break // Ran out of waves
			endif
			RemoveFromGraph/W=$window_name $theWave
			i += 1
		while (1) // Loop until break above
	else
		RemoveFromGraph/W=$window_name $subplot_trace
	endif
	
	ModifyGraph/W=$window_name axisEnab(left)={0,1}
end

// ********* EVENT SELECTION GUI


Function Make_Event_Selection_Window()

	Wave peakPositionsX = peakPositionsX
	Wave peakPositionsY = peakPositionsY
	
	Make/O/B/N=(numpnts(text_PPX),2) sel_events = 0
	sel_events[][0] += 2^4+2^5 // turn on bit 4 and 5 to have checkboxes

	DoWindow/K EventCont
	newpanel/w=(480,780,830,990) as "Event Selection Window"
	DoWindow/C EventCont
	
	SetDrawEnv fsize = 12, fstyle = 2
	Drawtext 25,18, "List of Detected Events"
//	SetDimLabel 1, , Time_Sec, text_PPX
	ListBox Event_List win=EventCont, pos={10,22}, size={165,175}, mode=4, listWave=text_events, selWave = sel_events, proc = bEvent_Listbox_Proc
	SetDrawEnv fsize = 10, fstyle = 2
	Drawtext 18,210, "time (sec)        amp (pA)"

	Button CalculateAmpsbtn pos={190,7}, size={140,20}, title="Save Amps and IEIs", proc = bCalcAmpsProc
	SetVariable Bline_Range, size = {115, 20}, pos = {192, 35}, value = bline_range, Title = "Baseline Range "
	SetVariable Peak_Range, size = {115, 20}, pos = {192, 55}, value = peak_range, Title = "Peak Range "
	SetVariable Peak_Offset, size = {115, 20}, pos = {192, 75}, value = peak_offset, Title = "Peak Offset "
	SetDrawEnv fsize = 12
	Drawtext 315,50, "ms"
	SetDrawEnv fsize = 12
	Drawtext 315,70, "ms"
	SetDrawEnv fsize = 12
	Drawtext 315,90, "ms"
	DrawLine 185, 100, 342, 100
	DrawLine 185, 102, 342, 102
	
	
	Button ConcatenateAmpbtn pos={180,118}, size={160,20}, title="Concat. Summary Waves", proc = bConcatenateAllProc
	Button PlotEventsbtn pos={180,148}, size={160,20}, title="Plot Events & Average", proc = bStoreEventTemplatesProc
	Button ClearAmpIEIWavesbtn pos={180,178}, size={160,20}, title="Reset Summary Waves", proc = bClearAmpIEIProc

end

Function bEvent_Listbox_Proc(LB_Struct) : ListboxControl
	STRUCT WMListboxAction &LB_Struct
	
	if (LB_Struct.eventCode == 2)
		Display_Event_Detections()
	endif
	
end

Function bConcatenateAllProc(ctrlName)

	string ctrlName
	
	bConcatAmpsProc("")
	bConcatIEIProc("")
	bConcatTimesProc("")

end

Function bConcatAmpsProc(ctrlName)

	string ctrlName
	String AllAmpsList
	
	AllAmpsList = WaveList("s*_amps", ";", "")
	
	Variable numList = ItemsInList(AllAmpsList)
	
	if(numList == 0)
		printf "\r No waves with amplitudes found. Quitting.\r"
		return 0
	endif
	
	Concatenate/O AllAmpsList, allAmplitudes
	printf("\rAll Amplitudes:\r\r")
	print allAmplitudes
	printf("\rWavestats on All Amplitudes\r\r")
	wavestats allAmplitudes
	
End

Function bConcatIEIProc(ctrlName)

	string ctrlName
	String AllEventIntervalList
	
	AllEventIntervalList = WaveList("s*_iei", ";", "")
	
	Variable numList = ItemsInList(AllEventIntervalList)
	
	if(numList == 0)
		printf "\r No waves with IEI found. Quitting.\r"
		return 0
	endif
	
	Concatenate/O AllEventIntervalList, allEventInts
	printf("\rAll Event Intervals:\r\r")
	print allEventInts
	printf("\rWavestats on All Event Intervals\r\r")
	wavestats allEventInts
	
End

Function bConcatTimesProc(ctrlName)

	string ctrlName
	String AllTimesList
	
	AllTimesList = WaveList("s*_times", ";", "")
	
	Variable numList = ItemsInList(AllTimesList)
	
	if(numList == 0)
		printf "\r No waves with Times found. Quitting.\r"
		return 0
	endif
	
	Concatenate/O AllTimesList, allEventTimes
	printf("\rAll Event Times:\r\r")
	print allEventTimes
	printf("\rWavestats on All Event Times\r\r")
	wavestats allEventTimes
	
End

Function bClearAmpIEIProc(ctrlName)

	string ctrlName
	string allAmpsList
	String AllEventIntervalList
	String AllTimesList
	
	AllEventIntervalList = WaveList("s*_iei", ";", "")
	AllAmpsList = WaveList("s*_amps", ";", "")
	AllTimesList = WaveList("s*_times", ";", "")

	printf "\r: killing amp, timing, and iei waves (s*_amps, s*_times and s*_iei)\r"
 	Variable numList = ItemsInList(AllEventIntervalList)
 	Variable index
 	
 	if (numList != 0)
 		for (index=0;index<numList;index+=1)
 			KillWaves/Z $(StringFromList(index, AllEventIntervalList))
 		endfor
 	endif
 	
 	numList = ItemsInList(AllAmpsList)
 	
 	if (numList != 0)
 		for (index=0;index<numList;index+=1)
 			KillWaves/Z $(StringFromList(index, AllAmpsList))
 		endfor
 	endif
 	
  	numList = ItemsInList(AllTimesList)
 	
 	if (numList != 0)
 		for (index=0;index<numList;index+=1)
 			KillWaves/Z $(StringFromList(index, AllTimesList))
 		endfor
 	endif

 	KillWaves/Z allEventInts, allAmplitudes, allTimes, setOfEvents; AbortOnRTE
 	
end

Function bMakeEKerProc(ctrlName,varNum,varStr,varName) : SetVariableControl
	String ctrlName
	Variable varNum	// value of variable as number
	String varStr		// value of variable as string
	String varName	// name of variable
	
	Make_EPSC_Kernel()
end

//				***END GUI THINGS***

//				***MINI ANALYSIS FUNCS***



Function Make_EPSC_Kernel()

	NVAR gRadioVal = gRadioVal
	
	switch (gRadioVal)
		case 1:
			bFxnKernelProc("")
			break
		case 2:
			bEmpiricalKernelProc("")
			break
	endswitch
	
end

Function bFxnKernelProc(ctrlName)

	string ctrlName	
	Variable/G tau_one = tau_one				// in ms
	Variable/G tau_two = tau_two
	Variable/G Kernel_Amp = Kernel_Amp		// in pA
	
	Variable/G kHz = kHz
	
	DoWindow/K Kernel_Window
	KillWaves/Z EPSC_Kernel, time_index 		// cleanup
	
	Variable tau_fast = tau_one*kHz
	Variable tau_slow =tau_two*kHz 			//scale to sampling frequency
	
	Variable kernel_window = 4*tau_slow
	Variable amp_prime = (tau_slow/tau_fast)^(tau_fast/(tau_fast-tau_slow))		// normalization factor
	
	make/O/N=(kernel_window) time_index=p
	SetScale/P x, 0, 1/(kHz), time_index			// normalize to units of ms
	make/O/N=(kernel_window) EPSC_kernel = (Kernel_Amp/amp_prime)*(-exp(-time_index/(tau_fast))+exp(-time_index/(tau_slow))		// sum of exponentials
	SetScale/P x, 0, 1/(kHz), EPSC_kernel		//normalize to units of s
	
	display/W=(0,125,345,285)/N=Kernel_Window EPSC_Kernel
	textbox/W=Kernel_Window/A=MT/X=-3/F=0/E "EPSC Kernel / Rise = "+num2str(tau_fast/kHz)+" ms / Decay = "+num2str(tau_slow/kHz)+" ms"		
	Label left "pA"
	Label bottom "ms"
	
end

Function bEmpiricalKernelProc(ctrlName)			//makes an kernel with detected events (rather than "idealized" kernel)
	string ctrlName
	wave sel_events = sel_events
	wave display_wave1 = display_wave1
	wave peakPositionsX = peakPositionsX
	NVAR kHz = kHz
	NVAR empKerDur = empKerDur
	NVAR empKerSmooth = empKerSmooth
	
	
	// normalize all events, average them, and fit a sum of exponentials with 3 parameters
	// or, perhaps just average and smooth?
	
	Make/O/N=(empKerDur*kHz) empiricalKernel

	variable i = 0		//iterator
	variable n = 0		//num counted events

	do
		if ((sel_events[i] & 2^4) != 0)			// if checked in manual selection window
			Make/O/N=(empKerDur*kHz) singleEvent = display_wave1[p+peakPositionsX(i)*kHz*1e3]
			empiricalKernel += singleEvent
			n += 1
		endif
		i +=1
	while (i < numpnts(peakPositionsX))
	
	if (n != 0)
		empiricalKernel /= n
		Smooth/B=(empKerSmooth) 4, empiricalKernel
		Variable baseSub = empiricalKernel[0]
		empiricalKernel -= baseSub
	else
		printf " : no checked events, empirical kernel not created. quitting."
		return 0
	endif
	
	SetScale/P x, 0, 1/(kHz), empiricalKernel			// normalize to units of ms
	
	duplicate/O empiricalKernel, EPSC_Kernel
	
	DoWindow/K Kernel_Window
	KillWaves/Z empiricalKernel, time_index
	
	display/W=(0,125,345,285)/N=Kernel_Window EPSC_Kernel
	textbox/W=Kernel_Window/A=MT/X=-3/F=0/E "Empirical Kernel"		
	Label left "pA"
	Label bottom "ms"
	
end



Function FFT_Deconv(OutputSignal, Kernel, DestName)

	//assumes no x-scaling on outputsignal or kernel (i.e. x-scaling is 0 to numpnts(wave)-1)
	// WARNING!!! SCALING OVERESTIMATES THE VARIANCE OF DECONVOLVED INPUT SIGNAL
	//		For better estimate fit the all-point histogram to a gaussian and take the SD (see "detect_peaks()")
	
	
	Wave OutputSignal
	Wave Kernel
	String DestName	
	NVAR kHz = kHz
	Variable Fs = kHz*1e3
	String/G Deconv_Output_Wavename = Deconv_Output_wavename
	String Output_Wavename = "s_"+Deconv_Output_Wavename				// s_ prefix so you can find subplot waves with WaveList

	if (exists("Saved_Deconv_Wavename")==2)								// if we've already got a wavename saved, remove it from the subplot
		String/G Saved_Deconv_Wavename = Saved_Deconv_Wavename	
		Remove_subplot("Sweep_window",Saved_Deconv_Wavename)
	elseif (exists("Saved_Deconv_Wavename")==0)
		String/G Saved_Deconv_Wavename = ""								// otherwise if it doesn't exist, put in placeholder and do nothing
	endif
	
	Saved_Deconv_Wavename = Output_Wavename							// reset the stored value
	
	Variable Lx = numpnts(OutputSignal) - numpnts(Kernel) + 1				// anticipated length of input ("DestName") in number of points

	SetScale x, 0, (NumPnts(OutputSignal)-1)/Fs, OutputSignal				// must scale signals for appropriate frequency decomposition

	FFT/OUT=1/DEST=Output_FFT OutputSignal							// move input and kernel to frequency domain
	FFT/OUT=1/PAD={NumPnts(OutputSignal)}/DEST=Kernel_FFT Kernel 		// pad the kernel so they're of equal lengths

	Make/N=(numpnts(Output_FFT))/D/C DeconvFFT							// Length of FFTs above^^ = NumPnts(OutputSignal)/2 +1, must be same here
	DeconvFFT=Output_FFT/Kernel_FFT

	IFFT/DEST=Deconv_raw DeconvFFT											// inverse fourier, convert back from frequency to time

	FilterFIR/LO={0.012, 0.013, 101} Deconv_raw								// uninterpretable without low-pass filter
	WaveStats/Q Deconv_raw
	Make/O/N=(numpnts(Deconv_raw)) $DestName = (Deconv_raw-V_avg)/V_sdev	// normalize to units of standard deviation, to use SD of noise as threshold for detection
	SetScale x, 0, (NumPnts(Deconv_raw)-1)/Fs, $DestName
	
	KillWaves Deconv_raw, DeconvFFT, Output_FFT, Kernel_FFT							// cleanup
	Subplot("Sweep_window",$DestName,OutputSignal,-1,7,"Event Amp (arb. units)")		// display on subplot of sweep window
	ModifyGraph/W=Sweep_window rgb($DestName) = (0,9472,39168)
	
end

Function FFT_Conv(InputSignal, Kernel, Fs, DestName)

	//assumes no x-scaling on inputsignal or kernel (i.e. x-scaling is 0 to numpnts(wave)-1)

	Wave InputSignal
	Wave Kernel
	Variable Fs
	String DestName

	Variable Lx = numpnts(InputSignal) + numpnts(Kernel) - 1				// anticipated length of output in number of points

	SetScale x, 0, (NumPnts(InputSignal)-1)/Fs, InputSignal				// must scale signals for appropriate frequency decomposition
	SetScale x, 0, (NumPnts(Kernel)-1)/Fs, Kernel

	FFT/OUT=1/DEST=Input_FFT InputSignal							// move input and kernel to frequency domain
	FFT/OUT=1/PAD={NumPnts(InputSignal)}/DEST=Kernel_FFT Kernel	// pad the kernel so they're of equal lengths

	Make/N=(numpnts(Input_FFT))/D/C ConvFFT							// Length of FFTs above^^ = NumPnts(InputSignal)/2 +1, must be same here
	ConvFFT=Input_FFT*Kernel_FFT									// Convolution is multiplication in the frequency domain

	IFFT/DEST=$DestName ConvFFT									// Inverse FFT and scale back to real time 
	SetScale x, 0, (Lx-1)/Fs, $DestName
	
	KillWaves ConvFFT, Input_FFT, Kernel_FFT							// cleanup and display
	display $DestName
	
end


Function Detect_Peaks(trace)

	wave trace									// trace data 
	Variable/G Event_Max = Event_Max			// cap on number of peaks
	Variable/G Event_Threshold = Event_Threshold	// threshold in units of S.D. from mean of gaussian fit to all-point histogram of data
	Variable min_spacing							// minimum time between detected events in ms
	
	Wave W_coef = W_coef
	Variable peaksFound=0
	NVAR kHz = kHz
	NVAR detection_start = detection_start	
	NVAR detection_end = detection_end
	NVAR peak_smoothing = peak_smoothing
	Variable/G minimum_time_spacing = minimum_time_spacing
	Variable startP=detection_start*kHz*1e3				//startP is sliding range that help to find maxima one-by-one
	Variable endP= detection_end*kHz*1e3 - 1					//endP is end of region for detection

	
	String graphname = "AllPointDistribution"

	DoWindow/K $graphname											//cleanup
	Killwaves/Z y_placeholder, fit_trace_histogram, trace_histogram, threshold_height, adjusted_thresh_wave, peakPositionsX, peakPositionsY
	
	// First, find the standard deviation of the noise in your trace 
	Make/N=1 trace_histogram
	histogram/B={-10,0.1,270} trace, trace_histogram			// all-point hist, binned from -10 to 16.9 in 0.1 bins (more not needed, as this is just for the gauss fit)
	
		
	// Then fix the offset to zero, set a couple other parameter guesses, fit the data to a gaussian and adjust
	K0 = 0
	K1 = 5000 
	K2 = -0.1
	K3 = 1
	
	CurveFit/Q/G/H="1000" gauss trace_histogram /D				// fit wave is fit_trace_histogram
	
	Variable noise_sd = abs(W_coef[3])							// W_coeff is wave of parameters for fit, 3 is SD (2 is mean)
	Variable noise_mean = W_coef[2]
	Variable adjusted_threshold = noise_mean + noise_sd*Event_Threshold	// convert thresh from SD units to arbitrary input units
	
	print "\n"
	print "\t-Mean of All-Point Histogram Gaussian Fit = "+num2str(noise_mean)
	print "\t-Standard Deviation of All-Point Histogram Gaussian Fit = "+num2str(noise_sd)
	print "\t\t---Adj. Thresh. (Arb. Units) = "+num2str(adjusted_threshold)
	print "\tPLOTTING ALL-POINT DISTRIBUTION...."
	
	display/W=(0,312,345,552)/N=$graphname trace_histogram
	ModifyGraph/W=$graphname rgb(trace_histogram)=(0,9472,39168)
	AppendToGraph/W=$graphname/C=(52224,0,0) fit_trace_histogram
	
	// NOW, go through and find all of the peaks that pass your threshold
	Make/O/N=(Event_Max) peakPositionsX= NaN, peakPositionsY= NaN    	// empty vectors to store data
	
	do
	    FindPeak/B=(peak_smoothing)/I/M=(adjusted_threshold)/P/Q/R=[startP, endP] trace		// find a peak in given range
	    
	    // FindPeak outputs are V_Flag, V_PeakLoc, V_LeadingEdgeLoc,
	    // V_TrailingEdgeLoc, V_PeakVal, and V_PeakWidth. 
	    
	    if( V_Flag != 0 )			// if you didn't find another peak
	        break
	    elseif( numtype(V_TrailingEdgeLoc) == 2 )			// if the max is at the end (trailingEdge is NaN)
	        break
	    endif
	    
	    peakPositionsX[peaksFound]=pnt2x(trace,V_PeakLoc)					// fill in time for this peak
	    peakPositionsY[peaksFound]=V_PeakVal								// fill in value for the peak
	    peaksFound += 1													// counter
	    
	    startP= V_TrailingEdgeLoc+minimum_time_spacing*kHz				// move start time for range of next peak 
	    																	// detection to after current peak
	while ( peaksFound < Event_Max )					//caps out max number of peaks
	
      Extract/O peakPositionsX, peakPositionsX, (numtype(peakPositionsX) != 2)			//kill all the NaNs
      Extract/O peakPositionsY, peakPositionsY, (numtype(peakPositionsY) != 2)

	print "\t-Peaks Detected = "+num2str(peaksFound)

	Make/N=(numpnts(trace_histogram)) y_placeholder=200							
	AppendToGraph/W=$graphname/C=(0,0,0) y_placeholder vs peakPositionsY			// to plot points of detected single events
	ModifyGraph/W=$graphname mode(y_placeholder)=3,marker(y_placeholder)=10
	
	// plot threshold line on histogram
	Make/N=1 threshold_height = 6000			
	Make/N=1 adjusted_thresh_wave = adjusted_threshold
	AppendToGraph/W=$graphname threshold_height vs adjusted_thresh_wave
	ModifyGraph/W=$graphname mode(threshold_height)=1, rgb(threshold_height)=(0,39168,19712)
	SetAxis/W=$graphname left 0, 210
	SetAxis/W=$graphname bottom -2,10
	textbox/W=$graphname/A=MT/X=1/F=0/E "Detected Peaks, Thresh. = "+num2str(Event_Threshold)+" S.D."
	Label left "Num. Points"
	Label bottom "Amplitude (Arbitrary Units)"
	
	
	//plot threshold onto deconvolved subplot, and range over which we're detecting peaks
	if (~cmpstr(Wavelist("s_thresh*",":","WIN:Sweep_window"), "s_threshold*"))
		RemoveFromGraph/W=Sweep_window s_threshold					// s_ prefix so you can find subplot waves with WaveList
	endif
	Make/O/N=(numpnts(trace)) s_threshold=adjusted_threshold
	Make/O/N=2 rangeThresh = {detection_start,detection_end}
	AppendToGraph/W=Sweep_window/L=subplot s_threshold vs rangeThresh
	ModifyGraph/W=Sweep_window rgb(s_threshold)=(0,39168,19712)
			
	if (peaksFound == Event_Max)
		doAlert 0, "Maximum number of events reached!\nYou may have missed events! Raise cap!"
	endif
	
	Make/O/T/n=(numpnts(peakPositionsX)) text_PPX = num2str(peakPositionsX)
	Make/O/T/n=(numpnts(peakPositionsY)) text_PPY = num2str(peakPositionsY)
	Concatenate/O {text_PPX,text_PPY}, text_events	

	Make_Event_Selection_Window()
	bCalcAmpsProc("")
	Display_Event_Detections()

end

Function Display_Event_Detections()
	NVAR events_on = events_on			// 1 if displaying event markers, 0 if not
	NVAR peaks_on = peaks_on			// 1 if displaying peak markers, 0 if not
	Wave peakPositionsX = peakPositionsX
	Wave display_wave1 = display_wave1
	Wave disp_index = disp_index
	Wave sel_events = sel_events
	wave peak_locs
	NVAR mRadioVal = mRadioVal			// = 1 if searching for minimum, 2 if max
	NVAR disk_sweep_no = disk_sweep_no
	
	
	string name_of_wave = "s"+num2str(disk_sweep_no)+"_amps"
	Wave current_sweep_amps = $name_of_wave
	
	if (~cmpstr(Wavelist("marker_*",":","WIN:Sweep_window"), "marker_placeholder*"))
		RemoveFromGraph/W=Sweep_window marker_placeholder
	endif
	if (~cmpstr(Wavelist("amp_*",":","WIN:Sweep_window"), "amp_placeholder*"))
		RemoveFromGraph/W=Sweep_window amp_placeholder
	endif
	
	Make/O/N=(numpnts(peakPositionsX)) marker_placeholder=0
	Make/O/N=(numpnts(current_sweep_amps)) amp_placeholder=0

	variable i = 0
	variable n = 0
	do
		if ((sel_events[i] & 2^4) != 0)			// if checked in manual selection window
			marker_placeholder[i] = display_wave1(peakPositionsX[i])-21
		else									// if unchecked
			marker_placeholder[i] = 10000		// off the screen
		endif
		
		i +=1
	while (i < numpnts(peakPositionsX))
	
	i=0
	do
		amp_placeholder[i] = display_wave1(peak_locs[i])-2
		i+=1
	while (i<numpnts(current_sweep_amps))
	
	if(events_on == 1)
	AppendToGraph/W=Sweep_window/C=(0,0,0) marker_placeholder vs peakPositionsX
	ModifyGraph/W=Sweep_window mode(marker_placeholder)=3, marker(marker_placeholder)=17
	endif
	
	if(peaks_on == 1)
		if (mRadioVal == 2)		//searching for maximum
			amp_placeholder += 5
		endif
	AppendToGraph/W=Sweep_window/C=(100,100,0) amp_placeholder vs peak_locs
	ModifyGraph/W=Sweep_window mode(amp_placeholder)=3, mrkThick(amp_placeholder)=2,rgb(amp_placeholder)=(0,52224,0), marker(amp_placeholder)=10
	endif
	
end

Function bCalcAmpsProc(ctrlName)			// calculates amplitudes of selected events in pA
	
	string ctrlName
	wave sel_events = sel_events
	wave display_wave1 = display_wave1
	wave peakPositionsX = peakPositionsX
	wave/T text_events = text_events
	wave text_PPX = text_PPX
	wave text_PPY = text_PPY
	NVAR kHz = kHz
	NVAR disk_sweep_no = disk_sweep_no
	variable peak_smoothing = peak_smoothing
	wave EPSC_Kernel = EPSC_Kernel
	wave display_wave1 = display_wave1
	NVAR hard_min_amp = hard_min_amp
	NVAR peak_range = peak_range			// range over which to average peak, in ms
	NVAR peak_offset = peak_offset			// amount by which to offset suspected peak location, in ms
	NVAR bline_range = bline_range			// range over which to average bline, in ms
	NVAR mRadioVal = mRadioVal			// 1 if searching for a minimum, 2 if maximum
	
	Concatenate/O/T {text_PPX,text_PPY}, text_events	
	
	print "----------------------------------------------"
	print "Calculating Selected Amplitudes for Sweep "+num2str(disk_sweep_no)+"...."
	print "\n"
	
	string name_of_amps = "s"+num2str(disk_sweep_no)+"_amps"
	string name_of_iei = "s"+num2str(disk_sweep_no)+"_iei"
	string name_of_times = "s"+num2str(disk_sweep_no)+"_times"
	
	Make/O/N=0 temp_amps
	Make/O/N=0 temp_ieis
	Make/O/N=0 temp_times
	Make/O/N=0 peak_locs
	variable baseline = 0			
	variable peak = 0
	variable event_amp = 0
	variable peak_location = 0

	variable i=0
	
	// FIND Kernel Extremum (so you know what range to look for peak)
	switch (mRadioVal)
		case 1:	//find min
			FindPeak/B=(peak_smoothing)/I/N/Q/P EPSC_Kernel	
			break
		case 2:	//find max
			FindPeak/B=(peak_smoothing)/I/Q/P EPSC_Kernel
			break
	endswitch
	
	variable kernel_peak = V_PeakLoc
	
	// now get peaks
	variable suspected_peak_loc = 0
	
	do
		if ((sel_events[i] & 2^4) != 0)		// if checked in manual selection window
	
			wavestats/Q/R=[peakPositionsX[i]*kHz*1e3-bline_range*kHz,peakPositionsX[i]*kHz*1e3] display_wave1
			baseline = V_avg			//determine baseline and peak for event amp
			
			suspected_peak_loc = (peakPositionsX[i]+peak_offset/1e3)*kHz*1e3+kernel_peak		// in samples
			
			wavestats/Q/R=[suspected_peak_loc-peak_range*kHz,suspected_peak_loc+peak_range*kHz] display_wave1
			
			switch (mRadioVal)
				case 1:	//find min
					peak_location = V_minloc
					wavestats/Q/R=[V_minloc*kHz*1e3-peak_range*kHz/2,V_minloc*kHz*1e3+peak_range*kHz/2] display_wave1
					break
				case 2:	//find max
					peak_location = V_maxloc
					wavestats/Q/R=[V_maxloc*kHz*1e3-peak_range*kHz/2,V_maxloc*kHz*1e3+peak_range*kHz/2] display_wave1
					break
			endswitch

			
			peak = V_avg
			
			event_amp = peak-baseline
			
			switch (mRadioVal)
				case 1:	//find min
					if (event_amp > abs(hard_min_amp)*-1)	// if it's smaller than the minimum amplitude, or positive, uncheck
						sel_events[i] = sel_events[i] %^ 2^4
					else		// if it's big enough, record it
						insertpoints numpnts(temp_amps), 1, temp_amps
						temp_amps[numpnts(temp_amps)-1] = event_amp
						insertpoints numpnts(peak_locs), 1, peak_locs
						peak_locs[numpnts(peak_locs)-1] = peak_location
					endif
					break
				case 2:	//find max
					if (event_amp < abs(hard_min_amp))	// if it's smaller than the minimum amplitude, or negative, uncheck
						sel_events[i] = sel_events[i] %^ 2^4
					else		// if it's big enough, record it
						insertpoints numpnts(temp_amps), 1, temp_amps
						temp_amps[numpnts(temp_amps)-1] = event_amp
						insertpoints numpnts(peak_locs), 1, peak_locs
						peak_locs[numpnts(peak_locs)-1] = peak_location
					endif
					break
			endswitch
							
			text_events[i][1] = num2str(event_amp)		// record in list anyways, since it'll just be unchecked
		
		endif
		
		i+=1
	while (i < numpnts(peakPositionsX))

		
	if (numpnts(temp_amps)==0)
		killwaves/z temp_amps, $name_of_amps
		print "No events found or stored"
	else
		duplicate/O temp_amps, $name_of_amps
	
		print "--"+num2str(numpnts($name_of_amps))+" amplitudes saved into "+name_of_amps
		wavestats/z/q $name_of_amps
		print "     Mean = "+num2str(V_avg)+"    Std. Dev. = "+num2str(V_sdev)
		print "\n"
	endif
	
	i=0
	variable n=0			//counter		
	variable iei = 0
	variable prev_time = 0
	
	do
		if ((sel_events[i] & 2^4) != 0)								// if checked in manual selection window
			
			if (n != 0)
				iei = peakPositionsX[i]-prev_time
				insertpoints numpnts(temp_ieis), 1, temp_ieis
				temp_ieis[numpnts(temp_ieis)-1] = iei
			endif
			n+=1
			prev_time = peakPositionsX[i]							// save as reference for next number
			
			insertpoints numpnts(temp_times), 1, temp_times		// append time to wave
			temp_times[numpnts(temp_times)-1] = peakPositionsX[i]
		endif
		
		i+=1
	while (i < numpnts(peakPositionsX))
	
	if (numpnts(temp_ieis)==0)
		killwaves/z temp_ieis, $name_of_times
		print "No IEIs found or stored"
	else
		duplicate/O temp_ieis, $name_of_iei
		print "--"+num2str(numpnts($name_of_iei))+" intervals saved into "+name_of_iei
		wavestats/z/q $name_of_iei
		print "     Mean = "+num2str(V_avg)+"    Std. Dev. = "+num2str(V_sdev)
		print "----------------------------------------------"
		print "\n"	
	endif
	
	if (numpnts(temp_times)==0)
		killwaves/z temp_times, $name_of_times
	else
		duplicate/O temp_times, $name_of_times
	endif
	
		
	KillWaves temp_amps, temp_ieis, temp_times

end

Function bStoreEventTemplatesProc(ctrlName)
	string ctrlName

	wave sel_events = sel_events
	wave display_wave1 = display_wave1
	wave peakPositionsX = peakPositionsX
	NVAR kHz = kHz
	NVAR empKerDur = empKerDur
	NVAR disk_sweep_no = disk_sweep_no
	SVAR Expt = Expt
		
	Make/O/N=(empKerDur*kHz,1) setOfEvents=0		//multidimensional wave that will be appended with new waves

	//first find all sweeps that have been saved so far
	String AllTimesList = WaveList("s*_times", ";", "")
	Variable numList = ItemsInList(AllTimesList)
	Variable thisSweepNum
	
	//then iterate through each sweep, navigating to that sweep, and summing up all inputs
	
	variable i = 0		//sweep iterator
	variable n = 0		//number of events in this sweep
	variable j = 0		//number of events in ALL SWEEPS

	for (i=0;i<numList;i+=1)	
		
		string currentSweepTimesList = stringfromlist(i,AllTimesList)		//pick first wave in list of event times
		sscanf currentSweepTimesList, "s%f_times", thisSweepNum		//parse sweep number	
		
		//go to the sweep
		disk_sweep_no = thisSweepNum
		Find_Sweep(disk_sweep_no, Expt)
		Read_Sweep(Expt)

		duplicate/O $currentSweepTimesList tempSweepTimesList
				
		for(n=0;n<numpnts($currentSweepTimesList);n+=1)
			if(j>0)
				InsertPoints/M=1 DimSize(setOfEvents,1), 1, setOfEvents		//add another column
			endif
			variable eventLoc = tempSweepTimesList(n)
			Make/O/N=(empKerDur*kHz) singleEvent = display_wave1[p+eventLoc*kHz*1e3]		//grab the data
			variable baseSub = singleEvent[0]
			singleEvent -= baseSub						//baseline subtraction
			setOfEvents[][j] = singleEvent[p]		//insert single event into the column
			j+=1
		endfor
		
		killwaves tempSweepTimesList
	endfor
	
	
	SetScale/P x, 0, 1/(kHz), setOfEvents			// normalize to units of ms
	
	Variable numRows = DimSize(setOfEvents,0)
	Variable numCols = DimSize(setOfEvents,1)		//also known as j

	Make/O/N=0 meanTrace						//make mean trace
	
	variable kk
	for (kk=0;kk<numRows;kk+=1)
	
		duplicate/O/R=[kk][] setOfEvents tempWave
		Redimension/N=(numCols,0) tempWave
		Variable rowMean = mean(tempWave)
		meanTrace[kk] = rowMean
		InsertPoints numpnts(meanTrace), 1, meanTrace
		
	endfor
		
	SetScale/P x, 0, 1/(kHz), meanTrace

	String graphname = "Average_Trace"
	
	DoWindow/K $graphname											//cleanup
	String AllEventsList
	AllEventsList = WaveList("event_*", ";", "")

	printf "\r: killing event waves (event_*)\r"
 	numList = ItemsInList(AllEventsList)
 	Variable index
 	
 	if (numList != 0)
 		for (index=0;index<numList;index+=1)
 			KillWaves/Z $(StringFromList(index, AllEventsList))
 		endfor
 	endif
 	
	variable k												//plot it
	
	for (k=0; k<numCols; k+=1)
		string tempwavename = "event_"+num2str(k)
		duplicate/O/R=[][k] setOfEvents $tempwavename
		Redimension/N=(numRows,0) $tempwavename
		if (k==0)
			display/W=(0,212,645,752)/N=$graphname $tempwavename
		else
			AppendToGraph/W=$graphname $tempwavename
		endif
		ModifyGraph/W=$graphname rgb($tempwavename)=(52224,32224,32224), lsize($tempwavename)=0.5
	endfor

	AppendToGraph/W=$graphname meanTrace
	ModifyGraph/W=$graphname rgb(meanTrace)=(0,0,0), lsize(meanTrace)=2
	
	textbox/W=$graphname/A=MT/X=-3/F=0/E "Events, Mean and "+num2str(j)+" Singles"		
	Label left "pA"
	Label bottom "ms"
	
	Make/O/N=2 baseline = {0,0}
	Make/O/N=2 baselineX = {0,25}
	
	AppendToGraph baseline vs baselineX
	ModifyGraph lstyle(baseline)=7,rgb(baseline)=(0,0,0)

	
end

//--------------------END DECONVOLUTION STUFF----------------------------------------------------------------------