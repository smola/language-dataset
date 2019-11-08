#pragma rtGlobals=1		// Use modern global access method.
#pragma ModuleName= GEN_optimise

// SVN date:    $Date$
// SVN author:  $Author$
// SVN rev.:    $Revision$
// SVN URL:     $HeadURL$
// SVN ID:      $Id$

///GeneticOptimisation is a IGOR PRO procedure that fits data using a Genetic Algorithm method :written by Andrew Nelson
//Copyright (C) 2006 Andrew Nelson and Australian Nuclear Science and Technology Organisation
//
//This program is free software; you can redistribute it and/or
//modify it under the terms of the GNU General Public License
//as published by the Free Software Foundation; either version 2
//of the License, or (at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program; if not, write to the Free Software
//Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

//GeneticOptimisation is a powerful code to fit data, and is an extremely good method of finding global minima in the optimisation map
//The procedure uses the algorithm given in:
//
// Wormington et al, Characterization of structures from X-ray scattering data using genetic algorithms, Phil. Trans. R. Soc. Lond. A (1999) 357, 2827-2848
//
//The software should be compatible with Macintosh/PC/NT platforms and requires that IGOR Pro* is installed. 
//You do not have to purchase IGOR Pro - a free demo version of IGOR Pro is available, however some utilities are disabled (such as copying to/from the clipboard)
//IGOR Pro is a commercial software product available to Mac/PC/NT users. 
//A free demo version of IGOR is available from WaveMetrics Inc. These experiments and procedures were created using IGOR Pro 5.04
//The routines have not been tested on earlier versions of IGOR.

#pragma rtGlobals=1		// Use modern global access method.
#pragma ModuleName= GEN_optimise
#include <WaveSelectorWidget>
#include <PopupWaveSelector>
#include <Scatter Plot Matrix 2>

Menu "Analysis"
	"-"
	"Genetic curvefitting /1", Genetic_curvefitting()
	"-"
End

Function Genetic_Curvefitting()
	string cdf = getdatafolder(1)
	if(itemsinlist(winlist("gencurvefitpanel",";",""))==0)
		if(!datafolderexists("root:packages:motofit:gencurvefit"))
			newdatafolder/o root:packages
			newdatafolder/o root:packages:motofit
			newdatafolder/o/s root:packages:motofit:gencurvefit
		endif
		setdatafolder root:packages:motofit:gencurvefit
		string/g cmd
		if(!exists("lasttab"))
			variable/g lasttab=0
		endif
		if(!exists("saveStatus"))
			string/g saveStatus
		endif
		if(!exists("numcoefs"))
			variable/g numcoefs
		endif
		if(!exists("weighting_radio"))
			variable/g weighting_radio=1
		endif
		if(!exists("destlen"))
			variable/g destlen = 200
		endif
		if(!exists("cursorstart"))
			string/g cursorstart=""
		endif
		if(!exists("cursorfinish"))
			string/g cursorfinish=""
		endif
		if(!exists("ydataWAV"))
			string/g ydataWAV = "_none_"
		endif
		if(!exists("coefWAV"))
			string/g coefWAV = "_new wave_"
		endif
		if(!exists("xdataWAV"))
			string/g xdataWAV = "_calculated_"
		endif
		if(!exists("weightWAV"))
			string/g weightWAV = "_none_"
		endif
		if(!exists("functionstr"))
			string/g functionstr = stringfromlist(1,GEN_Functionlists())
		endif
		if(!exists("nindvars"))
			variable/g nindvars = 1
		endif
		if(!exists("numpoints"))
			variable/g numpoints
		endif
		if(!exists("holdstring"))
			string/g holdstring=""
		endif
		if(!exists("maskWAV"))
			string/g maskWAV = "_none_"
		endif
		if(!exists("limitsWAV"))
			string/g limitsWAV = "_from below_"
		endif
		if(!exists("destWAV"))
			string/g destWAV = "_auto_"
		endif
		if(!exists("resWAV"))
			string/g resWAV = "_none_"
		endif
		if(!exists("useInitGuess"))
			variable/g useInitGuess = 0
		endif
		if(!exists("tol"))
			variable/g tol = 0.001
		endif
		if(!exists("iterations"))
			variable/g iterations = 100
		endif
		if(!exists("popsize"))
			variable/g popsize = 20
		endif
		if(!exists("recomb"))
			variable/g recomb = 0.5
		endif
		if(!exists("k_m"))
			variable/g k_m =0.7
		endif
	
		if(!waveexists(root:packages:motofit:gencurvefit:GEN_listwave))
			make/o/n=(0,5)/t Gen_listwave
			make/o/n=(0,5) Gen_listselwave
			numcoefs=1
		endif
		Gen_listwave[][0] = num2istr(p)
		Gen_listselwave=0
		Gen_listselwave[][1] = 2
		Gen_listselwave[][2] = 32
		Gen_listselwave[][3] = 2
		Gen_listselwave[][4] = 2
		
		Execute "Gen_curvefitpanel_init()"
	else
		Dowindow/F gencurvefitpanel
	endif
	gen_setstatus()
	setdatafolder $cdf
End

Function Gen_curvefitpanel_init() : Panel
	svar ydataWav = root:packages:motofit:gencurvefit:ydataWav
	svar xdataWav = root:packages:motofit:gencurvefit:xdataWav
	svar coefWav = root:packages:motofit:gencurvefit:coefWav
	svar weightWav = root:packages:motofit:gencurvefit:weightWav
	svar maskWav = root:packages:motofit:gencurvefit:maskWav
	svar resWav = root:packages:motofit:gencurvefit:resWav
	svar functionstr = root:packages:motofit:gencurvefit:functionstr
	svar holdstring = root:packages:motofit:gencurvefit:holdstring
	svar limitsWav = root:packages:motofit:gencurvefit:limitsWAV
	svar destWav = root:packages:motofit:gencurvefit:destwav
	
	PauseUpdate; Silent 1		// building window...
	NewPanel /k=2/W=(343,159,930,540) as "Curvefitting with Genetic Optimisation"
	Dowindow/C gencurvefitpanel
	TabControl tab0,pos={12,6},size={560,324},proc=gen_tabcontrol
	TabControl tab0,tabLabel(0)="Function and Data",tabLabel(1)="Data Options"
	TabControl tab0,tabLabel(2)="Coefficients",tabLabel(3)="Output Options",value= 0
	Button fit_button,pos={19,343},size={81,24},proc=Gen_doFitButtonProc,title="Do it"
	Button tocmdline_button,pos={114,343},size={103,24},proc=GEN_tocmdline_buttonproc,title="To Cmd Line"
	Button toclip_button,pos={231,343},size={103,24},proc=GEN_toclip_buttonproc,title="To Clip"
	Button cancel_button,pos={463,343},size={103,24},proc=GEN_cancelButtonProc,title="cancel"
	GroupBox Function_group_tab0,pos={41,63},size={174,199},disable=1,title="Function",mode=1
	PopupMenu FunctionStr_popup_tab0,pos={52,86},size={148,20},disable=1,proc=gen_functionstr
	PopupMenu FunctionStr_popup_tab0,mode=2,bodyWidth= 148,popvalue = functionstr, value= #"GEN_Functionlists()"
	GroupBox ydata_group_tab0,pos={275,63},size={232,51},disable=1,title="Y Data"
	GroupBox xdata_group_tab0,pos={275,127},size={233,133},disable=1,title="X Data"
	TitleBox xdata_title_tab0,pos={291,144},size={196,12},disable=1,title="If you only have a ywave select _calculated_"
	TitleBox xdata_title_tab0,frame=0
	
	SetVariable ydataWav_setvar_tab0,pos={294,86},size={180,20},title=" ",fsize=12
	SetVariable ydataWav_setvar_tab0,bodyWidth= 180,noedit=1
	MakeSetVarIntoWSPopupButton("gencurvefitpanel", "ydataWav_setvar_tab0", "Gen_waveselectionNotification", "root:Packages:Motofit:gencurvefit:ydataWav",content = WMWS_Waves)
	PopupWS_MatchOptions("gencurvefitpanel", "ydataWav_setvar_tab0", matchStr="*", listoptions="DIMS:1,CMPLX:0,TEXT:0,BYTE:0,WORD:0,INTEGER:0,MINROWS:1",namefilterproc = "Gen_filter_ydataWav")
	PopupWS_AddSelectableString("gencurvefitpanel", "ydataWav_setvar_tab0","_none_")
	PopupWS_SetSelectionFullPath("gencurvefitpanel", "ydataWav_setvar_tab0","_none_")
	
	SetVariable xdataWav_setvar_tab0,pos={294,164},size={180,20},title=" ",fsize=12
	SetVariable xdataWav_setvar_tab0,bodyWidth= 180,noedit=1
	MakeSetVarIntoWSPopupButton("gencurvefitpanel", "xdataWav_setvar_tab0", "Gen_waveselectionNotification", "root:Packages:Motofit:gencurvefit:xdataWav",content = WMWS_Waves)
	PopupWS_MatchOptions("gencurvefitpanel", "xdataWav_setvar_tab0", matchStr="*", listoptions="CMPLX:0,TEXT:0,BYTE:0,WORD:0,INTEGER:0,MINROWS:1",namefilterproc = "Gen_filter_xdataWav")
	PopupWS_AddSelectableString("gencurvefitpanel", "xdataWav_setvar_tab0","_calculated_")
	PopupWS_SetSelectionFullPath("gencurvefitpanel", "xdataWav_setvar_tab0","_calculated_")
	
	CheckBox fromtarget_tab0 title="From target?",fsize=10,pos={481,52}
	
	GroupBox range_group_tab1,pos={29,51},size={170,127},disable=1,title="Range"
	GroupBox weighting_group_tab1,pos={211,51},size={170,127},disable=1,title="Weighting"
	GroupBox mask_group_tab1,pos={394,51},size={170,127},disable=1,title="Data Mask"

	SetVariable weightWAV_setvar_tab1,pos={217,74},size={140,20},title=" ",fsize=12
	SetVariable weightWAV_setvar_tab1,bodyWidth= 140,noedit=1
	MakeSetVarIntoWSPopupButton("gencurvefitpanel", "weightWAV_setvar_tab1", "Gen_waveselectionNotification", "root:Packages:Motofit:gencurvefit:weightWav",content = WMWS_Waves)
	PopupWS_MatchOptions("gencurvefitpanel", "weightWAV_setvar_tab1", matchStr="*", listoptions="DIMS:1,CMPLX:0,TEXT:0,BYTE:0,WORD:0,INTEGER:0,MINROWS:1",namefilterproc = "Gen_filter_dataWav")
	PopupWS_AddSelectableString("gencurvefitpanel", "weightWAV_setvar_tab1","_none_")
	PopupWS_SetSelectionFullPath("gencurvefitpanel", "weightWav_setvar_tab1","_none_")

	SetVariable range_lower_tab1,pos={41,98},size={55,21},disable=1,title=" "
	SetVariable range_lower_tab1,fSize=14,proc=gen_range_setvarProc
	SetVariable range_lower_tab1,limits={-inf,inf,0},value= root:packages:motofit:gencurvefit:cursorstart,bodyWidth= 50
	SetVariable range_upper_tab1,pos={129,99},size={55,21},disable=1,title=" "
	SetVariable range_upper_tab1,fSize=14,proc=gen_range_setvarProc
	SetVariable range_upper_tab1,limits={-inf,inf,0},value= root:packages:motofit:gencurvefit:cursorfinish,bodyWidth= 50
	Button rangecursors_button_tab1,pos={37,143},size={67,26},disable=1,title="cursors", proc=gen_cursors_buttonproc
	Button range_button_tab1,pos={123,143},size={67,26},disable=1,title="clear",proc=gen_clearcursors_buttonproc
	TitleBox rangelimits_group_tab1,pos={50,74},size={106,15},disable=1,title="start\t\tend"
	TitleBox rangelimits_group_tab1,fSize=12,frame=0
	GroupBox weightingchoice_group_tab1,pos={222,102},size={148,69},disable=1,title="Wave contains"
	CheckBox weighting_check0_tab1,pos={235,125},size={101,14},disable=1,proc=Gen_weightingRadioProc,title="Standard Deviation"
	CheckBox weighting_check0_tab1,value= 1,mode=1
	CheckBox weighting_check1_tab1,pos={235,144},size={112,14},disable=1,proc=Gen_weightingRadioProc,title="1/Standard Deviation"
	CheckBox weighting_check1_tab1,value= 0,mode=1

	SetVariable maskWAV_setvar_tab1,pos={400,74},size={140,20},title=" ",fsize=12
	SetVariable maskWAV_setvar_tab1,bodyWidth= 140,noedit=1
	MakeSetVarIntoWSPopupButton("gencurvefitpanel", "maskWAV_setvar_tab1", "Gen_waveselectionNotification", "root:Packages:Motofit:gencurvefit:maskWav",content = WMWS_Waves)
	PopupWS_MatchOptions("gencurvefitpanel", "maskWAV_setvar_tab1", matchStr="*", listoptions="DIMS:1,CMPLX:0,TEXT:0,BYTE:0,WORD:0,INTEGER:0,MINROWS:1",namefilterproc = "Gen_filter_dataWav")
	PopupWS_AddSelectableString("gencurvefitpanel", "maskWAV_setvar_tab1","_none_")
	PopupWS_SetSelectionFullPath("gencurvefitpanel", "maskWav_setvar_tab1","_none_")
 	
	CheckBox fromtarget_tab1 title="Select waves from target?",fsize=10,pos={113,248}
	

	SetVariable coefWAV_setvar_tab2,pos={23,57},size={227,20},title="Coefficient Wave",fsize=11
	SetVariable coefWAV_setvar_tab2,bodyWidth= 140,noedit=1
	MakeSetVarIntoWSPopupButton("gencurvefitpanel", "coefWAV_setvar_tab2", "Gen_waveselectionNotification", "root:Packages:Motofit:gencurvefit:coefWav",content = WMWS_Waves)
	PopupWS_MatchOptions("gencurvefitpanel", "coefWAV_setvar_tab2", matchStr="*", listoptions="DIMS:1,CMPLX:0,TEXT:0,BYTE:0,WORD:0,INTEGER:0,MINROWS:1",namefilterproc = "Gen_filter_coefWav")
	PopupWS_AddSelectableString("gencurvefitpanel", "coefWAV_setvar_tab2","_new wave_")
	PopupWS_SetSelectionFullPath("gencurvefitpanel", "coefWav_setvar_tab1","_new wave_")

	SetVariable limitsWAV_setvar_tab2,pos={326,94},size={173,20},title="Limits Wave",fsize=11
	SetVariable limitsWAV_setvar_tab2,bodyWidth= 110,noedit=1
	MakeSetVarIntoWSPopupButton("gencurvefitpanel", "limitsWAV_setvar_tab2", "Gen_waveselectionNotification", "root:Packages:Motofit:gencurvefit:limitsWav",content = WMWS_Waves)
	PopupWS_MatchOptions("gencurvefitpanel", "limitsWAV_setvar_tab2", matchStr="*", listoptions="DIMS:2,CMPLX:0,TEXT:0,BYTE:0,WORD:0,INTEGER:0,MINROWS:1,maxcols:2,mincols:2",namefilterproc = "Gen_filter_limitsWav")
	PopupWS_AddSelectableString("gencurvefitpanel", "limitsWAV_setvar_tab2","_from below_")
	PopupWS_SetSelectionFullPath("gencurvefitpanel", "limitsWav_setvar_tab2","_from below_")


	Button graphnow_button_tab2,pos={91,94},size={90,20},proc=gen_graphnowbutton,title="Graph now"
	Button graphnow_button_tab2,fSize=11
	ListBox coefficients_listbox_tab2,pos={23,147},size={538,177},disable=1,proc=Gen_ListBoxProc
	ListBox coefficients_listbox_tab2,listWave=root:packages:motofit:gencurvefit:Gen_listwave
	ListBox coefficients_listbox_tab2,selWave=root:packages:motofit:gencurvefit:Gen_listselwave
	ListBox coefficients_listbox_tab2,mode= 5,editStyle= 1,widths={30,80,20,60,60}
	
	SetVariable destWav_setvar_tab3,pos={17,45},size={211,20},title="Destination",fsize=11
	SetVariable destWav_setvar_tab3,bodyWidth= 140,noedit=1
	MakeSetVarIntoWSPopupButton("gencurvefitpanel", "destWav_setvar_tab3", "Gen_waveselectionNotification", "root:Packages:Motofit:gencurvefit:destWav",content = WMWS_Waves)
	PopupWS_MatchOptions("gencurvefitpanel", "destWav_setvar_tab3", matchStr="*", listoptions="DIMS:1,CMPLX:0,TEXT:0,BYTE:0,WORD:0,INTEGER:0,MINROWS:1",namefilterproc = "Gen_filter_destWav")
	PopupWS_AddSelectableString("gencurvefitpanel", "destWav_setvar_tab3","_auto_")
	PopupWS_SetSelectionFullPath("gencurvefitpanel", "destWav_setvar_tab3","_auto_")

	SetVariable resWav_setvar_tab3,pos={38,75},size={191,20},title="residual",fsize=11
	SetVariable resWav_setvar_tab3,bodyWidth= 140,noedit=1
	MakeSetVarIntoWSPopupButton("gencurvefitpanel", "resWav_setvar_tab3", "Gen_waveselectionNotification", "root:Packages:Motofit:gencurvefit:reswav",content = WMWS_Waves)
	PopupWS_MatchOptions("gencurvefitpanel", "resWav_setvar_tab3", matchStr="*", listoptions="DIMS:1,CMPLX:0,TEXT:0,BYTE:0,WORD:0,INTEGER:0,MINROWS:1",namefilterproc = "Gen_filter_resWav")
	PopupWS_AddSelectableString("gencurvefitpanel", "resWav_setvar_tab3","_none_;_auto wave_")
	PopupWS_SetSelectionFullPath("gencurvefitpanel", "resWav_setvar_tab3","_none_")


	CheckBox covar_check_tab3,pos={36,224},size={163,15},title="Create covariance matrix"
	CheckBox covar_check_tab3,fSize=12,value= 0
	CheckBox suppress_check_tab3,pos={36,246},size={164,15},title="Suppress screen updates"
	CheckBox suppress_check_tab3,fSize=12,value= 0
	SetVariable destlen_setvar_tab3,pos={280,44},size={112,17},title="Length:  "
	SetVariable destlen_setvar_tab3,fSize=11
	SetVariable destlen_setvar_tab3,limits={0,inf,0},value= root:packages:motofit:gencurvefit:destlen,bodyWidth= 60
	TitleBox tit1_title_tab2,pos={44,127},size={24,14},disable=1,title="coef"
	TitleBox tit1_title_tab2,fSize=11,frame=0
	TitleBox tit2_title_tab2,pos={133,127},size={63,14},disable=1,title="Initial guess"
	TitleBox tit2_title_tab2,fSize=11,frame=0
	TitleBox tit3_title_tab2,pos={253,127},size={29,14},disable=1,title="hold?"
	TitleBox tit3_title_tab2,fSize=11,frame=0
	TitleBox tit4_title_tab2,pos={335,127},size={55,14},disable=1,title="lower limit"
	TitleBox tit4_title_tab2,fSize=11,frame=0
	TitleBox tit5_title_tab2,pos={450,127},size={57,14},disable=1,title="upper limit"
	TitleBox tit5_title_tab2,fSize=11,frame=0
	SetVariable iterations_setvar_tab1,pos={424,206},size={125,17},disable=1,title="iterations"
	SetVariable iterations_setvar_tab1,fSize=11
	SetVariable iterations_setvar_tab1,limits={1,inf,0},value= root:packages:motofit:gencurvefit:iterations,bodyWidth= 70
	SetVariable popsize_setvar_tab1,pos={395,232},size={154,17},disable=1,title="population size"
	SetVariable popsize_setvar_tab1,fSize=11
	SetVariable popsize_setvar_tab1,limits={1,inf,0},value= root:packages:motofit:gencurvefit:popsize,bodyWidth= 70
	SetVariable recom_setvar_tab1,pos={348,259},size={201,17},disable=1,title="recombination constant"
	SetVariable recom_setvar_tab1,fSize=11
	SetVariable recom_setvar_tab1,limits={0,1,0},value= root:packages:motofit:gencurvefit:recomb,bodyWidth= 70
	SetVariable km_setvar_tab1,pos={377,286},size={173,17},disable=1,title="mutation constant"
	SetVariable km_setvar_tab1,fSize=11
	SetVariable km_setvar_tab1,limits={0,1,0},value= root:packages:motofit:gencurvefit:k_m,bodyWidth= 70
	NVAR useInitGuess = root:packages:motofit:gencurvefit:useInitGuess
	CheckBox initGuesses_setvar_tab1, pos={113,270},size={173,17},disable=1,title="use initial guesses as starting values?"
	CheckBox initGuesses_setvar_tab1,fSize=11, variable = useInitGuess
	
	Button default_button_tab2,pos={522,92},size={42,20},proc=gen_defaultlims_buttonproc,title="default"
	Button default_button_tab2,fSize=9
	
End

Function gen_tabcontrol(tca) : TabControl
	STRUCT WMTabControlAction &tca

	switch( tca.eventCode )
		case 2: // mouse up
			Variable tab = tca.tab
			Modifycontrol PopupWS_Button0,win= gencurvefitpanel, disable=(tab!=0)
			Modifycontrol PopupWS_Button1,win= gencurvefitpanel, disable=(tab!=0)
			Modifycontrol PopupWS_Button2,win= gencurvefitpanel, disable=(tab!=1)
			Modifycontrol PopupWS_Button3,win= gencurvefitpanel, disable=(tab!=1)
			Modifycontrol PopupWS_Button4,win= gencurvefitpanel, disable=(tab!=2)
			Modifycontrol PopupWS_Button5,win= gencurvefitpanel, disable=(tab!=2)
			Modifycontrol PopupWS_Button6,win= gencurvefitpanel, disable=(tab!=3)
			Modifycontrol PopupWS_Button7,win= gencurvefitpanel, disable=(tab!=3)
						
			ModifyControlList ControlNameList("",";","*_tab0") disable=(tab!=0)
			ModifyControlList ControlNameList("",";","*_tab1") disable=(tab!=1)
			ModifyControlList ControlNameList("",";","*_tab2") disable=(tab!=2)
			ModifyControlList ControlNameList("",";","*_tab3") disable=(tab!=3)
			break
	endswitch

	return 0
End

Function gen_functionstr(pa) : PopupMenuControl
	STRUCT WMPopupAction &pa

	switch( pa.eventCode )
		case 2: // mouse up
			svar/z functionStr = root:packages:motofit:gencurvefit:functionstr
			functionstr = pa.popstr
			Variable popNum = pa.popNum
			String popStr = pa.popStr
			break
	endswitch

	return 0
End
Function Gen_waveselectionNotification(event, wavepath, windowName, ctrlName)
	Variable event
	String wavepath
	String windowName
	String ctrlName
	SVAR coefWav = root:packages:motofit:gencurvefit:coefWav
	NVAR numcoefs = root:packages:motofit:gencurvefit:numcoefs

	switch(event)
		case WMWS_SelectionChanged:				//WMWS_SelectionChanged = 4, the only event code.  Defined in WaveSelectorWidget
			strswitch(ctrlname)
				case "ydataWav_setvar_tab0":
					nvar numpoints = root:packages:motofit:gencurvefit:numpoints
					numpoints = numpnts($wavepath)
					break
				case "limitsWAV_setvar_tab2":
					if(cmpstr(wavepath,"_from below_"))
						wave limits = $(wavepath)
						Gen_insertLimits(limits)
					endif
					break	
				case "coefWAV_setvar_tab2":					
					if(cmpstr(wavepath,"_New Wave_")==0)
						string newwavename
						variable nc
						dowindow/w=popupWSpanel /k popupwspanel
						newpanel/W=(0,0,1,1)
						dowindow/c popupwspanel
							
						Prompt newwavename,"enter the new wave name"
						Prompt nc,"and the number of coefficients (>=1)"
						do
							doprompt "Create a new fit wave",newwavename,nc
							if(V_flag)
								break
							else
								newwavename = possiblyquotename(newwavename)
								if(exists(newwavename))
									Doalert 0, "name already in use"
									continue
								endif
								if(strlen(newwavename)==0 || itemsinlist(wavelist(newwavename,";","")) || strlen(newwavename)>31)
									Doalert 0,"Please enter a valid wavename"
									continue
								endif
								if(nc<1)
									Doalert 0, "number of coefficients must be >0"
									continue
								endif
								make/o/d/n=(nc) $newwavename = 0
								string fullpath = getdatafolder(1)+newwavename
									
								//another BODGE
								//the function that calls this notify procedure doesn't check that you have created anything before if returns.
								//this means if you create a wave, then use popws_setselectionfull path it overwrites whatever you did.
									
								string cmd = "PopupWS_SetSelectionFullPath(\""+windowname+"\",\""+ctrlname+"\",\""+ fullPath+"\")"
								execute/p cmd
								//									killcontrol/w = gencurvefitpanel coefWAV_setvar_tab2
								//									SetVariable coefWAV_setvar_tab2,win = gencurvefitpanel,pos={23,57},size={227,20},title="Coefficient Wave",fsize=11
								//									SetVariable coefWAV_setvar_tab2,win = gencurvefitpanel,bodyWidth= 140,noedit=1
								//									MakeSetVarIntoWSPopupButton("gencurvefitpanel", "coefWAV_setvar_tab2", "Gen_waveselectionNotification", "root:Packages:Motofit:gencurvefit:coefWav",content = WMWS_Waves)
								//									PopupWS_MatchOptions("gencurvefitpanel", "coefWAV_setvar_tab2", matchStr="*", listoptions="DIMS:1,CMPLX:0,TEXT:0,BYTE:0,WORD:0,INTEGER:0,MINROWS:1",namefilterproc = "Gen_filter_coefWav")
								//									PopupWS_AddSelectableString("gencurvefitpanel", "coefWAV_setvar_tab2","_new wave_")
								//
								//									PopupWS_SetSelectionFullPath(windowname,ctrlname, fullPath)
								coefWav = newwavename
								numcoefs = nc
								break
							endif
						while(1)
						Gen_expandnpars(numcoefs)
						Gen_insertCoefs($fullpath)
					else
						Wave coefs = $(wavepath)
						numcoefs = Dimsize(coefs,0)
						Gen_expandnpars(numcoefs)
						Gen_insertCoefs(coefs)
					endif
					break
			endswitch
			break
	endswitch
	Gen_rebuildPopups(event,wavepath,windowname,ctrlname)
End

Function Gen_expandnpars(numpars)
	variable numpars
	Wave/T Gen_listwave = root:packages:motofit:gencurvefit:Gen_listwave
	Wave Gen_listselwave = root:packages:motofit:gencurvefit:Gen_listselwave
	variable ii
	variable prev = dimsize(Gen_listwave,0)
	
	Redimension/n=(numpars,5) GEN_listselwave,GEN_listwave
	GEN_listselwave[][4] =2
	GEN_listselwave[][3]=2
	GEN_listselwave[][1]=2
	for(ii=prev ; ii< numpars ; ii+=1)
		GEN_listselwave[ii][2]=32
	endfor
	GEN_listwave[][0]=num2istr(p)
	
End

Function Gen_insertCoefs(coefs)
	Wave coefs
	Wave/T Gen_listwave = root:packages:motofit:gencurvefit:Gen_listwave
	Wave Gen_listselwave = root:packages:motofit:gencurvefit:Gen_listselwave
	if(numpnts(coefs)!=Dimsize(GEN_listwave,0) || Wavedims(coefs)!=1)
		Abort "error somewhere"
	endif
	Gen_listwave[][1] = num2str(coefs[p])
End

Function Gen_insertLimits(limits)
	Wave limits
	Wave/T Gen_listwave = root:packages:motofit:gencurvefit:Gen_listwave
	Wave Gen_listselwave = root:packages:motofit:gencurvefit:Gen_listselwave
	if(dimsize(limits,0)!=Dimsize(GEN_listwave,0) || dimsize(limits,1)!=2 || Wavedims(limits)!=2)
		Abort "error somewhere"
	endif
	Gen_listwave[][3] = num2str(limits[p][0])
	Gen_listwave[][4] = num2str(limits[p][1])
End

Function Gen_extractLimits()
	Wave/T Gen_listwave = root:packages:motofit:gencurvefit:Gen_listwave
	make/o/d/n=(dimsize(gen_listwave,0),2) root:packages:motofit:gencurvefit:gen_limits
	Wave gen_limits = root:packages:motofit:gencurvefit:gen_limits
	gen_limits[][0] = str2num(gen_listwave[p][3])
	gen_limits[][1] = str2num(gen_listwave[p][4])
End


Function Gen_weightingRadioProc(CB_Struct):checkboxcontrol
	STRUCT WMCheckboxAction &CB_Struct
	
	NVAR weighting_Radio = root:packages:motofit:gencurvefit:weighting_Radio
	
	strswitch (CB_Struct.ctrlname)
		case "weighting_check0_tab1":
			weighting_Radio= 1
			break
		case "weighting_check1_tab1":
			weighting_Radio= 2
			break
	endswitch
	CheckBox weighting_check0_tab1,value= weighting_Radio==1,win=gencurvefitpanel
	CheckBox weighting_check1_tab1,value= weighting_Radio==2,win=gencurvefitpanel
End


Function/s GEN_Functionlists([indvars])
	variable indvars
	string theList="", UserFuncs, XFuncs
	string options = "KIND:10"
	options += ",SUBTYPE:FitFunc"
	if(!paramisdefault(indvars))
		options += ",NINDVARS:"+num2istr(indvars)
	endif
	UserFuncs = FunctionList("*", ";",options)
	UserFuncs = RemoveFromList("GFFitFuncTemplate", UserFuncs)
	UserFuncs = RemoveFromList("GFFitAllAtOnceTemplate", UserFuncs)
	UserFuncs = RemoveFromList("NewGlblFitFunc", UserFuncs)
	UserFuncs = RemoveFromList("NewGlblFitFuncAllAtOnce", UserFuncs)
	UserFuncs = RemoveFromList("GlobalFitFunc", UserFuncs)
	UserFuncs = RemoveFromList("GlobalFitAllAtOnce", UserFuncs)
	UserFuncs = RemoveFromList("MOTO_GFFitFuncTemplate", UserFuncs)
	UserFuncs = RemoveFromList("MOTO_GFFitAllAtOnceTemplate", UserFuncs)
        
	XFuncs = FunctionList("*", ";", "KIND:12")
	if (strlen(UserFuncs) > 0)
		theList +=  "\\M1(   User-defined functions:;"
		theList += UserFuncs
	endif
	if (strlen(XFuncs) > 0)
		theList += "\\M1(   External Functions:;"
		theList += XFuncs
	endif
	if (strlen(theList) == 0)
		theList = "\\M1(No Fit Functions"
	endif
	return theList
End

Function GEN_numIndVars_func(funcStr)
	string funcstr
	variable ii
	string info
	for(ii = 0 ; ii<100; ii+=1)
		info = FunctionList(funcstr, ";", "KIND:31,NINDVARS:"+num2istr(ii))
		if(strlen(info))
			return ii
		endif
	endfor
	return NaN
End

Function GEN_cancelButtonProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			gen_savestatus()
			Killwindow GenCurvefitPanel
			break
	endswitch

	return 0
End

Function GEN_tocmdline_buttonproc(ba) : ButtonControl
	STRUCT WMButtonAction &ba
	SVAR cmd = root:packages:motofit:gencurvefit:cmd
	switch( ba.eventCode )
		case 2: // mouse up
			if(Gen_parsetoFitCmd())
				return 0
			endif
			Tocommandline cmd
			gen_savestatus()
			killwindow gencurvefitpanel
			break
	endswitch

	return 0
End

Function GEN_toclip_buttonproc(ba) : ButtonControl
	STRUCT WMButtonAction &ba
	SVAR cmd = root:packages:motofit:gencurvefit:cmd
	switch( ba.eventCode )
		case 2: // mouse up
			if(Gen_parsetoFitCmd())
				return 0
			endif
			putscraptext cmd
			gen_savestatus()
			killwindow gencurvefitpanel
			break
	endswitch

	return 0
End

Function Gen_ListBoxProc(lba) : ListBoxControl
	STRUCT WMListboxAction &lba

	Variable row = lba.row
	Variable col = lba.col
	WAVE/T/Z listWave = lba.listWave
	WAVE/Z selWave = lba.selWave

	switch( lba.eventCode )
		case -1: // control being killed
			break
		case 3: // double click
			break
		case 4: // cell selection
		case 5: // cell selection plus shift key
			break
		case 6: // begin edit
			break
		case 7: // finish edit
			if(numtype(str2num(listwave[row][col])))
				DoAlert 0, "Numeric value required"
			endif
			break
	endswitch

	return 0
End

Function Gen_checkLimits()
	Wave/T Gen_listwave = root:packages:motofit:gencurvefit:Gen_listwave
	Wave Gen_listselwave = root:packages:motofit:gencurvefit:Gen_listselwave
	variable ii
	for(ii=0;ii<dimsize(gen_listwave,0);ii+=1)
		if(numtype(str2num(gen_listwave[ii][3])))
			Doalert 0, "Lower Limit: "+num2istr(ii)+ " is not a number"
			return 1
		endif
		if(numtype(str2num(gen_listwave[ii][4])))
			Doalert 0, "Upper limit: "+num2istr(ii)+ " is not a number"
			return 1
		endif
	endfor
End


Function Gen_checkParams()
	Wave/T Gen_listwave = root:packages:motofit:gencurvefit:Gen_listwave
	Wave Gen_listselwave = root:packages:motofit:gencurvefit:Gen_listselwave
	variable ii
	for(ii=0;ii<dimsize(gen_listwave,0);ii+=1)
		if(numtype(str2num(gen_listwave[ii][1])))
			Doalert 0, "Parameter: "+num2istr(ii)+ " is not a number"
			return 1
		endif
	endfor
End

Function Gen_checkLimitBoundaries()
	Wave/T Gen_listwave = root:packages:motofit:gencurvefit:Gen_listwave
	Wave Gen_listselwave = root:packages:motofit:gencurvefit:Gen_listselwave
	variable ii
	variable val,lowlim,upperlim
	for(ii=0;ii<dimsize(gen_listwave,0);ii+=1)
		val = str2num(gen_listwave[ii][1])
		lowlim = str2num(gen_listwave[ii][3])
		upperlim = str2num(gen_listwave[ii][4])
		if( !(gen_listselwave[ii][2]&2^4))
			if(lowlim>upperlim)
				Doalert 0, "lower limit for parameter: "+num2istr(ii)+ " is greater than the upper limit"
				return 1
			endif
		Endif
	endfor
End

Function gen_isvalueinlimitboundaries()
	Wave/T Gen_listwave = root:packages:motofit:gencurvefit:Gen_listwave
	Wave Gen_listselwave = root:packages:motofit:gencurvefit:Gen_listselwave
	variable ii
	variable val,lowlim,upperlim
	for(ii=0;ii<dimsize(gen_listwave,0);ii+=1)
		val = str2num(gen_listwave[ii][1])
		lowlim = str2num(gen_listwave[ii][3])
		upperlim = str2num(gen_listwave[ii][4])
		if( !(gen_listselwave[ii][2]&2^4))
			if( val < lowlim || val > upperlim)
				Doalert 0, "Because you selected use initial guesses parameter: "+num2istr(ii)+ " should be between the limits"
				return 1
			endif
		Endif
	endfor
End

Function Gen_buildholdstring()
	Wave/T Gen_listwave = root:packages:motofit:gencurvefit:Gen_listwave
	Wave Gen_listselwave = root:packages:motofit:gencurvefit:Gen_listselwave
	SVAR holdstring = root:packages:motofit:gencurvefit:holdstring
	holdstring = ""
	variable ii
	for(ii=0;ii<dimsize(Gen_listwave,0);ii+=1)
		if(gen_listselwave[ii][2]&2^4)
			holdstring +="1"
		else
			holdstring +="0"
		endif
	endfor
End

Function Gen_parsetoFitCmd()
	variable err
	Gen_buildholdstring()
	if(Gen_checkParams())
		return 1
	endif
	if(Gen_checkLimits())
		return 1
	endif	
	if(gen_checklimitboundaries())
		return 1
	endif
	
	string hostwindow = "gencurvefitpanel"
	string ydataWav = PopupWS_GetSelectionFullPath(hostWindow, "ydataWav_setvar_tab0")
	string xdataWav = PopupWS_GetSelectionFullPath(hostWindow, "xdataWav_setvar_tab0")
	string coefWav = PopupWS_GetSelectionFullPath(hostWindow, "coefWav_setvar_tab2")
	string weightWav = PopupWS_GetSelectionFullPath(hostWindow, "weightWav_setvar_tab1")
	string  maskwav = PopupWS_GetSelectionFullPath(hostWindow, "maskWav_setvar_tab1")
	string resWav = PopupWS_GetSelectionFullPath(hostWindow, "resWav_setvar_tab3")
	svar functionstr = root:packages:motofit:gencurvefit:functionstr
	nvar weighting_radio = root:packages:motofit:gencurvefit:weighting_radio
	nvar destlen = root:packages:motofit:gencurvefit:destlen
	svar cursorstart = root:packages:motofit:gencurvefit:cursorstart
	svar cursorfinish = root:packages:motofit:gencurvefit:cursorfinish
	svar holdstring = root:packages:motofit:gencurvefit:holdstring
	string limitsWav = PopupWS_GetSelectionFullPath(hostWindow, "limitsWav_setvar_tab2")
	string destWav = PopupWS_GetSelectionFullPath(hostWindow, "destWav_setvar_tab3")
	svar cmd = root:packages:motofit:gencurvefit:cmd
	nvar tol = root:packages:motofit:gencurvefit:tol
	nvar iterations = root:packages:motofit:gencurvefit:iterations
	nvar popsize = root:packages:motofit:gencurvefit:popsize
	nvar recomb = root:packages:motofit:gencurvefit:recomb
	nvar k_m = root:packages:motofit:gencurvefit:k_m
	Wave ywave = $ydataWav
	nvar useInitGuess = root:packages:motofit:gencurvefit:useInitGuess
			
	cmd = "gencurvefit "
	if(cmpstr(xdataWav,"_calculated_") !=0)
		cmd += "/X="+xdataWav
	endif
	
	//you specified that you want to use the initial guess as starting parameters in the fit, instead of randomisation
	//this means that you need to check that the values are in between the limits
	if(useInitGuess)
		if(gen_isvalueinlimitboundaries())
			return 1
		endif
		cmd +="/OPT=1"
	endif
	
	cmd += "/K={"+num2str(iterations)+","+num2str(popsize)+","+num2str(k_m)+","+num2str(recomb)+"}"
	cmd += "/TOL="+num2str(tol)
	
	if(cmpstr(destWav,"_auto_") == 0)
		cmd+="/L="+num2str(destlen)
	else
		cmd += "/D="+destWav
	endif
	
	if(cmpstr(maskWav,"_none_") !=0)
		cmd +="/M="+maskwav
	endif
	
	if(cmpstr(resWav,"_none_")==0)
	elseif(cmpstr(resWav,"_auto wave_")==0)
		cmd += "/R"
	else
		cmd += "/R="+resWav
	endif
	
	if(cmpstr(weightWav,"_none_"))
		cmd+="/W="+weightWav
		if(weighting_radio ==1)
			cmd+="/I=1"
		elseif(weighting_radio==2)
			cmd+="/I=0"
		endif
	endif
	
	controlinfo/w=gencurvefitpanel suppress_check_tab3
	if(V_Value)
		cmd+="/N"
	endif
	
	controlinfo/w=gencurvefitpanel covar_check_tab3
	if(V_Value)
		cmd+="/MAT"
	endif
	
	if(cmpstr(functionstr,"_none_")==0)
		Doalert 0, "You haven't entered a fitfunction"
		return 1
	else
		cmd+= " "+functionstr
	endif
	if(cmpstr(ydataWav,"_none_")==0)
		DoAlert 0, "You haven't entered a ywave"
		return 1
	else
		cmd+= ","+ydataWav
	endif
	
	if(strlen(cursorstart)>0 || strlen (cursorfinish)>0)
		string topgraph = winname(0,1)
		SVAR ydataWavstr = root:packages:motofit:gencurvefit:ydataWav
		variable start,finish
		if(gen_checkcursors(cursorstart) || gen_checkcursors(cursorfinish))
			return 1
		endif
		strswitch(cursorstart)
			case "pcsr(A)":
				err = whichlistitem(ydatawavstr,tracenamelist(topgraph,";",1))
				if(err==-1)
					Doalert 0, "the y wave is not displayed as a trace in the topgraph"
					return 1
				endif
				if (WaveExists(CsrWaveRef(A)) %& WaveExists(CsrWaveRef(B)))
					if (CmpStr(CsrWave(A),CsrWave(B)) != 0)
						Doalert 0,"The cursors are not on the same wave. Please move them so that they are."
						return 1
					endif
				else
					doalert 0,"The cursors must be placed on the top graph.  Select Show Info from the Graph menu for access to the cursors."
					return 1
				endif
				start = pcsr(A)
				break
			case "pcsr(B)":
				err = whichlistitem(ydatawavstr,tracenamelist(topgraph,";",1))
				if(err==-1)
					Doalert 0, "the y wave is not displayed as a trace in the topgraph"
					return 1
				endif
				if (WaveExists(CsrWaveRef(A)) %& WaveExists(CsrWaveRef(B)))
					if (CmpStr(CsrWave(A),CsrWave(B)) != 0)
						Doalert 0,"The cursors are not on the same wave. Please move them so that they are."
						return 1
					endif
				else
					doalert 0,"The cursors must be placed on the top graph.  Select Show Info from the Graph menu for access to the cursors."
					return 1
				endif
				start = pcsr(B)
				break
			default:
				start = str2num(cursorstart)
				break
		endswitch
		strswitch(cursorfinish)
			case "pcsr(A)":
				err = whichlistitem(ydatawavstr,tracenamelist(topgraph,";",1))
				if(err==-1)
					Doalert 0, "the y wave is not displayed as a trace in the topgraph"
					return 1
				endif
				if (WaveExists(CsrWaveRef(A)) %& WaveExists(CsrWaveRef(B)))
					if (CmpStr(CsrWave(A),CsrWave(B)) != 0)
						Doalert 0,"The cursors are not on the same wave. Please move them so that they are."
						return 1
					endif
				else
					doalert 0,"The cursors must be placed on the top graph.  Select Show Info from the Graph menu for access to the cursors."
					return 1
				endif
				finish = pcsr(A)
				break
			case "pcsr(B)":
				err = whichlistitem(ydatawavstr,tracenamelist(topgraph,";",1))
				if(err==-1)
					Doalert 0, "the y wave is not displayed as a trace in the topgraph"
					return 1
				endif
				if (WaveExists(CsrWaveRef(A)) %& WaveExists(CsrWaveRef(B)))
					if (CmpStr(CsrWave(A),CsrWave(B)) != 0)
						Doalert 0,"The cursors are not on the same wave. Please move them so that they are."
						return 1
					endif
				else
					doalert 0,"The cursors must be placed on the top graph.  Select Show Info from the Graph menu for access to the cursors."
					return 1
				endif
				finish = pcsr(B)
				break
			default:
				finish= str2num(cursorfinish)
				break
		endswitch
		if(finish<start || start<0 || finish <0 || finish>numpnts(ywave) || finish-start <1)
			doalert 0, "There is something wrong with the cursor range entered"
			return 1
		endif
		cmd+="["+num2istr(start)+","+num2istr(finish)+"]"
	endif

	
	if(cmpstr(coefWav,"_default_")==0 || cmpstr(coefWav,"_new Wave_")==0)
		Doalert 0, "You need to select a coefficient Wave"
		return 1
	else 
		Wave/t gen_listwave = root:packages:motofit:gencurvefit:gen_listwave
		Wave coefs = $coefWav
		redimension/n=(dimsize(gen_listwave,0)) coefs
		coefs[] = str2num(gen_listwave[p][1])
		cmd+=","+coefWav
	endif
	cmd+=",\""+holdstring+"\""
	
	if(cmpstr(limitswav,"_from below_")==0)
		gen_extractlimits()
		cmd +=",root:packages:motofit:gencurvefit:gen_limits"
	else
		Wave/t gen_listwave = root:packages:motofit:gencurvefit:gen_listwave
		Wave limits = $limitsWav
		limits[][0] = str2num(gen_listwave[p][3])
		limits[][1] = str2num(gen_listwave[p][4])
		cmd+=","+limitsWav
	endif
	return 0
End

Function gen_graphnowbutton(ba) : ButtonControl
	STRUCT WMButtonAction &ba
	
	svar ydataWav = root:packages:motofit:gencurvefit:ydataWav
	svar xdataWav = root:packages:motofit:gencurvefit:xdataWav
	svar coefWav = root:packages:motofit:gencurvefit:coefWav
	svar weightWav = root:packages:motofit:gencurvefit:weightWav
	svar maskWav = root:packages:motofit:gencurvefit:maskWav
	svar resWav = root:packages:motofit:gencurvefit:resWav
	svar functionstr = root:packages:motofit:gencurvefit:functionstr
	svar destWav = root:packages:motofit:gencurvefit:destWav
	nvar destlen = root:packages:motofit:gencurvefit:destLen
	svar cursorStart = root:packages:motofit:gencurvefit:cursorStart
	svar cursorfinish = root:packages:motofit:gencurvefit:cursorfinish
	variable cost = 0, ii, startp, endp
	
	variable wasOffset = 0
	switch( ba.eventCode )
		case 2: // mouse up
			if(cmpstr(ydataWav,"_none_")==0)
				doalert 0, "Please select a ywave first"
				return 0
			endif
			Wave/z ywave = $PopupWS_GetSelectionFullPath("gencurvefitpanel","ydataWav_setvar_tab0")

			if(!waveexists(ywave))
				DoAlert 0, "the y wave doesn't exist, have you moved it, or did you select a data folder in the ywave popup by mistake?"
				return 0
			endif
			
			variable err = whichlistitem(ydatawav,(tracenamelist(winname(0,1),";",1)));
			if(err==-1)
				DoAlert 0, "It was impossible to plot your fitting function: the fit data is not on the top graph"
				return 0
			endif
			
			if( Gen_checkParams())
				return 0
			endif
			
			if(cmpstr(coefWav,"_new Wave_")==0)
				Doalert 0, "You need to select a coefficient Wave"
				return 0
			else 
				Wave/t gen_listwave = root:packages:motofit:gencurvefit:gen_listwave
				Wave coefs = $(PopupWS_GetSelectionFullPath("gencurvefitpanel","coefWav_setvar_tab2"))
				redimension/n=(dimsize(gen_listwave,0)) coefs
				coefs[] = str2num(gen_listwave[p][1])
			endif
			
			string output
			output = cleanupname("fit_"+ydatawav,1)
			make/o/d/n=(numpnts(ywave)) $output = NaN
			Wave outputWav = $(output)
			setscale/p x,dimoffset(ywave,0),dimdelta(ywave,0), outputWav

			string fulloutputpath = getwavesdatafolder(outputWav,2)
			string fullcoefpath =  getwavesdatafolder(coefs,2)
			string fullxwavepath =  PopupWS_GetSelectionFullPath("gencurvefitpanel","xdataWav_setvar_tab0")
			string cmd
			string funcinfo = functioninfo(functionstr)
			variable nparams = numberbykey("N_Params",funcinfo)
			switch(nparams)
				case 2:
					if(!(numberbykey("PARAM_0_TYPE",funcinfo)&16484) || (numberbykey("PARAM_1_TYPE",funcinfo)!=4))
						DoAlert 0, "Something may be wrong with your fitfunction"
					else
	
						if(cmpstr(xdataWav,"_calculated_")==0)
							cmd = fulloutputpath+"="+functionstr+"("+fullcoefpath+",x)"
						else
							cmd = fulloutputpath +"="+functionstr+"("+fullcoefpath+","+fullxwavepath+")"
						endif
					endif
					break
				case 3:
					if(!(numberbykey("PARAM_0_TYPE",funcinfo)&16484) || !(numberbykey("PARAM_1_TYPE",funcinfo)&16484) || !(numberbykey("PARAM_2_TYPE",funcinfo)&16484) )
						DoAlert 0, "Something may be wrong with your fitfunction"
					else
						if(cmpstr(xdataWav,"_calculated_")==0)
							make/o/d/n=(numpnts(outputWav)) root:packages:motofit:gencurvefit:tempx
							Wave tempx = root:packages:motofit:gencurvefit:tempx
							tempx[] = dimoffset(outputWav,0)+p*dimdelta(outputWav,0)
							cmd = functionstr+"("+fullcoefpath+","+fulloutputpath+",root:packages:motofit:gencurvefit:tempx)"
						else
							cmd = functionstr+"("+fullcoefpath+","+fulloutputpath+","+fullxwavepath+")"
						endif
					endif
					break
				default:
					Doalert 0, "Cannot handle multivariate fits in this dialogue at the moment"
					return 0
					break
			endswitch
			execute/q cmd
			err = whichlistitem(output,(tracenamelist(winname(0,1),";",1)))
			if(err!=-1)
				string thetrace = traceinfo(winname(0,1), output, 0)
				string offset = greplist(theTrace, "^offset")
				string muloffset = greplist(theTrace, "^muloffset")
				offset = removeending(replacestring("x", offset, output), ";")
				muloffset = removeending(replacestring("x", muloffset, output), ";")
				wasOffset = 1
				removefromgraph/w=$(winname(0,1)) $output
			endif
			if(cmpstr(xdataWav,"_calculated_")==0)
				appendtograph/w=$(winname(0,1)) outputWav
			else
				Wave xwave = $(fullxwavepath)
				appendtograph/w=$(winname(0,1)) outputWav vs xwave 
			endif
			if(wasOffset ==1)
				cmd = "modifygraph/W=" + winname(0,1) + " " + muloffset
				execute/q cmd
				cmd = "modifygraph/W=" + winname(0,1) + " " + offset
				execute/q cmd
			endif
			//lets calculate chi2
			make/n=(dimsize(ywave, 0))/free/d chi2
			chi2 = ywave - outputWav
			Wave/z weighting = $PopupWS_GetSelectionFullPath("gencurvefitpanel","weightWAV_setvar_tab1")
			if(waveexists(weighting))
				controlinfo/w=gencurvefitpanel weighting_check0_tab1
				if(V_value)
					chi2 /= weighting
				else
					chi2 *= weighting
				endif
			endif
			chi2 *= chi2
			Wave/z maskwave = $maskWav
			if(waveexists(maskWave))
				for(ii = 0 ; ii < dimsize(maskwave, 0) ; ii+=1)
					if(maskwave[ii] == 0 || numtype(maskwave[ii]) == 2)
						chi2[ii] = 0
					endif
				endfor
			endif
			endp = numpnts(chi2) - 1
			if(!numtype(str2num(cursorfinish)))
				endp = str2num(cursorfinish)
			endif
			if(!numtype(str2num(cursorstart)))
				startp = str2num(cursorstart)
			endif
			if(stringmatch(cursorfinish, "*pcsr*"))
				endp = pcsr(B)
			endif
			if(stringmatch(cursorstart, "*pcsr*"))
				startp = pcsr(A)
			endif
			print "Function:", functionStr,"ywave:",ydatawav,"(",dimsize(ywave, 0),")","Chi2: ", sum(chi2, startp, endp)
			
			
	endswitch

	return 0
End

Function Gen_doFitButtonProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			if(!Gen_parsetoFitCmd())
				svar cmd = root:packages:motofit:gencurvefit:cmd
				
				//if the fitwave is already present it's a good idea to remove it. 	
				svar ydataWav = root:packages:motofit:gencurvefit:ydataWav
				string output = cleanupname("fit_"+ydatawav,1)
				variable err = whichlistitem(output,(tracenamelist(winname(0,1),";",1)))
				if(err!=-1)
					removefromgraph/w=$(winname(0,1)) $output
				endif
				cmdToHistory(cmd)
				execute/p/q/z cmd
				gen_savestatus()
				
				killwindow gencurvefitpanel
			endif
			break
	endswitch

	return 0
End

Function cmdToHistory(cmd)	// returns 0 if Macintosh, 1 if Windows
	string cmd
	String platform= UpperStr(igorinfo(2))
	strswitch(platform)
		case "MACINTOSH":
			print num2char(-91) + cmd
			break
		case "WINDOWS":
			print num2char(-107)+cmd
			break
	endswitch
End

Function gen_cursors_buttonproc(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			svar cursorstart = root:packages:motofit:gencurvefit:cursorstart 
			svar cursorfinish= root:packages:motofit:gencurvefit:cursorfinish
			
			string topgraph = winname(0,1)
			svar ydataWav = root:packages:motofit:gencurvefit:ydataWav 
		
			variable err = whichlistitem(ydataWav,tracenamelist(topgraph,";",1))
			if(err==-1)
				Doalert 0, "the y wave is not displayed as a trace in the topgraph"
				cursorstart = ""
				cursorfinish = ""
				return 0
			endif
			if (WaveExists(CsrWaveRef(A)) %& WaveExists(CsrWaveRef(B)))
				if (CmpStr(CsrWave(A),CsrWave(B)) != 0)
					Doalert 0,"The cursors are not on the same wave. Please move them so that they are."
					cursorstart = ""
					cursorfinish = ""
					return 0
				endif
			else
				doalert 0,"The cursors must be placed on the top graph.  Select Show Info from the Graph menu for access to the cursors."
				cursorstart = ""
				cursorfinish = ""
				return 0
			endif
			svar cursorstart = root:packages:motofit:gencurvefit:cursorstart 
			svar cursorfinish= root:packages:motofit:gencurvefit:cursorfinish
			cursorstart = "pcsr(a)"
			cursorfinish = "pcsr(b)"
			break
	endswitch

	return 0
End

Function gen_clearcursors_buttonproc(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			svar cursorstart = root:packages:motofit:gencurvefit:cursorstart 
			svar cursorfinish= root:packages:motofit:gencurvefit:cursorfinish
			cursorstart = ""
			cursorfinish = ""
			break
	endswitch

	return 0
End

Function gen_range_setvarProc(sva) : SetVariableControl
	STRUCT WMSetVariableAction &sva

	switch( sva.eventCode )
		case 1: // mouse up
		case 2: // Enter key
		case 3: // Live update
		
			gen_checkcursors(sva.sval)
			return 0
			break
	endswitch

	return 0
End

Function gen_checkcursors(startORend)
	string startORend
	svar cursorstart= root:packages:motofit:gencurvefit:cursorstart
	svar cursorfinish= root:packages:motofit:gencurvefit:cursorfinish
	Wave ydata = $(PopupWS_GetSelectionFullPath("gencurvefitpanel", "ydataWav_setVar_tab0"))
	if(!waveexists(ydata))
		DoAlert 0, "y wave doesn't seem to exist in gen_checkcursors"
		return 1
	endif		
	if(cmpstr(startORend,"pcsr(A)")==0 || cmpstr(startORend,"pcsr(B)")==0)
		return 0
	endif
	if(numtype(str2num(startORend)))
		DoAlert 0, "Numeric value required"
		return 1
	endif
	cursorstart = num2istr(str2num(cursorstart))
	cursorfinish = num2istr(str2num(cursorfinish))
			
	if(str2num(cursorfinish) == str2num(cursorstart) || str2num(cursorstart)>str2num(cursorfinish))
		Doalert 0,"the start cursor must be less than the end cursor"
		return 1
	endif
	if(str2num(cursorfinish)<0 || str2num(cursorstart)<0)
		Doalert 0, "the start and end points must not have negative point numbers"
		return 1
	endif
	if(str2num(cursorstart)>numpnts(ydata)-1 || str2num(cursorstart)>numpnts(ydata)-1)
		Doalert 0," the point number entered is greater than the number of points in the ywave"
		return 1
	endif
End

Function gen_defaultlims_buttonproc(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			Wave/t  gen_listwave = root:packages:motofit:gencurvefit:gen_listwave
			variable ii
			for(ii=0; ii<dimsize(gen_listwave,0);ii+=1)
				if(str2num(gen_listwave[ii][1])<0)
					gen_listwave[ii][3] = num2str(2*str2num(gen_listwave[ii][1]))
					gen_listwave[ii][4] = "0"
				else				
					gen_listwave[ii][3] = "0"
					gen_listwave[ii][4] =  num2str(2*str2num(gen_listwave[ii][1]))
				endif
			endfor
			break
	endswitch

	return 0
End

Function Gen_isSameWave(wav1,wav2)
	Wave/z wav1,wav2

	if(!equalwaves(wav1,wav2,-1) || cmpstr(getWavesdatafolder(wav1,2),getwavesdatafolder(wav2,2)))
		return 0
	endif	

	return 1
End

//filters for displaying waves in waveselectors
Function Gen_filter_ydataWav(aName,contents)
	String aName
	variable contents
	
	if(cmpstr(aName,"_none_")==0)
		return 1
	endif
	Wave/z aWav = $aName
	
	controlinfo/w=gencurvefitpanel fromtarget_tab0
	variable fromtarget = v_value
	
	if(Wavedims(aWav)==1 && numpnts(aWav)>0)
		if(fromtarget)
			checkdisplayed $aName
			if(V_flag)
				return 1
			else
				return 0
			endif
		else
			return 1
		endif
	else
		return 0
	endif
End
Function Gen_filter_xdataWav(aName,contents)	//this can be used for x,weight,mask
	String aName
	variable contents
	
	controlinfo/W = gencurvefitpanel FunctionStr_popup_tab0
	string fitfuncname = S_value
	variable nindvar =  GEN_numIndVars_func(fitfuncname)
	
	Wave/z aWav = $aName

	if(cmpstr(aName,"_calculated_")==0 && nindvar == 1)
		return 1
	endif

	controlinfo/w=gencurvefitpanel fromtarget_tab0
	variable fromtarget = v_value
			
	Wave/z ywav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "ydataWav_setvar_tab0")
	if(waveexists(ywav) && (Wavedims(aWav)!=nindvar || dimsize(aWav, 0) != numpnts(ywav) || gen_isSamewave(aWav, yWAV)))
		return 0
	else 
		if(fromtarget)
			checkdisplayed $aName
			if(V_flag)
				return 1
			else
				return 0
			endif
		else
			return 1
		endif
	endif
End

Function Gen_filter_dataWav(aName,contents)	//this can be used for weight,mask
	String aName
	variable contents

	Wave/z aWav = $aName
	if(cmpstr(aName,"_none_")==0)
		return 1
	endif

	controlinfo/w=gencurvefitpanel fromtarget_tab1
	variable fromtarget = v_value
	
	Wave/z ywav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "ydataWav_setvar_tab0")
	if(waveexists(ywav) && (Wavedims(aWav)!=1 || numpnts(aWav) != numpnts(ywav) ))
		return 0
	else 
		if(fromtarget)
			checkdisplayed $aName
			if(V_flag)
				return 1
			else
				return 0
			endif
		else
			return 1
		endif
	endif
End

Function Gen_filter_coefWav(aName,contents)
	String aName
	variable contents

	Wave/z aWav = $aName
	if(cmpstr(aName,"_new wave_")==0)
		return 1
	endif

	Wave/z ywav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "ydataWav_setvar_tab0")
	Wave/z xwav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "xdataWav_setvar_tab0")
	Wave/z weightWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "weightWav_setvar_tab1")
	Wave/z maskWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "maskWav_setvar_tab1")
	Wave/z resWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "resWav_setvar_tab3")
	Wave/z destWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "destWav_setvar_tab3")
	
	if(waveexists(ywav) && Gen_isSameWave(yWav,aWAV))
		return 0
	endif
	if(waveexists(xwav) && Gen_isSameWave(xWav,aWav))
		return 0
	endif
	if(waveexists(weightwav) && Gen_isSameWave(weightWav,aWav))
		return 0
	endif
	if(waveexists(maskwav) && Gen_isSameWave(maskWav,aWav))
		return 0
	endif
	if(waveexists(reswav) && Gen_isSameWave(resWav,aWav))
		return 0
	endif
	if(waveexists(destwav) && Gen_isSameWave(destWav,aWav))
		return 0
	endif
	if(wavedims(aWav)!=1 || numpnts(aWav)<1)
		return 0
	endif
	
	return 1
End

Function Gen_filter_resWav(aName,contents)
	String aName
	variable contents

	Wave/z aWav = $aName
	if(cmpstr(aName,"_none_")==0 || cmpstr(aName,"_auto wave_")==0)
		return 1
	endif

	Wave/z ywav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "ydataWav_setvar_tab0")
	Wave/z xwav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "xdataWav_setvar_tab0")
	Wave/z weightWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "weightWav_setvar_tab1")
	Wave/z maskWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "maskWav_setvar_tab1")
	Wave/z resWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "resWav_setvar_tab3")
	Wave/z destWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "destWav_setvar_tab3")
	Wave/z coefWave = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "coefWav_setvar_tab2")

	if(waveexists(coefwav) && Gen_isSameWave(coefWav,aWav))
		return 0
	endif
	if(waveexists(xwav) && Gen_isSameWave(xWav,aWav))
		return 0
	endif
	if(waveexists(weightwav) && Gen_isSameWave(weightWav,aWav))
		return 0
	endif
	if(waveexists(maskwav) && Gen_isSameWave(maskwav,aWav))
		return 0
	endif
	if(waveexists(destwav) && Gen_isSameWave(destwav,aWav))
		return 0
	endif
	if(waveexists(coefwav) && Gen_isSameWave(coefWav,aWav))
		return 0
	endif
	if(waveexists(ywav) && (numpnts(aWav)<1 || Wavedims(aWav)!=1 || numpnts(aWav) != numpnts(ywav) || gen_isSamewave(aWav, yWAV)))
		return 0
	else 
		return 1
	endif
End

Function Gen_filter_destWav(aName,contents)
	String aName
	variable contents

	Wave/z aWav = $aName
	if(cmpstr(aName,"_none_")==0 || cmpstr(aName,"_auto wave_")==0)
		return 1
	endif

	Wave/z ywav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "ydataWav_setvar_tab0")
	Wave/z xwav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "xdataWav_setvar_tab0")
	Wave/z weightWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "weightWav_setvar_tab1")
	Wave/z maskWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "maskWav_setvar_tab1")
	Wave/z resWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "resWav_setvar_tab3")
	WAVE/z coefWav = $PopupWS_GetSelectionFullPath("gencurvefitpanel", "coefWav_setvar_tab2")
	
	if(waveexists(xwav) && Gen_isSameWave(xWav,aWav))
		return 0
	endif
	if(waveexists(weightwav) && Gen_isSameWave(weightWav,aWav))
		return 0
	endif
	if(waveexists(maskwav) && Gen_isSameWave(maskwav,aWav))
		return 0
	endif
	if(waveexists(reswav) && Gen_isSameWave(resWav,aWav))
		return 0
	endif
	if(waveexists(coefwav) && Gen_isSameWave(coefWav,aWav))
		return 0
	endif
	if(waveexists(ywav) && (numpnts(aWav)<1 || Wavedims(aWav)!=1 || numpnts(aWav) != numpnts(ywav) || gen_isSamewave(aWav, yWAV)))
		return 0
	else 
		return 1
	endif
End

Function Gen_filter_limitsWav(aName,contents)
	String aName
	variable contents

	Wave/z aWav = $aName
	if(cmpstr(aName,"_from below_")==0)
		return 1
	endif

	WAVE/t gen_listwave = root:packages:motofit:gencurvefit:gen_listwave
	nvar/z numcoefs = root:packages:motofit:gencurvefit:numcoefs
	
	if(Wavedims(aWav)!=2 || dimsize(aWav,1)!=2 || dimsize(aWav,0)!=numcoefs)
		return 0
	else
		return 1
	endif 
End

Function Gen_rebuildPopups(event,wavepath,windowname,ctrlname)
	Variable event
	String wavepath
	String windowName
	String ctrlName
	
	string ctrllist = "xdataWav_setvar_tab0:_calculated_;weightWav_setvar_tab1:_none_;maskWav_setvar_tab1:_none_;destWav_setvar_tab3:_auto_;resWav_setvar_tab3:_none_;"
	ctrllist += "coefWav_setvar_tab2:_new wave_;limitsWav_setvar_tab2:_from below_"
	
	string filterlist = "xdataWav_setvar_tab0:gen_filter_xdataWav;weightWav_setvar_tab1:gen_filter_dataWav;maskWav_setvar_tab1:gen_filter_dataWav;destWav_setvar_tab3:gen_filter_destWav;"
	filterlist+= "resWav_setvar_tab3:gen_filter_resWav;coefWav_setvar_tab2:gen_filter_coefWav;limitsWav_setvar_tab2:gen_filter_limitsWav"
	
	variable ii
	string ctrl
	string ctrlWavStr,nameFilterProcStr
	
	strswitch(ctrlname)
		default:
			Wave/z ywave = $wavepath
			variable numpoints = numpnts(ywave)
			for(ii=0;ii<itemsinlist(ctrllist,";");ii+=1)
				ctrl = stringfromlist(ii,ctrllist,";")
				ctrl = stringfromlist(0,ctrl,":")
				ctrlWavStr = PopupWS_GetSelectionFullPath("gencurvefitpanel", ctrl)
					
				nameFilterProcStr = stringbykey(ctrl,filterlist)
				FUNCREF namefiltertemplate namefilterproc = $nameFilterProcStr
				if(!namefilterproc(ctrlWavStr,0))
					PopupWS_SetSelectionFullPath("gencurvefitpanel", ctrl, stringbykey(ctrl,ctrllist))
				endif
			endfor
			break
	
	endswitch

End

function gen_savestatus()

	string ctrllist = "ydataWav_setVAR_tab0:_none_;xdataWav_setvar_tab0:_calculated_;weightWav_setvar_tab1:_none_;maskWav_setvar_tab1:_none_;destWav_setvar_tab3:_auto_;resWav_setvar_tab3:_none_;"
	ctrllist += "coefWav_setvar_tab2:_new wave_;limitsWav_setvar_tab2:_from below_"
	svar saveStatus = root:packages:motofit:gencurvefit:saveStatus
	nvar lasttab = root:packages:motofit:gencurvefit:lasttab
	
	variable ii
	string ctrl
	for(ii=0;ii<itemsinlist(ctrllist);ii+=1)
		ctrl = stringfromlist(ii,ctrllist)
		ctrl = stringfromlist(0,ctrl,":")
		ctrllist = replacestringbykey(ctrl,ctrllist,PopupWS_GetSelectionFullPath("gencurvefitpanel", ctrl))
	endfor
	savestatus = ctrllist
	
	controlinfo/w=gencurvefitpanel tab0
	lasttab = V_Value
End

function gen_setstatus()
	string ctrllist = "ydataWav_setVAR_tab0:_none_;xdataWav_setvar_tab0:_calculated_;weightWav_setvar_tab1:_none_;maskWav_setvar_tab1:_none_;destWav_setvar_tab3:_auto_;resWav_setvar_tab3:_none_;"
	ctrllist += "coefWav_setvar_tab2:_new wave_;limitsWav_setvar_tab2:_from below_"
	svar saveStatus = root:packages:motofit:gencurvefit:saveStatus
	nvar lasttab = root:packages:motofit:gencurvefit:lasttab

	if(strlen(saveStatus)==0)
		savestatus = ctrllist
	endif
	variable ii
	string ctrl,val
	for(ii=0;ii<itemsinlist(ctrllist);ii+=1)
		ctrl = stringfromlist(ii,ctrllist)
		ctrl = stringfromlist(0,ctrl,":")
		val = stringbykey(ctrl,saveStatus)
		if(waveexists($val))
			PopupWS_SetSelectionFullPath("gencurvefitpanel", ctrl, val)
		endif
		if(cmpstr(ctrl,"coefWav_setvar_tab2")==0 && waveexists($val))
			Gen_expandnpars(numpnts($val))
			Gen_insertCoefs($val)
		endif
	endfor
	
	struct WMTabControlAction tca
	tca.eventcode=2
	tca.tab=lasttab		
	gen_tabcontrol(tca)	
	tabcontrol tab0 win=gencurvefitpanel,value = lasttab
End
	
Structure GEN_optimisation
Wave GEN_parwave		//what are the initial parameters?
String GEN_parwavename

String GEN_holdstring		//the holdstring for holding parameters
Variable GEN_holdBits	//an integer representation of holdstring
variable GEN_numvarparams

Wave GEN_limits			//what are the limits on your parameters?
Wave GEN_b			//what is the best fit so far?
Funcref GEN_allatoncefitfunc fin	//what fit function are you going to use?
Funcref GEN_fitfunc fan
Variable GEN_popsize
Variable k_m
Wave GEN_trial
Wave GEN_yy
string GEN_ywavename
Wave GEN_xx
Wave GEN_ee
String GEN_ywaveDF
String GEN_xwaveDF
String GEN_ewaveDF
String GEN_parwaveDF	
Wave GEN_yybestfit
Variable GEN_generations	
Variable GEN_recombination
Wave GEN_chi2matrix
Wave GEN_populationvector
Wave GEN_bprime
Wave GEN_pvector
variable GEN_currentpvector
variable GEN_chi2best
variable GEN_whattype
Variable GEN_V_fittol
Wave GEN_parnumber
String GEN_callfolder
variable GEN_quiet //don't print stuff in history area
Endstructure

Function/S GEN_holdallstring(numvarparams)
	variable numvarparams
	variable ii
	string str=""
	for(ii=0 ; ii<numvarparams ; ii+=1)
		str+="1"
	endfor
	return str
End

Static Function GEN_searchparams(gen)
	Struct GEN_optimisation &gen
	Variable GEN_popsize=20
	Variable k_m=0.7
	Variable GEN_recombination=0.5
	Variable GEN_generations=100
	Variable GEN_V_fittol=0.05
	prompt k_m,"mutation constant, e.g.0.7"
	prompt GEN_recombination,"enter the recombination constant"
	prompt GEN_generations,"how many generations do you want to use?"
	prompt GEN_popsize,"enter the population size multiplier e.g. 10"
	prompt GEN_V_fittol,"enter the fractional tolerance to stop fit (e.g. 0.05%=0.0005)"
	Doprompt "Set up genetic optimisation",GEN_generations,k_m,GEN_popsize,GEN_recombination,GEN_V_fittol
	String CDF=gen.GEN_callfolder
	if(V_flag==1)
		setdatafolder CDF
		ABORT
	endif
	gen.GEN_generations=GEN_generations
	gen.GEN_popsize=GEN_popsize
	gen.k_m=k_m
	gen.GEN_recombination=GEN_recombination
	gen.GEN_V_fittol=GEN_V_fittol
End




Function GEN_allatoncefitfunc(coefficients,ydata,xdata)
	//the function template for an all at once fitfunction
	Wave coefficients,ydata,xdata
End

Function GEN_fitfunc(coefficients,xx)
	//the function template for a normal fit function
	Wave coefficients
	variable xx
End

Function GEN_setlimitsforGENcurvefit(coefs, holdstring [, limits, paramdescription])
	Wave coefs
	string holdstring
	wave/z limits
	wave/t/z paramdescription

	string cDF = getdatafolder(1)
	//sets the limits as 	root:packages:motofit:old_genoptimise:GENcurvefitlimits

	newdatafolder/o root:packages
	newdatafolder/o root:packages:motofit
	newdatafolder/o/s root:packages:motofit:old_genoptimise

	NVAR/z iterations, popsize, recomb, k_m, fittol
	variable ii, numbeingvaried=0, paramdescriptionoffset


	if(!NVAR_exists(iterations))
		variable/g iterations = 100
	endif
	if(!NVAR_exists(popsize))
		variable/g popsize = 10
	endif
	if(!NVAR_exists(recomb))
		variable/g recomb = 0.5
	endif
	if(!NVAR_exists(k_m))
		variable/g k_m = 0.7
	endif
	if(!NVAR_exists(fittol))
		variable/g fittol = 0.05
	endif
	if(!paramisdefault(paramdescription) && waveexists(paramdescription) && dimsize(paramdescription, 0) == numpnts(coefs))
		paramdescriptionoffset = 1
	endif
	
	//work out number being held.
	make/o/n = 0 thosebeingvaried
	for(ii = 0 ; ii < strlen(holdstring) ; ii+=1)
		if(stringmatch(holdstring[ii], "0"))
			redimension/n=(dimsize(thosebeingvaried,0) + 1) thosebeingvaried
			thosebeingvaried[numpnts(thosebeingvaried) - 1] = ii
			numbeingvaried +=1
		endif
	endfor
	
	make/o/n=(numbeingvaried, 4 + paramdescriptionoffset) limitsdialog_selwave = 0
	make/o/t/n=(numbeingvaried, 4+ paramdescriptionoffset) limitsdialog_listwave
	setdimlabel 1, 0, Parameter, limitsdialog_listwave
	if(paramdescriptionoffset)
		setdimlabel 1, 1, description, limitsdialog_listwave	
	endif
	setdimlabel 1, 1+ paramdescriptionoffset, coef, limitsdialog_listwave
	setdimlabel 1, 2+ paramdescriptionoffset, lower_lim, limitsdialog_listwave
	setdimlabel 1, 3+ paramdescriptionoffset, upper_lim, limitsdialog_listwave
	
	limitsdialog_listwave[][0] = num2istr(thosebeingvaried[p])
	if(paramdescriptionoffset)
		limitsdialog_listwave[][1] = paramdescription[thosebeingvaried[p]]	
	endif
	limitsdialog_listwave[][1+ paramdescriptionoffset] = num2str(coefs[thosebeingvaried[p]])
	limitsdialog_selwave[][2+ paramdescriptionoffset] = 2
	limitsdialog_selwave[][3+ paramdescriptionoffset] = 2	
	
	//limits for those being varied
	Wave/z limitsForThoseBeingVaried = root:packages:motofit:old_genoptimise:limitsForThoseBeingVaried
	if(!waveexists(limitsForThoseBeingVaried))
		make/o/n=(0, 2) limitsForThoseBeingVaried = 0 
	endif
	
	if(paramisdefault(limits) || !waveexists(limits))
		Wave/z limits = root:packages:motofit:old_genoptimise:GENcurvefitlimits
		if(!waveexists(limits) || dimsize(limits, 0) != dimsize(coefs, 0))
			make/o/n=(dimsize(coefs, 0), 2)/d root:packages:motofit:old_genoptimise:GENcurvefitlimits = 0
			Wave/z limits =root:packages:motofit:old_genoptimise:GENcurvefitlimits
			//				limits[][0] = coefs[thosebeingvaried[p]] < 0 ? 2* coefs[thosebeingvaried[p]] : 0
			//				limits[][1] = coefs[thosebeingvaried[p]] > 0 ? 2* coefs[thosebeingvaried[p]] : 0		
		endif
		
		//thosebeingvaried may be the same as previous
		if(dimsize(limitsForThoseBeingVaried, 0) == numbeingvaried)
			doalert 1, "Do you want to use the previous limits?"
			if(V_flag == 2)
				limitsforthosebeingvaried[][0] = coefs[thosebeingvaried[p]] < 0 ? 2* coefs[thosebeingvaried[p]] : 0
				limitsforthosebeingvaried[][1] = coefs[thosebeingvaried[p]] > 0 ? 2* coefs[thosebeingvaried[p]] : 0
			endif
		else
			redimension/n=(numbeingvaried,-1) limitsForThoseBeingVaried
			limitsforthosebeingvaried[][0] = coefs[thosebeingvaried[p]] < 0 ? 2* coefs[thosebeingvaried[p]] : 0
			limitsforthosebeingvaried[][1] = coefs[thosebeingvaried[p]] > 0 ? 2* coefs[thosebeingvaried[p]] : 0
		endif	
	else
		redimension/n=(numbeingvaried,-1) limitsForThoseBeingVaried
		limitsforthosebeingvaried[][0] = limits[thosebeingvaried[p]][0]
		limitsforthosebeingvaried[][1] = limits[thosebeingvaried[p]] [1]
	endif

	limitsdialog_listwave[][2+ paramdescriptionoffset] = num2str(limitsforthosebeingvaried[p][0])
	limitsdialog_listwave[][3+ paramdescriptionoffset] = num2str(limitsforthosebeingvaried[p][1])
	
	do
		variable thoseOK = 0
		NewPanel /W=(445,64,774,445) as "Gencurvefit limits"
		Dowindow/c GCF_dialog
		ListBox list0,pos={11,130},size={307,167}, win=GCF_dialog
		ListBox list0,listWave=root:packages:motofit:old_genoptimise:limitsdialog_listwave, win=GCF_dialog
		ListBox list0,selWave=root:packages:motofit:old_genoptimise:limitsdialog_selwave, win=GCF_dialog
		if(!paramisdefault(paramdescription))
			listbox list0, win = GCF_dialog, widths = {15, 25, 20, 20, 20}
		endif
		SetVariable setvar0,pos={11,8},size={216,19},title="iterations",fSize=12, win=GCF_dialog
		SetVariable setvar0,limits={1,inf,10},value= root:packages:motofit:old_genoptimise:iterations, win=GCF_dialog
		SetVariable setvar1,pos={12,32},size={215,19},title="population size",fSize=12, win=GCF_dialog
		SetVariable setvar1,limits={1,inf,4},value= root:packages:motofit:old_genoptimise:popsize, win=GCF_dialog
		SetVariable setvar2,pos={12,56},size={216,19},title="mutation constant",fSize=12, win=GCF_dialog
		SetVariable setvar2,limits={0,1,0.05},value=root:packages:motofit:old_genoptimise:k_m, win=GCF_dialog
		SetVariable setvar3,pos={12,80},size={216,19},title="recombination constant", win=GCF_dialog
		SetVariable setvar3,fSize=12, win=GCF_dialog
		SetVariable setvar3,limits={0,1,0.05},value= root:packages:motofit:old_genoptimise:recomb, win=GCF_dialog
		SetVariable setvar4,pos={12,104},size={215,19},title="fit tolerance",fSize=12, win=GCF_dialog
		SetVariable setvar4,limits={1e-7,0.1,0.001},value= root:packages:motofit:old_genoptimise:fittol, win=GCF_dialog
		Button button0,pos={30,310},size={266,25},proc=GCF_dialogProc,title="\f04D\f00o Fit",  fColor=(32768,54615,65535),win=GCF_dialog
		Button button1,pos={251, 103},size={45, 20},proc=GCF_dialogProc,title="default", fsize=9, win=GCF_dialog
		Button button2,pos={30,337},size={266,25},proc=GCF_dialogProc,title="\f04C\f00ancel", win=GCF_dialog
		Setwindow GCF_dialog, hook(GCF_dialog)=GCF_dialog_hook, userdata(continue) = "Do Fit"
		PauseForUser GCF_dialog

		NVAR GCF_continue = root:packages:motofit:old_genoptimise:GCF_continue
		if(!GCF_continue)
			setdatafolder $cDF
			return 1
		endif
		limitsforthosebeingvaried[][0] = str2num(limitsdialog_listwave[p][2+ paramdescriptionoffset])
		limitsforthosebeingvaried[][1] = str2num(limitsdialog_listwave[p][3+ paramdescriptionoffset])
	
		for(ii=0 ; ii < numbeingvaried ; ii+=1)
			limits[thosebeingvaried[ii]][0] = limitsforthosebeingvaried[ii][0] 
			limits[thosebeingvaried[ii]][1] = limitsforthosebeingvaried[ii][1]
			if(limits[thosebeingvaried[ii]][0] <= limits[thosebeingvaried[ii]][1])
				thoseOK+=1
			endif
		endfor
		if(thoseOK != numbeingvaried)
			Doalert 0, "Lower limit needs to be less than upper limit"
		endif
		
	while (thoseOK != numbeingvaried)
	duplicate/o limits, GENcurvefitlimits
	setdatafolder $cDF
	return 0
End

Function GCF_dialog_hook(s)
	STRUCT WMWinHookStruct &s

	Variable hookResult = 0

	switch(s.eventCode)
		case 0:				// Activate
			// Handle activate
			break

		case 1:				// Deactivate
			// Handle deactivate
			break
		case 11:
			switch (s.keycode)
				case 13:		//enter
					//do the fit.

					NVAR GCF_continue = root:packages:motofit:old_genoptimise:GCF_continue
					string option = GetUserdata("GCF_dialog", "", "continue")
					strswitch(option)
						case "cancel":
							GCF_continue = 0
							break
						case "Do Fit":
							GCF_continue = 1
							break
					endswitch
					dowindow/k $s.winname

					break
				case 99: 		//c for cancel
					Button button2, fColor=(32768,54615,65535),win=GCF_dialog
					Button button0, fColor=(65535,65535,65535),win=GCF_dialog
					SetWindow GCF_dialog, userdata(continue) = "cancel"
				break
				case 100:		//d for do fit
					Button button0, fColor=(32768,54615,65535),win=GCF_dialog
					Button button2, fColor=(65535,65535,65535),win=GCF_dialog
					SetWindow GCF_dialog, userdata(continue) = "Do Fit"
				break
			endswitch	
		// And so on . . .
	endswitch
	
	return hookResult		// 0 if nothing done, else 1
End


Function GCF_dialogProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	Wave/T listwave = root:packages:motofit:old_genoptimise:limitsdialog_listwave
	variable/g root:packages:motofit:old_genoptimise:GCF_continue
	NVAR GCF_continue = root:packages:motofit:old_genoptimise:GCF_continue
	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			strswitch(ba.ctrlname)
				case "button0":
					dowindow/k $ba.win
					GCF_continue = 1
					break
				case "button1":
					listwave[][2] = selectstring(str2num(listwave[p][1]) > 0, num2str(2 * str2num(listwave[p][1])), "0")
					listwave[][3] = selectstring(str2num(listwave[p][1]) < 0, num2str(2 * str2num(listwave[p][1])), "0")
					break
				case "button2":  //Cancel
					dowindow/k $ba.win
					GCF_continue = 0
					break
			endswitch
			break
	endswitch

	return 0
End

ThreadSafe Function motoMCWorkerFunc1()
	do
		do
			DFRef tdf  =ThreadGroupGetDFR(0,1000)	
			if(!Datafolderrefstatus(tdf))
				if( GetRTError(2) )	// New in 6.2 to allow this distinction:
//					Print "worker closing down due to group release"
				else
//					Print "worker thread still waiting for input queue"
				endif
			else
				break
			endif
		while(1)
		SetDataFolder tdf

		Wave/z wtemp, yytemp, xxtemp, eetemp, holdwavetemp, limitstemp
		SVAR fntemp
		NVAR iterstemp
		NVAR popsizetemp  
		NVAR recombtemp  
		NVAR k_mtemp  
		NVAR fittoltemp 
		NVAR fakeweighttemp
		NVAR cursorA
		NVAR cursorB
		variable/g V_Fiterror
		duplicate/free yytemp, fitoutput
	 	
		if(fakeweighttemp)
			yytemp = yytemp + gnoise(eetemp)
			Gencurvefit/d=fitoutput/q/n/hold=holdwavetemp/X=xxtemp/K={iterstemp, popsizetemp, k_mtemp, recombtemp}/TOL=(fittoltemp) $fntemp, yytemp, wtemp, "", limitstemp
		else
			Gencurvefit/d=fitoutput/MC/q/n/hold=holdwavetemp/X=xxtemp/I=1/W=eetemp/K={iterstemp, popsizetemp, k_mtemp, recombtemp}/TOL=(fittoltemp) $fntemp, yytemp, wtemp, "", limitstemp
		endif		
		//		print V_chisq,V_fitIters, V_fiterror
		Setdatafolder ::
		NewDataFolder/S outDF
		
		make/n=(dimsize(wtemp, 0))/d W_output
		W_output = wtemp
		Waveclear wtemp, yytemp, xxtemp, eetemp, holdwavetemp, limitstemp, W_output
		ThreadGroupPutDF 0,:		
		KillDataFolder tdf		// We are done with the input data folder
	while(1)

	return 0
End

Function gen_updatefunc(pars, pop, costmap, updatetime)
	Wave pars, pop, costmap
	variable updatetime

	NVAR/z finishedfit = root:packages:tempfittingfolder:finishedfit

	if( (NVAR_exists(finishedfit) && finishedfit == 1) || !datafolderexists("root:packages:tempfittingfolder"))
		newdatafolder/o root:packages
		newdatafolder/o root:packages:tempfittingfolder
		variable/g root:packages:tempfittingfolder:finishedfit = 0
		make/n=0/d/o root:packages:tempfittingfolder:chi2trend
		make/n=(dimsize(pop, 0))/d/o root:packages:tempfittingfolder:spread, root:packages:tempfittingfolder:spreadSD
		make/n=(dimsize(pop, 1), dimsize(pop, 0))/d/o root:packages:tempfittingfolder:temppop
	
		Wave chi2trend = root:packages:tempfittingfolder:chi2trend
		Wave spread = root:packages:tempfittingfolder:spread
		Wave spreadSD = root:packages:tempfittingfolder:spreadSD
		Wave temppop = root:packages:tempfittingfolder:temppop

		Display /W=(35,44,726,515)/K=1 spread
		AppendToGraph/L=vL/T chi2trend
		ModifyGraph mode(spread)=2
		ModifyGraph log(vL)=1
		ModifyGraph mode(spread)=3
		ModifyGraph marker(spread)=8
		ModifyGraph lSize(chi2trend)=2
	
		ModifyGraph lblPos(left)=60
		ModifyGraph freePos(vL)={0,top}
		ModifyGraph axisEnab(left)={0,0.6}
		ModifyGraph axisEnab(vL)={0.63,1}
		Label bottom "fitted parameter number"
		Label vL "chi\\S2"
		ModifyGraph noLabel(left)=2
		SetAxis left 0,1
		ErrorBars spread Y,wave=(spreadSD,spreadSD)
	endif

	switch(updatetime)
		case 8:
			Wave chi2trend = root:packages:tempfittingfolder:chi2trend
			Wave spread = root:packages:tempfittingfolder:spread
			Wave spreadSD = root:packages:tempfittingfolder:spreadSD
			Wave temppop = root:packages:tempfittingfolder:temppop
			
			redimension/n=(numpnts(chi2trend) + 1) chi2trend
			chi2trend[numpnts(chi2trend) - 1] = costmap[0]
		
			multithread temppop[][] = pop[q][p]
			spread[] = mean(temppop, dimsize(temppop, 0) * p, dimsize(temppop, 0) * (p + 1) - 1)
			spreadSD[] = sqrt(variance(temppop, dimsize(temppop, 0) * p, dimsize(temppop, 0) * (p + 1) - 1))
			Doupdate
			break
		case 16:
			finishedfit = 1	
			break
	endswitch
		
	return 0
End



Function Moto_montecarlo(fn, w, yy, xx, ee, holdstring, Iters,[limits, cursA, cursB, outf, fakeweight, verbose, otherDataDF])
	String fn
	Wave w, yy, xx, ee
	string holdstring
	Wave limits
	variable Iters, cursA, cursB
	string outf
	variable fakeweight //fake weight means that you know the weights, but aren't prepared to weight the data as well.
	variable verbose
	string otherDataDF	//folder containing information relevant to the fit. This cannot be the current datafolder, or the ancestor of the current datafolder
	
	string cDF = getdatafolder(1)
	string allwaves="", alltilewaves = "", funcinfo=""
	variable ii,jj,kk, summ, err = 0, fileID, nthreads= ThreadProcessorCount
	variable timed = datetime
	
	newdatafolder/o root:packages
	newdatafolder/o root:packages:motofit
	newdatafolder/o root:packages:motofit:old_genoptimise
	newdatafolder/o root:packages:WMScatterPlotMatrixPackage
	try
		//get limits wave, also sets default parameters.
		if(paramisdefault(limits))
			GEN_setlimitsforGENcurvefit(w,holdstring)
			Wave limits = root:packages:motofit:old_genoptimise:GENcurvefitlimits	
		endif

		NVAR  iterations = root:packages:motofit:old_genoptimise:iterations
		NVAR  popsize = root:packages:motofit:old_genoptimise:popsize
		NVAR recomb =  root:packages:motofit:old_genoptimise:recomb
		NVAR k_m =  root:packages:motofit:old_genoptimise:k_m
		NVAR fittol = root:packages:motofit:old_genoptimise:fittol
	
		make/n=(strlen(holdstring))/I/U/free holdwave
		holdwave[] = str2num(holdstring[p])
		
		//take care of cursors
		if(paramisdefault(cursA))
			cursA = 0
		endif
		if(paramisdefault(cursB))
			cursB = dimsize(yy, 0)-1
		endif
		
		if(paramisdefault(verbose))
			verbose = 1
		endif
	
		make/n=(iters, dimsize(w, 0))/o/d M_Montecarlo = NaN
		if(verbose)
			allwaves = make2DScatter_plot_matrix(M_monteCarlo, holdstring)
			for(ii = 0 ; ii < itemsinlist(allwaves) ; ii+=1)
				alltilewaves += stringfromlist(ii, allwaves) + "_HIST;"
			endfor
			maketileplot(alltilewaves)
		endif
			
		//see if the fitfunction is threadsafe
		funcinfo = functioninfo(fn)
		if(stringmatch(stringbykey("THREADSAFE", funcinfo), "NO"))
			if(!paramisdefault(otherDataDF))
				Setdatafolder $otherDataDF
			endif
			
			duplicate/free w, output
			duplicate/free yy, yytemp, tempcorefinement
			
			for(ii = 0 ; ii < iters ; ii += 1)
				if(fakeweight)
					yytemp = yytemp + gnoise(ee)
					Gencurvefit/d=tempcorefinement/q/n/hold=holdwave/X=xx/K={iterations, popsize, k_m, recomb}/TOL=(fittol) $fn, yytemp, output, "", limits
				else
					Gencurvefit/d=tempcorefinement/MC/q/n/hold=holdwave/X=xx/I=1/W=ee/K={iterations, popsize, k_m, recomb}/TOL=(fittol) $fn, yy, output, "", limits
				endif	
				print "done", ii, "of", iters
	 			M_Montecarlo[ii][] = output[q]
	 			
	 			if(verbose)
					allwaves = make2DScatter_plot_matrix(M_monteCarlo, holdstring)
				endif
				doupdate
			endfor
		
		else		//function is threadsafe
			variable/G tgID= ThreadGroupCreate(nthreads)
			for(ii =0 ; ii < nthreads ; ii += 1)
				ThreadStart tgID, ii, motoMCWorkerFunc1()
			endfor
			
			for(ii = 0 ; ii < iters ; ii += 1)
				if(paramisdefault(otherDataDF))
					NewDataFolder/S $(cdf + "forThread")
				else
					DuplicateDataFolder $(otherDataDF), $(cdf+"forThread")
					setdatafolder $(cdf+"forThread")
				endif
				duplicate w, wtemp
				duplicate yy, yytemp
				duplicate xx, xxtemp
				duplicate ee, eetemp
				duplicate holdwave, holdwavetemp
				duplicate limits, limitstemp
				string/g fntemp = fn
				variable/g iterstemp = iterations
				variable/g popsizetemp = popsize
				variable/g recombtemp = recomb
				variable/g k_mtemp = k_m
				variable/g fittoltemp = fittol
				variable/g fakeweighttemp = fakeweight
				variable/g cursorA = cursA
				variable/g cursorB = cursB
				
				waveclear wtemp, yytemp, xxtemp, eetemp, holdwavetemp, limitstemp
				ThreadGroupPutDF tgID, :
			endfor
			for(ii = 0 ; ii < iters ; ii += 1)
				do
					DFREF dfr= ThreadGroupGetDFR(tgID,1000)	// Get results in free data folder
					if ( DatafolderRefStatus(dfr) == 0 )
						//Print "Main still waiting for worker thread results"
					else
						break
					endif
				while(1)
				Wave/sdfr=dfr W_output
				if(verbose)
					print "Completed", ii, "iterations, time taken is:", datetime - timed, "."
				endif
				M_Montecarlo[ii][] = W_output[q]
				
				// The next two statements are not really needed as the same action
				// will happen the next time through the loop or, for the last iteration,
				// when this function returns.
				WAVEClear W_output
				KillDataFolder dfr
				if(verbose)
					//update the montecarlo scatterplot
					allwaves = make2DScatter_plot_matrix(M_monteCarlo, holdstring)
				endif
				Doupdate
			endfor
		endif

		//now work out correlation matrix and errors.
		//see Heinrich et al., Langmuir, 25(7), 4219-4229
		make/n=(dimsize(w, 0))/d/o W_sigma, means, stdevs
		make/n=(dimsize(w,0), dimsize(w, 0))/o/d M_correlation
		M_correlation = NaN
	
		iters = dimsize(M_montecarlo, 0)
		
		for(ii = 0 ; ii<dimsize(w, 0) ; ii+=1)
			make/o/d/n=(iters) goes
			goes = M_montecarlo[p][ii]
			Wavestats/M=2/q/w goes
			Wave M_wavestats
			W_sigma[ii] =  M_wavestats[4]
			w[ii] = M_wavestats[3]
			means[ii] = M_wavestats[3]
			stdevs[ii] = M_wavestats[4]
			if(stringmatch(holdstring[ii], "1"))
				W_sigma[ii] = NaN
			endif
		endfor
		for(ii=0 ; ii< dimsize(w, 0) ; ii+=1)
			for(jj= ii ; jj<dimsize(w,0) ; jj+=1)
				if(ii==jj || stringmatch(holdstring[ii], "1") || stringmatch(holdstring[jj], "1"))
					M_correlation[ii][jj]=NaN
				else			
					summ = 0
					for(kk = 0 ; kk < Iters ; kk+=1)
						summ += (M_montecarlo[kk][ii]-means[ii])*(M_montecarlo[kk][jj]-means[jj]) 
					endfor
					M_correlation[ii][jj] = summ / (Iters-1) / (stdevs[ii] * stdevs[jj])
				endif  
				M_correlation[jj][ii] = M_correlation[ii][jj]
			endfor
		endfor

	catch
		err = 1	
	endtry
	
	// This terminates the MyWorkerFunc by setting an abort flag
	Variable tstatus= ThreadGroupRelease(tgID)
	if( tstatus == -2 )
		Print "Thread would not quit normally, had to force kill it. Restart Igor."
	endif
		
	if(!err)
		SPM_FreeAxisPlotMatrix(allWaves, 21, 2, 1, MarkerSize=1)
	endif
	
	killwaves/z M_wavestats, goes, means, stdevs, fit_y_montecarlo
	
	setdatafolder cDF
	return err
End

Function/wave M_montecarloStatistics(M_monteCarlo)
	Wave M_montecarlo
	variable ii
	make/free/d/n=(dimsize(M_Montecarlo, 1), 2) M_montecarlostats
	make/d/free/n=(dimsize(M_montecarlo, 0)) vals

	for(ii = 0 ; ii < dimsize(M_Montecarlo, 1) ; ii += 1)
		vals[] = M_Montecarlo[p][ii]
		wavestats/q vals
		M_Montecarlostats[ii][0] = V_avg
		M_Montecarlostats[ii][1] = V_sdev	
	endfor
	return M_Montecarlostats
End

Static Function maketileplot(listofwaves)
	string listofwaves
	Display /K=1/W=(35,44,535,435)
	variable ii, frac, fracgrout
	frac = 1/itemsinlist(listofwaves)
	fracgrout = frac * 0.95
	for(ii = 0 ; ii < itemsinlist(listofwaves) ; ii+=1)
		appendtograph /B=$("B" + num2istr(ii))/L=$("L"+num2istr(ii)) $(stringfromlist(ii, listofwaves))
		modifygraph nolabel($("L"+num2istr(ii))) = 2
		ModifyGraph standoff($("L"+num2istr(ii)))=0, standoff($("B"+num2istr(ii)))=0
		ModifyGraph freePos($("B" + num2istr(ii)))={0,$("L" + num2istr(ii))}
		ModifyGraph axisEnab($("L" + num2istr(ii)))={ii * frac,(ii+1) * fracgrout}
		ModifyGraph freePos($("L"+num2istr(ii)))={0,$("B" + num2istr(ii))}
	endfor
ENd

Function/t make2DScatter_plot_matrix(M_monteCarlo, holdstring)
	Wave M_montecarlo
	string holdstring

	variable ii, jj
	string cDF = getdatafolder(1)
	string allWaves = ""

	newdatafolder/o root:packages
	newdatafolder/o root:packages:Motofit
	newdatafolder/o root:packages:Motofit:gencurvefit

	setdatafolder root:packages:Motofit:gencurvefit

	try
		for(ii = 0 ; ii < dimsize(M_montecarlo, 1) ; ii += 1)
			if(stringmatch(holdstring[ii], "0"))
				make/n=(dimsize(M_montecarlo, 0))/o/y=(wavetype(M_montecarlo)) $("MC_" + num2istr(ii)), $("MC_" + num2istr(ii) + "_HIST")
				Wave M_montecarloI = $("MC_" + num2istr(ii))
				Wave M_montecarloHIST = $("MC_" + num2istr(ii) + "_HIST")
				M_montecarloI[] = M_montecarlo[p][ii]
				allWaves += "root:packages:Motofit:gencurvefit:MC_" + num2istr(ii) + ";"
				Histogram/P/C/B=3 $("MC_" + num2istr(ii)), $("MC_" + num2istr(ii) + "_HIST")
			endif
		endfor
	catch
		setdatafolder $cDF
		abort
	endtry

	setdatafolder $cDF
	return allwaves
End

Function gen_synthMCfmCovar(coefs, M_covar, holdstring, howmany)
	wave coefs, m_covar
	string holdstring
	variable howmany

	variable varying = 0, ii, whichcol
	duplicate/free coefs, tempcoefs
	duplicate/free M_covar, tempM_covar

	varying = strlen(holdstring)
	for(ii = dimsize(coefs, 0)-1 ; ii >= 0 ; ii-=1)
		if(str2num(holdstring[ii]))
			deletepoints/M=0 ii, 1, tempcoefs, tempM_covar
			deletepoints/M=1 ii, 1, tempM_covar
			varying -=1
		endif
	endfor

	//create some gaussian noise
	make /free/d/n=(varying, howmany) noises = gnoise(1., 2)
	//and create the correlated noise from the covariance matrix.
	matrixop/free correlatedNoises = (chol(tempm_covar) x noises)^t

	make/n=(howmany, dimsize(coefs, 0))/d/o M_montecarlo
	Multithread M_montecarlo[][] = coefs[q]

	//now add the correlated noise back to the parameters
	whichcol = dimsize(correlatedNoises, 1) - 1
	for(ii = dimsize(coefs, 0)-1 ; ii >= 0 ; ii-=1)
		if(!str2num(holdstring[ii]))
			M_montecarlo[][ii] += correlatedNoises[p][whichCol]
			whichCol -= 1
		endif
	endfor

End



Function GEN_classifyN(values)
	Wave values
	//gets some stats about the convergence of an Monte Carlo run
	variable ii
	make/n=(dimsize(values, 0), 2)/o/d stats = 0
	make/n=100/free/d histo = 0

	for(ii = 1 ; ii < dimsize(values, 0) ; ii+=1)
		make/d/n=(ii + 1)/free subset_values
		multithread subset_values[] = values[p]
		variable V_fitoptions=4
		histo = 0
		Histogram/P/C/B=3 subset_values, histo

		CurveFit/NTHR=0/q/n/M=2/W=0 gauss histo
		Wave W_coef
		stats[ii][0] = W_coef[2]
		stats[ii][1] = W_coef[3]
		if(!mod(ii, 100))
			print ii
		endif
	endfor

End

Function gen_gcm(M_covar)
	Wave M_covar
	Duplicate/o M_Covar, M_correlation	 // You can use any name instead of CorMat
	M_correlation = M_Covar[p][q]/sqrt(M_Covar[p][p]*M_Covar[q][q])
End

Function gen_Chi2_guess(fitfuncstr, coefs, ywave, xwave, ewave[, cursA, cursB])
	//return the CHI2 value for an AAO fit function
	string fitfuncstr
	wave coefs,  ywave, xwave, ewave
	variable cursA, cursB
	variable v_fitoptions=4
	if(paramisdefault(cursA) || paramisdefault(cursB))
		cursA = 0
		cursB = dimsize(ywave, 0) - 1
	endif
	FuncFit/n/q/o $fitfuncstr coefs  ywave[cursA, cursB] /X=xwave /W=ewave /I=1
	return V_chisq
	
End