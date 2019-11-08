#pragma rtGlobals=3		// Use modern global access method and strict wave access.

//Todos

//	- Line size adjust
//	- Arbitrary constraints
//		- Display 'Kn' for each parameter
//	- Log file and history
//	- Fine slider adjustment (depending on zoom)

//Bug fixes
//	- Prevent the generation of unused fit curves.

//Function
//	- Slider 
//	- BG subtraction
//	- Peak select
//	- Copy Guess values
//	- Default constraint
//	- use default constraint and edit
//	- Peak library and import peak library from a .txt file
//	- Colour changes /w hold or guess out of range
//	- Save/Load guesses
//	- Fit and show fit results
//	- Display last fit result
//	- BG adjust and record
//	- Export fit results
//	- Check for duplicated new data W name
//		- Allow user to change data W name
//	- Peak area report

//Bug fixed
//	- Users can now click cancel for the load new data

//USE OFTEN
Function SLP_MGFit_UseOften()
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
	Variable DataRow=V_Value
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
	Variable PeakRow=V_Value
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Wave SLP_MGFit_DataListSelW=root:Package_SLP:MGFit:SLP_MGFit_DataListSelW
	
	Wave /T SLP_MGFit_ConstraintDisplayW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW
	Wave  SLP_MGFit_ConstraintDisplaySelW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplaySelW
	Wave /T SLP_MGFit_DefaultConstraintW=root:Package_SLP:MGFit:SLP_MGFit_DefaultConstraintW
	Wave SLP_MGFit_DefaultConstraintSelW=root:Package_SLP:MGFit:SLP_MGFit_DefaultConstraintSelW
	Wave /T SLP_MGFit_PeakLibraryW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibraryW
	Wave SLP_MGFit_PeakLibrarySelW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibrarySelW
	
	String DataName=SLP_MGFit_DataListW[DataRow][0]
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	
	String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
	String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
	String ConstraintW_FName=DataLocation+"ConstraintW"
	String HilightW_FName=DataLocation+"HilightW"
	String CoefGuessM_FName=DataLocation+"CoefGuessM" //Use for fitting
	String ConstraintV_FName=DataLocation+"ConstraintV" //Use for fitting

	String DataW_FName=DataLocation+DataName	//Raw data
	String BgW_FName=DataW_FName+"_BG"		//Background
	String Data_nBGW_FName=DataW_FName+"_nBG"	//Data after BG subtraction
	String Data_GssW_FName=DataW_FName+"_Gss"	//Guess curve
	
	variable i=0
	String Data_Comp_FName=DataW_FName+"_p"+num2str(i)	//Individual GLA component
End


Menu "SLPA"
	"Multi-GLAs Fit..",SLP_MGFit_CreatePanel()
End

Function SLP_MGFit_CreatePanel() : Panel
	Dowindow /K SLP_MultiGLAFit_01
	PauseUpdate; Silent 1		// building window...
	NewPanel /W=(45,45,1150,670) /N=SLP_MultiGLAFit_01
	
	SetDrawLayer UserBack
	SetDrawEnv fsize= 14
	DrawText 10,25,"SLP Multi GLA Peaks Fitting Panel"
	DrawText 380,30,"Parameter Guess and Results"
	DrawText 915,25,"Constraints and Hold"
	DrawText 715,205,"Data list"
	DrawText 380,280,"Peak library"
	
	DrawText 427,508,"Peak area"
	DrawText 379,570,"Area under curve"
	DrawText 915,194,"Data list"
	SetDrawEnv fsize= 10
	DrawText 452,522,"Guess"
	SetDrawEnv fsize= 10
	DrawText 468,538,"Fit"
	//SetVariable Setvar_N_Peaks,pos={378,13},size={170,15},title="Number of GLA peak(s)",noEdit=1
	
	NVAR GDataRow=root:Package_SLP:MGFit:GDataRow
	
	Variable a=30,b=60,c=90
	ListBox ListBox_PeakParam,pos={380,33},size={530,215},ListWave=root:Package_SLP:MGFit:SLP_MGFit_iCoefGuessDisplayW ,SelWave=root:Package_SLP:MGFit:SLP_MGFit_iCoefGuessDisplaySelW
	ListBox ListBox_PeakParam,widths={32,45,85,a,a,a,a,a,a,a,a,a,a,0,0,0},userColumnResize=1
	ListBox ListBox_PeakParam,mode=2,proc=ListBoxProc_MGFit_CoefGuess,SelRow=0,colorWave=root:Package_SLP:MGFit:ListColors
	
	ListBox ListBox_PeakConstraints,pos={915,31},size={170,110},ListWave=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW, SelWave=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplaySelW
	ListBox ListBox_PeakConstraints,widths={50,a,a,32},userColumnResize=1
	ListBox ListBox_PeakConstraints proc=ListBoxProc_MGFit_Constraints
	
	ListBox ListBox_DataList,pos={915,220},size={170,250},ListWave=root:Package_SLP:MGFit:SLP_MGFit_DataListDisplayW, Selwave=root:Package_SLP:MGFit:SLP_MGFit_DataListDisplaySelW
	ListBox ListBox_DataList,widths={32,50,100},SelRow=GDataRow,userColumnResize=1
	ListBox ListBox_DataList,mode=2,proc=ListBoxProc_MGFit_DataSelect
	
	ListBox ListBox_Library,pos={380,285},size={530,130},ListWave=root:Package_SLP:MGFit:SLP_MGFit_PeakLibraryW,SelWave=root:Package_SLP:MGFit:SLP_MGFit_PeakLibrarySelW
	ListBox ListBox_Library,widths={32,50,100,a,a,a,a,a,a,a,a,a,a,a,a,a},userColumnResize=1
	
	ListBox list_PeakArea,pos={485,494},size={601,64},mode=6,proc=ListBoxProc_MGFit_AreaRatio
	ListBox list_PeakArea,listWave=root:Package_SLP:MGFit:SLP_MGFit_PeakAreaW
	ListBox list_PeakArea,widths={b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b}
	ListBox list_PeakArea,userColumnResize= 1
	
	ListBox list_CurveArea,pos={483,562},size={601,54},mode=6,proc=ListBoxProc_MGFit_AreaRatio
	ListBox list_CurveArea,listWave=root:Package_SLP:MGFit:SLP_MGFit_CurveAreaW
	ListBox list_CurveArea,widths={c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c}
	ListBox list_CurveArea,userColumnResize= 1
	
	Variable SliderGroup_Xpos=10
	Variable SliderGroup_Ypos=355
	Variable SliderGroup_vspacing=48
	Variable SliderGroup_hWidth=250
	SetDrawLayer UserBack
	SetDrawEnv fsize= 10; DrawText SliderGroup_Xpos,SliderGroup_Ypos,"Intensity"
	SetDrawEnv fsize= 10; DrawText SliderGroup_Xpos+SliderGroup_hWidth+62,SliderGroup_Ypos+5,"Position"
	SetDrawEnv fsize= 10; DrawText SliderGroup_Xpos+SliderGroup_hWidth+62,SliderGroup_Ypos+5+SliderGroup_vspacing,"FWHM"
	SetDrawEnv fsize= 10; DrawText SliderGroup_Xpos+SliderGroup_hWidth+62,SliderGroup_Ypos+5+2*SliderGroup_vspacing,"Mixture"
	SetDrawEnv fsize= 10; DrawText SliderGroup_Xpos+SliderGroup_hWidth+62,SliderGroup_Ypos+5+3*SliderGroup_vspacing,"Asymmetry"
	
	Slider slider_Int,pos={SliderGroup_Xpos,SliderGroup_Ypos},size={60,170}
	Slider slider_Int,limits={0,2,0.05},value= 0,ticks=10,proc=SliderProc_MGFit_CoefAdjust
	Slider slider_Pos,pos={SliderGroup_Xpos+60,SliderGroup_Ypos-10},size={SliderGroup_hWidth,45}
	Slider slider_Pos,limits={270,350,0.1},value= 0,ticks=10,vert= 0,proc=SliderProc_MGFit_CoefAdjust
	Slider slider_Fwhm,pos={SliderGroup_Xpos+60,SliderGroup_Ypos-10+SliderGroup_vspacing},size={SliderGroup_hWidth,45}
	Slider slider_Fwhm,limits={0.3,5,0.1},value= 0,ticks=10,vert= 0,proc=SliderProc_MGFit_CoefAdjust
	Slider slider_Mix,pos={SliderGroup_Xpos+60,SliderGroup_Ypos-10+2*SliderGroup_vspacing},size={SliderGroup_hWidth,45}
	Slider slider_Mix,limits={0.01,0.99,0.01},value= 0,ticks=10,vert= 0,proc=SliderProc_MGFit_CoefAdjust
	Slider slider_As,pos={SliderGroup_Xpos+60,SliderGroup_Ypos-10+3*SliderGroup_vspacing},size={SliderGroup_hWidth,45}
	Slider slider_As,limits={0.01,0.99,0.01},value= 0,ticks=10,vert= 0,proc=SliderProc_MGFit_CoefAdjust
	
	SetVariable setvar_GssChisq,pos={11,323},size={150,15},title="Guess chi square"
	SetVariable setvar_GssChisq,limits={-inf,inf,0},value= root:Package_SLP:MGFit:Gss_Chisq,noedit= 1
	SetVariable setvar_GssChisq valueColor=(1,26221,39321)
	SetVariable setvar_FitChisq,pos={181,323},size={150,15},title="Fit chi square"
	SetVariable setvar_FitChisq,valueColor=(13102,26214,0)
	SetVariable setvar_FitChisq,limits={-inf,inf,0},value= root:Package_SLP:MGFit:Fit_Chisq,noedit= 1
	
	SetVariable setvar_LowX,pos={390,575},size={90,15},title="Low X",proc=SetVarProc_MGFit_AreaUnderCurve
	SetVariable setvar_LowX,value= root:Package_SLP:MGFit:LowX
  	SetVariable SetVar_HighX,pos={390,600},size={90,15},title="High X",proc=SetVarProc_MGFit_AreaUnderCurve
	SetVariable SetVar_HighX,value= root:Package_SLP:MGFit:HighX
	
	SetVariable setvar_AreaRatio,pos={485,477},size={100,15},title="Area ratio"
	SetVariable setvar_AreaRatio,limits={-inf,inf,0},value= root:Package_SLP:MGFit:AreaRatio,noedit= 1
	
	PopupMenu popup_BgSel,pos={155,25},size={113,20},title="Background type",Proc=PopMenuProc_MGFit_MakeBG
	PopupMenu popup_BgSel,mode=1,popvalue="None",value= #"\"None;Shirley;Erf;atan;Flat;Slope\""
	
	Button button_Done,pos={857,466},size={50,20},title="Done",proc=ButtonProc_MGFit_Done,fsize=10
	
	Button Button_ImportFromLib,pos={383,418},size={525,20},title="Import selected peak",fSize=10,proc=ButtonProc_MGFit_ImportFromLib
	Button Button_AddLibItem,pos={383,442},size={100,20},proc=ButtonProc_MGFit_AddLibItem,title="Add to library..",fSize=10
	Button Button_EditLib,pos={489,442},size={70,20},proc=ButtonProc_MGFit_EditLib,title="Edit library..",fSize=10
	Button Button_RemoveLibItem,pos={564,442},size={120,20},proc=ButtonProc_MGFit_RemoveFromLib,title="Remove selected peak",fSize=10
	Button Button_SaveLib,pos={689,442},size={95,20},proc=ButtonProc_MGFit_SaveLibAsTxt,title="Save library as..", fSize=10
	Button Button_LoadLib,pos={789,442},size={120,20},proc=ButtonProc_MGFit_LoadLibFile,title="Load library from file..",fSize=10

	Button Button_Export,pos={120,46},size={100,20},title="Export graph..",fsize=10,proc=ButtonProc_MGFit_ExportGraph
	Button Button_AddData,pos={915,195},size={85,20},title="Add data wave",fsize=10,proc=ButtonProc_MGFit_LoadData
  	Button Button_ConstraintDefault,pos={915,145},size={51,16},title="Default",fsize=10,proc=ButtonProc_MGFit_DefConstraints
  	Button Button_EditConstraintDefault,pos={965,145},size={80,16},title="Edit default..",fsize=10,proc=ButtonProc_MGFit_EditConstDef
  	Button Button_DeleteData,pos={1005,195},size={85,20},title="Delete data..",fSize=10,proc=ButtonProc_MGFit_DeleteData
	Button Button_CopyGssParam,pos={745,10},size={130,20},proc=ButtonProc_MGFit_CopyGssParam,title="Copy parameters from..",fSize=10
	Button Button_Holdall,pos={1046,146},size={51,16},proc=ButtonProc_MGFit_HoldAllTogg,title="Hold all",fSize=10
	Button Button_AddLibFromGss,pos={465,251},size={200,20},proc=ButtonProc_MGFit_AddLibItemGss,title="Add highlighted peak to library",fSize=10
  	Button button_Fit,pos={700,255},size={50,23},proc=ButtonProc_MGFit_Fit,title="FIT",fColor=(65535,21845,0)
	
	Button Button_SaveGss,pos={555,11},size={90,20},proc=ButtonProc_MGFit_SaveGssAsTxt,title="Save guesses..",fSize=10
	Button Button_LoadGss,pos={650,11},size={90,20},proc=ButtonProc_MGFit_LoadGssFile,title="Load guesses..",fSize=10
	
	Button Button_AcceptFitCoef,pos={758,254},size={150,20},proc=ButtonProc_MGFit_AcceptFit,title="Accept new fit coefficients",fSize=10
	Button Button_AdjBG,pos={10,46},size={100,20},proc=ButtonProc_MGFit_BGAdj,title="Adjust background",fSize=10

	CheckBox check_DisplayOriginal,pos={10,28},size={50,15},title="Original",value= 0,proc=CheckProc_MGFit_DisplayOrgBG
  	CheckBox check_DisplayBg,pos={70,28},size={69,15},title="Background",value= 0,proc=CheckProc_MGFit_DisplayOrgBG
	CheckBox check_DispFit,pos={282,51},size={59,15},proc=CheckProc_MGFit_DispFit,title="Show fit result"
	
	Display/W=(12,71,366,317)/HOST=SLP_MultiGLAFit_01 
	RenameWindow #,G0
	SetActiveSubwindow ##
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW	
	If (Dimsize(SLP_MGFit_DataListW,0)>0)
		String DataName=SLP_MGFit_DataListW[GDataRow][0]
		SLP_MGFit_DisplaySelectedData(DataName)
	EndIf
End

Function SLP_MGFit_Init()
	String Current_folder=getdatafolder(1)
	NewDataFolder /O  root:Package_SLP
	NewDataFolder /O  root:Package_SLP:MGFit 
	SetDataFolder root:Package_SLP:MGFit
	
	variable /G N_peaks=1
	
	Variable /G MaxN_peaks=64
	variable /G GDataRow=0
	
	variable /G GLA_int=0
	variable /G GLA_pos=0
	variable /G GLA_fwhm=0
	variable /G GLA_mix=0
	variable /G GLA_as=0

	variable /G Init_int=1
	variable /G Init_pos=0
	variable /G Init_fwhm=1
	variable /G Init_mix=0.5
	variable /G Init_as=0.5
	
	variable /G PosRange=0
	variable /G Pos_pm_flg=0
	
	variable /G Gss_Chisq
	variable /G Fit_Chisq
	
	variable /G LowX //to calculate area under curve
	variable /G HighX //to calculate area under curve
	
	variable /G AreaRatio
	
	Make /O/T/N=(0,64) SLP_MGFit_DataListW
	SetDimLabel 1,0,DataName,SLP_MGFit_DataListW
	SetDimLabel 1,1,DataFolder,SLP_MGFit_DataListW
	SetDimLabel 1,2,OrgData,SLP_MGFit_DataListW
	SetDimLabel 1,3,N_Peaks,SLP_MGFit_DataListW
	SetDimLabel 1,4,Peak_Sel,SLP_MGFit_DataListW
	SetDimLabel 1,5,BG_Type,SLP_MGFit_DataListW
	SetDimLabel 1,6,BG_Param0,SLP_MGFit_DataListW
	SetDimLabel 1,7,BG_Param1,SLP_MGFit_DataListW
	SetDimLabel 1,8,BG_Param2,SLP_MGFit_DataListW
	SetDimLabel 1,9,BG_Param3,SLP_MGFit_DataListW
	SetDimLabel 1,10,BG_Param4,SLP_MGFit_DataListW
	SetDimLabel 1,11,BG_Param5,SLP_MGFit_DataListW
	SetDimLabel 1,12,BG_Param6,SLP_MGFit_DataListW
	SetDimLabel 1,13,BG_Param7,SLP_MGFit_DataListW
	SetDimLabel 1,14,BG_Param8,SLP_MGFit_DataListW
	SetDimLabel 1,15,GssChiSq,SLP_MGFit_DataListW
	SetDimLabel 1,16,FitChiSq,SLP_MGFit_DataListW
	SetDimLabel 1,17,ShowFit_Flg,SLP_MGFit_DataListW //Show fit result instead of guess
	SetDimLabel 1,18,LowX,SLP_MGFit_DataListW //For area under curve calculations
	SetDimLabel 1,19,HighX,SLP_MGFit_DataListW //For area under curve calculations

	Make /O/T/N=(0,16) SLP_MGFit_DataListDisplayW
	SetDimLabel 1,0,Select,SLP_MGFit_DataListDisplayW
	SetDimLabel 1,1,DataName,SLP_MGFit_DataListDisplayW
	SetDimLabel 1,2,DataNote,SLP_MGFit_DataListDisplayW
	Make /O/N=(0,16) SLP_MGFit_DataListDisplaySelW
	
	Make /O/T/N=(MaxN_peaks,16) SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,0,Select,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,1,PeakName,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,2,PeakNote,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,3,Int,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,4,Pos,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,5,FWHM,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,6,Mix,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,7,As,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,8,RInt,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,9,RPos,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,10,RFWHM,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,11,RMix,SLP_MGFit_iCoefGuessDisplayW
	SetDimLabel 1,12,RAs,SLP_MGFit_iCoefGuessDisplayW
	SLP_MGFit_iCoefGuessDisplayW[][1]="p"+num2str(p)
	SLP_MGFit_iCoefGuessDisplayW[][3]=num2str(Init_int)
	SLP_MGFit_iCoefGuessDisplayW[][4]=num2str(Init_pos)
	SLP_MGFit_iCoefGuessDisplayW[][5]=num2str(Init_fwhm)
	SLP_MGFit_iCoefGuessDisplayW[][6]=num2str(Init_mix)
	SLP_MGFit_iCoefGuessDisplayW[][7]=num2str(Init_as)
	
	Make /O/N=(MaxN_peaks,16,2) SLP_MGFit_iCoefGuessDisplaySelW //Layer2 = cell color
	SLP_MGFit_iCoefGuessDisplaySelW[][0][0]=32 //Checkbox (unchecked)
	SLP_MGFit_iCoefGuessDisplaySelW[][2,7][0]=2 //Editable (peak description)
	
	Make /O/T/N=(5,8) SLP_MGFit_ConstraintDisplayW
	SetDimLabel 1,0,CoefName,SLP_MGFit_ConstraintDisplayW
	SetDimLabel 1,1, MinimumVal,SLP_MGFit_ConstraintDisplayW
	SetDimLabel 1,2, MaximumVal,SLP_MGFit_ConstraintDisplayW
	SetDimLabel 1,3,Hold,SLP_MGFit_ConstraintDisplayW
	SLP_MGFit_ConstraintDisplayW[0][0]={"Intensity","Position","FWHM","Mixture","Assymetry"}
	SLP_MGFit_ConstraintDisplayW[0][1]={"0","NaN","0.3","0.01","0.01"}
	SLP_MGFit_ConstraintDisplayW[0][2]={"NaN","NaN","5","0.99","0.99"}
	Duplicate /O/T SLP_MGFit_ConstraintDisplayW,SLP_MGFit_DefaultConstraintW
	
	Make /O/N=(5,8,2) SLP_MGFit_ConstraintDisplaySelW //Layer2 = cell color
	SLP_MGFit_ConstraintDisplaySelW[][1,2][0]=2
	SLP_MGFit_ConstraintDisplaySelW[][3][0]=32
	Duplicate /O SLP_MGFit_ConstraintDisplaySelW,SLP_MGFit_DefaultConstraintSelW

	Make /O/T/N=(6,32) SLP_MGFit_PeakLibraryW
	SetDimLabel 1,0,Select,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,1,Name,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,2,PeakNote,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,3,Int,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,4,Pos,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,5,FWHM,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,6,Mix,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,7,As,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,9,IntMin,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,10,IntMax,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,11,PosMin,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,12,PosMax,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,13,FWHMMin,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,14,FWHMMax,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,15,MixMin,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,16,MixMax,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,17,AsMin,SLP_MGFit_PeakLibraryW
	SetDimLabel 1,18,AsMax,SLP_MGFit_PeakLibraryW
	SLP_MGFit_PeakLibraryW[0][1]={{"C1s Atomic"},{""},{"1"},{"283"},{"1"},{"0.5"},{"0.5"}}
	SLP_MGFit_PeakLibraryW[1][1]={{"C1s C-C"},{""},{"1"},{"284"},{"1"},{"0.5"},{"0.5"}}
	SLP_MGFit_PeakLibraryW[2][1]={{"C1s C-OH"},{""},{"1"},{"285"},{"1"},{"0.5"},{"0.5"}}
	SLP_MGFit_PeakLibraryW[3][1]={{"C1s C=O"},{""},{"1"},{"286"},{"1"},{"0.5"},{"0.5"}}
	SLP_MGFit_PeakLibraryW[4][1]={{"C1s COOH"},{""},{"1"},{"288"},{"1"},{"0.5"},{"0.5"}}
	SLP_MGFit_PeakLibraryW[5][1]={{"C1s C-NH3"},{""},{"1"},{"285"},{"1"},{"0.5"},{"0.5"}}
	
	Make /O/N=(6,32) SLP_MGFit_PeakLibrarySelW
	SLP_MGFit_PeakLibrarySelW[][0]=32
	SLP_MGFit_PeakLibrarySelW[][2]=2
	
	Make/O/W/U ListColors= {{0,0,0},{65535,0,0},{25000,25000,25000},{0,0,65535},{0,65535,65535}}
	MatrixTranspose ListColors
	
	make /O/T /N=(2,16) SLP_MGFit_CurveAreaW
	SLP_MGFit_CurveAreaW=""
	make /O/T /N=(3,MaxN_peaks) SLP_MGFit_PeakAreaW
	SLP_MGFit_PeakAreaW=""
	SLP_MGFit_PeakAreaW[0][]="p"+num2str(q)
		
	Setdatafolder $Current_folder
End

Function SetVarProc_MGFit_AreaUnderCurve(sva) : SetVariableControl
	STRUCT WMSetVariableAction &sva

	switch( sva.eventCode )
		case 1: // mouse up
		case 2: // Enter key
		case 3: // Live update
			Variable dval = sva.dval
			String sval = sva.sval
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			SLP_MGFit_ReadAreaUnderCurve(DataName)
			NVAR LowX=root:Package_SLP:MGFit:LowX
			NVAR HighX=root:Package_SLP:MGFit:HighX
			SLP_MGFit_DataListW[DataRow][18]=num2str(LowX)
			SLP_MGFit_DataListW[DataRow][19]=num2str(HighX)
			SLP_MGFit_DrawVLine(dVal)
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_AddLibItemGss(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
			Variable PeakRow=V_Value
			
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			
			Wave /T SLP_MGFit_PeakLibraryW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibraryW
			Wave SLP_MGFit_PeakLibrarySelW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibrarySelW

			String DataName=SLP_MGFit_DataListW[DataRow][0]
			String DataLocation=SLP_MGFit_DataListW[DataRow][1]
			
			String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
			String ConstraintW_FName=DataLocation+"ConstraintW"
			Wave /T CoefGuessDispW=$CoefGuessDispW_FName
			Wave ConstraintW=$ConstraintW_FName
			
			String PeakName=CoefGuessDispW[PeakRow][1]
			String PeakNote=CoefGuessDispW[PeakRow][2]
			variable int=str2num(CoefGuessDispW[PeakRow][3]),pos=str2num(CoefGuessDispW[PeakRow][4])
			variable fwhm=str2num(CoefGuessDispW[PeakRow][5]),mix=str2num(CoefGuessDispW[PeakRow][6])
			Variable as=str2num(CoefGuessDispW[PeakRow][7])
			
			variable int_min=ConstraintW[0][0][PeakRow],pos_min=ConstraintW[1][0][PeakRow],fwhm_min=ConstraintW[2][0][PeakRow],mix_min=ConstraintW[3][0][PeakRow],as_min=ConstraintW[4][0][PeakRow]
			variable int_max=ConstraintW[0][1][PeakRow],pos_max=ConstraintW[1][1][PeakRow],fwhm_max=ConstraintW[2][1][PeakRow],mix_max=ConstraintW[3][1][PeakRow],as_max=ConstraintW[4][1][PeakRow]
			
			Redimension /N=(dimsize(SLP_MGFit_PeakLibraryW,0)+1,dimsize(SLP_MGFit_PeakLibraryW,1))  SLP_MGFit_PeakLibraryW
			Redimension /N=(dimsize(SLP_MGFit_PeakLibrarySelW,0)+1,dimsize(SLP_MGFit_PeakLibrarySelW,1))  SLP_MGFit_PeakLibrarySelW
			SLP_MGFit_PeakLibrarySelW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][0]=32
			SLP_MGFit_PeakLibrarySelW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][2]=2
		
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][1]=PeakName
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][2]=PeakNote
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][3]=Num2str(int)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][4]=Num2str(pos)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][5]=Num2str(fwhm)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][6]=Num2str(mix)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][7]=Num2str(as)
		
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][8]=Num2str(int_min)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][9]=Num2str(int_max)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][10]=Num2str(pos_min)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][11]=Num2str(pos_max)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][12]=Num2str(fwhm_min)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][13]=Num2str(fwhm_max)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][14]=Num2str(mix_min)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][15]=Num2str(mix_max)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][16]=Num2str(as_min)
			SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][17]=Num2str(as_min)
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_AddLibItem(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			String PeakName,PeakNote
			variable int=NaN,pos=NaN,fwhm=NaN,mix=NaN,as=NaN
			variable int_min=NaN,pos_min=NaN,fwhm_min=NaN,mix_min=NaN,as_min=NaN
			variable int_max=NaN,pos_max=NaN,fwhm_max=NaN,mix_max=NaN,as_max=NaN
			
			Prompt PeakName,"Peak Name"
			Prompt PeakNote,"Note"
			Prompt int,"Intensity"
			Prompt pos,"Position"
			prompt fwhm,"FWHM"
			prompt mix,"Mixture"
			prompt as,"Asymmetry"
			Doprompt "Enter peak parameters",PeakName,PeakNote,int,pos,fwhm,mix,as
			
			If (V_Flag==0) //If user clicks ok
				Prompt int_min,"Min intensity"
				Prompt int_max,"Max intensity"
				Prompt pos_min,"Min position"
				Prompt pos_max,"Max position"
				prompt fwhm_min,"Min FWHM"
				prompt fwhm_max,"Max FWHM"
				prompt mix_Min,"Min mixture"
				prompt mix_Max,"Max mixture"
				prompt as_min,"Min Asymmetry"
				prompt as_max,"Max Asymmetry"
				Doprompt "Enter peak parameters",int_min,int_max,pos_min,pos_max,fwhm_min,fwhm_max,mix_min,mix_max,as_min,as_max
				
				If (V_Flag==0) //if user clicks ok
					Wave /T SLP_MGFit_PeakLibraryW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibraryW
					Wave SLP_MGFit_PeakLibrarySelW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibrarySelW
					Redimension /N=(dimsize(SLP_MGFit_PeakLibraryW,0)+1,dimsize(SLP_MGFit_PeakLibraryW,1))  SLP_MGFit_PeakLibraryW
					Redimension /N=(dimsize(SLP_MGFit_PeakLibrarySelW,0)+1,dimsize(SLP_MGFit_PeakLibrarySelW,1))  SLP_MGFit_PeakLibrarySelW
					SLP_MGFit_PeakLibrarySelW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][0]=32
					SLP_MGFit_PeakLibrarySelW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][2]=2
				
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][1]=PeakName
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][2]=PeakNote
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][3]=Num2str(int)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][4]=Num2str(pos)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][5]=Num2str(fwhm)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][6]=Num2str(mix)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][7]=Num2str(as)
				
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][8]=Num2str(int_min)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][9]=Num2str(int_max)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][10]=Num2str(pos_min)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][11]=Num2str(pos_max)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][12]=Num2str(fwhm_min)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][13]=Num2str(fwhm_max)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][14]=Num2str(mix_min)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][15]=Num2str(mix_max)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][16]=Num2str(as_min)
					SLP_MGFit_PeakLibraryW[dimsize(SLP_MGFit_PeakLibrarySelW,0)-1][17]=Num2str(as_min)
				EndIf
			EndIf
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_ExportGraph(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
			Variable PeakRow=V_Value
			
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			String DataLocation=SLP_MGFit_DataListW[DataRow][1]
			String DataW_FName=DataLocation+DataName	//Raw data
			String BgW_FName=DataW_FName+"_BG"		//Background
			String Data_nBGW_FName=DataW_FName+"_nBG"	//Data after BG subtraction
			String Data_GssW_FName=DataW_FName+"_Gss"	//Guess curve
			Wave Data_nBGW=$Data_nBGW_FName
			
			String TraceList=TraceNameList("SLP_MultiGLAFit_01#G0",";",1)
			Variable N_curves=Itemsinlist(TraceList,";")
			String NewFolderName=":"+DataName
			String TraceName=""
			String orgW_FName=""
			String destW_Name=""
			variable i=0
			variable run_flg=0
			String LegendStr=DataName+"\r\Zr080"
			String FolderExistsWarning="Enter new folder name?"
			
			//Check if the folder to keep waves already exists
			If (!DataFolderExists(NewFolderName))
				run_flg=1
			Else
				DoAlert 1,NewFolderName+" folder already exists. Overwrite existing waves?"
				If (V_Flag==1) //Click yes
					run_flg=1
				Else //Click no
					Do
						DoAlert 1,FolderExistsWarning
						If (V_Flag==1)
							Prompt NewFolderName,"New folder:"
							DoPrompt "Enter new folder name",NewFolderName
							run_flg=1
						Else
							run_flg=0
						EndIf
						FolderExistsWarning="Still duplicated name. Enter new folder name?"
					While (run_flg!=0&&DataFolderExists(NewFolderName))
				Endif
			EndIf
			
			If (run_flg)
				NewDataFolder /O $NewFolderName				
				For (i=0;i<N_curves;i+=1)
					TraceName=StringFromList(i,TraceList)
					orgW_FName=DataLocation+TraceName
					destW_Name=NewFolderName+":"+TraceName
					duplicate /O $orgW_FName,$destW_Name
				EndFor	
				
				String Current_folder=getdatafolder(1)
				SetDataFolder $NewFolderName
				
				TraceName=StringFromList(0,TraceList)
				Display $TraceName
				ModifyGraph mode($TraceName)=7,hbFill($TraceName)=2,rgb($TraceName)=(65535,49151,49151)
				For (i=1;i<N_curves;i+=1)
					TraceName=StringFromList(i,TraceList)
					Appendtograph $TraceName
					Variable FitDisplay_Flg=str2num(SLP_MGFit_DataListW[DataRow][17])
					If (FitDisplay_Flg==0) //Display Guess curves
						//Modify the graph and trace colors
						If (i==1)//Data Wave (BG subtracted)
							Modifygraph mode($TraceName)=3, Marker($TraceName)=16, rgb($TraceName)=(43690,43690,43690)
							LegendStr+="\s("+TraceName+")"+"Data w/o BG\r"
						ElseIf (i==2) //Guess Wave
							ModifyGraph rgb($TraceName)=(1,34817,52428), lsize($TraceName)=3
							LegendStr+="\s("+TraceName+")Guess curve\r"
						Elseif (i==PeakRow+3)//Selected peak
							ModifyGraph rgb($TraceName)=(65535,43690,0),lsize($TraceName)=1.5
							LegendStr+="\s("+TraceName+")Selected component\r"
						EndIf
					Elseif (FitDisplay_Flg==1)	//Display fit results
						If (i==1)
							Modifygraph mode($TraceName)=3, Marker($TraceName)=16, rgb($TraceName)=(43690,43690,43690)
							LegendStr+="\s("+TraceName+")"+"Data w/o BG\r"
						Elseif (i==2)//Fit Wave
							ModifyGraph rgb($TraceName)=(0,35000,0), lsize($TraceName)=3
							LegendStr+="\s("+TraceName+")Fit result\r"
						Elseif (i==PeakRow+3) //Selected peak
							ModifyGraph rgb($TraceName)=(65535,43690,0),lsize($TraceName)=1.5
							LegendStr+="\s("+TraceName+")Selected component\r"
						EndIf
					EndIf
				EndFor
				
				//Orginal data w (before BG) and BG waves display
				Wavestats /Q Data_nBGW
				Variable OrgData_yOffset=(V_Max-V_Min)*1.10 //Determine offset value from the data y-range
				
				ControlInfo /W=SLP_MultiGLAFit_01 Check_DisplayOriginal; variable DisplayOrg_Flg=V_Value
				If (DisplayOrg_Flg)
					TraceName=StringFromList(ItemsInlist(TraceList,";")-2,TraceList,";")
					ModifyGraph offset($TraceName)={0,OrgData_yOffset},rgb($TraceName)=(30000,30000,30000)
					LegendStr+="\s("+TraceName+")Original data\r"
				EndIf
				
				ControlInfo /W=SLP_MultiGLAFit_01 Check_DisplayBG; variable DisplayBg_Flg=V_Value
				If (DisplayBg_Flg)
					TraceName=StringFromList(ItemsInlist(TraceList,";")-1,TraceList,";")
					ModifyGraph offset($TraceName)={0,OrgData_yOffset}, lstyle($TraceName)=2,rgb($TraceName)=(30000,30000,30000)
					LegendStr+="\s("+TraceName+")Background\r"
				EndIf
				
				SetDataFolder $Current_folder
				
				Modifygraph mirror=1,minor=1,tick=2
				LegendStr=removeending(LegendStr,"\r")
				Legend LegendStr
				
			EndIf
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End


Function ButtonProc_MGFit_AcceptFit(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
				
			DoAlert 1,"Overwrite the guess values with the fitted coefficients."
			
			If (V_Value==1)
				Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
				
				String DataName=SLP_MGFit_DataListW[DataRow][0]
				String DataLocation=SLP_MGFit_DataListW[DataRow][1]
				String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
				Wave /T CoefGuessDispW=$CoefGuessDispW_FName
				
				CoefGuessDispW[][3]=CoefGuessDispW[p][8]
				CoefGuessDispW[][4]=CoefGuessDispW[p][9]
				CoefGuessDispW[][5]=CoefGuessDispW[p][10]
				CoefGuessDispW[][6]=CoefGuessDispW[p][11]
				CoefGuessDispW[][7]=CoefGuessDispW[p][12]
				
				SLP_MGFit_DataListW[DataRow][17]="0"
				SLP_MGFit_DisplaySelectedData(DataName)
			EndIf
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_SaveGssAsTxt(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			String DataLocation=SLP_MGFit_DataListW[DataRow][1]
			String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
			Wave /T CoefGuessDispW=$CoefGuessDispW_FName
			
			Make /O/T/N=(dimsize(CoefGuessDispW,0)+1,dimsize(CoefGuessDispW,1)) root:Package_SLP:MGFit:SLP_MGFit_GssExportW /Wave=SLP_MGFit_GssExportW
			SLP_MGFit_GssExportW=""
			
			Variable i=0
			For (i=0;i<dimsize(CoefGuessDispW,0);i+=1)
				SLP_MGFit_GssExportW[i+1][]=CoefGuessDispW[i][q]
			EndFor
			For (i=0;i<dimsize(CoefGuessDispW,1);i+=1)
				SLP_MGFit_GssExportW[0][i]=GetDimLabel(CoefGuessDispW,1,i)
			EndFor
			
			String GssFileName="GuessExport"
			Save/G /I /M="\n" SLP_MGFit_GssExportW as GssFileName
			
			Killwaves SLP_MGFit_GssExportW
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_LoadGssFile(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			String DataLocation=SLP_MGFit_DataListW[DataRow][1]
			String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
			String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
			Wave /T CoefGuessDispW=$CoefGuessDispW_FName
			
			LoadWave /J /M  /K=2 /N=GssData /U={0,0,1,0}
			String NewGssWaveName=stringfromlist(0,S_Wavenames)
			Wave NewGssWave=$NewGssWaveName
			Duplicate /O /T NewGssWave,CoefGuessDispW
			Killwaves NewGssWave
			Make /O /N=(dimsize(CoefGuessDispW,0),dimsize(CoefGuessDispW,1),2) $CoefGuessDispSelW_FName /Wave=CoefGuessDispSelW
			CoefGuessDispSelW=0
			CoefGuessDispSelW[][0][0]=32
			CoefGuessDispSelW[][2,7][0]=2
			
			SLP_MGFit_DisplaySelectedData(DataName)
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_LoadLibFile(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			Wave /T SLP_MGFit_PeakLibraryW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibraryW

			LoadWave /J /M  /K=2 /N=LibData /U={0,0,1,0}
			String NewLibWaveName=stringfromlist(0,S_Wavenames)
			Wave NewLibWave=$NewLibWaveName
			Duplicate /O /T NewLibWave,SLP_MGFit_PeakLibraryW
			Killwaves NewLibWave
			Make /O /N=(dimsize(SLP_MGFit_PeakLibraryW,0),dimsize(SLP_MGFit_PeakLibraryW,1)) root:Package_SLP:MGFit:SLP_MGFit_PeakLibrarySelW /Wave=SLP_MGFit_PeakLibrarySelW
			SLP_MGFit_PeakLibrarySelW=0
			SLP_MGFit_PeakLibrarySelW[][0]=32
			SLP_MGFit_PeakLibrarySelW[][2]=2
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_SaveLibAsTxt(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
		
			Wave /T SLP_MGFit_PeakLibraryW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibraryW
			
			Make /O/T/N=(dimsize(SLP_MGFit_PeakLibraryW,0)+1,dimsize(SLP_MGFit_PeakLibraryW,1)) root:Package_SLP:MGFit:SLP_MGFit_PeakLibExportW /Wave=SLP_MGFit_PeakLibExportW
			SLP_MGFit_PeakLibExportW=""
			Variable i=0
			For (i=0;i<dimsize(SLP_MGFit_PeakLibraryW,0);i+=1)
				SLP_MGFit_PeakLibExportW[i+1][]=SLP_MGFit_PeakLibraryW[i][q]
			EndFor
			SLP_MGFit_PeakLibExportW[0][0]={{""},{"Peak name"},{"Note"},{"Int"},{"Pos"},{"FWHM"},{"Mix"},{"As"},{"Min Int"},{"Max Int"},{"Min Pos"},{"Max Pos"},{"Min FWHM"},{"Max FWHM"},{"Min Mix"},{"Max Mix"},{"Min As"},{"Max As"}}
			
			String LibFileName="LibraryExport"
			Save/G /I /M="\n" SLP_MGFit_PeakLibExportW as LibFileName
			
			Killwaves SLP_MGFit_PeakLibExportW
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_RemoveFromLib(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			Doalert 2,"Remove selected peak(s) from library?"
			If (V_Flag==1)
				Wave /T SLP_MGFit_PeakLibraryW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibraryW
				Wave SLP_MGFit_PeakLibrarySelW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibrarySelW
				
				Variable i
				For (i=dimsize(SLP_MGFit_PeakLibrarySelW,0)-1;i>=0;i-=1)
					If (SLP_MGFit_PeakLibrarySelW[i][0]==48)
						DeletePoints i,1,SLP_MGFit_PeakLibraryW
						DeletePoints i,1,SLP_MGFit_PeakLibrarySelW
					EndIf
				EndFor
			EndIf
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_EditLib(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			Wave /T SLP_MGFit_PeakLibraryW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibraryW
			DoWindow /K SLP_MGFit_PeakLibraryEdit_01
			Edit /K=1/N=SLP_MGFit_PeakLibraryEdit_01 SLP_MGFit_PeakLibraryW
			ModifyTable /W=SLP_MGFit_PeakLibraryEdit_01 horizontalIndex=2,rgb=(0,0,65535)
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_ImportFromLib(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
			Variable PeakRow=V_Value
			
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			
			Wave /T SLP_MGFit_ConstraintDisplayW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW
			Wave  SLP_MGFit_ConstraintDisplaySelW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplaySelW

			Wave /T SLP_MGFit_PeakLibraryW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibraryW
			Wave SLP_MGFit_PeakLibrarySelW=root:Package_SLP:MGFit:SLP_MGFit_PeakLibrarySelW
			
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			String DataLocation=SLP_MGFit_DataListW[DataRow][1]
			
			String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
			String ConstraintW_FName=DataLocation+"ConstraintW"
			
			Wave /T CoefGuessDispW=$CoefGuessDispW_FName
			Wave ConstraintW=$ConstraintW_FName
			
			Variable i=0
			Variable j=PeakRow
			For (i=0;i<dimsize(SLP_MGFit_PeakLibraryW,0);i+=1)
				If (SLP_MGFit_PeakLibrarySelW[i][0]==48)
					//Note
					CoefGuessDispW[j][2]=SLP_MGFit_PeakLibraryW[i][1]+"; "+SLP_MGFit_PeakLibraryW[i][2]
					//Guess
					CoefGuessDispW[j][3]=SLP_MGFit_PeakLibraryW[i][3]	//int
					CoefGuessDispW[j][4]=SLP_MGFit_PeakLibraryW[i][4]	//pos
					CoefGuessDispW[j][5]=SLP_MGFit_PeakLibraryW[i][5]	//fwhm
					CoefGuessDispW[j][6]=SLP_MGFit_PeakLibraryW[i][6]	//mix
					CoefGuessDispW[j][7]=SLP_MGFit_PeakLibraryW[i][7]	//as
					//Constraints
					ConstraintW[0][0][j]=str2num(SLP_MGFit_PeakLibraryW[i][8])	//int min
					ConstraintW[0][1][j]=str2num(SLP_MGFit_PeakLibraryW[i][9])	//int max
					ConstraintW[1][0][j]=str2num(SLP_MGFit_PeakLibraryW[i][10])	//pos min
					ConstraintW[1][1][j]=str2num(SLP_MGFit_PeakLibraryW[i][11])	//pos max
					ConstraintW[2][0][j]=str2num(SLP_MGFit_PeakLibraryW[i][12])	//fwhm min
					ConstraintW[2][1][j]=str2num(SLP_MGFit_PeakLibraryW[i][13])	//fwhm max
					ConstraintW[3][0][j]=str2num(SLP_MGFit_PeakLibraryW[i][14])	//mix min
					ConstraintW[3][1][j]=str2num(SLP_MGFit_PeakLibraryW[i][15])	//mix max
					ConstraintW[4][0][j]=str2num(SLP_MGFit_PeakLibraryW[i][16])	//as min
					ConstraintW[4][1][j]=str2num(SLP_MGFit_PeakLibraryW[i][17])	//as max
					
					j+=1
				Endif
			EndFor
			
			SLP_MGFit_DisplaySelectedData(DataName)
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_EditConstDef(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			SLP_MGFit_Panel_DefConstraints()
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function CheckProc_MGFit_Pos_pm_flg(cba) : CheckBoxControl
	STRUCT WMCheckboxAction &cba

	switch( cba.eventCode )
		case 2: // mouse up
			Variable checked = cba.checked
			Wave SLP_MGFit_DefaultConstraintSelW=root:Package_SLP:MGFit:SLP_MGFit_DefaultConstraintSelW
			SLP_MGFit_DefaultConstraintSelW[1][1,2]=!checked*2
			SetVariable setvar_PlusMinus disable=!checked
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function CheckProc_MGFit_DispFit(cba) : CheckBoxControl
	STRUCT WMCheckboxAction &cba

	switch( cba.eventCode )
		case 2: // mouse up
			Variable checked = cba.checked
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			SLP_MGFit_DataListW[DataRow][17]=num2str(checked)
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			SLP_MGFit_DisplaySelectedData(DataName)
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_ConstraintDone(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			DoWindow /K MGFit_DefaultConstraint
			
		break
		case -1: // control being killed
			break
	endswitch

	return 0
End



Function ButtonProc_MGFit_CopyGssParam(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW	
			String DataW_List=""
			Variable i=0
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			
			For (i=0;i<dimsize(SLP_MGFit_DataListW,0);i+=1)
				If (i!=DataRow)
					DataW_List+=SLP_MGFit_DataListW[i][0]+";"
				EndIf
			EndFor
			
			String Org_DataName=""
			Prompt Org_DataName,"Data to copy from..",popup,DataW_List
			DoPrompt "Choose the data to copy parameters from:",Org_DataName
			If (V_Flag==0) //If user click continue
				SLP_MGFit_CopyParam(Org_DataName,DataName)
			EndIf 
			
			SLP_MGFit_DisplaySelectedData(DataName)
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End


Function SLP_MGFit_CopyParam(Org_DataName,Dest_DataName)
	String Org_DataName,Dest_DataName
	
	Variable Org_DataRow=SLP_MGFit_FindDataRow(Org_DataName)
	Variable Dest_DataRow=SLP_MGFit_FindDataRow(Dest_DataName)
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW	
	
	String Org_CoefGuessDispW_FName=SLP_MGFit_DataListW[Org_DataRow][1]+"CoefGuessDispW"
	String Org_CoefGuessDispSelW_FName=SLP_MGFit_DataListW[Org_DataRow][1]+"CoefGuessDispSelW"
	String Org_CoefGuessM_FName=SLP_MGFit_DataListW[Org_DataRow][1]+"CoefGuessM"
	String Org_ConstraintW_FName=SLP_MGFit_DataListW[Org_DataRow][1]+"ConstraintW"
	
	String Dest_CoefGuessDispW_FName=SLP_MGFit_DataListW[Dest_DataRow][1]+"CoefGuessDispW"
	String Dest_CoefGuessDispSelW_FName=SLP_MGFit_DataListW[Dest_DataRow][1]+"CoefGuessDispSelW"
	String Dest_CoefGuessM_FName=SLP_MGFit_DataListW[Dest_DataRow][1]+"CoefGuessM"
	String Dest_ConstraintW_FName=SLP_MGFit_DataListW[Dest_DataRow][1]+"ConstraintW"
	
	Duplicate /T/O $Org_CoefGuessDispW_FName,$Dest_CoefGuessDispW_FName
	Duplicate /O $Org_CoefGuessDispSelW_FName,$Dest_CoefGuessDispSelW_FName
	Duplicate /O $Org_CoefGuessM_FName,$Dest_CoefGuessM_FName
	Duplicate /O $Org_ConstraintW_FName,$Dest_ConstraintW_FName
	
End

Function ButtonProc_MGFit_HoldAllTogg(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW	
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			Wave ConstraintDisplaySelW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplaySelW
			If (ConstraintDisplaySelW[0][3][0]==32)
				ConstraintDisplaySelW[][3][0]=48
			Elseif (ConstraintDisplaySelW[0][3][0]==48)
				ConstraintDisplaySelW[][3][0]=32
			Endif
			SLP_MGFit_Listbox2ConstraintW(DataName)
			SLP_MGFit_ConstraintLogic(DataName)
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End


Function SLP_MGFit_ConstraintLogic1p(DataName,PeakRow)
	String DataName
	Variable PeakRow
	//Logic:
	//	- If hold - do not constraint
	//	- Guess should be within constraint level
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW	
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String DataW_FName=DataLocation+DataName 
	
	String CoefGuessDispW_FName=SLP_MGFit_DataListW[DataRow][1]+"CoefGuessDispW"
	String CoefGuessDispSelW_FName=SLP_MGFit_DataListW[DataRow][1]+"CoefGuessDispSelW"
	String ConstraintW_FName=SLP_MGFit_DataListW[DataRow][1]+"ConstraintW"
	
	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName
	//Wave /T SLP_MGFit_ConstraintDisplayW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW
	Wave ConstraintW=$ConstraintW_FName
	
	CoefGuessDispSelW[PeakRow][3,7][1]=0
	
	Variable j=0
	For (j=3;j<=7;j+=1)
		If ((str2num(CoefGuessDispW[PeakRow][j]) < ConstraintW[j-3][0][PeakRow]) || (str2num(CoefGuessDispW[PeakRow][j])>ConstraintW[j-3][1][PeakRow]))
			CoefGuessDispSelW[PeakRow][j][1]=1
		EndIf
		If (ConstraintW[j-3][2][PeakRow]==1)
			CoefGuessDispSelW[PeakRow][j][1]=2
		EndIf
	EndFor

End

Function SLP_MGFit_ConstraintLogic(DataName)
	String DataName
	//Logic:
	//	- If hold - do not constraint
	//	- Guess should be within constraint level
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW	
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String DataW_FName=DataLocation+DataName 
	
	String CoefGuessDispW_FName=SLP_MGFit_DataListW[DataRow][1]+"CoefGuessDispW"
	String CoefGuessDispSelW_FName=SLP_MGFit_DataListW[DataRow][1]+"CoefGuessDispSelW"
	String ConstraintW_FName=SLP_MGFit_DataListW[DataRow][1]+"ConstraintW"
	
	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName
	//Wave /T SLP_MGFit_ConstraintDisplayW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW
	Wave ConstraintW=$ConstraintW_FName
	
	CoefGuessDispSelW[][][1]=0
	
	Variable i=0
	Variable j=0
	
	For (i=0;i<dimsize(CoefGuessDispW,0);i+=1)
		For (j=3;j<=7;j+=1)
			If ((str2num(CoefGuessDispW[i][j]) < ConstraintW[j-3][0][i]) || (str2num(CoefGuessDispW[i][j])>ConstraintW[j-3][1][i]))
				CoefGuessDispSelW[i][j][1]=1
			EndIf
			If (ConstraintW[j-3][2][i]==1)
				CoefGuessDispSelW[i][j][1]=2
			EndIf
		EndFor
	EndFor

End

Function SLP_MGFit_MakeBGPrompt(DataName,BG_type)
	String DataName
	String BG_type //"Erf","Flat","Slope","atan","Shirley"
	
	Variable FlatBG,StepPos,StepHeight,Width,Atten,StartX,EndX

	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW	

	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]

	String DataW_FName=DataLocation+DataName 
	String DataW_BG_FName=DataW_FName+"_BG"
	String DataW_nBG_FName=DataW_FName+"_nBG"
	
	Wave DataW=$DataW_FName
	Wave DataW_BG=$DataW_BG_FName
	Wave DataW_nBG=$DataW_nBG_FName
	
	If (Stringmatch(SLP_MGFit_DataListW[DataRow][6],""))
		FlatBG=DataW[0]
	Else
		FlatBG=str2num(SLP_MGFit_DataListW[DataRow][6])
	EndIf
	
	If (Stringmatch(SLP_MGFit_DataListW[DataRow][7],""))
		StepPos=dimoffset(DataW,0)+dimdelta(DataW,0)*dimsize(DataW,0)/2 //in the center
	Else
		StepPos=str2num(SLP_MGFit_DataListW[DataRow][7])
	EndIf
	
	If (Stringmatch(SLP_MGFit_DataListW[DataRow][8],""))
		StepHeight=DataW[dimsize(DataW,0)-1]-DataW[0]
	Else
		StepHeight=str2num(SLP_MGFit_DataListW[DataRow][8])
	EndIf
	
	If (Stringmatch(SLP_MGFit_DataListW[DataRow][9],""))
		Width=1
	Else
		Width=str2num(SLP_MGFit_DataListW[DataRow][9])
	EndIf
	
	If (Stringmatch(SLP_MGFit_DataListW[DataRow][10],""))
		Atten=100000
	Else
		Atten=str2num(SLP_MGFit_DataListW[DataRow][10])
	EndIf
	
	If (Stringmatch(SLP_MGFit_DataListW[DataRow][11],""))
		StartX=dimoffset(DataW,0)
	Else
		StartX=str2num(SLP_MGFit_DataListW[DataRow][11])
	EndIf
	
	If (Stringmatch(SLP_MGFit_DataListW[DataRow][12],""))
		EndX=dimoffset(DataW,0)+dimdelta(DataW,0)*(dimsize(DataW,0)-1)
	Else
		EndX=str2num(SLP_MGFit_DataListW[DataRow][12])
	EndIf
	
	strswitch(BG_type)	// string switch
		case "Erf":		// execute if case matches expression
			Prompt FlatBG, "Baseline level"
			Prompt StepPos, "Step position"
			Prompt StepHeight, "Step height"
			Prompt Width, "Step width"
			Prompt Atten, "Attenuation"
			DoPrompt "Enter background parameters:",FlatBG,StepPos,StepHeight,Width,Atten
			
			//Record the Input
			SLP_MGFit_DataListW[DataRow][6]=num2str(FlatBG)
			SLP_MGFit_DataListW[DataRow][7]=num2str(StepPos)
			SLP_MGFit_DataListW[DataRow][8]=num2str(StepHeight)
			SLP_MGFit_DataListW[DataRow][9]=num2str(Width)
			SLP_MGFit_DataListW[DataRow][10]=num2str(Atten)
			
			break						// exit from switch
		case "atan":	
			Prompt FlatBG, "Baseline level"
			Prompt StepPos, "Step position"
			Prompt StepHeight, "Step height"
			Prompt Width, "Step width"
			Prompt Atten, "Attenuation"
			DoPrompt "Enter background parameters:",FlatBG,StepPos,StepHeight,Width,Atten
			
			//Record the input
			SLP_MGFit_DataListW[DataRow][6]=num2str(FlatBG)
			SLP_MGFit_DataListW[DataRow][7]=num2str(StepPos)
			SLP_MGFit_DataListW[DataRow][8]=num2str(StepHeight)
			SLP_MGFit_DataListW[DataRow][9]=num2str(Width)
			SLP_MGFit_DataListW[DataRow][10]=num2str(Atten)
			
			break
		case "Slope":	
			Prompt StartX, "Low energy position"
			Prompt EndX, "High energy position"
			DoPrompt "Enter background parameters:",StartX,EndX
			//Record the input
			SLP_MGFit_DataListW[DataRow][11]=num2str(StartX)
			SLP_MGFit_DataListW[DataRow][12]=num2str(EndX)
								
			break
		case "Flat":	
			Prompt FlatBG, "Baseline level"
			DoPrompt "Enter background parameters:",FlatBG
			//Record the input
			SLP_MGFit_DataListW[DataRow][6]=num2str(FlatBG)
			
			break
		case "Shirley":
			Prompt StartX, "Low energy position"
			Prompt EndX, "High energy position"
			DoPrompt "Enter background parameters:",StartX,EndX
			//Record the input
			SLP_MGFit_DataListW[DataRow][11]=num2str(StartX)
			SLP_MGFit_DataListW[DataRow][12]=num2str(EndX)
			
			break
		default:							// optional default expression executed
			DataW_BG=0					// when no case matches
	endswitch
	
	SLP_MGFit_MakeBG(DataName,BG_type,FlatBG,StepPos,StepHeight,Width,Atten,StartX,EndX)

End



Function SLP_MGFit_MakeBG(DataName,BG_type,FlatBG,StepPos,StepHeight,Width,Atten,StartX,EndX)
	String DataName
	String BG_type //"Erf","Flat","Slope","atan","Shirley"
	Variable FlatBG,StepPos,StepHeight,Width,Atten,StartX,EndX

	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW	

	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]

	String DataW_FName=DataLocation+DataName 
	String DataW_BG_FName=DataW_FName+"_BG"
	String DataW_nBG_FName=DataW_FName+"_nBG"
	
	Wave DataW=$DataW_FName
	Wave DataW_BG=$DataW_BG_FName
	Wave DataW_nBG=$DataW_nBG_FName

	
	strswitch(BG_type)	// string switch
		case "Erf":		// execute if case matches expression
			DataW_BG=FlatBG+StepHeight/2+(StepHeight/2)*erf((x-StepPos)/Width)*exp(1/(Atten*x))
			break						// exit from switch
		case "atan":	
			DataW_BG=FlatBG+StepHeight/2+(StepHeight/2)*atan((x-StepPos)/Width)*exp(1/(Atten*x))
			break
		case "Slope":	
			DataW_BG=DataW(StartX)*(x<=StartX)+DataW(EndX)*(x>=EndX)+(x>StartX&&x<EndX)*(DataW(StartX)+(x-StartX)*(DataW(EndX)-DataW(StartX))/(EndX-StartX))		
			break
		case "Flat":	
			DataW_BG=FlatBG
			break
		case "Shirley":
			SLP_Util_ShirleyGen(DataW_FName,EndX,StartX,10,0)
			String ShirleyBGW_FName=DataW_FName+"_Shirley"
			Wave ShirleyBGW=$ShirleyBGW_FName
			DataW_BG=ShirleyBGW
			Killwaves ShirleyBGW
			break
		default:							// optional default expression executed
			DataW_BG=0					// when no case matches
	endswitch
	DataW_nBG=DataW-DataW_BG
End

Function PopMenuProc_MGFit_MakeBG(pa) : PopupMenuControl
	STRUCT WMPopupAction &pa

	switch( pa.eventCode )
		case 2: // mouse up
			Variable popNum = pa.popNum
			String popStr = pa.popStr
			
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			
			SLP_MGFit_MakeBGPrompt(DataName,popStr)
			SLP_MGFit_DataListW[DataRow][5]=popStr
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function SLP_MGFit_ListBox2CoefMatrix(DataName)
	String DataName
	
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW	
	//Wave /T SLP_MGFit_ConstraintDisplayW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW
	
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
	String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
	String CoefGuessM_FName=DataLocation+"CoefGuessM"
	String ConstraintW_FName=DataLocation+"ConstraintW"
	
	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName
	Wave CoefGuessM=$CoefGuessM_FName
	Wave ConstraintW=$ConstraintW_FName
	
	variable i=0
	variable j=0
	variable N_peak=0
	String Peak_List=""
	
	Make /O /N=(0,5) $CoefGuessM_FName /Wave=CoefGuessM
	CoefGuessM=NaN
	
	For (i=0;i<dimsize(CoefGuessDispW,0);i+=1)
		If (CoefGuessDispSelW[i][0][0]==48)
			Redimension /N=(dimsize(CoefGuessM,0)+1,dimsize(CoefGuessM,1))  CoefGuessM
			CoefGuessM[j][]=str2num(CoefGuessDispW[i][q+3])
			j+=1
		EndIf
	EndFor
	
	SLP_MGFit_DataListW[DataRow][3]=num2str(N_peak)

End

Function SLP_MGFit_AddDataW(DataW_FName)
	String DataW_FName //**Must be Full Name only**
	NVAR Init_pos=root:Package_SLP:MGFit:Init_pos
	NVAR Init_int=root:Package_SLP:MGFit:Init_int
	NVAR Init_fwhm=root:Package_SLP:MGFit:Init_fwhm
	NVAR Init_mix=root:Package_SLP:MGFit:Init_mix
	NVAR Init_as=root:Package_SLP:MGFit:Init_as
	NVAR MaxN_peaks=root:Package_SLP:MGFit:MaxN_peaks
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Wave /T SLP_MGFit_DataListDisplayW=root:Package_SLP:MGFit:SLP_MGFit_DataListDisplayW
	Wave SLP_MGFit_DataListDisplaySelW=root:Package_SLP:MGFit:SLP_MGFit_DataListDisplaySelW
	
	Variable N_Data=dimsize(SLP_MGFit_DataListW,0)
	
	NVAR GDataRow=root:Package_SLP:MGFit:GDataRow
	
	String DataW_Name=StringFromList(itemsinlist(DataW_FName,":")-1,DataW_FName,":")
	Wave DataW=$DataW_Name
	String DataFolder_Name=RemoveEnding(DataW_FName,DataW_Name)
	Variable N_Peaks=0
	Variable Peask_Sel=0
	
	Prompt DataW_Name,"Enter data name"
	DoPrompt "Add data wave",DataW_Name
	
	//Save everything in Package folder (separately between different data)
	String NewDataFolder_Name="root:Package_SLP:MGFit:"+DataW_Name
	
	//Check if the folder to keep waves already exists
	Variable run_flg=0
	String FolderExistsWarning="Enter new data name?"
	If (!DataFolderExists(NewDataFolder_Name))
		run_flg=1
	Else
		DoAlert 1,"Adding "+DataW_Name+". Data already exists. Overwrite?"
		If (V_Flag==1) //Click yes
			ListBox ListBox_PeakParam ListWave=root:Package_SLP:MGFit:SLP_MGFit_iCoefGuessDisplayW,SelWave=root:Package_SLP:MGFit:SLP_MGFit_iCoefGuessDisplaySelW
			SLP_Util_RemoveAllTraces("SLP_MultiGLAFit_01#G0")
			SLP_MGFit_DeleteData(DataW_Name)
			N_Data-=1
			run_flg=1
		Else //Click no
			Do
				DoAlert 1,FolderExistsWarning
				If (V_Flag==1)
					Prompt DataW_Name,"New data name:"
					DoPrompt "Enter new data name",DataW_Name
					run_flg=1
				Else
					run_flg=0
				EndIf
				NewDataFolder_Name="root:Package_SLP:MGFit:"+DataW_Name
				FolderExistsWarning="Still duplicated name. Enter new data name?"
			While (run_flg!=0&&DataFolderExists(NewDataFolder_Name))
		Endif
	EndIf
	
	If (run_flg)
		NewDataFolder /O  $NewDataFolder_Name
		
		Redimension /N=(dimsize(SLP_MGFit_DataListW,0)+1,64)  SLP_MGFit_DataListW
		SLP_MGFit_DataListW[N_Data][0]=DataW_Name
		SLP_MGFit_DataListW[N_Data][1]=NewDataFolder_Name+":"
		SLP_MGFit_DataListW[N_Data][2]=DataW_FName //Original wave name and location
		SLP_MGFit_DataListW[N_Data][3]="0"
		SLP_MGFit_DataListW[N_Data][4]="0"
		SLP_MGFit_DataListW[N_Data][5]="None" //Background for subtraction
		SLP_MGFit_DataListW[N_Data][17]="0" //Fit display flag
		SLP_MGFit_DataListW[N_Data][18]=num2str(Dimoffset(DataW,0))
		SLP_MGFit_DataListW[N_Data][19]=num2str(Dimoffset(DataW,0)+DimDelta(DataW,0)*(DimSize(DataW,0)-1))
		
		Redimension /N=(dimsize(SLP_MGFit_DataListDisplayW,0)+1,16)  SLP_MGFit_DataListDisplayW
		SLP_MGFit_DataListDisplayW[N_Data][1]=DataW_Name
		Redimension /N=(dimsize(SLP_MGFit_DataListDisplaySelW,0)+1,16)  SLP_MGFit_DataListDisplaySelW
		SLP_MGFit_DataListDisplaySelW[N_Data][0]=32
		SLP_MGFit_DataListDisplaySelW[N_Data][2]=2
		
		String DataW_Copy_FName=NewDataFolder_Name+":"+DataW_Name
		String DataW_BG_FName=DataW_Copy_FName+"_BG"
		String DataW_nBG_FName=DataW_Copy_FName+"_nBG"
		String FitW_FName=DataW_nBG_FName+"_Fit"
		String HilightW_FName=NewDataFolder_Name+":HiLightW"
	
		Duplicate /O $DataW_FName,$DataW_Copy_FName
		Duplicate /O $DataW_Copy_FName,$DataW_BG_FName /Wave=DataW_BG
		Duplicate /O $DataW_Copy_FName,$DataW_nBG_FName /Wave=DataW_nBG
		Duplicate /O $DataW_Copy_FName,$FitW_FName /Wave=FitW
		Duplicate /O $DataW_Copy_FName,$HilightW_FName /Wave=HilightW
		DataW_BG=0
		FitW=0
		//DataW_nBG=0
		HilightW=0
		
		String CoefGuessDispW_FName=NewDataFolder_Name+":CoefGuessDispW"
		Duplicate /T/O root:Package_SLP:MGFit:SLP_MGFit_iCoefGuessDisplayW,$CoefGuessDispW_FName /Wave=CoefGuessDispW
		CoefGuessDispW[][3]=num2str(Init_int)
		//CoefGuessDispW[][4]=num2str(Dimoffset(DataW,0)+DimDelta(DataW,0)*DimSize(DataW,0)/2) //init = center
		CoefGuessDispW[][4]=num2str(Dimoffset(DataW,0)) //init = min X
		CoefGuessDispW[][5]=num2str(Init_fwhm)
		CoefGuessDispW[][6]=num2str(Init_mix)
		CoefGuessDispW[][7]=num2str(Init_as)
		
		String CoefGuessDispSelW_FName=NewDataFolder_Name+":CoefGuessDispSelW"
		Duplicate /O root:Package_SLP:MGFit:SLP_MGFit_iCoefGuessDisplaySelW,$CoefGuessDispSelW_FName /Wave=CoefGuessDispSelW
		SetDimLabel 2,1,ForeColors,CoefGuessDispSelW				// define plane 1 as Foreground colors
		
		String CoefGuessM_FName=NewDataFolder_Name+":CoefGuessM"
		Make /O/N=(0,5) $CoefGuessM_FName /Wave=CoefGuessM
		//CoefGuessM[][0]=Init_pos
	//	CoefGuessM[][0]=Init_int
	//	CoefGuessM[][1]=(Dimoffset(DataW,0)+DimDelta(DataW,0)*DimSize(DataW,0)/2)
	//	CoefGuessM[][2]=Init_fwhm
	//	CoefGuessM[][3]=Init_mix
	//	CoefGuessM[][4]=Init_as
		
		String ConstraintW_FName=NewDataFolder_Name+":ConstraintW"
		Make /O/N=(5,3,MaxN_peaks) $ConstraintW_FName /Wave=ConstraintW
		ConstraintW[0][0][]=0 //min Int=0
		ConstraintW[2][0][]=0.1 //min FWHM=0.1 eV
		ConstraintW[3][0][]=0.001 //min mix=0.001
		ConstraintW[3][1][]=0.999 //max mix=0.999
		ConstraintW[4][0][]=0.001 //min as=0.001
		ConstraintW[4][1][]=0.999 //min as=0.999
		ConstraintW[][2][]=0 //Hold = 0
		
		String DefaultConstrintW_FName=NewDataFolder_Name+":DefaultConstraint"
	EndIf
End

Function ButtonProc_MGFit_DeleteData(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			DoAlert 1,"Delete "+DataName+"?"
			If (V_Flag==1)
				ListBox ListBox_PeakParam ListWave=root:Package_SLP:MGFit:SLP_MGFit_iCoefGuessDisplayW,SelWave=root:Package_SLP:MGFit:SLP_MGFit_iCoefGuessDisplaySelW
				SLP_Util_RemoveAllTraces("SLP_MultiGLAFit_01#G0")
				SLP_MGFit_DeleteData(DataName)
			EndIf
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_LoadData(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			String SelectedWaveFName=""
			Execute "CreateBrowser prompt=\"Select data wave\", showWaves=1, showVars=0, showStrs=0" 
			//Old style. in Igor7 no need to use Execute
			SVAR S_BrowserList=S_BrowserList
			NVAR V_Flag=V_Flag
			if(V_Flag!=0)
				SelectedWaveFName=stringfromlist(0,S_BrowserList)
				SLP_MGFit_AddDataW(SelectedWaveFName)
				Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
				Variable NewDataRow=Dimsize(SLP_MGFit_DataListW,0)-1
				ListBox ListBox_DataList win=SLP_MultiGLAFit_01,SelRow=NewDataRow//Select the new one
				String DataName=SLP_MGFit_DataListW[NewDataRow][0]
				SLP_MGFit_DisplaySelectedData(DataName)
			endif
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_Done(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			Dowindow /K SLP_MultiGLAFit_01
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function SLP_MGFit_DeleteData(DataName)
	String DataName
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Wave /T SLP_MGFit_DataListDisplayW=root:Package_SLP:MGFit:SLP_MGFit_DataListDisplayW
	Wave SLP_MGFit_DataListDisplaySelW=root:Package_SLP:MGFit:SLP_MGFit_DataListDisplaySelW
	
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	
	String DataFolder_FName=SLP_MGFit_DataListW[DataRow][1]
	
	KillDataFolder $DataFolder_FName
	Variable DeleteFolderError_Flg=V_Flag
	
	If (DeleteFolderError_Flg==0)
		DeletePoints DataRow,1,SLP_MGFit_DataListW
		DeletePoints DataRow,1,SLP_MGFit_DataListDisplayW
		DeletePoints DataRow,1,SLP_MGFit_DataListDisplaySelW
	EndIf
End

Function SLP_MGFit_Panel_DefConstraints() : Panel
	DoWindow /K MGFit_DefaultConstraint
	
	PauseUpdate; Silent 1		// building window...
	
	NVAR PosRange=root:Package_SLP:MGFit:PosRange
	NVAR Pos_pm_flg=root:Package_SLP:MGFit:Pos_pm_flg
	
	NewPanel /W=(340,110,550,300) /N=MGFit_DefaultConstraint
	
	Wave SLP_MGFit_DefaultConstraintSelW=root:Package_SLP:MGFit:SLP_MGFit_DefaultConstraintSelW
	
	SetDrawLayer UserBack
	DrawText 9,25,"Default constraints"
	ListBox ListBox_DefConstraints,pos={9,52},size={193,110}
	ListBox ListBox_DefConstraints,listWave=root:Package_SLP:MGFit:SLP_MGFit_DefaultConstraintW
	ListBox ListBox_DefConstraints,selWave=root:Package_SLP:MGFit:SLP_MGFit_DefaultConstraintSelW
	ListBox ListBox_DefConstraints,widths={57,41,40,33,7,32,32,32}
	ListBox ListBox_DefConstraints,userColumnResize= 1
	Button button_done,pos={152,166},size={50,20},title="done",proc=ButtonProc_MGFit_ConstraintDone
	CheckBox check_PosPercent,pos={13,31},size={69,15},title="Position (+/-)",variable= Pos_pm_flg,proc=CheckProc_MGFit_Pos_pm_flg
	SetVariable setvar_PlusMinus,pos={100,30},size={50,15},limits={-inf,inf,0},variable=PosRange,title=" ",disable=!Pos_pm_flg
	
	SLP_MGFit_DefaultConstraintSelW[1][1,2]=!Pos_pm_flg*2
	
End

Function ButtonProc_MGFit_DefConstraints(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
			Variable PeakRow=V_Value
			
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			Wave /T SLP_MGFit_ConstraintDisplayW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW
			Wave  SLP_MGFit_ConstraintDisplaySelW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplaySelW
			Wave /T SLP_MGFit_DefaultConstraintW=root:Package_SLP:MGFit:SLP_MGFit_DefaultConstraintW
			Wave SLP_MGFit_DefaultConstraintSelW=root:Package_SLP:MGFit:SLP_MGFit_DefaultConstraintSelW
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			String DataLocation=SLP_MGFit_DataListW[DataRow][1]
			String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
			Wave /T CoefGuessDispW=$CoefGuessDispW_FName
			Duplicate /O/T SLP_MGFit_DefaultConstraintW,SLP_MGFit_ConstraintDisplayW
			Duplicate /O SLP_MGFit_DefaultConstraintSelW,SLP_MGFit_ConstraintDisplaySelW
			
			NVAR PosRange=root:Package_SLP:MGFit:PosRange
			NVAR Pos_pm_flg=root:Package_SLP:MGFit:Pos_pm_flg
			
			If (Pos_pm_flg)
				SLP_MGFit_ConstraintDisplayW[1][1]=num2str(str2num(CoefGuessDispW[PeakRow][4])-PosRange)
				SLP_MGFit_ConstraintDisplayW[1][2]=num2str(str2num(CoefGuessDispW[PeakRow][4])+PosRange)
			EndIf
			SLP_MGFit_ConstraintDisplaySelW[1][1][0]=2
			SLP_MGFit_ConstraintDisplaySelW[1][2][0]=2
			
			SLP_MGFit_Listbox2ConstraintW(DataName)
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End



Function SLP_MGFit_DisplaySelectedData(DataName)
	String DataName

	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	//Wave /T SLP_MGFit_CoefGuessDisplayW=root:Package_SLP:MGFit:SLP_MGFit_CoefGuessDisplayW
	Wave /T SLP_MGFit_ConstraintDisplayW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW
	
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
	String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
	String CoefGuessM_FName=DataLocation+"CoefGuessM"
	String ConstraintW_FName=DataLocation+"ConstraintW"
	String HilightW_FName=DataLocation+"HilightW"
	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName
	Wave CoefGuessM=$CoefGuessM_FName
	Wave ConstraintW=$ConstraintW_FName
	
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
	Variable PeakRow=V_Value
	
	String DataW_FName=DataLocation+DataName
	String BgW_FName=DataW_FName+"_BG"
	String Data_nBGW_FName=DataW_FName+"_nBG"
	Wave DataW=$DataW_FName
	Wave Data_nBGW=$Data_nBGW_FName
	
	ListBox ListBox_PeakParam ListWave= $CoefGuessDispW_FName, SelWave=$CoefGuessDispSelW_FName, SelRow=str2num(SLP_MGFit_DataListW[DataRow][4])
	
	SLP_MGFit_ConstraintW2Listbox(DataName)
	
	String WavesToBeDisplayed=""
	String DisplayedWaves_NameList=""
	String TraceList=""
	String TraceName=""
	
	Variable i=0
	Variable j=0
	String CoefW_FName=""
	String Index_str=""
	String ComponentW_FName=""
	String SelPeak_Name=""
	
	//FORMAT THE GRAPH and TRACE
	//Clear drawings
	setdrawlayer /W=SLP_MultiGLAFit_01#G0 UserFront
	drawaction /W=SLP_MultiGLAFit_01#G0  delete
	DoUpdate
	
	Variable FitDisplay_Flg=str2num(SLP_MGFit_DataListW[DataRow][17])
	If (FitDisplay_Flg==0) //Display Guess curves
	
		DisplayedWaves_NameList=SLP_MGFit_CreateComponents(DataName)
		
		//Create highlighted peak
		SelPeak_Name=StringFromList(PeakRow+1,DisplayedWaves_NameList,";") //Selected peak name
		If (Exists(SelPeak_Name))
			Duplicate /O $SelPeak_Name,$HilightW_FName /Wave=HilightW
		EndIf
		
		//Display the wave on graph
		WavesToBeDisplayed=HilightW_FName+";"+Data_nBGW_FName+";"+DisplayedWaves_NameList
		SLP_Util_AddnRmTraces("SLP_MultiGLAFit_01#G0",WavesToBeDisplayed,0)

		//Modify the graph and trace colors
		
		TraceList=tracenamelist("SLP_MultiGLAFit_01#G0",";",1)
		TraceName=StringFromList(0,TraceList,";") //Filled BG for selected peak
		ModifyGraph /W=SLP_MultiGLAFit_01#G0  mode($TraceName)=7,hbFill($TraceName)=2,rgb($TraceName)=(65535,49151,49151)
		TraceName=StringFromList(1,TraceList,";") //Data Wave (BG subtracted)
		Modifygraph /W=SLP_MultiGLAFit_01#G0 mode($TraceName)=3, Marker($TraceName)=16, rgb($TraceName)=(43690,43690,43690)
		TraceName=StringFromList(2,TraceList,";") //Guess Wave
		ModifyGraph /W=SLP_MultiGLAFit_01#G0 rgb($TraceName)=(1,34817,52428), lsize($TraceName)=3
		TraceName=StringFromList(PeakRow+3,TraceList,";") //Selected peak
		ModifyGraph /W=SLP_MultiGLAFit_01#G0  rgb($TraceName)=(65535,43690,0),lsize($TraceName)=1.5
	
	Elseif (FitDisplay_Flg==1)	//Display fit results
		
		//Make a list of waves to be displayed
		String FitW_FName=Data_nBGW_FName+"_Fit"
		WavesToBeDisplayed=HilightW_FName+";"+Data_nBGW_FName+";"+FitW_FName+";" //Hilight - Data nBG - Fit
		
		For (i=0;i<dimsize(CoefGuessDispW,0);i+=1)
			If (CoefGuessDispSelW[i][0]==48)
				sprintf Index_str,"%02d",j
				ComponentW_FName=Data_nBGW_FName+"_f"+index_str //Add fit components
				WavesToBeDisplayed+=ComponentW_FName+";"				
				j+=1
			EndIf
		EndFor
		
		//Create highlighted peak
		SelPeak_Name=StringFromList(PeakRow+3,WavesToBeDisplayed,";") //Selected peak name
		If (Exists(SelPeak_Name))
			Duplicate /O $SelPeak_Name,$HilightW_FName /Wave=HilightW
		EndIf
	
		SLP_Util_AddnRmTraces("SLP_MultiGLAFit_01#G0",WavesToBeDisplayed,0)
		
		TraceList=tracenamelist("SLP_MultiGLAFit_01#G0",";",1)
		TraceName=StringFromList(0,TraceList,";") //Filled BG for selected peak
		ModifyGraph /W=SLP_MultiGLAFit_01#G0  mode($TraceName)=7,hbFill($TraceName)=2,rgb($TraceName)=(40000,50000,40000)
		TraceName=StringFromList(1,TraceList,";") //Data Wave (BG subtracted)
		Modifygraph /W=SLP_MultiGLAFit_01#G0 mode($TraceName)=3, Marker($TraceName)=16, rgb($TraceName)=(43690,43690,43690)
		TraceName=StringFromList(2,TraceList,";") //Fit Wave
		ModifyGraph /W=SLP_MultiGLAFit_01#G0 rgb($TraceName)=(0,35000,0), lsize($TraceName)=3
		TraceName=StringFromList(PeakRow+3,TraceList,";") //Selected peak
		ModifyGraph /W=SLP_MultiGLAFit_01#G0  rgb($TraceName)=(65535,43690,0),lsize($TraceName)=1.5
		
	EndIf
	
	ModifyGraph /W=SLP_MultiGLAFit_01#G0 margin(left)=28,margin(bottom)=28, margin(top)=9,margin(right)=9
	Modifygraph /W=SLP_MultiGLAFit_01#G0 mirror=1,minor=1,tick=2
	//Orginal data w (before BG) and BG waves display
	Wavestats /Q Data_nBGW
	Variable OrgData_yOffset=(V_Max-V_Min)*1.10 //Determine offset value from the data y-range
	ControlInfo /W=SLP_MultiGLAFit_01 Check_DisplayOriginal; variable DisplayOrg_Flg=V_Value
	If (DisplayOrg_Flg)
		appendtograph /W=SLP_MultiGLAFit_01#G0 $DataW_FName
		TraceName=StringFromList(ItemsInlist(TraceNameList("SLP_MultiGLAFit_01#G0",";",1))-1,TraceNameList("SLP_MultiGLAFit_01#G0",";",1))
		ModifyGraph /W=SLP_MultiGLAFit_01#G0 offset($TraceName)={0,OrgData_yOffset},rgb($TraceName)=(30000,30000,30000)
	EndIf
	ControlInfo /W=SLP_MultiGLAFit_01 Check_DisplayBG; variable DisplayBg_Flg=V_Value
	If (DisplayBg_Flg)
		appendtograph /W=SLP_MultiGLAFit_01#G0 $BgW_FName
		TraceName=StringFromList(ItemsInlist(TraceNameList("SLP_MultiGLAFit_01#G0",";",1))-1,TraceNameList("SLP_MultiGLAFit_01#G0",";",1))
		ModifyGraph /W=SLP_MultiGLAFit_01#G0 offset($TraceName)={0,OrgData_yOffset}, lstyle($TraceName)=2,rgb($TraceName)=(30000,30000,30000)
	EndIf
	
	//Slider controls
	Variable MinX=dimoffset(DataW,0)
	Variable MaxX=dimoffset(DataW,0)+(dimsize(DataW,0)-1)*dimdelta(DataW,0)
	Variable StepX=dimdelta(DataW,0)
	Slider slider_Pos, limits={MinX,MaxX,StepX/4}
	WaveStats /Q DataW
	Slider slider_Int, limits={0,V_Max,V_Max/400}
	Slider slider_Fwhm, limits={0.01,MaxX-MinX,(MaxX-MinX)/100}
	
	Slider Slider_Int, value=str2num(CoefGuessDispW[PeakRow][3])
	Slider Slider_Pos, value=str2num(CoefGuessDispW[PeakRow][4])
	Slider Slider_Fwhm, value=str2num(CoefGuessDispW[PeakRow][5])
	Slider Slider_Mix, value=str2num(CoefGuessDispW[PeakRow][6])
	Slider Slider_As, value=str2num(CoefGuessDispW[PeakRow][7])
	
	//Popup
	Variable BG_Sel=whichlistitem(SLP_MGFit_DataListW[DataRow][5],"None;Shirley;Erf;atan;Flat;Slope")+1
	PopupMenu popup_BgSel, mode=BG_Sel
	
	SLP_MGFit_ConstraintLogic(DataName)
	
	NVAR Gss_ChiSq=root:Package_SLP:MGFit:Gss_ChiSq
	NVAR Fit_ChiSq=root:Package_SLP:MGFit:Fit_ChiSq
	
	Gss_ChiSq=str2num(SLP_MGFit_DataListW[DataRow][15])
	Fit_ChiSq=str2num(SLP_MGFit_DataListW[DataRow][16])
	
	//Checkbox
	CheckBox check_DispFit value=str2num(SLP_MGFit_DataListW[DataRow][17])
	
	NVAR LowX=root:Package_SLP:MGFit:LowX
	NVAR HighX=root:Package_SLP:MGFit:HighX
	LowX=str2num(SLP_MGFit_DataListW[DataRow][18])
	HighX=str2num(SLP_MGFit_DataListW[DataRow][19])
	
	SLP_MGFit_ReadAreaUnderCurve(DataName)
	
	SLP_MGFit_CalculateARatio(DataName)
	
End

Function SLP_MGFit_Listbox2ConstraintW(DataName) //Copy user's inputs in Listbox to ConstraintW volume of the fitting
	String DataName
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
	String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
	String CoefGuessM_FName=DataLocation+"CoefGuessM"
	String ConstraintW_FName=DataLocation+"ConstraintW"
	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName
	Wave CoefGuessM=$CoefGuessM_FName
	Wave ConstraintW=$ConstraintW_FName
	
	Wave /T SLP_MGFit_ConstraintDisplayW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW
	Wave SLP_MGFit_ConstraintDisplaySelW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplaySelW
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
	Variable PeakRow=V_Value
	//DoUpdate
	ConstraintW[][0][PeakRow]=str2num(SLP_MGFit_ConstraintDisplayW[p][1])
	ConstraintW[][1][PeakRow]=str2num(SLP_MGFit_ConstraintDisplayW[p][2])
	ConstraintW[][2][PeakRow]=(SLP_MGFit_ConstraintDisplaySelW[p][3][0]==48)
	//edit ConstraintW
	//DoUpdate
	SLP_MGFit_ConstrW2ConstrV(DataName)
End

Function CheckProc_MGFit_DisplayOrgBG(cba) : CheckBoxControl
	STRUCT WMCheckboxAction &cba

	switch( cba.eventCode )
		case 2: // mouse up
			Variable checked = cba.checked
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			SLP_MGFit_DisplaySelectedData(DataName)
			
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function SliderProc_MGFit_CoefAdjust(sa) : SliderControl
	STRUCT WMSliderAction &sa

	switch( sa.eventCode )
		case -1: // control being killed
			break
		default:
			if( sa.eventCode & 1 ) // value set
				Variable curval = sa.curval
				
				ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
				Variable DataRow=V_Value
				ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
				Variable PeakRow=V_Value
				ControlInfo /W=SLP_MultiGLAFit_01 check_DispFit
				Variable FitResultChecked=V_Value
				
				Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
				String DataLocation=SLP_MGFit_DataListW[DataRow][1]
				String DataName=SLP_MGFit_DataListW[DataRow][0]
				
				If (FitResultChecked)
					SLP_MGFit_DataListW[DataRow][17]="0"
					SLP_MGFit_DisplaySelectedData(DataName)
					DoUpdate
				EndIf
				
				String DataW_FName=DataLocation+DataName
				String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
				Wave /T CoefGuessDispW=$CoefGuessDispW_FName
				String GssCurveW_FName=DataW_FName+"_Gss"
				Wave GssCurveW=$GssCurveW_FName
				
				ControlInfo /W=SLP_MultiGLAFit_01 Slider_Int
				Variable Int=V_Value
				ControlInfo /W=SLP_MultiGLAFit_01 Slider_Pos
				Variable Pos=V_Value
				ControlInfo /W=SLP_MultiGLAFit_01 Slider_Fwhm
				Variable Fwhm=V_Value
				ControlInfo /W=SLP_MultiGLAFit_01 Slider_Mix
				Variable Mix=V_Value
				ControlInfo /W=SLP_MultiGLAFit_01 Slider_AS
				Variable As=V_Value
				
				CoefGuessDispW[Peakrow][3]=num2str(Int)
				CoefGuessDispW[Peakrow][4]=num2str(Pos)
				CoefGuessDispW[Peakrow][5]=num2str(Fwhm)
				CoefGuessDispW[Peakrow][6]=num2str(Mix)
				CoefGuessDispW[Peakrow][7]=num2str(As)
				
				SLP_MGFit_UpdatePeakComponent()
				
				SLP_MGFit_ConstraintLogic1p(DataName,PeakRow)	
				
				SLP_MGFit_ReadAreaUnder1GssPeak(DataName,PeakRow)
				
				SLP_MGFit_CalculateARatio(DataName)
			endif
			break
	endswitch

	return 0
End

Function SLP_MGFit_UpdatePeakComponent()
	
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
	Variable DataRow=V_Value
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
	Variable PeakRow=V_Value
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String DataName=SLP_MGFit_DataListW[DataRow][0]
	String DataW_FName=DataLocation+DataName
	String DataW_nBG_FName=DataW_FName+"_nBG"
	String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	String GssCurveW_FName=DataW_FName+"_Gss"
	Wave GssCurveW=$GssCurveW_FName
	
	Variable int=str2num(CoefGuessDispW[Peakrow][3])
	Variable Pos=str2num(CoefGuessDispW[Peakrow][4])
	Variable Fwhm=str2num(CoefGuessDispW[Peakrow][5])
	Variable Mix=str2num(CoefGuessDispW[Peakrow][6])
	Variable As=str2num(CoefGuessDispW[Peakrow][7])
	
	String ComponentW_FName=DataW_FName+"_p"+num2str(PeakRow)
	If (Exists(ComponentW_FName))
		Wave ComponentW=$ComponentW_FName
		GssCurveW-=ComponentW		//Subtract the component from the guess W first
		ComponentW=SLP_GLA_Func(Int,Pos,Fwhm,Mix,As,x)
		GssCurveW+=ComponentW		//Add it back to the Gss W
	EndIf

	NVAR Gss_Chisq=root:Package_SLP:MGFit:Gss_Chisq
	Gss_Chisq=SLP_MGFit_CalcChiSq(GssCurveW_FName,DataW_nBG_FName)
	SLP_MGFit_DataListW[DataRow][15]=num2str(Gss_Chisq)
	
	SLP_MGFit_ReadAreaUnderCurve(DataName)
End

Function SLP_MGFit_DrawVLine(x)
	variable x
	setdrawlayer /W=SLP_MultiGLAFit_01#G0 UserFront
	drawaction /W=SLP_MultiGLAFit_01#G0  delete
	SetDrawEnv /W=SLP_MultiGLAFit_01#G0 xcoord= bottom,ycoord= left,linefgc=(50000,20000,20000 ),dash=2
	GetAxis /W=SLP_MultiGLAFit_01#G0 /Q left
	drawline /W=SLP_MultiGLAFit_01#G0 x,V_min,x,V_max

End

Function SLP_MGFit_ConstraintW2Listbox(DataName) //Copy ConstraintW values to the Constraint ListBox on the panel
	String DataName
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
	Variable PeakRow=V_Value
	
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
	String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
	String CoefGuessM_FName=DataLocation+"CoefGuessM"
	String ConstraintW_FName=DataLocation+"ConstraintW"
	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName
	Wave CoefGuessM=$CoefGuessM_FName
	Wave ConstraintW=$ConstraintW_FName
	
	Wave /T SLP_MGFit_ConstraintDisplayW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplayW
	Wave SLP_MGFit_ConstraintDisplaySelW=root:Package_SLP:MGFit:SLP_MGFit_ConstraintDisplaySelW
	
	SLP_MGFit_ConstraintDisplayW[][1]=num2str(ConstraintW[p][0][PeakRow])
	SLP_MGFit_ConstraintDisplayW[][2]=num2str(ConstraintW[p][1][PeakRow])
	SLP_MGFit_ConstraintDisplaySelW[][3][0]=(ConstraintW[p][2][PeakRow]==1)*16+32 //If true = put 48 (checkbox is checked). If faulse = put 32 (checkbox is unchecked)
	
End

Function ListBoxProc_MGFit_AreaRatio(lba) : ListBoxControl
	STRUCT WMListboxAction &lba

	Variable row = lba.row
	Variable col = lba.col
	WAVE/T/Z listWave = lba.listWave
	WAVE/Z selWave = lba.selWave

	switch( lba.eventCode )
		case -1: // control being killed
			break
		case 1: // mouse down
			break
		case 3: // double click
			break
		case 4: // cell selection
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			SLP_MGFit_CalculateARatio(DataName)
		case 5: // cell selection plus shift key
			break
		case 6: // begin edit
			break
		case 7: // finish edit
			break
		case 13: // checkbox clicked (Igor 6.2 or later)
			break
	endswitch

	return 0
End

Function SLP_MGFit_ReadAreaUnderCurve(DataName)
	String DataName
	NVAR LowX=root:Package_SLP:MGFit:LowX
	NVAR HighX=root:Package_SLP:MGFit:HighX
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	
	
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String DataW_FName=DataLocation+DataName	//Raw data
	String BgW_FName=DataW_FName+"_BG"		//Background
	String Data_nBGW_FName=DataW_FName+"_nBG"	//Data after BG subtraction
	String Data_GssW_FName=DataW_FName+"_Gss"	//Guess curve
	String FitW_FName=Data_nBGW_FName+"_Fit"		//Fit curve
	wave DataW=$DataW_FName
	wave BgW=$BgW_FName
	
	Wave /T SLP_MGFit_CurveAreaW=root:Package_SLP:MGFit:SLP_MGFit_CurveAreaW
	
	SLP_MGFit_CurveAreaW[0][]={{"Data wave area"},{"Data wave after BG subtraction area"},{"Data wave after flat BG subtraction area"},{"Guess curve area"},{"Fit curve area"},{"BG area"},{"BG after flat BG area"}}
	SLP_MGFit_CurveAreaW[1][0]={num2str(area($DataW_FName,LowX,HighX))}
	SLP_MGFit_CurveAreaW[1][1]={num2str(area($Data_nBGW_FName,LowX,HighX))}
	SLP_MGFit_CurveAreaW[1][2]={num2str(area($DataW_FName,LowX,HighX)-(DataW[0]*(HighX-LowX)))}
	SLP_MGFit_CurveAreaW[1][3]={num2str(area($Data_GssW_FName,LowX,HighX))}
	SLP_MGFit_CurveAreaW[1][4]={num2str(area($FitW_FName,LowX,HighX))}
	SLP_MGFit_CurveAreaW[1][5]={num2str(area($BgW_FName,LowX,HighX))}
	SLP_MGFit_CurveAreaW[1][6]={num2str(area($BgW_FName,LowX,HighX)-(BgW[0]*(HighX-LowX)))}
End

Function  SLP_MGFit_CalculateARatio(DataName)
	String DataName
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	Wave /T SLP_MGFit_PeakAreaW=root:Package_SLP:MGFit:SLP_MGFit_PeakAreaW
	Wave /T SLP_MGFit_CurveAreaW=root:Package_SLP:MGFit:SLP_MGFit_CurveAreaW

	Variable PeakCol,PeakRow,CurveCol,CurveRow
	ControlInfo /W=SLP_MultiGLAFit_01 list_PeakArea
	PeakRow=V_Value
	PeakCol=V_selCol
	ControlInfo /W=SLP_MultiGLAFit_01 list_CurveArea
	CurveRow=V_Value
	CurveCol=V_selCol
	
	NVAR AreaRatio=root:Package_SLP:MGFit:AreaRatio
	
	AreaRatio=str2num(SLP_MGFit_PeakAreaW[PeakRow][PeakCol])/str2num(SLP_MGFit_CurveAreaW[CurveRow][CurveCol])
	
End

Function  SLP_MGFit_ReadAreaUnder1GssPeak(DataName,PeakRow)
	String DataName
	Variable PeakRow
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	Wave /T SLP_MGFit_PeakAreaW=root:Package_SLP:MGFit:SLP_MGFit_PeakAreaW
	
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String DataW_FName=DataLocation+DataName	//Raw data
	wave DataW=$DataW_FName
	
	String BgW_FName=DataW_FName+"_BG"		//Background
	String Data_nBGW_FName=DataW_FName+"_nBG"	//Data after BG subtraction
	
	String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"

	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	
	duplicate /O  DataW,root:Package_SLP:MGFit:PeakAreaCalW /Wave=PeakAreaCalW
	PeakAreaCalW=0
	
	Variable Int,Pos,Fwhm,Mix,As

	int=str2num(CoefGuessDispW[PeakRow][3])
	Pos=str2num(CoefGuessDispW[PeakRow][4])
	Fwhm=str2num(CoefGuessDispW[PeakRow][5])
	Mix=str2num(CoefGuessDispW[PeakRow][6])
	As=str2num(CoefGuessDispW[PeakRow][7])

	PeakAreaCalW=SLP_GLA_Func(Int,Pos,Fwhm,Mix,As,x)
	SLP_MGFit_PeakAreaW[1][PeakRow]=num2str(area(PeakAreaCalW))
		
	Killwaves PeakAreaCalW
End

Function  SLP_MGFit_ReadAreaUnderPeaks(DataName)
	String DataName
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	Wave /T SLP_MGFit_PeakAreaW=root:Package_SLP:MGFit:SLP_MGFit_PeakAreaW
	
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String DataW_FName=DataLocation+DataName	//Raw data
	wave DataW=$DataW_FName
	
	String BgW_FName=DataW_FName+"_BG"		//Background
	String Data_nBGW_FName=DataW_FName+"_nBG"	//Data after BG subtraction
	String Data_GssW_FName=DataW_FName+"_Gss"	//Guess curve
	String PAreaW_FName=Data_nBGW_FName+"_PArea" //peak area from fitting (only selected peak(s))
	Wave PAreaW=$PAreaW_FName
	
	String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
	String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName
	String FitPeakFName=""
	String Index_str=""
	duplicate /O  DataW,root:Package_SLP:MGFit:PeakAreaCalW /Wave=PeakAreaCalW
	PeakAreaCalW=0
	
	Variable Int,Pos,Fwhm,Mix,As
	
	Variable i=0
	Variable j=0
	For (i=0;i<dimsize(CoefGuessDispW,0);i+=1)
		//Guess peaks
		
		int=str2num(CoefGuessDispW[i][3])
		Pos=str2num(CoefGuessDispW[i][4])
		Fwhm=str2num(CoefGuessDispW[i][5])
		Mix=str2num(CoefGuessDispW[i][6])
		As=str2num(CoefGuessDispW[i][7])
	
		PeakAreaCalW=SLP_GLA_Func(Int,Pos,Fwhm,Mix,As,x)
		SLP_MGFit_PeakAreaW[1][i]=num2str(area(PeakAreaCalW))
		
		//Fit peaks
		If (CoefGuessDispSelW[i][0]==48)
			sprintf Index_str,"%02d",j
			FitPeakFName=Data_nBGW_FName+"_f"+Index_str
			SLP_MGFit_PeakAreaW[2][i]=num2str(PAreaW[j])
			j+=1
		EndIf
	EndFor
	Killwaves PeakAreaCalW
End

Function /T SLP_MGFit_CreateComponents(DataName)
	String DataName
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
	Variable PeakRow=V_Value
	Variable N_Peaks=str2num(SLP_MGFit_DataListW[DataRow][3])
	
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
	String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
	String CoefGuessM_FName=DataLocation+"CoefGuessM"
	String ConstraintW_FName=DataLocation+"ConstraintW"
	Wave /T CoefGuessDispW=$CoefGuessDispW_FName
	Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName
	Wave CoefGuessM=$CoefGuessM_FName
	Wave ConstraintW=$ConstraintW_FName
	
	String ComponentW_FName=""
	String ComponentW_NameList=""
	
	variable i=0
	
	String DataW_FName=DataLocation+DataName
	Wave DataW=$DataW_FName
	
	String GssCurveW_FName=DataW_FName+"_Gss"
	Duplicate /O DataW,$GssCurveW_FName /wave=GssCurveW
	GssCurveW=0
	
	For (i=0;i<dimsize(CoefGuessDispW,0);i+=1)
		If (CoefGuessDispSelW[i][0][0]==48)
			ComponentW_FName=DataW_FName+"_p"+num2str(i)
			Duplicate /O DataW,$ComponentW_FName /wave=ComponentW
			ComponentW=SLP_GLA_Func(str2num(CoefGuessDispW[i][3]),str2num(CoefGuessDispW[i][4]),str2num(CoefGuessDispW[i][5]),str2num(CoefGuessDispW[i][6]),str2num(CoefGuessDispW[i][7]),x)
			ComponentW_NameList+=ComponentW_FName+";"
			GssCurveW+=ComponentW
		Endif
	EndFor
	
	ComponentW_NameList=GssCurveW_FName+";"+ComponentW_NameList
	
	Return ComponentW_NameList
End

Function SLP_MGFit_CalcChiSq(ObsW_FName,ExpW_FName)
	String ObsW_FName,ExpW_FName
	
	Wave ObsW=$ObsW_FName
	Wave ExpW=$ExpW_FName
	Variable ChiSq=0
	
	If (dimoffset(ObsW,0)==dimoffset(ExpW,0) && dimdelta(ObsW,0)==dimdelta(ExpW,0) && dimsize(ObsW,0)==dimsize(ExpW,0))
		Variable i=0
		For (i=0;i<dimsize(ObsW,0);i+=1)
			ChiSq+=(ObsW[i]-ExpW[i])^2
		EndFor
	Else
		print "Aborted. Wave 1 and 2 have different x-scaling"
	EndIf
	Return ChiSq
End

Function ButtonProc_MGFit_Fit(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
			Variable DataRow=V_Value
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
			Variable PeakRow=V_Value
			
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			
			String DataName=SLP_MGFit_DataListW[DataRow][0]
			String DataLocation=SLP_MGFit_DataListW[DataRow][1]
			
			String CoefGuessDispW_FName=DataLocation+"CoefGuessDispW"
			String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
			Wave /T CoefGuessDispW=$CoefGuessDispW_FName
			Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName

			String CoefGuessM_FName=DataLocation+"CoefGuessM" //Use for fitting
			String ConstraintV_FName=DataLocation+"ConstraintV" //Use for fitting
		
			String DataW_FName=DataLocation+DataName	//Raw data
			String BgW_FName=DataW_FName+"_BG"		//Background
			String Data_nBGW_FName=DataW_FName+"_nBG"	//Data after BG subtraction
			String Data_GssW_FName=DataW_FName+"_Gss"	//Guess curve
			String FitW_FName=Data_nBGW_FName+"_Fit"
			
			Wave Data_nBGW=$Data_nBGW_FName
			
			//Copy data to Constraint Volume and Coef Matrix
			SLP_MGFit_ConstrW2ConstrV(DataName)
			SLP_MGFit_ListBox2CoefMatrix(DataName)
			
			//	!!!! FIT !!!!
			SLP_Util_FitMultiGLAs2(Data_nBGW_FName,dimsize($ConstraintV_FName,2),CoefGuessM_FName,ConstraintV_FName)
			Wave FitW=$FitW_FName
			
			//DISPLAY RESULTS
			SLP_MGFit_DataListW[DataRow][17]="1" //== show fit result on the graph
			SLP_MGFit_DisplaySelectedData(DataName)
			
			//Update param guess table
			Variable i=0
			Variable j=0
			NVAR MaxN_peaks=root:Package_SLP:MGFit:MaxN_peaks
			String CoefW_FName=""
			String Index_str=""
			For (i=0;i<MaxN_peaks;i+=1)
				If (CoefGuessDispSelW[i][0]==48)
					sprintf Index_str,"%02d",j
					CoefW_FName=Data_nBGW_FName+"_c"+Index_str
					wave CoefW=$CoefW_FName
					
					CoefGuessDispW[i][8]=num2str(CoefW[0])
					CoefGuessDispW[i][9]=num2str(CoefW[1])
					CoefGuessDispW[i][10]=num2str(CoefW[2])
					CoefGuessDispW[i][11]=num2str(CoefW[3])
					CoefGuessDispW[i][12]=num2str(CoefW[4])
					
					j+=1
				EndIf
			EndFor
			
			//Calculate Chi Sq.
			NVAR Fit_ChiSq=root:Package_SLP:MGFit:Fit_ChiSq
			Fit_ChiSq= SLP_MGFit_CalcChiSq(FitW_FName,Data_nBGW_FName)
			SLP_MGFit_DataListW[DataRow][16]=num2str(Fit_ChiSq)
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function SLP_Util_FitMultiGLAs2(DataW_FName,N_Peaks,CoefGuessM_FName,ConstraintW_FName)
	String DataW_FName
	Variable N_Peaks
	String CoefGuessM_FName
	String ConstraintW_FName
	
	Wave DataW=$DataW_FName
	Wave CoefGuessM=$CoefGuessM_FName
	Wave ConstraintW=$ConstraintW_FName
	
	String ComponentW_FName=""
	String Constraint_txt=""
	String HoldStr=""
	String PAreaW_FName=DataW_FName+"_PArea"
	String FitW_FName=DataW_FName+"_Fit"
	Duplicate /O DataW,$FitW_FName /Wave=FitW
	FitW=0
	
	Make /O/N=(N_peaks) $PAreaW_FName /WAVE=PAreaW
	
	Make /O/N=(N_peaks,5) GLA_coef_M
	GLA_coef_M=0
	
	Make /T/O/N=0 FitConstraintW
	
	Variable i=0
	Variable j=0
	Variable k=0
	Variable m=0
	String CoefList=""
	String CoefW_FName=""
	String Index_str=""
	//String Fit_str="FuncFit /Q/W=2/NTHR=0/TBOX=768 {"
	//String Fit_str="FuncFit /Q/W=2/NTHR=0 {"
	
	//Create a string for Funcfit execution..
	String Fit_str="FuncFit /W=2/NTHR=0 {"

	For (i=0;i<N_Peaks;i+=1)
		sprintf Index_str,"%02d",i
		CoefW_FName=DataW_FName+"_c"+Index_str
		Make /O/N=5 $CoefW_FName /Wave=CoefW
		CoefW[]=CoefGuessM[i][p]
		
		//Hold String
		HoldStr=num2str(ConstraintW[0][2][i])+num2str(ConstraintW[1][2][i])+num2str(ConstraintW[2][2][i])+num2str(ConstraintW[3][2][i])+num2str(ConstraintW[4][2][i])

		Fit_str+="{SLP_GLA_FitFunc,"+CoefW_FName+",HOLD=\""+HoldStr+"\"},"		
		
		//Constraints
		For (j=0;j<5;j+=1) //Parameters: Int,Pos,FWHM,Mix,As
			If (!Numtype(ConstraintW[j][0][i]) && !(ConstraintW[j][2][i])) //If HOLD, don't put constraint!
				Constraint_txt="K"+num2str(k)+">="+num2str(ConstraintW[j][0])
				redimension /N=(dimsize(FitConstraintW,0)+1,dimsize(FitConstraintW,1)) FitConstraintW
				FitConstraintW[m]=Constraint_txt
				m+=1
			EndIf
			If (!Numtype(ConstraintW[j][1][i]) && !(ConstraintW[j][2][i])) //If HOLD, don't put constraint!
				Constraint_txt="K"+num2str(k)+"<="+num2str(ConstraintW[j][1])
				redimension /N=(dimsize(FitConstraintW,0)+1,dimsize(FitConstraintW,1)) FitConstraintW
				FitConstraintW[m]=Constraint_txt
				m+=1
			EndIf
			k+=1
		EndFor
		
	EndFor
	
	Fit_str=RemoveEnding(Fit_str)
	String DestW_FName=DataW_FName+"_Fit"
	Fit_str+="} "+DataW_FName+" /D"+" /C=FitConstraintW"
	
	//Execute funcfit
	Print Fit_str
	Execute Fit_str
	
	//Create results (components and fit)
	For (i=0;i<N_Peaks;i+=1)
		sprintf Index_str,"%02d",i
		CoefW_FName=DataW_FName+"_c"+Index_str
		wave CoefW=$CoefW_FName
		ComponentW_FName=DataW_FName+"_f"+index_str
		duplicate /O DataW,$ComponentW_FName /Wave=ComponentW
		
		ComponentW=SLP_GLA_Func(CoefW[0],CoefW[1],CoefW[2],CoefW[3],CoefW[4],x)
		
		FitW+=ComponentW
		
		//Individual peak area
		PAreaW[i]=area(ComponentW)
	EndFor
End



Function SLP_MGFit_ConstrW2ConstrV(DataName)
	String DataName
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	Variable DataRow=SLP_MGFit_FindDataRow(DataName)
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
	Variable PeakRow=V_Value
	
	Variable N_peak=str2num(SLP_MGFit_DataListW[DataRow][3])
	
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String ConstraintW_FName=DataLocation+"ConstraintW"
	String ConstraintV_FName=DataLocation+"ConstraintV"
	String CoefGuessDispSelW_FName=DataLocation+"CoefGuessDispSelW"
	
	Wave ConstraintW=$ConstraintW_FName
	Wave CoefGuessDispSelW=$CoefGuessDispSelW_FName
	
	Make /O/N=(5,3,0) $ConstraintV_FName /Wave=ConstraintV
	
	variable i=0
	variable j=0
	
	For (i=0;i<dimsize(CoefGuessDispSelW,0);i+=1)
		If (CoefGuessDispSelW[i][0][0]==48)
			redimension /N=(dimsize(ConstraintV,0),dimsize(ConstraintV,1),dimsize(ConstraintV,2)+1) ConstraintV
			ConstraintV[][][j]=ConstraintW[p][q][i]
			j+=1
		EndIf
	EndFor
End

Function SLP_MGFit_FindDataRow(DataName)
	String DataName
	
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	FindValue /TEXT=DataName /TXOP=6 SLP_MGFit_DataListW
	variable col=floor(V_value/dimsize(SLP_MGFit_DataListW,0))
	variable DataRow=V_value-col*dimsize(SLP_MGFit_DataListW,0) 
	
	Return DataRow
End

Function ListBoxProc_MGFit_Constraints(lba) : ListBoxControl
	STRUCT WMListboxAction &lba

	Variable row = lba.row
	Variable col = lba.col
	WAVE/T/Z listWave = lba.listWave
	WAVE/Z selWave = lba.selWave
	
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
	Variable DataRow=V_Value
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
	Variable PeakRow=V_Value
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	String DataName=SLP_MGFit_DataListW[DataRow][0]
	
	switch( lba.eventCode )
		case -1: // control being killed
			break
		case 1: // mouse down
			break
		case 3: // double click
			break
		case 4: // cell selection
			break
		case 5: // cell selection plus shift key
			break
		case 6: // begin edit
			break
		case 7: // finish edit
			SLP_MGFit_Listbox2ConstraintW(DataName)
			//SLP_MGFit_ConstrW2ConstrV(DataName)
			SLP_MGFit_ConstraintLogic1p(DataName,PeakRow)
			break
		case 13: // checkbox clicked (Igor 6.2 or later)
			SLP_MGFit_Listbox2ConstraintW(DataName)
			//SLP_MGFit_ConstrW2ConstrV(DataName)
			SLP_MGFit_ConstraintLogic1p(DataName,PeakRow)
			break
	endswitch

	return 0
End

Function ListBoxProc_MGFit_DataSelect(lba) : ListBoxControl
	STRUCT WMListboxAction &lba

	Variable row = lba.row
	Variable col = lba.col
	WAVE/T/Z listWave = lba.listWave
	WAVE/Z selWave = lba.selWave
	
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
	Variable DataRow=V_Value
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	String DataName=SLP_MGFit_DataListW[DataRow][0]
	NVAR GDataRow=root:Package_SLP:MGFit:GDataRow
	
	switch( lba.eventCode )
		case -1: // control being killed
			break
		case 1: // mouse down
			break
		case 3: // double click
			break
		case 4: // cell selection
			Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
			If (row!=-1&&row<dimsize(SLP_MGFit_DataListW,0))
				DataName=SLP_MGFit_DataListW[row][0]
				SLP_MGFit_DisplaySelectedData(DataName)
			EndIf
			GDataRow=row
			
			break
		case 5: // cell selection plus shift key
			break
		case 6: // begin edit
			break
		case 7: // finish edit
			break
		case 13: // checkbox clicked (Igor 6.2 or later)
			break
	endswitch

	return 0
End

Function ListBoxProc_MGFit_CoefGuess(lba) : ListBoxControl
	STRUCT WMListboxAction &lba

	Variable row = lba.row
	Variable col = lba.col
	WAVE/T/Z listWave = lba.listWave
	WAVE/Z selWave = lba.selWave
	
	String DataName
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
	Variable DataRow=V_Value
	DataName=SLP_MGFit_DataListW[DataRow][0]
	
	switch( lba.eventCode )
		case -1: // control being killed
			break
		case 1: // mouse down
			break
		case 3: // double click
			break
		case 4: // cell selection
			ControlInfo /W=SLP_MultiGLAFit_01 ListBox_PeakParam
			Variable PeakRow=V_Value
			SLP_MGFit_DataListW[DataRow][4]=num2str(PeakRow)
			SLP_MGFit_DisplaySelectedData(DataName)
			break
		case 5: // cell selection plus shift key
			break
		case 6: // begin edit
			break
		case 7: // finish edit
			SLP_MGFit_DataListW[DataRow][17]="0"
			SLP_MGFit_DisplaySelectedData(DataName)
			SLP_MGFit_UpdatePeakComponent()
			break
		case 13: // checkbox clicked (Igor 6.2 or later)
			SLP_MGFit_DataListW[DataRow][17]="0"
			SLP_MGFit_DisplaySelectedData(DataName)
			SLP_MGFit_UpdatePeakComponent()
			break
	endswitch

	return 0
End

Function SLP_MGFit_Panel_BGAdj() : Panel
	DoWindow /K MGFit_BGAdjust
	
	PauseUpdate; Silent 1		// building window...
	NewPanel /W=(471,157,894,468) /N=MGFit_BGAdjust
	
	ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
	Variable DataRow=V_Value
	Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
	String DataName=SLP_MGFit_DataListW[DataRow][0]
	String DataLocation=SLP_MGFit_DataListW[DataRow][1]
	String DataW_FName=DataLocation+DataName
	Wave DataW=$DataW_FName
	
	Variable MinX=dimoffset(DataW,0)
	Variable MaxX=dimoffset(DataW,0)+dimdelta(DataW,0)*(dimsize(DataW,0)-1)
	Variable StepX=(MaxX-MinX)/100
	
	WaveStats /Q DataW
	Variable MinY=V_Min-(V_Max-V_Min)/2
	Variable MaxY=V_Max+(V_Max-V_Min)/2
	Variable STepY=(MaxY-MinY)/100
	
	Variable FlatBG,StepPos,StepHeight,Width,Atten,StartX,EndX	
	//Starting values
	FlatBG=str2num(SLP_MGFit_DataListW[DataRow][6])
	StepPos=str2num(SLP_MGFit_DataListW[DataRow][7])
	StepHeight=str2num(SLP_MGFit_DataListW[DataRow][8])
	Width=str2num(SLP_MGFit_DataListW[DataRow][9])
	Atten=str2num(SLP_MGFit_DataListW[DataRow][10])
	StartX=str2num(SLP_MGFit_DataListW[DataRow][11])
	EndX=str2num(SLP_MGFit_DataListW[DataRow][12])
	
	String BGType=SLP_MGFit_DataListW[DataRow][5]
	make /O/N=9 MGFit_BGAdj_SliderDisableW
	If (Stringmatch(BGType,"Erf")||Stringmatch(BGType,"atan"))
		MGFit_BGAdj_SliderDisableW={0,0,0,0,0,2,2}
	ElseIf (Stringmatch (BGType,"Slope")||Stringmatch(BGType,"Shirley"))
		MGFit_BGAdj_SliderDisableW={2,2,2,2,2,0,0}
	ElseIf (Stringmatch (BGType,"Flat"))
		MGFit_BGAdj_SliderDisableW={0,2,2,2,2,2,2}
	ElseIf (Stringmatch (BGType,"None"))
		MGFit_BGAdj_SliderDisableW={2,2,2,2,2,2,2}
	EndIF
	
	Slider slider_BGAdj_FlatBG,pos={98,7},size={300,41},fSize=8,proc=SliderProc_MGFit_BGAdj,ticks=10
	Slider slider_BGAdj_FlatBG,limits={MinY,MaxY,StepY},value=FlatBG,vert= 0,disable=MGFit_BGAdj_SliderDisableW[0]
	
	Slider slider_BGAdj_StepPos,pos={98,47},size={300,41},fSize=8,proc=SliderProc_MGFit_BGAdj,ticks=10
	Slider slider_BGAdj_StepPos,limits={MinX,MaxX,StepX},value=StepPos,vert= 0,disable=MGFit_BGAdj_SliderDisableW[1]
	
	Slider slider_BGAdj_StepHeight,pos={98,87},size={300,41},fSize=8,proc=SliderProc_MGFit_BGAdj,ticks=10
	Slider slider_BGAdj_StepHeight,limits={MinY,MaxY,StepY},value=StepHeight,vert= 0,disable=MGFit_BGAdj_SliderDisableW[2]
	
	Slider slider_BGAdj_StepWidth,pos={98,126},size={300,41},fSize=8,proc=SliderProc_MGFit_BGAdj,ticks=10
	Slider slider_BGAdj_StepWidth,limits={0.01,(MaxX-MinX)/2,(MaxX-MinX)/200},value=Width,vert= 0,disable=MGFit_BGAdj_SliderDisableW[3]
	
	Slider slider_BGAdj_Atten,pos={98,163},size={300,41},fSize=8,proc=SliderProc_MGFit_BGAdj,ticks=10
	Slider slider_BGAdj_Atten,limits={0.001,10000,0},value=Atten,vert= 0,disable=MGFit_BGAdj_SliderDisableW[4]

	Slider slider_BGAdj_StartX,pos={98,202},size={300,41},fSize=8,proc=SliderProc_MGFit_BGAdj,ticks=10
	Slider slider_BGAdj_StartX,limits={MinX,MaxX,StepX},value=StartX,vert= 0,disable=MGFit_BGAdj_SliderDisableW[5]
	
	Slider slider_BGAdj_EndX,pos={98,242},size={300,41},fSize=8,proc=SliderProc_MGFit_BGAdj,ticks=10
	Slider slider_BGAdj_EndX,limits={MinX,MaxX,StepX},value=EndX,vert= 0,disable=MGFit_BGAdj_SliderDisableW[6]
	
	Killwaves MGFit_BGAdj_SliderDisableW
	
	
	Button button_BGAdj_Done,pos={348,284},size={50,20},title="Done",fSize=10,proc=ButtonProc_MGFit_BGAdj_Done

	//LABEL
	SetDrawLayer UserBack
	SetDrawEnv fsize= 10
	DrawText 17,23,"Flat background"
	SetDrawEnv fsize= 10
	DrawText 49,59,"Step pos"
	SetDrawEnv fsize= 10
	DrawText 38,98,"Step height"
	SetDrawEnv fsize= 10
	DrawText 40,138,"Step width"
	SetDrawEnv fsize= 10
	DrawText 37,177,"Attenuation"
	SetDrawEnv fsize= 10
	DrawText 59,215,"Start X"
	SetDrawEnv fsize= 10
	DrawText 66,255,"End X"

End

Function ButtonProc_MGFit_BGAdj(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			SLP_MGFit_Panel_BGAdj()
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ButtonProc_MGFit_BGAdj_Done(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	switch( ba.eventCode )
		case 2: // mouse up
			// click code here
			DoWindow /K MGFit_BGAdjust
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function SliderProc_MGFit_BGAdj(sa) : SliderControl
	STRUCT WMSliderAction &sa

	switch( sa.eventCode )
		case -1: // control being killed
			break
		default:
			if( sa.eventCode & 1 ) // value set
				Variable curval = sa.curval
				
				ControlInfo /W=SLP_MultiGLAFit_01 ListBox_DataList
				Variable DataRow=V_Value
				Wave /T SLP_MGFit_DataListW=root:Package_SLP:MGFit:SLP_MGFit_DataListW
				String DataName=SLP_MGFit_DataListW[DataRow][0]
				String BG_type=SLP_MGFit_DataListW[DataRow][5]
				
				
				ControlInfo /W=MGFit_BGAdjust slider_BGAdj_FlatBG
				Variable FlatBG=V_Value
				ControlInfo /W=MGFit_BGAdjust slider_BGAdj_StepPos
				Variable StepPos=V_Value
				ControlInfo /W=MGFit_BGAdjust slider_BGAdj_StepHeight
				Variable StepHeight=V_Value
				ControlInfo /W=MGFit_BGAdjust slider_BGAdj_StepWidth
				Variable Width=V_Value
				ControlInfo /W=MGFit_BGAdjust slider_BGAdj_Attend
				Variable Atten=V_Value
				ControlInfo /W=MGFit_BGAdjust slider_BGAdj_StartX
				Variable StartX=V_Value
				ControlInfo /W=MGFit_BGAdjust slider_BGAdj_EndX
				Variable EndX=V_Value
				
				SLP_MGFit_MakeBG(DataName,BG_type,FlatBG,StepPos,StepHeight,Width,Atten,StartX,EndX)
				
				SLP_MGFit_DataListW[DataRow][6]=num2str(FlatBG)
				SLP_MGFit_DataListW[DataRow][7]=num2str(StepPos)
				SLP_MGFit_DataListW[DataRow][8]=num2str(StepHeight)
				SLP_MGFit_DataListW[DataRow][9]=num2str(Width)
				SLP_MGFit_DataListW[DataRow][10]=num2str(Atten)
				SLP_MGFit_DataListW[DataRow][11]=num2str(StartX)
				SLP_MGFit_DataListW[DataRow][12]=num2str(EndX)
				
			endif
			break
	endswitch

	return 0
End