#pragma rtGlobals=3		// Use modern global access method and strict wave access.

//-------------------------------------------------------------------------------
//							Schroedinger_SOLVER
//-------------------------------------------------------------------------------
//					     		 July 2013
//
// 	Schroedinger_SOLVER is an IGOR PRO procedure (Igor 6.3) that integrates
//		numerically the Schroedinger equation with the NUMEROV method to obtain 
//		the time-independent -or stationary- solutions, for any given 1-D potential.
//		The solver assumes boundary conditions in which the wave function is zero 
//		at both extremes of the space domain (coordinate x). 
//
//		The procedure is freeware in the hope that it can be helpful for educational
//		purposes, but without any warranty. I would be thankful for any suggestion,
// 	comment or bug fixing that could improve this code.
//
//		 	    email address:  jmb.jesus@gmail.com
//						(Jesus Martinez Blanco)


//--------------------------------------------------------------
// USER INTERFACE
//--------------------------------------------------------------

menu "Macros"
	"-"
	"Schroedinger SOLVER", Schro_SOLVER()
end

//----------------------------------------------------------------------------------

Macro Schro_SOLVER()
//	Invoked from the menu Macros. Initializes and shows the panel.
	
	Ini_SchroSolver()
	
	DoWindow SchroSolverPanel
	
	if (V_Flag==0)	
		SchroSolverPanel()
		CreateNumerovWaves(root:NUMEROV:potentials:Harmonic_Oscillator)
	else
		Dowindow/F SchroSolverPanel
	endif
end

//----------------------------------------------------------------------------------

Function Ini_SchroSolver()
//	Creates the necessary waves, strings and variables.

	String savedDF= GetDataFolder(1)	
	newdatafolder/O/S root:NUMEROV

	variable/G E1
	variable/G E2
	variable/G x0
	variable/G anyE
	string/G selectedPotential
	
	if (waveexists(root:NUMEROV:levelsText)==0)
		variable/G hb2_2m=0.5//3.81	
		make/O/T/N=0 levelsText
		make/O/D/N=0 levels
		make/O/D/N=2 Psi,Psi2,V_R
		
		newdatafolder/O/S root:NUMEROV:potentials
	
		make/D/N=1000/O Harmonic_Oscillator
			setscale/i x,-10,10,Harmonic_Oscillator
			Harmonic_Oscillator=0.5*x^2

		make/D/N=1000/O Square_Well
			setscale/i x,-10,10,Square_Well
			Square_Well=(x>=-1 && x<=1) ? -1 : 0
	
	endif

	SetDataFolder savedDF

end

//----------------------------------------------------------------------------------

Window SchroSolverPanel() : Panel
	PauseUpdate; Silent 1		// building window...
	NewPanel /K=1 /W=(34,84,571,524) as "Schroedinger SOLVER"
	SetDrawLayer UserBack
	SetDrawEnv fillpat= 0
	DrawRect 113,61,522,97
	SetVariable setE1,pos={13,6},size={181,19},proc=SetEnergies,title="Find Eigenenergies from"
	SetVariable setE1,fSize=12,limits={-inf,inf,0},value= root:NUMEROV:E1
	SetVariable setE2,pos={139,28},size={55,19},proc=SetEnergies,title="to",fSize=12
	SetVariable setE2,limits={-inf,inf,0},value= root:NUMEROV:E2
	SetVariable setX0,pos={222,63},size={171,19},title="Matching Position:",fSize=12
	SetVariable setX0,frame=0,limits={-inf,inf,0},value= root:NUMEROV:x0,noedit= 1
	ListBox listLevels,pos={13,133},size={90,294},proc=LevelSelect,fSize=12
	ListBox listLevels,listWave=root:NUMEROV:levelsText,mode= 1,selRow= 0
	Slider sliderMPos,pos={114,83},size={406,13},proc=SliderMatchingPosition
	Slider sliderMPos,limits={-9.95996,9.97998,0},value= 0.01001001001001,side= 0,vert= 0
	PopupMenu PotentialChoose,pos={246,5},size={200,20},proc=PotentialChooser,title="Potential"
	PopupMenu PotentialChoose,fSize=12
	PopupMenu PotentialChoose,mode=3,popvalue="Harmonic_Oscillator",value= #"PotentialList()"
	TitleBox title0,pos={13,62},size={90,72},title="Select Energy\rto display the\rcorresponding\rEigenfunction:"
	TitleBox title0,fSize=12,frame=2
	SetVariable sethb2_2m,pos={246,28},size={129,19},proc=SetEnergies,title="hb^2/2m"
	SetVariable sethb2_2m,fSize=12,limits={-inf,inf,0},value= root:NUMEROV:hb2_2m
	String fldrSav0= GetDataFolder(1)
	SetDataFolder root:NUMEROV:
	Display/W=(113,101,523,427)/HOST=# /R Psi2
	AppendToGraph V_R
	AppendToGraph/R Psi
	SetDataFolder fldrSav0
	ModifyGraph mode(Psi2)=7
	ModifyGraph lSize(V_R)=2
	ModifyGraph rgb(Psi2)=(2,39321,1),rgb(V_R)=(0,0,65535)
	ModifyGraph hbFill(Psi2)=5
	ModifyGraph fSize=14
	ModifyGraph standoff(left)=0
	ModifyGraph axOffset(bottom)=0.1875
	ModifyGraph axRGB(right)=(65535,0,0),axRGB(left)=(0,0,65535)
	ModifyGraph tlblRGB(right)=(65535,0,0),tlblRGB(left)=(0,0,65535)
	ModifyGraph alblRGB(right)=(65535,0,0),alblRGB(left)=(0,0,65535)
	Label right "Wave Function\\K(0,0,0)/\\K(2,39321,1)Square of Wave Function"
	Label bottom "Position"
	Label left "Potential"
	RenameWindow #,SchroDISPLAY
	SetActiveSubwindow ##
EndMacro

//----------------------------------------------------------------------------------

Function LevelSelect(ctrlName,row,col,event) : ListBoxControl
//	Obtains the wave function associated with the selected energy level.
	String ctrlName
	Variable row
	Variable col
	Variable event	//1=mouse down, 2=up, 3=dbl click, 4=cell select with mouse or keys
					//5=cell select with shift key, 6=begin edit, 7=end
	if(event==4)
		WAVE levels=root:NUMEROV:levels
		CreateWaveFunction(levels[row])
	endif

	return 0
End

//--------------------------------------------------------------

Function SetEnergies(ctrlName,varNum,varStr,varName) : SetVariableControl
//	Obtains the list of eigenenergies (energy levels) between two input values.
	String ctrlName
	Variable varNum
	String varStr
	String varName
	
	NVAR E1=root:NUMEROV:E1
	NVAR E2=root:NUMEROV:E2

	FindEnergies(E1,E2)

End

//--------------------------------------------------------------

Function SliderMatchingPosition(ctrlName,sliderValue,event) : SliderControl
//	Recalculates the wave function for a given matching position (position where the forward and the backward integrations must connect smoothly).
	String ctrlName
	Variable sliderValue
	Variable event	// bit field: bit 0: value set, 1: mouse down, 2: mouse up, 3: mouse moved

	wave levels=root:NUMEROV:levels
	ControlInfo/W=SchroSolverPanel listLevels

	if((event %& 0x1) && (V_Value>-1 && V_value<numpnts(levels)))
		NVAR x0=root:NUMEROV:x0
		x0=sliderValue
		CreateWaveFunction(levels[V_Value])
	endif

	return 0
End

//--------------------------------------------------------------

Function PotentialChooser(ctrlName,popNum,popStr) : PopupMenuControl
//	To select a potential.
	String ctrlName
	Variable popNum
	String popStr

	switch(popNum)
	case 1:
		Execute/Q/Z "CreateBrowser prompt=\"Select one Wave and Click OK\", Command1=\"PotentialFromWave(%s)\", showWaves=1,showVars=0,showStrs=0"
		break
	case 2:
		DefineNewPotential()
		break
	default:
		wave V=WaveRefIndexedDFR(root:NUMEROV:potentials,popNum-3)
		CreateNumerovWaves(V)
		SVAR sp=root:NUMEROV:selectedPotential
		sp=popStr
	endswitch

End

//--------------------------------------------------------------

Function PotentialFromWave(w)
//	Creates a potential out of a chosen wave.
wave w

	string potname=NameOfWave(w)
	
	Prompt potname, "The wave will be copied with the name... (if it exists, it will be overwritten): "
	DoPrompt "Ptential from wave", potname
	
	if (V_Flag)
		SVAR sp=root:NUMEROV:selectedPotential
		PopupMenu PotentialChoose,win=SchroSolverPanel,mode=3,popvalue=sp
		return -1								// User canceled
	endif
	
	potname=ReplaceString(" ",potname,"/")
	execute/Q/Z "make/O root:NUMEROV:potentials:"+potname
	if (V_Flag==0)
		execute/Q/Z "killwaves root:NUMEROV:potentials:"+potname
	else
		DoAlert/T="Schroedinger SOLVER wants you to know..." 0, "You must provide a valid Igor wave name."
		SVAR sp=root:NUMEROV:selectedPotential
		PopupMenu PotentialChoose,win=SchroSolverPanel,mode=3,popvalue=sp
		return -1
	endif

	duplicate/O w,$("root:NUMEROV:potentials:"+potname)
	wave pot=$("root:NUMEROV:potentials:"+potname)
	redimension/D pot //to ensure that the wave will be double precission
	
	ControlUpdate/W=SchroSolverPanel PotentialChoose
	PopupMenu PotentialChoose,win=SchroSolverPanel,mode=3,popvalue=potname
	string pl=PotentialList()
	variable last=WhichListItem(potname, pl)+1
	PotentialChooser("",last,"")

end

//--------------------------------------------------------------

Function DefineNewPotential()
//	Asks for the necessary parameters to create a potential.
	string potname
	variable np=1000,xmin=-10,xmax=10
	string expre
	
	Prompt potname, "Name (if it exists, it will be overwritten): "
	Prompt np, "Number of points: "
	Prompt xmin, "Minimum x: "
	Prompt xmax, "Maximum x: "
	Prompt expre, "V(x) = "
	DoPrompt "Define your own potential", potname,np,xmin,xmax,expre
	
	if (V_Flag)
		SVAR sp=root:NUMEROV:selectedPotential
		PopupMenu PotentialChoose,win=SchroSolverPanel,mode=3,popvalue=sp
		return -1								// User canceled
	endif
	
	potname=ReplaceString(" ",potname,"/")
	execute/Q/Z "make/O root:NUMEROV:potentials:"+potname
	if (V_Flag==0)
		execute/Q/Z "killwaves root:NUMEROV:potentials:"+potname
	else
		DoAlert/T="Schroedinger SOLVER wants you to know..." 0, "You must provide a valid Igor wave name."
		SVAR sp=root:NUMEROV:selectedPotential
		PopupMenu PotentialChoose,win=SchroSolverPanel,mode=3,popvalue=sp
		return -1
	endif
	
	make/D/o/n=(np) $("root:NUMEROV:potentials:"+potname)
	wave pot=$("root:NUMEROV:potentials:"+potname)
	setscale/i x,xmin,xmax,pot
	Execute/Q/Z "root:NUMEROV:potentials:"+potname+"="+expre

	ControlUpdate/W=SchroSolverPanel PotentialChoose
	PopupMenu PotentialChoose,win=SchroSolverPanel,mode=3,popvalue=potname
	string pl=PotentialList()
	variable last=WhichListItem(potname, pl)+1
	PotentialChooser("",last,"")
	
End

//--------------------------------------------------------------

Function/S PotentialList()
//	Returns a string with the list of existing potentials (waves within root:NUMEROV:potentials:).
	String savedDF= GetDataFolder(1)	
	SetDataFolder root:NUMEROV:potentials
	string lw=WaveList("*", ";","")
	lw="_Copy from wave_;_Define New Potential_;"+lw
	SetDataFolder savedDF
	
	return lw
end


//--------------------------------------------------------------
// ALGORITHM
//--------------------------------------------------------------

Function CreateNumerovWaves(V)
//	Given a potential V, prepares the waves and the panel controls according to its x scaling.
	wave V

	String savedDF= GetDataFolder(1)	
	NewDataFolder/O/S root:NUMEROV

	duplicate/o V,Psi_R,Psi_L,V_R,V_L,Psi,Psi2
	Psi=0
	Psi2=0
	//to reset displayed waves
	wave/T	levelsText
	wave levels
	Redimension/N=0 levelsText,levels
	
	variable minX=DimOffset(V,0)
	variable maxX=DimOffset(V,0)+(DimSize(V,0)-1)*DimDelta(V,0)
	variable Ldelta=DimDelta(V,0)
	setscale/p x,maxX,-Ldelta,Psi_L,V_L
	
	variable np=numpnts(V)
	V_L=V_R[np-1-p]

	NVAR x0=root:NUMEROV:x0
	x0=(minX+maxX)/2
	
	Slider sliderMPos win=SchroSolverPanel,limits={minX+2*Ldelta,maxX-2*Ldelta,0},value=((minX+maxX)/2)
	
	SetDataFolder savedDF
end

//--------------------------------------------------------------

Function CrossingsFunction(w,E)
wave w
variable E

	return NumCrossings(E)-w[0]-0.5

end

//--------------------------------------------------------------

Function NumCrossings(E)
//	Returns the number of times that the forward integrated wave function crosses the abscise, for a given energy (nodal structure).
variable E

	NVAR hb2_2m=root:NUMEROV:hb2_2m
	variable C=1/hb2_2m
	variable h
	
	variable fw0bw1=0
	
	if (fw0bw1==0) // use forward integration
	
		WAVE Psi_R=root:NUMEROV:Psi_R
		WAVE V_R=root:NUMEROV:V_R
	
		h=abs(deltax(Psi_R))
	
		// Numerov FORWARD
		Psi_R[0]=0
		Psi_R[1]=1

		Psi_R=(p>1) ? (Psi_R[p-1]*(2-5*h^2*C*(E-V_R[p-1])/6)-Psi_R[p-2]*(1+h^2*C*(E-V_R[p-2])/12))/(1+h^2*C*(E-V_R[p])/12) : Psi_R[p]

		Findlevels/Q Psi_R,0
	
	else // use backward integration (for debugging purposes)

		WAVE Psi_L=root:NUMEROV:Psi_L
		WAVE V_L=root:NUMEROV:V_L
	
		h=abs(deltax(Psi_L))	
		
		// Numerov BACKWARD
		Psi_L[0]=0
		Psi_L[1]=1

		Psi_L=(p>1) ? (Psi_L[p-1]*(2-5*h^2*C*(E-V_L[p-1])/6)-Psi_L[p-2]*(1+h^2*C*(E-V_L[p-2])/12))/(1+h^2*C*(E-V_L[p])/12) : Psi_L[p]

		Findlevels/Q Psi_L,0

	endif

	return V_LevelsFound
end

//--------------------------------------------------------------

Function FwBwNumerov(E)
//	Integrates the solution in both directions (forward and backward) corresponding to the _R and _L waves.
variable E

	NVAR hb2_2m=root:NUMEROV:hb2_2m
	variable C=1/hb2_2m
	NVAR x0=root:NUMEROV:x0
	
	WAVE Psi_R=root:NUMEROV:Psi_R
	WAVE PSi_L=root:NUMEROV:PSi_L
	WAVE V_R=root:NUMEROV:V_R
	WAVE V_L=root:NUMEROV:V_L
	
	variable h=abs(deltax(Psi_R))
	variable D_R,D_L,aa

	// Numerov FORWARD
	Psi_R[0]=0
	Psi_R[1]=1

	Psi_R=(p>1) ? (Psi_R[p-1]*(2-5*h^2*C*(E-V_R[p-1])/6)-Psi_R[p-2]*(1+h^2*C*(E-V_R[p-2])/12))/(1+h^2*C*(E-V_R[p])/12) : Psi_R[p]
	aa=Psi_R(x0)
	Psi_R/=aa // provisional normalisation 

	// Numerov BACKWARD
	Psi_L[0]=0
	Psi_L[1]=1

	Psi_L=(p>1) ? (Psi_L[p-1]*(2-5*h^2*C*(E-V_L[p-1])/6)-Psi_L[p-2]*(1+h^2*C*(E-V_L[p-2])/12))/(1+h^2*C*(E-V_L[p])/12) : Psi_L[p]
	
	aa=Psi_L(x0)
	Psi_L/=aa // provisional normalisation 

	// Derivative at x0
	D_R=(Psi_R(x0+h)-Psi_R(x0-h))/(2*h)
	D_L=(Psi_L(x0+h)-Psi_L(x0-h))/(2*h)

	return (D_R-D_L) // this value is not used for the moment

end

//--------------------------------------------------------------

Function FindEnergies(E1,E2)
//	Obtains the list of energies between E1 and E2 for which NumCrossings returns an extra abscise crossing of the forward integrated solution.
	variable E1,E2
	
	if(E1>E2)
		variable Eaux=E2
		E2=E1
		E1=Eaux
	endif
	
	make/FREE/D/N=1 w
	variable Eini=E1
	variable finalnumcrossings=NumCrossings(E2)
	
	make/D/O/n=0 root:NUMEROV:levels
	wave levels=root:NUMEROV:levels
	
	do
		w[0]=NumCrossings(Eini)
		findroots/Q/B/T=0.000001/L=(Eini)/H=(E2) CrossingsFunction,w
		Eini=V_Root+0.000001
		
		Insertpoints 0,1,levels
		levels[0]=V_Root
	while(finalnumcrossings>w[0])
	
	deletepoints 0,1,levels
	Reverse/P levels	
		
	WAVE/T	levelsText=root:NUMEROV:levelsText
	Redimension/N=(NumPnts(levels)) levelsText
	
	if(NumPnts(levels)>0)
		levelsText=Num2Str(levels)
	endif	
	
end

//--------------------------------------------------------------

Function CreateWaveFunction(E)
//	Creates the wave function and its square for a given energy E.
variable E

	WAVE Psi_R=root:NUMEROV:Psi_R
	WAVE Psi_L=root:NUMEROV:Psi_L
	NVAR x0=root:NUMEROV:x0

	FwBwNumerov(E)
	
	duplicate/O Psi_R,root:NUMEROV:Psi,root:NUMEROV:Psi2
	WAVE Psi=root:NUMEROV:Psi
	WAVE Psi2=root:NUMEROV:Psi2
	
	Psi=(x>x0) ? Psi_L(x) : Psi(x)
	Psi2=Psi^2
	
	variable factor=area(Psi2)
	Psi/=sqrt(factor)
	Psi2/=factor

end

