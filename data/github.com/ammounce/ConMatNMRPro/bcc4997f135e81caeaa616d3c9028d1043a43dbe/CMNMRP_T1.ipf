#pragma rtGlobals=1	// Use modern global access method and strict wave access.

Window t1panel() : Panel
	NewPanel /W=(0,0,650,580)
	ShowInfo/W=t1panel
	SetDrawLayer UserBack

	//TitleandGraph	
	TitleBox filename,pos={263,5},size={123,20}
	TitleBox filename,variable= root:analysis:system:gfilename
	PopupMenu popupseclectwave,pos={253,30},size={143,20},proc=loadspectrum,title="Select Wave"
	PopupMenu popupseclectwave,mode=18,popvalue="NMR Data",value= #"ListofWavesinFolder()"

	Display/W=(59,55,558,362)/HOST=#  root:analysis:system:M vs root:analysis:system:timewave
	AppendToGraph root:analysis:system:fit_M vs root:analysis:system:timewave
	ModifyGraph mode(M)=3
	ModifyGraph marker(M)=19
	ModifyGraph log(bottom)=1
	Cursor/P A M 15;Cursor/P B M 7
	RenameWindow #,G0

	//Time wave
	
	SetVariable setvargtstart,pos={25,370},size={100,15},title="time start"
	SetVariable setvargtstart,value= root:analysis:system:gtstart
	SetVariable setvargtend,pos={25,390},size={100,15},title="time end"
	SetVariable setvargtend,value= root:analysis:system:gtend
	SetVariable setvargtoffset,pos={25,410},size={100,15},title="time offset"
	SetVariable setvargtoffset,value= root:analysis:system:gtoffset
	Button buttonMaketimewave,pos={10,430},size={140,20},proc=Maketimewave,title="Make log Time wave"
	
	//Store Data
	//Button buttonStoragewaveName,pos={34,500},size={140,20},proc=SetStorageWaveName,title="Storage Wave Name"
	PopupMenu popupt1storage,pos={5,500},size={186,20},proc=T1storagePopup,title="StorageWave"
	PopupMenu popupt1storage,mode=1, popvalue="T1vs", value= #"ListofT1vsWavesinFolder()"
	
	
	TitleBox title0,pos={183,500},size={9,9}
	TitleBox title0,variable= root:analysis:system:gt1storagename
	SetVariable setvargindex,pos={73,530},size={110,15},title="Storage Index"
	SetVariable setvargindex,value= root:analysis:system:gt1index
	SetVariable setvargindexparameter,pos={3,550},size={180,15},title="Index Parameter (H, w, T, ect)"
	SetVariable setvargindexparameter,value= root:analysis:system:gt1indexparameter
	Button buttonStoreData,pos={191,530},size={90,20},proc=StoreFitParameters,title="Store fit data"
	
	//Spin for fit
	PopupMenu popupgspin,pos={180,370},size={107,20},proc=Spinpopup,title="Nuclear Spin"
	PopupMenu popupgspin,mode=1,popvalue="",value= #"\"1/2;3/2;5/2;7/2;9/2\""
	
	SetVariable setvargrecoveries,pos={295,370},size={90,15},title="#recoveries", proc=RecoveriesControl
	SetVariable setvargrecoveries,limits={1,inf,1},value= root:analysis:system:grecoveries	
	
	PopupMenu popupgtransition,pos={180,395},size={186,20},proc=Spinpopup,title="Nuclear Transiiton"
	PopupMenu popupgtransition,mode=1,popvalue="",value= #"\"1/2<->-1/2; 1/2<->3/2; 3/2<->5/2; 5/2<->7/2;7/2<->9/2\""
	
	PopupMenu popupgNQR,pos={180,420},size={116,20},proc=Spinpopup,title="NMR or NQR?"
	PopupMenu popupgNQR,mode=1,popvalue="",value= #"\"NMR;NQR\""

	CheckBox checkgstretched,pos={309,420},size={66,14},title="Stretched?", proc=stretchcheckbox
	CheckBox checkgstretched,variable= root:analysis:system:gstretch

	PopupMenu popupT1type,pos={180,445},size={116,20},proc=Spinpopup,title="T\\B1\\M Expt. Type"
	PopupMenu popupT1type,mode=1,popvalue="",value= #"\"Inv./Sat.;Prog. Sat\""

	string spintitle
	
	spintitle="I="+num2istr(root:analysis:system:gspin)+"/2     T=" +num2istr(root:analysis:system:gtransition)+"/2<->"+num2istr(root:analysis:system:gtransitionlower)
	spintitle+= "/2     "+root:analysis:system:gNQRstring
	TitleBox titlespin,pos={180,470},size={100,17},fSize=12, frame=0
	TitleBox titlespin,limits={0,0,0},barmisc={0,1000},mode= 1
	TitleBox titlespin,title=spintitle
	
	//Guess T1
	SetVariable setvargMguess,pos={425,370},size={100,15},proc=T1guesscontrol,title="M guess"
	SetVariable setvargMguess,value= root:analysis:system:gMguess
	SetVariable setvargT1guess,pos={422,390},size={100,15},proc=T1guesscontrol,title="T\\B1\\M guess"
	SetVariable setvargT1guess,value= root:analysis:system:gT1guess
	SetVariable setvargMinfM0guess,pos={396,410},size={120,15},proc=T1guesscontrol,title="Tip angle guess"
	SetVariable setvargMinfM0guess,value= root:analysis:system:gtipguess
	SetVariable setvargexponentguess,pos={395,430},size={120,15},proc=T1guesscontrol,title="Exponent guess"
	SetVariable setvargexponentguess,value= root:analysis:system:gstretchguess
	
	
	Button buttonCursorguess,pos={528,387},size={100,20},proc=Cursorguess,title="Cursor Guess"
	SetVariable setvargrecoveriesguessindex,pos={545,370},size={45,15},title="#", proc=RecoveriesControl
	SetVariable setvargrecoveriesguessindex,limits={1,inf,1},value= root:analysis:system:grecoveriesguessindex
	
	
	//FitT1
	
	Button buttonFitT1,pos={480,463},size={50,20},proc=FitT1,title="Fit T1"
	SetVariable setvargrecoveriesindex,pos={400,465},size={45,15},title="#", Proc=RecoveriesControl
	SetVariable setvargrecoveriesindex,limits={1,inf,1},value= root:analysis:system:grecoveriesindex
	
	ValDisplay valdispgM,pos={400,485},size={95,13},title="M fit"
	ValDisplay valdispgM,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgM,value= #"root:analysis:system:gM"
	ValDisplay valdispgMerror,pos={500,485},size={95,13},title="+/-"
	ValDisplay valdispgMerror,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgMerror,value= #"root:analysis:system:gMerror"
	
	ValDisplay valdispgT1,pos={400,505},size={95,13},title="T\\B1\\M fit"
	ValDisplay valdispgT1,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgT1,value= #"root:analysis:system:gT1"
	ValDisplay valdispgT1error,pos={500,505},size={95,13},title="+/-"
	ValDisplay valdispgT1error,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgT1error,value= #"root:analysis:system:gT1error"
	
	ValDisplay valdispgtip,pos={400,525},size={95,13},title="Tip fit"
	ValDisplay valdispgtip,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgtip,value= #"root:analysis:system:gtip"
	ValDisplay valdispgtiperror,pos={500,525},size={95,13},title="+/-"
	ValDisplay valdispgtiperror,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgtiperror,value= #"root:analysis:system:gtiperror"
	
	ValDisplay valdispgexpfit,pos={400,545},size={95,13},title="Exp fit"
	ValDisplay valdispgexpfit,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgexpfit,value= #"root:analysis:system:gstretchfit"
	ValDisplay valdispgexpfiterror,pos={500,545},size={95,13},title="+/-"
	ValDisplay valdispgexpfiterror,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgexpfiterror,value= #"root:analysis:system:gstretchfiterror"
EndMacro

Function T1panelmaster()
	variable/g gspin
		
	NewPanel /W=(0,0,650,560)
	ShowInfo/W=t1panel
	SetDrawLayer UserBack

	//TitleandGraph	
	TitleBox filename,pos={263,5},size={123,20}
	TitleBox filename,variable= root:analysis:system:gfilename
	PopupMenu popupseclectwave,pos={253,30},size={143,20},proc=loadspectrum,title="Select Wave"
	PopupMenu popupseclectwave,mode=18,popvalue="NMR Data",value= #"ListofWavesinFolder()"

	Display/W=(59,55,558,362)/HOST=#  root:analysis:system:M vs root:analysis:system:timewave
	AppendToGraph root:analysis:system:fit_M vs root:analysis:system:timewave
	ModifyGraph mode(M)=3
	ModifyGraph marker(M)=19
	ModifyGraph log(bottom)=1
	Cursor/P A M 15;Cursor/P B M 7
	RenameWindow #,G0

	//Time wave
	
	SetVariable setvargtstart,pos={25,370},size={100,15},title="time start"
	SetVariable setvargtstart,value= root:analysis:system:gtstart
	SetVariable setvargtend,pos={25,390},size={100,15},title="time end"
	SetVariable setvargtend,value= root:analysis:system:gtend
	SetVariable setvargtoffset,pos={25,410},size={100,15},title="time offset"
	SetVariable setvargtoffset,value= root:analysis:system:gtoffset
	Button buttonMaketimewave,pos={10,430},size={140,20},proc=Maketimewave,title="Make log Time wave"
	
	//Store Data
	Button buttonStoragewaveName,pos={34,480},size={140,20},proc=SetStorageWaveName,title="Storage Wave Name"
	TitleBox title0,pos={183,481},size={9,9}
	TitleBox title0,variable= root:analysis:system:gt1storagename
	SetVariable setvargindex,pos={73,509},size={110,15},title="Storage Index"
	SetVariable setvargindex,value= root:analysis:system:gt1index
	SetVariable setvargindexparameter,pos={3,531},size={180,15},title="Index Parameter (H, w, T, ect)"
	SetVariable setvargindexparameter,value= root:analysis:system:gt1indexparameter
	Button buttonStoreData,pos={191,514},size={90,20},proc=StoreFitParameters,title="Store fit data"
	
	//Spin for fit
	PopupMenu popupgspin,pos={180,370},size={107,20},proc=Spinpopup,title="Nuclear Spin"
	PopupMenu popupgspin,mode=1,popvalue="1/2",value= #"\"1/2;3/2;5/2;7/2;9/2\""
	
	SetVariable setvargrecoveries,pos={295,370},size={90,15},title="#recoveries"
	SetVariable setvargrecoveries,limits={1,inf,1},value= root:analysis:system:grecoveries	
	
	PopupMenu popupgtransition,pos={180,400},size={186,20},proc=Spinpopup,title="Nuclear Transiiton"
	PopupMenu popupgtransition,mode=1,popvalue="1/2<->-1/2",value= #"\"1/2<->-1/2; 1/2<->3/2; 3/2<->5/2; 5/2<->7/2;7/2<->9/2\""
	
	PopupMenu popupgNQR,pos={180,430},size={116,20},proc=Spinpopup,title="NMR or NQR?"
	PopupMenu popupgNQR,mode=1,popvalue="NMR",value= #"\"NMR;NQR\""

	CheckBox checkgstretched,pos={309,430},size={66,14},title="Stretched?"
	CheckBox checkgstretched,variable= root:analysis:system:gstretch


	ValDisplay valdispgspin,pos={180,450},size={25,17},fSize=12
	ValDisplay valdispgspin,limits={0,0,0},barmisc={0,1000},mode= 1
	ValDisplay valdispgspin,value= #"root:analysis:system:gspin"
	DrawText 190,466,"/2"
	
	string spintitle
	
	//spintitle="I="+num2istr(root:analysis:system:gspin)+"/2     T=" +num2istr(root:analysis:system:gtransition)+"/2<->"+num2istr(root:analysis:system:gtransitionlower)
	//spintitle+= "/2     "+root:analysis:system:gNQRstring
	print spintitle
	TitleBox titlespin,pos={180,450},size={100,17},fSize=12, frame=0
	TitleBox titlespin,limits={0,0,0},barmisc={0,1000},mode= 1
	TitleBox titlespin,title=spintitle
	
	//Guess T1
	SetVariable setvargMguess,pos={425,370},size={100,15},proc=T1guesscontrol,title="M guess"
	SetVariable setvargMguess,value= root:analysis:system:gMguess
	SetVariable setvargT1guess,pos={422,390},size={100,15},proc=T1guesscontrol,title="T1 guess"
	SetVariable setvargT1guess,value= root:analysis:system:gT1guess
	SetVariable setvargMinfM0guess,pos={396,410},size={120,15},proc=T1guesscontrol,title="Tip angle guess"
	SetVariable setvargMinfM0guess,value= root:analysis:system:gtipguess
	SetVariable setvargexponentguess,pos={395,430},size={120,15},proc=T1guesscontrol,title="Exponent guess"
	SetVariable setvargexponentguess,value= root:analysis:system:gstretchguess
	
	
	Button buttonCursorguess,pos={528,387},size={100,20},proc=Cursorguess,title="Cursor Guess"
	SetVariable setvargrecoveriesguessindex,pos={545,370},size={45,15},title="#"
	SetVariable setvargrecoveriesguessindex,limits={1,inf,1},value= root:analysis:system:grecoveriesguessindex
	
	
	//FitT1
	
	Button buttonFitT1,pos={480,450},size={50,20},proc=FitT1,title="Fit T1"
	SetVariable setvargrecoveriesindex,pos={400,450},size={45,15},title="#"
	SetVariable setvargrecoveriesindex,limits={1,inf,1},value= root:analysis:system:grecoveriesindex
	
	ValDisplay valdispgM,pos={400,485},size={95,13},title="M fit"
	ValDisplay valdispgM,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgM,value= #"root:analysis:system:gM"
	ValDisplay valdispgMerror,pos={500,495},size={95,13},title="+/-"
	ValDisplay valdispgMerror,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgMerror,value= #"root:analysis:system:gMerror"
	
	ValDisplay valdispgT1,pos={400,505},size={95,13},title="T1 fit"
	ValDisplay valdispgT1,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgT1,value= #"root:analysis:system:gT1"
	ValDisplay valdispgT1error,pos={500,505},size={95,13},title="+/-"
	ValDisplay valdispgT1error,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgT1error,value= #"root:analysis:system:gT1error"
	
	ValDisplay valdispgtip,pos={400,525},size={95,13},title="Tip fit"
	ValDisplay valdispgtip,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgtip,value= #"root:analysis:system:gtip"
	ValDisplay valdispgtiperror,pos={500,425},size={95,13},title="+/-"
	ValDisplay valdispgtiperror,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgtiperror,value= #"root:analysis:system:gtiperror"
	
	ValDisplay valdispgexpfit,pos={400,545},size={95,13},title="Exp fit"
	ValDisplay valdispgexpfit,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgexpfit,value= #"root:analysis:system:gstretchfit"
	ValDisplay valdispgexpfiterror,pos={500,545},size={95,13},title="+/-"
	ValDisplay valdispgexpfiterror,limits={0,0,0},barmisc={0,1000}
	ValDisplay valdispgexpfiterror,value= #"root:analysis:system:gstretchfiterror"
	
end


Function Maketimewave(ctrlname):ButtonControl
	string ctrlname

	STRUCT NMRdata expt; Initexpt(expt)	

	make/o/n=(expt.points2D)	root:analysis:system:timewave
	initexpt(expt)
	
	expt.syst=expt.toffset+ expt.tstart*e^(ln(expt.tend/expt.tstart)/(expt.points2D-1)*x)
	expt.syst*=10^7
	expt.syst=ceil(expt.syst)
	expt.syst/=10^7

	duplicate/o expt.syst, root:analysis:timewaves:$("t"+expt.filename)
	initexpt(expt)
	
	storestats(expt)
End

Function RecoveriesControl(ctrlName,varNum,varStr,varName) : SetVariableControl
	String ctrlName
	Variable varNum	// value of variable as number
	String varStr		// value of variable as string
	String varName	// name of variable
	
	STRUCT NMRdata expt; initexpt(expt)
	
	if(samestring(varname, "grecoveries")==1)
		make/o/n=(3*expt.recoveries+1) root:analysis:system:w_coef, root:analysis:system:t1guesswave;initexpt(expt)
		make/o/n=(3*expt.recoveries+1,2) root:analysis:fits:$("T1fit"+expt.filename);initexpt(expt)
	elseif(samestring(varname, "grecoveriesguessindex")==1)
		if(expt.recoveriesguessindex>expt.recoveries)
			expt.recoveriesguessindex=expt.recoveries
		endif
		
		loadT1guessparameters(expt)
	elseif(samestring(varname, "grecoveriesindex")==1)
		if(expt.recoveriesindex>expt.recoveries)
			expt.recoveriesindex=expt.recoveries
		endif	
			
		loadT1fitparameters(expt)
	endif	
	
End

Function LoadT1guessparameters(s)	
	STRUCT NMRData &s
	
	s.mguess=s.t1guesswave[0]
	s.tipguess=s.t1guesswave[3*(s.recoveriesguessindex-1)+1]
	s.t1guess=s.t1guesswave[3*(s.recoveriesguessindex-1)+2]
	s.stretchguess=s.t1guesswave[3*(s.recoveriesguessindex-1)+3]

End

Function LoadT1fitparameters(s)
	STRUCT NMRData &s

	s.m=s.t1fitwave[0][0]
	s.tip=s.t1fitwave[3*(s.recoveriesindex-1)+1][0]
	s.t1=s.t1fitwave[3*(s.recoveriesindex-1)+2][0]
	s.stretchfit=s.t1fitwave[3*(s.recoveriesindex-1)+3][0]
	
	s.merror=s.t1fitwave[0][1]
	s.tiperror=s.t1fitwave[3*(s.recoveriesindex-1)+1][1]
	s.t1error=s.t1fitwave[3*(s.recoveriesindex-1)+2][1]
	s.stretchfiterror=s.t1fitwave[3*(s.recoveriesindex-1)+3][1]
End
	

Function T1guesscontrol(ctrlName,varNum,varStr,varName) : SetVariableControl
	String ctrlName
	Variable varNum	// value of variable as number
	String varStr		// value of variable as string
	String varName	// name of variable
	
	STRUCT NMRdata expt; initexpt(expt)
	
	T1guess(expt)
	Displayguessfit(expt)
End

Function T1guess(s)
	STRUCT NMRdata &s
		
	make/o/n=(s.recoveries*3+1) root:analysis:system:w_coef, root:analysis:system:t1guesswave
	
	s.t1guesswave[0]=s.Mguess
	s.t1guesswave[3*(s.recoveriesguessindex-1)+1]=s.tipguess
	s.t1guesswave[3*(s.recoveriesguessindex-1)+2]=s.T1guess
	s.t1guesswave[3*(s.recoveriesguessindex-1)+3]=s.stretchguess
	
	variable i=1
	do
		if(s.stretched==0)
			s.t1guesswave[3*(i-1)+3]=1
			s.stretchguess=1
		endif	
		i+=1
	while(i<s.recoveries+1)
	s.w_coef=s.t1guesswave
	
	s.guessfit=T1_fits(s.w_coef, s.syst[p])
End

Function Spinpopup (ctrlName,popNum,popStr) : PopupMenuControl
	String ctrlName
	Variable popNum	// which item is currently selected (1-based)
	String popStr		// contents of current popup item as string
	
	STRUCT NMRdata expt; Initexpt(expt)	
	
	if(samestring(ctrlname, "popupgspin")==1)
		expt.spin=2*(popnum+expt.NQR)-1
		print expt.spin, popnum
		if(expt.NQR==0)
			if(expt.spin==1)
				PopupMenu popupgtransition,value= #"\"1/2<->-1/2\""
			elseif(expt.spin==3)
				PopupMenu popupgtransition,value= #"\"1/2<->-1/2;1/2<->3/2\""
			elseif(expt.spin==5)
				PopupMenu popupgtransition,value= #"\"1/2<->-1/2;1/2<->3/2;3/2<->5/2\""
			elseif(expt.spin==7)
				PopupMenu popupgtransition,value= #"\"1/2<->-1/2;1/2<->3/2;3/2<->5/2;5/2<->7/2\""
			elseif(expt.spin==9)
				PopupMenu popupgtransition,value= #"\"1/2<->-1/2;1/2<->3/2;3/2<->5/2;5/2<->7/2;7/2<->9/2\""
			endif
		elseif(expt.NQR==1)
			if(expt.spin==1)
				PopupMenu popupgtransition,value= #"\"\""
			elseif(expt.spin==3)
				PopupMenu popupgtransition,value= #"\"1/2<->3/2\""
			elseif(expt.spin==5)
				PopupMenu popupgtransition,value= #"\"1/2<->3/2;3/2<->5/2\""
			elseif(expt.spin==7)
				PopupMenu popupgtransition,value= #"\"1/2<->3/2;3/2<->5/2;5/2<->7/2\""
			elseif(expt.spin==9)
				PopupMenu popupgtransition,value= #"\"1/2<->3/2;3/2<->5/2;5/2<->7/2;7/2<->9/2\""
			endif
		elseif(expt.NQR==1)
		endif	
		
	elseif(samestring(ctrlname, "popupgtransition")==1)
		expt.transition=2*(popnum+expt.NQR)-1
		expt.transitionlower=2*(popnum+expt.NQR)-3
	elseif(samestring(ctrlname, "popupgNQR")==1)
		expt.NQR=popnum-1
		if(popnum==1)
			expt.nqrstring="NMR"
		elseif(popnum==2)
			expt.nqrstring="NQR"
		endif
		
		if(expt.NQR==1)
			PopupMenu popupgspin,value= #"\"3/2;5/2;7/2;9/2\""
		else
			PopupMenu popupgspin,mode=1,value= #"\"1/2;3/2;5/2;7/2;9/2\""
		endif
	elseif(samestring(ctrlname, "PopupT1type")==1)
		expt.t1type=popnum-1
	endif
	//print expt.spin, expt.transition, expt.transitionlower, expt.NQR
	
	string spintitle="I="+num2istr(expt.spin)+"/2     T=" +num2istr(expt.transition)+"/2<->"+num2istr(expt.transitionlower)
	spintitle+= "/2     "+expt.NQRstring

	TitleBox titlespin,title=spintitle
	
End


	PopupMenu popupgspin,mode=1,popvalue="1/2",value= #"\"1/2;3/2;5/2;7/2;9/2\""

Function Cursorguess(ctrlname):ButtonControl
	string ctrlname
	
	STRUCT NMRdata expt; initexpt(expt)	
	variable i=0
	do
		if(expt.spin==1)
			if(expt.recoveries==1)
				expt.T1guesswave[3*(i)+2]=max(hcsr(A, expt.t1window),hcsr(B,expt.t1window))/5
			else
				expt.T1guesswave[3*(i)+2]=10^(log(expt.tstart)-(log(expt.tstart)-log(expt.tend))*(i+1)/(expt.recoveries+1))/5
			endif
		elseif(expt.spin==3)
			if(expt.recoveries==1)
				expt.T1guesswave[3*(i)+2]=max(hcsr(A, expt.t1window),hcsr(B,expt.t1window))/3
			else
				expt.T1guesswave[3*(i)+2]=10^(log(expt.tstart)-(log(expt.tstart)-log(expt.tend))*(i+1)/(expt.recoveries+1))/3
			endif
		elseif(expt.spin==5)
			if(expt.recoveries==1)
				expt.T1guesswave[3*(i)+2]=max(hcsr(A, expt.t1window),hcsr(B,expt.t1window))
			else
				expt.T1guesswave[3*(i)+2]=10^(log(expt.tstart)-(log(expt.tstart)-log(expt.tend))*(i+1)/(expt.recoveries+1))
			endif	
		elseif(expt.spin==7)
			if(expt.recoveries==1)
				expt.T1guesswave[3*(i)+2]=max(hcsr(A, expt.t1window),hcsr(B,expt.t1window))*3
			else
				expt.T1guesswave[3*(i)+2]=10^(log(expt.tstart)-(log(expt.tstart)-log(expt.tend))*(i+1)/(expt.recoveries+1))*3
			endif		
		elseif(expt.spin==9)
			if(expt.recoveries==1)
				expt.T1guesswave[3*(i)+2]=max(hcsr(A, expt.t1window),hcsr(B,expt.t1window))*5
			else
				expt.T1guesswave[3*(i)+2]=10^(log(expt.tstart)-(log(expt.tstart)-log(expt.tend))*(i+1)/(expt.recoveries+1))*5
			endif			
		endif			
			
		expt.t1guesswave[0]= max(vcsr(A, expt.t1window), vcsr(B,expt.t1window))
	
		expt.t1guesswave[3*(i)+1] =(max(vcsr(A, expt.t1window),vcsr(B,expt.t1window))-min(vcsr(B,expt.t1window),vcsr(A,expt.t1window))
		expt.t1guesswave[3*(i)+1]/=max(vcsr(A,expt.t1window),vcsr(B,expt.t1window))*expt.recoveries
		
		expt.t1guesswave[3*(i)+3]=1 
		i+=1
	while(i<expt.recoveries)
	expt.recoveriesguessindex=1
	LoadT1guessparameters(expt)

	T1guess(expt)	
	Displayguessfit(expt)

end

Function FitT1(ctrlname):Buttoncontrol
	string ctrlname
	
	SetDataFolder root:analysis:system:
	
	STRUCT NMRdata expt; Initexpt(expt)	
	
	Make/D/N=(3*expt.recoveries+1)/O root:analysis:system:W_coef
	Make/D/N=(3*expt.recoveries+1,2)/O root:analysis:fits:$("T1fit"+expt.filename);initexpt(expt)
	
	expt.W_coef=expt.t1guesswave
	
	duplicate/o expt.sysM, expt.fit_m
	
	string lockstring="0"
	variable i=0
	if(expt.stretched==0)
		do
			lockstring+="001"
			i+=1
		while(i<expt.recoveries)
		print lockstring
		FuncFit/H=lockstring/NTHR=0 T1_fits, expt.W_coef,  expt.sysM /D=expt.fit_M /X=expt.syst
	else
		FuncFit/NTHR=0 T1_fits, expt.W_coef,  expt.sysM /D=expt.fit_M /X=expt.syst	
	endif
	
	expt.t1fitwave[][0]=expt.w_coef[p]
	expt.t1fitwave[][1]=expt.w_sigma[p]
	
	expt.recoveriesindex=1
	loadt1fitparameters(expt)
	
	DisplayT1fit(expt)	
	SetDataFolder root:

end

Function T1storagePopup(ctrlName,popNum,popStr) : PopupMenuControl
	String ctrlName
	Variable popNum	// which item is currently selected (1-based)
	String popStr		// contents of current popup item as string
	
	STRUCT NMRdata expt; initexpt(expt)
	
	string localstoragewavename="T1vs"

	print popstr
	if(samestring(popstr, "New")==1)
		
		Prompt localstoragewavename, "Enter storage wave name:"
		DoPrompt "Storage Wave Name", localstoragewavename
		
		if(v_flag==1)
			return 0
		endif
		
		make/n=(1,1)  root:$(localstoragewavename)
	else
		localstoragewavename=popstr
	endif

	expt.t1storagename=localstoragewavename

End

Function/s ListofT1vswavesinFolder()

	string list="New;"
	variable numwaves,index=0
	
	numwaves = Countobjects("root:",1)
	do
		if(strsearch(GetIndexedObjName("root:", 1, index),"T1vs",0)==0)
			list+=GetIndexedObjName("root:", 1, index)+";"
		endif
		index +=1
	while(index < numwaves)

	return(Sortlist(list))
End





Function SetStorageWaveName(ctrlname):ButtonControl
	string ctrlname
	
	STRUCT NMRdata expt; initexpt(expt)
	
	string localstoragewavename
	
	Prompt localstoragewavename, "Enter storage wave name:"
	DoPrompt "Storage Wave Name", localstoragewavename
	
	if(v_flag==1)
		return 0
	endif
		
	expt.t1storagename=localstoragewavename
	
end

Function StoreFitParameters(ctrlname):ButtonControl
	string ctrlname

	STRUCT NMRdata expt; Initexpt(expt)
	print expt.t1storagename
	if(exists(expt.t1storagename)==0)
		make/n=(1,8*expt.recoveries+3) root:$(expt.t1storagename)=nan;initexpt(expt)
	endif				
	variable initialxdimsize=dimsize(expt.t1storage,0), initialydimsize=dimsize(expt.t1storage,1)

	if(expt.t1index+1>dimsize(expt.t1storage,0))
		insertpoints dimsize(expt.t1storage,0)+1, expt.t1index-dimsize(expt.t1storage,0)+1, expt.t1storage
		expt.t1storage[initialxdimsize,dimsize(expt.t1storage,0)-1]=nan
	endif
	
	if(dimsize(expt.t1storage,1)<8*expt.recoveries+3)
		insertpoints/M=1 dimsize(expt.t1storage,1), 8*expt.recoveries+3-dimsize(expt.t1storage,1), expt.t1storage
		expt.t1storage[][initialydimsize, dimsize(expt.t1storage,1)-1]=nan
	endif
	expt.t1storage[expt.t1index][0]=expt.t1indexparameter
	expt.t1storage[expt.t1index][1]=expt.t1fitwave[0][0]
	expt.t1storage[expt.t1index][2]=expt.t1fitwave[0][1]
	
	variable i=0
	do
		expt.t1storage[expt.t1index][8*i+3]=expt.t1fitwave[3*i+1][0]
		expt.t1storage[expt.t1index][8*i+5]=expt.t1fitwave[3*i+2][0]
		expt.t1storage[expt.t1index][8*i+7]=1/expt.t1fitwave[3*i+2][0]
		expt.t1storage[expt.t1index][8*i+9]=expt.t1fitwave[3*i+3][0]

		expt.t1storage[expt.t1index][8*i+4]=expt.t1fitwave[3*i+1][1]
		expt.t1storage[expt.t1index][8*i+6]=expt.t1fitwave[3*i+2][1]
		expt.t1storage[expt.t1index][8*i+8]=expt.t1fitwave[3*i+2][1]/expt.t1fitwave[i+2][0]^2
		expt.t1storage[expt.t1index][8*i+10]=expt.t1fitwave[3*i+3][1]
		i+=1
	while(i<expt.recoveries)
	
End	

Function NormalizeM(ctrlname):ButtonControl
	string ctrlname
	
	STRUCT NMRdata expt; initexpt(expt)
	
	variable zero, minf	
	zero=mean(expt.sysM, 0, 10)	
	expt.sysM-=zero
	expt.Mwave-=zero
	
	minf=mean(expt.sysM, expt.points2D-4, expt.points2D-1)	
	expt.sysM/=minf
	expt.mwave/=minf
	
end



Function Stretchcheckbox(ctrlName,checked) : CheckBoxControl
	String ctrlName
	Variable checked			// 1 if selected, 0 if not
	
	STRUCT NMRdata expt; initexpt(expt)
	
	if(checked==0)
		expt.stretchguess=1
	endif		
	
End
	
Function DisplayFFTsum(s)
	STRUCT NMRdata &s	

	checkdisplayed/W=$(s.fftsumintwindow) s.sysfftsum
	removefromgraph/Z/W=$(s.fftsumintwindow) $"integral"

	if(v_flag==0)
		AppendtoGraph/W=$(s.fftsumintwindow) s.sysfftsum
	endif
	
	if(s.fieldsweep==1)
		Label/W=$(s.fftsumintwindow) bottom, "Field (T)"
	elseif(s.frequencysweep==1)
		Label/W=$(s.fftsumintwindow) bottom, "Frequency (MHz)"
	endif
	
	Label/W=$(s.fftsumintwindow) left, "Intensity (a.u.)"
	
End

Function DisplayIntegral(s)
	STRUCT NMRdata &s	

	checkdisplayed/W=$(s.fftsumintwindow) s.sysint
	removefromgraph/Z/W=$(s.fftsumintwindow) $"FFTSumwave"

	if(v_flag==0)
		AppendtoGraph/W=$(s.fftsumintwindow) s.sysint
	endif
	
	if(s.fieldsweep==1)
		Label/W=$(s.fftsumintwindow) bottom, "Field (T)"
	elseif(s.frequencysweep==1)
		Label/W=$(s.fftsumintwindow) bottom, "Frequency (MHz)"
	endif
	
	Label/W=$(s.fftsumintwindow) left, "Intensity (a.u.)"
	
End


Function DisplayT1fit(s)
	STRUCT NMRdata &s	

	checkdisplayed/W=$(s.t1window) s.fit_M
	removefromgraph/Z/W=$(s.t1window) $"guessfit"

	if(v_flag==0)
		AppendtoGraph/W=$(s.t1window) s.fit_M vs s.syst
	endif
	
	Label/W=$(s.t1window) bottom, "time (s)"	
	Label/W=$(s.t1window) left, "M(t)"
End

Function DisplayGuessfit(s)
	STRUCT NMRdata &s	

	checkdisplayed/W=$(s.t1window) s.guessfit
	removefromgraph/Z/W=$(s.t1window) $"Fit_M"

	if(v_flag==0)
		AppendtoGraph/W=$(s.t1window) s.guessfit vs s.syst
	endif
	
	Label/W=$(s.t1window) bottom, "time (s)"	
	Label/W=$(s.t1window) left, "M(t)"
	
End


Function T1_fits(w,x) : FitFunc
	Wave w
	Variable x

	STRUCT NMRdata expt; initexpt(expt)
	//CurveFitDialog/ These comments were created by the Curve Fitting dialog. Altering them will
	//CurveFitDialog/ make the function less convenient to work with in the Curve Fitting dialog.
	//CurveFitDialog/ Equation:
	//CurveFitDialog/ f(x) = Minf*(1-(1-cos(pi/180*theta))*exp(-x/T1))
	//CurveFitDialog/ End of Equation
	//CurveFitDialog/ Independent Variables 1
	//CurveFitDialog/ x
	//CurveFitDialog/ Coefficients 3
	//CurveFitDialog/ w[0] = Minf
	//CurveFitDialog/ w[1] = theta
	//CurveFitDialog/ w[2] = T1

	variable expsum, i=0
	
	if(expt.NQR==0)
		if(expt.spin==1)
			if(expt.T1type==0)
				do
					expsum+=w[3*i+1]*exp(-(x/w[3*i+2])^w[3*i+3])
					i+=1
				while(i<expt.recoveries)
				return w[0]*(1-expsum)
			elseif(expt.t1type==1)
				do
					expsum+=w[3*i+1]*exp(-(x/w[3*i+2])^w[3*i+3])/(1-w[3*i+1]*exp(-(x/w[3*i+3])^w[3*i+1]))
					i+=1
				while(i<expt.recoveries)
				return w[0]*(1-expsum)
			endif
		elseif(expt.spin==3)
			if(expt.transition==1)
				do
					expsum+=w[3*i+1]*(9/10*exp(-(6*x/w[3*i+2])^w[3*i+3])+1/20*exp(-(x/w[3*i+2])^w[3*i+3]))
					i+=1
				while(i<expt.recoveries)
				return w[0]*(1-expsum)
			elseif(expt.transition==3)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(2/5*exp(-(6*x/w[2])^w[3])+1/2*exp(-(3*x/w[2])^w[3])+1/10*exp(-(x/w[2])^w[3])))
			endif
		elseif(expt.spin==5)
			if(expt.transition==1)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(50/63*exp(-(15*x/w[2])^w[3])+8/45*exp(-(6*x/w[2])^w[3])+1/35*exp(-(x/w[2])^w[3])))
			elseif(expt.transition==3)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(25/56*exp(-(15*x/w[2])^w[3])+25/56*exp(-(10*x/w[2])^w[3])+1/40*exp(-(6*x/w[2])^w[3])+3/56*exp(-(3*x/w[2])^w[3])+1/35*exp(-(x/w[2])^w[3])))
			elseif(expt.transition==5)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(1/14*exp(-(15*x/w[2])^w[3])+2/7*exp(-(10*x/w[2])^w[3])+2/5*exp(-(6*x/w[2])^w[3])+3/14*exp(-(3*x/w[2])^w[3])+1/35*exp(-(x/w[2])^w[3])))
			endif
		elseif(expt.spin==7)
			if(expt.transition==1)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(1225/1716*exp(-(28*x/w[2])^w[3])+75/364*exp(-(15*x/w[2])^w[3])+3/44*exp(-(6*x/w[2])^w[3])+1/84*exp(-(x/w[2])^w[3])))
			elseif(expt.transition==3)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(196/429*exp(-(28*x/w[2])^w[3])+49/132*exp(-(21*x/w[2])^w[3])+1/1092*exp(-(15*x/w[2])^w[3])+9/77*exp(-(10*x/w[2])^w[3])+1/33*exp(-(6*x/w[2])^w[3])+1/84*exp(-(3*x/w[2])^w[3])+1/84*exp(-(x/w[2])^w[3])))
			elseif(expt.transition==5)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(49/429*exp(-(28*x/w[2])^w[3])+49/132*exp(-(21*x/w[2])^w[3])+100/273*exp(-(15*x/w[2])^w[3])+25/308*exp(-(10*x/w[2])^w[3])+1/132*exp(-(6*x/w[2])^w[3])+1/21*exp(-(3*x/w[2])^w[3])+1/84*exp(-(x/w[2])^w[3])))
			elseif(expt.transition==7)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(4/429*exp(-(28*x/w[2])^w[3])+3/44*exp(-(21*x/w[2])^w[3])+75/364*exp(-(15*x/w[2])^w[3])+25/77*exp(-(10*x/w[2])^w[3])+3/11*exp(-(6*x/w[2])^w[3])+3/28*exp(-(3*x/w[2])^w[3])+1/84*exp(-(x/w[2])^w[3])))
			endif
		elseif(expt.spin==9)
			if(expt.transition==1)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(7938/12155*exp(-(45*x/w[2])^w[3])+1568/7293*exp(-(28*x/w[2])^w[3])+6/65*exp(-(15*x/w[2])^w[3])+24/715*exp(-(6*x/w[2])^w[3])+1/165*exp(-(x/w[2])^w[3])))
			elseif(expt.transition==3)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(2205/4862*exp(-(45*x/w[2])^w[3])+441/1430*exp(-(36*x/w[2])^w[3])+49/14586*exp(-(28*x/w[2])^w[3])+349/330*exp(-(21*x/w[2])^w[3])+5/312*exp(-(15*x/w[2])^w[3])+45/1144*exp(-(10*x/w[2])^w[3])+361/17160*exp(-(6*x/w[2])^w[3])+1/264*exp(-(3*x/w[2])^w[3])+1/165*exp(-(x/w[2])^w[3])))
			elseif(expt.transition==5)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(360/2431*exp(-(45*x/w[2])^w[3])+288/715*exp(-(36*x/w[2])^w[3])+2048/7293*exp(-(28*x/w[2])^w[3])+2/165*exp(-(21*x/w[2])^w[3])+5/78*exp(-(15*x/w[2])^w[3])+10/143*exp(-(10*x/w[2])^w[3])+2/2145*exp(-(6*x/w[2])^w[3])+1/66*exp(-(3*x/w[2])^w[3])+1/165*exp(-(x/w[2])^w[3])))
			elseif(expt.transition==7)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(405/19448*exp(-(45*x/w[2])^w[3])+729/5720*exp(-(36*x/w[2])^w[3])+17689/58344*exp(-(28*x/w[2])^w[3])+147/440*exp(-(21*x/w[2])^w[3])+15/104*exp(-(15*x/w[2])^w[3])+5/1144*exp(-(10*x/w[2])^w[3])+147/5720*exp(-(6*x/w[2])^w[3])+3/88*exp(-(3*x/w[2])^w[3])+1/165*exp(-(x/w[2])^w[3])))
			elseif(expt.transition==9)
				return w[0]*(1-(1-cos(pi/180*w[1]))*(5/4862*exp(-(45*x/w[2])^w[3])+8/715*exp(-(36*x/w[2])^w[3])+392/7293*exp(-(28*x/w[2])^w[3])+49/330*exp(-(21*x/w[2])^w[3])+10/39*exp(-(15*x/w[2])^w[3])+40/143*exp(-(10*x/w[2])^w[3])+392/2145*exp(-(6*x/w[2])^w[3])+2/33*exp(-(3*x/w[2])^w[3])+1/165*exp(-(x/w[2])^w[3])))
			endif
		endif
	if(expt.NQR==1)
		if(expt.spin==3)
			return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))
		elseif(expt.spin==5)
			if(expt.transition==3)
				return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))
			elseif(expt.transition==5)
				return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))
			endif
		elseif(expt.spin==7)
			if(expt.transition==3)
				return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))
			elseif(expt.transition==5)
				return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))
			elseif(expt.transition==7)
				return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))	
			endif
		elseif(expt.spin==9)
			elseif(expt.transition==3)
				return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))
			elseif(expt.transition==5)
				return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))
			elseif(expt.transition==7)
				return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))
			elseif(expt.transition==9)
				return w[0]*(1-(1-cos(pi/180*w[1]))*exp(-(x/w[2])^w[3]))
			endif
		endif
	endif
End
