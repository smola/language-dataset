#pragma rtGlobals=3
#pragma version=1.3
#pragma IgorVersion = 6.3.0
#pragma IndependentModule=CodeBrowserModule

#include <Resize Controls>

// Copyright (c) 2019, () byte physics support@byte-physics.de
// All rights reserved.
//
// This source code is licensed under the BSD 3-Clause license found in the
// LICENSE file in the root directory of this source tree.

// define size
Constant panelWidth    = 300
Constant panelHeight   = 170
// define width
Constant panelLeft     = 100
Constant panelTop      = 100

static StrConstant panel = "CodeBrowser"
static StrConstant userDataRawList = "rawList"
static StrConstant userDataNiceList = "niceList"

static StrConstant oneTimeInitUserData = "oneTimeInit"

static StrConstant genericError = "_error_"

Function/S GetPanel()
	return panel
End

Function ResetPanel()
	KillWindow $GetPanel()
	ResetPackagePrefs()
	CreatePanel(resize=0)
End

// Creates the main panel
Function createPanel([resize])
	variable resize

	STRUCT CodeBrowserPrefs prefs
	LoadPackagePrefsFromDisk(prefs)

	resize = ParamIsDefault(resize) ? 1 : !!resize

	compile()

	if(existsPanel())
		DoWindow/F $panel
		return NaN
	endif

	NewPanel/N=$panel/K=1/W=(panelLeft, panelTop, panelLeft + panelWidth, panelTop + panelHeight)

	setGlobalStr("procFilter", prefs.procFilter)
	setGlobalStr("search", prefs.search)

	CodeBrowserPanel()
	ListBox List1, win = $panel, listWave=getDeclWave()
#if (IgorVersion() >= 8.00)
	ListBox List1, win = $panel, helpWave=getHelpWave()
#endif

	PopupMenu PopupNamespace, win=$panel, mode=prefs.panelNameSpace
	PopupMenu PopupProcedure, win=$panel, mode=prefs.panelProcedure
	ListBox List1, win=$panel, selRow=prefs.panelElement, row=prefs.panelTopElement
	Checkbox CheckboxSort, win=$panel, value=prefs.panelCheckboxSort

	if(resize)
		resizeToPackagePrefs()
	endif
	DoUpdate/W=$panel
	initializePanel()
End

/// @brief static panel preferences that can be updated by ResizeControls
Function CodeBrowserPanel()
	ModifyPanel fixedSize=0

	SetVariable SetProcedureFilter,pos={68,31},size={227,19},proc=CodeBrowserModule#SetVarProcedureFilter,title="Filter"
	SetVariable SetProcedureFilter,limits={-inf,inf,0},value= root:Packages:CodeBrowser:procFilter,live= 1
	SetVariable SetProcedureFilter,help={"Filter procedures matching the specified filter pattern from the selected namespace context."}
	SetVariable SetProcedureFilter,userdata(ResizeControlsInfo)= A"!!,EB!!#=[!!#Ar!!#<Pz!!#`-A7TLfzzzzzzzzzzzzzz!!#`-A7TLfzz"
	SetVariable SetProcedureFilter,userdata(ResizeControlsInfo) += A"zzzzzzzzzzzz!!#u:DuaGl<C]S6zzzzzzzzzz"
	SetVariable SetProcedureFilter,userdata(ResizeControlsInfo) += A"zzz!!#N3Bk1ct<C]S6zzzzzzzzzzzzz!!!"

	CheckBox CheckboxSort,pos={15.00,77.00},size={36.00,15.00},proc=CodeBrowserModule#CheckboxSort,title="Sort"
	CheckBox CheckboxSort,help={"Sort results alphabetically. Uncheck to sort by line number."}
	CheckBox CheckboxSort,userdata(ResizeControlsInfo)= A"!!,B)!!#?S!!#=s!!#<(z!!#](Aon#azzzzzzzzzzzzzz!!#`-A7TLfzz"
	CheckBox CheckboxSort,userdata(ResizeControlsInfo) += A"zzzzzzzzzzzz!!#u:DuaGl<C]S6zzzzzzzzzz"
	CheckBox CheckboxSort,userdata(ResizeControlsInfo) += A"zzz!!#N3Bk1ct<C]S6zzzzzzzzzzzzz!!!"
	CheckBox CheckboxSort,value= 1

	SetVariable SetSearch,pos={57.00,75.00},size={238.00,18.00},bodyWidth=200,proc=CodeBrowserModule#SetVarProcedureSearch,title="Search"
	SetVariable SetSearch,help={"Search for elements in the list below. Search patterns will be automatically appended with leading and trailing wildcards."}
	SetVariable SetSearch,userdata(ResizeControlsInfo)= A"!!,Ds!!#?O!!#B(!!#<Hz!!#`-A7TLfzzzzzzzzzzzzzz!!#`-A7TLfzz"
	SetVariable SetSearch,userdata(ResizeControlsInfo) += A"zzzzzzzzzzzz!!#u:DuaGl<C]S6zzzzzzzzzz"
	SetVariable SetSearch,userdata(ResizeControlsInfo) += A"zzz!!#N3Bk1ct<C]S6zzzzzzzzzzzzz!!!"
	SetVariable SetSearch,limits={-inf,inf,0},value= root:Packages:CodeBrowser:search,live= 1

	ListBox List1,pos={5.00,100.00},size={290,65},proc=CodeBrowserModule#ListBoxProc
	ListBox List1,listWave=root:Packages:CodeBrowser:declarations
	ListBox List1,selCol= 1,widths={4,40},keySelectCol= 1
	ListBox List1,mode= 5,selRow= 0
	ListBox List1,help={"Elements matching the search pattern in the selected procedure file(s)"}
	ListBox List1,userdata(ResizeControlsInfo)= A"!!,?X!!#@,!!#BK!!#?;z!!#](Aon\"Qzzzzzzzzzzzzzz!!#o2B4uAezz"
	ListBox List1,userdata(ResizeControlsInfo) += A"zzzzzzzzzzzz!!#N3Bk1ct<C]S6zzzzzzzzzz"
	ListBox List1,userdata(ResizeControlsInfo) += A"zzz!!#N3Bk1ct<C]S7zzzzzzzzzzzzz!!!"

	PopupMenu PopupProcedure,pos={38,52},size={257,17},bodyWidth=200,proc=CodeBrowserModule#popupProcedures,title="Procedure"
	PopupMenu PopupProcedure,mode=1,popvalue="<ALL>",value= #"CodeBrowserModule#generateProcedureList()"
	PopupMenu PopupProcedure,help={"Display elements for this procedure file. Set to <ALL> to select all procedures from the current list."}
	PopupMenu PopupProcedure,userdata(ResizeControlsInfo)= A"!!,D'!!#>^!!#B:J,hlkz!!#`-A7TLfzzzzzzzzzzzzzz!!#`-A7TLfzz"
	PopupMenu PopupProcedure,userdata(ResizeControlsInfo) += A"zzzzzzzzzzzz!!#u:Du]k<zzzzzzzzzzz"
	PopupMenu PopupProcedure,userdata(ResizeControlsInfo) += A"zzz!!#N3Bk1ct<C]S6zzzzzzzzzzzzz!!!"
	PopupMenu PopupProcedure,userdata(niceList)=  "<ALL>"
	PopupMenu PopupProcedure,userdata(rawList)=  "<ALL>"

	PopupMenu PopupNamespace,pos={30,10},size={265,17},bodyWidth=200,proc=CodeBrowserModule#popupModules,title="Namespace"
	PopupMenu PopupNamespace,userdata(niceList)=  "<ALL>;ProcGlobal;",value= #"CodeBrowserModule#generateModuleList()"
	PopupMenu PopupNamespace,mode=1,popvalue="<ALL>"
	PopupMenu PopupNamespace,help={"NameSpace of Independent Module or ProcGlobal context. Set to <ALL> to ignore the Namespace."}
	PopupMenu PopupNamespace,userdata(ResizeControlsInfo)= A"!!,CT!!#;-!!#B>J,hlkz!!#`-A7TLfzzzzzzzzzzzzzz!!#`-A7TLfzz"
	PopupMenu PopupNamespace,userdata(ResizeControlsInfo) += A"zzzzzzzzzzzz!!#u:Du]k<zzzzzzzzzzz"
	PopupMenu PopupNamespace,userdata(ResizeControlsInfo) += A"zzz!!#N3Bk1ct<C]S6zzzzzzzzzzzzz!!!"

	// Control area 100 from top, Borders: 5
	DefineGuide UGH0={FT,100}
	DefineGuide UGH1={FB,5}
	DefineGuide UGHL={FL,5}
	DefineGuide UGHR={FR,5}

	SetWindow kwTopWin,userdata(ResizeControlsInfo)= A"!!*'\"z!!#BP!!#A9zzzzzzzzzzzzzzzzzzzzz"
	SetWindow kwTopWin,userdata(ResizeControlsInfo) += A"zzzzzzzzzzzzzzzzzzzzzzzzz"
	SetWindow kwTopWin,userdata(ResizeControlsInfo) += A"zzzzzzzzzzzzzzzzzzz!!!"
	SetWindow kwTopWin,userdata(ResizeControlsGuides)=  "UGH0;UGH1;UGHL;UGHR;"
	SetWindow kwTopWin,userdata(ResizeControlsInfoUGH0)=  "NAME:UGH0;WIN:CodeBrowser;TYPE:User;HORIZONTAL:1;POSITION:100.00;GUIDE1:FT;GUIDE2:;RELPOSITION:100;"
	SetWindow kwTopWin,userdata(ResizeControlsInfoUGH1)=  "NAME:UGH1;WIN:CodeBrowser;TYPE:User;HORIZONTAL:1;POSITION:170.00;GUIDE1:FB;GUIDE2:;RELPOSITION:5;"
	SetWindow kwTopWin,userdata(oneTimeInit)=  "1"
	SetWindow kwTopWin,userdata(ResizeControlsInfoUGHL)=  "NAME:UGHL;WIN:CodeBrowser;TYPE:User;HORIZONTAL:0;POSITION:5.00;GUIDE1:FL;GUIDE2:;RELPOSITION:5;"
	SetWindow kwTopWin,userdata(ResizeControlsInfoUGHR)=  "NAME:UGHR;WIN:CodeBrowser;TYPE:User;HORIZONTAL:0;POSITION:300.00;GUIDE1:FR;GUIDE2:;RELPOSITION:5;"

	SetWindow kwTopWin, hook(mainHook)=CodeBrowserModule#panelHook
End

Function resizeToPackagePrefs()
	STRUCT CodeBrowserPrefs prefs
	LoadPackagePrefsFromDisk(prefs)

	Variable prefsLeft   = prefs.panelCoords[0]
	Variable prefsTop    = prefs.panelCoords[1]
	Variable prefsRight  = prefs.panelCoords[2]
	Variable prefsBottom = prefs.panelCoords[3]

	if(!existsPanel())
		createPanel()
	endif
	MoveWindow/W=$panel prefsLeft, prefsTop, prefsRight, prefsBottom
End

// Callback for the modules popup
// Stores the raw list as user data
Function/S generateModuleList()
	debugPrint("called")

	string niceList = getModuleList()
	niceList = AddListItem(CB_selectAll, niceList)

	PopupMenu PopupNamespace, win=$panel, userData($userDataNiceList)=niceList

	return niceList
End

// Callback for the procedure popup, returns a nicified list
// Stores both the nicified list and the raw list as user data
Function/S generateProcedureList()
	string procList, niceList

	procList = AddListItem(CB_selectAll, "")
	niceList = AddListItem(CB_selectAll, "")

	getProcedureList(procList, niceList)
	PopupMenu PopupProcedure, win=$panel, userData($userDataRawList)=procList, userData($userDataNiceList)=niceList

	return niceList
End

/// @brief get a list of all procedures from the currently selected module
///
/// @param[in]  module   select a valid module or CB_selectAll
/// @param[out] procList list of procedures with unique items
/// @param[out] niceList list of procedures for display (without module)
Function getProcedureList(procList, niceList)
	string &procList, &niceList

	string module, modules
	variable numModules, i

	module = getCurrentItem(module = 1)
	if(!cmpstr(module, CB_selectAll))
		modules = getModuleList()
		numModules = ItemsInList(modules)
		for(i = 0; i < numModules; i += 1)
			module = StringFromList(i, modules)
			procList += getProcList(module)
			if(isProcGlobal(module))
				niceList += getProcList(module)
			else
				niceList += getProcList(module, addModule = 1)
			endif
		endfor
	else
		procList += getProcList(module)
		niceList += procList[strlen(CB_selectAll) + 1, inf]
	endif

	niceList = ProcedureListRemoveModule(niceList)
	niceList = ProcedureListRemoveEnding(niceList)
End

// Must be called after every change which might affect the panel contents
// Installed as AfterCompiledHook
Function updatePanel()

	saveReParse()
	debugPrint("All Procedures were marked for parsing")

	if(!existsPanel())
		return 0
	endif

	ControlUpdate/A/W=$panel
	updateListBoxHook()

	return 0
End

Function existsPanel()
	DoWindow $panel
	if(V_flag == 0)
		debugPrint("panel does not exist")
		return 0
	endif
	debugPrint("panel exists")
	return 1
End

Function markAsUnInitialized()
	if(!existsPanel())
		return 0
	endif

	setGlobalVar("initialized", 0)
	debugPrint("panel marked as uninitialized")
End

Function markAsInitialized()
	if(!existsPanel())
		return 0
	endif

	setGlobalVar("initialized", 1)
	debugPrint("panel marked as initialized")
End

Function isInitialized()
	if(!existsPanel())
		return 0
	endif

	return getGlobalVar("initialized") == 1
End

/// Returns the currently selected item from the panel defined by the optional arguments.
///
/// Exactly one optional argument must be given.
///
/// @param module     [optional] Module from ProcGlobal/Independent Module list
/// @param procedure  [optional] "myProcedure.ipf [moduleName]"
/// @param index      [optional] Zero-based index into main listbox
///
/// @returns the currently selected item
Function/S getCurrentItem([module, procedure, index])
	variable module, procedure, index

	string procName, rawList

	module    =  ParamIsDefault(module)    ? 0 : 1
	procedure =  ParamIsDefault(procedure) ? 0 : 1
	index     =  ParamIsDefault(index)     ? 0 : 1

	// only one optional argument allowed
	if(module + procedure + index != 1)
		return genericError
	endif

	if(module)
		ControlInfo/W=$panel PopupNamespace

		if(V_Value > 0)
			return S_Value
		endif
	elseif(index)
		ControlInfo/W=$panel List1

		if(V_Value >= 0)
			return num2str(V_Value)
		endif
	elseif(procedure)
		ControlInfo/W=$panel PopupProcedure
		V_Value -= 1 // 1-based index

		rawList = GetUserData(panel, "PopupProcedure", userDataRawList)
		if(V_Value < 0 || V_Value >= ItemsInList(rawList))
			if(ItemsInList(rawList) > 0)
				// fall back to first item
				return StringFromList(0, rawList)
			endif
			return genericError
		endif

		procName = StringFromList(V_Value, rawList)
		return procName
	endif

	return genericError
End

/// Get the basic procedure name from a full procedure name
///
/// @param fullName  "myProcedure.ipf [moduleName]"
///
/// @returns myProcedure.ipf without module definition
Function/S ProcedureWithoutModule(fullName)
	string fullName

	return RemoveEverythingAfter(fullName, " [")
End

/// Get the module name from a full procedure name
///
/// @param fullName  "myProcedure.ipf [moduleName]"
///
/// @returns moduleName without procedure specification
Function/S ModuleWithoutProcedure(fullName)
	string fullName

	string module, procedure

	SplitString/E="(.*)\ \[(\w+)\]" fullName, procedure, module
	if(V_flag != 2)
		return ""
	endif

	return module
End

// Returns the currently selected item from the panel defined by the optional arguments.
// Argument is returned as number in current list
// Exactly one optional argument must be given.
//
// module:              return selected NameSpace
// procedure:           return selected procedure
// index:               return selected index in listbox
Function getCurrentItemAsNumeric([module, procedure, index, indexTop])
	variable module, procedure, index, indexTop

	string procName

	module                 =  ParamIsDefault(module)                 ? 0 : 1
	procedure              =  ParamIsDefault(procedure)              ? 0 : 1
	index                  =  ParamIsDefault(index)                  ? 0 : 1
	indexTop               =  ParamIsDefault(indexTop)               ? 0 : 1

	// only one optional argument allowed
	if(module + procedure + index + indexTop != 1)
		return -1 // error
	endif

	if(module)
		ControlInfo/W=$panel PopupNamespace
	elseif(procedure)
		ControlInfo/W=$panel PopupProcedure
	elseif(index || indexTop)
		ControlInfo/W=$panel List1
	endif

	if(V_Value >= 0)
		if(indexTop)
			return V_startRow
		endif
		return V_Value
	endif

	return -1 // error
End

// Updates the given popup menu
// Tries to preserve the currently selected item
Function updatePopup(ctrlName)
	string ctrlName

	string itemText = "", list
	variable index

	ControlInfo/W=$panel $ctrlName
	index = V_Value
	if(!isEmpty(S_Value))
		itemText = S_Value
	endif

	ControlUpdate/W=$panel $ctrlName

	list = GetUserData(panel, "PopupProcedure", userDataNiceList)

	if(ItemsInList(list) == 1)
		PopupMenu $ctrlName win=$panel, disable=2
	else
		PopupMenu $ctrlName win=$panel, disable=0
	endif

	// try to restore the previously selected item if it differs from the current one
	variable newIndex = WhichListItem(itemText, list) + 1

	if(newIndex != index) // only update if required, as the update triggers the list generating function
		if(newIndex > 0)
			PopupMenu $ctrlName win=$panel, mode=newIndex
		else
			PopupMenu $ctrlName win=$panel, mode=1
		endif
	endif
End

Function popupModules(pa) : PopupMenuControl
	STRUCT WMPopupAction &pa

	string procedure

	switch(pa.eventCode)
		case 2: // mouse up
			debugprint("mouse up")

			string module = pa.popStr

			if(isEmpty(module))
				break
			endif

			updatePopup("PopupProcedure")

			if(updateListBoxHook() == 0)
				procedure = getCurrentItem(procedure = 1)
				if(!!cmpstr(procedure, CB_selectAll))
					DisplayProcedure/W=$procedure
				endif
			endif
			break
	endswitch

	return 0
End

Function popupProcedures(pa) : PopupMenuControl
	STRUCT WMPopupAction &pa

	switch(pa.eventCode)
		case 2: // mouse up
			debugprint("mouse up")

			string procedure = pa.popStr

			if(isEmpty(procedure))
				break
			endif

			if(updateListBoxHook() == 0)
				if(!!cmpstr(procedure, CB_selectAll))
					DisplayProcedure/W=$procedure
				endif
			endif
			break
	endswitch

	return 0
End

Function CheckboxSort(cba) : CheckBoxControl
	STRUCT WMCheckboxAction &cba

	switch(cba.eventCode)
		case 2: // mouse up
			updateListBoxHook()
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

// returns 0 if checkbox is deselected or 1 if it is selected.
Function returnCheckBoxSort()
	ControlInfo/W=$panel CheckboxSort
	if(V_flag == 2)		// Checkbox found?
		return V_Value
	else
		//Fallback: Sorting as default behaviour
		return 1
	endif
End

/// @brief Action procedure for the SetVariable @c SetSearch
Function SetVarProcedureSearch(sva) : SetVariableControl
	STRUCT WMSetVariableAction &sva

	switch( sva.eventCode )
		case 1: // mouse up
		case 2: // Enter key
		case 3: // Live update
			Variable dval = sva.dval
			String sval = sva.sval
			updateListBoxHook()
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

/// @brief Action procedure for the SetVariable @c SetProcedureFilter
Function SetVarProcedureFilter(sva) : SetVariableControl
	STRUCT WMSetVariableAction &sva

	switch( sva.eventCode )
		case 1: // mouse up
		case 2: // Enter key
		case 3: // Live update
			Variable dval = sva.dval
			String sval = sva.sval
			updatePopup("PopupProcedure")
			updateListBoxHook()
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function listBoxProc(lba) : ListBoxControl
	STRUCT WMListboxAction &lba

	Variable row = lba.row
	Variable col = lba.col
	WAVE/T/Z listWave = lba.listWave
	WAVE/Z selWave = lba.selWave
	string procedure

	switch(lba.eventCode)
		case -1: // control being killed
			break
		case 1: // mouse down
			break
		case 3: // double click

			if(!WaveExists(listWave) || row >= DimSize(listWave,0))
				return 0
			endif

			showCode(row)
			break
		case 4: // cell selection
		case 5: // cell selection plus shift key
			ControlInfo/W=$panel List1
			if(V_selCol == 0)
				// forcefully deselect column zero if it is selected
				ListBox List1, win=$panel, selCol=1
			endif
			break
		case 12: // keystroke
			if(!WaveExists(listWave))
				return 0
			endif

			if(row == openkey)
				procedure = getCurrentItem(procedure=1)
				variable listIndex = str2num(getCurrentItem(index=1))
				showCode(listIndex)
			endif
			break
		case 13: // checkbox clicked (Igor 6.2 or later)
			break
	endswitch

	return 0
End
