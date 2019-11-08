#cs
	MIT License

	Copyright (c) 2016 Tariqul Islam
	http://teenyscript.tarre.nu/
	https://www.autoitscript.com/forum/profile/65348-tarretarretarre/
	https://github.com/tarreislam

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
#ce
#include <ButtonConstants.au3>
#include <EditConstants.au3>
#include <GUIConstantsEx.au3>
#include <GuiListView.au3>
#include <StaticConstants.au3>
#include <TabConstants.au3>
#include <WindowsConstants.au3>
Global Const $___MacroZzzZz = @CRLF & @CRLF & "~Avilable macros " & @CRLF & @CRLF & "%main.name% = Project name" & @CRLF & "%main.ver% = Project version" & @CRLF & "%build.arch% = 32, 64 or 96" & @CRLF &"%project.dir% = Directory of TS.project.ini" & @CRLF

Global $Gui_Main, $gui_Project_Settings, $gui_Main_Input_CmdLine, $gui_Main_btn_create_new_project, $gui_Main_btn_edit_project, $gui_Hotkeys_list_hotkeys, $gui_Hotkeys_btn_change, $gui_Hotkeys_btn_reset_to_default, _
$gui_Main_btn_install_calltips, $gui_Project_Settings_radio_32_bit, $gui_Project_Settings_radio_64_bit, $gui_Project_Settings_radio_32_and_64_bit, $gui_Project_Settings_input_project_name, _
$gui_Project_Settings_input_project_version, $gui_Project_Settings_btn_output_dir, $gui_Project_Settings_input_output_dir, $gui_Project_Settings_btn_icon, $gui_Project_Settings_input_icon, _
$gui_Project_Settings_btn_save, $gui_Project_Settings_input_project_copyright_holder, $gui_Project_Settings_radio_gui, $gui_Project_Settings_radio_console, $gui_Project_Settings_checkbox_include_launcher, _
$gui_Project_Settings_btn_update_ts_ver, $GuiOpt_Project_Settings_UpdateTsver
; When exiting from the main function
Func GuiOpt_Main_Exit()
	Local $_GET_Main_Input_CmdLine = GUICtrlRead($gui_Main_Input_CmdLine)
	$_resource_CmdLine = $_GET_Main_Input_CmdLine
	;write the last used CmdLine
	IniWrite($_TS_OptFile, "misc", "CmdLine", $_GET_Main_Input_CmdLine)
	Return GuiOpt_Exit()
EndFunc

Func GuiOpt_Exit()
	GUIDelete(@GUI_WinHandle)
EndFunc

Func GuiOpt_Main()
	If IsHWnd($Gui_Main) Then Return WinActivate($Gui_Main)
	$Gui_Main = GUICreate(StringFormat("%s Settings", $_TS_AppTitle), 280, 248, 418, 269)
	GUICtrlCreateTab(2, 2, 273, 233)
	GUICtrlCreateTabItem("Main")
	GUICtrlCreateLabel("CmdLine parameters", 10, 42, 100, 17)
	$gui_Main_Input_CmdLine = GUICtrlCreateInput("", 10, 66, 145, 21)
	$gui_Main_btn_create_new_project = GUICtrlCreateButton("Create new project", 154, 202, 115, 25)
	$gui_Main_btn_edit_project = GUICtrlCreateButton("&Edit Project Settings", 8, 200, 115, 25)
	GUICtrlCreateTabItem("Hotkeys")
	$gui_Hotkeys_btn_change = GUICtrlCreateButton("Change", 10, 202, 75, 25)
	$gui_Hotkeys_btn_reset_to_default = GUICtrlCreateButton("Reset to default", 178, 202, 91, 25)
	$gui_Hotkeys_list_hotkeys = GUICtrlCreateListView("Name|Key", 8, 32, 258, 166)
	GUICtrlCreateTabItem("Misc")
	$gui_Main_btn_install_calltips = GUICtrlCreateButton("Install Calltips to scite", 8, 32, 155, 25)
	GUICtrlCreateTabItem("")
	GUISetState(@SW_SHOW)


	; Set data
	GUICtrlSetData($gui_Main_Input_CmdLine, IniRead($_TS_OptFile, "misc", "CmdLine", ""))
	; Exit window
	GUISetOnEvent($GUI_EVENT_CLOSE, "GuiOpt_Main_Exit")
	;Set hotkey
	GUICtrlSetOnEvent($gui_Hotkeys_btn_change, "GuiOpt_Main_SetHotkey")
	;Reset to default hotkeys
	GUICtrlSetOnEvent($gui_Hotkeys_btn_reset_to_default, "GuiOpt_Main_ResetHotkey")

	;Install Calltips
	GUICtrlSetOnEvent($gui_Main_btn_install_calltips, "GuiOpt_Main_btn_install_calltips")

	;Edit project
	GUICtrlSetOnEvent($gui_Main_btn_edit_project, "GuiOpt_Project_Settings_Edit_Project")
	;New project
	GUICtrlSetOnEvent($gui_Main_btn_create_new_project, "GuiOpt_Project_Settings_create_new_project")
	; Display hotkeys
	GuiOpt_Main_getHotkeyList()
EndFunc

#Region Misc
Func GuiOpt_Main_btn_install_calltips()
	MsgBox($MB_ICONINFORMATION, $_TS_AppTitle, "Installning calltips requires SciTE to restart.", 0, $Gui_Main)

	If FileExists($_SCITE_USER_CALLTIPS_API) Then
		$MsgBox = MsgBox($MB_ICONWARNING + $MB_YESNO, $_TS_AppTitle, StringFormat("The file '%s' already exists. Previous content will be ereased, do you wish to proceed?",$_SCITE_USER_CALLTIPS_API), 0, $Gui_Main)
		If $MsgBox == $IDNO Then Return _TS_AbortedByUser($Gui_Main)
	EndIf

	If FileExists($_SCITE_USER_UDFS_PROPS) Then
		$MsgBox = MsgBox($MB_ICONWARNING + $MB_YESNO, $_TS_AppTitle, StringFormat("The file '%s' already exists. Previous content will be ereased, do you wish to proceed?",$_SCITE_USER_CALLTIPS_API), 0, $Gui_Main)
		If $MsgBox == $IDNO Then Return _TS_AbortedByUser($Gui_Main)
	EndIf

	;install Calltips
	Local Const $sCalltips_api = _ArrayToString($_SCITE_aCALLTIPS, @CRLF)
	Local $sUserUdfs_props = StringFormat("au3.keywords.user.udfs=%s", StringLower(_ArrayToString($_SCITE_aUSER_UDFS, " ")))

	; Install Api
	Local Const $sCalltips_apiHandle = FileOpen($_SCITE_USER_CALLTIPS_API, $FO_OVERWRITE + $FO_CREATEPATH)
	If Not $sCalltips_apiHandle Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("Failed to open '%s' for writing", $_SCITE_USER_CALLTIPS_API), 0, $Gui_Main)
	FileWrite($sCalltips_apiHandle, $sCalltips_api)
	FileClose($sCalltips_apiHandle)

	; Install au3.UserUdfs.properties
	Local Const $sUserUdfs_propsHandle = FileOpen($_SCITE_USER_UDFS_PROPS, $FO_OVERWRITE + $FO_CREATEPATH)
	If Not $sUserUdfs_propsHandle Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("Failed to open '%s' for writing", $_SCITE_USER_UDFS_PROPS), 0, $Gui_Main)
	FileWrite($sUserUdfs_propsHandle, $sUserUdfs_props)
	FileClose($sUserUdfs_propsHandle)

	If FileExists($_AU3_SCITE_ROAMING_DIR) Then
		$MsgBox = MsgBox($MB_ICONINFORMATION + $MB_YESNO, $_TS_AppTitle, StringFormat("Scite was also detected in '%s' would you like to copy the files there?", $_AU3_SCITE_ROAMING_DIR), 0, $Gui_Main)
		; Copy to roaming dir and replace
		If $MsgBox == $IDYES Then
			FileCopy($_SCITE_USER_CALLTIPS_API, $_AU3_SCITE_ROAMING_DIR, $FC_OVERWRITE)
			FileCopy($_SCITE_USER_UDFS_PROPS, $_AU3_SCITE_ROAMING_DIR, $FC_OVERWRITE)
		EndIf
	EndIf

	MsgBox($MB_ICONINFORMATION, $_TS_AppTitle, "Calltips installed successfully!", 0, $Gui_Main)
	Return True
EndFunc
#EndRegion Misc

#Region Project related
Func GuiOpt_Project_Settings_Save()
	Local $Warnings = 0
	Local Const $getFromFilepath_basedir = getFromFilepath_basedir(_SciTe_getOpenFileName())
	Local Const $_TS_ProjectFile = StringFormat($_TS_Project_FilePatt, $getFromFilepath_basedir)
	If Not FileExists($_TS_ProjectFile) Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, "Failed to save in 'TS.project.ini', make sure SciTE is focused on the main file that co-exists with the TS.project.ini file", 0, $gui_Project_Settings)


	Local $main_name = GUICtrlRead($gui_Project_Settings_input_project_name), _
	 $main_ver = GUICtrlRead($gui_Project_Settings_input_project_version), _
	 $main_copyright = GUICtrlRead($gui_Project_Settings_input_project_copyright_holder), _
	 $build_arch, _ ; Defined below
	 $build_dir = GUICtrlRead($gui_Project_Settings_input_output_dir), _
	 $build_icon = GUICtrlRead($gui_Project_Settings_input_icon), _
	 $build_type; gui \ console

	If isEmpty($main_name) Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, "The project name may not be empty", 0, $gui_Project_Settings)
	If StringRight($build_dir, 1) == "\" Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, "The Output dir may not end with an backslash '\'", 0, $gui_Project_Settings)

	If Not StringRegExp($main_ver, "^[0-9.]+$") Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("The project version '%s' may only contain numbers and dots", $main_ver), 0, $gui_Project_Settings)
	If StringRegExp($main_name, '\"') Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("The project name '%s' may not contain ''", $main_name), 0, $gui_Project_Settings)
	If StringRegExp($main_copyright, '\"') Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("The copyright holder '%s' may not contain ''", $main_copyright), 0, $gui_Project_Settings)
	If StringRegExp($build_dir, '\"') Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("The project icon '%s' may not contain ''", $build_dir), 0, $gui_Project_Settings)
	If StringRegExp($build_icon, '\"') Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("The build directory '%s' may not contain ''", $build_icon), 0, $gui_Project_Settings)

	If GUICtrlRead($gui_Project_Settings_radio_gui) == $GUI_CHECKED Then $build_type = "gui"
	If GUICtrlRead($gui_Project_Settings_radio_console) == $GUI_CHECKED Then $build_type = "console"

	If GUICtrlRead($gui_Project_Settings_radio_32_bit) == $GUI_CHECKED Then $build_arch = "32"
	If GUICtrlRead($gui_Project_Settings_radio_64_bit) == $GUI_CHECKED Then $build_arch = "64"
	If GUICtrlRead($gui_Project_Settings_radio_32_and_64_bit) == $GUI_CHECKED Then $build_arch = "96"

	_TS_Project_setSettings($_TS_ProjectFile, _
	$main_name, _
	$main_ver, _
	$main_copyright, _
	$build_arch, _
	$build_dir, _
	$build_icon, _
	$build_type, _
	GUICtrlRead($gui_Project_Settings_checkbox_include_launcher) == $GUI_CHECKED, _
	$GuiOpt_Project_Settings_UpdateTsver)

	; Generate warning if the directory dosent exist (Dont prompt it as an error)
	Local Const $oProjectSettings = _TS_Project_getSettings($_TS_ProjectFile, $getFromFilepath_basedir)

	If Not FileExists($oProjectSettings.dir) Then
		$Warnings+=1
		MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("Warning! '%s' the directory '%s' does not exist yet.", GUICtrlRead($gui_Project_Settings_input_output_dir), $oProjectSettings.dir), 0, $gui_Project_Settings)
	EndIf

	If Not FileExists($oProjectSettings.icon) Then
		$Warnings+=1
		MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("Warning! '%s' the icon '%s' does not exist yet.", GUICtrlRead($gui_Project_Settings_input_icon), $oProjectSettings.icon), 0, $gui_Project_Settings)
	EndIf

	MsgBox(($Warnings ? $MB_ICONWARNING : $MB_ICONINFORMATION), $_TS_AppTitle, "Project settings updated " & ($Warnings ? StringFormat("with %d warnings", $Warnings) :  "successfully!"), 0, $gui_Project_Settings)
EndFunc

Func GuiOpt_Project_Settings_Edit_Project()
	If IsHWnd($gui_Project_Settings) Then Return WinActivate($gui_Project_Settings)
	; Check if we can find that damn file xD
	Local Const $getFromFilepath_basedir = getFromFilepath_basedir(_SciTe_getOpenFileName())
	Local Const $_TS_ProjectFile = StringFormat($_TS_Project_FilePatt, $getFromFilepath_basedir)

	If Not FileExists($_TS_ProjectFile) Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("Could not find '%s', make sure SciTE is focused on the main file that co-exists with the '%s' file", $_TS_Project_Ts_PROJECT_INI, $_TS_Project_Ts_PROJECT_INI), 0, $Gui_Main)
	$GuiOpt_Project_Settings_UpdateTsver = False; Mark for update

	$gui_Project_Settings = GUICreate("Edit Project Settings", 474, 269, 418, 321, -1, -1, $Gui_Main)
	GUICtrlCreateGroup("Architecture  options", 240, 8, 225, 57)
	$gui_Project_Settings_radio_32_bit = GUICtrlCreateRadio("32 bit", 248, 24, 49, 17)
	$gui_Project_Settings_radio_64_bit = GUICtrlCreateRadio("64 bit", 304, 24, 49, 17)
	$gui_Project_Settings_radio_32_and_64_bit = GUICtrlCreateRadio("32 && 64 bit", 360, 24, 81, 17)
	$gui_Project_Settings_checkbox_include_launcher = GUICtrlCreateCheckbox("Include launcher", 360, 40, 97, 17)
	GUICtrlCreateGroup("", -99, -99, 1, 1)
	GUICtrlCreateGroup("File Details", 8, 56, 225, 177)
	GUICtrlCreateLabel("Project name", 16, 72, 66, 17)
	$gui_Project_Settings_input_project_name = GUICtrlCreateInput("", 16, 88, 209, 21)
	GUICtrlCreateLabel("Project version", 16, 120, 74, 17)
	$gui_Project_Settings_input_project_version = GUICtrlCreateInput("", 16, 144, 209, 21)
	GUICtrlCreateLabel("Copyright holder", 16, 176, 80, 17)
	$gui_Project_Settings_input_project_copyright_holder = GUICtrlCreateInput("", 16, 200, 209, 21)
	GUICtrlCreateGroup("", -99, -99, 1, 1)
	GUICtrlCreateGroup("Misc", 240, 64, 225, 137)
	$gui_Project_Settings_btn_output_dir = GUICtrlCreateButton("&Output dir", 248, 80, 75, 25)
	$gui_Project_Settings_input_output_dir = GUICtrlCreateInput("", 248, 112, 209, 21, BitOR($GUI_SS_DEFAULT_INPUT,$ES_READONLY))
	$gui_Project_Settings_btn_icon = GUICtrlCreateButton("&Icon", 248, 136, 75, 25)
	$gui_Project_Settings_input_icon = GUICtrlCreateInput("", 248, 168, 209, 21, BitOR($GUI_SS_DEFAULT_INPUT,$ES_READONLY))
	GUICtrlCreateGroup("", -99, -99, 1, 1)
	$gui_Project_Settings_btn_save = GUICtrlCreateButton("Verify && Apply", 240, 208, 227, 25)
	GUICtrlCreateGroup("Type of application ", 8, 8, 225, 41)
	$gui_Project_Settings_radio_gui = GUICtrlCreateRadio("Gui", 16, 24, 57, 17)
	$gui_Project_Settings_radio_console = GUICtrlCreateRadio("Console", 80, 24, 81, 17)
	GUICtrlCreateGroup("", -99, -99, 1, 1)
	$gui_Project_Settings_btn_update_ts_ver = GUICtrlCreateButton("Update project's TeenyScript version", 8, 240, 227, 25)

	; Set project data to gui
	Local $oProject = _TS_Project_getSettings($_TS_ProjectFile, $getFromFilepath_basedir, False)
	GUICtrlSetData($gui_Project_Settings_input_project_name, $oProject.name)
	GUICtrlSetData($gui_Project_Settings_input_project_version, $oProject.ver)
	GUICtrlSetData($gui_Project_Settings_input_project_copyright_holder, $oProject.copyright)
	GUICtrlSetData($gui_Project_Settings_input_output_dir, $oProject.dir)
	GUICtrlSetData($gui_Project_Settings_input_icon, $oProject.icon)


	; Use Launcher?

	Switch $oProject.includeLauncher
		Case "True"
			GUICtrlSetState($gui_Project_Settings_checkbox_include_launcher, $GUI_CHECKED)
		Case "False"
			GUICtrlSetState($gui_Project_Settings_checkbox_include_launcher, $GUI_UNCHECKED)
	EndSwitch

	; Determine options for Arch
	Switch $oProject.arch; Will default 32 in WCS
		Case '32'
			GUICtrlSetState($gui_Project_Settings_radio_32_bit, $GUI_CHECKED)
			GUICtrlSetState($gui_Project_Settings_checkbox_include_launcher, $GUI_DISABLE)
		Case '64'
			GUICtrlSetState($gui_Project_Settings_radio_64_bit, $GUI_CHECKED)
			GUICtrlSetState($gui_Project_Settings_checkbox_include_launcher, $GUI_DISABLE)
		Case '96'
			GUICtrlSetState($gui_Project_Settings_radio_32_and_64_bit, $GUI_CHECKED)
			GUICtrlSetState($gui_Project_Settings_checkbox_include_launcher, $GUI_ENABLE)
		Case Default
			GUICtrlSetState($gui_Project_Settings_radio_32_bit, $GUI_CHECKED)
			GUICtrlSetState($gui_Project_Settings_checkbox_include_launcher, $GUI_DISABLE)
	EndSwitch

	; Determine build type
	Switch $oProject.type
		Case "gui"
			GUICtrlSetState($gui_Project_Settings_radio_gui, $GUI_CHECKED)
		Case "console"
			GUICtrlSetState($gui_Project_Settings_radio_console, $GUI_CHECKED)
		Case Else
			GUICtrlSetState($gui_Project_Settings_radio_gui, $GUI_CHECKED)
	EndSwitch

	GUISetState(@SW_SHOW)
	; Check if a launcher can be used (only with 32 and 64)
	GUICtrlSetOnEvent($gui_Project_Settings_radio_32_bit, "GuiOpt_Project_Settings_checkbox_include_launcher")
	GUICtrlSetOnEvent($gui_Project_Settings_radio_64_bit, "GuiOpt_Project_Settings_checkbox_include_launcher")
	GUICtrlSetOnEvent($gui_Project_Settings_radio_32_and_64_bit, "GuiOpt_Project_Settings_checkbox_include_launcher")
	; Exit window && Apply
	GUISetOnEvent($GUI_EVENT_CLOSE, "GuiOpt_Exit"); Ignore changes
	GUICtrlSetOnEvent($gui_Project_Settings_btn_save, "GuiOpt_Project_Settings_Save")
	;Editz
	GUICtrlSetOnEvent($gui_Project_Settings_btn_output_dir, "GuiOpt_Project_Settings_Set_Output_dir")
	GUICtrlSetOnEvent($gui_Project_Settings_btn_icon, "GuiOpt_Project_Settings_Set_Icon")
	GUICtrlSetOnEvent($gui_Project_Settings_btn_update_ts_ver, "GuiOpt_Project_Settings_UpdateTsver")
	; VCS
	_TS_Project_VCS($oProject, $gui_Project_Settings)
EndFunc

Func GuiOpt_Project_Settings_create_new_project(); CNP
	Local Static $hGui = Null
	If IsHWnd($hGui) Then WinActivate($hGui)

	; This gui dosent need any events. so we gonna toggle it here.
	$hGui = GUICreate("Create new project", 339, 214, 220, 147, -1, -1, $Gui_Main)
	Local $list_projects = GUICtrlCreateListView("Project name|Size", 8, 8, 322, 166)
	Local $btn_next = GUICtrlCreateButton("Next", 256, 176, 75, 33)
	; List the default projects, followed by the user defined ones

	Local $aoProjects = _TS_Project_getProjectCollection(), $aoProject_id

	If $aoProjects[0] == 0 Then Return MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("Failed to load templates from '%s' ", $_TS_Project_TS_Template_DIR), 0 , $Gui_Main)
	GuiOpt_Project_Settings_CNP_getProjectList($list_projects, $aoProjects)
	GUISetState(@SW_SHOW)

	; Disable Gui events for this step
	Opt("GUIOnEventMode", 0)
	Local $GUIGetMsg
	While True
		$GUIGetMsg = GUIGetMsg()
		Switch $GUIGetMsg
			Case $btn_next
				; Check for indices before we remove the GUI
				$aoProject_id = Int(_GUICtrlListView_GetSelectedIndices($list_projects)) + 1
				ExitLoop
			Case $GUI_EVENT_CLOSE
				ExitLoop
		EndSwitch
	WEnd
	; Delete the gui and set the static variable $hGui to null again
	GUIDelete($hGui)
	$hGui = Null
	Opt("GUIOnEventMode",1)

	; If we aborted
	If $GUIGetMsg == $GUI_EVENT_CLOSE Then Return _TS_AbortedByUser($Gui_Main)

	Local $oProject = $aoProjects[$aoProject_id]

	Local Const $sNewProjectTargetDir = FileSelectFolder(StringFormat("%s - Select a new folder for your '%s' project", $_TS_AppTitle, $oProject.project.name), @DocumentsCommonDir, 0, "", $Gui_Main)
	If @error Then Return GuiOpt_Project_Settings_create_new_project(); Go back to prev window

	; Create
	If Not _TS_Project_createNewproject($oProject, $sNewProjectTargetDir) Then
		MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("There was a problem creating '%s'. @error: %d, @extended: %d. Opening folder for inspection", $oProject.project.name, @error, @extended), 0, $Gui_Main)
	Else
		MsgBox($MB_ICONINFORMATION, $_TS_AppTitle, StringFormat("The project '%s' has been created succesfully!", $oProject.project.name), 0, $Gui_Main)
		; Edit project
		GuiOpt_Project_Settings_Edit_Project()
	EndIf

EndFunc

Func GuiOpt_Project_Settings_Set_Output_dir()
	Local $try = InputBox($_TS_AppTitle, "Set output dir. ! TS AUTOMATICLY INCLUDES PROJECT NAME ! ONLY ENTER THE DESIRED OUT DIRECTORY !" & $___MacroZzzZz, GUICtrlRead($gui_Project_Settings_input_output_dir), Default, 350, 250, Default, Default, 0, $gui_Project_Settings)
	If Not @error Then GUICtrlSetData($gui_Project_Settings_input_output_dir, $try)
EndFunc

Func GuiOpt_Project_Settings_Set_Icon()
	Local $try = InputBox($_TS_AppTitle, "Set icon name." & $___MacroZzzZz, GUICtrlRead($gui_Project_Settings_input_icon), Default, 350, 250, Default, Default, 0, $gui_Project_Settings)
	If Not @error Then GUICtrlSetData($gui_Project_Settings_input_icon, $try)
EndFunc

Func GuiOpt_Project_Settings_UpdateTsver()
	Local $MsgBox = MsgBox($MB_ICONWARNING + $MB_YESNO, $_TS_AppTitle, StringFormat("You are about the mark this project with the current TeenyScript version '%s'. Do you wish to continue?", $_TS_AppVer), 0, $gui_Project_Settings)
	Switch $MsgBox
		Case $IDYES
			$GuiOpt_Project_Settings_UpdateTsver = True
			Return MsgBox($MB_ICONINFORMATION, $_TS_AppTitle, StringFormat("This project TeenyScript version is now set with '%s'. Changes will take affect when you apply changes", $_TS_AppVer), 0, $gui_Project_Settings)
		Case $IDNO
			$GuiOpt_Project_Settings_UpdateTsver = False
			Return _TS_AbortedByUser($gui_Project_Settings)
	EndSwitch
EndFunc

Func GuiOpt_Project_Settings_checkbox_include_launcher()
	If @GUI_CtrlId == $gui_Project_Settings_radio_32_and_64_bit Then
		GUICtrlSetState($gui_Project_Settings_checkbox_include_launcher, $GUI_ENABLE)
	Else
		GUICtrlSetState($gui_Project_Settings_checkbox_include_launcher, $GUI_UNCHECKED)
		GUICtrlSetState($gui_Project_Settings_checkbox_include_launcher, $GUI_DISABLE)
	EndIf
EndFunc

Func GuiOpt_Project_Settings_CNP_getProjectList(ByRef $list_projects, ByRef $aoProjects)
	_GUICtrlListView_DeleteAllItems($list_projects)
	Local $oProject
	For $i = 1 To $aoProjects[0]
		$oProject = $aoProjects[$i]
		GUICtrlCreateListViewItem(StringFormat("%s|%s", $oProject.project.name, _rGetSize($oProject.dir)), $list_projects)
	Next
	_GUICtrlListView_SetItemSelected($list_projects, 0)
	_GUICtrlListView_SetColumnWidth($list_projects, 0, $LVSCW_AUTOSIZE)
EndFunc
#EndRegion Project related

#Region Hotkey related
Func GuiOpt_Main_getHotkeyList()
	_GUICtrlListView_DeleteAllItems($gui_Hotkeys_list_hotkeys)
	For $i = 1 To $_SCITE_HotkeyCollectionKeys[0]
		GUICtrlCreateListViewItem(StringFormat("%s|%s",$_SCITE_HotkeyCollectionDisplayNames[$i], $_SCITE_HotkeyCollectionKeys[$i]), $gui_Hotkeys_list_hotkeys)
	Next
	_GUICtrlListView_SetColumnWidth($gui_Hotkeys_list_hotkeys, 0, $LVSCW_AUTOSIZE)
EndFunc

Func GuiOpt_Main_ResetHotkey()
	; Just reset 8-|
	IniWrite($_TS_OptFile, "hotkeys", "run", "{F5}")
	IniWrite($_TS_OptFile, "hotkeys", "build_au3", "{F6}")
	IniWrite($_TS_OptFile, "hotkeys", "build_exe", "{F7}")
	IniWrite($_TS_OptFile, "hotkeys", "set_options", "{F8}")
	IniWrite($_TS_OptFile, "hotkeys", "exit", "{F10}")
	$_SCITE_HotkeyCollectionKeys[1] = "{F5}"
	$_SCITE_HotkeyCollectionKeys[2] = "{F6}"
	$_SCITE_HotkeyCollectionKeys[3] = "{F7}"
	$_SCITE_HotkeyCollectionKeys[4] = "{F8}"
	$_SCITE_HotkeyCollectionKeys[5] = "{F10}"
	GuiOpt_Main_getHotkeyList()
	MsgBox($MB_ICONINFORMATION, $_TS_AppTitle, "Hotkeys reset!", 0, $Gui_Main)
EndFunc

Func GuiOpt_Main_SetHotkey()
	Local $ListVeiw_Index = _GUICtrlListView_GetSelectedIndices($gui_Hotkeys_list_hotkeys)
	Local $HotkeyArr_Index = $ListVeiw_Index + 1
	Local $inputBox_neWHotkey = InputBox($_TS_AppTitle, StringFormat("Enter a new hotkey for '%s'. Rember to use {} if you are going to use F-keys", $_SCITE_HotkeyCollectionDisplayNames[$HotkeyArr_Index]), $_SCITE_HotkeyCollectionKeys[$HotkeyArr_Index], "", -1, -1, Default, Default, 0, $Gui_Main)
	If @error Then
		_TS_AbortedByUser($Gui_Main)
	Else
		; Check colission
		For $i = 1 To 5
			If $_SCITE_HotkeyCollectionKeys[$i] == $inputBox_neWHotkey Then
				MsgBox($MB_ICONWARNING, $_TS_AppTitle, StringFormat("The hotkey '%s' is already in use", $inputBox_neWHotkey), 0, $Gui_Main)
				Return
			EndIf
		Next
		; Change the hotkey
		$_SCITE_HotkeyCollectionKeys[$HotkeyArr_Index] = $inputBox_neWHotkey
		;Save the hotkey
		IniWrite($_TS_OptFile, "hotkeys", $_SCITE_HotkeyCollectionIniNames[$HotkeyArr_Index], $inputBox_neWHotkey)
		;Update the hotkey list
		GuiOpt_Main_getHotkeyList()
	EndIf

EndFunc

#EndRegion Hotkey related


