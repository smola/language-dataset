################################################################################
## OCS Inventory NG
## Copyleft OCS Inventory NG Team
## Web : http://www.ocsinventory-ng.org
##
## This code is open source and may be copied and modified as long as the source
## code is always made freely available.
## Please refer to the General Public Licence http://www.gnu.org/ or Licence.txt
################################################################################

; 2005 Shengalts Aleksander aka Instructor (Shengalts@mail.ru)
; Modified 2007 by Emmanuel GUILLORY (bibi92) for OCSPACKAGER
;-----------------------------------------------------------------------------------------------

!include "MUI.nsh"
!include "WordFunc.nsh"

!insertmacro MUI_LANGUAGE "English"

!define COL_FILE "Plugins.lst"
!define Compile_Version "2.6.0.0"

Var /GLOBAL HWND

BRANDINGTEXT "OCS Inventory NG Packager"
Icon "OCSInventory.ico"
ShowInstDetails hide
Name "OCS Inventory NG Packager (ListBox component)"
OutFile "ListBox.exe"
XPStyle on


################################################################################
# Version information
################################################################################
    VIProductVersion "${Compile_version}"
    VIAddVersionKey /LANG=${LANG_ENGLISH} "ProductName" "OCS Inventory NG Packager for Windows (ListBox component)"
    VIAddVersionKey /LANG=${LANG_ENGLISH} "Comments" "ListBox handling component for OCS Inventory NG Packager, used to select OCS Inventory NG Agent Plugins"
    VIAddVersionKey /LANG=${LANG_ENGLISH} "CompanyName" "OCS Inventory NG Team"
    VIAddVersionKey /LANG=${LANG_ENGLISH} "LegalTrademarks" "OCS Inventory NG Team"
    VIAddVersionKey /LANG=${LANG_ENGLISH} "LegalCopyright" "OCS Inventory NG Team"
    VIAddVersionKey /LANG=${LANG_ENGLISH} "FileDescription" "ListBox.exe"
    VIAddVersionKey /LANG=${LANG_ENGLISH} "FileVersion" "${Compile_version}"

Function .onInit
	setoutpath $EXEDIR
	delete "$exedir\${COL_FILE}"
	!insertmacro MUI_INSTALLOPTIONS_EXTRACT_AS "ListBox.ini" "ListBox.ini"
	call enumreg
FunctionEnd

Page Custom ShowCustom LeaveCustom
Page instfiles

LangString TEXT_IO_TITLE ${LANG_ENGLISH} "OCS Inventory NG Agent Plugins"
LangString TEXT_IO_SUBTITLE ${LANG_ENGLISH} "Select plugin files to package..."

Function ShowCustom
    !insertmacro MUI_HEADER_TEXT "$(TEXT_IO_TITLE)" "$(TEXT_IO_SUBTITLE)"
    !insertmacro MUI_INSTALLOPTIONS_INITDIALOG "ListBox.ini"
	Pop $HWND
;	GetDlgItem $1 $HWNDPARENT 1
;	EnableWindow $1 0
	!insertmacro MUI_INSTALLOPTIONS_SHOW
	Pop $0
FunctionEnd


Function LeaveCustom
	!insertmacro MUI_INSTALLOPTIONS_READ $0 "ListBox.ini" "Settings" "State"
	!insertmacro MUI_INSTALLOPTIONS_READ $R0 "ListBox.ini" "Field 1" "State"
	!insertmacro MUI_INSTALLOPTIONS_READ $R1 "ListBox.ini" "Field 4" "State"
	StrCmp $0 4 EnterEnable
	StrCmp $0 3 DeleteString
	StrCmp $0 1 AddString   ;//Autoadd after browse in FileRequest
	StrCmp $0 0 Enter

	abort

	EnterEnable:
	;StrCmp $R1 '' AddString
;	GetDlgItem $1 $HWNDPARENT 1                ;//Enable "Next" button
;	EnableWindow $1 1                          ;
;	GetDlgItem $1 $HWND 1203                   ;//Enable "delete" button
;	EnableWindow $1 1                          ;
	GetDlgItem $1 $HWND 1200                   ;//When selecting ListBox item
	SendMessage $1 ${WM_SETTEXT} 1 "STR:$R1"   ;// it sends to FileRequest
	abort

	AddString:
	IfFileExists $R0 +3
	MessageBox MB_OK 'File does not exists'
	abort
	GetDlgItem $1 $HWND 1204
	SendMessage $1 ${LB_FINDSTRING} 1 "STR:$R0" $0
	StrCmp $0 -1 +2
	MessageBox MB_OK 'File already exists in ListBox' IDOK +3
	SendMessage $1 ${LB_ADDSTRING} 1 "STR:$R0"
	CALL ADDstringtoreg
	goto EnterEnable
	abort

	DeleteString:
	GetDlgItem $1 $HWND 1204
	SendMessage $1 ${LB_FINDSTRING} 1 "STR:$R1" $0
	SendMessage $1 ${LB_DELETESTRING} $0 1
;	MessageBox MB_OK 'delete $0$R1'
	CALL DELstringtoreg
;	GetDlgItem $1 $HWNDPARENT 1                ;//Disable "Next" button
;	EnableWindow $1 0                          ;
;	GetDlgItem $1 $HWND 1203                   ;//Disable "delete" button
;	EnableWindow $1 0                          ;
	GetDlgItem $1 $HWND 1200                   ;//When deleting ListBox item
	SendMessage $1 ${WM_SETTEXT} 1 "STR:"      ;// it clear FileRequest
	abort

;=Enter=
	Enter:

       !insertmacro MUI_INSTALLOPTIONS_READ $R1 "ListBox.ini" "Field 4" "ListItems"
FunctionEnd

Function ADDstringtoreg
   call enumreg
   !insertmacro MUI_INSTALLOPTIONS_READ $R9 "ListBox.ini" "Field 4" "ListItems"
   ;messagebox mb_ok "reprise sur registre:: $r9"
   strcmp $R9 "" 0 addplus
   WriteRegExpandStr HKCU Software\OCS_PACKAGER\SETTINGS\FILELIST File $R0
   goto noadd
   addplus:
   ;messagebox mb_ok "addplus:: $R9 |  $R0 "
   WriteRegExpandStr HKCU Software\OCS_PACKAGER\SETTINGS\FILELIST File "$R9|$R0"
noadd:
 
Functionend

Function delstringtoreg
   ;lire fichier
   !insertmacro MUI_INSTALLOPTIONS_READ $R9 "ListBox.ini" "Field 4" "ListItems"
   ; chercher si on trouve |
   ;
   ;MEsSAGEBOX MB_OK "dans '$R9'  enlever $r1"
   ; remplacer srting
   ${WordReplace} $R9 "|$r1" "" "+" $r8
   ${WordReplace} $r8 $r1| "" "+"  $r8
   ${WordReplace} $r8 $r1 "" "+"  $r8
   ; messagebox mb_ok "RESULAT--  $R8"
   !insertmacro MUI_INSTALLOPTIONS_WRITE "ListBox.ini" "Field 4" "ListItems" $R8
   ; copier contenu sur registre
   writeregstr HKCU Software\OCS_PACKAGER\SETTINGS\FILELIST  File $R8
Functionend

Function enumreg
   ReadRegStr $2 HKCU Software\OCS_PACKAGER\SETTINGS\FILELIST  File
   !insertmacro MUI_INSTALLOPTIONS_WRITE "ListBox.ini" "Field 4" "ListItems" $2
   writeinistr "$exedir\${COL_FILE}" "collection" "Liste" $2
Functionend

Section "Empty"
   setautoclose true
   ReadRegStr $2 HKCU Software\OCS_PACKAGER\SETTINGS\FILELIST  File
   writeinistr "$exedir\${COL_FILE}" "collection" "Liste" $2
   ; open coll file and read collection
   Readinistr $2 "$exedir\${COL_FILE}" "collection" "Liste"
  ; sum of files in collection in $r0
   ${WordFind} $2 "|" "*" $R0
   intop $R0 $R0 + 1
  ; editing each files
   strcpy $1 "0"
loopfiles:
   intop $1 $1 + 1
   intcmp $R0 $1 0 endloopfiles
   ; retrieve current indexed_file
   ${WordFind} $2 "|" "+$1" $R1
   strcmp $R1 "" endloopfiles
   goto loopfiles
endloopfiles:

SectionEnd
