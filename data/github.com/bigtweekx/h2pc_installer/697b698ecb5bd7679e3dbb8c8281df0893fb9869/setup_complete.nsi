# Halo 2 Project Cartographer installer

#Includes code created using:
#NSIS Installation Script created by NSIS Quick Setup Script Generator v1.09.18
#Entirely Edited with NullSoft Scriptable Installation System                
#by Vlasis K. Barkas aka Red Wine red_wine@freemail.gr Sep 2006    

RequestExecutionLevel admin
      
!define APP_NAME "Halo 2 Project Cartographer"
!define COMP_NAME "H2PC"
!define WEB_SITE "www.halo2.online"
!define VERSION "01.8.00.00"
!define COPYRIGHT "H2PC"
!define DESCRIPTION "H2PC Installer"
!define INSTALLER_NAME "C:\Git\h2pc_installer\h2pc_setup.exe"
!define MAIN_APP_EXE "halo2.exe"
!define INSTALL_TYPE "SetShellVarContext current"
!define REG_ROOT "HKCU"
!define REG_APP_PATH "Software\Microsoft\Windows\CurrentVersion\App Paths\${MAIN_APP_EXE}"
!define UNINSTALL_PATH "Software\Microsoft\Windows\CurrentVersion\Uninstall\${APP_NAME}"

!define REG_START_MENU "Start Menu Folder"

var SM_Folder
Var DirectXSetupError
Var netSetupError
var vcredistSetupError
######################################################################

VIProductVersion  "${VERSION}"
VIAddVersionKey "ProductName"  "${APP_NAME}"
VIAddVersionKey "CompanyName"  "${COMP_NAME}"
VIAddVersionKey "LegalCopyright"  "${COPYRIGHT}"
VIAddVersionKey "FileDescription"  "${DESCRIPTION}"
VIAddVersionKey "FileVersion"  "${VERSION}"

######################################################################

SetCompressor LZMA
Name "${APP_NAME}"
Caption "${APP_NAME}"
OutFile "${INSTALLER_NAME}"
BrandingText "${APP_NAME}"
XPStyle on
InstallDirRegKey "${REG_ROOT}" "${REG_APP_PATH}" ""
InstallDir "C:\Games\Halo 2 Project Cartographer"

######################################################################

!include "MUI.nsh"
!include "DirectSetup.nsh"
!include "x64.nsh"
!include "LogicLib.nsh"
!include "psexec.nsh" 
!include "Sections.nsh"
!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_ABORTWARNING
!define MUI_UNABORTWARNING

!define MUI_ICON "doc_map.ico"

!insertmacro MUI_PAGE_WELCOME


!define MUI_PAGE_CUSTOMFUNCTION_SHOW LicenseShow
!insertmacro MUI_PAGE_LICENSE "termsofservice.rtf"
LicenseForceSelection checkbox
Function LicenseShow
 ScrollLicense::Set /NOUNLOAD
FunctionEnd

Function .onGUIEnd
 ScrollLicense::Unload
FunctionEnd

!insertmacro MUI_PAGE_DIRECTORY

!ifdef REG_START_MENU
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "Halo 2 Project Cartographer"
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "${REG_ROOT}"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "${UNINSTALL_PATH}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "${REG_START_MENU}"
!insertmacro MUI_PAGE_STARTMENU Application $SM_Folder
!endif

!insertmacro MUI_PAGE_INSTFILES

!define MUI_FINISHPAGE_LINK "Please visit our website to learn about Project Cartographer"
!define MUI_FINISHPAGE_LINK_LOCATION "https://halo2.online/forums/current-news.11/"


!define MUI_FINISHPAGE_RUN "$INSTDIR\halo2.exe"
!define MUI_FINISHPAGE_RUN_PARAMETERS "-windowed"  
!define MUI_FINISHPAGE_RUN_TEXT "Run Halo 2 to login and play"
;!define MUI_FINISHPAGE_SHOWREADME_TEXT 

!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM

!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_UNPAGE_FINISH

!insertmacro MUI_LANGUAGE "English"

ShowInstDetails show

######################################################################
Function Callback
Pop $R9
${If} $R9 = "358"
	DetailPrint "h2pc data file not found, please redownload or check your antivirus"
${EndIf}
FunctionEnd

Section -MainProgram
${INSTALL_TYPE}
SetOverwrite ifnewer
InitPluginsDir
${PowerShellExec} "Unblock-File -Path '$EXEDIR\h2pc_data.bin'" ;unblock the data file
Pop $R1 
${If} $R1 != ""
	DetailPrint $R1
${EndIf}

DetailPrint "Checking data file integrity... Might take a while if you have a slow computer"
CRCCheck::GenCRC "$EXEDIR\h2pc_data.bin"
Pop $R1
${If} $R1 != "3828064033"
	DetailPrint "$R1 "
	MessageBox MB_OKCANCEL "Install failed: data file is corrupted.$\nPlease redownload with a different browser or use the torrent link.$\nPress OK to open the download page. Press cancel to close installer" IDOK download
	Quit 
	download: 
		ExecShell "open" "http://www.h2maps.net/Cartographer/Installer/"
	Quit
${EndIf}
DetailPrint "Data file OK"

CreateDirectory $INSTDIR  ;Start extraction of game files
SetOutPath $INSTDIR
SetCompress off
DetailPrint "Extracting game data ..."
SetDetailsPrint listonly
SetCompress auto
SetDetailsPrint both

GetFunctionAddress $R9 Callback
Nsis7z::ExtractWithCallback "$EXEDIR\h2pc_data.bin" $R9

Pop $R9
DetailPrint $R9
;Abort
; ${If} $R9 = "3786240"
	; Goto continue
; ${Else}
	; Goto nextIf
; ${EndIf}

; nextIf:
; ${If} $R9 = "379825772"
	; Goto continue
; ${Else}
	; MessageBox MB_OK|MB_ICONSTOP "h2pc_data.7z file not present or blocked by Windows. Please right click h2pc_data.7z, go to properties, click unblock, apply, then restart this setup"
	; DetailPrint "h2pc data extract error"
	; RMDir /r "$INSTDIR\"
	; Abort
; ${EndIf}

; continue:
AddSize 4600000
;Abort
SectionEnd

Section "xlive.dll update"

inetc::get "https://cartographer.online/latest/xlive.dll" "$EXEDIR\xlive.dll" /end            
Pop $0
${If} $0 == "OK"
	DetailPrint "Download OK, installing $EXEDIR\xlive.dll to $INSTDIR"
	CopyFiles "$EXEDIR\xlive.dll" $INSTDIR
${Else}
	DetailPrint "xlive.dll download error, using built in dll"
${EndIf}
Delete "$EXEDIR\xlive.dll"
SectionEnd

;Sections for dependencies install
Section "DirectX Install" 
 
 SectionIn RO
 
 DetailPrint "Running DirectX Setup..."
 ExecWait '"$INSTDIR\temp\directx.exe" /q /T:"$INSTDIR\temp\Directx\"' 
 ExecWait '"$INSTDIR\Temp\Directx\DXSETUP.exe" /silent' $DirectXSetupError
 DetailPrint "Finished DirectX Setup"
 Delete "$INSTDIR\temp\directx.exe"
 RMDir /r "$INSTDIR\temp\Directx\"

SectionEnd

Section "dotNET 4.5 Install" 

DetailPrint "Running .NET 4.5 setup..."
ExecWait '"$INSTDIR\temp\dotnetfx452.exe" /passive /norestart' $netSetupError
${If} $netSetupError != "0"
	DetailPrint ".NET 4.5 not installed"
${EndIf}
DetailPrint "Finished .NET 4.5 setup. Return code: $netSetupError"
Delete "$INSTDIR\temp\dotnetfx452.exe"

SectionEnd

Section "Visual c++ 2013"
 
 DetailPrint "Running Visual C++ 2013 setup..."
 DetailPrint "Installer may appear stuck, please wait for setup to continue"
 ExecWait '"$INSTDIR\temp\vcredist_2013_x86.exe" /q /norestart' $R1
 DetailPrint "Finished visual C++ 2013 setup ... "
 ${If} $R1 = "0" 
	DetailPrint "$R1"
		
${ElseIf} $R1 = "3010"
	DetailPrint "$R1"
	
${Else}
	DetailPrint "Visual C++ Error $R1"
	MessageBox MB_OK|MB_ICONSTOP "Error with Visual C++ 2013 install. You may encounter issues when running the game.Please download and install manually, then run the game. Press OK to finish setup"
	
${EndIf}
 ;MessageBox MB_OK|MB_ICONSTOP "return code: $R1"
 Delete "$INSTDIR\temp\vcredist_2013_x86.exe"
 RMDir /r "$INSTDIR\temp"

SectionEnd

######################################################################

Section -Icons_Reg
SetOutPath "$INSTDIR"
WriteUninstaller "$INSTDIR\uninstall.exe"

!ifdef REG_START_MENU
!insertmacro MUI_STARTMENU_WRITE_BEGIN Application
CreateDirectory "$SMPROGRAMS\$SM_Folder"
CreateShortCut "$SMPROGRAMS\$SM_Folder\${APP_NAME} (Windowed).lnk" "$INSTDIR\halo2.exe" "-windowed"
CreateShortCut "$SMPROGRAMS\$SM_Folder\${APP_NAME} (No Vsync).lnk" "$INSTDIR\halo2.exe" "-novsync"
CreateShortCut "$SMPROGRAMS\$SM_Folder\${APP_NAME} (No Sound).lnk" "$INSTDIR\halo2.exe" "-nosound"
CreateShortCut "$SMPROGRAMS\$SM_Folder\${APP_NAME} (HiRes fix).lnk" "$INSTDIR\halo2.exe" "-hiresfix"
CreateShortCut "$SMPROGRAMS\$SM_Folder\${APP_NAME}.lnk" "$INSTDIR\halo2.exe"
CreateShortCut "$DESKTOP\${APP_NAME}.lnk" "$INSTDIR\halo2.exe" 
CreateShortCut "$SMPROGRAMS\$SM_Folder\Uninstall ${APP_NAME}.lnk" "$INSTDIR\uninstall.exe"

!ifdef WEB_SITE
WriteIniStr "$INSTDIR\${APP_NAME} website.url" "InternetShortcut" "URL" "${WEB_SITE}"
CreateShortCut "$SMPROGRAMS\$SM_Folder\${APP_NAME} Website.lnk" "$INSTDIR\${APP_NAME} website.url"
!endif
!insertmacro MUI_STARTMENU_WRITE_END
!endif

WriteRegStr ${REG_ROOT} "${REG_APP_PATH}" "" "$INSTDIR\${MAIN_APP_EXE}"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "DisplayName" "${APP_NAME}"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "UninstallString" "$INSTDIR\uninstall.exe"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "DisplayIcon" "$INSTDIR\${MAIN_APP_EXE}"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "DisplayVersion" "${VERSION}"
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "Publisher" "${COMP_NAME}"

!ifdef WEB_SITE
WriteRegStr ${REG_ROOT} "${UNINSTALL_PATH}"  "URLInfoAbout" "${WEB_SITE}"
!endif

; Writes Halo 2 registry keys for install directory. Different for x64 or x86 versions of windows
${If} ${RunningX64}
  SetRegView 64
  WriteRegStr HKLM "SOFTWARE\WOW6432Node\Microsoft\Microsoft Games\Halo 2\1.0" GameInstallDir "$INSTDIR\"
  DetailPrint "x64" 
${Else}
  SetRegView 32
  WriteRegStr HKLM "SOFTWARE\Microsoft\Microsoft Games\Halo 2\1.0\" GameInstallDir "$INSTDIR\"
  DetailPrint "x32" 
${EndIf}

WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\.Halo2" "" "Halo2Type"
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\.Halo2\PersistentHandler" "" "{C7691FD1-BA80-4E56-9E91-6934EB3C2C67}"
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\.Halo2\ShellEx" "" ""
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\.Halo2\ShellEx\{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}" "" "{4E5BFBF8-F59A-4e87-9805-1F9B42CC254A}"
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\.map" "" "Halo2MapType"
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\Halo2MapType" "" ""
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\Halo2MapType\DefaultIcon" "" "$INSTDIR\icons\doc_map.ico"
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\Halo2Type" "PreviewTitle" "prop:System.Game.RichSaveName;System.Game.RichApplicationName"
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\Halo2Type" "PreviewDetails" "prop:System.Game.RichLevel;System.DateChanged;System.Game.RichComment;System.DisplayName;System.DisplayType"
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\Halo2Type\DefaultIcon" "" "$INSTDIR\icons\doc_savegame.ico"
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\Halo2Type\Shell" "" ""
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\Halo2Type\Shell\Open" "" ""
WriteRegStr HKEY_LOCAL_MACHINE "Software\Classes\Halo2Type\Shell\Open\Command" "" "$INSTDIR\halo2.exe -save:\$\"%1\$\""

WriteRegStr HKEY_CURRENT_USER "SOFTWARE\Microsoft\Halo 2\Video Settings" DisplayMode "1"

;Delete temp directory inside install folder
RMDir /r "$INSTDIR\temp"
SectionEnd

;HKEY_LOCAL_MACHINE\SOFTWARE\WOW6432Node\Microsoft\Microsoft Games\Halo 2

######################################################################

Section Uninstall
${INSTALL_TYPE}
Delete "$INSTDIR\activate.exe"
Delete "$INSTDIR\Durazno.exe"
Delete "$INSTDIR\gungame.ini"
Delete "$INSTDIR\H2Launcher.exe"
Delete "$INSTDIR\h2startup1.ini"
Delete "$INSTDIR\halo2.exe"
Delete "$INSTDIR\halo2.exe.cat"
Delete "$INSTDIR\halo2.exe.cfg"
Delete "$INSTDIR\halo2final.ini"
Delete "$INSTDIR\ImeUiRes.dll"
Delete "$INSTDIR\launch.cab"
Delete "$INSTDIR\loading.bin"
Delete "$INSTDIR\MF.dll"
Delete "$INSTDIR\mspac.dll"
Delete "$INSTDIR\mspacres.dll"
Delete "$INSTDIR\mss32.dll"
Delete "$INSTDIR\mssdsp.flt"
Delete "$INSTDIR\PCCompat.dll"
Delete "$INSTDIR\Resource.dll"
Delete "$INSTDIR\sldlext.dll"
Delete "$INSTDIR\SLDL_DLL.dll"
Delete "$INSTDIR\startup.exe"
Delete "$INSTDIR\TnPCacheEngine.exe"
Delete "$INSTDIR\TnPUI.dll"
Delete "$INSTDIR\ts3client_win32.dll"
Delete "$INSTDIR\ts3server_win32.dll"
Delete "$INSTDIR\xinput9_1_0.dll"
Delete "$INSTDIR\xlive.dll"
Delete "$INSTDIR\xlive.ini"
Delete "$INSTDIR\xinput\p02\xinput9_1_0.dll"
Delete "$INSTDIR\sounds\infected.wav"
Delete "$INSTDIR\sounds\infection.wav"
Delete "$INSTDIR\sounds\last_man_standing.wav"
Delete "$INSTDIR\sounds\new_zombie.wav"
Delete "$INSTDIR\soundbackends\directsound_win32.dll"
Delete "$INSTDIR\soundbackends\windowsaudiosession_win32.dll"
Delete "$INSTDIR\privacy\Privacy.htm"
Delete "$INSTDIR\privacy\Privacy_da.htm"
Delete "$INSTDIR\privacy\Privacy_de.htm"
Delete "$INSTDIR\privacy\Privacy_en.htm"
Delete "$INSTDIR\privacy\Privacy_es.htm"
Delete "$INSTDIR\privacy\Privacy_fr.htm"
Delete "$INSTDIR\privacy\Privacy_it.htm"
Delete "$INSTDIR\privacy\Privacy_jp.htm"
Delete "$INSTDIR\privacy\Privacy_ko.htm"
Delete "$INSTDIR\privacy\Privacy_no.htm"
Delete "$INSTDIR\privacy\Privacy_sv.htm"
Delete "$INSTDIR\privacy\Privacy_zh-Hant.htm"
Delete "$INSTDIR\movie\credits_60.wmv"
Delete "$INSTDIR\movie\intro_60.wmv"
Delete "$INSTDIR\movie\intro_low_60.wmv"
Delete "$INSTDIR\maps\lockout.map"
Delete "$INSTDIR\maps\mainmenu.map"
Delete "$INSTDIR\maps\mainmenu.zip"
Delete "$INSTDIR\maps\shared.map"
Delete "$INSTDIR\maps\fonts\arial-13"
Delete "$INSTDIR\maps\fonts\arial-14"
Delete "$INSTDIR\maps\fonts\cht_dft_r5_conduit-12"
Delete "$INSTDIR\maps\fonts\cht_dft_r5_conduit-13"
Delete "$INSTDIR\maps\fonts\cht_dft_r5_conduit-9"
Delete "$INSTDIR\maps\fonts\cht_dft_r5_h_g-11"
Delete "$INSTDIR\maps\fonts\cht_dft_r5_h_g-13"
Delete "$INSTDIR\maps\fonts\cht_dft_r5_h_g-24"
Delete "$INSTDIR\maps\fonts\cht_dft_r5_mslcd-14"
Delete "$INSTDIR\maps\fonts\cht_jhenghei_conduit-9"
Delete "$INSTDIR\maps\fonts\conduit-12"
Delete "$INSTDIR\maps\fonts\conduit-13"
Delete "$INSTDIR\maps\fonts\conduit-9"
Delete "$INSTDIR\maps\fonts\fixedsys-9"
Delete "$INSTDIR\maps\fonts\font_table.txt"
Delete "$INSTDIR\maps\fonts\font_table_cht.txt"
Delete "$INSTDIR\maps\fonts\font_table_jpn.txt"
Delete "$INSTDIR\maps\fonts\font_table_kor.txt"
Delete "$INSTDIR\maps\fonts\handel_gothic-11"
Delete "$INSTDIR\maps\fonts\handel_gothic-13"
Delete "$INSTDIR\maps\fonts\handel_gothic-14"
Delete "$INSTDIR\maps\fonts\handel_gothic-24"
Delete "$INSTDIR\maps\fonts\jpn_dfghsmgoth_anti_con-12"
Delete "$INSTDIR\maps\fonts\jpn_dfghsmgoth_anti_con-13"
Delete "$INSTDIR\maps\fonts\jpn_dfghsmgoth_anti_con-9"
Delete "$INSTDIR\maps\fonts\jpn_dfghsmgoth_anti_h_g-11"
Delete "$INSTDIR\maps\fonts\jpn_dfghsmgoth_anti_h_g-13"
Delete "$INSTDIR\maps\fonts\jpn_dfghsmgoth_anti_h_g-24"
Delete "$INSTDIR\maps\fonts\jpn_dfghsmgoth_anti_mslcd-14"
Delete "$INSTDIR\maps\fonts\kor_malgun_16_conduit-9"
Delete "$INSTDIR\maps\fonts\kor_sdgd_m_anti_conduit-12"
Delete "$INSTDIR\maps\fonts\kor_sdgd_m_anti_conduit-13"
Delete "$INSTDIR\maps\fonts\kor_sdgd_m_anti_conduit-9"
Delete "$INSTDIR\maps\fonts\kor_sdgd_m_anti_h_g-11"
Delete "$INSTDIR\maps\fonts\kor_sdgd_m_anti_h_g-13"
Delete "$INSTDIR\maps\fonts\kor_sdgd_m_anti_h_g-24"
Delete "$INSTDIR\maps\fonts\kor_sdgd_m_anti_mslcd-14"
Delete "$INSTDIR\maps\fonts\mslcd-14"
Delete "$INSTDIR\icons\doc_map.ico"
Delete "$INSTDIR\icons\doc_savegame.ico"


Delete "$INSTDIR\sounds\AchievementUnlocked.wav"
Delete "$INSTDIR\sounds\h2pc_logo.png"

Delete "$INSTDIR\sounds\grunt_birthday_party.wav"
Delete "$INSTDIR\sounds\Halo1PCHitSound.wav"
Delete "$INSTDIR\sounds\headhunter.wav"
Delete "$INSTDIR\sounds\skull_scored.wav"
Delete "$INSTDIR\sounds\smb3_powerup.wav"
Delete "$INSTDIR\sounds\infected.wav"

Delete "$INSTDIR\soundbackends\directsound_win32.dll"
Delete "$INSTDIR\soundbackends\windowsaudiosession_win32.dll"
 
RmDir "$INSTDIR\icons"
RmDir "$INSTDIR\maps\fonts"
RmDir /r "$INSTDIR\maps"
RmDir "$INSTDIR\movie"
RmDir "$INSTDIR\privacy"
RmDir "$INSTDIR\soundbackends"
RmDir "$INSTDIR\sounds"
RmDir "$INSTDIR\xinput\p02"
RmDir "$INSTDIR\xinput"
RmDir "$INSTDIR\Temp"
 
Delete "$INSTDIR\uninstall.exe"
!ifdef WEB_SITE
Delete "$INSTDIR\${APP_NAME} website.url"
!endif

RmDir /r "$INSTDIR"

!ifdef REG_START_MENU
!insertmacro MUI_STARTMENU_GETFOLDER "Application" $SM_Folder
Delete "$SMPROGRAMS\$SM_Folder\${APP_NAME}.lnk"
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME} (Windowed).lnk" 
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME} (No Vsync).lnk" 
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME} (No Sound).lnk"
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME} (HiRes fix).lnk" 
Delete "$SMPROGRAMS\$SM_Folder\Uninstall ${APP_NAME}.lnk"
!ifdef WEB_SITE
Delete "$SMPROGRAMS\$SM_Folder\${APP_NAME} Website.lnk"
!endif
Delete "$DESKTOP\${APP_NAME}.lnk"

RmDir "$SMPROGRAMS\$SM_Folder"
!endif

!ifndef REG_START_MENU
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME}.lnk"
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME} (Windowed).lnk" 
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME} (No Vsync).lnk" 
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME} (No Sound).lnk"
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME} (HiRes fix).lnk" 
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\Uninstall ${APP_NAME}.lnk"
!ifdef WEB_SITE
Delete "$SMPROGRAMS\Halo 2 Project Cartographer\${APP_NAME} Website.lnk"
!endif
Delete "$DESKTOP\${APP_NAME}.lnk"

RmDir "$SMPROGRAMS\Halo 2 Project Cartographer"
!endif

DeleteRegKey ${REG_ROOT} "${REG_APP_PATH}"
DeleteRegKey ${REG_ROOT} "${UNINSTALL_PATH}"

${If} ${RunningX64}
  SetRegView 64
  DeleteRegKey HKLM "SOFTWARE\WOW6432Node\Microsoft\Microsoft Games\Halo 2\1.0"
  DetailPrint "x64" 
${Else}
  SetRegView 32
  DeleteRegKey HKLM "SOFTWARE\Microsoft\Microsoft Games\Halo 2\1.0\"
  DetailPrint "x32" 
${EndIf}
SectionEnd

######################################################################

