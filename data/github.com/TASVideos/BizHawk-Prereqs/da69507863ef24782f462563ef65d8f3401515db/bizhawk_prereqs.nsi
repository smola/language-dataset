!include LogicLib.nsh
!include "MUI2.nsh"
!include "NsisDotNetChecker\DotNetChecker.nsh"

; Request application privileges for Windows Vista+
RequestExecutionLevel admin 

Function .onInit
UserInfo::GetAccountType
pop $0
${If} $0 != "admin" ;Require admin rights on NT4+
        MessageBox mb_iconstop "Administrator rights required!"
        SetErrorLevel 740 ;ERROR_ELEVATION_REQUIRED
        Quit
${EndIf}
FunctionEnd


; The name of the installer
Name "BizHawk Prerequisites"

; The file to write
OutFile "bizhawk_prereqs.exe"

; Request application privileges for Windows Vista+
RequestExecutionLevel admin 

LicenseText "The following prerequisites will be checked and installed:" "OK"
LicenseData "dist\info.txt"
Page license
Page instfiles

Section "KB2999226 (prerequisite for installing C++ 2015 runtime on win7-win8.1)" SEC_KB2999226
  SetOutPath "$TEMP"
  File "dist\UCRT\Windows6.1-KB2999226-x64.msu"
  File "dist\UCRT\Windows8.1-KB2999226-x64.msu"
  File "dist\UCRT\Windows8-RT-KB2999226-x64.msu"

  DetailPrint "Trying to install 3x KB2999226 for various platforms."
  ExecWait 'wusa.exe "dist\UCRT\Windows6.1-KB2999226-x64.msu" /quiet /norestart'
  ExecWait 'wusa.exe "dist\UCRT\Windows8.1-KB2999226-x64.msu" /quiet /norestart'
  ExecWait 'wusa.exe "dist\UCRT\Windows8-RT-KB2999226-x64.msu" /quiet /norestart'
  DetailPrint "Finished KB2999226"

SectionEnd

Section "Microsoft Visual C++ 2010 SP1 Runtime (x64)" SEC_CRT2010_SP1_X64

  SetOutPath "$TEMP"
  File "dist\vcredist_2010_sp1_x64.exe"
  DetailPrint "Running Visual C++ 2010 SP1 Runtime (x64) Setup..."
  ExecWait '"$TEMP\vcredist_2010_sp1_x64.exe" /q /promptrestart'
  DetailPrint "Finished Visual C++ 2010 SP1 Runtime (x64) Runtime Setup"
  
  Delete "$TEMP\vcredist_2010_sp1_x64.exe"

SectionEnd

Section "Microsoft Visual C++ 2012 Runtime Update 4 (x64)" SEC_CRT2012_X64

  SetOutPath "$TEMP"
  File "dist\vcredist_2012_u4_x64.exe"
  DetailPrint "Running Visual C++ 2012 Runtime Update 4 (x64) Setup..."
  ExecWait '"$TEMP\vcredist_2012_u4_x64.exe" /q /promptrestart'
  DetailPrint "Finished Visual C++ 2012 Runtime Update 4(x64) Runtime Setup"
  
  Delete "$TEMP\vcredist_2012_u4_x64.exe"

SectionEnd

Section "Microsoft Visual C++ 2013 Runtime (x64)" SEC_CRT2013_X64

  SetOutPath "$TEMP"
  File "dist\vcredist_2013_x64.exe"
  DetailPrint "Running Visual C++ 2013 Runtime (x64) Setup..."
  ExecWait '"$TEMP\vcredist_2013_x64.exe" /q /promptrestart'
  DetailPrint "Finished Visual C++ 2013 Runtime (x64) Runtime Setup"
  
  Delete "$TEMP\vcredist_2013_x64.exe"

SectionEnd

Section "Microsoft Visual C++ 2015 Runtime (x64)" SEC_CRT2015_X64

  SetOutPath "$TEMP"
  File "dist\vcredist_2015_x64.exe"
  DetailPrint "Running Visual C++ 2015 Runtime SP1 (x64) Setup..."
  ExecWait '"$TEMP\vcredist_2015_x64.exe" /quiet'
  DetailPrint "Finished Visual C++ 2015 Runtime SP1 (x64) Runtime Setup"
  
  Delete "$TEMP\vcredist_2015_x64.exe"

SectionEnd

Section "MS .NET Framework 4.6.1" SecFramework

	!insertmacro CheckNetFramework 461
 
SectionEnd

Section "DirectX Setup" SEC_DIRECTX
                                                                              
 ;SectionIn RO

 SetOutPath "$TEMP"
 File "dist\DirectX.exe"
 DetailPrint "Running DirectX Setup (just in case web setup isn't going to work)..."
 RMDir /r "$TEMP\bizphrack-dxsetup"                                                                             
 ExecWait '"$TEMP\DirectX.exe" /S'
 
 Delete "$TEMP\DirectX.exe"
 RMDir /r "$TEMP\bizphrack-dxsetup"

 DetailPrint "Finished DirectX Setup"                                     

SectionEnd

Section "DirectX Web Setup" SEC_DIRECTXWEB
                                                                              
 ;SectionIn RO

 SetOutPath "$TEMP"
 File "dist\dxwebsetup.exe"
 DetailPrint "Running DirectX Web Setup..."
 ExecWait '"$TEMP\dxwebsetup.exe" /Q'
 DetailPrint "Finished DirectX Web Setup"                                     
                                                                              
 Delete "$TEMP\dxwebsetup.exe"

SectionEnd