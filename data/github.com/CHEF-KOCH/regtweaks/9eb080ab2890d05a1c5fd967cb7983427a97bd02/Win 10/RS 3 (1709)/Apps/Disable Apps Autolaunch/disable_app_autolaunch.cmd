@echo off
:: Disable Windows 10 Autolaunch Feature
:: Author: Hendrik Vermaak, 03 February 2018
:: Source: https://winaero.com/blog/disable-apps-autolaunch-windows-10/

:: Check for administrative permissions
>nul 2>&1 "%SYSTEMROOT%\system32\cacls.exe" "%SYSTEMROOT%\system32\config\system"
:: If error flag set, we do not have admin.
if '%errorlevel%' NEQ '0' (
echo Requesting administrative privileges...
goto UACPrompt
) else ( goto gotAdmin )
:UACPrompt
echo Set UAC = CreateObject^("Shell.Application"^) > "%temp%\getadmin.vbs"
echo UAC.ShellExecute "cmd.exe", "/C """"%~f0""""", , "runas", 1 >> "%temp%\getadmin.vbs"
cscript "%temp%\getadmin.vbs"
exit /B
:gotAdmin
if exist "%temp%\getadmin.vbs" ( del "%temp%\getadmin.vbs" )
pushd "%CD%"
CD /D "%~dp0"
:: BatchGotAdmin (Run as Admin code ends)

:: Disable AutoLaunch Feature
echo.
for /F "tokens=* skip=1" %%n in ('wmic useraccount where "name='%username%'" get sid ^| findstr "."') do (set SID=%%n)
reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon\UserARSO\%SID%" /v OptOut /t REG_DWORD /d 1 /f
echo.
echo Autolaunch feature disabled.
echo.
echo.
echo Please press any key to exit...
pause >nul