<#
	Found on http://boards.4chan.org/g/thread/49538199/windows-10-general-thread-30

	NAME
		Debloat-Windows10.ps1
	DESCRIPTION
		Debloats and customizes Windows 10 Enterprise N LTSB. Please contain this on /g/ and don't leak it outside.
        It changes your privacy options in the settings app and disables scheduled tasks and services that are there
        to gather information about you. It also tweaks the registry to customize settings, make your font display properly
        on DPI scaling 125% and disable OneDrive completely. Windows Features are also disabled, such as Internet Explorer
        and XPS Viewer, while others are enabled such as .NET framework 3.5. On top of it all, it appends new lines to your
        hosts file that block Microsoft from collecting data on you, as well as enables or disables local policies to 
        strengthen your privacy and security. This is a webm that illustrates the installation process:
        https://fuwa.se/m6b9oq.webm/debloat-windows.webm
	NOTES
		Website			| the world is burning, everyone all together collectively install gentoo
		Author			| Microsoft Engineer !JeCZI7VUg2
		Date			| 8/5/2015
		Version			| 1.4.0.0
	REQUIREMENTS
        ° Clean Windows installation
        ° Internet connection because I'm using 'PolicyFileEditor' module,
          if you need this to work offline, download the v2.0 of that module (google) and make sure it's installed
        ° You have to manually allow scripts to run (one time only, elevated powershell) so execute this command:
          Set-ExecutionPolicy RemoteSigned
        ° This is important! You have to wait for the OneDrive installation, after your first Windows login.
          It might take 5 minutes to pop up but you have to wait for it to install completely so we can nuke it properly.
          You'll know when it's done, because you'll have an icon in the bottom right tray bar.
	CHANGELOG
	8/5/2015, 1.4.0.0	| tested with installing KB3081424 after script+reboot; no issues, no reset of settings
                          settings: disabled sharing updates in local area network
                          customize: added the old windows 7-8.1 volume mixer
                          customize: disabling hibernation
                          fixed the issue where you'll see errors when running the script multiple times (it's ok now)

	8/5/2015, 1.3.0.0	| disabling new scheduled task: microsoft\windows\application experience\programdataupdater
                          added another customization which removes 'Network' in your explorer's left pane
                          added another customization which removes 'HomeGroup' in your explorer's left pane
                          added another customization which restores old windows update ui
                          added more entries to the hosts file to make Skype ad-free
                          doesn't download/prompt for PolicyFileEditor if you already have the module installed
                          onedrive doesn't hang up the script anymore if it has been previously removed
    KNOWN ISSUES
        ° If another process is accessing your hosts file or your OneDrive folders, you will see error messages.
          I've only tested this on a barebone Windows 10 Enterprise N LTSB installation, so please make sure you're
          not syncing fils and folders with OneDrive and you don't have some weird virus.
#>

cls
$ErrorActionPreference 	= "Continue"

# =========================================================================================== Variables and Objects
$settings = $true # Set to false to disable editing settings
$hosts = $true # Set to false to disable editing hosts file
$localpolicy = $true # Set to false to disable editing local policy
$registry = $true # Set to false to disable editing registry
$features = $false # Set to true to enable removing and enabling features
$services = $true # Set to false to disable removing services
$schdtasks = $true # Set to false to disable OOTB scheduled tasks
$customize = $false # Set to true to enable customization tweaks

# ================================================================================= Functions (non script specific)
# Takes Ownership of a registry sub key
# hive values = ClassesRoot, CurrentUser, LocalMachine
function TakeOwnership-RegKey($hive, $subkey)
{
$definition = @"
using System;
using System.Runtime.InteropServices; 

namespace Win32Api
{

	public class NtDll
	{
		[DllImport("ntdll.dll", EntryPoint="RtlAdjustPrivilege")]
		public static extern int RtlAdjustPrivilege(ulong Privilege, bool Enable, bool CurrentThread, ref bool Enabled);
	}
}
"@

    Add-Type -TypeDefinition $definition -PassThru
    
    $bEnabled = $false

    # Enable SeTakeOwnershipPrivilege
    $res = [Win32Api.NtDll]::RtlAdjustPrivilege(9, $true, $false, [ref]$bEnabled)

    # Taking ownership
    switch ($hive.ToString().tolower())
    {
        "classesroot" { $key = [Microsoft.Win32.Registry]::ClassesRoot.OpenSubKey($subkey, [Microsoft.Win32.RegistryKeyPermissionCheck]::ReadWriteSubTree,[System.Security.AccessControl.RegistryRights]::TakeOwnership) }
        "currentuser" { $key = [Microsoft.Win32.Registry]::CurrentUser.OpenSubKey($subkey, [Microsoft.Win32.RegistryKeyPermissionCheck]::ReadWriteSubTree,[System.Security.AccessControl.RegistryRights]::TakeOwnership) }
        "localmachine" { $key = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey($subkey, [Microsoft.Win32.RegistryKeyPermissionCheck]::ReadWriteSubTree,[System.Security.AccessControl.RegistryRights]::TakeOwnership) }
    }
    $acl = $key.GetAccessControl()
    $acl.SetOwner([System.Security.Principal.NTAccount]"Administrators")
    $key.SetAccessControl($acl)

    # Setting access to the key
    $acl = $key.GetAccessControl()
    $person = [System.Security.Principal.NTAccount]"Administrators"
    $access = [System.Security.AccessControl.RegistryRights]"FullControl"
    $inheritance = [System.Security.AccessControl.InheritanceFlags]"ContainerInherit"
    $propagation = [System.Security.AccessControl.PropagationFlags]"None"
    $type = [System.Security.AccessControl.AccessControlType]"Allow"

    $rule = New-Object System.Security.AccessControl.RegistryAccessRule($person,$access,$inheritance,$propagation,$type)
    $acl.SetAccessRule($rule)
    $key.SetAccessControl($acl)

    $key.Close()
}

# ===================================================================================== Functions (script specific)
# Disable scheduled tasks
function Disable-ScheduledTasks($isenable)
{
    if ($schdtasks -eq $true)
    {
        Write-Progress -Activity "Disabling scheduled tasks" -Status "Progress:" -PercentComplete 0
        
        schtasks /Change /TN "Microsoft\Windows\Application Experience\ProgramDataUpdater" /Disable | out-null
        schtasks /Change /TN "Microsoft\Windows\AppID\SmartScreenSpecific" /Disable | out-null
        schtasks /Change /TN "Microsoft\Windows\Application Experience\Microsoft Compatibility Appraiser" /Disable | out-null
        schtasks /Change /TN "Microsoft\Windows\Customer Experience Improvement Program\Consolidator" /Disable | out-null
        schtasks /Change /TN "Microsoft\Windows\Customer Experience Improvement Program\KernelCeipTask" /Disable | out-null
        schtasks /Change /TN "Microsoft\Windows\Customer Experience Improvement Program\UsbCeip" /Disable | out-null
        schtasks /Change /TN "Microsoft\Windows\DiskDiagnostic\Microsoft-Windows-DiskDiagnosticDataCollector" /Disable | out-null
        schtasks /Change /TN "Microsoft\Windows\NetTrace\GatherNetworkInfo" /Disable | out-null
        schtasks /Change /TN "Microsoft\Windows\Windows Error Reporting\QueueReporting" /Disable | out-null
        # Not sure about the following task, but the reg hack doesn't work either, so this is a pain in the fucking ass, maybe someone will figure it out, leaving it here:
        # schtasks /Change /TN "Microsoft\Windows\SettingSync\BackgroundUploadTask" /Disable | Out-Null
        # TakeOwnership-RegKey "LocalMachine" "SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Tasks" | Out-Null
        # New-Item -ErrorAction SilentlyContinue -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Tasks\{00524425-019B-4FDD-B1C5-04767424D01B}" -Force | Out-Null
        # New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Tasks\{00524425-019B-4FDD-B1C5-04767424D01B}" -Name "Triggers" -PropertyType Binary -Value ([byte[]](0x17,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x4a,0x85,0x00,0x42,0x48,0x48,0x48,0x48,0xd9,0x2b,0x30,0x29,0x48,0x48,0x48,0x48,0x0c,0x00,0x00,0x00,0x48,0x48,0x48,0x48,0x55,0x00,0x73,0x00,0x65,0x00,0x72,0x00,0x73,0x00,0x00,0x00,0x48,0x48,0x48,0x48,0x00,0x00,0x00,0x00,0x48,0x48,0x48,0x48,0x00,0x48,0x48,0x48,0x48,0x48,0x48,0x48,0x00,0x48,0x48,0x48,0x48,0x48,0x48,0x48,0x05,0x00,0x00,0x00,0x48,0x48,0x48,0x48,0x0c,0x00,0x00,0x00,0x48,0x48,0x48,0x48,0x01,0x01,0x00,0x00,0x00,0x00,0x00,0x05,0x04,0x00,0x00,0x00,0x48,0x48,0x48,0x48,0x00,0x00,0x00,0x00,0x48,0x48,0x48,0x48,0x58,0x00,0x00,0x00,0x48,0x48,0x48,0x48,0x00,0x00,0x00,0x00,0x30,0x2a,0x00,0x00,0x80,0xf4,0x03,0x00,0xff,0xff,0xff,0xff,0x07,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xa2,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00)) -Force | Out-Null
        
        Write-Progress -Activity "Disabling scheduled tasks" -Status "Progress:" -PercentComplete 4
    }
}
# Disable services
function Disable-Services($isenable)
{
    if ($isenable -eq $true)
    {
        Write-Progress -Activity "Disabling services" -Status "Progress:" -PercentComplete 4
        # Disable DiagTrack
        cmd /c sc config DiagTrack start= disabled | out-null
        cmd /c sc config dmwappushservice start= disabled | out-null
        cmd /c sc config diagnosticshub.standardcollector.service start= disabled | out-null
        cmd /c sc config TrkWks start= disabled | out-null
        cmd /c sc config WMPNetworkSvc start= disabled | out-null # Shouldn't exist but just making sure ...
        # Making sure the DiagTrack log is empty (tinfoil)
        Set-Content C:\ProgramData\Microsoft\Diagnosis\ETLLogs\AutoLogger\AutoLogger-Diagtrack-Listener.etl -Value "" -Force
        Write-Progress -Activity "Disabling services" -Status "Progress:" -PercentComplete 7
    }
}
# Tweak settings app
function Tweak-Settings($isenable)
{
    if ($isenable -eq $true)
    {
        Write-Progress -Activity "Backing up registry" -Status "Progress:" -PercentComplete 10 # Let's be save
        if (!(test-path -PathType Leaf C:\registry-backup-hklm.reg)) { reg export HKLM C:\registry-backup-hklm.reg | Out-Null }
        if (!(test-path -PathType Leaf C:\registry-backup-hkcu.reg)) { reg export HKCU C:\registry-backup-hkcu.reg | Out-Null }
        if (!(test-path -PathType Leaf C:\registry-backup-hkcr.reg)) { reg export HKCR C:\registry-backup-hkcr.reg | Out-Null }

        Write-Progress -Activity "Tweaking settings app" -Status "Progress:" -PercentComplete 12
        # Privacy -> General -> let websites provide locally relevant content by accessing my language list
        if ((Get-ItemProperty -Path "HKCU:SOFTWARE\Microsoft\Internet Explorer\International\" -Name AcceptLanguage -ErrorAction SilentlyContinue) -ne $null) { Remove-ItemProperty -Path "HKCU:SOFTWARE\Microsoft\Internet Explorer\International" -Name "AcceptLanguage" -Force }
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:Control Panel\International\User Profile" -Name HttpAcceptLanguageOptOut -Value 1 | Out-Null
        # Privacy -> General -> turn on smartscreen filter to check web content that windows store apps use
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\AppHost\" -Name EnableWebContentEvaluation -Value 0 -Force | Out-Null
        # Privacy -> Camera -> let apps use my camera
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:SOFTWARE\Microsoft\Windows\CurrentVersion\DeviceAccess\Global\{E5323777-F976-4f5b-9B55-B94699C46E44}" -Name Value -Value "Deny" | Out-Null
        # Privacy -> Microphone -> let apps use my microphone
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:SOFTWARE\Microsoft\Windows\CurrentVersion\DeviceAccess\Global\{2EEF81BE-33FA-4800-9670-1CD474972C3F}\" -Name Value -Value "Deny" | Out-Null
        # Privacy -> Account info -> let apps access my name, picture and other account info
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:SOFTWARE\Microsoft\Windows\CurrentVersion\DeviceAccess\Global\{C1D23ACC-752B-43E5-8448-8D0E519CD6D6}\" -Name Value -Value "Deny" | Out-Null
        # Privacy -> Calendar -> let apps access my calendar
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:SOFTWARE\Microsoft\Windows\CurrentVersion\DeviceAccess\Global\{D89823BA-7180-4B81-B50C-7E471E6121A3}\" -Name Value -Value "Deny" | Out-Null
        # Privacy -> Messaging -> let apps read or send sms and text messages
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:SOFTWARE\Microsoft\Windows\CurrentVersion\DeviceAccess\Global\{992AFA70-6F47-4148-B3E9-3003349C1548}\" -Name Value -Value "Deny" | Out-Null
        # Privacy -> Radio -> let apps control radios
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:SOFTWARE\Microsoft\Windows\CurrentVersion\DeviceAccess\Global\{A8804298-2D5F-42E3-9531-9C8C39EB29CE}\" -Name Value -Value "Deny" | Out-Null
        # Privacy -> Other devices -> sync with devices
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:SOFTWARE\Microsoft\Windows\CurrentVersion\DeviceAccess\Global\LooselyCoupled\" -Name Value -Value "Deny" | Out-Null
        # Privacy -> Feedback & Diagnostics -> feedback frequency
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:SOFTWARE\Microsoft\Siuf\Rules" -Force | Out-Null
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:SOFTWARE\Microsoft\Siuf\Rules" -Name NumberOfSIUFInPeriod -Value 0 -Force | Out-Null
        if ((Get-ItemProperty -Path "HKCU:SOFTWARE\Microsoft\Siuf\Rules" -Name PeriodInNanoSeconds -ErrorAction SilentlyContinue) -ne $null) { Remove-ItemProperty -Path "HKCU:SOFTWARE\Microsoft\Siuf\Rules" -Name PeriodInNanoSeconds }
        # Ease of Access -> Other options -> Visual options -> play animations
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:Control Panel\Desktop\WindowMetrics" -Name MinAnimate -Value 0 | Out-Null
        # Update & Security -> Windows Update -> Advanced -> Choose how updates are delviered -> Updates from more than one place (this is a GUI bug, registry is set properly even though it may show 'ON')
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization\Config" -Name "DownloadMode" -PropertyType DWORD -Value 0 | Out-Null
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization\Config" -Name "DODownloadMode" -Value 0 | Out-Null
        Set-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization\" -Name "SystemSettingsDownloadMode" -Value 0 | Out-Null
        
        Write-Progress -Activity "Tweaking settings app" -Status "Progress:" -PercentComplete 15
    }
}
# Append hosts file entries
function Edit-Hosts($isenable)
{
    if ($isenable -eq $true)
    {
        Write-Progress -Activity "Appending entries to hosts file" -Status "Progress:" -PercentComplete 15
        $file = "C:\Windows\System32\drivers\etc\hosts"
    
        "127.0.0.1 vortex.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 vortex-win.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telecommand.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telecommand.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 oca.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 oca.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sqm.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sqm.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 redir.metaservices.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 choice.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 choice.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 reports.wes.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 services.wes.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sqm.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.ppe.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.appex.bing.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.urs.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.appex.bing.net:443" | Out-File -encoding ASCII -append $file
        "127.0.0.1 vortex-sandbox.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 settings-sandbox.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 vortex.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 vortex-win.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telecommand.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telecommand.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 oca.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 oca.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sqm.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sqm.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 redir.metaservices.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 choice.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 choice.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 vortex-sandbox.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 settings-sandbox.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 reports.wes.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sqm.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.ppe.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 wes.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.appex.bing.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.urs.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 survey.watson.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.live.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 services.wes.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.appex.bing.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 vortex.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 vortex-win.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telecommand.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telecommand.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 oca.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 oca.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sqm.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sqm.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.telemetry.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 redir.metaservices.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 choice.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 choice.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 reports.wes.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 wes.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 services.wes.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sqm.df.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.ppe.telemetry.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.appex.bing.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.urs.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 telemetry.appex.bing.net:443" | Out-File -encoding ASCII -append $file
        "127.0.0.1 settings-sandbox.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 vortex-sandbox.data.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 survey.watson.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.live.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 watson.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 statsfe2.ws.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 corpext.msitadfs.glbdns2.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 compatexchange.cloudapp.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 cs1.wpc.v0cdn.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-0001.a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-0002.a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-0003.a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-0004.a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-0005.a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-0006.a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-0007.a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-0008.a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-0009.a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a-msedge.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 statsfe2.update.microsoft.com.akadns.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sls.update.microsoft.com.akadns.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 fe2.update.microsoft.com.akadns.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 diagnostics.support.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 corp.sts.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 statsfe1.ws.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 pre.footprintpredict.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 i1.services.social.microsoft.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 i1.services.social.microsoft.com.nsatc.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 feedback.windows.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 feedback.microsoft-hohm.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 feedback.search.microsoft.com" | Out-File -encoding ASCII -append $file

        # Skype ad-free
        "127.0.0.1 live.rads.msn.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 ads1.msn.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 static.2mdn.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 g.msn.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 a.ads2.msads.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 b.ads2.msads.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 ad.doubleclick.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 ac3.msn.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 rad.msn.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 msntest.serving-sys.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 bs.serving-sys.com1" | Out-File -encoding ASCII -append $file
        "127.0.0.1 flex.msn.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 ec.atdmt.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 cdn.atdmt.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 db3aqu.atdmt.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 cds26.ams9.msecn.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 sO.2mdn.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 aka-cdn-ns.adtech.de" | Out-File -encoding ASCII -append $file
        "127.0.0.1 secure.flashtalking.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 adnexus.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 adnxs.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 *.rad.msn.com" | Out-File -encoding ASCII -append $file
        "127.0.0.1 *.msads.net" | Out-File -encoding ASCII -append $file
        "127.0.0.1 *.msecn.net" | Out-File -encoding ASCII -append $file

        Write-Progress -Activity "Appending entries to hosts file" -Status "Progress:" -PercentComplete 30
    }
}
# Secure local group policy for privacy
# We'll need the PolicyFileEditor module for this
function Tweak-LocalPolicy($isenable)
{
    if ($isenable -eq $true)
    {
        Write-Progress -Activity "Securing local group policy for privacy (this might take a minute or two)" -Status "Progress:" -PercentComplete 30
    
        $command = get-command Set-PolicyFileEntry -ErrorAction SilentlyContinue
        if ($command -eq $null) # Can't use the Set command so the module likely isn't working
        {
            Write-Host "No PolicyFileEditor 2.0 found. Please accept the download for NuGet by pressing Y when the prompt appears in a moment:" -ForegroundColor Red
            if ((Get-Command Set-PolicyFileEntry -ErrorAction SilentlyContinue) -eq $null) # Don't have the module, download it
            {
                install-module PolicyFileEditor -Force -Confirm:$true
                Start-Sleep 5
                $command = get-command Set-PolicyFileEntry -ErrorAction SilentlyContinue
            }
        }
        if ($command -ne $null) # We're good, command found so we can continue
        {
            Write-Progress -Activity "Securing local group policy for privacy" -Status "Progress:" -PercentComplete 35
            # The reason I'm waiting 1s after each edit is to let the filesystem make necessary edits in the background, without the delay this will break local policies
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\DataCollection" -ValueName AllowTelemetry -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\Windows\Sidebar" -ValueName TurnOffSidebar -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Assistance\Client\1.0" -ValueName NoActiveHelp -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Biometrics" -ValueName Enabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Conferencing" -ValueName NoRDS -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\InputPersonalization" -ValueName AllowInputPersonalization -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Internet Explorer\Geolocation" -ValueName PolicyDisableGeolocation -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Internet Explorer\Infodelivery\Restrictions" -ValueName NoUpdateCheck -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Internet Explorer\Main" -ValueName DoNotTrack -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Internet Explorer\Privacy" -ValueName EnableInPrivateBrowsing -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Internet Explorer\SQM" -ValueName DisableCustomerImprovementProgram -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Messenger\Client" -ValueName CEIP -Type DWord -Data 2
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Messenger\Client" -ValueName PreventAutoRun -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\MicrosoftEdge\Main" -ValueName Cookies -Type DWord -Data 2
            Start-Sleep 1
            Write-Progress -Activity "Securing local group policy for privacy (this might take a minute or two)" -Status "Progress:" -PercentComplete 40
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\PCHealth\ErrorReporting" -ValueName DoReport -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\PCHealth\ErrorReporting" -ValueName ForceQueueMode -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\PCHealth\ErrorReporting\DW" -ValueName DWFileTreeRoot -Type String -Data ""
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\PCHealth\ErrorReporting\DW" -ValueName DWNoExternalURL -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\PCHealth\ErrorReporting\DW" -ValueName DWNoFileCollection -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\PCHealth\ErrorReporting\DW" -ValueName DWNoSecondLevelCollection -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\PCHealth\ErrorReporting\DW" -ValueName DWReporteeName -Type String -Data ""
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\SearchCompanion" -ValueName DisableContentFileUpdates -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\SQMClient\Windows" -ValueName CEIPEnable -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows Defender" -ValueName DisableAntiSpyware -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows Defender\Spynet" -ValueName **del.SpynetReporting -Type String -Data ""
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows Defender\Spynet" -ValueName SubmitSamplesConsent -Type DWord -Data 2
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows NT\CurrentVersion\NetworkList\Signatures\010103000F0000F0080000000F0000F0D0B4EB5D3C24F17D10AE531C7DCEF4A94F4A085AD0D4C88B75082573E36F857A" -ValueName Category -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows NT\CurrentVersion\NetworkList\Signatures\010103000F0000F0080000000F0000F0D0B4EB5D3C24F17D10AE531C7DCEF4A94F4A085AD0D4C88B75082573E36F857A" -ValueName CategoryReadOnly -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows NT\CurrentVersion\Software Protection Platform" -ValueName NoGenTicket -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows NT\IIS" -ValueName PreventIISInstall -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows NT\Printers" -ValueName PhysicalLocation -Type String -Data anonymous
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\AdvertisingInfo" -ValueName DisabledByGroupPolicy -Type DWord -Data 1
            Start-Sleep 1
            Write-Progress -Activity "Securing local group policy for privacy (this might take a minute or two)" -Status "Progress:" -PercentComplete 50
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\AppCompat" -ValueName AITEnable -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\AppCompat" -ValueName DisableInventory -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\AppCompat" -ValueName DisableUAR -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Device Metadata" -ValueName PreventDeviceMetadataFromNetwork -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\DeviceInstall\Settings" -ValueName DisableSendGenericDriverNotFoundToWER -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\DeviceInstall\Settings" -ValueName DisableSendRequestAdditionalSoftwareToWER -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Explorer" -ValueName NoUseStoreOpenWith -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\GameUX" -ValueName DownloadGameInfo -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\GameUX" -ValueName GameUpdateOptions -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\GameUX" -ValueName ListRecentlyPlayed -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Internet Connection Wizard" -ValueName ExitOnMSICW -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\LocationAndSensors" -ValueName DisableLocation -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\OneDrive" -ValueName DisableFileSyncNGSC -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\PowerShell" -ValueName EnableScripts -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\PowerShell" -ValueName ExecutionPolicy -Type String -Data "RemoteSigned"
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\PreviewBuilds" -ValueName **del.EnableExperimentation -Type String -Data ""
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\PreviewBuilds" -ValueName AllowBuildPreview -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\PreviewBuilds" -ValueName EnableConfigFlighting -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\System" -ValueName AsyncScriptDelay -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\System" -ValueName EnableLogonScriptDelay -Type DWord -Data 1
            Start-Sleep 1
            Write-Progress -Activity "Securing local group policy for privacy (this might take a minute or two)" -Status "Progress:" -PercentComplete 55
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{186f47ef-626c-4670-800a-4a30756babad}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{2698178D-FDAD-40AE-9D3C-1371703ADC5B}" -ValueName **del.EnabledScenarioExecutionLevel -Type String -Data ""
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{2698178D-FDAD-40AE-9D3C-1371703ADC5B}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{67144949-5132-4859-8036-a737b43825d8}" -ValueName **del.EnabledScenarioExecutionLevel -Type String -Data ""
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{67144949-5132-4859-8036-a737b43825d8}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{86432a0b-3c7d-4ddf-a89c-172faa90485d}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{9c5a40da-b965-4fc3-8781-88dd50a6299d}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{a7a5847a-7511-4e4e-90b1-45ad2a002f51}" -ValueName **del.EnabledScenarioExecutionLevel -Type String -Data ""
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{a7a5847a-7511-4e4e-90b1-45ad2a002f51}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{C295FBBA-FD47-46ac-8BEE-B1715EC634E5}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{dc42ff48-e40d-4a60-8675-e71f7e64aa9a}" -ValueName EnabledScenarioExecutionLevel -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{dc42ff48-e40d-4a60-8675-e71f7e64aa9a}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{ecfb03d1-58ee-4cc7-a1b5-9bc6febcb915}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{ffc42108-4920-4acf-a4fc-8abdcc68ada4}" -ValueName **del.EnabledScenarioExecutionLevel -Type String -Data ""
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WDI\{ffc42108-4920-4acf-a4fc-8abdcc68ada4}" -ValueName ScenarioExecutionEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Windows Error Reporting" -ValueName Disabled -Type DWord -Data 1
            Start-Sleep 1
            Write-Progress -Activity "Securing local group policy for privacy (this might take a minute or two)" -Status "Progress:" -PercentComplete 60
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Windows Error Reporting" -ValueName DontSendAdditionalData -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Windows Search" -ValueName AllowCortana -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Windows Search" -ValueName AllowSearchToUseLocation -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Windows Search" -ValueName ConnectedSearchPrivacy -Type DWord -Data 3
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Windows Search" -ValueName ConnectedSearchSafeSearch -Type DWord -Data 3
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Windows Search" -ValueName ConnectedSearchUseWeb -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Windows Search" -ValueName ConnectedSearchUseWebOverMeteredConnections -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\Windows Search" -ValueName DisableWebSearch -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate" -ValueName DeferUpgrade -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate" -ValueName DoNotConnectToWindowsUpdateInternetLocations -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" -ValueName **del.AutomaticMaintenanceEnabled -Type String -Data ""
            Start-Sleep 1
            Write-Progress -Activity "Securing local group policy for privacy (this might take a minute or two)" -Status "Progress:" -PercentComplete 65
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" -ValueName **del.DetectionFrequency -Type String -Data ""
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" -ValueName AUOptions -Type DWord -Data 2
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" -ValueName DetectionFrequencyEnabled -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" -ValueName EnableFeaturedSoftware -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" -ValueName NoAutoUpdate -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" -ValueName ScheduledInstallDay -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\Windows\WindowsUpdate\AU" -ValueName ScheduledInstallTime -Type DWord -Data 3
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\Machine\registry.pol -Key "SOFTWARE\Policies\Microsoft\WMDRM" -ValueName DisableOnline -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\User\registry.pol -Key "Software\Microsoft\Windows\CurrentVersion\Policies\Explorer" -ValueName NoInstrumentation -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\User\registry.pol -Key "Software\Policies\Microsoft\Internet Explorer\Privacy" -ValueName EnableInPrivateBrowsing -Type DWord -Data 0
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\User\registry.pol -Key "Software\Policies\Microsoft\Internet Explorer\Safety\PrivacIE" -ValueName DisableLogging -Type DWord -Data 1
            Start-Sleep 1
            Set-PolicyFileEntry -Path $env:systemroot\system32\GroupPolicy\User\registry.pol -Key "Software\Policies\Microsoft\Windows\EdgeUI" -ValueName DisableMFUTracking -Type DWord -Data 1
            gpupdate /force | Out-Null
        }
        else
        {
            Write-Warning "Local policies not configured, did not find the PolicyFileEditor module" 
        }
        Write-Progress -Activity "Securing local group policy for privacy" -Status "Progress:" -PercentComplete 70
    }
}
# Tweak registry
function Tweak-Registry($isenable)
{
    if ($isenable -eq $true)
    {
        Write-Progress -Activity "Tweaking registry" -Status "Progress:" -PercentComplete 70
        New-PSDrive -Name HKCR -PSProvider Registry -Root HKEY_CLASSES_ROOT | Out-Null

        # PhotoViewer fix so it appears in your Open With... menu and is enabled as your standard viewer
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.ico" -Force | Out-Null
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.tiff" -Force | Out-Null
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.bmp" -Force | Out-Null
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.png" -Force | Out-Null
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.gif" -Force | Out-Null
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.jpeg" -Force | Out-Null
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.jpg" -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.ico" -Name '(Default)' -Value "PhotoViewer.FileAssoc.Tiff" -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.tiff" -Name '(Default)' -Value "PhotoViewer.FileAssoc.Tiff" -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.bmp" -Name '(Default)' -Value "PhotoViewer.FileAssoc.Tiff" -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.png" -Name '(Default)' -Value "PhotoViewer.FileAssoc.Tiff" -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.gif" -Name '(Default)' -Value "PhotoViewer.FileAssoc.Tiff" -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.jpeg" -Name '(Default)' -Value "PhotoViewer.FileAssoc.Tiff" -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Classes\.jpg" -Name '(Default)' -Value "PhotoViewer.FileAssoc.Tiff" -Force | Out-Null

        # Fix DPI scaling blurry/fuzzy display at 125% (Might get reset by reboot/windows update)
        
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Control Panel\Desktop" -Name "DpiScalingVer" -Value "0x00001018" -PropertyType DWORD -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Control Panel\Desktop" -Name "Win8DpiScaling" -Value "0x00000001" -PropertyType DWORD -Force | Out-Null
        # This sets it to 125% DPI scaling, un-comment if you do need it (you use 125% dpi scaling)
        # New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Control Panel\Desktop" -Name "LogPixels" -Value "0x00000078" -PropertyType DWORD -Force | Out-Null

        # Add a 'Take Owner' option in your right-click menu (Powershell has problems with '*', using reg.exe)
        
        echo Y | reg add "HKEY_CLASSES_ROOT\*\shell\runas" /ve /t REG_SZ /d "Take Ownership" /f | Out-Null
        echo Y | reg add "HKEY_CLASSES_ROOT\*\shell\runas" /v NoWorkingDirectory /t REG_SZ /d "" /f | Out-Null
        echo Y | reg add "HKEY_CLASSES_ROOT\*\shell\runas\command" /ve /t REG_SZ /d "cmd.exe /c takeown /f \`"%1\`" && icacls \`"%1\`" /grant administrators:F" /f | Out-Null
        echo Y | reg add "HKEY_CLASSES_ROOT\*\shell\runas\command" /v IsolatedCommand /t REG_SZ /d "cmd.exe /c takeown /f \`"%1\`" && icacls \`"%1\`" /grant administrators:F" /f | Out-Null
   
        New-Item -ErrorAction SilentlyContinue -Force -Path "HKCR:\Directory\shell\runas" | Out-Null
        New-Item -ErrorAction SilentlyContinue -Force -Path "HKCR:\Directory\shell\runas\command" | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Force -Path "HKCR:\Directory\shell\runas" -Name '(Default)' -Value "Take Ownership" | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Force -Path "HKCR:\Directory\shell\runas" -Name NoWorkingDirectory -Value "" | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Force -Path "HKCR:\Directory\shell\runas\command" -Name '(Default)' -Value "cmd.exe /c takeown /f `"%1`" /r /d y && icacls `"%1`" /grant administrators:F /t" | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Force -Path "HKCR:\Directory\shell\runas\command" -Name IsolatedCommand -Value "cmd.exe /c takeown /f `"%1`" /r /d y && icacls `"%1`" /grant administrators:F /t" | Out-Null

        # Remove OneDrive completely
        # Let's find out if it's already removed first!
        $OneDriveEnabled = $false
        if ((Get-Process *OneDrive*) -ne $null) # Checking if the process exists
        {
            # Process exists, therefore you must have OneDrive installed, unless you messed up big time
            $OneDriveEnabled = $true
        }
        if ($OneDriveEnabled -eq $true)
        {
            $OneDrivex86 = "$env:SystemRoot\System32\OneDriveSetup.exe"
            $OneDrivex64 = "$env:SystemRoot\SysWOW64\OneDriveSetup.exe"

            Get-Process *OneDrive* | Stop-Process -Force | Out-Null
            Start-Sleep 3
        
            if (Test-Path $OneDrivex86)
            {
                & $OneDrivex86 "/uninstall" | Out-Null
                Start-Sleep 15 # Uninstallation needs time to let go off the files
            }

            if (Test-Path $OneDrivex64)
            {
                & $OneDrivex64 "/uninstall" | Out-Null
                Start-Sleep 20 # Uninstallation needs time to let go off the files
            }
    
            # Explorer.exe gets in our way by locking the files for some reason
        
            taskkill /F /IM explorer.exe | Out-Null

            if (Test-Path "$env:USERPROFILE\OneDrive") { rd "$env:USERPROFILE\OneDrive" -Recurse -Force | Out-Null }
            if (Test-Path "C:\OneDriveTemp") { rd "C:\OneDriveTemp" -Recurse -Force | Out-Null }
            if (Test-Path "$env:LOCALAPPDATA\Microsoft\OneDrive")
            {
                cmd.exe "/c takeown /f `"$env:LOCALAPPDATA\Microsoft\OneDrive`" /r /d y && icacls `"$env:LOCALAPPDATA\Microsoft\OneDrive`" /grant administrators:F /t" | Out-Null
                Start-Sleep 1
                rd "$env:LOCALAPPDATA\Microsoft\OneDrive" -Recurse -Force | Out-Null
            }
            if (Test-Path "$env:PROGRAMDATA\Microsoft OneDrive") { rd "$env:PROGRAMDATA\Microsoft OneDrive" -Recurse -Force | Out-Null }
        
            if (Test-Path "HKCR:\CLSID\{018D5C66-4533-4307-9B53-224DE2ED1FE6}")
            {
                TakeOwnership-RegKey "ClassesRoot" "CLSID\{018D5C66-4533-4307-9B53-224DE2ED1FE6}" | Out-Null
                Remove-Item -ErrorAction SilentlyContinue -Force -Path "HKCR:\CLSID\{018D5C66-4533-4307-9B53-224DE2ED1FE6}" -Recurse | Out-Null
            }
            if (Test-Path "HKCR:\Wow6432Node\CLSID\{018D5C66-4533-4307-9B53-224DE2ED1FE6}")
            {
                TakeOwnership-RegKey "ClassesRoot" "Wow6432Node\CLSID\{018D5C66-4533-4307-9B53-224DE2ED1FE6}" | Out-Null
                Remove-Item -ErrorAction SilentlyContinue -Force -Path "HKCR:\Wow6432Node\CLSID\{018D5C66-4533-4307-9B53-224DE2ED1FE6}" -Recurse | Out-Null
            }
            Start-Sleep 1
            Start-Process explorer.exe
        }

        Write-Progress -Activity "Tweaking registry" -Status "Progress:" -PercentComplete 90
    }
}
# Customization
function Customize-Windows($isenable)
{
    if ($isenable -eq $true)
    {
        New-PSDrive -Name HKCR -PSProvider Registry -Root HKEY_CLASSES_ROOT -ErrorAction SilentlyContinue | Out-Null

        Write-Progress -Activity "Tweaking registry for customization" -Status "Progress:" -PercentComplete 90

        # Allows Powershell Invoke-WebRequest to be usable again, without generating a Security Dialog (for developers)
        New-ItemProperty -ErrorAction SilentlyContinue -Force -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Internet Settings\Zones\3" -Name 1A10 -Value 0 | Out-Null

        # Use the Windows 7-8.1 Style Volume Mixer
        If (-Not (Test-Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\MTCUVC"))
        {
            New-Item -ErrorAction SilentlyContinue -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion" -Name MTCUVC | Out-Null
            New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\MTCUVC" -Name EnableMtcUvc -Type DWord -Value 0 | Out-Null
        }

        # Remove tablet lock screen (No need for in LTSB)
        # New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:SOFTWARE\Policies\Microsoft\Windows\Personalization" -Name NoLockScreen -Value 1 -PropertyType DWORD -Force | Out-Null

        # Remove Action Center from the right
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:\Software\Policies\Microsoft\Windows\Explorer" -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Policies\Microsoft\Windows\Explorer" -Name DisableNotificationCenter -PropertyType DWORD -Value 1 -Force | Out-Null
    
        # Disable Hibernation
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\Power" -Name "HiberbootEnabled" -PropertyType DWORD -Value 0 -Force | Out-Null

        # Removes 'Network' from left pane in explorer (requires ownership of the key)
        TakeOwnership-RegKey "ClassesRoot" "CLSID\{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}\ShellFolder" | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCR:\CLSID\{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}\ShellFolder" -Name Attributes -PropertyType DWORD -Value 0xb0940064 -Force | Out-Null

        # Disable New Windows Update UI and Enable Previous UI (requires ownership of the key)
        TakeOwnership-RegKey "LocalMachine" "Software\Microsoft\WindowsUpdate\UX" | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:\Software\Microsoft\WindowsUpdate\UX" -Name IsConvergedUpdateStackEnabled -PropertyType DWORD -Value 0 -Force | Out-Null

        # Set explorer to open to "This PC"
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name LaunchTo -PropertyType DWORD -Value 1 -Force | Out-Null

        # Hide 'Search' bar (needs reboot or explorer.exe restart)
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Search\" -Name SearchboxTaskbarMode -PropertyType DWORD -Value 0 -Force | Out-Null
    
        # Set UAC not to dim screen, but still display a warning (requires reboot)
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:Software\Microsoft\Windows\CurrentVersion\policies\system" -Name ConsentPromptBehaviorAdmin -PropertyType DWord -Value 5 -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:Software\Microsoft\Windows\CurrentVersion\policies\system" -Name EnableLUA -PropertyType DWord -Value 1 -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:Software\Microsoft\Windows\CurrentVersion\policies\system" -Name PromptOnSecureDesktop -PropertyType DWord -Value 0 -Force | Out-Null

        # This disables UAC, only use it if you're a l33t h4x0r
        # New-ItemProperty -ErrorAction SilentlyContinue -Path "HKLM:Software\Microsoft\Windows\CurrentVersion\policies\system" -Name EnableLUA -PropertyType DWord -Value 0 -Force | Out-Null

        # Show file extensions
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name HideFileExt -PropertyType DWORD -Value 0 -Force | Out-Null

        # Remove 'Customize this folder' from context menu
        New-Item -ErrorAction SilentlyContinue -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer" -Force | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer" -Name NoCustomizeThisFolder -Value 1 -PropertyType DWORD -Force | Out-Null

        # Remove 'Restore to previous versions' from context menu (might be superflous, just in case)
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\AllFilesystemObjects\shellex\ContextMenuHandlers\{596AB062-B4D2-4215-9F74-E9109B0A8153}" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\CLSID\{450D8FBA-AD25-11D0-98A8-0800361B1103}\shellex\ContextMenuHandlers\{596AB062-B4D2-4215-9F74-E9109B0A8153}" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\Directory\shellex\ContextMenuHandlers\{596AB062-B4D2-4215-9F74-E9109B0A8153}" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\Drive\shellex\ContextMenuHandlers\{596AB062-B4D2-4215-9F74-E9109B0A8153}" -Force -Recurse | Out-Null

        # Remove 'Share with' from context menu (First 9 might be superflous, just in case)
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\Directory\Background\shellex\ContextMenuHandlers\Sharing" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\Directory\shellex\ContextMenuHandlers\Sharing" -Force -Recurse | Out-Null
        reg delete "HKEY_CLASSES_ROOT\*\shellex\ContextMenuHandlers\Sharing" /f | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\Directory\shellex\CopyHookHandlers\Sharing" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\Directory\shellex\PropertySheetHandlers\Sharing" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\Drive\shellex\ContextMenuHandlers\Sharing" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\Drive\shellex\PropertySheetHandlers\Sharing" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\LibraryFolder\background\shellex\ContextMenuHandlers\Sharing" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\UserLibraryFolder\shellex\ContextMenuHandlers\Sharing" -Force -Recurse | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced" -Name SharingWizardOn -PropertyType DWORD -Value 0 -Force | Out-Null

        # Remove Homegroup from left explorer pane (requires ownership of the keys)
        TakeOwnership-RegKey "ClassesRoot" "CLSID\{B4FB3F98-C1EA-428d-A78A-D1F5659CBA93}\ShellFolder" | Out-Null
        TakeOwnership-RegKey "ClassesRoot" "Wow6432Node\CLSID\{B4FB3F98-C1EA-428d-A78A-D1F5659CBA93}\ShellFolder" | Out-Null
        New-ItemProperty -ErrorAction SilentlyContinue "HKCR:\CLSID\{B4FB3F98-C1EA-428d-A78A-D1F5659CBA93}\ShellFolder" -Name Attributes -PropertyType DWORD -Value 2962489612 -Force | Out-Null # hex: b094010c
        New-ItemProperty -ErrorAction SilentlyContinue "HKCR:\Wow6432Node\CLSID\{B4FB3F98-C1EA-428d-A78A-D1F5659CBA93}\ShellFolder" -Name Attributes -PropertyType DWORD -Value 2962489612 -Force | Out-Null # hex: b094010c

        # Remove 'Include in library' from context menu (might be superflous, just in case)
        Remove-Item -ErrorAction SilentlyContinue "HKCR:\Folder\ShellEx\ContextMenuHandlers\Library Location" -Force -Recurse | Out-Null
        Remove-Item -ErrorAction SilentlyContinue "HKLM:\SOFTWARE\Classes\Folder\ShellEx\ContextMenuHandlers\Library Location" -Force -Recurse | Out-Null

        # Remove 'Send to' from context menu (might be superflous, just in case)
        Remove-Item -ErrorAction SilentlyContinue -Path "HKCR:\AllFilesystemObjects\shellex\ContextMenuHandlers\SendTo" -Force -Recurse | Out-Null
    
        Write-Progress -Activity "Tweaking registry for customization" -Status "Progress:" -PercentComplete 95
    }
}
# Remove features
function Remove-Features($isenable)
{
    if ($isenable -eq $true)
    {
        Write-Progress -Activity "Removing features" -Status "Progress:" -PercentComplete 95
    
        # XPS Viewer
        Dism /online /Disable-Feature /FeatureName:Xps-Foundation-Xps-Viewer /quiet /norestart
        # XPS Services
        Dism /online /Disable-Feature /FeatureName:Printing-XPSServices-Features /quiet /norestart
        # Internet Explorer
        Dism /online /Disable-Feature /FeatureName:Internet-Explorer-Optional-amd64 /quiet /norestart
        # Work Folders
        Dism /online /Disable-Feature /FeatureName:WorkFolders-Client /quiet /norestart
        # Enabling .NET 3.5 framework because a lot of programs still use it
        Dism /online /Enable-Feature /FeatureName:NetFx3 /quiet /norestart

        Write-Progress -Activity "Removing features" -Status "Progress:" -PercentComplete 100
    }
}

# ======================================================================================================= Main Code
Write-Host "YOUR COMPUTER IS BEING HACKED. HOLD ON." -ForegroundColor Green

Disable-ScheduledTasks $schdtasks
Disable-Services $services
Tweak-Settings $settings
Edit-Hosts $hosts
Tweak-LocalPolicy $localpolicy
Tweak-Registry $registry
Customize-Windows $customize
Remove-Features $features

Write-Host "FINISHED. CHECK SYSTEM32 AND CHANGE YOUR SOCIAL SECURITY NUMBER." -ForegroundColor Green
Read-Host  "Debloat complete. Please restart your system to make sure everything works properly."