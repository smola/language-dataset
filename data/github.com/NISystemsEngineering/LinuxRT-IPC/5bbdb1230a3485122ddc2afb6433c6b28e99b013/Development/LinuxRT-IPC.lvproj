<?xml version='1.0' encoding='UTF-8'?>
<Project Type="Project" LVVersion="16008000">
	<Item Name="Linux RT" Type="Target Folder">
		<Item Name="Linux RT CompactRIO" Type="RT CompactRIO">
			<Property Name="alias.name" Type="Str">Linux RT CompactRIO</Property>
			<Property Name="alias.value" Type="Str">0.0.0.0</Property>
			<Property Name="CCSymbols" Type="Str">TARGET_TYPE,RT;OS,Linux;CPU,ARM;DeviceCode,76D6;</Property>
			<Property Name="crio.ControllerPID" Type="Str">76D6</Property>
			<Property Name="crio.family" Type="Str">ARMLinux</Property>
			<Property Name="host.ResponsivenessCheckEnabled" Type="Bool">true</Property>
			<Property Name="host.ResponsivenessCheckPingDelay" Type="UInt">5000</Property>
			<Property Name="host.ResponsivenessCheckPingTimeout" Type="UInt">1000</Property>
			<Property Name="host.TargetCPUID" Type="UInt">8</Property>
			<Property Name="host.TargetOSID" Type="UInt">8</Property>
			<Property Name="target.cleanupVisa" Type="Bool">false</Property>
			<Property Name="target.FPProtocolGlobals_ControlTimeLimit" Type="Int">300</Property>
			<Property Name="target.getDefault-&gt;WebServer.Port" Type="Int">80</Property>
			<Property Name="target.getDefault-&gt;WebServer.Timeout" Type="Int">60</Property>
			<Property Name="target.IOScan.Faults" Type="Str"></Property>
			<Property Name="target.IOScan.NetVarPeriod" Type="UInt">100</Property>
			<Property Name="target.IOScan.NetWatchdogEnabled" Type="Bool">false</Property>
			<Property Name="target.IOScan.Period" Type="UInt">10000</Property>
			<Property Name="target.IOScan.PowerupMode" Type="UInt">0</Property>
			<Property Name="target.IOScan.Priority" Type="UInt">0</Property>
			<Property Name="target.IOScan.ReportModeConflict" Type="Bool">true</Property>
			<Property Name="target.IsRemotePanelSupported" Type="Bool">true</Property>
			<Property Name="target.RTCPULoadMonitoringEnabled" Type="Bool">true</Property>
			<Property Name="target.RTDebugWebServerHTTPPort" Type="Int">8001</Property>
			<Property Name="target.RTTarget.ApplicationPath" Type="Path">/c/ni-rt/startup/startup.rtexe</Property>
			<Property Name="target.RTTarget.EnableFileSharing" Type="Bool">true</Property>
			<Property Name="target.RTTarget.IPAccess" Type="Str">+*</Property>
			<Property Name="target.RTTarget.LaunchAppAtBoot" Type="Bool">false</Property>
			<Property Name="target.RTTarget.VIPath" Type="Path">/home/lvuser/natinst/bin</Property>
			<Property Name="target.server.app.propertiesEnabled" Type="Bool">true</Property>
			<Property Name="target.server.control.propertiesEnabled" Type="Bool">true</Property>
			<Property Name="target.server.tcp.access" Type="Str">+*</Property>
			<Property Name="target.server.tcp.enabled" Type="Bool">false</Property>
			<Property Name="target.server.tcp.paranoid" Type="Bool">true</Property>
			<Property Name="target.server.tcp.port" Type="Int">3363</Property>
			<Property Name="target.server.tcp.serviceName" Type="Str">Main Application Instance/VI Server</Property>
			<Property Name="target.server.tcp.serviceName.default" Type="Str">Main Application Instance/VI Server</Property>
			<Property Name="target.server.vi.access" Type="Str">+*</Property>
			<Property Name="target.server.vi.callsEnabled" Type="Bool">true</Property>
			<Property Name="target.server.vi.propertiesEnabled" Type="Bool">true</Property>
			<Property Name="target.WebServer.Config" Type="Str">Listen 8000

NI.ServerName default
DocumentRoot "$LVSERVER_DOCROOT"
TypesConfig "$LVSERVER_CONFIGROOT/mime.types"
DirectoryIndex index.htm
WorkerLimit 10
InactivityTimeout 60

LoadModulePath "$LVSERVER_MODULEPATHS"
LoadModule LVAuth lvauthmodule
LoadModule LVRFP lvrfpmodule

#
# Pipeline Definition
#

SetConnector netConnector

AddHandler LVAuth
AddHandler LVRFP

AddHandler fileHandler ""

AddOutputFilter chunkFilter


</Property>
			<Property Name="target.WebServer.Enabled" Type="Bool">false</Property>
			<Property Name="target.WebServer.LogEnabled" Type="Bool">false</Property>
			<Property Name="target.WebServer.LogPath" Type="Path">/c/ni-rt/system/www/www.log</Property>
			<Property Name="target.WebServer.Port" Type="Int">80</Property>
			<Property Name="target.WebServer.RootPath" Type="Path">/c/ni-rt/system/www</Property>
			<Property Name="target.WebServer.TcpAccess" Type="Str">c+*</Property>
			<Property Name="target.WebServer.Timeout" Type="Int">60</Property>
			<Property Name="target.WebServer.ViAccess" Type="Str">+*</Property>
			<Property Name="target.webservices.SecurityAPIKey" Type="Str">PqVr/ifkAQh+lVrdPIykXlFvg12GhhQFR8H9cUhphgg=:pTe9HRlQuMfJxAG6QCGq7UvoUpJzAzWGKy5SbZ+roSU=</Property>
			<Property Name="target.webservices.ValidTimestampWindow" Type="Int">15</Property>
			<Item Name="Dependencies" Type="Dependencies"/>
			<Item Name="Build Specifications" Type="Build"/>
		</Item>
		<Item Name="Linux RT myRIO" Type="RT myRIO">
			<Property Name="alias.name" Type="Str">Linux RT myRIO</Property>
			<Property Name="alias.value" Type="Str">0.0.0.0</Property>
			<Property Name="CCSymbols" Type="Str">TARGET_TYPE,RT;OS,Linux;CPU,ARM;DeviceCode,762F;</Property>
			<Property Name="crio.ControllerPID" Type="Str">762F</Property>
			<Property Name="host.ResponsivenessCheckEnabled" Type="Bool">true</Property>
			<Property Name="host.ResponsivenessCheckPingDelay" Type="UInt">5000</Property>
			<Property Name="host.ResponsivenessCheckPingTimeout" Type="UInt">1000</Property>
			<Property Name="host.TargetCPUID" Type="UInt">8</Property>
			<Property Name="host.TargetOSID" Type="UInt">8</Property>
			<Property Name="target.cleanupVisa" Type="Bool">false</Property>
			<Property Name="target.FPProtocolGlobals_ControlTimeLimit" Type="Int">300</Property>
			<Property Name="target.getDefault-&gt;WebServer.Port" Type="Int">80</Property>
			<Property Name="target.getDefault-&gt;WebServer.Timeout" Type="Int">60</Property>
			<Property Name="target.IOScan.Faults" Type="Str"></Property>
			<Property Name="target.IOScan.NetVarPeriod" Type="UInt">100</Property>
			<Property Name="target.IOScan.NetWatchdogEnabled" Type="Bool">false</Property>
			<Property Name="target.IOScan.Period" Type="UInt">10000</Property>
			<Property Name="target.IOScan.PowerupMode" Type="UInt">0</Property>
			<Property Name="target.IOScan.Priority" Type="UInt">0</Property>
			<Property Name="target.IOScan.ReportModeConflict" Type="Bool">true</Property>
			<Property Name="target.IsRemotePanelSupported" Type="Bool">true</Property>
			<Property Name="target.RTCPULoadMonitoringEnabled" Type="Bool">true</Property>
			<Property Name="target.RTDebugWebServerHTTPPort" Type="Int">8001</Property>
			<Property Name="target.RTTarget.ApplicationPath" Type="Path">/c/ni-rt/startup/startup.rtexe</Property>
			<Property Name="target.RTTarget.EnableFileSharing" Type="Bool">true</Property>
			<Property Name="target.RTTarget.IPAccess" Type="Str">+*</Property>
			<Property Name="target.RTTarget.LaunchAppAtBoot" Type="Bool">false</Property>
			<Property Name="target.RTTarget.VIPath" Type="Path">/home/lvuser/natinst/bin</Property>
			<Property Name="target.server.app.propertiesEnabled" Type="Bool">true</Property>
			<Property Name="target.server.control.propertiesEnabled" Type="Bool">true</Property>
			<Property Name="target.server.tcp.access" Type="Str">+*</Property>
			<Property Name="target.server.tcp.enabled" Type="Bool">false</Property>
			<Property Name="target.server.tcp.paranoid" Type="Bool">true</Property>
			<Property Name="target.server.tcp.port" Type="Int">3363</Property>
			<Property Name="target.server.tcp.serviceName" Type="Str">Main Application Instance/VI Server</Property>
			<Property Name="target.server.tcp.serviceName.default" Type="Str">Main Application Instance/VI Server</Property>
			<Property Name="target.server.vi.access" Type="Str">+*</Property>
			<Property Name="target.server.vi.callsEnabled" Type="Bool">true</Property>
			<Property Name="target.server.vi.propertiesEnabled" Type="Bool">true</Property>
			<Property Name="target.WebServer.Enabled" Type="Bool">false</Property>
			<Property Name="target.WebServer.LogEnabled" Type="Bool">false</Property>
			<Property Name="target.WebServer.LogPath" Type="Path">/c/ni-rt/system/www/www.log</Property>
			<Property Name="target.WebServer.Port" Type="Int">80</Property>
			<Property Name="target.WebServer.RootPath" Type="Path">/c/ni-rt/system/www</Property>
			<Property Name="target.WebServer.TcpAccess" Type="Str">c+*</Property>
			<Property Name="target.WebServer.Timeout" Type="Int">60</Property>
			<Property Name="target.WebServer.ViAccess" Type="Str">+*</Property>
			<Property Name="target.webservices.SecurityAPIKey" Type="Str">PqVr/ifkAQh+lVrdPIykXlFvg12GhhQFR8H9cUhphgg=:pTe9HRlQuMfJxAG6QCGq7UvoUpJzAzWGKy5SbZ+roSU=</Property>
			<Property Name="target.webservices.ValidTimestampWindow" Type="Int">15</Property>
			<Item Name="Chassis" Type="myRIO Chassis">
				<Property Name="crio.ProgrammingMode" Type="Str">fpga</Property>
				<Property Name="crio.ResourceID" Type="Str">RIO0</Property>
				<Property Name="crio.Type" Type="Str">myRIO-1900</Property>
			</Item>
			<Item Name="Dependencies" Type="Dependencies"/>
			<Item Name="Build Specifications" Type="Build"/>
		</Item>
		<Item Name="Linux RT Single-Board RIO" Type="RT Single-Board RIO">
			<Property Name="alias.name" Type="Str">Linux RT Single-Board RIO</Property>
			<Property Name="alias.value" Type="Str">0.0.0.0</Property>
			<Property Name="CCSymbols" Type="Str">TARGET_TYPE,RT;OS,Linux;CPU,ARM;DeviceCode,77D5;</Property>
			<Property Name="crio.ControllerPID" Type="Str">77D5</Property>
			<Property Name="host.ResponsivenessCheckEnabled" Type="Bool">true</Property>
			<Property Name="host.ResponsivenessCheckPingDelay" Type="UInt">5000</Property>
			<Property Name="host.ResponsivenessCheckPingTimeout" Type="UInt">1000</Property>
			<Property Name="host.TargetCPUID" Type="UInt">8</Property>
			<Property Name="host.TargetOSID" Type="UInt">8</Property>
			<Property Name="target.cleanupVisa" Type="Bool">false</Property>
			<Property Name="target.FPProtocolGlobals_ControlTimeLimit" Type="Int">300</Property>
			<Property Name="target.getDefault-&gt;WebServer.Port" Type="Int">80</Property>
			<Property Name="target.getDefault-&gt;WebServer.Timeout" Type="Int">60</Property>
			<Property Name="target.IOScan.Faults" Type="Str"></Property>
			<Property Name="target.IOScan.NetVarPeriod" Type="UInt">100</Property>
			<Property Name="target.IOScan.NetWatchdogEnabled" Type="Bool">false</Property>
			<Property Name="target.IOScan.Period" Type="UInt">10000</Property>
			<Property Name="target.IOScan.PowerupMode" Type="UInt">0</Property>
			<Property Name="target.IOScan.Priority" Type="UInt">0</Property>
			<Property Name="target.IOScan.ReportModeConflict" Type="Bool">true</Property>
			<Property Name="target.IsRemotePanelSupported" Type="Bool">true</Property>
			<Property Name="target.RTCPULoadMonitoringEnabled" Type="Bool">true</Property>
			<Property Name="target.RTDebugWebServerHTTPPort" Type="Int">8001</Property>
			<Property Name="target.RTTarget.ApplicationPath" Type="Path">/c/ni-rt/startup/startup.rtexe</Property>
			<Property Name="target.RTTarget.EnableFileSharing" Type="Bool">true</Property>
			<Property Name="target.RTTarget.IPAccess" Type="Str">+*</Property>
			<Property Name="target.RTTarget.LaunchAppAtBoot" Type="Bool">false</Property>
			<Property Name="target.RTTarget.VIPath" Type="Path">/home/lvuser/natinst/bin</Property>
			<Property Name="target.server.app.propertiesEnabled" Type="Bool">true</Property>
			<Property Name="target.server.control.propertiesEnabled" Type="Bool">true</Property>
			<Property Name="target.server.tcp.access" Type="Str">+*</Property>
			<Property Name="target.server.tcp.enabled" Type="Bool">false</Property>
			<Property Name="target.server.tcp.paranoid" Type="Bool">true</Property>
			<Property Name="target.server.tcp.port" Type="Int">3363</Property>
			<Property Name="target.server.tcp.serviceName" Type="Str">Main Application Instance/VI Server</Property>
			<Property Name="target.server.tcp.serviceName.default" Type="Str">Main Application Instance/VI Server</Property>
			<Property Name="target.server.vi.access" Type="Str">+*</Property>
			<Property Name="target.server.vi.callsEnabled" Type="Bool">true</Property>
			<Property Name="target.server.vi.propertiesEnabled" Type="Bool">true</Property>
			<Property Name="target.WebServer.Enabled" Type="Bool">false</Property>
			<Property Name="target.WebServer.LogEnabled" Type="Bool">false</Property>
			<Property Name="target.WebServer.LogPath" Type="Path">/c/ni-rt/system/www/www.log</Property>
			<Property Name="target.WebServer.Port" Type="Int">80</Property>
			<Property Name="target.WebServer.RootPath" Type="Path">/c/ni-rt/system/www</Property>
			<Property Name="target.WebServer.TcpAccess" Type="Str">c+*</Property>
			<Property Name="target.WebServer.Timeout" Type="Int">60</Property>
			<Property Name="target.WebServer.ViAccess" Type="Str">+*</Property>
			<Property Name="target.webservices.SecurityAPIKey" Type="Str">PqVr/ifkAQh+lVrdPIykXlFvg12GhhQFR8H9cUhphgg=:pTe9HRlQuMfJxAG6QCGq7UvoUpJzAzWGKy5SbZ+roSU=</Property>
			<Property Name="target.webservices.ValidTimestampWindow" Type="Int">15</Property>
			<Item Name="Chassis" Type="sbRIO Chassis">
				<Property Name="crio.ProgrammingMode" Type="Str">fpga</Property>
				<Property Name="crio.ResourceID" Type="Str">RIO0</Property>
				<Property Name="crio.Type" Type="Str">sbRIO-9627</Property>
			</Item>
			<Item Name="Dependencies" Type="Dependencies"/>
			<Item Name="Build Specifications" Type="Build"/>
		</Item>
	</Item>
	<Item Name="My Computer" Type="My Computer">
		<Property Name="server.app.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.control.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.tcp.enabled" Type="Bool">false</Property>
		<Property Name="server.tcp.port" Type="Int">0</Property>
		<Property Name="server.tcp.serviceName" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.tcp.serviceName.default" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.vi.callsEnabled" Type="Bool">true</Property>
		<Property Name="server.vi.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="specify.custom.address" Type="Bool">false</Property>
		<Item Name="Libraries" Type="Folder">
			<Item Name="Linux RT IPC.C.lvlib" Type="Library" URL="../../Source/C/Linux RT IPC.C.lvlib"/>
			<Item Name="Linux RT IPC.Pipes.lvlib" Type="Library" URL="../../Source/Pipes/Linux RT IPC.Pipes.lvlib"/>
			<Item Name="Linux RT IPC.POSIX Threads.lvlib" Type="Library" URL="../../Source/POSIX Threads/Linux RT IPC.POSIX Threads.lvlib"/>
			<Item Name="Linux RT IPC.Shared Memory.lvlib" Type="Library" URL="../../Source/Shared Memory/Linux RT IPC.Shared Memory.lvlib"/>
		</Item>
		<Item Name="Templates" Type="Folder">
			<Item Name="Shared Memory" Type="Folder">
				<Item Name="Basic Scalar Write-Read.vi" Type="VI" URL="../../Source/Shared Memory/Templates/Basic Scalar Write-Read.vi"/>
				<Item Name="Initialize Shared Memory.vi" Type="VI" URL="../../Source/Shared Memory/Templates/Initialize Shared Memory.vi"/>
			</Item>
		</Item>
		<Item Name="Test VIs" Type="Folder">
			<Item Name="C" Type="Folder">
				<Item Name="epoll test.vi" Type="VI" URL="../Testing/C/epoll test.vi"/>
				<Item Name="timerfd test.vi" Type="VI" URL="../Testing/C/timerfd test.vi"/>
			</Item>
			<Item Name="Pipes" Type="Folder">
				<Item Name="loopback cluster (in place).vi" Type="VI" URL="../Testing/Pipes/loopback cluster (in place).vi"/>
				<Item Name="loopback cluster.vi" Type="VI" URL="../Testing/Pipes/loopback cluster.vi"/>
				<Item Name="pipe writer.vi" Type="VI" URL="../Testing/Pipes/pipe writer.vi"/>
			</Item>
			<Item Name="Shared Memory" Type="Folder">
				<Item Name="Array Test.vi" Type="VI" URL="../Testing/Shared Memory/Array Test.vi"/>
			</Item>
		</Item>
		<Item Name="NI Linux RT IPC.vipb" Type="Document" URL="../NI Linux RT IPC.vipb"/>
		<Item Name="Dependencies" Type="Dependencies">
			<Item Name="vi.lib" Type="Folder">
				<Item Name="BuildHelpPath.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/BuildHelpPath.vi"/>
				<Item Name="Check Special Tags.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Check Special Tags.vi"/>
				<Item Name="Clear Errors.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Clear Errors.vi"/>
				<Item Name="Convert property node font to graphics font.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Convert property node font to graphics font.vi"/>
				<Item Name="Details Display Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Details Display Dialog.vi"/>
				<Item Name="DialogType.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/DialogType.ctl"/>
				<Item Name="DialogTypeEnum.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/DialogTypeEnum.ctl"/>
				<Item Name="Error Cluster From Error Code.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Error Cluster From Error Code.vi"/>
				<Item Name="Error Code Database.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Error Code Database.vi"/>
				<Item Name="ErrWarn.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/ErrWarn.ctl"/>
				<Item Name="eventvkey.ctl" Type="VI" URL="/&lt;vilib&gt;/event_ctls.llb/eventvkey.ctl"/>
				<Item Name="Find Tag.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Find Tag.vi"/>
				<Item Name="Format Message String.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Format Message String.vi"/>
				<Item Name="General Error Handler Core CORE.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/General Error Handler Core CORE.vi"/>
				<Item Name="General Error Handler.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/General Error Handler.vi"/>
				<Item Name="Get String Text Bounds.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Get String Text Bounds.vi"/>
				<Item Name="Get Text Rect.vi" Type="VI" URL="/&lt;vilib&gt;/picture/picture.llb/Get Text Rect.vi"/>
				<Item Name="GetHelpDir.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/GetHelpDir.vi"/>
				<Item Name="GetRTHostConnectedProp.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/GetRTHostConnectedProp.vi"/>
				<Item Name="Linux Errno.lvlib" Type="Library" URL="/&lt;vilib&gt;/NI/NI Linux RT Errno/Linux Errno.lvlib"/>
				<Item Name="Linux RT Utilities.lvlib" Type="Library" URL="/&lt;vilib&gt;/NI/NI Linux RT Utilities/Linux RT Utilities.lvlib"/>
				<Item Name="Longest Line Length in Pixels.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Longest Line Length in Pixels.vi"/>
				<Item Name="LVBoundsTypeDef.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/miscctls.llb/LVBoundsTypeDef.ctl"/>
				<Item Name="LVRectTypeDef.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/miscctls.llb/LVRectTypeDef.ctl"/>
				<Item Name="Not Found Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Not Found Dialog.vi"/>
				<Item Name="Search and Replace Pattern.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Search and Replace Pattern.vi"/>
				<Item Name="Set Bold Text.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Set Bold Text.vi"/>
				<Item Name="Set String Value.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Set String Value.vi"/>
				<Item Name="Simple Error Handler.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Simple Error Handler.vi"/>
				<Item Name="TagReturnType.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/TagReturnType.ctl"/>
				<Item Name="Three Button Dialog CORE.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Three Button Dialog CORE.vi"/>
				<Item Name="Three Button Dialog.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Three Button Dialog.vi"/>
				<Item Name="Trim Whitespace.vi" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/Trim Whitespace.vi"/>
				<Item Name="whitespace.ctl" Type="VI" URL="/&lt;vilib&gt;/Utility/error.llb/whitespace.ctl"/>
			</Item>
		</Item>
		<Item Name="Build Specifications" Type="Build"/>
	</Item>
</Project>
