<?xml version='1.0' encoding='UTF-8'?>
<Project Type="Project" LVVersion="14008000">
	<Item Name="My Computer" Type="My Computer">
		<Property Name="NI.SortType" Type="Int">3</Property>
		<Property Name="server.app.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.control.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="server.tcp.enabled" Type="Bool">false</Property>
		<Property Name="server.tcp.port" Type="Int">0</Property>
		<Property Name="server.tcp.serviceName" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.tcp.serviceName.default" Type="Str">My Computer/VI Server</Property>
		<Property Name="server.vi.callsEnabled" Type="Bool">true</Property>
		<Property Name="server.vi.propertiesEnabled" Type="Bool">true</Property>
		<Property Name="specify.custom.address" Type="Bool">false</Property>
		<Item Name="Example" Type="Folder" URL="../Example">
			<Property Name="NI.DISK" Type="Bool">true</Property>
		</Item>
		<Item Name="CSharp" Type="Folder" URL="../CSharp">
			<Property Name="NI.DISK" Type="Bool">true</Property>
		</Item>
		<Item Name="Notes" Type="Folder"/>
		<Item Name="Helpers" Type="Folder">
			<Item Name="MongoDB.JSONtoQueryDoc.vi" Type="VI" URL="../MongoDB.Helper/MongoDB.JSONtoQueryDoc.vi">
				<Property Name="marked" Type="Int">0</Property>
			</Item>
			<Item Name="MongoDB.Parse.CommandResult.vi" Type="VI" URL="../MongoDB.Helper/MongoDB.Parse.CommandResult.vi">
				<Property Name="marked" Type="Int">0</Property>
			</Item>
			<Item Name="MongoDB.EnumerateIEnum.vi" Type="VI" URL="../MongoDB.Helper/MongoDB.EnumerateIEnum.vi"/>
			<Item Name="MongoDB.EnumerateCursor.vi" Type="VI" URL="../MongoDB.Helper/MongoDB.EnumerateCursor.vi">
				<Property Name="marked" Type="Int">0</Property>
			</Item>
			<Item Name="MongoDB.FormatObject_ID.vi" Type="VI" URL="../MongoDB.Helper/MongoDB.FormatObject_ID.vi"/>
		</Item>
		<Item Name="UnitTests" Type="Folder" URL="../UnitTests">
			<Property Name="NI.DISK" Type="Bool">true</Property>
		</Item>
		<Item Name="Controls" Type="Folder" URL="../Controls">
			<Property Name="NI.DISK" Type="Bool">true</Property>
		</Item>
		<Item Name="MongoDB.Client.lvclass" Type="LVClass" URL="../MongoDB.Client.lvclass"/>
		<Item Name="MongoDB.Database.lvclass" Type="LVClass" URL="../MongoDB.Database.lvclass"/>
		<Item Name="MongoDB.Collection.lvclass" Type="LVClass" URL="../MongoDB.Collection.lvclass"/>
		<Item Name="Dependencies" Type="Dependencies">
			<Item Name="vi.lib" Type="Folder">
				<Item Name="NI_AALBase.lvlib" Type="Library" URL="/&lt;vilib&gt;/Analysis/NI_AALBase.lvlib"/>
			</Item>
			<Item Name="mscorlib" Type="VI" URL="mscorlib">
				<Property Name="marked" Type="Int">0</Property>
				<Property Name="NI.PreserveRelativePath" Type="Bool">true</Property>
			</Item>
			<Item Name="lvanlys.dll" Type="Document" URL="/&lt;resource&gt;/lvanlys.dll"/>
		</Item>
		<Item Name="Build Specifications" Type="Build"/>
	</Item>
</Project>
