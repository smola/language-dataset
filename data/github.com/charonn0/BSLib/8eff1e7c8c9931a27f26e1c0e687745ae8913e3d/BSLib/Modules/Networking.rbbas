#tag Module
Protected Module Networking
	#tag Method, Flags = &h0
		Function CombineURL(BaseURL As String, relativeURL As String, ByRef outputURL As String) As Integer
		  //Safely combines a given base URL with a given relative URL.
		  
		  
		  Dim mb As New MemoryBlock((BaseURL.LenB + relativeURL.LenB) * 4)
		  Dim count As Integer = mb.Size
		  If InternetCombineUrl(BaseURL, relativeURL, mb, count, ICU_BROWSER_MODE) Then
		    outputURL = mb.WString(0)
		    Return 0
		  Else
		    Return GetLastError
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DNSLookup(domain As String) As String
		  //Performs a DNS query on the passed domain name. Returns a string containing the dotted IPv4 address
		  //of the server. On error, returns ""
		  //FIXME: very buggy
		  
		  #If TargetWin32 Then
		    Dim mb As Ptr
		    Dim x As Integer = DnsQuery(ConvertEncoding(domain, Encodings.ASCII), DNS_TYPE_A, 0, 0, mb, 0)
		    If x = 0 Then
		      Dim mb1 As MemoryBlock = mb
		      Return IPv4IntToDot(mb1.UInt32Value(24))  //Convert the integer to a dotted string
		    Else
		      Return ""
		    End If
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function EscapeURL(URL As String) As String
		  Dim mb As New MemoryBlock(URL.LenB * 4)
		  Dim count As Integer = mb.Size
		  
		  If InternetCanonicalizeUrl(URL, mb, count, ICU_BROWSER_MODE) Then
		    Return mb.WString(0)
		  Else
		    Return URL
		  End If
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function InternetAvailable() As Boolean
		  //Returns True if the computer appears to have Internet access.
		  //On Windows XP and earlier, we just check whether GoogleDNS is reachable
		  
		  #If TargetWin32 Then
		    If System.IsFunctionAvailable("IsInternetConnected", "Connect") Then
		      Return IsInternetConnected = 0
		    Else
		      Return IsDestinationReachable("8.8.8.8")  //Google's distributed DNS server.
		    End If
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function InternetConnect() As Boolean
		  //Attempts to connect to the internet
		  //Returns True if the connection succeeded or was already established.
		  
		  #If TargetWin32 Then
		    Return InternetAttemptConnect(0) = 0
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function InternetDial() As Boolean
		  //DIAL UP ONLY
		  //Attempts to dial the default dial-up internet connection, if one exists.
		  //Returns True if the connection succeeded or another connection was already established.
		  
		  #If TargetWin32 Then
		    Return InternetAutodial(0, -1)
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IPv4DotToInt(dottedIP As String) As UInt32
		  //Converts a standard dotted IPv4 address to an Unsigned 32 bit Integer
		  //Returns an unsigned 32 bit integer (UInt32) NOT a plain Integer. Putting the return value into a signed integer variable will
		  //cause weirdness
		  //This function should be cross-platform safe.
		  
		  If CountFields(dottedIP, ".") <> 4 Then Raise New UnsupportedFormatException
		  Dim mb As New MemoryBlock(4)
		  mb.LittleEndian = False
		  mb.Byte(0) = Val(NthField(dottedIP, ".", 1))
		  mb.Byte(1) = Val(NthField(dottedIP, ".", 2))
		  mb.Byte(2) = Val(NthField(dottedIP, ".", 3))
		  mb.Byte(3) = Val(NthField(dottedIP, ".", 4))
		  Return mb.UInt32Value(0)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IPv4IntToDot(s As UInt64) As String
		  //Returns a formatted IPv4 address like "192.168.0.1" See also: IPv4DotToInt
		  //This function should be cross-platform safe.
		  
		  Dim mb As New MemoryBlock(4)
		  mb.LittleEndian = False
		  mb.UInt32Value(0) = s
		  Return Format(mb.Byte(0), "##0") + "." + Format(mb.Byte(1), "##0") + "." + Format(mb.Byte(2), "##0") + "." + Format(mb.Byte(3), "##0")
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsDestinationReachable(destination As String) As Boolean
		  //This function attempts to reach the network location specified by destination. This can be an IP address, URL, or UNC path.
		  //Do NOT include the protocol prefix, e.g. use "www.google.com" rather than "http://www.google.com"
		  //Returns True if the destination was reachable, False if not.
		  //Support for the IsDestinationReachable API was dropped in Windows Vista, so we just ping instead. Even though it was
		  //dropped, System.IsFunctionAvailable("IsDestinationReachableW", "Sensapi") will still return True so we check the kernel version instead.
		  
		  #If TargetWin32 Then
		    If Platform.KernelVersion > 5.0 And Platform.KernelVersion < 6.0 Then
		      Dim info As QOCINFO
		      info.sSize = info.Size
		      Return IsDestinationReachable(destination, info)
		    Else
		      Return Ping(destination) > -1
		    End If
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function NetworkAvailable() As Boolean
		  //Returns True if at least one network connection is alive. This does not neccessarily mean that Internet access is available.
		  //On error, or if no connections are alive, returns False
		  
		  #If TargetWin32 Then
		    Dim flags As Integer
		    If IsNetWorkAlive(flags) Then
		      If GetLastError = 0 Then
		        Return True
		      Else
		        Return False
		      End If
		    Else
		      Return False
		    End If
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Ping(server As String, timeout As Integer = 5000) As Integer
		  //Sends a single ICMP ping to the specified server address. Server can be an IPv4 address or a domain name. timeout is in milliseconds.
		  //Returns the response time in milliseconds. If the destination is unreachable or any other error occurs, returns -1
		  //This function blocks execution while waiting for a reply.
		  //Based heavily on the Ping function from WFS.
		  
		  #If TargetWin32 Then
		    Dim mb As New MemoryBlock(256 + 128 + 8 + 4)
		    WSAStartup(&h0101, mb)
		    
		    If mb.Short(0) <> &h0101 Then
		      WSACleanup
		      Return -1
		    End
		    Dim address As Integer = inet_addr(server)
		    Dim addrMemBlock As MemoryBlock
		    
		    If address = -1 Then
		      addrMemBlock = gethostbyname(server)
		      address = addrMemBlock.Ptr(12).Ptr(0).Long(0)
		    end
		    
		    Dim icmpFile As Integer = IcmpCreateFile
		    Dim pingInfo As New MemoryBlock(8)
		    pingInfo.Byte(0) = 255
		    
		    Dim reply As New MemoryBlock(28)
		    Dim ret As Integer
		    ret = IcmpSendEcho(icmpFile, address, 0, 0,pingInfo, reply, reply.Size, timeout)
		    
		    WSACleanup()
		    IcmpCloseHandle(icmpFile)
		    
		    If ret <> 0 Then
		      return reply.Long(8)
		    else
		      return -1
		    end
		  #endif
		  
		Exception
		  Return -1
		End Function
	#tag EndMethod


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			InheritedFrom="Object"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
