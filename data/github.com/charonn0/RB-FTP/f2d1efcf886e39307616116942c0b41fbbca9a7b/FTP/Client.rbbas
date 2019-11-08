#tag Class
Protected Class Client
Inherits FTP.Connection
	#tag Event
		Sub Connected()
		  FTPLog("Connected to " + Me.RemoteAddress + ":" + Str(Me.Port))
		  VerbDispatchTimer = New Timer
		  VerbDispatchTimer.Period = 100
		  AddHandler VerbDispatchTimer.Action, WeakAddressOf VerbDispatchHandler
		End Sub
	#tag EndEvent

	#tag Event
		Sub DataAvailable()
		  Dim i As Integer = InStrB(Me.Lookahead, CRLF)
		  Do Until i <= 0
		    Dim data As String = Me.Read(i + 1)
		    ParseResponse(data)
		    i = InStrB(Me.Lookahead, CRLF)
		  Loop
		  
		  
		End Sub
	#tag EndEvent

	#tag Event
		Sub Disconnected()
		  Me.Close()
		End Sub
	#tag EndEvent

	#tag Event
		Sub TransferComplete(UserAborted As Boolean)
		  #pragma Unused UserAborted
		  Me.CloseData
		  VerbDispatchTimer.Mode = Timer.ModeMultiple
		End Sub
	#tag EndEvent

	#tag Event
		Function TransferProgress(BytesSent As Integer, BytesLeft As Integer) As Boolean
		  #pragma Unused BytesSent
		  Return RaiseEvent TransferProgress(100 - (BytesLeft * 100 / DataBuffer.Length))
		End Function
	#tag EndEvent


	#tag Method, Flags = &h0
		Sub ABOR()
		  If TransferInProgress Then
		    Me.Write("ABOR" + CRLF)
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub APPE(RemoteFileName As String, LocalFile As FolderItem, Mode As Integer = 1, StartOffset As Integer = 0)
		  DataBuffer = BinaryStream.Open(LocalFile)
		  DataBuffer.Position = StartOffset
		  TYPE = Mode
		  If Me.Passive Then
		    PASV()
		  Else
		    PORT(Me.Port + 1)
		  End If
		  DoVerb("APPE", RemoteFileName)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CDUP()
		  DoVerb("CDUP")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CHMOD(RemoteFileName As String, NewPerms As String)
		  DoVerb("SITE", "CHMOD " + NewPerms.Trim + " " + RemoteFileName.Trim)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Close()
		  VerbDispatchTimer = Nil
		  mWorkingDirectory = ""
		  LastVerb.Verb = ""
		  LastVerb.Arguments = ""
		  Super.Close
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Connect(ClearPending As Boolean = True)
		  VerbDispatchTimer = Nil
		  If ClearPending Then ReDim PendingVerbs(-1)
		  If ClearPending Or PendingTransfers = Nil Then PendingTransfers = New Dictionary
		  Super.Connect()
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CWD(NewDirectory As String)
		  'Change the WorkingDirectory
		  DoVerb("CWD", NewDirectory)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DELE(RemoteFileName As String)
		  'Delete the file named RemoteFileName on the FTP server
		  DoVerb("DELE", RemoteFileName)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub DoVerb(Verb As String, Params As String = "", HighPriority As Boolean = False)
		  'Use this method to queue up verbs to be executed
		  Dim nextverb As FTPVerb
		  nextverb.Verb = Uppercase(Verb)
		  nextverb.Arguments = Trim(Params)
		  If Not HighPriority Then
		    PendingVerbs.Insert(0, nextverb)
		  Else
		    'Some verbs can't wait in line
		    PendingVerbs.Append(nextverb)
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FEAT()
		  DoVerb("FEAT")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub List(TargetDirectory As String = "")
		  'Retrieves a directory listing
		  TargetDirectory = TargetDirectory
		  If Me.Passive Then
		    PASV()
		  Else
		    PORT(Me.Port + 1)
		  End If
		  DoVerb("LIST", TargetDirectory)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MDTM(RemoteFileName As String)
		  DoVerb("MDTM", RemoteFileName)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub MKD(NewDirectoryName As String)
		  DoVerb("MKD", NewDirectoryName)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub NLST(TargetDirectory As String = "")
		  'Retrieves a directory listing
		  TargetDirectory = TargetDirectory
		  If Me.Passive Then
		    PASV()
		  Else
		    PORT(Me.Port + 1)
		  End If
		  DoVerb("NLST", TargetDirectory)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub NOOP()
		  DoVerb("NOOP")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub ParseResponse(Data As String)
		  Dim Code As Integer = Val(Left(Data, 3))
		  Dim msg As String = data.Replace(Format(Code, "000"), "")
		  If Code > 0 And msg <> "" Then
		    FTPLog(Str(Code) + " " + msg)
		  ElseIf Code > 0 Then
		    FTPLog(Str(Code) + " " + FTP.FormatCode(Code))
		  Else
		    FTPLog(msg)
		  End If
		  
		  Select Case LastVerb.Verb
		  Case "USER"
		    Select Case Code
		    Case 230  'Logged in W/O pass
		      LoginOK = True
		      RaiseEvent Connected()
		    Case 331, 332  'Need PASS/ACCT
		      DoVerb("PASS", Me.Password, True)
		    End Select
		    
		  Case "PASS"
		    Select Case Code
		    Case 230 'Logged in with pass
		      If Not LoginOK Then ' some servers send multi-line log-in messages, we only want the first one
		        LoginOK = True
		        RaiseEvent Connected()
		      End If
		    Case 530  'not logged in
		      'If LastVerb.Verb <> "USER" Then
		      'DoVerb("USER", Me.Username, True) 'Warning: some FTP servers (Microsoft IIS) send this code for ALL errors, resulting in an infinite loop.
		      'End If
		    End Select
		  Case "RETR"
		    Select Case Code
		    Case 150 'About to start data transfer
		      Dim size As String = NthField(msg, "(", 2)
		      size = NthField(size, ")", 1)
		    Case 425, 426 'Data connection not ready
		      Dim lv, la As String
		      lv = LastVerb.Verb
		      la = LastVerb.Arguments
		      If Passive Then
		        PASV()
		      Else
		        PORT(Me.Port + 1)
		      End If
		      DoVerb(lv, la)
		    Case 451, 551 'Disk read error
		      DataBuffer.Close
		      Call GetData()
		      TransferComplete(LastVerb.Arguments, False)
		    Case 226 'Done
		      Dim s As String = Me.GetData
		      DataBuffer.Write(s)
		      DataBuffer.Close
		      TransferComplete(LastVerb.Arguments, True)
		    End Select
		    
		  Case "STOR", "APPE"
		    Select Case Code
		    Case 150  'Ready
		      Me.TransmitData(DataBuffer.Read(DataBuffer.Length - DataBuffer.Position))
		      TransferInProgress = True
		    Case 226  'Success
		      TransferComplete(LastVerb.Arguments, True)
		      Me.CloseData
		      DataBuffer.Close
		    Case 425  'No data connection!
		      Dim lv, la As String
		      lv = LastVerb.Verb
		      la = LastVerb.Arguments
		      If Passive Then
		        PASV()
		      Else
		        PORT(Me.Port + 1)
		      End If
		      DoVerb(lv, la)
		    Case 426  'Data connection lost
		      DataBuffer.Close
		      Call GetData()
		      TransferComplete(LastVerb.Arguments, False)
		    End Select
		  Case "STAT"
		    If Code = 211 Or Code = 212 Or Code = 213 Then
		      Dim Stats() As String = Split(msg, EndOfLine.Windows)
		      Stats.Remove(Stats.Ubound)
		      Stats.Remove(0)
		      For Each Stat As String In Stats
		        Stat = Stat.Trim
		        FTPLog("   " + Stat)
		      Next
		    End If
		    
		  Case "FEAT"
		    Select Case Left(msg, 1)
		    Case "-"
		      Return
		    Case " "
		      ServerFeatures.Append(msg.Trim)
		    Else
		      Return
		    End Select
		    
		  Case "SYST"
		    ServerType = msg
		  Case "CWD"
		    Select Case Code
		    Case 250, 200 'OK
		      mWorkingDirectory = LastVerb.Arguments.Trim
		    End Select
		    
		  Case "PWD"
		    If Code = 257 Then 'OK
		      mWorkingDirectory = NthField(msg, " ", 2)
		      mWorkingDirectory = ReplaceAll(msg, """", "")
		    End If
		  Case "LIST", "NLST"
		    Select Case Code
		    Case 226, 150 'Here comes the directory list
		      Dim s() As String = Split(Me.GetData(), CRLF)
		      For i As Integer = UBound(s) DownTo 0
		        If s(i).Trim = "" Or s(i).Trim = "." Or s(i).Trim = ".." Then s.Remove(i)
		        
		      Next
		      ListResponse(s)
		    Case 425, 426  'no connection or connection lost
		    Case 451  'Disk error
		    End Select
		    DataBuffer = Nil
		  Case "CDUP"
		    If Code = 200 Or Code = 250 Then
		      DoVerb("PWD")
		    End If
		    
		  Case "PASV"
		    If Code = 227 Then 'Entering Passive Mode <h1,h2,h3,h4,p1,p2>.
		      Me.ConnectData(msg)
		    End If
		    
		  Case "REST"
		    If Code = 350 Then
		      DataBuffer.Position = Val(LastVerb.Arguments)
		    End If
		    
		  Case "PORT"
		    If Code = 200 Then
		      'Active mode OK. Connect to the following port
		      'Me.PASVAddress = msg
		    End If
		    
		  Case "SIZE"
		  Case "TYPE"
		    If Code = 200 Then
		      Select Case LastVerb.Arguments
		      Case "A"
		        Me.TransferMode = ASCIIMode
		      Case "L8"
		        Me.TransferMode = LocalMode
		      Case "I"
		        Me.TransferMode = BinaryMode
		      Case "E"
		        Me.TransferMode = EBCDICMode
		      End Select
		    End If
		    
		  Case "MKD"
		  Case "RMD"
		  Case "DELE"
		  Case "RNFR"
		    If Code = 350 Then
		      DoVerb("RNTO", RNT)
		    Else
		      RNT = ""
		      RNF = ""
		    End If
		    
		  Case "RNTO"
		    If Code = 250 Then
		      FTPLog(RNF + " renamed to " + RNT + " successfully.")
		    End If
		    RNT = ""
		    RNF = ""
		    
		  Case "QUIT"
		    Me.Close
		    
		  Else
		    If Code = 220 Then  'Server now ready
		      'The server is now ready to begin the login handshake
		      If Me.Anonymous Then
		        Me.Username = "anonymous"
		        Me.Password = "bsftp@boredomsoft.org"
		      End If
		      DoVerb("USER", Me.Username, True)
		    ElseIf Code = 421 Then  'Timeout
		      Me.Close
		    End If
		  End Select
		  If VerbDispatchTimer <> Nil Then VerbDispatchTimer.Mode = Timer.ModeMultiple
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PASV()
		  'You must call either PASV or PORT before transferring anything over the DataSocket
		  DoVerb("PASV")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PORT(PortNumber As Integer)
		  'You must call either PASV or PORT before transferring anything over the DataSocket
		  'Data port.
		  If Me.IsDataConnected Then Me.CloseData
		  Me.ListenData(PortNumber)
		  DoVerb("PORT", Me.PASVAddress)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub PWD()
		  DoVerb("PWD")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Quit()
		  DoVerb("QUIT")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Rename(OriginalName As String, NewName As String)
		  RNF = OriginalName
		  RNT = NewName
		  DoVerb("RNFR", RNF)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub REST(StartPosition As Integer = 0)
		  DoVerb("REST", Str(StartPosition))
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub RETR(RemoteFileName As String, SaveTo As FolderItem, Mode As Integer = 1)
		  TYPE = Mode
		  If Me.Passive Then
		    PASV()
		  Else
		    PORT(Me.Port + 1)
		  End If
		  If PendingTransfers = Nil Then PendingTransfers = New Dictionary
		  PendingTransfers.Value(RemoteFileName.Trim) = SaveTo
		  DoVerb("RETR", RemoteFileName.Trim)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub RMD(RemovedDirectoryName As String)
		  DoVerb("RMD", RemovedDirectoryName)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SIZE(RemoteFileName As String)
		  DoVerb("SIZE", RemoteFileName)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub STAT(RemoteFileName As String = "")
		  DoVerb("STAT", RemoteFileName)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub STOR(RemoteFileName As String, LocalFile As FolderItem, Mode As Integer = 1)
		  TYPE = Mode
		  If Me.Passive Then
		    PASV()
		  Else
		    PORT(Me.Port + 1)
		  End If
		  If PendingTransfers = Nil Then PendingTransfers = New Dictionary
		  PendingTransfers.Value(RemoteFileName.Trim) = LocalFile
		  DoVerb("STOR", RemoteFileName.Trim)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub SYST()
		  DoVerb("SYST")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub TYPE(Assigns TransferType As Integer)
		  Select Case TransferType
		  Case ASCIIMode
		    DoVerb("TYPE", "A")
		  Case LocalMode
		    DoVerb("TYPE", "L8")
		  Case BinaryMode
		    DoVerb("TYPE", "I")
		  Case EBCDICMode
		    DoVerb("TYPE", "E")
		  End Select
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Sub VerbDispatchHandler(Sender As Timer)
		  //Handles the FTPClientSocket.VerbDispatchTimer.Action event
		  If Not TransferInProgress And UBound(PendingVerbs) > -1 Then
		    Dim nextverb As FTPVerb = PendingVerbs.Pop
		    If nextverb.Verb = "STOR" Or nextverb.Verb = "APPE" Then
		      DataBuffer = BinaryStream.Open(PendingTransfers.Value(nextverb.Arguments))
		      PendingTransfers.Remove(nextverb.Arguments)
		    ElseIf nextverb.Verb = "RETR" Then
		      DataBuffer = BinaryStream.Create(PendingTransfers.Value(nextverb.Arguments), True)
		      PendingTransfers.Remove(nextverb.Arguments)
		    End If
		    Dim cmd As String = Trim(nextverb.Verb + " " + nextverb.Arguments)
		    FTPLog(cmd)
		    Me.Write(cmd + CRLF)
		    LastVerb = nextverb
		    Sender.Mode = Timer.ModeOff
		  End If
		  
		Exception Err As KeyNotFoundException
		  FTPLog("Local file could not be found!")
		  If DataBuffer <> Nil Then DataBuffer.Close
		  
		  
		End Sub
	#tag EndMethod


	#tag Hook, Flags = &h0
		Event Connected()
	#tag EndHook

	#tag Hook, Flags = &h0
		Event ListResponse(Listing() As String)
	#tag EndHook

	#tag Hook, Flags = &h0
		Event TransferComplete(RemoteFileName As String, Success As Boolean)
	#tag EndHook

	#tag Hook, Flags = &h0
		Event TransferProgress(PercentComplete As Single) As Boolean
	#tag EndHook


	#tag Note, Name = Copying
		Copyright ©2012 Andrew Lambert, All Rights Reserved.
		
		Licensed under the Creative Commons Attribution-Share Alike 3.0 Unported license.
	#tag EndNote

	#tag Note, Name = FTPClientSocket Notes
		This class subclasses FTPSocket and provides a client socket.
		
		When an FTP control connnection is established, the client waits for the server to
		initiate the FTP handshake. Once the handshake is completed, the Connected event is
		raised and commands may be sent to the server.
		
		Commands
	#tag EndNote


	#tag Property, Flags = &h1
		Protected DataBuffer As BinaryStream
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected LastVerb As FTPVerb
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mWorkingDirectory As String
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected PendingTransfers As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected PendingVerbs() As FTPVerb
	#tag EndProperty

	#tag Property, Flags = &h21
		Private RNF As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private RNT As String
	#tag EndProperty

	#tag Property, Flags = &h21
		Private VerbDispatchTimer As Timer
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  return mWorkingDirectory
			End Get
		#tag EndGetter
		WorkingDirectory As String
	#tag EndComputedProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Address"
			Visible=true
			Group="Behavior"
			Type="String"
			InheritedFrom="TCPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Anonymous"
			Visible=true
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			InheritedFrom="FTPSocket"
		#tag EndViewProperty
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
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Passive"
			Visible=true
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			InheritedFrom="FTPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Password"
			Visible=true
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
			InheritedFrom="FTPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Port"
			Visible=true
			Group="Behavior"
			InitialValue="21"
			Type="Integer"
			InheritedFrom="FTPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Username"
			Visible=true
			Group="Behavior"
			Type="String"
			EditorType="MultiLineEditor"
			InheritedFrom="FTPSocket"
		#tag EndViewProperty
		#tag ViewProperty
			Name="WorkingDirectory"
			Visible=true
			Group="Behavior"
			InitialValue="/"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
