#tag Module
Protected Module Globals
	#tag Method, Flags = &h1
		Protected Sub AddProfileDialog(Config As Configuration, Username As String, Keys() As String, Values() As String, Product As UInt32)
		  
		  Dim w As New ProfileWindow()
		  
		  w.Config = Config
		  w.IsRemoving = False
		  w.Keys = Keys
		  
		  w.Title = Username + "'s Profile - BNRBot"
		  w.fldUsername.Text = Username
		  w.fldGame.Text = Globals.ProductName(Product)
		  
		  Dim Game As String = MemClass.WriteDWORD(Product, False)
		  
		  Dim FieldDate As Date
		  
		  Dim i As Integer = 0
		  Try
		    While i <= UBound(Values)
		      Select Case Keys(i)
		      Case "profile\age"
		        w.fldAge.Text = Values(i)
		        
		      Case "profile\sex"
		        w.fldSex.Text = Values(i)
		        
		      Case "profile\location"
		        w.fldLocation.Text = Values(i)
		        
		      Case "profile\description"
		        w.fldDescription.Text = Values(i)
		        
		      Case "record\" + Game + "\0\wins"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord0Wins.Text = Values(i)
		        
		      Case "record\" + Game + "\0\losses"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord0Losses.Text = Values(i)
		        
		      Case "record\" + Game + "\0\disconnects"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord0Disconnects.Text = Values(i)
		        
		      Case "record\" + Game + "\0\last game result"
		        If LenB(Values(i)) < 1 Then Values(i) = "n/a"
		        w.fldRecord0LastGameResult.Text = Values(i)
		        
		      Case "record\" + Game + "\0\last game"
		        If LenB(Values(i)) < 1 Then
		          Values(i) = "n/a"
		        ElseIf CountFields(Values(i), " ") = 2 And _
		          IsNumeric(NthField(Values(i), " ", 1)) = True And _
		          IsNumeric(NthField(Values(i), " ", 2)) = True Then
		          FieldDate = Globals.WindowsFileTimeToDate(Val(NthField(Values(i), " ", 2)), Val(NthField(Values(i), " ", 1)))
		          If FieldDate <> Nil Then Values(i) = FieldDate.ShortDate + " " + FieldDate.LongTime
		        End If
		        w.fldRecord0LastGame.Text = Values(i)
		        
		      Case "record\" + Game + "\1\wins"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord1Wins.Text = Values(i)
		        
		      Case "record\" + Game + "\1\losses"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord1Losses.Text = Values(i)
		        
		      Case "record\" + Game + "\1\disconnects"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord1Disconnects.Text = Values(i)
		        
		      Case "record\" + Game + "\1\last game result"
		        If LenB(Values(i)) < 1 Then Values(i) = "n/a"
		        w.fldRecord1LastGameResult.Text = Values(i)
		        
		      Case "record\" + Game + "\1\last game"
		        If LenB(Values(i)) < 1 Then
		          Values(i) = "n/a"
		        ElseIf CountFields(Values(i), " ") = 2 And _
		          IsNumeric(NthField(Values(i), " ", 1)) = True And _
		          IsNumeric(NthField(Values(i), " ", 2)) = True Then
		          FieldDate = Globals.WindowsFileTimeToDate(Val(NthField(Values(i), " ", 2)), Val(NthField(Values(i), " ", 1)))
		          If FieldDate <> Nil Then Values(i) = FieldDate.ShortDate + " " + FieldDate.LongTime
		        End If
		        w.fldRecord1LastGame.Text = Values(i)
		        
		      Case "record\" + Game + "\1\rating"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord1Rating.Text = Values(i)
		        
		      Case "record\" + Game + "\1\high rating"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord1HighRating.Text = Values(i)
		        
		      Case "DynKey\" + Game + "\1\rank"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord1Rank.Text = Values(i)
		        
		      Case "record\" + Game + "\1\high rank"
		        If LenB(Values(i)) < 1 Then Values(i) = "0"
		        w.fldRecord1HighRank.Text = Values(i)
		        
		      End Select
		      i = i + 1
		    Wend
		  Catch OOBE As OutOfBoundsException
		  End Try
		  
		  Dim bUpdate As Boolean = False
		  If Config.BNET <> Nil Then
		    If Config.BNET.Product = Packets.BNETProduct_D2DV _
		      Or Config.BNET.Product = Packets.BNETProduct_D2XP Then
		      bUpdate = (Config.BNET.AccountName = Mid(Username, Len(NthField(Username, "*", 1)) + 2))
		    Else
		      bUpdate = (Config.BNET.AccountName = Username)
		    End If
		  End If
		  
		  w.btnUpdate.Visible = bUpdate
		  w.btnUpdate.Default = w.btnUpdate.Visible
		  w.btnClose.Default = Not w.btnUpdate.Default
		  w.fldAge.ReadOnly = Not w.btnUpdate.Visible
		  w.fldSex.ReadOnly = w.fldAge.ReadOnly
		  w.fldLocation.ReadOnly = w.fldAge.ReadOnly
		  w.fldDescription.ReadOnly = w.fldAge.ReadOnly
		  
		  If w.btnUpdate.Visible = False Then w.btnClose.SetFocus() Else w.btnUpdate.SetFocus()
		  
		  w.ShowWithin(MainWindow)
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub AssignSocketDetails(Sock As SocketCore, Details As String, DefaultPort As Integer)
		  
		  If Sock = Nil Then Return
		  Dim SockT As TCPSocket
		  Dim SockU As UDPSocket
		  
		  If Sock IsA TCPSocket Then SockT = TCPSocket(Sock)
		  If Sock IsA UDPSocket Then SockU = UDPSocket(Sock)
		  
		  Dim sAddress As String
		  Dim iPort As Integer
		  
		  If InStr(Details, ":") > 0 Then
		    sAddress = NthField(Details, ":", 1)
		    iPort = Val(NthField(Details, ":", 2))
		  Else
		    sAddress = Details
		    iPort = DefaultPort
		  End If
		  
		  If Sock <> Nil Then Sock.Port = iPort
		  
		  If SockT <> Nil Then
		    SockT.Address = sAddress
		    SockT.Port = iPort
		  End If
		  
		  If SockU <> Nil Then SockU.Port = iPort
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Avg(ParamArray Values As Integer) As Integer
		  
		  // Average the Values together and return the result.
		  
		  Dim ret As Integer, i As Integer
		  
		  ret = 0
		  If UBound(Values) < 0 Then Return ret
		  
		  For i = 0 To UBound(Values)
		    ret = ret + Values(i)
		  Next
		  
		  Return ret / (UBound(Values) + 1)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function BoolS(Value As Boolean, retTrue As String = "True", retFalse As String = "False") As String
		  
		  Select Case Value
		  Case True
		    Return retTrue
		  Case False
		    Return retFalse
		  End Select
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function BSHA1(Data As String) As String
		  
		  // "Broken" SHA-1 Hashing
		  // U.S. Standard Hashing Algorithm Version 1
		  
		  Soft Declare Sub hashPassword Lib App.BNCSUtil (Password As Ptr, outBuffer As Ptr)
		  
		  Dim kData As New MemoryBlock(LenB(Data) + 1)
		  Dim Hash As New MemoryBlock(20)
		  
		  kData.StringValue(0, LenB(Data)) = Data
		  hashPassword(kData, Hash)
		  
		  Return Hash.StringValue(0, Hash.Size)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function CheckRevision(Product As UInt32, ValueString As String, ByRef EXEVersion As UInt32, ByRef Checksum As UInt32, ByRef EXEInfo As String, DLLName As String) As UInt32
		  
		  Soft Declare Function CheckRevisionEx Lib "CheckRevision.dll" (GameFile1 As CString, _
		  GameFile2 As CString, GameFile3 As CString, ValueString As CString,     version As Ptr, Checksum As Ptr, _
		  EXEinfo As Ptr, PathToDLL As CString, sUnused As CString, PathToVideoBin As CString) As Integer
		  
		  Soft Declare Function getExeInfo Lib App.BNCSUtil (Filename As Ptr, EXEInfo As Ptr, _
		  EXEInfoSize As Integer, Version As Ptr, Platform As Integer) As Integer
		  
		  //Soft Declare Function CheckRevisionEx Lib "Files/CheckRevision.dll" (ByVal GameFile1 As String, _
		  //ByVal GameFile2 As String, ByVal GameFile3 As String, ByVal ValueString As String, _
		  //ByRef version As Long, ByRef Checksum As Long, ByVal EXEinfo As String, _
		  //ByVal PathToDLL As String, ByVal sUnused As String, ByVal PathToVideoBin As String) As Long
		  //CRevResult = CheckRevisionEx(Starcraft.exe, Storm.Dll, Battle.Snp, ChecksumFormula, _
		  //EXEVersion, ChecksumResult, EXEinfo,LockdownFile.dll, vbNullString, Star.Bin)
		  
		  //Soft Declare Function checkrevision_ld Lib "libbnet.dll" (sFile1 As String, _
		  //sFile2 As String, sFile3 As String, sValueString As String, ByRef lVersion As Long, ByRef lCheck
		  //As String, ByVal sLockdownFile As String, ByVal sVideoFile As String) As Long
		  
		  Dim GameFile1 As New MemoryBlock(1)
		  Dim GameFile2 As New MemoryBlock(1)
		  Dim GameFile3 As New MemoryBlock(1)
		  Dim sValueString As MemoryBlock = Globals.StringToMemory(ValueString)
		  Dim dwEXEVersion As New MemoryBlock(4) // for the result
		  Dim dwChecksum As New MemoryBlock(4) // for the result
		  Dim sEXEInfo As New MemoryBlock(256)
		  Dim sDLLPath As String, DLLFile As MemoryBlock
		  Dim sUnused As New MemoryBlock(1)
		  Dim PathToVideoBin As New MemoryBlock(1)
		  
		  Dim Files() As String = Settings.GetProductFiles( Product, DLLName, sDLLPath )
		  If UBound(Files) >= 0 Then GameFile1 = Globals.StringToMemory(Files(0))
		  If UBound(Files) >= 1 Then GameFile2 = Globals.StringToMemory(Files(1))
		  If UBound(Files) >= 2 Then GameFile3 = Globals.StringToMemory(Files(2))
		  If UBound(Files) >= 3 Then PathToVideoBin = Globals.StringToMemory(Files(3))
		  
		  DLLFile = Globals.StringToMemory(sDLLPath) // buffer the path to the DLL
		  
		  Dim CRevResult As UInt32 = CheckRevisionEx(GameFile1, GameFile2, GameFile3, sValueString, _
		  dwEXEVersion, dwChecksum, sEXEInfo, DLLFile, sUnused, PathToVideoBin)
		  
		  EXEVersion = dwEXEVersion.UInt32Value(0)
		  Checksum = dwChecksum.UInt32Value(0)
		  EXEInfo = sEXEInfo.CString(0)
		  
		  Return CRevResult
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ChrSearch(Source As String, Pattern As String, Invert As Boolean = False) As Boolean
		  
		  Dim i As Integer = 1
		  While i <= Len(Source)
		    
		    If Invert = False Then
		      // Source must contain nothing outside of pattern.
		      If InStr(Pattern, Mid(Source, i, 1)) <= 0 Then Return False
		    Else
		      // Source must contain nothing inside of pattern.
		      If InStr(Pattern, Mid(Source, i, 1)) >= 1 Then Return False
		    End If
		    
		    i = i + 1
		  Wend
		  
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ChrSearchB(Source As String, Pattern As String, Invert As Boolean) As Boolean
		  
		  Dim i As Integer = 1
		  While i <= LenB(Source)
		    
		    If Invert = False Then
		      // Source must contain nothing outside of pattern.
		      If InStrB(Pattern, MidB(Source, i, 1)) <= 0 Then Return False
		    Else
		      // Source must contain nothing inside of pattern.
		      If InStrB(Pattern, MidB(Source, i, 1)) >= 1 Then Return False
		    End If
		    
		    i = i + 1
		  Wend
		  
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ClanRankName(Rank As Byte) As String
		  
		  Select Case Rank
		  Case &H00
		    Return "Initiate"
		  Case &H01
		    Return "Peon"
		  Case &H02
		    Return "Grunt"
		  Case &H03
		    Return "Shaman"
		  Case &H04
		    Return "Chieftain"
		  Case Else
		    Return "Unknown (" + Str(Rank) + ")"
		  End Select
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ColorMix(aMix As Color, bMix As Color, favor As Integer = 0) As Color
		  
		  // favor:
		  //   -2 = favor aMix x2
		  //   -1 = favor aMix x1
		  //    0 = favor both
		  //   +1 = favor bMix x1
		  //   +2 = favor bMix x2
		  
		  Dim ret As Color
		  Dim f1, f2, f3 As Integer
		  Dim a, b, c, d, e, f As Integer
		  
		  If favor < -1 Then
		    f1 = 5
		    f2 = 4
		    f3 = 1
		  ElseIf favor = -1 Then
		    f1 = 3
		    f2 = 2
		    f3 = 1
		  ElseIf favor = 1 Then
		    f1 = 3
		    f2 = 1
		    f3 = 2
		  ElseIf favor > 1 Then
		    f1 = 5
		    f2 = 1
		    f3 = 4
		  Else // favor < -2 Or favor == 0 Or favor > 2
		    f1 = 2
		    f2 = 1
		    f3 = 1
		  End If
		  
		  a = (aMix.Red / f1 * f2)
		  b = (bMix.Red / f1 * f3)
		  c = (aMix.Green / f1 * f2)
		  d = (bMix.Green / f1 * f3)
		  e = (aMix.Blue / f1 * f2)
		  f = (bMix.Blue / f1 * f3)
		  
		  ret = RGB(a + b, c + d, e + f)
		  
		  Return ret
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function ColorToHex(c As Color) As String
		  
		  Return Right("0" + Hex(c.Red), 2) + Right("0" + Hex(c.Green), 2) + Right("0" + Hex(c.Blue), 2)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ConfirmClanAction(Config As Configuration, Action As Byte)
		  
		  If Config = Nil Then Return
		  If Config.BNET = Nil Then Return
		  If Config.BNET.IsConnected = False Or Config.BNET.ClanTag = 0 Then Return
		  
		  Dim w As New ClanConfirmationWindow()
		  Dim ret As Boolean
		  
		  Select Case Action
		  Case Globals.ClanConfirmLeave
		    w.Title = ReplaceAll(w.Title, "%ACTION%", "Leave")
		    w.txtHeading.Text = ReplaceAll(w.txtHeading.Text, "%ACTION%", "leave")
		    
		  Case Globals.ClanConfirmDisband
		    w.Title = ReplaceAll(w.Title, "%ACTION%", "Disband")
		    w.txtHeading.Text = ReplaceAll(w.txtHeading.Text, "%ACTION%", "disband")
		    
		  End Select
		  
		  w.txtHeading.Text = ReplaceAll(w.txtHeading.Text, "%TAG%", Globals.SClanTag(Config.BNET.ClanTag))
		  w.ShowModal()
		  
		  If w = Nil Then Return
		  ret = (w.Result = w.btnOkay)
		  
		  If ret = True Then
		    Select Case Action
		    Case Globals.ClanConfirmLeave
		      
		      Dim Cookie As New Cookie(Cookie.TypeClanMemberRemove)
		      Cookie.Value("Username") = Config.BNET.AccountName
		      Config.BNET.Send(Packets.CreateSID_CLANREMOVEMEMBER(Cookie.Cookie, Config.BNET.AccountName))
		      
		    Case Globals.ClanConfirmDisband
		      
		      Dim Cookie As New Cookie(Cookie.TypeClanDisband)
		      Config.BNET.Send(Packets.CreateSID_CLANDISBAND(Cookie.Cookie))
		      
		    End Select
		  End If
		  
		  w.Close()
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ConfirmRemoveClanMember(Config As Configuration, MemberName As String, MemberRank As Byte)
		  
		  If Config = Nil Then Return
		  If Config.BNET = Nil Then Return
		  If Config.BNET.IsConnected = False Or Config.BNET.ClanTag = 0 Then Return
		  
		  Dim ret As Boolean
		  
		  If Config.ConfirmRemovingClanMembers = True Then
		    Dim w As New ClanRemoveMemberWindow()
		    
		    w.fldUsername.Text = MemberName
		    w.fldClanRank.Text = Globals.ClanRankName(MemberRank)
		    w.ShowModal()
		    
		    If w = Nil Then Return
		    ret = (w.Result = w.btnRemove)
		    w.Close()
		    
		  Else
		    ret = True
		    
		  End If
		  
		  If ret = True Then
		    Dim Cookie As New Cookie(Cookie.TypeClanMemberRemove)
		    Cookie.Value("Username") = MemberName
		    Config.BNET.Send(Packets.CreateSID_CLANREMOVEMEMBER(Cookie.Cookie, MemberName))
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ConnectAll()
		  
		  Dim i As Integer = 0
		  While i <= UBound(Settings.Configurations)
		    If Settings.Configurations(i) <> Nil And Settings.Configurations(i).BNET <> Nil Then _
		    Settings.Configurations(i).BNET.DoConnect()
		    i = i + 1
		  Wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CreateLagIcons()
		  
		  // Sizes:
		  //   03x11 x3
		  //   18x11 x1
		  //  =27x11
		  
		  If Globals.imgLagGreen = Nil Then
		    Globals.imgLagGreen = New Picture(3, imgLagIcons.Height, imgLagIcons.Depth)
		    Globals.imgLagGreen.Graphics.DrawPicture(imgLagIcons, 0, 0, 3, 11, 0, 0, 3, 11)
		  End If
		  
		  If Globals.imgLagYellow = Nil Then
		    Globals.imgLagYellow = New Picture(3, imgLagIcons.Height, imgLagIcons.Depth)
		    Globals.imgLagYellow.Graphics.DrawPicture(imgLagIcons, 0, 0, 3, 11, 3, 0, 3, 11)
		  End If
		  
		  If Globals.imgLagRed = Nil Then
		    Globals.imgLagRed = New Picture(3, imgLagIcons.Height, imgLagIcons.Depth)
		    Globals.imgLagRed.Graphics.DrawPicture(imgLagIcons, 0, 0, 3, 11, 6, 0, 3, 11)
		  End If
		  
		  If Globals.imgNoUDP = Nil Then
		    Globals.imgNoUDP = New Picture(18, imgLagIcons.Height, imgLagIcons.Depth)
		    Globals.imgNoUDP.Graphics.DrawPicture(imgLagIcons, 0, 0, 18, 11, 9, 0, 18, 11)
		  End If
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub CreateUserIcons()
		  
		  Dim Icons() As UserIcon
		  Dim Img As Picture = imgUserIcons
		  
		  // Column 0: Products & Flags
		  Icons.Append(New UserIcon(0, &H01, 0, Img, 0, 0))
		  Icons.Append(New UserIcon(0, &H08, 0, Img, 1, 0))
		  Icons.Append(New UserIcon(0, &H02, 0, Img, 2, 0))
		  Icons.Append(New UserIcon(0, &H04, 0, Img, 3, 0))
		  Icons.Append(New UserIcon(0, &H40, 0, Img, 4, 0))
		  Icons.Append(New UserIcon(0, &H20, 0, Img, 5, 0))
		  Icons.Append(New UserIcon(0, &H00100000, 0, Img, 6, 0))
		  Icons.Append(New UserIcon(0, &H00200000, 0, Img, 7, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_CHAT, 0, 0, Img, 8, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 0, Img, 9, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 0, Img, 10, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DSHR, 0, 0, Img, 11, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_SSHR, 0, 0, Img, 12, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_SEXP, 0, 0, Img, 13, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_JSTR, 0, 0, Img, 14, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_D2DV, 0, 0, Img, 15, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 0, Img, 16, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_D2XP, 0, 0, Img, 17, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, 0, Img, 18, 0))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, 0, Img, 19, 0))
		  Icons.Append(New UserIcon(0, 0, MemClass.ReadDWORD("SSPN", 1, False), Img, 20, 0))
		  Icons.Append(New UserIcon(0, 0, MemClass.ReadDWORD("JSPN", 1, False), Img, 21, 0))
		  Icons.Append(New UserIcon(0, 0, MemClass.ReadDWORD("W2SP", 1, False), Img, 22, 0))
		  
		  // Column 1: Warcraft III Characters
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3H1", 1, False), Img, 0, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3H2", 1, False), Img, 1, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3H3", 1, False), Img, 2, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3H4", 1, False), Img, 3, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3H5", 1, False), Img, 4, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3O1", 1, False), Img, 5, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3O2", 1, False), Img, 6, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3O3", 1, False), Img, 7, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3O4", 1, False), Img, 8, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3O5", 1, False), Img, 9, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3N1", 1, False), Img, 10, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3N2", 1, False), Img, 11, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3N3", 1, False), Img, 12, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3N4", 1, False), Img, 13, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3N5", 1, False), Img, 14, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3U1", 1, False), Img, 15, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3U2", 1, False), Img, 16, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3U3", 1, False), Img, 17, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3U4", 1, False), Img, 18, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3U5", 1, False), Img, 19, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3R1", 1, False), Img, 20, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3R2", 1, False), Img, 21, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3R3", 1, False), Img, 22, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3R4", 1, False), Img, 23, 1))
		  Icons.Append(New UserIcon(Packets.BNETProduct_WAR3, 0, MemClass.ReadDWORD("W3R5", 1, False), Img, 24, 1))
		  
		  // Column 2: Warcraft III The Frozen Throne Characters
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3H1", 1, False), Img, 0, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3H2", 1, False), Img, 1, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3H3", 1, False), Img, 2, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3H4", 1, False), Img, 3, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3H5", 1, False), Img, 4, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3H6", 1, False), Img, 5, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3O1", 1, False), Img, 0, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3O2", 1, False), Img, 6, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3O3", 1, False), Img, 7, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3O4", 1, False), Img, 8, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3O5", 1, False), Img, 9, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3O6", 1, False), Img, 10, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3N1", 1, False), Img, 0, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3N2", 1, False), Img, 11, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3N3", 1, False), Img, 12, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3N4", 1, False), Img, 13, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3N5", 1, False), Img, 14, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3N6", 1, False), Img, 15, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3U1", 1, False), Img, 0, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3U2", 1, False), Img, 16, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3U3", 1, False), Img, 17, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3U4", 1, False), Img, 18, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3U5", 1, False), Img, 19, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3U6", 1, False), Img, 20, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3R1", 1, False), Img, 0, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3R2", 1, False), Img, 21, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3R3", 1, False), Img, 22, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3R4", 1, False), Img, 23, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3R5", 1, False), Img, 24, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3R6", 1, False), Img, 25, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3D1", 1, False), Img, 0, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3D2", 1, False), Img, 26, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3D3", 1, False), Img, 27, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3D4", 1, False), Img, 28, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3D5", 1, False), Img, 29, 2))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W3XP, 0, MemClass.ReadDWORD("W3D6", 1, False), Img, 30, 2))
		  
		  // Column 3: Clan Icons
		  Icons.Append(New UserIcon(MemClass.ReadDWORD("CLAN", 1, False), 0, 0, Img, 0, 3))
		  Icons.Append(New UserIcon(MemClass.ReadDWORD("CLAN", 1, False), 0, 1, Img, 1, 3))
		  Icons.Append(New UserIcon(MemClass.ReadDWORD("CLAN", 1, False), 0, 2, Img, 2, 3))
		  Icons.Append(New UserIcon(MemClass.ReadDWORD("CLAN", 1, False), 0, 3, Img, 3, 3))
		  Icons.Append(New UserIcon(MemClass.ReadDWORD("CLAN", 1, False), 0, 4, Img, 4, 3))
		  
		  // Column 4: Diablo Shareware & Diablo Retail
		  Icons.Append(New UserIcon(Packets.BNETProduct_DSHR, 0, 1, Img, 0, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 1, Img, 1, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 2, Img, 2, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 3, Img, 3, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 4, Img, 4, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 5, Img, 5, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 6, Img, 6, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 7, Img, 7, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 8, Img, 8, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 9, Img, 9, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 10, Img, 9, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 11, Img, 10, 4))
		  Icons.Append(New UserIcon(Packets.BNETProduct_DRTL, 0, 12, Img, 11, 4))
		  
		  // Column 7: Starcraft & Warcraft II Sponsors
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, MemClass.ReadDWORD("WCRF", 1, False), Img, 0, 7))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, MemClass.ReadDWORD("WCPL", 1, False), Img, 1, 7))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, MemClass.ReadDWORD("WCGO", 1, False), Img, 2, 7))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, MemClass.ReadDWORD("WCSI", 1, False), Img, 3, 7))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, MemClass.ReadDWORD("WCBR", 1, False), Img, 4, 7))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, MemClass.ReadDWORD("WCPG", 1, False), Img, 5, 7))
		  
		  // Column 5: Starcraft & Warcraft II Wins, Ranks & Ratings
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 12, Img, 11, 5)) // Ladder Rating
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 13, Img, 12, 5)) // Ladder Rank
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 14, Img, 13, 5)) // Highest Ladder Rating
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 1, Img, 0, 5)) // 0 Wins
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 2, Img, 1, 5)) // 1 Win
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 3, Img, 2, 5)) // 2 Wins
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 4, Img, 3, 5)) // etc.
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 5, Img, 4, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 6, Img, 5, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 7, Img, 6, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 8, Img, 7, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 9, Img, 8, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 10, Img, 9, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_STAR, 0, 11, Img, 10, 5)) // 10+ Wins
		  
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 16, Img, 29, 5)) // Blue #1
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 14, Img, 27, 5)) // Blue Border
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 15, Img, 28, 5)) // Gold #1
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 13, Img, 26, 5)) // Blue Tint Gold Border
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 12, Img, 25, 5)) // Red Tint Gold Border
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 1, Img, 14, 5)) // 0 Wins
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 2, Img, 15, 5)) // 1 Win
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 3, Img, 16, 5)) // 2 Wins
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 4, Img, 17, 5)) // etc.
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 5, Img, 18, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 6, Img, 19, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 7, Img, 20, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 8, Img, 21, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 9, Img, 22, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 10, Img, 23, 5))
		  Icons.Append(New UserIcon(Packets.BNETProduct_W2BN, 0, 11, Img, 24, 5)) // 10+ Wins
		  
		  // Give this new list to the cache:
		  Globals.UserIcons = Icons
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DecryptCDKey(CDKey As String, ClientToken As UInt32, ServerToken As UInt32) As String
		  
		  // We must return 36 bytes exactly.
		  // If we return 0 bytes, an error occurred.
		  
		  Soft Declare Function kd_quick Lib App.BNCSUtil (CDKey As Ptr, _
		  Client_Token As Integer, Server_Token As Integer, Public_Value As Ptr, _
		  Product_Value As Ptr, Hash_Buffer As Ptr, Buffer_Len As Integer) As Integer
		  
		  Dim kCDKey As New MemoryBlock(LenB(CDKey) + 1)
		  kCDKey.StringValue(0, LenB(CDKey)) = CDKey
		  
		  Dim kPublicValue As New MemoryBlock(4)
		  Dim kProductValue As New MemoryBlock(4)
		  Dim kHashedKey As New MemoryBlock(20)
		  
		  Dim Result As Integer
		  Result = kd_quick(kCDKey, ClientToken, ServerToken, _
		  kPublicValue, kProductValue, kHashedKey, kHashedKey.Size)
		  
		  If Result = 0 Then Return ""
		  
		  Dim Buffer As String
		  
		  MemClass.WriteDWORD(Buffer, LenB(CDKey), True)
		  MemClass.WriteDWORD(Buffer, kProductValue.UInt32Value(0), True)
		  MemClass.WriteDWORD(Buffer, kPublicValue.UInt32Value(0), True)
		  MemClass.WriteDWORD(Buffer, 0, True)
		  MemClass.WriteRaw(Buffer, kHashedKey)
		  
		  Return Buffer
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DesktopNotification(summary As String, body As String, icon As Integer)
		  
		  #If TargetWin32 = True Then
		    
		    #pragma Unused summary
		    #pragma Unused body
		    #pragma Unused icon
		    
		    MainWindow.FlashWindowEx(1)
		    
		  #ElseIf TargetLinux = True Then
		    
		    Const libnotify = "libnotify.so.4"
		    
		    Soft Declare Function notify_notification_new Lib libnotify (summary As Ptr, body As Ptr, icon As Integer) As Ptr
		    Soft Declare Sub notify_notification_set_timeout Lib libnotify (handle As Ptr, timeout As Integer)
		    Soft Declare Function notify_notification_show Lib libnotify (handle As Ptr, error As Ptr) As Boolean
		    
		    Dim summaryPtr As New MemoryBlock(LenB(summary) + 1)
		    summaryPtr.CString(0) = summary
		    
		    Dim bodyPtr As New MemoryBlock(LenB(body) + 1)
		    bodyPtr.CString(0) = body
		    
		    Dim handle As Ptr = notify_notification_new(summaryPtr, bodyPtr, icon)
		    
		    notify_notification_set_timeout(handle, 1000 + (LenB(summary + body) * 35))
		    
		    If Not notify_notification_show(handle, Nil) Then
		      Dim e As New RuntimeException()
		      e.Message = "Failed to display desktop notification to user."
		      Raise e
		    End If
		    
		  #Else
		    
		    #pragma Unused summary
		    #pragma Unused body
		    #pragma Unused icon
		    
		    // No platform-specific method, default to showing main window:
		    MainWindow.Show()
		    
		  #EndIf
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub DisconnectAll()
		  
		  Dim i As Integer = 0
		  While i <= UBound(Settings.Configurations)
		    If Settings.Configurations(i) <> Nil And Settings.Configurations(i).BNET <> Nil Then _
		    Settings.Configurations(i).BNET.DoDisconnect()
		    i = i + 1
		  Wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function DWORDToIP(Value As UInt32) As String
		  
		  Dim Buffer As String = MemClass.WriteDWORD(Value, True)
		  Dim IPAddress, Delimiter As String, i As Integer
		  
		  Select Case LenB(Buffer)
		  Case 4
		    Delimiter = "."
		  Case 6
		    Delimiter = ":"
		  Case Else
		    Delimiter = " "
		  End Select
		  
		  i = 1
		  While i <= LenB(Buffer)
		    If Len(IPAddress) > 0 Then IPAddress = IPAddress + Delimiter
		    IPAddress = IPAddress + Str(AscB(MidB(Buffer, i, 1)))
		    i = i + 1
		  Wend
		  
		  Return IPAddress
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function EncryptOLSPW(Password As String) As String
		  
		  // OLS Password Hashing
		  
		  Return Globals.BSHA1(Password)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function EncryptOLSPW(Password As String, ClientToken As UInt32, ServerToken As UInt32) As String
		  
		  // OLS Password Hashing
		  
		  Soft Declare Sub doubleHashPassword Lib App.BNCSUtil (Password As Ptr, _
		  ClientToken As Integer, ServerToken As Integer, outBuffer As Ptr)
		  
		  Dim PW As New MemoryBlock(LenB(Password) + 1)
		  Dim Hash As New MemoryBlock(20)
		  
		  PW.StringValue(0, LenB(Password)) = Password
		  doubleHashPassword(PW, ClientToken, ServerToken, Hash)
		  
		  Return Hash.StringValue(0, Hash.Size)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function FillString(Length As UInt64, Pattern As String) As String
		  
		  If Length < 1 Then Return ""
		  Dim Buffer As String
		  
		  While Len(Buffer) < Length
		    Buffer = Buffer + Pattern
		  Wend
		  
		  Return Left(Buffer, Length)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function FillStringB(Length As UInt64, Pattern As String) As String
		  
		  If Length < 1 Then Return ""
		  Dim Buffer As String
		  
		  While LenB(Buffer) < Length
		    Buffer = Buffer + Pattern
		  Wend
		  
		  Return LeftB(Buffer, Length)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub FlashWindowEx(Extends win as window, x as integer)
		  //Windows Functionality Suite 2.5
		  #if TargetWin32
		    //requires windows 98 or higher
		    Const FLASHW_STOP = 0 'Stop flashing. The system restores the window to its original state.
		    Const FLASHW_CAPTION = &H1 'Flash the window caption.
		    Const FLASHW_TRAY = &H2 'Flash the taskbar button.
		    'Const FLASHW_ALL = (FLASHW_CAPTION Or FLASHW_TRAY) 'Flash both the window caption and taskbar button. This is equivalent to setting the FLASHW_CAPTION Or FLASHW_TRAY flags.
		    Const FLASHW_TIMER = &H4 'Flash continuously, until the FLASHW_STOP flag is set.
		    Const FLASHW_TIMERNOFG = &HC 'Flash continuously until the window comes to the foreground.
		    'Private Type FLASHWINFO
		    'cbSize As Long '0
		    'hwnd As Long '4
		    'dwFlags As Long '8
		    'uCount As Long '12
		    'dwTimeout As Long '16
		    'End Type
		    Declare Function FlashWindowEx Lib "user32" (pfwi As ptr) As integer
		    Dim FlashInfo As New MemoryBlock(20)
		    'Specifies the size of the structure.
		    flashinfo.Long(0) = 20
		    'Specifies the flash status
		    flashinfo.long(8) = FLASHW_CAPTION + FLASHW_TRAY
		    'Specifies the rate, in milliseconds, at which the window will be flashed. If dwTimeout is zero, the function uses the default cursor blink rate.
		    FlashInfo.long(16) = 0
		    'Handle to the window to be flashed. The window can be either opened or minimized.
		    FlashInfo.long(4) = win.Handle
		    'Specifies the number of times to flash the window.
		    FlashInfo.long(12) = x
		    if FlashWindowEx(FlashInfo) =0 then
		      ///succeeded
		    end
		  #endif
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function FontUnitCSS(fontUnit As FontUnits) As String
		  
		  Select Case fontUnit
		  Case FontUnits.Default
		    #If TargetMacOS Then
		      Return "pt"
		    #ElseIf TargetWin32 Then
		      Return "px"
		    #EndIf
		  Case FontUnits.Pixel
		    Return "px"
		  Case FontUnits.Point
		    Return "pt"
		  Case FontUnits.Inches
		    Return "in"
		  Case fontUnits.Millimeter
		    Return "mm"
		  End Select
		  
		  Return ""
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub ForceRedraw(list As ListBox)
		  
		  // REALbasic is retarded as hell and both .Refresh and .Invalidate will
		  // not actually touch the ListBox (RectControl) so that it will redraw, so
		  // I'm using my little cheat:
		  Dim i As Integer = list.DefaultRowHeight
		  Dim j As Integer = list.ScrollPosition
		  list.DefaultRowHeight = 0
		  list.DefaultRowHeight = i
		  list.ScrollPosition = j
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GenerateBYTE() As Byte
		  
		  Return Round(Rnd() * 256)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GenerateDWORD() As UInt32
		  
		  Dim Buffer As String
		  
		  Buffer = ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256))
		  
		  Return MemClass.ReadDWORD(Buffer, 1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GenerateQWORD() As UInt64
		  
		  Dim Buffer As String
		  
		  Buffer = ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256)) + _
		  ChrB(Round(Rnd() * 256))
		  
		  Return MemClass.ReadQWORD(Buffer, 1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GenerateString(Length As UInt64, Base As String) As String
		  
		  Dim Buffer As String
		  
		  While Len(Buffer) < Length
		    Buffer = Buffer + Mid(Base, Round(Rnd() * (Len(Base) + 1)), 1)
		  Wend
		  
		  Return Buffer
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GenerateStringB(Length As UInt64, Base As String) As String
		  
		  Dim Buffer As String
		  
		  While LenB(Buffer) < Length
		    Buffer = Buffer + MidB(Base, Round(Rnd() * (LenB(Base) + 1)), 1)
		  Wend
		  
		  Return Buffer
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GenerateWORD() As UInt16
		  
		  Dim Buffer As String
		  
		  Buffer = ChrB(Round(Rnd() * 256)) + ChrB(Round(Rnd() * 256))
		  
		  Return MemClass.ReadWORD(Buffer, 1)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetClanIcon(Rank As Byte) As Picture
		  
		  Dim Icon As UserIcon
		  
		  Dim dwProduct As UInt32 = MemClass.ReadDWORD("CLAN", 1, False)
		  
		  Dim i As Integer = UBound(Globals.UserIcons)
		  While i >= 0
		    
		    Icon = Globals.UserIcons(i)
		    If Icon = Nil Then
		      Globals.UserIcons.Remove(i)
		      i = i - 1
		      Continue While
		    End If
		    
		    // Clan
		    If Icon.Product = dwProduct And Icon.Flags = 0 And Icon.ExtraData = Rank Then Exit While
		    
		    i = i - 1
		  Wend
		  
		  If i < 0 Then Return Globals.imgUserIconUnrecognized() Else Return Icon.Bitmap
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetErrorMessage(Extends obj As SocketCore) As String
		  
		  Select Case obj.LastErrorCode
		  Case SocketCore.NoError
		    Return "No error"
		    
		  Case SocketCore.OpenDriverError
		    Return "Open driver error"
		    
		  Case SocketCore.LostConnection
		    Return "Lost connection"
		    
		  Case SocketCore.NameResolutionError
		    Return "Name resolution error"
		    
		  Case SocketCore.AddressInUseError
		    Return "Address in use error"
		    
		  Case SocketCore.InvalidStateError
		    Return "Invalid state error"
		    
		  Case SocketCore.InvalidPortError
		    Return "Invalid port error"
		    
		  Case SocketCore.OutOfMemoryError
		    Return "Out of memory error"
		    
		  Case Else
		    Return ""
		    
		  End Select
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetLagIcon(Ping As Integer, UDPSupport As Boolean) As Picture
		  
		  Dim ret, retUDP, retPing As Picture
		  Dim i, j, w, h, d As Integer
		  Dim PR, retPR As PingRange
		  
		  If UDPSupport = False Then retUDP = Globals.imgNoUDP Else retUDP = Nil
		  
		  // w is used to determine how much width we need per bar:
		  w = (Globals.imgLagGreen.Width + Globals.imgLagYellow.Width + Globals.imgLagRed.Width) / 3
		  
		  // h is used to determine how much height we need for everything:
		  h = (Globals.imgLagGreen.Height + Globals.imgLagYellow.Height + Globals.imgLagRed.Height + Globals.imgNoUDP.Height) / 4
		  
		  // d is used to determine how much depth we need for everything:
		  d = (Globals.imgLagGreen.Depth + Globals.imgLagYellow.Depth + Globals.imgLagRed.Depth + Globals.imgNoUDP.Depth) / 4
		  
		  // Find the ping value, and calculate our final width:
		  If Settings.PrefPingRangesFlushRight = False Then
		    
		    // Find width based on bar count. While doing this, look for our ping range.
		    i = 0
		    j = w
		    While i <= UBound(Settings.PrefPingRanges)
		      PR = Settings.PrefPingRanges(i)
		      If PR <> Nil Then
		        If PR.BarCount * w > j Then j = PR.BarCount * w
		        If Ping >= PR.LowestPing And Ping <= PR.HighestPing Then retPR = PR
		      End If
		      i = i + 1
		    Wend
		    
		  Else
		    
		    // Find our ping range, ignoring the widths of all other ping ranges:
		    i = 0
		    j = 0
		    While i <= UBound(Settings.PrefPingRanges)
		      PR = Settings.PrefPingRanges(i)
		      If PR <> Nil And Ping >= PR.LowestPing And Ping <= PR.HighestPing Then
		        retPR = PR
		        Exit While // We're done, so let's exit this loop to save time.
		      End If
		      i = i + 1
		    Wend
		    If retPR <> Nil Then j = retPR.BarCount * w
		    
		  End If
		  
		  If retPR <> Nil Then
		    // Compile the ping icon:
		    retPing = New Picture(j, h, d)
		    retPing.Graphics.ForeColor = Colors.Black
		    retPing.Graphics.FillRect(0, 0, retPing.Graphics.Width, retPing.Graphics.Height)
		    retPing.Mask(True).Graphics.ForeColor = Colors.White
		    retPing.Mask().Graphics.FillRect(0, 0, retPing.Mask().Width, retPing.Mask().Height)
		    retPing.Mask().Graphics.ForeColor = Colors.Black
		    i = 0
		    While i < retPR.BarCount
		      Select Case retPR.BarColor
		      Case retPR.BarColor_Green
		        retPing.Graphics.DrawPicture(Globals.imgLagGreen, i * w, 0)
		        retPing.Mask().Graphics.FillRect(i * w, 0, w, h)
		        
		      Case retPR.BarColor_Yellow
		        retPing.Graphics.DrawPicture(Globals.imgLagYellow, i * w, 0)
		        retPing.Mask().Graphics.FillRect(i * w, 0, w, h)
		        
		      Case retPR.BarColor_Red
		        retPing.Graphics.DrawPicture(Globals.imgLagRed, i * w, 0)
		        retPing.Mask().Graphics.FillRect(i * w, 0, w, h)
		        
		      End Select
		      i = i + 1
		    Wend
		  End If
		  
		  // Compile the final icon:
		  If retUDP <> Nil And retPing = Nil Then
		    // We did not compile the ping, and UDP is not supported.
		    ret = retUDP
		    
		  ElseIf retUDP = Nil And retPing <> Nil Then
		    // We compiled the ping, but UDP is supported.
		    ret = retPing
		    
		  ElseIf retUDP <> Nil And retPing <> Nil Then
		    // We compiled both the ping and the UDP icon. We must merge them.
		    ret = New Picture(retUDP.Width + retPing.Width, _
		    (retUDP.Height + retPing.Height) / 2, (retUDP.Depth + retPing.Depth) / 2)
		    
		    ret.Graphics.DrawPicture(retUDP, 0, 0)
		    ret.Mask(True).Graphics.DrawPicture(retUDP.Mask(True), 0, 0)
		    
		    ret.Graphics.DrawPicture(retPing, retUDP.Width, 0)
		    ret.Mask(True).Graphics.DrawPicture(retPing.Mask(True), retUDP.Width, 0)
		    
		  Else
		    // Nothing was compiled whatsoever.
		    ret = Nil
		    
		  End If
		  
		  Return ret
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetProductIcon(Product As UInt32) As Picture
		  
		  Dim Icon As UserIcon
		  
		  Dim i As Integer = UBound(Globals.UserIcons)
		  While i >= 0
		    
		    Icon = Globals.UserIcons(i)
		    If Icon = Nil Then
		      Globals.UserIcons.Remove(i)
		      i = i - 1
		      Continue While
		    End If
		    
		    // Product
		    If Icon.Product = Product And Icon.Flags = 0 And Icon.ExtraData = 0 Then Exit While
		    
		    i = i - 1
		  Wend
		  
		  If i < 0 Then Return Globals.imgUserIconUnrecognized() Else Return Icon.Bitmap
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetUserIcon(Flags As UInt32, Statstring As String) As Picture
		  
		  Dim Buffer As New Picture(28 * 2, 14, 24), g As Graphics = Buffer.Graphics
		  Dim FoundFlags As Boolean = False
		  Dim FoundStats As Boolean = False
		  Dim FoundProduct As Boolean = False
		  Dim Icon As UserIcon
		  Dim Product As UInt32 = MemClass.ReadDWORD(Statstring, 1, True)
		  Dim Rating As Integer = Val(NthField(Statstring, " ", 2))
		  Dim Rank As Integer = Val(NthField(Statstring, " ", 3))
		  Dim Wins As Integer = Val(NthField(Statstring, " ", 4))
		  Dim Spawned As Boolean = (Val(NthField(Statstring, " ", 5)) <> 0)
		  Dim HighestRating As Integer = Val(NthField(Statstring, " ", 7))
		  Dim IronManRating As Integer = Val(NthField(Statstring, " ", 8))
		  Dim IronManRank As Integer = Val(NthField(Statstring, " ", 9))
		  Dim WCIcon As UInt32 = MemClass.ReadDWORD(NthField(Statstring, " ", 2), 1, True)
		  Dim LegacyIcon As UInt32 = MemClass.ReadDWORD(NthField(Statstring, " ", 10), 1, True)
		  Dim OverlayText As String
		  
		  #pragma Unused IronManRating
		  
		  Dim IconJSPN As UInt32 = MemClass.ReadDWORD("JSPN", 1, False)
		  Dim IconSSPN As UInt32 = MemClass.ReadDWORD("SSPN", 1, False)
		  Dim IconW2SP As UInt32 = MemClass.ReadDWORD("W2SP", 1, False)
		  
		  g.TextFont = "Microsoft Sans Serif"
		  g.TextSize = 9
		  
		  g.ForeColor = Colors.UI.ControlBackColor
		  g.FillRect(0, 0, g.Width, g.Height)
		  
		  Dim i As Integer = 0
		  While i <= UBound(Globals.UserIcons)
		    
		    Icon = Globals.UserIcons(i)
		    If Icon = Nil Then
		      Globals.UserIcons.Remove(i)
		      Continue While
		    End If
		    
		    // Statstring: WAR3
		    If FoundStats = False And Product = Packets.BNETProduct_WAR3 And _
		      Icon.Product = Packets.BNETProduct_WAR3 And Icon.ExtraData = WCIcon And _
		      WCIcon <> 0 And WCIcon <> Product Then
		      FoundStats = True
		      g.DrawPicture(Icon.Bitmap, 0, 0)
		    End If
		    
		    // Statstring: W3XP
		    If FoundStats = False And Product = Packets.BNETProduct_W3XP And _
		      Icon.Product = Packets.BNETProduct_W3XP And Icon.ExtraData = WCIcon And _
		      WCIcon <> 0 And WCIcon <> Product Then
		      FoundStats = True
		      g.DrawPicture(Icon.Bitmap, 0, 0)
		    End If
		    
		    // Statstring: JSTR (Spawn)
		    If FoundStats = False And Product = Packets.BNETProduct_JSTR And _
		      Icon.Product = 0 And Icon.Flags = 0 And Icon.ExtraData = IconJSPN And _
		      Spawned = True Then
		      FoundStats = True
		      g.DrawPicture(Icon.Bitmap, 0, 0)
		    End If
		    
		    // Statstring: STAR (Spawn)
		    If FoundStats = False And Product = Packets.BNETProduct_STAR And _
		      Icon.Product = 0 And Icon.Flags = 0 And Icon.ExtraData = IconSSPN And _
		      Spawned = True Then
		      FoundStats = True
		      g.DrawPicture(Icon.Bitmap, 0, 0)
		    End If
		    
		    // Statstring: W2BN (Spawn)
		    If FoundStats = False And Product = Packets.BNETProduct_W2BN And _
		      Icon.Product = 0 And Icon.Flags = 0 And Icon.ExtraData = IconW2SP And _
		      Spawned = True Then
		      FoundStats = True
		      g.DrawPicture(Icon.Bitmap, 0, 0)
		    End If
		    
		    // Statstring: STAR, SEXP, JSTR, SSHR, W2BN (Sponsors)
		    If FoundStats = False And (Globals.IsStarcraft(Product) = True Or _
		      Globals.IsWarcraftII(Product) = True) And Icon.ExtraData = LegacyIcon And _
		      Icon.Flags = 0 And LegacyIcon <> 0 And LegacyIcon <> Product Then
		      FoundStats = True
		      g.DrawPicture(Icon.Bitmap, 0, 0)
		    End If
		    
		    // Statstring: STAR, SEXP, JSTR, SSHR (Rating/Rank/Highest Rating)
		    If FoundStats = False And Globals.IsStarcraft(Product) = True And _
		      Globals.IsStarcraft(Icon.Product) = True And Icon.Flags = 0 And _
		      ((Icon.ExtraData = 12 And Rating > 0) _
		      Or (Icon.ExtraData = 13 And Rank > 0) _
		      Or (Icon.ExtraData = 14 And HighestRating > 0)) Then
		      FoundStats = True
		      g.DrawPicture(Icon.Bitmap, 0, 0)
		      If (Icon.ExtraData = 12 And Rating > 0) Then
		        OverlayText = NthField(Statstring, " ", 2)
		      ElseIf (Icon.ExtraData = 13 And Rank > 0) Then
		        OverlayText = NthField(Statstring, " ", 3)
		      ElseIf (Icon.ExtraData = 14 And HighestRating > 0) Then
		        OverlayText = NthField(Statstring, " ", 7)
		      Else
		        OverlayText = ""
		      End If
		    End If
		    
		    // Statstring: STAR, SEXP, JSTR, SSHR (Wins)
		    If FoundStats = False And Globals.IsStarcraft(Product) = True And _
		      Globals.IsStarcraft(Icon.Product) = True And Icon.ExtraData <> 0 Then
		      If (Icon.ExtraData - 1 = Wins And Wins >= 0 And Wins <= 10) Or (Icon.ExtraData = 11 And Wins > 10) Then
		        FoundStats = True
		        g.DrawPicture(Icon.Bitmap, 0, 0)
		      End If
		    End If
		    
		    // Statstring: W2BN (Rating/Rank/Highest Rating/IronMan Rating/IronMan Rank)
		    If FoundStats = False And Globals.IsWarcraftII(Product) = True And _
		      Globals.IsWarcraftII(Icon.Product) = True And Icon.Flags = 0 And _
		      ((Icon.ExtraData = 16 And IronManRank = 1) Or _
		      (Icon.ExtraData = 14 And IronManRank > 1) Or _
		      (Icon.ExtraData = 15 And Rank = 1) Or _
		      (Icon.ExtraData = 13 And Rank > 0 And Rank <= 1000) Or _
		      (Icon.ExtraData = 12 And Rank > 0)) Then
		      FoundStats = True
		      g.DrawPicture(Icon.Bitmap, 0, 0)
		      If (Icon.ExtraData = 16 And IronManRank = 1) Then
		        OverlayText = NthField(Statstring, " ", 8)
		      ElseIf (Icon.ExtraData = 14 And IronManRank > 1) Then
		        OverlayText = NthField(Statstring, " ", 9)
		      ElseIf (Icon.ExtraData = 15 And Rank = 1) Then
		        OverlayText = NthField(Statstring, " ", 2)
		      ElseIf (Icon.ExtraData = 13 And Rank > 0 And Rank <= 1000) Then
		        OverlayText = NthField(Statstring, " ", 3)
		      ElseIf (Icon.ExtraData = 12 And Rank > 0) Then
		        OverlayText = NthField(Statstring, " ", 3)
		      Else
		        OverlayText = ""
		      End If
		    End If
		    
		    // Statstring: W2BN (Wins)
		    If FoundStats = False And Globals.IsWarcraftII(Product) = True And _
		      Globals.IsWarcraftII(Icon.Product) = True And Icon.ExtraData <> 0 Then
		      If (Icon.ExtraData - 1 = Wins And Wins >= 0 And Wins <= 10) Or (Icon.ExtraData = 11 And Wins > 10) Then
		        FoundStats = True
		        g.DrawPicture(Icon.Bitmap, 0, 0)
		      End If
		    End If
		    
		    // Flags
		    If FoundFlags = False And BitAnd(Flags, Icon.Flags) > 0 And Icon.ExtraData = 0 And Icon.Product = 0 Then
		      FoundFlags = True
		      If FoundStats = False Then g.DrawPicture(Icon.Bitmap, 0, 0)
		    End If
		    
		    // Product
		    If FoundProduct = False And Icon.Flags = 0 And Icon.Product = Product And Icon.ExtraData = 0 Then
		      FoundProduct = True
		      g.DrawPicture(Icon.Bitmap, 28, 0)
		    End If
		    
		    i = i + 1
		  Wend
		  
		  If FoundFlags = False And FoundStats = False And FoundProduct = False Then Buffer = Globals.imgUserIconUnrecognized()
		  
		  If LenB(OverlayText) > 0 Then
		    // Draw once with black, then again with yellow
		    // (that way it's easier to see the text if the icon is yellow already)
		    g.ForeColor = Colors.Black
		    g.DrawString(OverlayText, 28 - g.StringWidth(OverlayText), g.Height - 1, 26, False)
		    g.ForeColor = Colors.Yellow
		    g.DrawString(OverlayText, 27 - g.StringWidth(OverlayText), g.Height - 2, 26, False)
		  End If
		  
		  Return Buffer
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IIf(condition As Boolean, trueValue As Variant, falseValue As Variant) As Variant
		  
		  If condition Then Return trueValue Else Return falseValue
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function imgUserIconUnrecognized() As Picture
		  
		  Dim Buffer As Picture = New Picture(28, 14, 24)
		  Dim g As Graphics = Buffer.Graphics
		  
		  g.ForeColor = Colors.Black
		  g.FillRect(0, 0, g.Width, g.Height)
		  g.ForeColor = Colors.Red
		  g.TextFont = "Arial"
		  g.TextSize = 12
		  g.Bold = True
		  g.DrawString("?", g.Width/2 - g.StringWidth("?") / 2, g.Height / 2 + g.TextAscent / 2)
		  
		  Return Buffer
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IPToBytes(IPAddress As String) As String
		  
		  Dim Fields() As String
		  
		  If InStr(IPAddress, ".") > 0 Then
		    Fields = Split(IPAddress, ".")
		  ElseIf InStr(IPAddress, ":") > 0 Then
		    Fields = Split(IPAddress, ":")
		  ElseIf InStr(IPAddress, " ") > 0 Then
		    Fields = Split(IPAddress, " ")
		  Else
		    Return ""
		  End If
		  
		  Dim Buffer As String
		  
		  For Each Field As String In Fields
		    MemClass.WriteBYTE(Buffer, Val(Field))
		  Next
		  
		  Return Buffer
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IPToDWORD(IPAddress As String) As UInt32
		  
		  Dim Fields() As String
		  
		  If InStr(IPAddress, ".") > 0 Then
		    Fields = Split(IPAddress, ".")
		  ElseIf InStr(IPAddress, ":") > 0 Then
		    Fields = Split(IPAddress, ":")
		  ElseIf InStr(IPAddress, " ") > 0 Then
		    Fields = Split(IPAddress, " ")
		  Else
		    Return 0
		  End If
		  
		  Dim Buffer As String
		  
		  For Each Field As String In Fields
		    MemClass.WriteBYTE(Buffer, Val(Field))
		  Next
		  
		  Return MemClass.ReadDWORD(Buffer, 1, True)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsDiablo(Product As UInt32) As Boolean
		  
		  Select Case Product
		  Case Packets.BNETProduct_DRTL
		  Case Packets.BNETProduct_DSHR
		  Case Else
		    Return False
		  End Select
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsDiabloII(Product As UInt32) As Boolean
		  
		  Select Case Product
		  Case Packets.BNETProduct_D2DV
		  Case Packets.BNETProduct_D2XP
		  Case Else
		    Return False
		  End Select
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsFriend(Sock As BNETSocket, Username As String) As Boolean
		  
		  If Sock = Nil Or Sock.FriendsList = Nil Then Return False
		  
		  Dim i As Integer = 0, dTmp As Dictionary
		  While i < Sock.FriendsList.Count
		    
		    dTmp = Sock.FriendsList.Value(Sock.FriendsList.Key(i))
		    If dTmp <> Nil And dTmp.Value("Username") = Username Then Return True
		    
		    i = i + 1
		  Wend
		  
		  Return False
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsIPv4(value As String) As Boolean
		  
		  If CountFields(value, ".") <> 4 Then Return False
		  
		  Dim a, b, c, d As Integer
		  
		  a = Val(NthField(value, ".", 1))
		  b = Val(NthField(value, ".", 2))
		  c = Val(NthField(value, ".", 3))
		  d = Val(NthField(value, ".", 4))
		  
		  Return (a >= 0 And a <= 255 And b >= 0 And b <= 255 And c >= 0 And c <= 255 And d >= 0 And d <= 255)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsIPv6(value As String) As Boolean
		  
		  Return (InStr(value, ":") > 0)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsStarcraft(Product As UInt32) As Boolean
		  
		  Select Case Product
		  Case Packets.BNETProduct_STAR
		  Case Packets.BNETProduct_SEXP
		  Case Packets.BNETProduct_JSTR
		  Case Packets.BNETProduct_SSHR
		  Case Else
		    Return False
		  End Select
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsWarcraftII(Product As UInt32) As Boolean
		  
		  Return (Product = Packets.BNETProduct_W2BN)
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function IsWarcraftIII(Product As UInt32) As Boolean
		  
		  Select Case Product
		  Case Packets.BNETProduct_WAR3
		  Case Packets.BNETProduct_W3DM
		  Case Packets.BNETProduct_W3XP
		  Case Else
		    Return False
		  End Select
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function MessageBlacklistMatch(Pattern As String) As Boolean
		  
		  Dim regex As New RegEx()
		  Dim match As RegExMatch
		  Dim i As Integer = UBound(Settings.PrefMessageBlacklist)
		  While i >= 0
		    Select Case Settings.PrefMessageBlacklist(i).Left
		    Case 0 // Exact
		      If Settings.PrefMessageBlacklist(i).Right = Pattern Then Return True
		    Case 1 // Regex
		      Try
		        regex.SearchPattern = Settings.PrefMessageBlacklist(i).Right
		        match = regex.Search(Pattern)
		        If match <> Nil Then Return True
		      Catch err As RegExException
		        i = i - 1
		        Continue // Regex problem, skip this pattern
		      End Try
		    Case Else // Unknown
		    End Select
		    i = i - 1
		  Wend
		  
		  Return False // No Match
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function NeedsCDKey(Product As UInt32) As Boolean
		  
		  Select Case Product
		  Case Packets.BNETProduct_STAR
		  Case Packets.BNETProduct_SEXP
		  Case Packets.BNETProduct_JSTR
		  Case Packets.BNETProduct_D2DV
		  Case Packets.BNETProduct_D2XP
		  Case Packets.BNETProduct_W2BN
		  Case Packets.BNETProduct_WAR3
		  Case Packets.BNETProduct_W3XP
		  Case Else
		    Return False
		  End Select
		  
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function NeedsCDKeyExpansion(Product As UInt32) As Boolean
		  
		  Select Case Product
		  Case Packets.BNETProduct_D2XP
		  Case Packets.BNETProduct_W3XP
		  Case Else
		    Return False
		  End Select
		  
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ProductConvert(Product As UInt32, ToBNLS As Boolean) As UInt32
		  
		  If ToBNLS = True Then
		    Select Case Product
		    Case Packets.BNETProduct_STAR
		      Return &H01
		      
		    Case Packets.BNETProduct_SEXP
		      Return &H02
		      
		    Case Packets.BNETProduct_W2BN
		      Return &H03
		      
		    Case Packets.BNETProduct_D2DV
		      Return &H04
		      
		    Case Packets.BNETProduct_D2XP
		      Return &H05
		      
		    Case Packets.BNETProduct_JSTR
		      Return &H06
		      
		    Case Packets.BNETProduct_WAR3
		      Return &H07
		      
		    Case Packets.BNETProduct_W3XP
		      Return &H08
		      
		    Case Packets.BNETProduct_DRTL
		      Return &H09
		      
		    Case Packets.BNETProduct_DSHR
		      Return &H0A
		      
		    Case Packets.BNETProduct_SSHR
		      Return &H0B
		      
		    Case Packets.BNETProduct_W3DM
		      Return &H0C
		      
		    Case Else
		      Return &H00
		      
		    End Select
		  Else
		    Select Case Product
		    Case &H01
		      Return Packets.BNETProduct_STAR
		      
		    Case &H02
		      Return Packets.BNETProduct_SEXP
		      
		    Case &H03
		      Return Packets.BNETProduct_W2BN
		      
		    Case &H04
		      Return Packets.BNETProduct_D2DV
		      
		    Case &H05
		      Return Packets.BNETProduct_D2XP
		      
		    Case &H06
		      Return Packets.BNETProduct_JSTR
		      
		    Case &H07
		      Return Packets.BNETProduct_WAR3
		      
		    Case &H08
		      Return Packets.BNETProduct_W3XP
		      
		    Case &H09
		      Return Packets.BNETProduct_DRTL
		      
		    Case &H0A
		      Return Packets.BNETProduct_DSHR
		      
		    Case &H0B
		      Return Packets.BNETProduct_SSHR
		      
		    Case &H0C
		      Return Packets.BNETProduct_W3DM
		      
		    Case Else
		      Return &H00000000
		      
		    End Select
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ProductName(Product As UInt32, ForChannels As Boolean = False) As String
		  
		  Select Case Product
		  Case 0
		    Return "(null)"
		    
		  Case Packets.BNETProduct_STAR
		    If ForChannels = False Then Return "Starcraft Original" Else Return "StarCraft"
		    
		  Case Packets.BNETProduct_SEXP
		    If ForChannels = False Then Return "Starcraft Broodwar" Else Return "Brood War"
		    
		  Case Packets.BNETProduct_JSTR
		    If ForChannels = False Then Return "Starcraft Japanese" Else Return "StarCraft Japanese"
		    
		  Case Packets.BNETProduct_SSHR
		    If ForChannels = False Then Return "Starcraft Shareware" Else Return "StarCraft Shareware"
		    
		  Case Packets.BNETProduct_DRTL
		    Return "Diablo"
		    
		  Case Packets.BNETProduct_DSHR
		    Return "Diablo Shareware"
		    
		  Case Packets.BNETProduct_D2DV
		    Return "Diablo II"
		    
		  Case Packets.BNETProduct_D2XP
		    If ForChannels = False Then Return "Diablo II Lord of Destruction" Else Return "Lord of Destruction"
		    
		  Case Packets.BNETProduct_W2BN
		    If ForChannels = False Then Return "Warcraft II BNE" Else Return "WarCraft II"
		    
		  Case Packets.BNETProduct_WAR3
		    If ForChannels = False Then Return "Warcraft III Reign of Chaos" Else Return "WarCraft III"
		    
		  Case Packets.BNETProduct_W3DM
		    If ForChannels = False Then Return "Warcraft III Demo" Else Return "WarCraft III Demo"
		    
		  Case Packets.BNETProduct_W3XP
		    If ForChannels = False Then Return "Warcraft III The Frozen Throne" Else Return "Frozen Throne"
		    
		  Case Packets.BNETProduct_CHAT
		    If ForChannels = False Then Return "Chat Client" Else Return "Chat"
		    
		  Case Else
		    Return "(unknown: " + MemClass.HexPrefix(Product, "0x", 8) + ")"
		    
		  End Select
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function QWORDToIP(Value As UInt64) As String
		  
		  Dim Buffer As String = MemClass.WriteDWORD(Value, False)
		  Dim IPAddress, Delimiter As String, i As Integer
		  
		  Select Case LenB(Buffer)
		  Case 4
		    Delimiter = "."
		  Case 6
		    Delimiter = ":"
		  Case Else
		    Delimiter = " "
		  End Select
		  
		  i = 1
		  While i <= LenB(Buffer)
		    If Len(IPAddress) > 0 Then IPAddress = IPAddress + Delimiter
		    IPAddress = IPAddress + Str(AscB(MidB(Buffer, i, 1)))
		    i = i + 1
		  Wend
		  
		  Return IPAddress
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ReconnectAll()
		  
		  Dim i As Integer = 0
		  While i <= UBound(Settings.Configurations)
		    If Settings.Configurations(i) <> Nil And Settings.Configurations(i).BNET <> Nil Then _
		    Settings.Configurations(i).BNET.DoReconnect()
		    i = i + 1
		  Wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub RemoveProfileDialog(Dialog As ProfileWindow)
		  
		  Dim i As Integer = UBound(Globals.ProfileDialogs)
		  While i >= 0
		    
		    If Globals.ProfileDialogs(i) = Dialog Then
		      Globals.ProfileDialogs(i).IsRemoving = True
		      Globals.ProfileDialogs(i).Close()
		      Globals.ProfileDialogs.Remove(i)
		      Exit While
		    End If
		    
		    i = i - 1
		  Wend
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ReverseString(value As String) As String
		  
		  Dim i As Integer
		  Dim j As Integer
		  Dim r As String
		  
		  j = Len( value )
		  
		  For i = j DownTo 1
		    r = r + Mid( value, i, 1 )
		  Next
		  
		  Return r
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ReverseStringB(value As String) As String
		  
		  Dim i As Integer
		  Dim j As Integer
		  Dim r As String
		  
		  j = LenB( value )
		  
		  For i = j DownTo 1
		    r = r + MidB( value, i, 1 )
		  Next
		  
		  Return r
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SafeString(strValue As String, strSafeValue As String = " ") As String
		  
		  Dim i As Integer, buf As String = strValue
		  
		  For i = 0 To 31
		    buf = ReplaceAll(buf, Chr(i), strSafeValue)
		  Next
		  
		  Return buf
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SBool(Source As String) As Boolean
		  
		  Select Case Source
		    
		  Case "True"
		  Case "Yes"
		  Case "On"
		  Case "Y"
		  Case "Positive"
		  Case "Yep"
		  Case "Agree"
		  Case "Agreed"
		  Case "Correct"
		  Case "Okay"
		  Case "Ok"
		  Case "K"
		    
		  Case Else
		    If IsNumeric(Source) = True And Val(Source) > 0 Then Return True Else Return False
		    
		  End Select
		  
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SBoolCheck(Source As String) As Boolean
		  
		  Select Case Source
		    
		  Case "True", "False"
		  Case "Yes", "No"
		  Case "On", "Off"
		  Case "Y", "N"
		  Case "Positive", "Negative"
		  Case "Yep", "Nope"
		  Case "Agree", "Disagree"
		  Case "Agreed", "Disagreed"
		  Case "Correct", "Incorrect"
		  Case "Okay", "Cancel"
		  Case "Ok", "Fail"
		  Case "K", "Bad"
		    
		  Case Else
		    Return IsNumeric(Source)
		    
		  End Select
		  
		  Return True
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SChannelFlags(Flags As UInt32) As String
		  
		  Dim Buffer As String
		  
		  If BitAnd(Flags, &H00001) > 0 Then Buffer = "Public" Else Buffer = "Private"
		  If BitAnd(Flags, &H00002) > 0 Then Buffer = Buffer + ", Moderated"
		  If BitAnd(Flags, &H00004) > 0 Then Buffer = Buffer + ", Restricted"
		  If BitAnd(Flags, &H00008) > 0 Then Buffer = Buffer + ", Silent"
		  If BitAnd(Flags, &H00010) > 0 Then Buffer = Buffer + ", System"
		  If BitAnd(Flags, &H00020) > 0 Then Buffer = Buffer + ", Product-Specific"
		  If BitAnd(Flags, &H01000) > 0 Then Buffer = Buffer + ", Global"
		  If BitAnd(Flags, &H04000) > 0 Then Buffer = Buffer + ", Redirected"
		  If BitAnd(Flags, &H08000) > 0 Then Buffer = Buffer + ", Chat"
		  If BitAnd(Flags, &H10000) > 0 Then Buffer = Buffer + ", Tech Support"
		  
		  Return Buffer
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function SClanTag(Tag As UInt32) As String
		  
		  Return ReplaceAll(MemClass.WriteDWORD(Tag, False), ChrB(0), "")
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ShowClanMemberInfo(Username As String, ClanTag As UInt32, ClanName As String, ClanRank As Byte, DateJoined As Date)
		  
		  Dim w As New ClanMemberInfoWindow()
		  
		  w.Title = Username + " - Clan Info"
		  w.fldUsername.Text = Username
		  w.fldClanTag.Text = Globals.SClanTag(ClanTag)
		  w.fldClanName.Text = ClanName
		  w.fldClanRank.Text = Globals.ClanRankName(ClanRank)
		  If DateJoined = Nil Then
		    w.fldClanDateJoined.Text = ""
		  Else
		    w.fldClanDateJoined.Text = DateJoined.ShortDate + " " + DateJoined.LongTime
		  End If
		  
		  w.ShowModal()
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function StringToJSON(value As String) As String
		  
		  Dim i, a, valueLen As Integer, c, json As String
		  
		  i = 1
		  valueLen = Len(value)
		  
		  While i <= valueLen
		    c = Mid(value, i, 1)
		    a = Asc(c)
		    If a = 9 Then
		      json = json + "\t"
		    ElseIf a = 10 Then
		      json = json + "\n"
		    ElseIf a = 13 Then
		      json = json + "\r"
		    ElseIf a >= 0 And a <= 31 Then
		      json = json + "\u" + Lowercase(Right("0000" + Hex(a), 4))
		    ElseIf a = 34 Then
		      json = json + "\"""
		    ElseIf a = 92 Then
		      json = json + "\\"
		    Else
		      json = json + c
		    End If
		    i = i + 1
		  Wend
		  
		  Return """" + json + """"
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function StringToMemory(Source As String) As MemoryBlock
		  
		  // Used for API calls.
		  // Allows us to give a string through a pointer address.
		  
		  Dim kMemory As New MemoryBlock(LenB(Source) + 1)
		  kMemory.StringValue(0, LenB(Source)) = Source
		  Return kMemory
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function TimeString(Period As UInt64, Fullstring As Boolean = False, ShortLegend As Boolean = False) As String
		  
		  Dim Buffer As String = ""
		  Dim Years, Days, Hours, Minutes, Seconds As UInt64
		  Dim LYear, LDay, LHour, LMinute, LSecond As String
		  
		  If ShortLegend = False Then
		    LYear = "year"
		    LDay = "day"
		    LHour = "hour"
		    LMinute = "minute"
		    LSecond = "second"
		  Else
		    LYear = "y"
		    LDay = "d"
		    LHour = "h"
		    LMinute = "m"
		    LSecond = "s"
		  End If
		  
		  // BEGIN CONVERSIONS
		  
		  // Period is in seconds:
		  Seconds = Period
		  
		  // 60 seconds in 1 minute:
		  Minutes = Seconds \ 60
		  Seconds = Seconds Mod 60
		  
		  // 60 minutes in 1 hour:
		  Hours = Minutes \ 60
		  Minutes = Minutes Mod 60
		  
		  // 24 hours in 1 day:
		  Days = Hours \ 24
		  Hours = Hours Mod 24
		  
		  // 365 days in 1 year:
		  Years = Days \ 365
		  Days = Days Mod 365
		  
		  // END CONVERSIONS
		  
		  If Fullstring = True Then
		    // Return something like "5 days, 0 hours, 1 minute, 13 seconds"
		    If ShortLegend = False Then
		      Buffer = Buffer + Str(Years) + " " + LYear
		      If Years <> 1 Then Buffer = Buffer + "s"
		      Buffer = Buffer + Str(Days) + " " + LDay
		      If Days <> 1 Then Buffer = Buffer + "s"
		      Buffer = Buffer + ", " + Str(Hours) + " " + LHour
		      If Hours <> 1 Then Buffer = Buffer + "s"
		      Buffer = Buffer + ", " + Str(Minutes) + " " + LMinute
		      If Minutes <> 1 Then Buffer = Buffer + "s"
		      Buffer = Buffer + ", " + Str(Seconds) + " " + LSecond
		      If Seconds <> 1 Then Buffer = Buffer + "s"
		    Else
		      Buffer = Buffer + Str(Years) + LYear + " " + Str(Days) + LDay + " " _
		      + Str(Hours) + LHour + " " + Str(Minutes) + LMinute + " " _
		      + Str(Seconds) + LSecond
		    End If
		    Return Buffer
		  End If
		  
		  // Return something like "5 days, 1 minute, 13 seconds"
		  
		  If Years <> 0 Then
		    If ShortLegend = False Then
		      Buffer = Buffer + ", " + Str(Years) + " " + LYear
		      If Years <> 1 Then Buffer = Buffer + "s"
		    Else
		      Buffer = Buffer + Str(Years) + LYear + " "
		    End If
		  End If
		  
		  If Days <> 0 Then
		    If ShortLegend = False Then
		      Buffer = Buffer + ", " + Str(Days) + " " + LDay
		      If Days <> 1 Then Buffer = Buffer + "s"
		    Else
		      Buffer = Buffer + Str(Days) + LDay + " "
		    End If
		  End If
		  
		  If Hours <> 0 Then
		    If ShortLegend = False Then
		      Buffer = Buffer + ", " + Str(Hours) + " " + LHour
		      If Hours <> 1 Then Buffer = Buffer + "s"
		    Else
		      Buffer = Buffer + Str(Hours) + LHour + " "
		    End If
		  End If
		  
		  If Minutes <> 0 Then
		    If ShortLegend = False Then
		      Buffer = Buffer + ", " + Str(Minutes) + " " + LMinute
		      If Minutes <> 1 Then Buffer = Buffer + "s"
		    Else
		      Buffer = Buffer + Str(Minutes) + LMinute + " "
		    End If
		  End If
		  
		  If Seconds <> 0 Then
		    If ShortLegend = False Then
		      Buffer = Buffer + ", " + Str(Seconds) + " " + LSecond
		      If Seconds <> 1 Then Buffer = Buffer + "s"
		    Else
		      Buffer = Buffer + Str(Seconds) + LSecond + " "
		    End If
		  End If
		  
		  If Seconds = 0 And Minutes = 0 And Hours = 0 And Days = 0 And Years = 0 Then
		    If ShortLegend = False Then
		      Buffer = ", 0 seconds"
		    Else
		      Buffer = "0s"
		    End If
		  End If
		  
		  If Left(Buffer, 2) = ", " Then
		    Return Mid(Buffer, 3)
		  ElseIf Right(Buffer, 1) = " " Then
		    Return Mid(Buffer, 1, Len(Buffer) - 1)
		  Else
		    Return Buffer
		  End If
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function TimezoneBias() As UInt32
		  
		  Dim objDate As New Date()
		  Return (0 - objDate.GMTOffset) * 60
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function WildcardMatch(Source As String, Target As String) As Boolean
		  
		  If Left(Source, Len(Target)) = Target Then Return True
		  If Right(Source, Len(Target)) = Target Then Return True
		  If InStr(Source, Target) > 0 Then Return True
		  
		  Return False
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function WildcardMatchVB(Source As String, Pattern As String) As Boolean
		  
		  // This will mimic the Visual Basic style of wildcard matching,
		  // which uses asterisks as the "wildcard" character.
		  //
		  // UNSUPPORTED PATTERN: *Some*thing*   (* >= 3)
		  
		  Dim Asterisk As String = "*"
		  
		  Dim sPattern As String = ReplaceAll(Pattern, Asterisk + Asterisk, Asterisk)
		  
		  Dim LPattern As String = Right(sPattern, Len(sPattern) - Len(Asterisk)) // *Some* -> Some*
		  Dim MPattern As String = Mid(sPattern, Len(Asterisk) + 1, Len(sPattern) - Len(Asterisk) * 2)
		  Dim RPattern As String = Mid(sPattern, Len(Asterisk) + 1)
		  
		  // Match left side    (Pattern:  *Some)
		  If Left(sPattern, Len(Asterisk)) = Asterisk And _   // YES Left-side
		    Right(sPattern, Len(Asterisk)) <> Asterisk And _ // NOT right-side
		    Right(Source, Len(RPattern)) = RPattern Then Return True
		    
		    // Match right side   (Pattern:  Some*)
		    If Left(sPattern, Len(Asterisk)) <> Asterisk And _  // NOT left-side
		      Right(sPattern, Len(Asterisk)) = Asterisk And _  // YES right-side
		      Left(Source, Len(LPattern)) = LPattern Then Return True
		      
		      // Match middle       (Pattern: *Some*)
		      If Left(sPattern, Len(Asterisk)) = Asterisk And _   // YES left-side
		        Right(sPattern, Len(Asterisk)) = Asterisk And _  // YES right-side
		        InStr(Source, MPattern) > 0 Then Return True
		        
		        // Match left + right (Pattern: Some*thing)
		        If Left(sPattern, Len(Asterisk)) <> Asterisk And _   // NOT left-side
		          Right(sPattern, Len(Asterisk)) <> Asterisk And _  // NOT right-side
		          CountFields(sPattern, Asterisk) = 2 And _         // Field1*Field2
		          Right(Source, Len(RPattern)) = RPattern And _    // *Field2
		          Left(Source, Len(LPattern)) = LPattern Then Return True
		          
		          Return False
		          
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function WindowsFileTimeToDate(LowValue As UInt32, HighValue As UInt32) As Date
		  
		  Dim FileTime As New MemoryBlock(8)
		  FileTime.UInt32Value(0) = LowValue
		  FileTime.UInt32Value(4) = HighValue
		  Return Globals.WindowsFileTimeToDate(FileTime.UInt64Value(0))
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function WindowsFileTimeToDate(Value As UInt64) As Date
		  
		  // Converts Battle.net FileTime structures (that are really
		  // Windows FILETIME structs) to native Date objects.
		  
		  Const WINDOWS_TICK = 10000000
		  
		  Dim timestamp As New Date()
		  
		  timestamp.Year   = 1601
		  timestamp.Month  = 1
		  timestamp.Day    = 1
		  timestamp.Hour   = 0
		  timestamp.Minute = 0
		  timestamp.Second = 0
		  
		  timestamp.TotalSeconds = timestamp.TotalSeconds + (Value / WINDOWS_TICK)
		  
		  Return timestamp
		  
		End Function
	#tag EndMethod


	#tag Property, Flags = &h1
		Protected BNETs() As BNETSocket
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected ClanMemberInfoCookies As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected ConfigWindow_Open As Boolean
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected Cookies As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected imgLagGreen As Picture
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected imgLagRed As Picture
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected imgLagYellow As Picture
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected imgNoUDP As Picture
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected ProfileDialogs() As ProfileWindow
	#tag EndProperty

	#tag Property, Flags = &h0
		TimeStarted As Double
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected UserIcons() As UserIcon
	#tag EndProperty


	#tag Constant, Name = ClanConfirmDisband, Type = Double, Dynamic = False, Default = \"1", Scope = Protected
	#tag EndConstant

	#tag Constant, Name = ClanConfirmLeave, Type = Double, Dynamic = False, Default = \"0", Scope = Protected
	#tag EndConstant


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
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
			Name="Super"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="TimeStarted"
			Group="Behavior"
			InitialValue="0"
			Type="Double"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
