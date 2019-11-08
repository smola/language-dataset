#tag Class
Protected Class RLEStream
Implements Readable,Writeable
	#tag Method, Flags = &h0
		Sub Close()
		  IOStream.Close
		  IOStream = Nil
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(Stream As BinaryStream)
		  IOStream = Stream
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(mb As MemoryBlock)
		  IOStream = New BinaryStream(mb)
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function Create(f As FolderItem, OverWrite As Boolean = False) As RLEStream
		  Dim bs As BinaryStream = BinaryStream.Create(f, OverWrite)
		  Return New RLEStream(bs)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Destructor()
		  If IOStream <> Nil Then IOStream.Close
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function EOF() As Boolean
		  // Part of the Readable interface.
		  Return IOStream.EOF
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Flush()
		  // Part of the Writeable interface.
		  If Runcount > 1 Then
		    IOStream.Write(Str(Runcount) + RunChar)
		  ElseIf RunChar <> "" Then
		    IOStream.Write(RunChar)
		  End If
		  IOStream.Flush
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Length() As Integer
		  Return IOStream.Length
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function Open(f As FolderItem, ReadWrite As Boolean = False) As RLEStream
		  Dim bs As BinaryStream = BinaryStream.Open(f, ReadWrite)
		  Return New RLEStream(bs)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Position() As Integer
		  Return IOStream.Position
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Read(Count As Integer, encoding As TextEncoding = Nil) As String
		  // Part of the Readable interface.
		  If RawIO Then Return IOStream.Read(Count, encoding)
		  Dim ret As String
		  Dim rcount As String
		  While Not IOStream.EOF And ret.Len < Count
		    If Runcount = 0 Then
		      Do
		        Dim m As String = IOStream.Read(1)
		        Select Case m
		        Case "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
		          rcount = rcount + m
		        Case Else
		          If rcount.Trim = "" Then rcount = "1"
		          Runcount = Val(rcount)
		          RunChar = m
		          rcount = ""
		          Exit Do
		        End Select
		      Loop
		    Else
		      For i As Integer = 0 To Count - 1
		        ret = ret + RunChar
		        Runcount = Runcount - 1
		        If Runcount < 1 Then
		          Runcount = 0
		          RunChar = ""
		          Exit For
		        End If
		      Next
		    End If
		  Wend
		  
		  Return DefineEncoding(ret, encoding)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ReadError() As Boolean
		  // Part of the Readable interface.
		  Return IOStream.ReadError
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Write(text As String)
		  // Part of the Writeable interface.
		  If RawIO Then
		    IOStream.Write(text)
		    Return
		  End If
		  Dim data As MemoryBlock = text
		  Dim sz As Integer = Data.Size - 1
		  If RunChar = "" Then RunChar = Data.StringValue(0, 1)
		  For i As Integer = 0 To sz
		    Dim char As String = Data.StringValue(i, 1)
		    If char <> RunChar Then
		      If Runcount > 1 Then
		        IOStream.Write(Str(Runcount) + RunChar)
		      Else
		        IOStream.Write(RunChar)
		      End If
		      RunChar = char
		      Runcount = 1
		    Else
		      Runcount = Runcount + 1
		    End If
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function WriteError() As Boolean
		  // Part of the Writeable interface.
		  Return IOStream.WriteError
		End Function
	#tag EndMethod


	#tag Property, Flags = &h1
		Protected IOStream As BinaryStream
	#tag EndProperty

	#tag Property, Flags = &h0
		RawIO As Boolean = False
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected RunChar As String
	#tag EndProperty

	#tag Property, Flags = &h1
		Protected Runcount As Integer = 0
	#tag EndProperty


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
			Name="RawIO"
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
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
	#tag EndViewBehavior
End Class
#tag EndClass
