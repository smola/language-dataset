#tag Module
Protected Module MongoDriver
	#tag Constant, Name = OP_DELETE, Type = Double, Dynamic = False, Default = \"2006", Scope = Public
	#tag EndConstant

	#tag Constant, Name = OP_GET_MORE, Type = Double, Dynamic = False, Default = \"2005", Scope = Public
	#tag EndConstant

	#tag Constant, Name = OP_INSERT, Type = Double, Dynamic = False, Default = \"2002", Scope = Public
	#tag EndConstant

	#tag Constant, Name = OP_KILL_CURSORS, Type = Double, Dynamic = False, Default = \"2007", Scope = Public
	#tag EndConstant

	#tag Constant, Name = OP_QUERY, Type = Double, Dynamic = False, Default = \"2004", Scope = Public
	#tag EndConstant

	#tag Constant, Name = OP_REPLY, Type = Double, Dynamic = False, Default = \"1", Scope = Public
	#tag EndConstant

	#tag Constant, Name = OP_UPDATE, Type = Double, Dynamic = False, Default = \"2001", Scope = Public
	#tag EndConstant

	#tag Constant, Name = TCP_TIMEOUT, Type = Double, Dynamic = False, Default = \"5000", Scope = Private
	#tag EndConstant


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
