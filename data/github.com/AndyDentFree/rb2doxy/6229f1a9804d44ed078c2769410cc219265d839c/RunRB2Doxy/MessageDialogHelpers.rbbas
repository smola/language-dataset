#tag Module
Module MessageDialogHelpers
	#tag Method, Flags = &h0
		Function ShowModalWith(extends dlg as MessageDialog, inMsg as string, actionBtn as string, cancelBtn as string, iconCode as integer = 1) As MessageDialogButton
		  dlg.icon = iconCode
		  dlg.ActionButton.Caption = actionBtn
		  dlg.CancelButton.Caption = cancelBtn
		  dlg.CancelButton.Visible = True
		  dlg.Message = inMsg
		  
		  return dlg.ShowModal()
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ShowModalWith(extends dlg as MessageDialog, inMsg as string, actionBtn as string, altActionBtn as string, cancelBtn as string, iconCode as integer = 1) As MessageDialogButton
		  dlg.icon = iconCode
		  dlg.ActionButton.Caption = actionBtn
		  dlg.AlternateActionButton.Caption = altActionBtn
		  dlg.CancelButton.Caption = cancelBtn
		  dlg.AlternateActionButton.Visible = True
		  dlg.CancelButton.Visible = True
		  dlg.Message = inMsg
		  
		  return dlg.ShowModal()
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ModalMessage(inMsg as string, actionBtn as string, cancelBtn as string, iconCode as integer = 1) As boolean
		  dim dlg as new MessageDialog
		  dlg.icon = iconCode
		  dlg.ActionButton.Caption = actionBtn
		  dlg.CancelButton.Caption = cancelBtn
		  dlg.CancelButton.Visible = True
		  dlg.Message = inMsg
		  
		  return dlg.ShowModal() = dlg.ActionButton
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub ModalMessage(inMsg as string, actionBtn as string = "OK", iconCode as integer = 1)
		  dim dlg as new MessageDialog
		  dlg.icon = iconCode
		  dlg.ActionButton.Caption = actionBtn
		  dlg.CancelButton.Visible = false
		  dlg.Message = inMsg
		  call dlg.ShowModal()
		End Sub
	#tag EndMethod


	#tag Note, Name = about
		shortcut methods to make it quick to compose & display a MessageDialog
	#tag EndNote


	#tag Constant, Name = cautionIcon, Type = Integer, Dynamic = False, Default = \"1", Scope = Public
	#tag EndConstant

	#tag Constant, Name = noMessageIcon, Type = Integer, Dynamic = False, Default = \"-1", Scope = Public
	#tag EndConstant

	#tag Constant, Name = noteIcon, Type = Integer, Dynamic = False, Default = \"0", Scope = Public
	#tag EndConstant

	#tag Constant, Name = stopIcon, Type = Integer, Dynamic = False, Default = \"2", Scope = Public
	#tag EndConstant

	#tag Constant, Name = questionIcon, Type = Integer, Dynamic = False, Default = \"3", Scope = Public
	#tag EndConstant

	#tag Constant, Name = buttonCancel, Type = Integer, Dynamic = False, Default = \"1", Scope = Public
	#tag EndConstant

	#tag Constant, Name = buttonDontSave, Type = Integer, Dynamic = False, Default = \"2", Scope = Public
	#tag EndConstant

	#tag Constant, Name = buttonOK, Type = Double, Dynamic = False, Default = \"0", Scope = Public
	#tag EndConstant


	#tag ViewBehavior
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			InheritedFrom="Object"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
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
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			InheritedFrom="Object"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
