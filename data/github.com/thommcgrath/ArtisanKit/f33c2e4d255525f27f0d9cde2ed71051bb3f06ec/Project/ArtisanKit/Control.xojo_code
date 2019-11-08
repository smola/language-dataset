#tag Class
Protected Class Control
Inherits Canvas
	#tag Event
		Sub Activated()
		  RaiseEvent Activated
		  Self.Invalidate
		End Sub
	#tag EndEvent

	#tag Event
		Sub Deactivated()
		  RaiseEvent Deactivated
		  Self.Invalidate
		End Sub
	#tag EndEvent

	#tag Event
		Sub GotFocus()
		  Self.mHasFocus = True
		  RaiseEvent GotFocus
		  Self.Invalidate
		End Sub
	#tag EndEvent

	#tag Event
		Sub LostFocus()
		  Self.mHasFocus = False
		  RaiseEvent LostFocus
		  Self.Invalidate
		End Sub
	#tag EndEvent

	#tag Event
		Function MouseWheel(X As Integer, Y As Integer, deltaX as Integer, deltaY as Integer) As Boolean
		  Var WheelData As New ArtisanKit.ScrollEvent(Self.ScrollSpeed, DeltaX, DeltaY)
		  Return MouseWheel(X, Y, WheelData.ScrollX, WheelData.ScrollY, WheelData)
		End Function
	#tag EndEvent

	#tag Event
		Sub Opening()
		  Self.NeedsFullKeyboardAccessForFocus = True
		  RaiseEvent Opening
		  Self.AllowFocus = Self.AllowFocus And (ArtisanKit.FullKeyboardAccessEnabled Or Self.NeedsFullKeyboardAccessForFocus = False)
		End Sub
	#tag EndEvent

	#tag Event
		Sub Paint(g As Graphics, areas() As REALbasic.Rect)
		  If Self.mLastPaintWidth <> G.Width Or Self.mLastPaintHeight <> G.Height Then
		    RaiseEvent Resized
		    Self.mLastPaintWidth = G.Width
		    Self.mLastPaintHeight = G.Height
		  End If
		  
		  G.ClearRect(0, 0, G.Width, G.Height)
		  
		  Var Highlighted As Boolean
		  If Self.Enabled Then
		    #if TargetCocoa
		      Declare Function IsMainWindow Lib "Cocoa.framework" Selector "isMainWindow" (Target As Integer) As Boolean
		      Declare Function IsKeyWindow Lib "Cocoa.framework" Selector "isKeyWindow" (Target As Integer) As Boolean
		      Highlighted = IsKeyWindow(Self.TrueWindow.Handle) Or IsMainWindow(Self.TrueWindow.Handle)
		    #else
		      Highlighted = True
		    #endif
		  End If
		  
		  Var ConvertedAreas() As Xojo.Rect
		  ConvertedAreas.ResizeTo(Areas.LastRowIndex)
		  For I As Integer = 0 To Areas.LastRowIndex
		    ConvertedAreas(I) = Areas(I)
		  Next
		  
		  RaiseEvent Paint(G, ConvertedAreas, Highlighted)
		  Self.mInvalidated = False
		End Sub
	#tag EndEvent


	#tag Method, Flags = &h21
		Private Sub AnimationTimerAction(Sender As Timer)
		  #pragma Unused Sender
		  
		  For I As Integer = Self.mAnimations.KeyCount - 1 DownTo 0
		    Var Key As String = Self.mAnimations.Key(I)
		    Var Details As AnimationDetails = Self.mAnimations.Value(Key)
		    Var Elapsed As Double = (System.Microseconds - Details.StartTime) / 1000000
		    Var ChangeInValue As Double = Details.EndValue - Details.StartValue
		    Var Value As Double
		    If Elapsed >= Details.Duration Then
		      // Finished
		      Value = Details.EndValue
		    Else
		      If Details.Ease Then
		        Value = (ChangeInValue * Sin((Elapsed / Details.Duration) * (Self.PI / 2))) + Details.StartValue
		      Else
		        Value = (ChangeInValue * (Elapsed / Details.Duration)) + Details.StartValue
		      End If
		      If Details.EndValue > Details.StartValue Then
		        Value = Min(Value, Details.EndValue)
		      ElseIf Details.EndValue < Details.StartValue Then
		        Value = Max(Value, Details.EndValue)
		      Else
		        Value = Details.StartValue
		      End If
		    End If
		    If Value = Details.EndValue Then
		      Self.mAnimations.Remove(Key)
		    End If
		    RaiseEvent AnimationStep(Key, Value, Value = Details.EndValue)
		    Self.Invalidate
		  Next
		  If Self.mAnimations.KeyCount = 0 Then
		    RemoveHandler mAnimationTimer.Run, WeakAddressOf AnimationTimerAction
		    Self.mAnimationTimer.RunMode = Timer.RunModes.Off
		    Self.mAnimationTimer = Nil
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Attributes( Deprecated = "ArtisanKit.BeginFocusRing" ) Protected Sub BeginFocusRing(G As Graphics)
		  #Pragma Unused G
		  ArtisanKit.BeginFocusRing()
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub CancelAnimation(Key As String, Finish As Boolean = False)
		  If Self.mAnimations = Nil Or Self.mAnimations.HasKey(Key) = False Then
		    Return
		  End If
		  
		  Var Details As AnimationDetails = Self.mAnimations.Value(Key)
		  Self.mAnimations.Remove(Key)
		  If Finish Then
		    RaiseEvent AnimationStep(Key, Details.EndValue, True)
		    Self.Invalidate
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h1
		Attributes( Deprecated = "ArtisanKit.EndFocusRing" ) Protected Sub EndFocusRing(G As Graphics)
		  #Pragma Unused G
		  ArtisanKit.EndFocusRing()
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Invalidate(EraseBackground As Boolean = True)
		  If Not Self.mInvalidated Then
		    Super.Invalidate(EraseBackground)
		    Self.mInvalidated = True
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Invalidate(X As Integer, Y As Integer, Width As Integer, Height As Integer, EraseBackground As Boolean = True)
		  If Not Self.mInvalidated Then
		    Super.Invalidate(X, Y, Width, Height, EraseBackground)
		    Self.mInvalidated = True
		  End If
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Render(Width As Integer, Height As Integer, Highlighted As Boolean = True, MinScale As Double = 1.0, MaxScale As Double = 3.0) As Picture
		  Var Areas(0) As Xojo.Rect
		  Areas(0) = New Xojo.Rect(0, 0, Width, Height)
		  
		  Var Bitmaps() As Picture
		  For Factor As Double = MinScale To MaxScale
		    Var Pic As New Picture(Width, Height)
		    Pic.HorizontalResolution = 72 * Factor
		    Pic.VerticalResolution = 72 * Factor
		    Pic.Graphics.ScaleX = Factor
		    Pic.Graphics.ScaleY = Factor
		    RaiseEvent Paint(Pic.Graphics, Areas, Highlighted)
		    Bitmaps.AddRow(Pic)
		  Next
		  
		  Return New Picture(Width, Height, Bitmaps)
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Sub StartAnimation(Key As String, StartValue As Double, EndValue As Double, Duration As Double, Ease As Boolean = True)
		  If Self.mAnimations = Nil Then
		    Self.mAnimations = New Dictionary
		  End If
		  
		  Var Details As AnimationDetails
		  Details.StartValue = StartValue
		  Details.EndValue = EndValue
		  Details.StartTime = System.Microseconds
		  Details.Duration = Duration
		  Details.Ease = Ease
		  
		  Self.mAnimations.Value(Key) = Details
		  
		  If Self.mAnimationTimer = Nil Then
		    Var AnimTimer As New Timer
		    AnimTimer.RunMode = Timer.RunModes.Multiple
		    AnimTimer.Period = 16
		    AddHandler AnimTimer.Run, WeakAddressOf AnimationTimerAction
		    Self.mAnimationTimer = AnimTimer
		  End If
		End Sub
	#tag EndMethod


	#tag Hook, Flags = &h0
		Event Activated()
	#tag EndHook

	#tag Hook, Flags = &h0
		Event AnimationStep(Key As String, Value As Double, Finished As Boolean)
	#tag EndHook

	#tag Hook, Flags = &h0
		Event Deactivated()
	#tag EndHook

	#tag Hook, Flags = &h0
		Event GotFocus()
	#tag EndHook

	#tag Hook, Flags = &h0
		Event LostFocus()
	#tag EndHook

	#tag Hook, Flags = &h0
		Event MouseWheel(MouseX As Integer, MouseY As Integer, PixelsX As Integer, PixelsY As Integer, WheelData As ArtisanKit.ScrollEvent) As Boolean
	#tag EndHook

	#tag Hook, Flags = &h0
		Event Opening()
	#tag EndHook

	#tag Hook, Flags = &h0
		Event Paint(G As Graphics, Areas() As Xojo.Rect, Highlighted As Boolean)
	#tag EndHook

	#tag Hook, Flags = &h0
		Event Resized()
	#tag EndHook


	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return Self.mHasFocus
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  If Self.mHasFocus <> Value Then
			    If Value Then
			      Self.SetFocus()
			    Else
			      Self.TrueWindow.SetFocus()
			    End If
			  End If
			End Set
		#tag EndSetter
		HasFocus As Boolean
	#tag EndComputedProperty

	#tag Property, Flags = &h21
		Private mAnimations As Dictionary
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mAnimationTimer As Timer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mHasFocus As Boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mInvalidated As Boolean
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mLastPaintHeight As Integer
	#tag EndProperty

	#tag Property, Flags = &h21
		Private mLastPaintWidth As Integer
	#tag EndProperty

	#tag Property, Flags = &h0
		NeedsFullKeyboardAccessForFocus As Boolean
	#tag EndProperty

	#tag Property, Flags = &h0
		ScrollSpeed As Integer = 20
	#tag EndProperty


	#tag Constant, Name = PI, Type = Double, Dynamic = False, Default = \"3.14159265358979323846264338327950", Scope = Private
	#tag EndConstant


	#tag Structure, Name = AnimationDetails, Flags = &h21
		StartValue As Double
		  EndValue As Double
		  StartTime As Double
		  Duration As Double
		Ease As Boolean
	#tag EndStructure


	#tag ViewBehavior
		#tag ViewProperty
			Name="AllowAutoDeactivate"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Tooltip"
			Visible=true
			Group="Appearance"
			InitialValue=""
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowFocusRing"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowFocus"
			Visible=true
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="AllowTabs"
			Visible=true
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Backdrop"
			Visible=false
			Group="Appearance"
			InitialValue=""
			Type="Picture"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="DoubleBuffer"
			Visible=true
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Enabled"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="HasFocus"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Height"
			Visible=true
			Group="Position"
			InitialValue="100"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="InitialParent"
			Visible=false
			Group=""
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockBottom"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockLeft"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockRight"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="LockTop"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Name"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="NeedsFullKeyboardAccessForFocus"
			Visible=false
			Group="Behavior"
			InitialValue=""
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="ScrollSpeed"
			Visible=false
			Group="Behavior"
			InitialValue="20"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Super"
			Visible=true
			Group="ID"
			InitialValue=""
			Type="String"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TabIndex"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TabPanelIndex"
			Visible=false
			Group="Position"
			InitialValue="0"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="TabStop"
			Visible=true
			Group="Position"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue=""
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Transparent"
			Visible=true
			Group="Behavior"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Visible"
			Visible=true
			Group="Appearance"
			InitialValue="True"
			Type="Boolean"
			EditorType=""
		#tag EndViewProperty
		#tag ViewProperty
			Name="Width"
			Visible=true
			Group="Position"
			InitialValue="100"
			Type="Integer"
			EditorType=""
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
