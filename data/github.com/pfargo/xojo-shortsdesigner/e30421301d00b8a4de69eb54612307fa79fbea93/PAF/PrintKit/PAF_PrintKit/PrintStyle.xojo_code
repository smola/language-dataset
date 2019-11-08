#tag Class
Protected Class PrintStyle
	#tag Method, Flags = &h0
		Function CompareAllButName(otherStyle As PAF_PrintKit.PrintStyle) As Boolean
		  
		  if otherStyle.Bold <> bold then
		    Return false
		  end if
		  if otherStyle.CornerHeight <> CornerHeight then
		    Return false
		  end if
		  if otherStyle.CornerWidth <> CornerWidth then
		    Return false
		  end if
		  if otherStyle.FillColor <> FillColor then
		    Return false
		  end if
		  if otherStyle.Italic <> Italic then
		    Return false
		  end if
		  if otherStyle.LineColor <> LineColor then
		    Return false
		  end if
		  if otherStyle.LineWidth <> LineWidth then
		    Return false
		  end if
		  if otherStyle.TextAlign <> TextAlign then
		    Return false
		  end if
		  if otherStyle.TextColor <> TextColor then
		    Return false
		  end if
		  if otherStyle.TextFont <> TextFont then
		    Return false
		  end if
		  if otherStyle.TextSize <> TextSize then
		    Return false
		  end if
		  if otherStyle.Underline <> Underline then
		    Return false
		  end if
		  if otherStyle.Type <> Type then
		    Return false
		  end if
		  
		  Return True
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor()
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(jStyle as JSONItem)
		  
		  dim typ as Introspection.TypeInfo
		  dim prop, Props() as Introspection.PropertyInfo
		  
		  ' Get the object type
		  typ = Introspection.GetType(me)
		  
		  ' Get the object properties
		  Props = typ.GetProperties
		  
		  for each prop in Props
		    
		    ' We only load primitive properties
		    if prop.PropertyType.isPrimitive then
		      
		      if jStyle.Lookup(prop.name,"") <> "" then
		        prop.Value(me) = jStyle.Value(prop.name)
		      end if
		      
		    end if
		    
		  Next
		  
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Sub Constructor(d As xojo.Core.Dictionary)
		  
		  dim a As Auto = d.Lookup("Bold",False)
		  
		  Dim info As Xojo.Introspection.TypeInfo
		  info = Xojo.Introspection.GetType(a)
		  
		  Bold = d.Lookup("Bold",False)
		  CornerHeight = d.Lookup("CornerHeight",0.0)
		  CornerWidth = d.Lookup("CornerWidth",0.0)
		  FillColor = d.Lookup("FillColor",&cFFFFFF00)
		  Italic = d.Lookup("Italic",False)
		  LineColor = d.Lookup("LineColor",&c00000000)
		  LineWidth = d.Lookup("LineWidth",1.0)
		  name = d.Lookup("name","")
		  TextAlign = d.Lookup("TextAlign","Left")
		  TextColor = d.Lookup("TextColor",&c00000000)
		  TextFont = d.Lookup("TextFont","Arial")
		  TextSize = d.Lookup("TextSize",12)
		  Underline = d.Lookup("Underline",false)
		  Type = d.Lookup("Type","")
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function Draw() As Picture
		  dim g as Graphics = ScratchGraphics
		  
		  g.Bold = Bold
		  g.Italic = Italic
		  g.ForeColor = TextColor
		  g.TextFont = TextFont
		  g.TextSize = TextSize
		  g.Underline = Underline
		  
		  dim dWidth, dHeight as Double
		  
		  dWidth = g.StringWidth(Name)
		  dHeight = g.StringHeight(Name,1000)
		  
		  if dWidth > 0 and dHeight > 0 then
		    
		    dim p as new Picture(dWidth,dHeight)
		    
		    if FillColor <> &cFFFFFFFF and FillColor <> &cFFFFFF00 then
		      p.Graphics.ForeColor = FillColor
		      p.Graphics.FillRect 0,0,p.Width,p.Height
		    end if
		    
		    p.Graphics.Bold = Bold
		    p.Graphics.Italic = Italic
		    p.Graphics.ForeColor = TextColor
		    p.Graphics.TextFont = TextFont
		    p.Graphics.TextSize = TextSize
		    p.Graphics.Underline = Underline
		    
		    p.Graphics.DrawString(name,0,g.TextAscent)
		    
		    Return p
		    
		  else
		    
		    Return nil
		    
		  end if
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		 Shared Function SerializeJSON(ps as PAF_PrintKit.PrintStyle) As JSONItem
		  dim prop, props() as Introspection.PropertyInfo
		  
		  props = Introspection.GetType(ps).GetProperties
		  
		  dim jsonOut as new JSONItem
		  
		  for each prop in props
		    
		    jsonOut.Value(prop.Name) = prop.Value(ps).StringValue
		    
		  next
		  
		  Return jsonOut
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		Bold As Boolean = false
	#tag EndProperty

	#tag Property, Flags = &h0
		CornerHeight As Double = 0.0
	#tag EndProperty

	#tag Property, Flags = &h0
		CornerWidth As Double = 0.0
	#tag EndProperty

	#tag Property, Flags = &h0
		FillColor As Color = &cFFFFFF00
	#tag EndProperty

	#tag Property, Flags = &h0
		Italic As Boolean = false
	#tag EndProperty

	#tag Property, Flags = &h0
		LineColor As Color = &c00000000
	#tag EndProperty

	#tag Property, Flags = &h0
		LineWidth As Double = 1.0
	#tag EndProperty

	#tag Property, Flags = &h0
		name As String
	#tag EndProperty

	#tag Property, Flags = &h0
		TextAlign As String = "Left"
	#tag EndProperty

	#tag Property, Flags = &h0
		TextColor As Color = &c00000000
	#tag EndProperty

	#tag Property, Flags = &h0
		TextFont As String = "Arial"
	#tag EndProperty

	#tag Property, Flags = &h0
		TextSize As Integer = 12
	#tag EndProperty

	#tag Property, Flags = &h0
		Type As String
	#tag EndProperty

	#tag Property, Flags = &h0
		Underline As Boolean = false
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Bold"
			Group="Behavior"
			InitialValue="false"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="CornerHeight"
			Group="Behavior"
			InitialValue="0.0"
			Type="Double"
		#tag EndViewProperty
		#tag ViewProperty
			Name="CornerWidth"
			Group="Behavior"
			InitialValue="0.0"
			Type="Double"
		#tag EndViewProperty
		#tag ViewProperty
			Name="FillColor"
			Group="Behavior"
			InitialValue="&cFFFFFFFF"
			Type="Color"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="-2147483648"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Italic"
			Group="Behavior"
			InitialValue="false"
			Type="Boolean"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Left"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="LineColor"
			Group="Behavior"
			InitialValue="&c000000"
			Type="Color"
		#tag EndViewProperty
		#tag ViewProperty
			Name="LineWidth"
			Group="Behavior"
			InitialValue="1.0"
			Type="Double"
		#tag EndViewProperty
		#tag ViewProperty
			Name="name"
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
			Name="TextAlign"
			Group="Behavior"
			InitialValue="Left"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="TextColor"
			Group="Behavior"
			InitialValue="&c00000000"
			Type="Color"
		#tag EndViewProperty
		#tag ViewProperty
			Name="TextFont"
			Group="Behavior"
			InitialValue="Arial"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="TextSize"
			Group="Behavior"
			InitialValue="12"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Type"
			Group="Behavior"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Underline"
			Group="Behavior"
			InitialValue="false"
			Type="Boolean"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
