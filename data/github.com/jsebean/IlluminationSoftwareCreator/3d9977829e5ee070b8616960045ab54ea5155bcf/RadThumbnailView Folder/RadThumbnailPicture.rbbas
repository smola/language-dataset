#tag Class
Protected Class RadThumbnailPicture
	#tag Method, Flags = &h21
		Private Sub GenerateThumbs()
		  'dim p as Picture
		  'dim h,w as Integer
		  'dim x,y as Integer
		  '
		  'if OriginalPicture <> nil then
		  '
		  'p = NewPicture(50,50,32)
		  '
		  'if OriginalPicture.Width > OriginalPicture.Height then
		  '
		  'w = 50
		  'h = OriginalPicture.Height * (50 / OriginalPicture.Width)
		  'x = 0
		  'y = (50 - h) / 2
		  '
		  'else
		  '
		  'h = 50
		  'w = OriginalPicture.Width * (50 / OriginalPicture.Height)
		  'x = (50 - w) / 2
		  'y = 0
		  '
		  'end if
		  '
		  'p.Mask.Graphics.ForeColor = rgb(255,255,255)
		  'p.Mask.Graphics.FillRect 0,0,p.Width, p.Height
		  '
		  'p.Graphics.DrawPicture OriginalPicture,x,y,w,h,0,0,OriginalPicture.Width, OriginalPicture.Height
		  '
		  'p.Mask.Graphics.ForeColor = rgb(0,0,0)
		  'p.Mask.Graphics.FillRect x,y,w,h
		  '
		  'Thumb50 = p
		  '
		  '
		  '
		  'p = NewPicture(100,100,32)
		  '
		  'if OriginalPicture.Width > OriginalPicture.Height then
		  '
		  'w = 100
		  'h = OriginalPicture.Height * (100 / OriginalPicture.Width)
		  'x = 0
		  'y = (100 - h) / 2
		  '
		  'else
		  '
		  'h = 100
		  'w = OriginalPicture.Width * (100 / OriginalPicture.Height)
		  'x = (100 - w) / 2
		  'y = 0
		  '
		  'end if
		  '
		  'p.Mask.Graphics.ForeColor = rgb(255,255,255)
		  'p.Mask.Graphics.FillRect 0,0,p.Width, p.Height
		  '
		  'p.Graphics.DrawPicture OriginalPicture,x,y,w,h,0,0,OriginalPicture.Width, OriginalPicture.Height
		  '
		  'p.Mask.Graphics.ForeColor = rgb(0,0,0)
		  'p.Mask.Graphics.FillRect x,y,w,h
		  '
		  '
		  'Thumb100 = p
		  '
		  '
		  'p = NewPicture(128,128,32)
		  '
		  'if OriginalPicture.Width > OriginalPicture.Height then
		  '
		  'w = 128
		  'h = OriginalPicture.Height * (128 / OriginalPicture.Width)
		  'x = 0
		  'y = (128 - h) / 2
		  '
		  'else
		  '
		  'h = 128
		  'w = OriginalPicture.Width * (128 / OriginalPicture.Height)
		  'x = (128 - w) / 2
		  'y = 0
		  '
		  'end if
		  '
		  'p.Mask.Graphics.ForeColor = rgb(255,255,255)
		  'p.Mask.Graphics.FillRect 0,0,p.Width, p.Height
		  '
		  'p.Graphics.DrawPicture OriginalPicture,x,y,w,h,0,0,OriginalPicture.Width, OriginalPicture.Height
		  '
		  'p.Mask.Graphics.ForeColor = rgb(0,0,0)
		  'p.Mask.Graphics.FillRect x,y,w,h
		  '
		  '
		  '
		  'Thumb128 = p
		  '
		  '
		  'p = NewPicture(150,150,32)
		  '
		  'if OriginalPicture.Width > OriginalPicture.Height then
		  '
		  'w = 150
		  'h = OriginalPicture.Height * (150 / OriginalPicture.Width)
		  'x = 0
		  'y = (150 - h) / 2
		  '
		  'else
		  '
		  'h = 150
		  'w = OriginalPicture.Width * (150 / OriginalPicture.Height)
		  'x = (150 - w) / 2
		  'y = 0
		  '
		  'end if
		  'p.Mask.Graphics.ForeColor = rgb(255,255,255)
		  'p.Mask.Graphics.FillRect 0,0,p.Width, p.Height
		  '
		  'p.Graphics.DrawPicture OriginalPicture,x,y,w,h,0,0,OriginalPicture.Width, OriginalPicture.Height
		  '
		  'p.Mask.Graphics.ForeColor = rgb(0,0,0)
		  'p.Mask.Graphics.FillRect x,y,w,h
		  '
		  'Thumb150 = p
		  '
		  '
		  'p = NewPicture(200,200,32)
		  '
		  'if OriginalPicture.Width > OriginalPicture.Height then
		  '
		  'w = 200
		  'h = OriginalPicture.Height * (200 / OriginalPicture.Width)
		  'x = 0
		  'y = (200 - h) / 2
		  '
		  'else
		  '
		  'h = 200
		  'w = OriginalPicture.Width * (200 / OriginalPicture.Height)
		  'x = (200 - w) / 2
		  'y = 0
		  '
		  'end if
		  '
		  'p.Mask.Graphics.ForeColor = rgb(255,255,255)
		  'p.Mask.Graphics.FillRect 0,0,p.Width, p.Height
		  '
		  'p.Graphics.DrawPicture OriginalPicture,x,y,w,h,0,0,OriginalPicture.Width, OriginalPicture.Height
		  '
		  'p.Mask.Graphics.ForeColor = rgb(0,0,0)
		  'p.Mask.Graphics.FillRect x,y,w,h
		  '
		  'Thumb200 = p
		  '
		  'end if
		End Sub
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetThumb(Zoom as integer) As picture
		  dim p as Picture
		  dim h,w as Integer
		  dim x,y as Integer
		  
		  if Zoom < 5 then
		    zoom = 5
		  end if
		  
		  if zoom > 200 then
		    Zoom = 200
		  end if
		  
		  
		  if OriginalPicture <> nil then
		    
		    p = NewPicture(zoom,zoom,32)
		    
		    if OriginalPicture.Width > OriginalPicture.Height then
		      
		      w = zoom
		      h = OriginalPicture.Height * (zoom / OriginalPicture.Width)
		      x = 0
		      y = (zoom - h) / 2
		      
		    else
		      
		      h = zoom
		      w = OriginalPicture.Width * (zoom / OriginalPicture.Height)
		      x = (zoom - w) / 2
		      y = 0
		      
		    end if
		    
		    p.Mask.Graphics.ForeColor = rgb(255,255,255)
		    p.Mask.Graphics.FillRect 0,0,p.Width, p.Height
		    
		    p.Graphics.DrawPicture OriginalPicture,x,y,w,h,0,0,OriginalPicture.Width, OriginalPicture.Height
		    
		    p.Mask.Graphics.ForeColor = rgb(0,0,0)
		    p.Mask.Graphics.FillRect x,y,w,h
		    
		    
		  end if
		  
		  Return p
		  
		  'Select case zoom
		  'case 50
		  'Return Thumb50
		  'case 100
		  'Return Thumb100
		  'case 128
		  'Return Thumb128
		  'case 150
		  'Return Thumb150
		  'case 200
		  'Return Thumb200
		  '
		  'end Select
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		Name As string
	#tag EndProperty

	#tag Property, Flags = &h21
		Private OriginalPicture As picture
	#tag EndProperty

	#tag ComputedProperty, Flags = &h0
		#tag Getter
			Get
			  Return OriginalPicture
			End Get
		#tag EndGetter
		#tag Setter
			Set
			  dim p,p2 as Picture
			  dim w, h as Integer
			  dim pw, ph as Integer
			  
			  p2 = value
			  
			  pw = p2.Width
			  ph = p2.Height
			  
			  if pw < 201 and ph < 201 then
			    
			    p = p2
			    
			  else
			    
			    if pw > pw then
			      
			      w = 200
			      h =  (w / pw)  * ph
			      
			    else
			      
			      h = 200
			      w =  (h / ph)  * pw
			      
			    end if
			    
			    p = NewPicture(w,h,32)
			    
			    p.Graphics.DrawPicture p2,0,0,w,h,0,0,pw, ph
			    
			  end if
			  
			  OriginalPicture = p
			  
			  'GenerateThumbs
			End Set
		#tag EndSetter
		Picture As Picture
	#tag EndComputedProperty

	#tag Property, Flags = &h0
		Tag() As variant
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="Index"
			Visible=true
			Group="ID"
			InitialValue="2147483648"
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
			Type="string"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Picture"
			Group="Behavior"
			InitialValue="0"
			Type="Picture"
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
