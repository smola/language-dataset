'The circular progressBar works by displaying 4 masked quadrants and rotating them to the proper orientation.

Import mojo
Import ui

Class CircularProgressBar
  Private
	Field quad:Image[4]     'Quadrant image

  Public	
	Field r:Float          'Radius
	Field CloverLeaf:Bool  'Display as cloverleaf instead of full circle
	
	Field Percent:Float    'Amount full to display this progress bar
	
	Method New(img:Image, CloverLeaf:Bool = False)
		Self.CloverLeaf = CloverLeaf
		Self.r = img.Width / 2
		SetImages(img)
	End Method
	
	Method SetImages(img:Image)
		quad[0] = img.GrabImage(0, r, r, r)
		quad[1] = img.GrabImage(0, 0, r, r)
		quad[2] = img.GrabImage(r, 0, r, r)
		quad[3] = img.GrabImage(r, r, r, r)
		
		'Set every corner to virtual center.
		quad[0].SetHandle(r, 0)
		quad[1].SetHandle(r, r)
		quad[2].SetHandle(0, r)
		quad[3].SetHandle(0, 0)
	End Method
	
	Method Render:Void(x:Float, y:Float, scl:Float = 1.0)
		If CloverLeaf
			DrawQuad(0, x, y, Percent, scl)
			DrawQuad(1, x, y, Percent, scl)
			DrawQuad(2, x, y, Percent, scl)
			DrawQuad(3, x, y, Percent, scl)
		Else
		
			Local quadrant:Int = Min(3.0, Percent * 4)  'returns 0-3
			Select quadrant
			 Case 0
			 	DrawQuad(0, x, y, Percent / 0.25, scl)
			 Case 1
				DrawQuad(0, x, y, scl)
			 	DrawQuad(1, x, y, (Percent - 0.25) / 0.25, scl)
			 Case 2
				DrawQuad(0, x, y, scl)
				DrawQuad(1, x, y, scl)
			 	DrawQuad(2, x, y, (Percent - 0.5) / 0.25, scl)
			 Case 3
				DrawQuad(0, x, y, scl)
				DrawQuad(1, x, y, scl)
				DrawQuad(2, x, y, scl)
			 	DrawQuad(3, x, y, (Percent - 0.75) / 0.25, scl)
			End Select
		
		
		End If
	End Method
	
	'Summary:  Draw full quadrants
	Method DrawQuad:Void(quadrant:Int, x:Float, y:Float, scl:Float)
		DrawImage(quad[quadrant], x, y, 0, scl, scl)
	End Method

	'Summary:  Draw partial quadrant
	Method DrawQuad(quadrant:Int, x:Float, y:Float, Percent:Float, scl:Float)
		UI.PushScissor()
		Select quadrant
			Case 0
				xfScissor(x - r, y, r, r) 'LL Quadrant 0
			Case 1
				xfScissor(x - r, y - r, r, r) 'UL Quadrant 1
			Case 2
				xfScissor(x, y - r, r, r) 'UR Quadrant 2
			Case 3
				xfScissor(x, y, r, r) 'LR Quadrant 3
		End Select
			
			DrawImage(quad[quadrant], x, y, 90 - (90 * Percent), scl, scl)
			UI.PopScissor()
	End Method
		
	'Summary:  Transformed scissor
	Function xfScissor:Void(x:Float, y:Float, w:Float, h:Float)
		Local matrix:Float[] = GetMatrix()
		SetScissor(x * matrix[0] + matrix[4], y * matrix[3] + matrix[5], w * matrix[0], h * matrix[3])
	End Function	
End Class
