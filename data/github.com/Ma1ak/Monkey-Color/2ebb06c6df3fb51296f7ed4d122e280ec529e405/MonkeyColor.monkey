''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'Who: Aman Alshurafa
'When: Don't remember. It's all foggy
'What: Color class to let you declare and play with colors
'Why: To make life a little bit easier. 
'Where: On my broken laptop if you're gonna buy me a new one.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Import mojo

Class Color
Private
	Global PrimaryColor:Color = New Color(255, 0, 0)
Public
'	Const BLACK:Color = New Color(0, 0, 0)
'	Const WHITE:Color = New Color(255, 255, 255)
'	Const RED:Color = New Color(255, 0, 0)
'	Const LIME:Color = New Color(0, 255, 0)
'	Const BLUE:Color = New Color(0, 0, 255)
'	Const YELLOW:Color = New Color(255, 255, 0)
'	Const CYAN:Color = New Color(0, 255, 255)
'	Const MAGENTA:Color = New Color(255, 0, 255)
'	Const SILVER:Color = New Color(192, 192, 192)
'	Const GRAY:Color = New Color(128, 128, 128)
'	Const MAROON:Color = New Color(128, 0, 0)
'	Const OLIVE:Color = New Color(128, 128, 0)
'	Const GREEN:Color = New Color(0, 128, 0)
'	Const PURPLE:Color = New Color(128, 0, 128)
'	Const TEAL:Color = New Color(0, 128, 128)
'	Const NAVY:Color = New Color(0, 0, 128)
	Global ColorPicker:Color = New Color(0, 0, 0)
    Global GrayShades:Image
	Global ColorShades:Image
	Global MainColors:Image
	Field r:Int
	Field g:Int
	Field b:Int
	
	'Summary: Create a new color (Black by default)
	Method New()
		Self.r = 0
		Self.g = 0
		Self.b = 0
	End
	
	'Summary: Create a new rgb color
	Method New(r:Int, g:Int, b:Int)
		Self.r = r
		Self.g = g
		Self.b = b
	End
	
	'Summary: Draws the color picker tool composed of a [255 x 255] shades sqaure and a [p_size x 255] or [255 x p_size] depeding on p_horizontal.
	Function DrawColorPicker(x:Int, y:Int, p_horizontal:Bool = False, gap:Int = 0, p_size:Int = 50)
		If Not GrayShades Or Not ColorShades
			For Local i:= 0 Until 256
				SetColor(255 - i, 255 - i, 255 - i)
				DrawLine(x, y + i, x + 256, y + i)
			Next
			Local mask:Int[] = New Int[255 * 255]
			If Not (x < 0 Or y < 0 Or x + 255 > DeviceWidth() Or y + 255 > DeviceHeight())
				ReadPixels(mask, x, y, 255, 255)
				GrayShades = CreateImage(255, 255)
				GrayShades.WritePixels(mask, 0, 0, 255, 255)
			EndIf
			For Local i:Int = 0 Until 256
				For Local j:Int = 0 Until 256
					SetColor Min(Min(j, i), 255), Min(Min(j, i), 255), Min(Min(j, i), 255)
					DrawPoint(x + j, y + 255 - i)
				Next
			Next
			If Not (x < 0 Or y < 0 Or x + 255 > DeviceWidth() Or y + 255 > DeviceHeight())
				ReadPixels(mask, x, y, 255, 255)
				For Local i:Int = 0 Until mask.Length
					mask[i] = $00FFFFFF + ( (mask[i] & $FF0000) Shl 8)
				Next
				ColorShades = CreateImage(255, 255)
				ColorShades.WritePixels(mask, 0, 0, 255, 255)
			EndIf
		Else
			SetColor(255, 255, 255)
			DrawImage(GrayShades, x, y)
			PrimaryColor.setColor
			DrawImage(ColorShades, x, y)
		EndIf
		Local cx = x
		Local cy = y
		If Not MainColors
			SetColor(0, 0, 0)
			If p_horizontal
				cx += 1
				cy += gap + 255
				DrawRect(cx - 1, cy, 256, p_size)
			Else
				cx += gap + 255
				DrawRect(cx, cy - 1, p_size, 256)
			EndIf
			cx += 2
			cy += 2
			p_size -= 4
			For Local i:Int = 0 Until 43
				If p_horizontal
					SetColor(255, 0, i * 6)
					DrawLine(cx + i, cy, cx + i, cy + p_size)
					SetColor(255 - i * 6, 0, 255)
					If i > 0 Then DrawLine(cx + i + 42, cy, cx + i + 42, cy + p_size)
					SetColor(0, i * 6, 255)
					If i > 0 Then DrawLine(cx + i + 84, cy, cx + i + 84, cy + p_size)
					SetColor(0, 255, 255 - i * 6)
					If i > 0 Then DrawLine(cx + i + 126, cy, cx + i + 126, cy + p_size)
					SetColor(i * 6, 255, 0)
					If i > 0 Then DrawLine(cx + i + 168, cy, cx + i + 168, cy + p_size)
					SetColor(255, 255 - i * 6, 0)
					If i > 0 And i < 42 Then DrawLine(cx + i + 210, cy, cx + i + 210, cy + p_size)
				Else
					SetColor(255, 0, i * 6)
					DrawLine(cx, cy + i, cx + p_size, cy + i)
					SetColor(255 - i * 6, 0, 255)
					If i > 0 Then DrawLine(cx, cy + i + 42, cx + p_size, cy + i + 42)
					SetColor(0, i * 6, 255)
					If i > 0 Then DrawLine(cx, cy + i + 84, cx + p_size, cy + i + 84)
					SetColor(0, 255, 255 - i * 6)
					If i > 0 Then DrawLine(cx, cy + i + 126, cx + p_size, cy + i + 126)
					SetColor(i * 6, 255, 0)
					If i > 0 Then DrawLine(cx, cy + i + 168, cx + p_size, cy + i + 168)
					SetColor(255, 255 - i * 6, 0)
					If i > 0 And i < 42 Then DrawLine(cx, cy + i + 210, cx + p_size, cy + i + 210)
				EndIf
			Next
			Local mask:Int[] = New Int[256 * (p_size + 4)]
			If p_horizontal
				If Not (cx - 3 < 0 Or cy - 2 < 0 Or cx - 3 + 256 > DeviceWidth() Or cy - 2 + p_size + 4 > DeviceHeight())
				ReadPixels(mask, cx - 3, cy - 2, 256, p_size + 4)
				MainColors = CreateImage(256, p_size + 4)
				MainColors.WritePixels(mask, 0, 0, 256, p_size + 4)
				EndIf
			Else
				If Not (cx - 2 < 0 Or cy - 3 < 0 Or cx - 2 + p_size + 4 > DeviceWidth() Or cy - 3 + 256 > DeviceHeight())
				ReadPixels(mask, cx - 2, cy - 3, p_size + 4, 256)
				MainColors = CreateImage(p_size, 256)
				MainColors.WritePixels(mask, 0, 0, p_size + 4, 256)
				EndIf
			EndIf
		Else
			SetColor(255, 255, 255)
			If p_horizontal
				cy += gap + 255
				DrawImage(MainColors, cx, cy + 1)
				cx += 1
			Else
				cx += gap + 255
				DrawImage(MainColors, cx + 1, cy)
			EndIf
			cx += 2
			cy += 2
			p_size -= 4
		EndIf
		If TouchDown
				If p_horizontal
					If TouchX >= cx - 1 And TouchX < cx +251 And TouchY > cy And TouchY < cy + p_size Then PrimaryColor = GetColorAt(TouchX, TouchY)
				Else
					If TouchX > cx And TouchX < cx + p_size And TouchY >= cy - 1 And TouchY < cy + 251 Then PrimaryColor = GetColorAt(TouchX, TouchY)
				EndIf
				If TouchX > x And TouchX < x + 255 And TouchY > y And TouchY < y + 255 Then ColorPicker = GetColorAt(TouchX, TouchY)
			EndIf
	End Function
	
	'Summary: Return Color at position(x,y) or black if out of screen
	Function GetColorAt:Color(x:Int, y:Int)
		If x >= DeviceWidth Or x < 0 Or y >= DeviceHeight Or y < 0 Then Return New Color()
		Local mask:= New Int[1]
		ReadPixels(mask, x, y, 1, 1)
		Return Int2Color(mask[0])
	End Function
	
	'Summary: Set as the current drawing color
	Method setColor()
		SetColor(r, g, b)
	End Method
	
	'Summary: Return a color from a given pixle in rgb or argb format. num can be a hex $RRGGBB
	Function Int2Color:Color(num:Int)
		Return New Color( (num & $FF0000) / $10000, (num & $FF00) / $100, num & $FF)
	End Function
	
	'Summary: Return a number from a given r,g,b color. Hint: convert result to hex string to obtain readable hex string
	Function Color2Int:Int(color:Color)
		Return color.r * $10000 + color.g * $100 + color.b
	End Function
	
	'Summary: Return h(0-360),s(0-1),v(0-1) color from a given rgb color (0-255)
	Method HSV:Float[] ()
		Return RGB2HSV(r, g, b)
	End Method
	
	'Summary: Return h(0-360),s(0-1),v(0-1) color from a given rgb color (0-255)
	Function RGB2HSV:Float[] (r:Float, g:Float, b:Float)
		r /= 255.0
		g /= 255.0
		b /= 255.0
		Print r + "," + g + "," + b
		Local v:Float = Max(r, Max(g, b))
		If v = 0 Then Return[0.0, 0.0, 0.0]
		r /= v
		g /= v
		b /= v
		Local max:Float = Max(r, Max(g, b))
		Local min:Float = Min(r, Min(g, b))
		Local h:Float, s:Float = max - min
		If s = 0 Then Return[0, 0, v]
		r = (r - min) / s
		g = (g - min) / s
        b = (b - min) / s
		max = Max(r, Max(g, b))
		min = Min(r, Min(g, b))
		If max = r
			h = 60 * (g - b)
			If h < 360 Then h += 360
		ElseIf max = g
			h = 120 + 60 * (b - r)
		Else
			h = 240 + 60 * (r - g)
		EndIf
		Return[h, s, v]
	End Function
	
	Function HSV2RGB:Int[] (h:Float, s:Float, v:Float)
		Local c:Int = v * s, m:Int = v - c, x:Int = c * (1 - Abs( (h / 60.0) mod 2 - 1))
		If h >= 0 And h < 60 Then Return[c + m, x + m, m]
		If h >= 60 And h < 120 Then Return[x + m, c + m, m]
		If h >= 120 And h < 180 Then Return[m, c + m, x + m]
		If h >= 180 And h < 240 Then Return[m, x + m, c + m]
		If h >= 240 And h < 300 Then Return[x + m, m, c + m]
        If h >= 300 And h < 360 Then Return[c + m, m, x + m]
    End Function
End Class