'------------------------------------------------------------------------------
' Component: TColor
' Info: Range Checking is implented in Debug-Mode only
'------------------------------------------------------------------------------
Type TColor

'------------------------------------------------------------------------------
' Color Properties + Alpha
'------------------------------------------------------------------------------	
	Field r:Byte	= 255
	Field g:Byte	= 255
	Field b:Byte	= 255
	Field a:Float	= 1.0
	Field blend:Int = ALPHABLEND
	

'------------------------------------------------------------------------------
' Set the RGB values
'------------------------------------------------------------------------------	
	Method SetRGB( red:Byte, green:Byte, blue:Byte )
		r = red
		g = green
		b = blue
	EndMethod
	

'------------------------------------------------------------------------------
' Set the Alpha value
'------------------------------------------------------------------------------	
	Method SetAlpha( alpha:Float )
		?debug
		If (a < 0) Or (a > 1)
			DebugLog( "The Alpha value is out of Range! Turning on debugger ..." )
			DebugStop()
		EndIf
		?
		a = alpha
	EndMethod
	

'------------------------------------------------------------------------------
' Set the blendmode
'------------------------------------------------------------------------------
	Method SetBlend( blendmode:Int )
		blend = blendmode
	EndMethod
	
		
EndType
