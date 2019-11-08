
Strict

Module Prime.KBSplines
ModuleInfo "Author: Kevin Primm"
ModuleInfo "License: MIT"
ModuleInfo "Credit: Adapted from Adam Gore's 'Bird Demo' included in Blitz3D"

Import BRL.Stream

Type TKBKey
	Const CHANNELS = 9
	
	Field fstep						'frame number
	Field cv#[CHANNELS]				'array of channel values (x y z h p b sx sy sz)
	Field linear						'linear flag
	Field tens#, cont#, bias#
End Type

Type TKBSpline
	Field keys:TKBKey[], lastframe
	
	Method Load:TKBSpline(url:Object)
		Local stream:TStream = ReadStream(url)
		If stream = Null Return Null
		
		If ReadLine(stream) <> "B3D Motion" Then Return Null
		If Int(ReadLine(stream)) <> 1 Then Return Null
		ReadLine(stream)
		
		keys = New TKBKey[Int(ReadLine(stream))]
 		
		For Local i = 0 To Length()-1
			Local params$[] = ReadLine(stream).Split(" ")
			keys[i] = New TKBKey
			keys[i].fstep = Int(params[0])
			For Local j = 0 To TKBKey.CHANNELS-1
				keys[i].cv[j] = Float(params[1+j])
			Next
			keys[i].linear = Int(params[10])
			keys[i].tens = Float(params[11])
			keys[i].cont = Float(params[12])
			keys[i].bias = Float(params[13])
		Next
		lastframe = keys[Length()-1].fstep
		
		CloseStream stream
				
		Return Self
	End Method
	
	Method Length()
		Return keys.length
	End Method
	
	Method Frames()
		Return lastframe
	End Method
	
	Method Interpolate#[](tstep#)
		Local icv#[TKBKey.CHANNELS]

		If Length() = 1 Then
			For Local i = 0 To TKBKey.CHANNELS-1
				icv[i] = keys[0].cv[i]
			Next
			Return icv
		EndIf

		If tstep < keys[0].fstep Then Return Null
		
		Local k
		For k = 0 To Length()-1
			If tstep <= keys[k].fstep Then Exit
		Next
		
		Local keya:TKBKey,keyb:TKBKey,keyc:TKBKey,keyd:TKBKey
		Select k
		Case 0
			keya = keys[k]
			keyb = keys[Length()-1]
			keyc = keys[Length()-2]
			keyd = keys[1]
		Case 1
			keya = keys[k]
			keyb = keys[0]
			keyc = keys[Length()-1]
			keyd = keys[k+1]
		Case Length()-1
			keya = keys[k]
			keyb = keys[k-1]
			keyc = keys[k-2]
			keyd = keys[0]
		Default
			keya = keys[k]
			keyb = keys[k-1]
			keyc = keys[k-2]
			keyd = keys[k+1]
		End Select

		Local tlen = keya.fstep - keyb.fstep
		Local t# = (tstep - keyb.fstep) / tlen
		
		Local h#[4], dd0a#, dd0b#, ds1a#, ds1b#, adj0#, adj1#
		If keya.linear = 0 Then
			Local t2# = t * t
			Local t3# = t * t2
			Local z# = 3 * t2 - t3 - t3
		
			h = [1.0 - z, z, t3 - t2 - t2 + t, t3 - t2]

			dd0a = (1 - keyb.tens) * (1 + keyb.cont) * (1 + keyb.bias)
			dd0b = (1 - keyb.tens) * (1 - keyb.cont) * (1 - keyb.bias)
			ds1a = (1 - keya.tens) * (1 - keya.cont) * (1 + keya.bias)
			ds1b = (1 - keya.tens) * (1 + keya.cont) * (1 - keya.bias)
		
			Local d2#, adj0#, adj1#
			If keyb.fstep <> 0
				d2 = (keya.fstep - keyc.fstep)
				adj0 = tlen / d2
			End If

			If keya.fstep <> lastframe
				d2 = (keys[k+1].fstep - keyb.fstep)
				adj1 = tlen / d2
			End If
		End If

		For Local i = 0 To TKBKey.CHANNELS-1
			Local d10# = keya.cv[i] - keyb.cv[i]

			If keya.linear = 0
				Local dd0#
				If keyb.fstep = 0
					dd0 = 0.5 * (dd0a + dd0b) * d10
				Else
					dd0 = adj0 * (dd0a * (keyb.cv[i] -  keyc.cv[i]) + dd0b * d10)
				End If
				
				Local ds1#
				If keya.fstep = lastframe
					ds1 = 0.5 * (ds1a + ds1b) * d10
				Else
					ds1 = adj1 * (ds1a * d10 + ds1b * ( keyd.cv[i] - keya.cv[i] ))
				End If

				icv[i] = (h[0] * keyb.cv[i]) + (h[1] * keya.cv[i]) + (h[2] * dd0) + (h[3] * ds1)
			Else
				icv[i] = keyb.cv[i] + t * d10
			End If
		Next
 
		Return icv
	End Method
End Type

Function LoadKBSpline:TKBSpline(url:Object)
	Return New TKBSpline.Load(url)
End Function
