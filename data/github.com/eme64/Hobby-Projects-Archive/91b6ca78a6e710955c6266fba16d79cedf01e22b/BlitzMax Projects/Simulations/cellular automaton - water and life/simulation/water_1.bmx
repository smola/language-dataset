SuperStrict

'asdölfhjalsködfjö
'problem -> fish both on same place


Global xmax:Int = 160
Global ymax:Int = 120

Global dx:Int = 800/xmax
Global dy:Int = 600/ymax

Global ramen:Int = 1

Global feld:Int[xmax,ymax]
Global f2:Int[xmax,ymax]
Global borders:Int = 1

'-------------------- RENDERING -----------------

Type DIRECTION
	Global render_direction:Int = 0
	
	Function render()
		render_direction:+2
		If render_direction>3 Then render_direction=0
	End Function
	
	
	Function x_start:Int()
		If render_direction<2 Then
			Return -1
		Else
			Return xmax
		End If
	End Function
	
	Function x_end:Int()
		If render_direction<2 Then
			Return xmax-1
		Else
			Return 0
		End If
	End Function
	
	Function y_start:Int()
		If (render_direction Mod 2)=1 Then
			Return -1
		Else
			Return ymax
		End If
	End Function
	
	Function y_end:Int()
		If (render_direction Mod 2)=1 Then
			Return ymax-1
		Else
			Return 0
		End If
	End Function
	
	Function x_step:Int()
		If render_direction<2 Then
			Return 1
		Else
			Return -1
		End If
	End Function
	
	Function y_step:Int()
		If (render_direction Mod 2)=1 Then
			Return 1
		Else
			Return -1
		End If
	End Function
End Type


' ################ TYPES #########################

Function RENDER_DOWN(x:Int,y:Int,xx:Int,yy:Int)
	
	If feld[xx,yy]=-1 Then
		'nothing
	ElseIf feld[xx,yy]=0 Then
		feld[xx,yy]=feld[x,y]
		feld[x,y]=0
	Else
		If feld[x,y]+2 > feld[xx,yy] Then
			Local d:Int=(feld[x,y]+2-feld[xx,yy])/2
			
			feld[x,y]:-d
			feld[xx,yy]:+d
		End If
	End If
End Function


Function RENDER_SIDE(x:Int,y:Int,xx:Int,yy:Int)
	
	If feld[xx,yy]=-1 Then
		'nothing
	ElseIf feld[xx,yy]=0 Then
		
		feld[xx,yy]=feld[x,y]/2
		feld[x,y]:-feld[xx,yy]
	Else
		If feld[x,y]+2 > feld[xx,yy] Then
			Local d:Int=(feld[x,y]+2-feld[xx,yy])/2
			
			feld[x,y]:-d
			feld[xx,yy]:+d
		End If
	End If
End Function

Function RENDER_UP(x:Int,y:Int,xx:Int,yy:Int)
	
	If feld[xx,yy]=-1 Then
		'nothing
	ElseIf feld[xx,yy]=0 And feld[x,y]>2 Then
		
		feld[xx,yy]=(feld[x,y]-1)/2
		feld[x,y]:-feld[xx,yy]
		
	ElseIf feld[x,y] > feld[xx,yy]+2 Then
		Local d:Int=(feld[x,y]-2-feld[xx,yy])/2
		
		feld[x,y]:-d
		feld[xx,yy]:+d
	End If
End Function

' ################ END TYPES ######################


Function render_settings()
	If KeyHit(key_f1) Then
		xmax = 20
		ymax = 15
		
		dx = 800/xmax
		dy = 600/ymax
		
		ramen:Int = 1
		
		feld = New Int[xmax,ymax]
	End If
	
	If KeyHit(key_f2) Then
		xmax = 40
		ymax = 30
		
		dx = 800/xmax
		dy = 600/ymax
		
		ramen:Int = 1
		
		feld = New Int[xmax,ymax]
	End If
	
	If KeyHit(key_f3) Then
		xmax = 80
		ymax = 60
		
		dx = 800/xmax
		dy = 600/ymax
		
		ramen:Int = 1
		
		feld = New Int[xmax,ymax]
	End If
	
	If KeyHit(key_f4) Then
		xmax = 200
		ymax = 150
		
		dx = 800/xmax
		dy = 600/ymax
		
		ramen:Int = 1
		
		feld = New Int[xmax,ymax]
	End If
	
	If KeyHit(key_f5) Then
		xmax = 400
		ymax = 300
		
		dx = 800/xmax
		dy = 600/ymax
		
		ramen:Int = 0
		
		feld = New Int[xmax,ymax]
	End If
End Function


' ############################# START ###################################
feld = New Int[xmax,ymax]
f2 = New Int[xmax,ymax]

Graphics 800,600
SetBlend alphablend

Repeat
	render_settings()
	
	If KeyHit(key_b) Then
		borders = 1 - borders
	End If
	
	DIRECTION.render()
	
	If KeyHit(key_u) Or KeyDown(key_r) Then
		'f2 = New Int[xmax,ymax]
		
		
		' -------------------- BEUTE ------------
		Local x:Int = DIRECTION.x_start()
		
		While x <> DIRECTION.x_end()
			x:+DIRECTION.x_step()
			
			Local y:Int = DIRECTION.y_start()
			
			While y <> DIRECTION.y_end()
				y:+DIRECTION.y_step()
				'------------------
				
				If feld[x,y]>0 Then
					
					'--------------------------------- DOWN
					If borders = 1 Then
						
						Local xx:Int = x
						Local yy:Int = (y+1) Mod ymax
						
						RENDER_DOWN(x,y,xx,yy)
					Else
						Local xx:Int = x
						Local yy:Int = y+1
						
						If xx < 0 Or yy < 0 Or xx >= xmax Or yy >= ymax Then
							'nothing
						Else
							RENDER_DOWN(x,y,xx,yy)
						End If
					End If
					
					'--------------------------------- SIDES
					
					Local l_ok:Int = 1
					Local r_ok:Int = 1
					
					If borders = 0 Then
						
						If x=0 Then l_ok=0
						If x = xmax-1 Then r_ok=0
					End If
					
					If l_ok And r_ok Then
						If Rand(0,1)=1 Then
							RENDER_SIDE(x,y,(x+1) Mod xmax,y)
							RENDER_SIDE(x,y,(x-1+xmax) Mod xmax,y)
						Else
							RENDER_SIDE(x,y,(x-1+xmax) Mod xmax,y)
							RENDER_SIDE(x,y,(x+1) Mod xmax,y)
						End If
					ElseIf l_ok Then
						RENDER_SIDE(x,y,(x-1) Mod xmax,y)
					ElseIf r_ok Then
						RENDER_SIDE(x,y,(x+1) Mod xmax,y)
					End If
					
					'--------------------------------- UP
					If borders = 1 Then
						
						Local xx:Int = x
						Local yy:Int = (y-1+ymax) Mod ymax
						
						RENDER_UP(x,y,xx,yy)
					Else
						Local xx:Int = x
						Local yy:Int = y-1
						
						If xx < 0 Or yy < 0 Or xx >= xmax Or yy >= ymax Then
							'nothing
						Else
							RENDER_UP(x,y,xx,yy)
						End If
					End If
					
					
				End If
				'------------------
			Wend
		Wend
		
		'feld = f2
	End If
	
	
	If MouseDown(1) Then feld[MouseX()/dx,MouseY()/dy]:+10+KeyDown(key_w)*100+KeyDown(key_q)*1000000
	If MouseDown(2) Then feld[MouseX()/dx,MouseY()/dy] = 0
	'If MouseDown(3) Then feld[MouseX()/dx,MouseY()/dy] = -1
	If MouseDown(3) Then
		For Local p:Int = 0 To 1
			For Local q:Int = 0 To 1
				feld[(MouseX()/dx+p) Mod xmax,(MouseY()/dy+q) Mod ymax] = -1
			Next
		Next
	End If
	
	If KeyHit(key_space) Then
		For Local x:Int = 0 To xmax-1
			For Local y:Int = 0 To ymax-1
				If feld[x,y]>0 Then feld[x,y]=0
			Next
		Next
	End If
	
	If KeyHit(key_enter) Then
		feld = New Int[xmax,ymax]
	End If
	
	Cls
	
	For Local x:Int = 0 To xmax-1
		For Local y:Int = 0 To ymax-1
			If feld[x,y]=-1 Then' ------------- WALL
				SetColor 255,255,255
			Else If feld[x,y] = 0 Then' ------- AIR
				SetColor 20,20,20
			Else' ----------------------------- WATER
				
				'Local c:Float = Cos(feld[x,y]*10)'0.95^feld[x,y]
				
				'SetColor 255*c,255*c,255
				
				SetColor 70+50*Cos(feld[x,y]*(1.1^MouseZ())),70+50*Cos(feld[x,y]*(1.1^MouseZ())+120),70+50*Cos(feld[x,y]*(1.1^MouseZ())+240)
			End If
			
			DrawRect dx*x,dy*y,dx-ramen,dy-ramen
		Next
	Next
	
	
	SetAlpha 0.5
	SetColor 0,0,100
	DrawRect 0,0,280,150
	SetAlpha 0.8
	SetColor 255,255,255
	
	DrawText "[R] and [U] to render",10,10
	DrawText "[SPACE] clean     [ENTER] new",10,40
	DrawText "Mouse-Right and Left to draw",10,70
	DrawText "[F1] to [F5] for size",10,100
	DrawText "[B] to toggle borders property: " + borders,10,130
	
	SetAlpha 1
	
	Flip
Until KeyHit(key_escape) Or AppTerminate()