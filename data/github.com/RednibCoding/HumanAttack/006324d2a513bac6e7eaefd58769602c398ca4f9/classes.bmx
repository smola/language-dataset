


'-----------------------------------------------------------------------
'Hauptklasse für Entitys
Type tEntity
  	Field x:Float,y:Float
  	Field height:Float
  	Field rotation:Int
 	 Field speed:Float = 1

 	Method Update() Abstract
  	Method DrawBody() Abstract
  	Method DrawShadow() Abstract
	'Zum überprüfen wer hinter wem steht(ZOrder), um somit falsches einzeichnen der Objekte zu verhindern
	'Siehe Function UpdateObjects()
	'Methode muss den Namen "Compare" haben!
	Method Compare:Int(other:Object)
		If other=Null Return 0
		If y = tEntity(other).y Return 0
		If y < tEntity(other).y Then Return 1 Else Return -1
	End Method
EndType
'-----------------------------------------------------------------------



'-----------------------------------------------------------------------
'Klasse für Player

Type tPlayer Extends tEntity
	Field frameWalk:Int = 0
	Field frameStand:Int = 0
  	Field frmSpeed:Int = 1
  	Field frmTimer:Int = 0
  	Field direction:Int = 0
  	Field life:Int
	Field animType:Int '0 = stand, 1 = walk
	Field shootTimer:Int = 0, shootDelay:Int = 5, canShoot:Int = 0
	
	Method Update()	'Abgeleitete Methode von der Klasse tEntity
    		UpdateFrames()
		UpdateDirection()	
		UpdateShootDelay()
		
		If MouseDown_Left = True And canShoot = 1
			Shoot(0)
			canShoot = 0
		EndIf
  	EndMethod

	'Checkt und setzt die Animationsgeschwindigkeit der Spielerbilder
	Method UpdateFrames()
		frmTimer:+1
    		If frmTimer > frmSpeed
      		frmTimer = 0
			If animType = 0
      			frameStand = frameStand +1
      			If frameStand = 20
					frameStand = 0
    				EndIf
			ElseIf animType = 1
				frameWalk = frameWalk +1
				If frameWalk = 10
					frameWalk = 0
    				EndIf
			EndIf
		EndIf
	EndMethod
	
	Method UpdateDirection()
		If Self.x < 0 Then Self.x = 1024
		If Self.x > 1024 Then Self.x = 0
		If Self.y < 0 Then Self.y = 768
		If Self.y > 768 Then Self.y = 0
		
		If KeyDown_W Or KeyDown_UP
			y = y - speed
			animType = 1
		ElseIf KeyDown_S Or KeyDown_DOWN
			y = y + speed
			animType = 1
		EndIf
		If KeyDown_A Or KeyDown_LEFT
			x = x - speed
			animType = 1
		ElseIf KeyDown_D Or KeyDown_RIGHT
			x = x + speed
			animType = 1
		EndIf
		
		If Not KeyDown_W And Not KeyDown_A And Not KeyDown_S And Not KeyDown_D And Not KeyDown_UP And Not KeyDown_DOWN And Not KeyDown_LEFT And Not KeyDown_RIGHT
			animType = 0
		EndIf
		
		
		rotation = ATan((Self.x - MouseXVal) / (Self.y - MouseYVal))
		Print rotation
		
		'Up
		If ( MouseXVal < Self.x And MouseYVal < Self.y And rotation < 22.5 ) Or ( MouseXVal > Self.x And MouseYVal < Self.y And rotation > -22.5 ) 
			direction = 7
		EndIf
		'Up/Right
		If ( MouseXVal > Self.x And MouseYVal < Self.y And rotation < -22.5 And rotation > -67.5 )
			direction = 0
		EndIf
		'Right
		If ( MouseXVal > Self.x And MouseYVal < Self.y And rotation < -67.5 ) Or ( MouseXVal > Self.x And MouseYVal > Self.y And rotation > 67.5 )
			direction = 1
		EndIf
		'Down/Right
		If ( MouseXVal > Self.x And MouseYVal > Self.y And rotation < 67.5 And rotation > 22.5 )
			direction = 2
		EndIf
		'Down
		If ( MouseXVal > Self.x And MouseYVal > Self.y And rotation < 22.5 ) Or ( MouseXVal < Self.x And MouseYVal > Self.y And rotation > -22.5 )
			direction = 3
		EndIf
		'Down/Left
		If ( MouseXVal < Self.x And MouseYVal > Self.y And rotation < -22.5 And rotation > -67.5 )
			direction = 4
		EndIf
		'Left
		If ( MouseXVal < Self.x And MouseYVal > Self.y And rotation < -67.5 ) Or ( MouseXVal < Self.x And MouseYVal < Self.y And rotation > 67.5 )
			direction = 5
		EndIf
		'Up/Left
		If( MouseXVal < Self.x And MouseYVal < Self.y And rotation < 67.5 And rotation > 22.5 )
			direction = 6
		EndIf
	EndMethod
	
	' Erstellen eines Schusses (siehe Type tShot)
	Method Shoot(typ:Int)
		tShot.Create:tShot(Self.x,Self.y,20, 0)
	EndMethod
	
	
	'Regelt wie schnell der Spieler schiessen kann
	Method UpdateShootDelay()
		shootTimer:+1
    		If shootTimer > shootDelay
      		shootTimer = 0
      		canShoot = 1
		EndIf
	EndMethod
	
	'Zeichnet den Schatten
	Method DrawShadow()'Abgeleitete Methode von der Klasse tEntity
    		SetBlend ALPHABLEND
    		SetColor 0,0,0
    		SetAlpha 0.5
		SetScale 1,1
    		SetRotation -30
		If Self.animType = 0
    			DrawImage Img_PlayerStand[direction],x-20, y-2, frameStand
		Else
			DrawImage Img_PlayerRun[direction],x-20, y-2, frameWalk
		EndIf
  	EndMethod
	
	'Zeichnet das eigentliche Bild
	Method DrawBody()'Abgeleitete Methode von der Klasse tEntity
		SetBlend ALPHABLEND
		SetColor 255,255,255
		SetAlpha 1
		SetScale 1,1
    		SetRotation 0
    		If Self.animType = 0
    			DrawImage Img_PlayerStand[direction],x, y, frameStand
		Else
			DrawImage Img_PlayerRun[direction],x, y, frameWalk
		EndIf
		DrawHealthBar()
  	EndMethod

	'Zeichnet die Healthbar des Spielers über seinen Kopf ein (siehe DrawBody())
	Method DrawHealthBar()
		SetBlend SOLIDBLEND
  		SetColor 255,255,255
		SetScale life/2,1
		DrawImage Img_Health,x, y-64
	EndMethod
	
	
	' Getter Methoden
	Method getX:Float()
		Return Self.x
	EndMethod
	
	Method getY:Float()
		Return Self.y
	EndMethod
	
	Method getLife:Int()
		Return Self.life
	EndMethod
	
	Method getRot:Int()
		Return Self.rotation
	EndMethod
	
	' Setter Methoden
	Method setLife(life:Int)
		Self.life = life
	EndMethod
	
	Method setRot(rot:Int)
		Self.rotation = rot
	EndMethod
	
	'Create
	Function Create:tPlayer(x:Float,y:Float,speed:Float)
    		Local p:tPlayer = New tPlayer
    		p.x=x
    		p.y=y
    		p.speed = speed
    		p.height = 0
    		p.life = 100
    		Lst_ObjectList.AddLast p
    		Return p
  	EndFunction
	
	'Destroy
	Method destroy()
		If life <= 0
			Lst_ObjectList.Remove Self
		EndIf
	End Method
EndType
'-----------------------------------------------------------------------



'-----------------------------------------------------------------------
'Klasse für Enemy
'typ 0 = skeleton, 1 = , 2 = , 3 = 

Type tEnemy Extends tEntity
	Field typ:Int = 0
	Field frame:Int = 0, frmSpeed:Int = 1, frmTimer:Int = 0, maxFrames:Int = 20
  	Field direction:Int = 0, canChangeDirection:Int, changeDirectionDelay:Int = 10, changeDirectionTimer:Int, canFollow:Int
  	Field life:Int
	Field shootTimer:Int, shootDelay:Int, canShoot:Int
	Field image:Timage[8]
	Field animType:Int '0 = walk, 1 = attack, 2 = wounded
	
	Method Update() 'Abgeleitete Methode von der Klasse tEntity
    		UpdateFrames()
		UpdateChangeDirectionDelay()
		UpdateDirection()	
		UpdateCollisions()
  	EndMethod

	'Überprüft auf Kollisionen
	Method UpdateCollisions()
		If canChangeDirection = 1
			' Self und andere Gegner
			For Local e:tEnemy = EachIn Lst_Objectlist
				If e.getX() <> Self.x And e.getY() <> Self.y
					If checkDistance(e.x, e.y, Self.x, Self.y) < 50
						canFollow = 0
						Self.rotation = Rand(0,360)
						canChangeDirection = 0
					EndIf
				EndIf
			Next
			'Self und Spieler
			For Local p:tPlayer = EachIn Lst_Objectlist
				If p.getX() <> Self.x And p.getY() <> Self.y
					If checkDistance(p.x, p.y, Self.x, Self.y) < 40
						canFollow = 0
						canChangeDirection = 0
						animType = 1 '(attack)
					Else
						animType = 0 '(walk)
					EndIf
				EndIf
			Next
		EndIf
	EndMethod
	
	
	'Überprüft und setzt die Animationsgeschwindigkeit der Gegnerbilder
	Method UpdateFrames()
		frmTimer:+1
    		If frmTimer > frmSpeed
      		frmTimer = 0
      		frame:+1
      		If frame = maxFrames 
				frame = 0
    			EndIf
		EndIf
	EndMethod
	
	
	'Regelt wie schnell der Gegner hintereinander seine Richtung ändern kann
	Method UpdateChangeDirectionDelay()
		changeDirectionTimer:+1
    		If changeDirectionTimer > changeDirectionDelay
      		changeDirectionTimer = 0
      		canChangeDirection = 1
			canFollow = 1
		EndIf
	EndMethod
	
	
	Method UpdateDirection()
	If animType <> 1
		' Distanz zwischen Self und Spieler prüfen
		For Local p:tPlayer = EachIn Lst_ObjectList
			If checkDistance(p.x, p.y, Self.x, Self.y) < 350 And canFollow = 1
				'Wenn kleiner als 350, dann dreht sich Gegner zum Spieler und läuft auf ihn zu
				rotation = ATan((Self.x - p.x) / (Self.y - p.y))

				'Up
				If ( p.x < Self.x And p.y < Self.y And rotation < 22.5 ) Or ( p.x > Self.x And p.y < Self.y And rotation > -22.5 ) 
					direction = 7
				EndIf
				'Up/Right
				If ( p.x > Self.x And p.y < Self.y And rotation < -22.5 And rotation > -67.5 )
					direction = 0
				EndIf
				'Right
				If ( p.x > Self.x And p.y < Self.y And rotation < -67.5 ) Or ( p.x > Self.x And p.y > Self.y And rotation > 67.5 )
					direction = 1
				EndIf
				'Down/Right
				If ( p.x > Self.x And p.y > Self.y And rotation < 67.5 And rotation > 22.5 )
					direction = 2
				EndIf
				'Down
				If ( p.x > Self.x And p.y > Self.y And rotation < 22.5 ) Or ( p.x < Self.x And p.y > Self.y And rotation > -22.5 )
					direction = 3
				EndIf
				'Down/Left
				If ( p.x < Self.x And p.y > Self.y And rotation < -22.5 And rotation > -67.5 )
					direction = 4
				EndIf
				'Left
				If ( p.x < Self.x And p.y > Self.y And rotation < -67.5 ) Or ( p.x < Self.x And p.y < Self.y And rotation > 67.5 )
					direction = 5
				EndIf
				'Up/Left
				If( p.x < Self.x And p.y < Self.y And rotation < 67.5 And rotation > 22.5 )
					direction = 6
				EndIf
				
				If Self.y >= p.y
					x=x+(speed*Cos(rotation+90))
    					y=y-(speed*Sin(rotation+90))
				ElseIf Self.y < p.y
					x=x-(speed*Cos(rotation+90))
    					y=y+(speed*Sin(rotation+90))
				EndIf

			Else
				'Wenn Gegner nicht am angreifen ist
				
					'Andernfalls läuft Gegner ziellos im Kreis herum
					rotation = rotation + Rand(0,3)
					If rotation<0 rotation:+360
    					If rotation>=360 rotation:-360
    					direction = rotation / 45
					x=x+(speed*Cos(rotation-90))
    					y=y+(speed*Sin(rotation-90))
				
			EndIf
		Next
	EndIf
		'Wenn Gegner über den Bildschirmrand hinausläuft - kommt er am gegenüberliegenen Rand wieder raus
		If Self.x < 0 Then Self.x = 1024
		If Self.x > 1024 Then Self.x = 0
		If Self.y < 0 Then Self.y = 768
		If Self.y > 768 Then Self.y = 0
	EndMethod
	
	'Überprüft und gibt die Distanz zwischen 2 Punkten zurück
	Method checkDistance:Float(x1:Float, y1:Float, x2:Float, y2:Float)
		Return Sqr((x1 - x2)^2 + (y1-y2)^2)
	EndMethod
	
	'Die Methode zum erstellen eines Schusses (siehe Type tShot)
	Method Shoot(typ:Int)
		tShot.Create:tShot(Self.x,Self.y,5, typ)
	EndMethod
	
	'Regelt wie schnell der Gegner hintereinander schiessen kann
	Method UpdateShootDelay()
		shootTimer:+1
    		If shootTimer > shootDelay
      		shootTimer = 0
      		canShoot = 1
		EndIf
	EndMethod
	
	
	'Zeichnet den Schatten
	Method DrawShadow() 'Abgeleitete Methode von der Klasse tEntity
    		SetBlend ALPHABLEND
    		SetColor 0,0,0
		SetScale 1,1
    		SetAlpha 0.5
    		SetRotation -30
		If animType = 0 'Walk
    			DrawImage Img_EnemySkeletonWalk[direction],x-15, y-2, frame
		ElseIf animType = 1 'Attack
			DrawImage Img_EnemySkeletonAttack[direction],x-15, y-2, frame
		ElseIf animType = 2 'Wounded
			'DrawImage Img_EnemySkeletonWounded[direction],x-15, y-2, frame
		EndIf
			
  	EndMethod
	
	'Zeichnet das eigentliche Bild
	Method DrawBody() 'Abgeleitete Methode von der Klasse tEntity
		SetBlend ALPHABLEND
		SetColor 255,255,255
		SetScale 1,1
		SetAlpha 1
    		SetRotation 0
    		If animType = 0 'Walk
    			DrawImage Img_EnemySkeletonWalk[direction],x, y, frame
		ElseIf animType = 1 'Attack
			DrawImage Img_EnemySkeletonAttack[direction],x, y, frame
		ElseIf animType = 2 'Wounded
			'DrawImage Img_EnemySkeletonWounded[direction],x, y, frame
		EndIf
		DrawHealthBar()
  	EndMethod
	
	'Zeichnet die Healthbar des Gegners über seinen Kopf ein (siehe DrawBody())
	Method DrawHealthBar()
		SetBlend SOLIDBLEND
  		SetColor 255,255,255
		SetScale life/2,1
		DrawImage Img_Health,x, y-64
	EndMethod
	
	
	' Getter Methoden
	Method getX:Float()
		Return Self.x
	EndMethod
	
	Method getY:Float()
		Return Self.y
	EndMethod
	
	Method getImage:Timage()
		Return Self.image[direction]
	EndMethod
	
	Method getFrame:Int()
		Return Self.frame
	EndMethod
	
	Method getLife:Int()
		Return Self.life
	EndMethod
	
	Method getRot:Int()
		Return Self.rotation
	EndMethod
	
	' Setter Methoden
	Method setLife(life:Int)
		Self.life = life
	EndMethod
	
	Method setRot(rot:Int)
		Self.rotation = rot
	EndMethod


	'Create
	Function Create:tEnemy(x:Float,y:Float,speed:Float, typ:Int)
    		Local e:tEnemy = New tEnemy
    		e.x=x
    		e.y=y
    		e.rotation = Rand(0,90)
    		e.speed = speed
		e.typ = typ
		e.maxFrames = 20
    		e.height = 0
    		e.life = 100
    		Lst_ObjectList.AddLast e
    		Return e
  	EndFunction
	
	'Destroy
	Method destroy()
		Lst_ObjectList.Remove Self
	End Method
	
EndType
'-----------------------------------------------------------------------





'-----------------------------------------------------------------------
'Klasse für den Schuss
' typ 0 = normaler Schuss, typ 1 = Rakete

Type tShot Extends tEntity

	Field typ:Int, dmg:Int, setDirection:Float = 1
	Field image:Timage
	
	'0 = up, 1 = down
	Field direction:Int 
	
	
	
	Method Update()	
		UpdateDirection()	
		UpdateCollisions()
  	EndMethod


	Method UpdateDirection()
		If setDirection = 1
			rotation = ATan((Self.x - MouseXVal) / (Self.y - MouseYVal))
			If Self.y >= MouseYVal
				direction = 1
			ElseIf Self.y < MouseYVal
				direction = 0
			EndIf
			setDirection = 0
		EndIf
		
		If direction = 1
			x=x+(speed*Cos(rotation+90))
    			y=y-(speed*Sin(rotation+90))
		ElseIf direction = 0
			x=x-(speed*Cos(rotation+90))
    			y=y+(speed*Sin(rotation+90))
		EndIf
		
		If Self.x < 0 Then destroy()
		If Self.x > 1024 Then destroy()
		If Self.y < 0 Then destroy()
		If Self.y > 768 Then destroy()
		
	EndMethod
	
	Method UpdateCollisions()
		' Self mit Enemy 
		For Local e:tEnemy = EachIn Lst_ObjectList
			If checkDistance(Self.x, Self.y, e.x, e.y) < 20
				e.setLife(e.getLife() - Self.dmg)
				If e.getlife() <= 0 e.destroy()
				destroy()
			EndIf
		Next
	EndMethod
	
	
	'Überprüft und gibt die Distanz zwischen 2 Punkten zurück
	Method checkDistance:Float(x1:Float, y1:Float, x2:Float, y2:Float)
		Return Sqr((x1 - x2)^2 + (y1-y2)^2)
	EndMethod
	
	
	'Zeichnet den Schatten
	Method DrawShadow()
    		SetBlend ALPHABLEND
    		SetColor 0,0,0
		SetScale 1,1
    		SetAlpha 0.5
    		SetRotation -30
		If typ = 0
    			DrawImage Img_Shot,x-15, y-2
		Else
			'DrawImage Img_Missile,x-15, y-2
		EndIf
  	EndMethod
	
	'Zeichnet das eigentliche Bild
	Method DrawBody()
		SetBlend ALPHABLEND
		SetColor 255,255,255
		SetScale 1,1
		SetAlpha 1
    		SetRotation 0
    		DrawImage Self.image,x, y
  	EndMethod
	
	'Getter Methoden
	Method getX:Float()
		Return Self.x
	EndMethod
	
	Method getY:Float()
		Return Self.y
	EndMethod
	
	Method getDmg:Int()
		Return Self.dmg
	EndMethod
	
	Method getImage:Timage()
		Return Self.image
	EndMethod

	' Create
	Function Create:tShot(x:Float,y:Float,speed:Float, typ:Int)
		SeedRnd MilliSecs()
    		Local s:tShot = New tShot
    		s.x=x
    		s.y=y
    		s.speed = speed
		s.typ = typ
    		s.height = 0
		s.dmg = 10
		If typ = 0
    			s.image = Img_Shot
		Else
			's.image = Img_Missile
		EndIf
    		Lst_ObjectList.AddLast s
    		Return s
  	EndFunction

	' Destroy
	Method destroy()
		Lst_ObjectList.Remove Self
	End Method

EndType









