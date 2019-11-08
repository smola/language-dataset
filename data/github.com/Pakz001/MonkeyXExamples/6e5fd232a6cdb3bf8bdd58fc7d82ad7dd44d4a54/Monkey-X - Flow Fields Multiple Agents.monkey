Import mojo

Class flowfield
    Field mapwidth:Int,mapheight:Int
    Field tilewidth:Float,tileheight:Float
    Field screenwidth:Int,screenheight:Int
    Field map:Int[][]
	Field tempmap:Int[][]
    Field flowlinestartx:Int
    Field flowlinestarty:Int

    Method New(screenwidth:Int,screenheight:Int,mapwidth:Int,mapheight:Int)
        Self.screenwidth = screenwidth
        Self.screenheight = screenheight
        Self.tilewidth = Float(screenwidth)/Float(mapwidth)
        Self.tileheight = Float(screenheight)/Float(mapheight)
        Self.mapwidth = mapwidth
        Self.mapheight = mapheight
        ' make a array
        map = New Int[mapwidth][]
        tempmap = New Int[mapwidth][]        
        For Local i = 0 Until mapwidth
            map[i] = New Int[mapheight]
            tempmap[i] = New Int[mapheight]
        Next    
        ' -1 if no direction
        For Local y:=0 Until mapheight
        For Local x:=0 Until mapheight
            map[x][y] = -1
        Next
        Next

        '
        ' Here we create a number of points
        ' with which we draw the lines in between.
        '
        Seed = GetDate[5]
        Local lastx:Int=Rnd(2,mapwidth-2)
        Local lasty:Int=Rnd(2,mapheight-2)
        flowlinestartx = lastx
        flowlinestarty = lasty
        For Local i:=0 Until mapwidth*mapheight/10
            Local newx:Int=Rnd(2,mapwidth-2)
            Local newy:Int=Rnd(2,mapheight-2)
            flowline(lastx,lasty,newx,newy)
            lastx=newx
            lasty=newy
        Next
    End Method

	'copy temp into map
	Method refreshmap()
		For Local y:=0 Until mapheight
		For Local x:=0 Until mapwidth
			map[x][y] = tempmap[x][y]
		Next
		Next
	End Method
	
    ' Make a flowfield(line) between two points
    Method flowline(x1:Int,y1:Int,x2:Int,y2:Int)
        Local dx:Int, dy:Int, sx:Int, sy:Int, e:Int
          dx = Abs(x2 - x1)
          sx = -1
          If x1 < x2 Then sx = 1      
          dy = Abs(y2 - y1)
          sy = -1
          If y1 < y2 Then sy = 1
          If dx < dy Then 
             e = dx / 2 
          Else 
             e = dy / 2          
          End If
          Local exitloop:Bool=False
          While exitloop = False
            'SetColor 255,255,255
            'DrawPoint x1,y1
            If x1 = x2 
                If y1 = y2
                    exitloop = True
                End If
            End If
            For Local y:=-6 To 6
            For Local x:=-6 To 6
	            If x1+x<0 Or x1+x>=mapwidth Or y1+y<0 Or y1+y>=mapheight Then continue
                map[x1+x][y1+y] = pointto(x1+x,y1+y,x1,y1)
            Next
            Next
            map[x1][y1] = pointto(x1,y1,x2,y2)
            If dx > dy Then
                x1 += sx ; e -= dy 
                  If e < 0 Then e += dx ; y1 += sy
            Else
                y1 += sy ; e -= dx 
                If e < 0 Then e += dy ; x1 += sx
            Endif
          Wend
          ' put contents in tempmap
		For Local y:=0 Until mapheight
        For Local x:=0 Until mapwidth
        	tempmap[x][y] = map[x][y]
        Next
        Next
     End Method
    ' point the flow field direction to the x2,y2 from x1,y1
     Function pointto:Int(x1:Int,y1:Int,x2:Int,y2:Int)
        Local nd:Int=-1
        If x1<x2 Then nd=0
        If x1>x2 Then nd=4
        If y1<y2 Then nd=2
        If y1>y2 Then nd=6                    
        If x1<x2 And y1<y2 Then nd=1
        If x1>x2 And y1<y2 Then nd=3
        If x1<x2 And y1>y2 Then nd=7
        If x1>x2 And y1>y2 Then nd=5
        Return nd
     End Function

    Method draw()
        SetColor 255,255,255
        For Local y:=0 Until mapheight
        For Local x:=0 Until mapwidth
            Local direction:Int = map[x][y]
            If direction=-1 Then Continue
            Local x1:Float=Float(x)*tilewidth+tilewidth/2
            Local y1:Float=Float(y)*tileheight+tileheight/2
            Local ang:Int= (360/8*direction)
            Local x2:Float=x1+(Cos(ang)*tilewidth/2)
            Local y2:Float=y1+(Sin(ang)*tileheight/2)        
            Local x3:Float=x1+(Cos(ang+150)*tilewidth/4)
            Local y3:Float=y1+(Sin(ang+150)*tileheight/4)        
            Local x4:Float=x1+(Cos(ang-150)*tilewidth/4)
            Local y4:Float=y1+(Sin(ang-150)*tileheight/4)        
            
            DrawPoly([x2,y2,x3,y3,x4,y4])
        Next
        Next
    End Method
End Class

Class alien
	Field alienx:Float,alieny:Float
	Field speed:Float
	Method New(x:Int,y:Int)
		Self.alienx = x
		Self.alieny = y
		Self.speed = Rnd(0.1,1)
	End Method
	'
	' here we modify the flowfield so that other units can move around the
	' unit. We do this by placing arrows around the unit based on his direction.
	Method modifyflowmap()
		Local x:Int=alienx / myflowfield.tilewidth
		Local y:Int=alieny / myflowfield.tileheight
		If x-1<0 Or x+1>=myflowfield.mapwidth Or y-1<0 Or y+1>=myflowfield.mapheight Then Return
		Local d:Int=myflowfield.map[x][y]
		Local a:Int[8]
		Select d
			Case 2 'moving down
			a[0]=2;a[1]=1;a[2]=2;a[3]=7
			a[4]=0;a[5]=5;a[6]=2;a[7]=3
			Case 3 'moving left down
			a[0]=4;a[1]=3;a[2]=2;a[3]=3
			a[4]=4;a[5]=1;a[6]=2;a[7]=3
			Case 4 'moving left
			a[0]=4;a[1]=5;a[2]=4;a[3]=3
			a[4]=4;a[5]=5;a[6]=6;a[7]=3
			Case 5 'moving left up
			a[0]=4;a[1]=5;a[2]=6;a[3]=5
			a[4]=4;a[5]=5;a[6]=6;a[7]=3
			Case 6 ' moving up
			a[0]=4;a[1]=5;a[2]=6;a[3]=7
			a[4]=6;a[5]=5;a[6]=6;a[7]=7
			Case 7 ' moving right up
			a[0]=0;a[1]=5;a[2]=6;a[3]=7
			a[4]=0;a[5]=7;a[6]=6;a[7]=7
			Case 0 ' moving right
			a[0]=0;a[1]=1;a[2]=6;a[3]=7
			a[4]=0;a[5]=1;a[6]=0;a[7]=7
			Case 1 ' moving right down
			a[0]=0;a[1]=1;a[2]=2;a[3]=3
			a[4]=0;a[5]=1;a[6]=2;a[7]=7
			
			
			
			
			
		End Select
		
		myflowfield.map[x][y+1] = a[0]
		myflowfield.map[x-1][y+1] = a[1]
		myflowfield.map[x-1][y] = a[2]
		myflowfield.map[x-1][y-1] = a[3]
		myflowfield.map[x][y-1] = a[4]
		myflowfield.map[x+1][y-1] = a[5]
		myflowfield.map[x+1][y] = a[6]
		myflowfield.map[x+1][y+1] = a[7]
		
	End Method
	Method move()
		Local x2:Int=alienx/myflowfield.tilewidth
		Local y2:Int=alieny/myflowfield.tileheight
		
		Local d:Int=myflowfield.map[x2][y2]

        ' Move the alien based on the flowfield array's direction 0=right 1=rightdown 7=rightup
        Select d
            Case 0
            alienx+=speed
            Case 1
            alienx+=speed;alieny+=speed
            Case 2
            alieny+=speed
            Case 3
            alienx-=speed
            alieny+=speed
            Case 4
            alienx-=speed
            Case 5
            alienx-=speed
            alieny-=speed
            Case 6
            alieny-=speed
            Case 7
            alieny-=speed
            alienx+=speed
        End Select
        ' stay inside array(screen)
        If alienx+10>myflowfield.screenwidth Then alienx = myflowfield.screenwidth-10
        If alienx-10<0 Then alienx = 10
        If alieny+10>myflowfield.screenheight Then alieny = myflowfield.screenheight-10
        If alieny-10<0 Then alieny = 10
		
	End Method
	Method draw()
		SetColor 255,0,0
		DrawCircle(alienx,alieny,myflowfield.tilewidth/2)
	End Method
End Class

Global myflowfield:flowfield
Global myalien:List<alien> = New List<alien>


Class MyGame Extends App
	Field mapwidth:Int,mapheight:Int
    Method OnCreate()
    	Seed = GetDate[5] + GetDate[4]
        SetUpdateRate(60)        
        myflowfield = New flowfield(DeviceWidth(),DeviceHeight(),20,20)
		For Local i:=0 Until 10
			myalien.AddLast(New alien(Rnd(DeviceWidth),Rnd(DeviceHeight)))
		Next
    End Method
    Method OnUpdate()    

    	myflowfield.refreshmap()    	
    	For Local i:=Eachin myalien
	    	i.modifyflowmap
	    	i.move()
    	Next	    	
	       
        ' if pressed space or no move by alien then new flowfield
        If KeyHit(KEY_SPACE) Or MouseHit(MOUSE_LEFT)
        	Seed = Millisecs()
        	mapwidth  = Rnd(20,80)
        	mapheight = mapwidth
            myflowfield = New flowfield(DeviceWidth(),DeviceHeight(),mapwidth,mapheight)
			myalien.Clear()
			For Local i:=0 Until mapwidth*mapheight/20
				Local x:Int=Rnd(DeviceWidth)
				Local y:Int=Rnd(DeviceHeight)
				myalien.AddLast(New alien(x,y))
			Next
        End If    
        
    End Method
    Method OnRender()
        Cls 0,0,0 
        SetColor 255,255,255
        
        myflowfield.draw()
		
		For Local i:=Eachin myalien
			i.draw
		Next

        SetColor 255,255,255
        DrawText("Flow Fields multiple agents - space(touch/lmb) new map.",0,0)
    End Method
End Class

Function rectsoverlap:Bool(x1:Int, y1:Int, w1:Int, h1:Int, x2:Int, y2:Int, w2:Int, h2:Int)
    If x1 >= (x2 + w2) Or (x1 + w1) <= x2 Then Return False
    If y1 >= (y2 + h2) Or (y1 + h1) <= y2 Then Return False
    Return True
End

Function Main()
    New MyGame()
End Function
