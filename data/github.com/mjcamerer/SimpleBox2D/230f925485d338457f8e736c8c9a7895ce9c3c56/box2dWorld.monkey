'Set Strict Mode
Strict

'Import Box2d Modules
Import box2d
Import box2dEntity
Import box2dPolygon

'Box2d World Class
Class Box2D_World

	'Fields
	Field world:b2World
	Field scale:Float
	Field velocityIterations:Int
    Field positionIterations:Int
    Field timeStep:Float
	Field entityList:List<Entity>
	Field debug:Bool
	Field debugVisible:Bool
	Field dbgDraw:b2DebugDraw  
	Field dbgClear:Bool
	    
	'Create New World
	Method New(debug:Bool=False, debugVisible:Bool=False)
	
		'Default world values
		Local gravityX:Float = 0.0
		Local gravityY:Float = 10.0
		Local scale:Float = 64.0
		Local doSleep:Bool = True
		
		'Create World
		Self.world = New b2World(New b2Vec2(gravityX, gravityY), doSleep)	
		
		'Set up initial settings
		Self.velocityIterations	= 3
		Self.positionIterations	= 3
		Self.timeStep = 1.0 / 60.0
		Self.scale = 64.0
		
		'Create entity list
		Self.entityList = New List<Entity>()
		
		'Set up debug settings
		Self.debug = debug
		If Self.debug = True
			Self.dbgDraw = New b2DebugDraw()       
		    Self.dbgDraw.SetDrawScale(Self.scale) 
			Self.dbgDraw.SetFillAlpha(0.3)
			Self.dbgDraw.SetLineThickness(1.0)
			Self.dbgDraw.SetFlags(b2DebugDraw.e_shapeBit | b2DebugDraw.e_jointBit)
			Self.world.SetDebugDraw(dbgDraw)
			Self.debugVisible = debugVisible
			Self.dbgClear = False
		Endif
		
	End
	
	'Update Method (Update World)
	Method Update:Void()
	
		'Update World TimeStep
		Self.world.TimeStep(Self.timeStep, Self.velocityIterations, Self.positionIterations)
		
		'Clear World Forces
		Self.world.ClearForces()
		
	End
	
	'Render Method (Render World)
	Method Render:Void()
	
		'Draw Debug
		If Self.debug = True
			world.DrawDebugData()
			SetAlpha(0.5)
		Elseif Self.dbgClear = True
			Self.dbgDraw.Clear()
			Self.dbgDraw = Null
			Self.dbgClear = False
		Endif
		
		'Render each Entity in the world
		If Self.entityList.Count() > 0
			For Local e:Entity = Eachin Self.entityList
				If e <> Null e.Draw()
			Next				
		Endif
		
		'Reset Alpha if in debug mode
		If Self.debug = True
			SetAlpha(1.0)
		Endif
		
	End
	
	'SetGravity Method
	Method SetGravity:Void(gravityX:Float, gravityY:Float)
		Self.world.SetGravity(new b2Vec2(gravityX, gravityY))
	End Method
	
	'GetGravity Method
	Method GetGravity:b2Vec2()
		Return Self.world.GetGravity()
	End Method
	
	'SetScale Method
	Method SetScale:Void(s:Float)
		Self.scale = s
		If Self.debug = True
			Self.SetDebugMode(True)
		EndIf
	End Method
	
	'GetScale Method
	Method GetScale:Float()
		Return Self.scale
	End Method
	
	'SetDebugMode Method
	Method SetDebugMode:Void(value:Bool, visible:Bool=True)
		
		'Set Debug Mode
		Self.debug = value
			
		'Turn Debug Mode ON
		If Self.debug = True
		
			Self.dbgDraw = Null
			Self.dbgDraw = New b2DebugDraw()       
		    Self.dbgDraw.SetDrawScale(Self.scale) 
			Self.dbgDraw.SetFillAlpha(0.3)
			Self.dbgDraw.SetLineThickness(1.0)
			Self.dbgDraw.SetFlags(b2DebugDraw.e_shapeBit | b2DebugDraw.e_jointBit)
			Self.world.SetDebugDraw(dbgDraw)
			Self.debugVisible = visible
		
		'Turn Debug Mode OFF
		Elseif Self.debug = False
		
			Self.dbgClear = True
			
		Endif
		
	End Method
	
	'Creates a box
	Method CreateBox:Entity(x:Float, y:Float, width:Float, height:Float, static:Bool = False, restitution:Float=0.3, density:Float = 1.0, friction:Float=0.3, sleep:Bool=True)
		Local e:Entity = New Entity()
		e.CreateBox(Self.world, x/Self.scale, y/Self.scale, width/Self.scale, height/Self.scale, Self.scale, restitution, density, friction, static, sleep)
		Self.entityList.AddLast(e)
		Return(e)
	End
	
	'Creates a circle
	Method CreateCircle:Entity(x:Float, y:Float, radius:Float, static:Bool = False, restitution:Float=0.2, density:Float = 1.0, friction:Float=0.3, sleep:Bool=True)
		Local e:Entity = New Entity()
		e.CreateCircle(Self.world, x/Self.scale, y/Self.scale, radius/Self.scale, Self.scale, restitution, density, friction, static, sleep)
		Self.entityList.AddLast(e)
		Return e
	End
	
	'Creates a multi-polygon
	Method CreateMultiPolygon:Entity(x:Float, y:Float, polygonList:List<Polygon>, static:Bool = False, restitution:Float=0.3, density:Float = 1.0, friction:Float=0.3, sleep:Bool=True)
		Local e:Entity = New Entity()
		e.CreateMultiPolygon(Self.world, x/Self.scale, y/Self.scale, polygonList, Self.scale, restitution, density, friction, static, sleep)
		Self.entityList.AddLast(e)
		Return e
	End Method
	
End