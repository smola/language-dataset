
Namespace AST

Class Asteroids Extends Window
	
	Field player:Player
	Field meteor:Meteor
	Field meteor2:Meteor
	Global DebugHitboxes:Bool = True
	Global W_WIDTH:Int = 640
	Global W_HEIGHT:Int = 480
	Global FIXED_RESOLUTION:= New Vec2i(W_WIDTH, W_HEIGHT)
	
	Method OnCreateWindow() Override
		
		player = New Player(New Vec2f(W_WIDTH/2,W_HEIGHT/2), ORIGIN)
		meteor = New Meteor(New Vec2f(100, 100), New Vec2f(1,1), 50)
		meteor2 = New Meteor(New Vec2f(400,400), New Vec2f(1.0,0.5), 40)
		
	End
	
	Method OnMeasure:Vec2i() Override
		Return FIXED_RESOLUTION
	End
	
	Method New( title:String="AST",width:Int=W_WIDTH,height:Int=W_HEIGHT,flags:WindowFlags=WindowFlags.Resizable )
		
		Super.New(title, width, height, flags)
		Layout = "letterbox"
	
	End
	
	Method OnRender(canvas:Canvas) Override
		
		App.RequestRender()
		player.Update()
		player.RenderHitboxes(canvas)
		player.Render(canvas)
		
		meteor.Update()
		meteor.RenderHitboxes(canvas)
		meteor.Render(canvas)
		
		meteor2.Update()
		meteor2.RenderHitboxes(canvas)
		meteor2.Render(canvas)
	End
	
End