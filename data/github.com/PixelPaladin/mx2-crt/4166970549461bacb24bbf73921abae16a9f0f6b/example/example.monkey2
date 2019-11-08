
Namespace application

#Import "<std>"
#Import "<mojo>"
'#Import "<crt>"
#Import "../crt/crt"

#import "assets/"

Using std..
Using mojo..
Using crt..

Class AppWindow Extends Window
	Field crt:GraphicsCRT
	
	Field _fullscreen:Bool = False
	Field _fullscreenTime:Int
	Field fnt:Font
	Field mode:Int = 0
	Field menuVisible:Bool = True
	Field testImages:Image[]
	Field ani:Float = 0.0
	
	Method New(title:String, width:Int, height:Int)
		'Call super class constructor - this just passes the arguments 'up' to the Window class constructor.
		Super.New(title, width, height, WindowFlags.Resizable)
		
		fnt = Font.Load("asset::DejaVuSansMono.ttf", 13)
		
		testImages = New Image[3]
		testImages[0] = Image.Load("asset::test1.jpg")
		testImages[1] = Image.Load("asset::test2.jpg")
		testImages[2] = Image.Load("asset::test3.jpg")
		
		' initialize crt screen (width, heightn)
		crt = New GraphicsCRT(320, 200)
		crt.RGBSplitIntensity = 0.4
		crt.ScanlineIntensity = 0.2
		
		crt.Canvas.Font = fnt
		
		_fullscreenTime = Millisecs()-500
		
		Layout = "resize"
		ClearColor = New Color(0.0, 0.2, 0.4)
		Style.BackgroundColor = Color.Black
		SwapInterval = 1
	End

	Method OnRender(canvas:Canvas) Override
		If Keyboard.KeyReleased(Key.Escape) Then App.Terminate()
		App.RequestRender()
		If Mouse.ButtonDown(MouseButton.Left) Then
			Select mode
			Case 0
				crt.Brightness = Float(Mouse.X)/Float(Width)
			Case 1
				crt.Contrast = Float(Mouse.X)/Float(Width)
			Case 2
				crt.Gamma = Float(Mouse.X)/Float(Width)
			Case 3
				crt.Resolution = New Vec2i(Float(Mouse.X+31)/Float(Width+31)*800.0+1, Float(Mouse.Y+16)/Float(Height+16)*600.0+1)
			Case 4
				crt.Curvature = New Vec2f((Float(Mouse.X)/Float(Width)-0.5)*20.0, (Float(Mouse.Y)/Float(Height)-0.5)*20.0)
			Case 5
				crt.Scale = New Vec2f((Float(Mouse.X)/Float(Width)-0.5)*4.0, (Float(Mouse.Y)/Float(Height)-0.5)*4.0)
			Case 6
				crt.ScanlineIntensity = Float(Mouse.X)/Float(Width)
			Case 7
				crt.RGBSplitIntensity = Float(Mouse.X)/Float(Width)
			End
		End
		' get crt canvas:
		Local crtCanvas:Canvas = crt.Canvas
		
		If Keyboard.KeyPressed(Key.M) Then menuVisible = Not menuVisible
		If Keyboard.KeyPressed(Key.F11) Then
			Print(_fullscreenTime)
			If Millisecs()-_fullscreenTime > 500 Then
				_fullscreenTime = Millisecs()
				If Not _fullscreen Then
					_fullscreen = True
					BeginFullscreen()
				Else
					_fullscreen = False
					EndFullscreen()
				End
			End
		End
		
		' render some stuff on crt screen:
		crtCanvas.Clear(Color.Black)
		
		' render test images:
		Local i1:Int = (ani/300) Mod 3
		Local z1:Float = 0.125*(ani Mod 300.0)/300.0
		ani += 1.0
		crtCanvas.Color = Color.White
		crtCanvas.DrawRect(0.0-Float(crt.Resolution.x)*z1,
		                   0.0-Float(crt.Resolution.y)*z1,
		                   crt.Resolution.x+Float(crt.Resolution.x)*z1*2.0,
		                   crt.Resolution.y+Float(crt.Resolution.y)*z1*2.0,
		                   testImages[i1])
		
		' menu
		If menuVisible Then
			crtCanvas.BlendMode = BlendMode.Multiply
			crtCanvas.Color = New Color(0.0, 0.0, 0.0, 0.75)
			crtCanvas.DrawRect(0,0,184, 192)
			crtCanvas.BlendMode = BlendMode.Alpha
			crtCanvas.Color = Color.White
			crtCanvas.DrawText("MODE", 5, 5)
			crtCanvas.DrawText("  brightness", 5, 20+0*12)
			crtCanvas.DrawText("  contrast",   5, 20+1*12)
			crtCanvas.DrawText("  gamma",      5, 20+2*12)
			crtCanvas.DrawText("  resolution", 5, 20+3*12)
			crtCanvas.DrawText("  curvature",  5, 20+4*12)
			crtCanvas.DrawText("  scale",      5, 20+5*12)
			crtCanvas.DrawText("  scanline",   5, 20+6*12)
			crtCanvas.DrawText("  grb split",  5, 20+7*12)
		
			crtCanvas.DrawText(String(crt.Brightness).Left(4), 100, 20+0*12)
			crtCanvas.DrawText(String(crt.Contrast).Left(4),   100, 20+1*12)
			crtCanvas.DrawText(String(crt.Gamma).Left(4),      100, 20+2*12)
			crtCanvas.DrawText(crt.Resolution.x+"x"+crt.Resolution.y, 100, 20+3*12)
			crtCanvas.DrawText(String(crt.Curvature.x).Left(5)+";"+String(crt.Curvature.y).Left(5), 100, 20+4*12)
			crtCanvas.DrawText(String(crt.Scale.x).Left(5)+";"+String(crt.Scale.y).Left(5), 100, 20+5*12)
			crtCanvas.DrawText(String(crt.ScanlineIntensity).Left(4), 100, 20+6*12)
			crtCanvas.DrawText(String(crt.RGBSplitIntensity).Left(4),   100, 20+7*12)
		
			crtCanvas.DrawText(">", 5, 20+mode*12)
		
			crtCanvas.Color = Color.Green
			crtCanvas.DrawText("• click and drag mouse to", 5, 20+ 9*12)
			crtCanvas.DrawText("  change values",           5, 20+10*12)
			crtCanvas.Color = Color.White
			crtCanvas.DrawText("• press F11 to toggle",     5, 20+11*12)
			crtCanvas.DrawText("  fullscreen mode",         5, 20+12*12)
			crtCanvas.DrawText("• press M to toggle menu",  5, 20+13*12)
			crtCanvas.Color = Color.White
		End
		
		If Keyboard.KeyPressed(Key.Up)   Then mode -= 1
		If Keyboard.KeyPressed(Key.Down) Then mode += 1
		If mode < 0 Then mode = 7
		If mode > 7 Then mode = 0
		
		' draw crt screen on canvas:
		If Float(Width)/Float(Height) < 4.0/3.0 Then
			crt.DrawScreen(canvas, 0, Int(Height-Int(Float(Width)*3.0/4.0))/2.0, Width, Int(Float(Width)*3.0/4.0))
		Else
			crt.DrawScreen(canvas, Int(Width-Int(Float(Height)*4.0/3.0))/2.0, 0, Int(Float(Height)*4.0/3.0), Height)
		End
	End
End

Function Main()
	New AppInstance
	New AppWindow("crt example", App.DesktopSize.x/2 ,App.DesktopSize.y/2)
	App.Run()
End

