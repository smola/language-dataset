#tag IOSView
Begin iosView SetNavigationBarColor
   BackButtonTitle =   "Back"
   Compatibility   =   ""
   Left            =   0
   NavigationBarVisible=   True
   TabTitle        =   ""
   Title           =   "Color me"
   Top             =   0
   Begin iOSTextArea TextArea1
      AccessibilityHint=   ""
      AccessibilityLabel=   ""
      AutoLayout      =   TextArea1, 8, , 0, False, +1.00, 1, 1, 200, 
      AutoLayout      =   TextArea1, 2, <Parent>, 2, False, +1.00, 1, 1, 0, 
      AutoLayout      =   TextArea1, 1, <Parent>, 1, False, +1.00, 1, 1, 0, 
      AutoLayout      =   TextArea1, 3, <Parent>, 3, False, +1.00, 1, 1, 87, 
      Editable        =   True
      Height          =   200.0
      KeyboardType    =   "0"
      Left            =   0
      LockedInPosition=   False
      Scope           =   0
      Text            =   "For more information, see the note attached to this view\n"
      TextAlignment   =   "0"
      TextColor       =   &c00000000
      TextFont        =   ""
      TextSize        =   0
      Top             =   87
      Visible         =   True
      Width           =   320.0
   End
   Begin iOSButton Button1
      AccessibilityHint=   ""
      AccessibilityLabel=   ""
      AutoLayout      =   Button1, 8, , 0, False, +1.00, 1, 1, 30, 
      AutoLayout      =   Button1, 9, <Parent>, 9, False, +1.00, 1, 1, 0, 
      AutoLayout      =   Button1, 7, , 0, False, +1.00, 1, 1, 100, 
      AutoLayout      =   Button1, 3, <Parent>, 3, False, +1.00, 1, 1, 306, 
      Caption         =   "Color in Red"
      Enabled         =   True
      Height          =   30.0
      Left            =   110
      LockedInPosition=   False
      Scope           =   0
      TextColor       =   &c007AFF00
      TextFont        =   ""
      TextSize        =   0
      Top             =   306
      Visible         =   True
      Width           =   100.0
   End
   Begin iOSButton Button2
      AccessibilityHint=   ""
      AccessibilityLabel=   ""
      AutoLayout      =   Button2, 8, , 0, False, +1.00, 1, 1, 30, 
      AutoLayout      =   Button2, 9, <Parent>, 9, False, +1.00, 1, 1, 0, 
      AutoLayout      =   Button2, 7, , 0, False, +1.00, 1, 1, 130, 
      AutoLayout      =   Button2, 3, Button1, 4, False, +1.00, 1, 1, *kStdControlGapV, 
      Caption         =   "For tabbed view"
      Enabled         =   True
      Height          =   30.0
      Left            =   95
      LockedInPosition=   False
      Scope           =   0
      TextColor       =   &c007AFF00
      TextFont        =   ""
      TextSize        =   0
      Top             =   344
      Visible         =   False
      Width           =   130.0
   End
End
#tag EndIOSView

#tag WindowCode
	#tag Event
		Sub Open()
		  'app.ShowStatusBar(self)
		End Sub
	#tag EndEvent


	#tag Note, Name = Change navigationBar color
		
		https://forum.xojo.com/18202-navigationbar-color
		
		Richard Berglund 5 hours ago Beta Testers
		Hi,
		
		With the help from Jim Mckay. Changing the color of the NavigationBar.
		
		Private Function UIColor(c as Color) As Ptr
		// returns a Ptr to a new Uicolor Created from a Xojo Color
		declare function colorFromRGBA lib UIKit selector "colorWithRed:green:blue:alpha:" (id as Ptr, red as Single, green as Single, blue as Single, alpha as Single) as Ptr
		dim r as single = c.red/255
		dim g as single = c.Green/255
		dim b as single = c.Blue/255
		dim a as single = (255 - c.Alpha) / 255
		
		return colorFromRGBA(NSClassFromString ("UIColor"), r, g, b, a)
		End Function
		
		declare function NSClassFromString      lib "Foundation" (classname as CFStringRef) as ptr
		declare function keyWindow              lib "UIKit" selector "keyWindow"  (obj_ref as ptr) as ptr
		declare function sharedApplication      lib "UIKit" selector "sharedApplication"  (obj_ref as ptr) as ptr
		declare function rootViewController      lib "UIKit" selector "rootViewController"  (obj_ref as ptr) as ptr
		declare function navigationBar          lib "UIKit" selector "navigationBar"  (obj_ref as ptr) as ptr
		
		dim sApp as ptr=sharedApplication(NSClassFromString("UIApplication"))
		dim currentWindow as ptr=keyWindow(sApp)
		dim navController as ptr=rootViewController(currentWindow)
		dim navBar as ptr= navigationBar(navController)
		
		Declare Sub setBarTintColor lib UIKit selector "setBarTintColor:" (id as ptr, UIColor as Ptr)
		setBarTintColor navBar, UIColor(&cFF0000)
		
		declare Sub setTintColor lib UIKit selector "setTintColor:" (id as ptr, UIColor as Ptr)
		setTintColor navBar, UIColor(&cFF7700)
		
		declare sub setTranslucent lib UIKit selector "setTranslucent:" (id as ptr)
		setTranslucent navBar
		You can set it to translucent or solid by excluding setTranslucent.
		
		For changing the text color for the top text. I'm still trying to figure out.
		
		let titleDict: NSDictionary = [NSForegroundColorAttributeName: UIColor.whiteColor()]
		self.navigationController.navigationBar.titleTextAttributes = titleDict
		
		or
		[self.navigationController.navigationBar setTitleTextAttributes:@{NSForegroundColorAttributeName : [UIColor whiteColor]}];
	#tag EndNote


#tag EndWindowCode

#tag Events Button1
	#tag Event
		Sub Action()
		  declare function NSClassFromString      lib "Foundation" (classname as CFStringRef) as ptr
		  declare function keyWindow              lib "UIKit" selector "keyWindow"  (obj_ref as ptr) as ptr
		  declare function sharedApplication      lib "UIKit" selector "sharedApplication"  (obj_ref as ptr) as ptr
		  declare function rootViewController      lib "UIKit" selector "rootViewController"  (obj_ref as ptr) as ptr
		  declare function navigationBar          lib "UIKit" selector "navigationBar"  (obj_ref as ptr) as ptr
		  
		  dim sApp as ptr=sharedApplication(NSClassFromString("UIApplication"))
		  dim currentWindow as ptr=keyWindow(sApp)
		  dim navController as ptr=rootViewController(currentWindow)
		  dim navBar as ptr= navigationBar(navController)
		  
		  Declare Sub setBarTintColor lib UIKit selector "setBarTintColor:" (id as ptr, UIColor as Ptr)
		  setBarTintColor navBar, UIColor(&cFF0000)
		  
		  declare Sub setTintColor lib UIKit selector "setTintColor:" (id as ptr, UIColor as Ptr)
		  setTintColor navBar, UIColor(&cFF7700)
		  
		  declare sub setTranslucent lib UIKit selector "setTranslucent:" (id as ptr)
		  setTranslucent navBar
		End Sub
	#tag EndEvent
#tag EndEvents
#tag Events Button2
	#tag Event
		Sub Action()
		  // Use this code for view in a tabbed view setting
		  
		  declare function NSClassFromString      lib "Foundation" (classname as CFStringRef) as ptr
		  declare function keyWindow              lib "UIKit" selector "keyWindow"  (obj_ref as ptr) as ptr
		  declare function sharedApplication      lib "UIKit" selector "sharedApplication"  (obj_ref as ptr) as ptr
		  declare function rootViewController      lib "UIKit" selector "rootViewController"  (obj_ref as ptr) as ptr
		  declare function navigationBar          lib "UIKit" selector "navigationBar"  (obj_ref as ptr) as ptr
		  
		  declare function navigationController lib "UIKit" selector "navigationController" (viewController as ptr) as ptr
		  dim navigationControllerRef as ptr = navigationController(self.ViewControllerHandle) //self is the iOSView
		  
		  dim navigationBarRef as ptr = navigationBar(navigationControllerRef)
		  
		  dim sApp as ptr=sharedApplication(NSClassFromString("UIApplication"))
		  dim currentWindow as ptr=keyWindow(sApp)
		  dim navController as ptr=rootViewController(currentWindow)
		  dim navBar as ptr= navigationBar(navigationControllerRef)
		  
		  
		  
		  Declare Sub setBarTintColor lib UIKit selector "setBarTintColor:" (id as ptr, UIColor as Ptr)
		  setBarTintColor navBar, UIColor(&cFF0000)
		  
		  declare Sub setTintColor lib UIKit selector "setTintColor:" (id as ptr, UIColor as Ptr)
		  setTintColor navBar, UIColor(&cFF7700)
		  
		  declare sub setTranslucent lib UIKit selector "setTranslucent:" (id as ptr)
		  setTranslucent navBar
		  
		  
		End Sub
	#tag EndEvent
#tag EndEvents
#tag ViewBehavior
	#tag ViewProperty
		Name="BackButtonTitle"
		Group="Behavior"
		Type="Text"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Index"
		Visible=true
		Group="ID"
		InitialValue="-2147483648"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Left"
		Visible=true
		Group="Position"
		InitialValue="0"
		Type="Integer"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Name"
		Visible=true
		Group="ID"
		Type="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="NavigationBarVisible"
		Group="Behavior"
		Type="Boolean"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Super"
		Visible=true
		Group="ID"
		Type="String"
	#tag EndViewProperty
	#tag ViewProperty
		Name="TabTitle"
		Group="Behavior"
		Type="Text"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Title"
		Group="Behavior"
		Type="Text"
		EditorType="MultiLineEditor"
	#tag EndViewProperty
	#tag ViewProperty
		Name="Top"
		Visible=true
		Group="Position"
		InitialValue="0"
		Type="Integer"
	#tag EndViewProperty
#tag EndViewBehavior
