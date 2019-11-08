#tag Module
Protected Module TPSF
	#tag Method, Flags = &h1
		Protected Function AppParent() As FolderItem
		  return App.ExecutableFile.Parent
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function AppSupport() As FolderItem
		  dim fReturn as FolderItem = SpecialFolder.ApplicationData
		  
		  // Prevent NOEs
		  if fReturn = nil then return nil
		  
		  #if TargetMacOS then
		    return fReturn.Child(App.BundleIdentifier)
		    
		  #elseif TargetWin32 then
		    return fReturn.Child(ThisAppName)
		    
		  #elseif TargetLinux then
		    return fReturn.Child("." + ThisAppName)
		    
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function BundleIdentifier(Extends a as Application) As String
		  #pragma unused a
		  static mBundleIdentifier as string
		  
		  #if TargetMacOS
		    if mBundleIdentifier = "" then
		      declare function mainBundle lib "AppKit" selector "mainBundle" (NSBundleClass as Ptr) as Ptr
		      declare function NSClassFromString lib "AppKit" (className as CFStringRef) as Ptr
		      declare function getValue lib "AppKit" selector "bundleIdentifier" (NSBundleRef as Ptr) as CfStringRef
		      mBundleIdentifier = getValue(mainBundle(NSClassFromString("NSBundle")))
		    end if
		  #endif
		  
		  return mBundleIdentifier
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function BundleParent() As FolderItem
		  #if TargetMacOS then
		    static mBundlePath as folderitem
		    
		    if mBundlePath = nil or mBundlePath.exists = false then
		      declare function NSClassFromString lib "AppKit" (className as CFStringRef) as Ptr
		      declare function mainBundle lib "AppKit" selector "mainBundle" (NSBundleClass as Ptr) as Ptr
		      declare function resourcePath lib "AppKit" selector "bundlePath" (NSBundleRef as Ptr) as CfStringRef
		      mBundlePath = GetFolderItem(resourcePath(mainBundle(NSClassFromString( "NSBundle"))), FolderItem.PathTypeNative)
		    end if
		    
		    return mBundlePath
		    
		  #elseif TargetWin32 or TargetLinux then
		    return App.ExecutableFile.Parent
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Contents() As FolderItem
		  #if TargetMacOS then
		    return App.ExecutableFile.Parent.Parent
		  #elseif TargetWin32 or TargetLinux then
		    return App.ExecutableFile.Parent
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Frameworks() As FolderItem
		  #if TargetMacOS then
		    static mFrameworks as folderitem
		    
		    if mFrameworks = nil or mFrameworks.exists = false then
		      declare function NSClassFromString lib "AppKit" (className as CFStringRef) as Ptr
		      declare function mainBundle lib "AppKit" selector "mainBundle" (NSBundleClass as Ptr) as Ptr
		      declare function resourcePath lib "AppKit" selector "privateFrameworksPath" (NSBundleRef as Ptr) as CfStringRef
		      mFrameworks = GetFolderItem(resourcePath(mainBundle(NSClassFromString("NSBundle"))), FolderItem.PathTypeNative)
		    end if
		    
		    return mFrameworks
		    
		  #elseif TargetWin32 or TargetLinux then
		    dim fLibsFolder as FolderItem = App.ExecutableFile.Parent.Child("Libs")
		    
		    // Old style libs folder?
		    if fLibsFolder <> nil and fLibsFolder.exists = true then
		      return fLibsFolder
		    end
		    
		    // New style libs folder?
		    fLibsFolder = App.ExecutableFile.Parent.Child(ThisAppName + " Libs")
		    if fLibsFolder <> nil and fLibsFolder.exists = true then
		      return fLibsFolder
		    end
		    
		    // Neither was found at this point.
		    return nil
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h1
		Protected Function Resources() As FolderItem
		  #if TargetMacOS then
		    static mResourcesFolder as folderitem
		    
		    if mResourcesFolder = nil or mResourcesFolder.exists = false then
		      declare function NSClassFromString lib "AppKit" (className as CFStringRef) as Ptr
		      declare function mainBundle lib "AppKit" selector "mainBundle" (NSBundleClass as Ptr) as Ptr
		      declare function resourcePath lib "AppKit" selector "resourcePath" (NSBundleRef as Ptr) as CfStringRef
		      mResourcesFolder = GetFolderItem(resourcePath(mainBundle(NSClassFromString("NSBundle"))), FolderItem.PathTypeNative)
		    end if
		    
		    return mResourcesFolder
		    
		  #elseif TargetWin32 or TargetLinux then
		    dim fLibsFolder as FolderItem = App.ExecutableFile.Parent.Child("Resources")
		    
		    // Old style resources folder?
		    if fLibsFolder <> nil and fLibsFolder.exists = true then
		      return fLibsFolder
		    end
		    
		    // New style resources folder?
		    fLibsFolder = App.ExecutableFile.Parent.Child(ThisAppName + " Resources")
		    if fLibsFolder <> nil and fLibsFolder.exists = true then
		      return fLibsFolder
		    end
		    
		    // Neither was found at this point.
		    return nil
		  #endif
		End Function
	#tag EndMethod

	#tag Method, Flags = &h21
		Private Function ThisAppName() As String
		  return ReplaceAll(App.ExecutableFile.Name, ".exe", "")
		  
		End Function
	#tag EndMethod


	#tag Note, Name = About
		NOTE: This module requires that your root Application object be named "App"
		
		About:
		     This module makes it easier to access executable relative files since Xojo
		     does not include a SepcialFolder.Resources or the like.
		
		Thanks to:
		     Sam Rowlands with the Mac declares
		     Axel Schneider with Linux locations
		
		Usage:
		     To access "Copy Files" build step locations:
		         TPSF.AppParent
		         TPSF.BundleParent
		         TPSF.Contents
		         TPSF.Frameworks
		         TPSF.Resources
		
		
		     Direct access to your own Application Support Folder:
		         TPSF.AppSupport
		
		     This will return a FolderItem with the path:
		           Mac: ~/Library/Application Support/[bundle identifier]
		           Win: \Users\[user]\AppData\Roaming\[app name]
		         Linux: /home/UserName/.[app name]
		
		
		     To get your app's Bundle Identifier via proper Cocoa declares:
		         App.BundleIdentifier
	#tag EndNote


	#tag ViewBehavior
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
			Name="Super"
			Visible=true
			Group="ID"
			Type="String"
		#tag EndViewProperty
		#tag ViewProperty
			Name="Top"
			Visible=true
			Group="Position"
			InitialValue="0"
			Type="Integer"
		#tag EndViewProperty
	#tag EndViewBehavior
End Module
#tag EndModule
