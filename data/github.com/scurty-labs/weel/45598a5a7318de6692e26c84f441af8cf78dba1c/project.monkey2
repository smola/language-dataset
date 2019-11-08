Namespace APPLICATION_WEEL

Function WeelBuildProject(title:String, clean:Bool, release:Bool, target:String)

	Local proj:ProjectConf = New ProjectConf()
	Local projectPath:String = CurrentDir() + title
	
	Print "Locating Project: " + projectPath
	If(proj.LoadJson(projectPath))
		
		Local curDir:String = CurrentDir()
		ChangeDir(projectPath)
		
		If(FileExists(proj.MainFileName+".monkey2"))
			
			CheckDependencies(proj.Depends, target)
			
			If(proj.PreDebug<>"" And Not release) Then libc.system(proj.PreDebug) ' Execute PreDebug Script
			If(proj.PreRelease<>"" And release) Then libc.system(proj.PreRelease) ' Execute PreRelease Script
			
			PROC.BuildProject(proj.MainFileName+".monkey2", clean, release, (proj.Type="gui") ? True Else False , target, proj.Name)
			
			If(proj.PostDebug<>"" And Not release) Then libc.system(proj.PostDebug) ' Execute PostDebug Script
			If(proj.PostRelease<>"" And release) Then libc.system(proj.PostRelease) ' Execute PostRelease Script
			
		Else
			
			Print "Couldn't find '" + proj.MainFileName + ".monkey2" + "'"
			
		Endif
		
		ChangeDir(curDir)
		
	Else
		Print "Couldn't load 'project.json'"
	Endif
	
End

Class ProjectConf

	#REM - 'project.json' FORMAT
	{
		"main":"main", // Main file name
		"name":"%NAME%", // Executable file name
		"type":"gui", // Type of application being worked on (might be useful in the future)
		
		"depends":["...", "..."],
		
		// Optional Post/Pre | Debug/Release Shell Commands/Scripts
		"preDebug":"",
		"postDebug":""',
		
		"preRelease":"",
		"postRelease":""
	}
	
	#END
	
	Field Directory:String
	Field MainFileName:String
	Field Name:String
	Field Type:String
	Field Depends:Stack<JsonValue>
	Field PreDebug:String
	Field PostDebug:String
	Field PreRelease:String
	Field PostRelease:String
	
	Method New()	
	End
	
	Method LoadJson:Bool(path:String)
		Local result:Bool = False
		If(FileExists(path+"/project.json"))
			
			Self.Directory = path
			
			' Required Values
			Local obj:JsonObject = JsonObject.Load(path+"/project.json", True)
			MainFileName = obj["main"].ToString()
			Name = obj["name"].ToString()
			Type = obj["type"].ToString()
			Depends = obj["depends"].ToArray()
			
			' Optional Structures
			PreDebug = obj["preDebug"].ToString()
			PostDebug = obj["postDebug"].ToString()
			PreRelease = obj["preRelease"].ToString()
			PostRelease = obj["postRelease"].ToString()
			
		Endif
		Return True
		
	End
	
End Class


