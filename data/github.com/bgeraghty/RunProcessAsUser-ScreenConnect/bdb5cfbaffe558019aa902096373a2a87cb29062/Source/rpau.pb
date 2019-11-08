Prototype protoWTSGetActiveConsoleSessionId() : Global WTSGetActiveConsoleSessionId.protoWTSGetActiveConsoleSessionId ; Prototype for WTSGetActiveConsoleSessionId() @ kernel32.dll
Prototype.b protoWTSQueryUserToken(SessionId, phToken) : Global WTSQueryUserToken.protoWTSQueryUserToken ; Prototype for WTSQueryUserToken() @ wtsapi32.dll
Prototype.b protoCreateEnvironmentBlock(lpEnvironment, hToken, bInherit.b) : Global CreateEnvironmentBlock.protoCreateEnvironmentBlock ; Prototype for CreateEnvironmentBlock() @ userenv.dll
Prototype.b protoDestroyEnvironmentBlock(lpEnvironment) : Global DestroyEnvironmentBlock.protoDestroyEnvironmentBlock ; Prototype for DestroyEnvironmentBlock() @ userenv.dll

Global ArgC.i = CountProgramParameters() ; Total Arguement Count
Global sParamC.i = 0 ; Count of Arguements that are not --options (String Values)
Global NewList Params.s() ; String List to hold Parameters / Arguements.
Global RunProgramHidden.i = #False ; Default Option To Run As User With All Windows Hidden.
Global ReturnUserName$ = "" ; Store the username, if it is recorded.

Procedure RPAU_(lpApplicationName.s, lpCommandLine.s = #Null$, lpCurrentDirectory.s = #Null$, Hide.b = #False, Mode$ = "NORMAL") ; Run Process As User in Session
  PrintN("")
  kernel32 = OpenLibrary(#PB_Any, "kernel32.dll") ; Open Requred DLLs, Assign Functions     
  If IsLibrary(kernel32)
    WTSGetActiveConsoleSessionId = GetFunction(kernel32, "WTSGetActiveConsoleSessionId")
  Else 
    PrintN("Error: Failed to access 'kernel32.dll'.")
  EndIf
  wtsapi32 = OpenLibrary(#PB_Any, "wtsapi32.dll")
  If IsLibrary(wtsapi32)
    WTSQueryUserToken = GetFunction(wtsapi32, "WTSQueryUserToken")
  Else
    PrintN("Error: Failed to access 'wtsapi32.dll'.")
  EndIf
  userenv = OpenLibrary(#PB_Any, "userenv.dll")
  If IsLibrary(userenv)
    CreateEnvironmentBlock = GetFunction(userenv, "CreateEnvironmentBlock")
    DestroyEnvironmentBlock = GetFunction(userenv, "DestroyEnvironmentBlock")
  Else
    PrintN("Error: Failed to access 'userenv.dll'.")
  EndIf
  Result.b = #False ; Success 
  dwSessionId = WTSGetActiveConsoleSessionId() ; Get SessionID of User
  If dwSessionId
    If WTSQueryUserToken(dwSessionId, @TokenHandle) ; Get User Token
      #SecurityImpersonation = 2
      #TokenPrimary = 1
      If DuplicateTokenEx_(TokenHandle, #MAXIMUM_ALLOWED, #Null, #SecurityImpersonation, #TokenPrimary, @ImpersonateToken) ; Duplicate Token
        If ImpersonateLoggedOnUser_(ImpersonateToken) ; Impersonate User
          If CreateEnvironmentBlock(@pEnvironment, ImpersonateToken, #False) ; Create Environment Block As User
            Select Mode$
              Case "NORMAL"
                #CREATE_UNICODE_ENVIRONMENT = $400
                dwCreationFlag = #NORMAL_PRIORITY_CLASS | #CREATE_NEW_CONSOLE | #CREATE_UNICODE_ENVIRONMENT
                si.STARTUPINFO
                ZeroMemory_(@si, SizeOf(STARTUPINFO))
                si\cb = SizeOf(STARTUPINFO)
                si\lpDesktop = @"WinSta0\Default"
                si\dwFlags = #STARTF_USESHOWWINDOW
                Select Hide ; Option to Run Program Hidden
                  Case #True
                    si\wShowWindow = #SW_HIDE
                  Case #False
                    si\wShowWindow = #SW_SHOWNORMAL
                EndSelect
                pi.PROCESS_INFORMATION ; Struct to catch resulting Process Information
                ZeroMemory_(@pi, SizeOf(PROCESS_INFORMATION))
                Result = CreateProcessAsUser_(ImpersonateToken, lpApplicationName, lpCommandLine, #Null, #Null, #False, dwCreationFlag, pEnvironment, lpCurrentDirectory.s, @si, @pi) ; Create Process As User
              Case "USERNAME"
                ReturnUserName$ = UserName()
            EndSelect
            SetLastError_(0)
            DestroyEnvironmentBlock(pEnvironment) ; End Environment Block As User
          Else
            PrintN("Error: Failed to create Environment Block.")
          EndIf
          RevertToSelf_()
        Else
          PrintN("Error: Failed to impersonate Logged On User.")
        EndIf 
        CloseHandle_(ImpersonateToken)
      Else
        PrintN("Error: Failed to duplicate User Token.")
      EndIf 
      CloseHandle_(TokenHandle)
    Else
      PrintN("Error: Failed to query User Token.")
    EndIf 
  Else
    PrintN("Error: Failed to get Active Console Session ID.")
  EndIf 
  Select Mode$
    Case "NORMAL"
      Select Result ; Check Results
        Case #True
          PrintN("The Program Was Executed In Session: "+Str(dwSessionId))
        Default
          PrintN("The Program Failed To Execute. Please Check Syntax of Commandline Text.")
      EndSelect
    Case "USERNAME"
      PrintN("Console Session["+Str(dwSessionId)+"] UserName: " + ReturnUserName$)
  EndSelect
  If IsLibrary(userenv) : CloseLibrary(userenv) : EndIf ; Close DLLs
  If IsLibrary(wtsapi32) : CloseLibrary(wtsapi32) : EndIf
  If IsLibrary(kernel32) : CloseLibrary(kernel32) : EndIf
EndProcedure

Procedure PrintVersion()
  PrintN("")
  PrintN("       [Run Process As User] v0."+Str(#PB_Editor_BuildCount)+"."+Str(#PB_Editor_CompileCount))
  PrintN("")
EndProcedure

Procedure PrintHelp()
  PrintVersion()
  PrintN("Syntax1: rpau.exe --hidden "+#DQUOTE$+"<ProgramName>"+#DQUOTE$+" "+#DQUOTE$+"<CmdLineArgs>"+#DQUOTE$+" "+#DQUOTE$+"<WorkingDirectory>"+#DQUOTE$)
  PrintN("       : You can optionally include "+#DQUOTE$+"--hidden"+#DQUOTE$+" to visibly hide the executed")
  PrintN("       : program. Enclose parameters in double-quotes as shown.")
  PrintN("       : The only required parameter is <ProgramName>. However, if you want")
  PrintN("       : to specify <WorkingDirectory> without specifying <CmdLineArgs>,") 
  PrintN("       : <CmdLineArgs> must still be passed as: "+#DQUOTE$+#DQUOTE$+". (Empty String).")
  PrintN("")
  PrintN("Example: rpau.exe "+#DQUOTE$+"cmd.exe"+#DQUOTE$+" "+#DQUOTE$+"/c ipconfig > test.txt"+#DQUOTE$+" "+#DQUOTE$+"C:\ProgramData"+#DQUOTE$)
  PrintN("")
  PrintN("")
  PrintN("Syntax2: rpau.exe --username")
  PrintN("       : Returns the Username of the account in the Console Session. No ")
  PrintN("       : Additional Parameters Expected.")
  PrintN("")
  PrintN("Example: rpau.exe --username")
  PrintN("")
EndProcedure

Procedure Main()
  OpenConsole()
  PrintN("")
  If ArgC >= 1
    For I = 1 To ArgC
      CurrentParam$ = ProgramParameter()
      Select Left(CurrentParam$, 2)
        Case "--"
          Select UCase(Right(CurrentParam$, Len(CurrentParam$) - 2))
            Case "HIDDEN"
              PrintN("'--hidden' Switch Detected, Program Output Will Be Suppressed...")
              RunProgramHidden = #True
            Case "USERNAME"
              RPAU_(#Null$, #Null$, #Null$, #False, "USERNAME")
              End
            Default
              PrintN("Error: Unknown Switch: '"+CurrentParam$+"'. Aborting...")
              PrintHelp()
              End
          EndSelect
        Default
          sParamC + 1
          AddElement(Params())
          If CurrentParam$ = "" : CurrentParam$ = #Null$ : EndIf
          Params() = CurrentParam$
      EndSelect
    Next
    FirstElement(Params())
    Param1$ = "" : Param2$ = "" : Param3$ = ""
    Select sParamC
      Case 0
        PrintN("Error: No Parameters Given.")
        PrintHelp()
        End
      Case 1
        Param1$ = Params()
        PrintN("")
        PrintN("Attempting To Run: " + #DQUOTE$ + Param1$ + #DQUOTE$)
        RPAU_(Param1$, #Null$, #Null$, RunProgramHidden, "NORMAL")
      Case 2
        Param1$ = Params()
        NextElement(Params())
        Param2$ = Params()
        PrintN("")
        PrintN("Attempting To Run: " + #DQUOTE$ + Param1$ + " " + Param2$ + #DQUOTE$)
        RPAU_(Param1$, Param2$, #Null$, RunProgramHidden, "NORMAL")
      Case 3
        Param1$ = Params()
        NextElement(Params())
        Param2$ = Params()
        NextElement(Params())
        Param3$ = Params()
        PrintN("")
        PrintN("Attempting To Run: " + #DQUOTE$ + Param1$ + " " + Param2$ + #DQUOTE$ + " in Directory: " + #DQUOTE$ + Param3$ + #DQUOTE$)
        RPAU_(Param1$, Param2$, Param3$, RunProgramHidden, "NORMAL")
      Default
        PrintN("Error: Too Many Parameters, Cannot Be Greater Than 3")
        PrintHelp()
        End
    EndSelect
  Else
    PrintN("Error: No Options / Parameters Given.")
    PrintHelp()
    End
  EndIf
EndProcedure

Select UserName()
  Case "SYSTEM"
    Main()
  Default
    End
EndSelect
; IDE Options = PureBasic 5.41 LTS (Windows - x86)
; ExecutableFormat = Console
; CursorPosition = 100
; Folding = w
; EnableThread
; Executable = rpau.exe
; DisableDebugger
; EnableCompileCount = 132
; EnableBuildCount = 33