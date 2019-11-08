DataSection
  gatewaysa:
    IncludeBinary "gateways"
  gatewaysb:
EndDataSection

  UseSHA2Fingerprint()
  UseMD5Fingerprint()

  Global NewList GateWays.s()
  Global link$
  Global *mem
  Global ok
  
;-------------------------------------------------------------------------------------------------------------------------------------
  
OpenPreferences(GetPathPart(ProgramFilename())+"preferences")
  
  Global download$ = ReadPreferenceString("ipfs","")                     ; <----- The IPFS hash
  
  Global md5$ = ReadPreferenceString("md5","")                           ; <----- The MD5 hash
  
  Global sha256$ = ReadPreferenceString("sha256","")                     ; <----- The SHA256 hash
    
  Global size = Val( ReadPreferenceString("size","") )                   ; <----- Filesize in bytes
  
  Global title$ = ReadPreferenceString("title","")                       ; <----- The window title 
  
  Global saveas$ = ReadPreferenceString("saveas","")                     ; <----- Default file name used in the Savefile dialog 
  
  Global filepattern$ = ReadPreferenceString("pattern","")               ; <----- Filepattern used in the Savefile dialog
  
  Global width = Val( ReadPreferenceString("width","300") )              ; <----- Window width (Default 300)
  
  Global height = Val( ReadPreferenceString("height","65") )             ; <----- Window height (Default 65)
  
ClosePreferences()

;-------------------------------------------------------------------------------------------------------------------------------------
  
Macro DownloadProgress()
  
  p = HTTPProgress(down)
  
Select p
    
Case #PB_HTTP_Failed
  
  Failed()
   
Case #PB_HTTP_Success
  
  CloseWindow(0)
  *mem = FinishHTTP(down)
  
  IntegrityCheck()
  
  Break
  
Default
  
  SetGadgetState(1,p)
  
EndSelect
  
EndMacro    
Macro Timer()
  
If EventTimer()=1
  
If ok=1
  
  RemoveWindowTimer(0,1)
  
  SetGadgetAttribute(1,#PB_ProgressBar_Maximum,size)
  
  down = ReceiveHTTPMemory( link$ , #PB_HTTP_Asynchronous )
  
  AddWindowTimer(0,2,5)
  
Else
  
If IsThread(t)
  KillThread(t)
EndIf

  NextElement(GateWays())
  t=CreateThread(@GetHeader(),UTF8(GateWays()))
    
EndIf 

ElseIf EventTimer()=2
  
  DownloadProgress()
  
EndIf
  
EndMacro
Macro IPFS()
  
  i$ = "http://127.0.0.1:8080/ipfs/"
  
  *ipfs = ReceiveHTTPMemory(i$) 
  
If *ipfs

  FreeMemory(*ipfs)
  
  AddElement( GateWays() )
  GateWays() = i$
  
EndIf
  
EndMacro

Procedure Failed()
  
If IsWindow(0)
  CloseWindow(0)
EndIf

  MessageRequester("","Download failed")
   
  End  
  
EndProcedure
Procedure Save()
      
  s$=SaveFileRequester("Save as...", GetPathPart(ProgramFilename()) + saveas$, filepattern$, 0)
  
If s$
  
If CreateFile(0,s$)
  
  WriteData(0,*mem,size)
  CloseFile(0)
  MessageRequester("","Done")
  
  End
  
Else
  
  MessageRequester("","Unable to save the file")
  Save()
  
EndIf

Else
  
  End
  
EndIf

  ProcedureReturn

EndProcedure
Procedure IntegrityCheck()
  
If Fingerprint(*mem, size, #PB_Cipher_SHA2,256) = sha256$ And Fingerprint(*mem, size, #PB_Cipher_MD5) = md5$
  
  Save()

Else
  
  MessageRequester("", "Integrity check failed")
  
EndIf
  
EndProcedure
Procedure GetSize(l$)
    
  h$ = GetHTTPHeader(l$+download$)
  
  cl$="Content-Length:"
  
  pos = FindString(h$,cl$)
    
If pos
  
  h$ = StringField( Mid(h$,pos) , 1, Chr(10) )
  h$ = Trim( StringField( h$ , 2, ":" ) )
  
  v = Val(h$)
  
EndIf

  ProcedureReturn v
  
EndProcedure
Procedure GetHeader(*a)
    
If GetSize(PeekS(*a,-1,#PB_UTF8)) = size
  
  link$ = PeekS(*a,-1,#PB_UTF8) +download$
  ok=1
    
EndIf
  
  ProcedureReturn

EndProcedure  
Procedure Download()
  
  FirstElement(GateWays())
    
If OpenWindow(0, 0, 0, width, height, title$, #PB_Window_MinimizeGadget|#PB_Window_SystemMenu | #PB_Window_ScreenCentered)
  
  AddWindowTimer(0,1,1000)
  
CompilerIf #PB_Compiler_OS = #PB_OS_Windows
  
  ProgressBarGadget(1,  20, WindowHeight(0) / 2 - 10, WindowWidth(0)-40,  20, 0, 1)
  
CompilerElse
  
  ProgressBarGadget(1,  20, WindowHeight(0) / 2 - 12, WindowWidth(0)-40,  20, 0, 1)
  
CompilerEndIf
  
Repeat
  
Select WaitWindowEvent()
    
Case #PB_Event_Timer
  
  Timer()

Case #PB_Event_CloseWindow
  
  End
  
EndSelect
    
ForEver      
    
EndIf

  ProcedureReturn

EndProcedure
Procedure Main()

  IPFS()

  gw$=PeekS(?gatewaysa,?gatewaysb-?gatewaysa,#PB_UTF8)
  
  c=CountString(gw$,Chr(10))
  
For a=1 To c
  
  AddElement( GateWays() )
  GateWays() = StringField(gw$,a,Chr(10))
    
Next

  Download()
  
EndProcedure

OnErrorCall(@Failed())

If InitNetwork()
  
  Main()
  
Else
  
  MessageRequester("","Couldn't initiliaze the network.")
  End
  
EndIf
; IDE Options = PureBasic 5.70 LTS (Linux - x64)
; Folding = Ao
; EnableThread
; EnableXP
; Executable = downloader
; CompileSourceDirectory