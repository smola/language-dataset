

   MEMBER('MPlayer.clw')                                   ! This is a MEMBER module


   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE

                     MAP
                       INCLUDE('MPLAYER001.INC'),ONCE        !Local module procedure declarations
                     END


!!! <summary>
!!! Generated from procedure template - Window
!!! Window
!!! </summary>
Main PROCEDURE 

LOC:MyPath           CSTRING(255)                          !
LOC:SelectedFile     CSTRING(255)                          !
QuickWindow          WINDOW,AT(0,0,150,517),FONT('MS Sans Serif',8,,FONT:regular,CHARSET:DEFAULT),NOFRAME,TILED, |
  AUTO,IMM,HLP('Main'),TOOLBOX,WALLPAPER('Dark-BG.jpg')
                       BUTTON('Mini mode'),AT(16,87,51,45),USE(?Mini),FONT(,,00F5F5F5h),ICON('off.png'),FLAT,SKIP, |
  TRN
                       BUTTON('Full mode'),AT(85,87,51,45),USE(?Full),FONT(,,00F5F5F5h),ICON('off.png'),FLAT,SKIP, |
  TRN
                       BUTTON('None mode'),AT(16,140,51,45),USE(?None),FONT(,,00F5F5F5h),ICON('off.png'),FLAT,SKIP, |
  TRN
                       BUTTON('Play Random Media'),AT(9,295,127,71),USE(?None:3),FONT(,,00F5F5F5h),LEFT,ICON('random.png'), |
  FLAT,SKIP,TRN
                       BUTTON('Invisible mode'),AT(85,140,51,45),USE(?Invisible),FONT(,,00F5F5F5h),ICON('off.png'), |
  FLAT,SKIP,TRN
                       IMAGE('text.png'),AT(9,7),USE(?Media_Demo)
                       STRING('Player options'),AT(6,65,76),USE(?STRING1),FONT(,,00E0E0E0h),COLOR(COLOR:Black),TRN
                       STRING('Other options'),AT(6,200,76,10),USE(?STRING1:2),FONT(,,00E0E0E0h),COLOR(COLOR:Black), |
  TRN
                       BUTTON('Full Screen'),AT(16,222,51,45),USE(?FullScreen_Videos),FONT(,,00F5F5F5h),ICON('off.png'), |
  FLAT,SKIP,TRN
                       BUTTON('Load Media'),AT(9,369,127,71),USE(?LookupFile),FONT(,,00F5F5F5h),LEFT,ICON('load.png'), |
  FLAT,SKIP,TRN
                       BUTTON('Play test after mode change'),AT(85,222,51,45),USE(?Test),FONT(,,00F5F5F5h),ICON('off.png'), |
  FLAT,SKIP,TRN
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeAccepted           PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END

FileLookup2          SelectFileClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
Play_Random_Media_ROUTINE   ROUTINE
        
    R# = RANDOM(1,7)
    CASE R#
        OF 1
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'Ladybug Full HD.mp4"'
        OF 2
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'piano_inside_1280x720.mp4"'
        OF 3
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'sunflower-like_1280x720.mp4"'
        OF 4
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'I love SoftVelocity.wav"'
        OF 5
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'Productivity.wav"'
        OF 6
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'rapid.wav"'
        OF 7
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'secret weapon.wav"'

    END !CASE

        POST(EVENT:Playmedia,,GLO:MplayerHandle)
Start_Media_Player_ROUTINE  ROUTINE
    GLO:MplayerHandle = START(Media_Player,25000)
Query_Modes_ROUTINE ROUTINE

        ?Full{PROP:Icon} = 'off.png'
        ?Mini{PROP:Icon} = 'off.png'
        ?None{PROP:Icon} = 'off.png'
        ?Invisible{PROP:Icon} = 'off.png'
    
        CASE GLO:uiMode
            OF 'mini'        
                ?Mini{PROP:Icon} = 'on.png'
            OF 'full'
                ?Full{PROP:Icon} = 'on.png'
            OF 'none'
                ?None{PROP:Icon} = 'on.png'
            OF 'invisible'
                ?Invisible{PROP:Icon} = 'on.png'
        END !CASE
    
        IF GLO:FullScreen = TRUE
            ?FullScreen_Videos{PROP:Icon} = 'on.png'
        ELSE
            
            ?FullScreen_Videos{PROP:Icon} = 'off.png'
        END !IF
        
        IF GLO:Test = TRUE
            ?Test{PROP:Icon} = 'on.png'
        ELSE
            ?Test{PROP:Icon} = 'off.png'
        END !IF
    
        

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Main')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Mini
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  QuickWindow{PROP:MinWidth} = 150                         ! Restrict the minimum window width
  QuickWindow{PROP:MinHeight} = QuickWindow{PROP:Height}   ! Restrict the minimum window height to the 'design time' height
  QuickWindow{PROP:MaxWidth} = 150                         ! Restrict the maximum window width
  QuickWindow{PROP:MaxHeight} = QuickWindow{PROP:Height}   ! Restrict the maximum window height to the 'design time' height
  Resizer.Init(AppStrategy:Spread)                         ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  FileLookup2.Init
  FileLookup2.ClearOnCancel = True
  FileLookup2.Flags=BOR(FileLookup2.Flags,FILE:LongName)   ! Allow long filenames
  FileLookup2.SetMask('All Media Files','*.avi;*.mp4;*.mp3;*.wav;*.aif') ! Set the file mask
  FileLookup2.DefaultDirectory='.\'
  FileLookup2.WindowTitle='Please select a video or audio file'
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeAccepted PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receive all EVENT:Accepted's
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeAccepted()
    CASE ACCEPTED()
    OF ?Mini
      ThisWindow.Update()
          GLO:uiMode = 'mini'
          POST(EVENT:CloseWindow,,GLO:MplayerHandle)
          DO Start_Media_Player_ROUTINE
          DO Query_Modes_ROUTINE
    OF ?Full
      ThisWindow.Update()
          GLO:uiMode = 'full'
          POST(EVENT:CloseWindow,,GLO:MplayerHandle)
          DO Start_Media_Player_ROUTINE
          DO Query_Modes_ROUTINE
    OF ?None
      ThisWindow.Update()
          GLO:uiMode = 'none'
          POST(EVENT:CloseWindow,,GLO:MplayerHandle)
          DO Start_Media_Player_ROUTINE
          DO Query_Modes_ROUTINE
    OF ?None:3
      ThisWindow.Update()
      DO Play_Random_Media_ROUTINE
    OF ?Invisible
      ThisWindow.Update()
          GLO:uiMode = 'invisible'
          POST(EVENT:CloseWindow,,GLO:MplayerHandle)
          DO Start_Media_Player_ROUTINE
          DO Query_Modes_ROUTINE
    OF ?FullScreen_Videos
      ThisWindow.Update()
      IF  GLO:FullScreen = TRUE
          GLO:FullScreen = FALSE
      ELSE    
          GLO:FullScreen = TRUE
      END !IF
      
      DO Query_Modes_ROUTINE
      
      
    OF ?LookupFile
      ThisWindow.Update()
      LOC:SelectedFile = FileLookup2.Ask(1)
      DISPLAY
      IF LOC:SelectedFile ~= ''
          GLO:MediaFile = LOC:SelectedFile
          POST(EVENT:Playmedia,,GLO:MplayerHandle)
      ELSE
          MESSAGE('Please select a media file...')
      END
          
    OF ?Test
      ThisWindow.Update()
          IF  GLO:Test = TRUE
              GLO:Test = FALSE
          ELSE    
              GLO:Test = TRUE
          END !IF
      
          DO Query_Modes_ROUTINE
      
      
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
      0{PROP:Buffer} = 1
      0{PROP:MaxWidth} = 150
      DO Query_Modes_ROUTINE
      DO Start_Media_Player_ROUTINE
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Window
!!! Window
!!! </summary>
Media_Player PROCEDURE 

LOC:MyPath           CSTRING(255)                          !
QuickWindow          WINDOW,AT(,,415,224),FONT('MS Sans Serif',8,COLOR:White,FONT:regular,CHARSET:DEFAULT),CENTERED, |
  AUTO,CENTER,COLOR(COLOR:Black),DOCK(DOCK:Float),IMM,HLP('Main'),TIMER(1000),TOOLBOX,WALLPAPER('Dark-BG.jpg')
                       OLE,AT(0,0,415,224),USE(?WMP)
                       END
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeEvent              PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass
Resizer              CLASS(WindowResizeClass)
Init                   PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)
                     END


  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------
Build_Media_Player_ROUTINE  ROUTINE
    
        ?WMP{prop:create}='WMPlayer.OCX.7'
        ?WMP{prop:reportexception} = true 
        ?WMP{'enableContextMenu'} = 'false'
        ?WMP{'WindowlessVideo'} = 'true'
        ?WMP{'stretchToFit'} = 'true'    
    
        IF GLO:uiMode = ''
            GLO:uiMode = 'none'
        ELSE
            ?WMP{'uiMode'} = GLO:uiMode    
        END !IF
PlayMedia_ROUTINE   ROUTINE
    ?WMP{'URL'} = GLO:MediaFile

    IF GLO:FullScreen = TRUE
        DO FullScreen_ROUTINE
    END
    
     
FullScreen_ROUTINE  ROUTINE
    !Going full screen using OLE automation requires a Message to be displayed, using an alternate method
    !MESSAGE('Going full screen, press ESC or double click the video to return')
    !?WMP{'FullScreen'} = 1
    0{PROP:Timer} = 100 !Let's increase the freq of our Timer
    0{PROP:Maximize} = TRUE

    
Restore_FullScreen_ROUTINE  ROUTINE

    IF ?WMP{'Playstate'} = 1 AND GLO:FullScreen = TRUE       
        0{PROP:Maximize} = FALSE
        0{PROP:Timer} = 1000    !Let's restore the timer to 1000
    END
    
    
    

    
Play_Random_Media_ROUTINE   ROUTINE
        
    R# = RANDOM(1,7)
    CASE R#
        OF 1
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'Ladybug Full HD.mp4"'
        OF 7
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'piano_inside_1280x720.mp4"'
        OF 3
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'sunflower-like_1280x720.mp4"'
        OF 5
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'I love SoftVelocity.wav"'
        OF 4
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'Productivity.wav"'
        OF 6
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'rapid.wav"'
        OF 2
        GLO:MediaFile = '"FILE:///' & LONGPATH() & '\' & 'secret weapon.wav"'

    END !CASE

        POST(EVENT:Playmedia,,GLO:MplayerHandle)

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Media_Player')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = 1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(QuickWindow)                                   ! Open window
  Do DefineListboxStyle
  Resizer.Init(AppStrategy:Spread,Resize:SetMinSize)       ! Controls will spread out as the window gets bigger
  SELF.AddItem(Resizer)                                    ! Add resizer to window manager
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  CASE EVENT()
      OF EVENT:PlayMedia
      DO PlayMedia_ROUTINE
      OF EVENT:FullScreen
      DO FullScreen_ROUTINE
      OF EVENT:Timer
      DO Restore_FullScreen_ROUTINE
  END
  ReturnValue = PARENT.TakeEvent()
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
          0{PROP:Buffer} = 1
          !let's build the OLE Media Player Control
          DO Build_Media_Player_ROUTINE
      
          IF GLO:Test = TRUE
              DO Play_Random_Media_ROUTINE
          END
          
          IF GLO:uiMode = 'invisible'
              0{PROP:Hide} = TRUE
          END
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue


Resizer.Init PROCEDURE(BYTE AppStrategy=AppStrategy:Resize,BYTE SetWindowMinSize=False,BYTE SetWindowMaxSize=False)


  CODE
  PARENT.Init(AppStrategy,SetWindowMinSize,SetWindowMaxSize)
  SELF.SetParentDefaults()                                 ! Calculate default control parent-child relationships based upon their positions on the window

!!! <summary>
!!! Generated from procedure template - Frame
!!! </summary>
Frame PROCEDURE 

AppFrame             APPLICATION('SoftVelocity Media Demo'),AT(,,678,433),FONT('MS Sans Serif',8,,FONT:regular), |
  NOFRAME,MAXIMIZE,TILED,AUTO,CENTER,MASK,MAX,SYSTEM,WALLPAPER('dark-bg.jpg'),IMM
                     END

ThisWindow           CLASS(WindowManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
TakeWindowEvent        PROCEDURE(),BYTE,PROC,DERIVED
                     END

Toolbar              ToolbarClass

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('Frame')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = 1
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  SELF.AddItem(Toolbar)
  SELF.Open(AppFrame)                                      ! Open window
  Do DefineListboxStyle
  SELF.SetAlerts()
      AppFrame{PROP:TabBarVisible}  = False
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisWindow.TakeWindowEvent PROCEDURE

ReturnValue          BYTE,AUTO

Looped BYTE
  CODE
  LOOP                                                     ! This method receives all window specific events
    IF Looped
      RETURN Level:Notify
    ELSE
      Looped = 1
    END
  ReturnValue = PARENT.TakeWindowEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
      0{PROP:Buffer} = 1
      START(Main,25000)
    END
    RETURN ReturnValue
  END
  ReturnValue = Level:Fatal
  RETURN ReturnValue

