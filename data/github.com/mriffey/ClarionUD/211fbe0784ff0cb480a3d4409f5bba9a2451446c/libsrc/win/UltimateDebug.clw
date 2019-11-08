                                MEMBER()

!==============================================================================
!If all fields begin with the same prefix, then you can have that auto-stripped
!by uncommenting the following line.
!E_StripPrefix         EQUATE(1)
!==============================================================================
WidthMultiplier:Narrow          EQUATE(5)  !normal columns (numbers, lowercase, etc.)
WidthMultiplier:Wide            EQUATE(7)  !uppercase columns
!==============================================================================
    INCLUDE('EQUATES.CLW')
    INCLUDE('Errors.clw')
    INCLUDE('UltimateDebug.INC'),ONCE 
! ToDo _wsldebug$setlogfile

                                MAP
                                    module('kernel')     
!see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/debug/base/debugging_functions.asp
                                        debugbreak(),PASCAL !,RAW,NAME('debugbreak')
                                        OutputDebugString(*CSTRING),PASCAL,RAW,NAME('OutputDebugStringA')
                                    end  
                                    MODULE('C%V%RUN%X%')
                                        debugerNameMessage (*CSTRING, UNSIGNED EventNum ),RAW,NAME('WslDebug$NameMessage')   !Note: use   Event() + EVENT:FIRST  else will get WM_*
                                        COMPILE('** C55+ **',_C55_)
                                        debugerGetFieldName(SIGNED FEQ            ),*CSTRING,RAW,NAME('Cla$FIELDNAME')
                                        !END-COMPILE('** C55+ **',_C55_)
                                        OMIT('** C55+ **',_C55_)
                                        debugerGetFieldName(SIGNED FEQ            ),LONG,RAW,NAME('Cla$FIELDNAME')
                                        !END-OMIT('** C55+ **',_C55_)
                                    END
                                    DebugerAssertHook(UNSIGNED LineNumber, STRING filename, STRING message)
                                END


!OMIT('_ifdef_',EVENT:APP=08000h)
    COMPILE('_ifdef_',EVENT:APP=0)
EVENT:APP                       EQUATE(08000h)
EVENT:APP_LAST                  EQUATE(0BFFFh)
    !END_COMPILE('_ifdef_',EVENT:APP=0)
!==============================================================================
UltimateDebug.Construct         PROCEDURE  

    CODE
    
    SELF.EventQ               &=  NEW ST::DebugEventQueue
    SELF.IgnoreEventQ         &=  NEW ST::DebugEventQueue
    SELF.CategoryQueue        &=  NEW DebugCategoryQueue
    SELF.TemporaryPrefixQueue &=  NEW TemporaryPrefixQueueType
	
	
    SELF.SetPurgeTime(5*60*100)  !5 minutes 
    SELF.SetEventOffset 
    
    SELF.ShowAll        =  TRUE
    SELF.ShowField      =  FALSE
    SELF.ShowFocus      =  FALSE
    SELF.ShowSelected   =  FALSE
    SELF.ShowSelStart   =  FALSE
    SELF.ShowSelEnd     =  FALSE
    SELF.ShowKeyCode    =  FALSE
    SELF.ShowError      =  FALSE
    SELF.ShowThread     =  FALSE
    SELF.ShowContents   =  FALSE
    SELF.ShowScreenText =  FALSE
    SELF.ShowAcceptAll  =  FALSE      
    
    SELF.InitAssertHook()    
     
!==============================================================================
UltimateDebug.Destruct          PROCEDURE

    CODE
    
    IF SELF.ModuleName <> ''
        SELF.BeginTemporaryPrefix('[OpenClose]')
        SELF.Debug('<<<<' & ',MT ' & CLIP(SELF.ModuleName) & ',A ' & CLIP(SELF.AppName) & ',L ' & CLIP(SELF.Modified)) 
        SELF.EndTemporaryPrefix()
    END
    
    FREE(SELF.EventQ)
    DISPOSE(SELF.EventQ)

    FREE(SELF.IgnoreEventQ)
    DISPOSE(SELF.IgnoreEventQ)

    FREE(SELF.CategoryQueue)
    DISPOSE(SELF.CategoryQueue)
    
    FREE(SELF.TemporaryPrefixQueue)
    DISPOSE(SELF.TemporaryPrefixQueue)   
    
    SYSTEM{prop:asserthook}  = 0  !Stop calls into assert handler
    SYSTEM{prop:asserthook2} = 0  !Stop calls into assert handler 
    
    
!==============================================================================

!-----------------------------------
UltimateDebug.Init              PROCEDURE(<STRING pProcedure>,<*Group pSettings>)
!-----------------------------------

    CODE
     
    if ~OMITTED(pSettings)
        SELF.udb_Settings    =  pSettings
     
        SELF.ProcedureThread =  THREAD()
        SELF.ProcedureName   =  pProcedure
        SELF.DebugPrefix     =  SELF.udb_Settings.DebugPrefix
        SELF.LineWrap        =  SELF.udb_Settings.LineWrap
        SELF.ASCIIFileName   =  SELF.udb_Settings.ASCIIFileName
        SELF.DebugOff        =  SELF.udb_Settings.DebugOff
        SELF.SaveToFile      =  SELF.udb_Settings.SaveToFile
        SELF.DebugNoCR       =  SELF.udb_Settings.DebugNoCR
        SELF.ModuleName      =  SELF.udb_Settings.ModuleName
        SELF.AppName         =  SELF.udb_Settings.AppName
        SELF.Modified        =  SELF.udb_Settings.Modified 
        
    ELSE
        SELF.DebugPrefix     =  'DBG'
        SELF.LineWrap        =  true
        SELF.ASCIIFileName   =  ''
        SELF.DebugOff        =  false
        SELF.SaveToFile      =  false
        SELF.DebugNoCR       =  false
        SELF.ModuleName      =  'UltimateDebug'
        SELF.AppName         =  'Class'
        SELF.Modified        =  Today()
        
    END
    
    SELF.BeginTemporaryPrefix('[OpenClose]')
    SELF.Debug('>>' & ',M ' & CLIP(SELF.ModuleName) & ',A ' & CLIP(SELF.AppName) & ',L ' & CLIP(SELF.Modified)) 
    SELF.EndTemporaryPrefix()
     
    
    RETURN

!-----------------------------------
UltimateDebug.Kill              PROCEDURE()
!-----------------------------------

    CODE

    RETURN
	
!-----------------------------------------
UltimateDebug.AddCategoryToDebug        PROCEDURE(STRING pCategory)
!-----------------------------------------

    CODE
	
    SELF.CategoryQueue.Category =  pCategory
    GET(SELF.CategoryQueue,SELF.CategoryQueue.Category)
    IF ERROR()
        ADD(SELF.CategoryQueue,SELF.CategoryQueue.Category)
    END
	
		
!-----------------------------------------
UltimateDebug.Debug             PROCEDURE(STRING pDebugString,<STRING pCustomPrefix>,BYTE pNoClip = 0,BYTE pForce = 0)
!-----------------------------------------


lc_CDebugString                     STRING(100000)
lc_COutDebugString                  STRING(100000)
lc_Temp                             STRING(100000)
ll_Count                            LONG
ll_End                              LONG
lb_Flag                             BYTE(FALSE)
lb_FirstTime                        BYTE(TRUE)
lb_FirstLoop                        BYTE(0)
ll_StringPosition                   LONG


    CODE  
    
    IF pForce
    ELSE
        IF SELF.DebugOff AND ~SELF.SaveToFile;RETURN.
    END
    
  
    ll_Count =  1  
    IF SELF.DebugNoCR   
        lc_CDebugString =  CLIP(pDebugString) 
!!!        IF ~SELF.DebugOff  
            LOOP
                IF LEN(CLIP(lc_CDebugString)) < 4000
                    SELF.SendOutputToDebug(lc_CDebugString,pCustomPrefix,pNoClip)
                    BREAK
                END
                SELF.SendOutputToDebug(SUB(lc_CDebugString,1,4000),pCustomPrefix,pNoClip)
                lc_CDebugString = SUB(lc_CDebugString,4001,100000)
                
            END
            
!!!        END  
!!!        IF SELF.SaveToFile
!!!            ASC1:String =  '[' & FORMAT(TODAY(),@D17) & '-' & FORMAT(CLOCK(),@T7) & ']- ' & pCustomPrefix & lc_CDebugString
!!!            ADD(ASC1:ASCIIFile)  
!!!            
!!!        END
    ELSE
        IF LEN(CLIP(pDebugString)) = 0
!!!            IF SELF.SaveToFile
!!!                ASC1:String =  '[' & FORMAT(TODAY(),@D17) & '-' & FORMAT(CLOCK(),@T7) & ']' & pCustomPrefix
!!!                ADD(ASC1:ASCIIFile)
!!!            END
!!!            IF ~SELF.DebugOff
                lc_COutDebugString =  ''
                SELF.SendOutputToDebug(lc_COutDebugString,pCustomPrefix,pNoClip)
!!!            END
        ELSE
			
            Loop Until lb_Flag
                If Len(Clip(pDebugString)) - ll_Count < 9999 Then
                    ll_end  =  Len(Clip(pDebugString))
                    lb_Flag =  True
                Else
                    ll_end =  ll_Count + 9998
                End         
                lc_CDebugString =  CLIP(pDebugString[ll_Count : ll_end])
                        
                ll_Count        =  ll_end + 1
                LOOP
                    ll_StringPosition =  INSTRING('<13>',lc_CDebugString,1,1)

                    IF ll_StringPosition                                      
                        lc_COutDebugString =  SUB(lc_CDebugString,1,ll_StringPosition - 1)
                        lc_CDebugString    =  SUB(lc_CDebugString,ll_StringPosition + 2,10000)
!!!                        IF SELF.SaveToFile
!!!                            ASC1:String =  '[' & FORMAT(TODAY(),@D17) & '-' & FORMAT(CLOCK(),@T7) & ']- ' & pCustomPrefix & lc_COutDebugString
!!!                            ADD(ASC1:ASCIIFile)
!!!                        END
!!!                        IF ~SELF.DebugOff
                            SELF.SendOutputToDebug(lc_COutDebugString,pCustomPrefix,pNoClip)
!!!                        END

                        lb_FirstLoop =  TRUE
                        CYCLE
                    END
!!!                    IF SELF.SaveToFile
!!!                        ASC1:String =  '[' & FORMAT(TODAY(),@D17) & '-' & FORMAT(CLOCK(),@T7) & ']- ' & pCustomPrefix & lc_CDebugString
!!!                        ADD(ASC1:ASCIIFile)
!!!                    END
!!!                    IF ~SELF.DebugOff
                        SELF.SendOutputToDebug(lc_CDebugString,pCustomPrefix,pNoClip)
!!!                    END
                    BREAK
                END 
            END
        END
    END
!!!    IF SELF.SaveToFile
!!!        CLOSE(ASC1:ASCIIFile)      
!!!        
!!!    END

!-----------------------------------------------------------------------------------------!
UltimateDebug.SendOutputToDebug         PROCEDURE(STRING pOutput,<STRING pCustomPrefix>,BYTE pNoClip = 0)
!-----------------------------------------------------------------------------------------!

lc_CDebugString                             CSTRING(10000)
lc_COutDebugString                          CSTRING(10000)
lc_Temp                                     CSTRING(10000)
Count                                       LONG(0)

                                MAP
SendLineToDebug                     PROCEDURE()
                                END

    CODE
     
    IF pOutput = '';RETURN.
    
    lc_CDebugString =  CLIP(pOutput)
    
    SendLineToDebug()
        
 
    
SendLineToDebug                 PROCEDURE()

LocalPrefix                         STRING(100)
EndPrefix                           LONG
      
ASC1:ASCIIFile                      FILE,PRE(ASC1),DRIVER('ASCII'),CREATE
RECORD                                  RECORD,PRE()
STRING                                      STRING(512)
                                        END
                                    END

    CODE
    
    IF lc_CDebugString = '';RETURN.
    
    LocalPrefix = SELF.DebugPrefix
    IF LEN(CLIP(lc_CDebugString)) > 4
        IF lc_CDebugString[1:2] = '[*'
            EndPrefix = INSTRING('*]',lc_CDebugString,1,1) - 1
            IF EndPrefix > 3
                LocalPrefix = lc_CDebugString[3:EndPrefix]
                lc_CDebugString = SUB(lc_CDebugString,EndPrefix + 3,100000)
            END
        END
    END
    
    
    lc_COutDebugString =  '[||]X ' & CLIP(LocalPrefix) & |
            CHOOSE(SELF.ProcedureThread = 0,',T ' & Thread(),',T ' & SELF.ProcedureThread) & |
            CHOOSE(SELF.ProcedureName = '',',P ',',P ' & CLIP(SELF.ProcedureName)) & '[||]' & |
            CLIP(lc_CDebugString)
    
    IF ~SELF.DebugOff
        OutputDebugString(lc_COutDebugString)   
        
    END
    
    IF SELF.SaveToFile  
        ASC1:ASCIIFile{PROP:Name} =  SELF.ASCIIFileName    
        
        OPEN(ASC1:ASCIIFile)
        IF ERROR()
            CREATE(ASC1:ASCIIFile)
        END
        
        ASC1:String =  lc_COutDebugString
        ADD(ASC1:ASCIIFile)
        CLOSE(ASC1:ASCIIFile)
            
    END

!-----------------------------------------------------------------------------------------!
UltimateDebug.ClearDebugView    PROCEDURE()   !Requires Debugview 4.3 or greater
!-----------------------------------------------------------------------------------------!
!From:  http://www.sysinternals.com/ntw2k/freeware/debugview.shtml
!       Clear-output string: When DebugView sees the special debug output string "DBGVIEWCLEAR" it clears the output.

!Note: If this doesn't appear to work, then you are either:
!          a) using an older version of debugview
!       OR b) you are filtering the message

    CODE

    SELF.Debug('DBGVIEWCLEAR')
	
		
	
!==============================================================================
UltimateDebug.DebugRecord       PROCEDURE(*FILE pFile,STRING pMsg)
!--------------------------------------
G                                   &GROUP
!--------------------------------------
    CODE
    IF SELF.DebugOff;RETURN.
    G &=  pFile{PROP:Record}
    SELF.DebugGroup(G,, pMsg, 'Record')
    RETURN
!==============================================================================
UltimateDebug.DebugRecord       PROCEDURE(*FILE pFile,*FILE pFile2,STRING pMsg)
!--------------------------------------
G1                                  &GROUP
G2                                  &GROUP
!--------------------------------------
    CODE
    IF SELF.DebugOff;RETURN.
    G1 &=  pFile{PROP:Record}
    G2 &=  pFile2{PROP:Record}
    SELF.DebugGroup(G1, G2, pMsg, 'Record')
    RETURN
!==============================================================================
UltimateDebug.DebugGroup        PROCEDURE(*GROUP pGroup,STRING pMsg,<STRING pStructureType>)
!--------------------------------------
    CODE
    IF SELF.DebugOff;RETURN.
    SELF.DebugGroup(pGroup,, pMsg)
    RETURN
!==============================================================================
UltimateDebug.DebugGroup        PROCEDURE(*GROUP pGroup,<*GROUP pGroup2>,STRING pMsg,<STRING pStructureType>)
!--------------------------------------
epGroup2                            EQUATE(2)
SavePointer                         LONG,AUTO
NumFields                           SHORT(0)
NumFields2                          SHORT(0)
FieldQ                              QUEUE
Name                                    CSTRING(100)
Value                                   CSTRING(1000)
Value2                                  CSTRING(1000)
                                    END
MsgLineQ                            QUEUE
Text                                    STRING(100)
                                    END

F                                   ANY                  !Field reference for value assignment
X                                   SHORT,AUTO
M                                   SHORT(0)
!--------------------------------------
Window                              WINDOW('Debug'),AT(,,676,416),FONT('Tahoma',8,,),CENTER,SYSTEM,GRAY,DOUBLE
                                        LIST,AT(4,4,668,356),USE(?DebugList),VSCROLL,FORMAT('125L(2)|M~Field Name~S(1)@S100@180L(2)|M~Value~S(1)@S255@1000L(2)|M~Value2~S(1)@' &|
                                                'S255@'),FROM(FieldQ)
                                        LIST,AT(4,364,668,48),USE(?MessageList),VSCROLL,FROM(MsgLineQ)
                                    END
!--------------------------------------
    CODE
    IF SELF.DebugOff;RETURN.
    DO LoadFieldQ
    !--- Prepare window
    OPEN(Window)
    IF OMITTED(epGroup2)
        ?DebugList{PROPLIST:Width,2} =  1000
    END
    0{PROP:Text} =  0{PROP:Text} &' '& CHOOSE(pStructureType='', 'Group', pStructureType) &' ('& NumFields &' Fields)'
    IF pMsg
        SELF.FormatMessageList(pMsg, MsgLineQ)
    ELSE
        HIDE(?MessageList)
        ?DebugList{PROP:Height} =  ?DebugList{PROP:Height} + ?MessageList{PROP:Height} + 4
    END
    !--- Display window
    ACCEPT
    END
    RETURN
!======================================
LoadFieldQ                      ROUTINE
!--------------------------------------
    DATA

!--------------------------------------
    CODE
 !!   OMIT('!+++!!!+++!')
    LOOP X = 10000 TO 1 BY -1
        IF NumFields = 0 AND WHO(pGroup, X) <> ''
            NumFields =  X
            IF M = 0
                M =  NumFields
            END
        END
        IF NOT OMITTED(epGroup2)  |
                AND NumFields2 = 0 AND WHO(pGroup2, X) <> ''
            NumFields2 =  X
            IF M = 0
                M =  NumFields2
            END
        END
        IF M = 0 AND (NumFields OR NumFields2)
            M =  X
        END
        IF NumFields AND (OMITTED(epGroup2) OR NumFields2)
            BREAK
        END
    END
    LOOP X = 1 TO M
        CLEAR(FieldQ)
        IF NumFields >= X
            FieldQ.Name  =  WHO(pGroup, X)
            F           &=  WHAT(pGroup, X, 1)
            FieldQ.Value =  F
            IF NumFields2 >= X
                DO AssignValue2
            END
        ELSE
            FieldQ.Name =  WHO(pGroup2, X)
            DO AssignValue2
        END
        ADD(FieldQ)
        ASSERT(ERRORCODE()=0)
    END
 !!   !+++!!!+++!
    EXIT
!======================================
AssignValue2                    ROUTINE
!--------------------------------------
  !!  OMIT('!+++!!!+++!')
    F            &=  WHAT(pGroup2, X, 1)
    FieldQ.Value2 =  F
 !!   !+++!!!+++!
    EXIT
!==============================================================================
UltimateDebug.DebugQueue        PROCEDURE(*QUEUE pQueue,<STRING pMsg>,<BYTE pNoTouch>)
                                MAP
LoadFieldQ                          PROCEDURE
                                END
!--------------------------------------
SavePointer                         LONG,AUTO
NumFields                           SHORT,AUTO
                                    COMPILE('***---***', E_StripPrefix)
!StripPrefixLength   BYTE,AUTO
                                    ***---***
FieldQ                              QUEUE
Header                                  CSTRING(100)
Width                                   LONG
IsNumeric                               BOOL
IsGroup                                 BOOL
                                    END
MsgLineQ                            QUEUE
Text                                    STRING(1000)
                                    END    
!--------------------------------------
Window                              WINDOW('Debug Queue'),SYSTEM,AT(,,676,416),CENTER,FONT('Tahoma', 8),GRAY,DOUBLE
                                        LIST, AT(4,4,668,356), USE(?DebugList), HVSCROLL
                                        LIST, AT(4,364,668,48), USE(?MessageList), VSCROLL, FROM(MsgLineQ)
                                    END
!--------------------------------------
    CODE
    IF SELF.DebugOff;RETURN.
    IF pQueue &= NULL
        MESSAGE('Queue passed to ST::DebugQueue was a NULL pointer!', 'Debug Queue')
    ELSE
        !--- Save current queue pointer
        SavePointer =  CHOOSE(RECORDS(pQueue)=0, 0, POINTER(pQueue))
        !--- Scan passed queue
        DO FindLastField
        IF NumFields = 0
            MESSAGE('Queue passed to ST::DebugQueue has no fields!', 'Debug Queue')
        ELSE
            COMPILE('***---***', E_StripPrefix)
            DO CheckStripPrefix
            ***---***
            LoadFieldQ
            !--- Prepare window
            OPEN(Window)
            Display()
            0{PROP:Text} =  0{PROP:Text} &' ('& NumFields &' Fields, '& RECORDS(pQueue) &' Records)'
            DO FormatFieldList
            IF pMsg
                SELF.FormatMessageList(pMsg, MsgLineQ)
            ELSE
                HIDE(?MessageList)
                ?DebugList{PROP:Height} =  ?DebugList{PROP:Height} + ?MessageList{PROP:Height} + 4
            END
            !--- Display window
            ACCEPT
            END
            !--- Restore queue pointer
            IF SavePointer <> 0 AND pNoTouch <> 1
                GET(pQueue, SavePointer)
            END
        END
    END
    RETURN
!======================================
FindLastField                   ROUTINE
!--------------------------------------
    NumFields =  5000
    LOOP WHILE NumFields > 0  |
            AND   WHO(pQueue, NumFields) = ''
        NumFields -=  1
    END
    EXIT
!**************************************
    COMPILE('***---***', E_StripPrefix)
!======================================
CheckStripPrefix                ROUTINE
!--------------------------------------
    DATA
FieldNo SHORT,AUTO
PrefixFound     CSTRING(20)
!--------------------------------------
    CODE
    LOOP FieldNo = 1 TO NumFields
        FieldQ.Header     =  WHO(pQueue, FieldNo)
        StripPrefixLength =  INSTRING(':', FieldQ.Header)
        IF StripPrefixLength
            IF FieldNo = 1
                PrefixFound =  FieldQ.Header[1:S]
            ELSIF FieldQ.Header[1:S] <> PrefixFound
                StripPrefixLength =  0
                BREAK
            END
        ELSIF PrefixFound
            StripPrefixLength =  0
            BREAK
        END
    END
    EXIT
    ***---***
!======================================
FormatFieldList                 ROUTINE
!--------------------------------------
    DATA
FieldNo SHORT,AUTO
ColumnNo        SHORT(0)
!--------------------------------------
    CODE
    ?DebugList{PROP:From    } =  pQueue
    ?DebugList{PROP:Selected} =  SavePointer
    LOOP FieldNo = 1 TO NumFields
        GET(FieldQ, FieldNo)
        IF FieldQ.IsGroup
            CYCLE
        END
        ColumnNo                            +=  1
        ?DebugList{PROPLIST:Header,ColumnNo} =  FieldQ.Header
        ?DebugList{PROPLIST:Picture    +PROPLIST:Group, ColumnNo} = '@S'& FieldQ.Width
        ?DebugList{PROPLIST:RightBorder+PROPLIST:Group, ColumnNo} = 1
        ?DebugList{PROPLIST:Resize,ColumnNo      } =  1
        ?DebugList{PROPLIST:Width,ColumnNo       } =  FieldQ.Width
        ?DebugList{PROPLIST:HeaderCenter,ColumnNo} =  True
!   ?DebugList{PROPLIST:HeaderLeft                , ColumnNo} = True
!   ?DebugList{PROPLIST:HeaderLeftOffset          , ColumnNo} = 1
        IF FieldQ.IsNumeric
            ?DebugList{PROPLIST:Right,ColumnNo      } =  True
            ?DebugList{PROPLIST:RightOffset,ColumnNo} =  1
        ELSE
            ?DebugList{PROPLIST:Left,ColumnNo      } =  True
            ?DebugList{PROPLIST:LeftOffset,ColumnNo} =  1
        END
        ?DebugList{PROPLIST:FieldNo,ColumnNo} =  FieldNo
    END
    !ST::Debug(?DebugList{PROP:Format})
    EXIT
!**************************************
!======================================
LoadFieldQ                      PROCEDURE
!--------------------------------------
FieldNo                             SHORT,AUTO
FieldRef                            ANY
RecNo                               LONG,AUTO
SampleLength                        LONG,AUTO
HeaderLength                        LONG,AUTO
DataLength                          LONG,AUTO
IsDataUpper                         BOOL,AUTO
!--------------------------------------
    CODE
    !ST::Debug('ST::DebugQueue/LoadFieldQ/IN: NumFields='& NumFields)
    LOOP FieldNo = 1 TO NumFields
        CLEAR(FieldQ)
        !ST::Debug('ST::DebugQueue/LoadFieldQ: FieldNo='& FieldNo)
        FieldQ.Header =  LOWER(WHO(pQueue, FieldNo))
        COMPILE('***---***', E_StripPrefix)
        IF StripPrefixLength
            HeaderLength  =  LEN(FieldQ.Header) - StripPrefixLength
            FieldQ.Header =  SUB(FieldQ.Header, StripPrefixLength+1, HeaderLength)
        ELSE
            ***---***
            HeaderLength =  LEN(FieldQ.Header)
            COMPILE('***---***', E_StripPrefix)
        END
        ***---***
        IF HeaderLength < 1
            HeaderLength =  1
        END
        COMPILE('***---***', _C60_)
        FieldRef &=  WHAT(pQueue, FieldNo, 1)
        ***---***
        OMIT('***---***', _C60_)
        FieldRef &=  WHAT(pQueue, FieldNo)
        ***---***
        FieldQ.IsGroup   =  ISGROUP(pQueue, FieldNo)
        FieldQ.IsNumeric =  TRUE
        IsDataUpper      =  FALSE
        !ST::Debug('ST::DebugQueue/LoadFieldQ: RECORDS(pQueue)='& RECORDS(pQueue))
        IF RECORDS(pQueue) > 0 AND pNoTouch <> 1
            DataLength =  0
            LOOP RecNo = 1 TO RECORDS(pQueue)
                GET(pQueue, RecNo)
                !ST::Debug('ST::DebugQueue/LoadFieldQ: RecNo='& RecNo &'; FieldRef='& FieldRef)
                IF FieldRef <> ''
                    IF NOT NUMERIC(FieldRef)
                        FieldQ.IsNumeric =  FALSE
                    END
                    SampleLength =  LEN(CLIP(FieldRef))
                    IF NOT FieldQ.IsNumeric AND UPPER(FieldRef) = FieldRef
                        IsDataUpper =  TRUE
                    END
                    IF SampleLength > 25
                        DataLength =  25
                    ELSIF DataLength < SampleLength
                        DataLength =  SampleLength
                    END
                END
            END
        ELSE
            IsDataUpper      =  TRUE
            FieldQ.IsNumeric =  FALSE
            DataLength       =  LEN(FieldRef)
        END
        DO CalculateColumnWidth
        ADD(FieldQ)
    END  
    !ST::Debug('ST::DebugQueue/LoadFieldQ/OUT')

CalculateColumnWidth            ROUTINE
    DATA
HeaderWidth     SHORT,AUTO
DataWidth       SHORT,AUTO
    CODE
    HeaderWidth  =  HeaderLength * WidthMultiplier:Narrow
    DataWidth    =  DataLength * CHOOSE(~IsDataUpper, WidthMultiplier:Narrow, WidthMultiplier:Wide)
    FieldQ.Width =  CHOOSE(DataWidth > HeaderWidth, DataWidth, HeaderWidth)
    !ST::Debug(FieldQ.Header &' - '& HeaderLength &' - '& DataLength &' - '& HeaderWidth &' - '& DataWidth &' - '& FieldQ.Width)  

!==============================================================================  
UltimateDebug.Message           PROCEDURE(STRING pDebugString)  

    CODE
    SELF.Debug(pDebugString)
                                               
!==============================================================================  
  
UltimateDebug.SetApplicationName        PROCEDURE(STRING pApplicationName,STRING pProgramExtension)    !,STRING

ApplicationName                             STRING(50)

    CODE
    CASE (pProgramExtension)
    OF ('EXE')
        ApplicationName =  'Application  <9>' & pApplicationName & '.EXE'
    OF ('DLL')
        ApplicationName =  'DLL          <9>' & pApplicationName & '.DLL'
    OF ('LIB')
        ApplicationName =  'Library      <9>' & pApplicationName & '.LIB'
    END   
    
    RETURN ApplicationName     
    
!==============================================================================  
  
UltimateDebug.SetShortApplicationName   PROCEDURE(STRING pApplicationName,STRING pProgramExtension)    !,STRING

ApplicationName                             STRING(50)

    CODE
    CASE (pProgramExtension)
    OF ('EXE')
        ApplicationName =  pApplicationName & '.EXE'
    OF ('DLL')
        ApplicationName =  pApplicationName & '.DLL'
    OF ('LIB')
        ApplicationName =  pApplicationName & '.LIB'
    END   
    
    RETURN ApplicationName    
!==============================================================================  
UltimateDebug.ShowProcedureInfo         PROCEDURE(STRING pProcedure,STRING pApplication,STRING pHelpID,STRING pCreated,STRING pModified,STRING pCompiled)

TheStats                                    STRING(500)

Window                                      WINDOW('Procedure Information'),AT(,,275,121),CENTER,GRAY
                                                GROUP('Procedure Information'),AT(3,8,190,102),USE(?GROUP1),BOXED
                                                END
                                                TEXT,AT(11,20,176,84),USE(TheStats),SKIP,TRN
                                                BUTTON('Send To Clipboard'),AT(198,61,72),USE(?BUTTONToClipboard)
                                                BUTTON('Send To Debug'),AT(198,78,72),USE(?BUTTONToDebug)
                                                BUTTON('Close'),AT(198,96,72),USE(?BUTTONClose)
                                            END


    CODE
    LOOP WHILE KEYBOARD() !Empty the keyboard buffer
        ASK                  !without processing keystrokes
    END
    SETKEYCODE(0)
    TheStats =  ('Procedure:<9>' & CLIP(pProcedure) & '<13,10>' & |
            CLIP(pApplication) & '<13,10><13,10>' & |
            'Help ID    <9>' & CLIP(pHelpID) & '<13,10>' & |
            'Created  On<9>' & CLIP(pCreated) & '<13,10>' & |
            'Modified On<9>' & CLIP(pModified) & '<13,10>' & |
            'Compiled On<9>' & CLIP(pCompiled)) 
    OPEN(Window)
    ACCEPT
        CASE FIELD()
        OF ?BUTTONClose
            CASE EVENT()
            OF EVENT:Accepted
                BREAK
            END 
        OF ?BUTTONToClipboard
            CASE EVENT()
            OF EVENT:Accepted
                SETCLIPBOARD(TheStats)
            END
        OF ?BUTTONToDebug
            CASE EVENT()
            OF EVENT:Accepted
                Self.Debug(TheStats)
            END  
        END
      
    END
    
    LOOP WHILE KEYBOARD() !Empty the keyboard buffer
        ASK                  !without processing keystrokes
    END
    SETKEYCODE(0)
!==============================================================================  
UltimateDebug.FormatMessageList         PROCEDURE(STRING pMsg,*QUEUE pMsgQueue)   

!--------------------------------------
StartPos                                    LONG(1)
Pos                                         LONG,AUTO
!--------------------------------------
    CODE
    IF pMsg
        LOOP WHILE StartPos
            Pos       =  INSTRING('|', pMsg, 1, StartPos)
            pMsgQueue =  CHOOSE(Pos=0, pMsg[StartPos : LEN(pMsg)], pMsg[StartPos : Pos-1])
            ADD(pMsgQueue)
            ASSERT(ERRORCODE()=0)
            StartPos =  Pos+1
        UNTIL Pos = 0
    END
    RETURN
!==============================================================================

  
UltimateDebug.SetEventOffSet    PROCEDURE
    
    code
  
    COMPILE('**++** _C60_Plus_',_C60_)
    SELF.EventOffset =  0A000h
    !  **++** _C60_Plus_
    OMIT   ('**--** _PRE_C6_',_C60_)
    SELF.EventOffset =  01400h
    !  **--** _PRE_C6_

    IF UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) <> 'EVENT:ACCEPTED' 
        self.HuntForOffSet                                               
    end
  

UltimateDebug.HuntForOffset     procedure
EventNum                            LONG
Pass                                BYTE
Lo                                  LONG
Hi                                  LONG
    code
  
    SELF.Debug('SELF.EventOffset is not correct, trying to find a correct value')

    SELF.EventOffset =  CHOOSE(SELF.EventOffset = 01400h, 0A000h, 01400h)
    if UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED' THEN RETURN END

    SELF.EventOffset =  GETINI('Debuger','EventOffset', -1)
    CASE SELF.EventOffset
    OF -2; SELF.Debug('Stored value for EventOffset indicates no valid offset to be found, not searching')
    OF -1; SELF.Debug('SELF.EventOffset is not correct, searching for correct value')
    ELSE   ; IF UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED'
        SELF.Debug('Using stored value for SELF.EventOffset')
        RETURN
    END
    END

    !The loops are split out to search more likely ranges first
    !for efficiency it makes more sense to check offsets incrementing by 100, searching for a result that starts with 'EVENT'
    LOOP Pass = 1 TO 4
        EXECUTE Pass
            BEGIN; Lo = 0A000h; Hi = 0AFFFh END
            BEGIN; Lo = 01000h; Hi = 01FFFh END
            BEGIN; Lo = 00000h; Hi = 00FFFh END
            BEGIN; Lo = 0B000h; Hi = 0FFFFh END
        END
        LOOP EventNum = Lo TO Hi
            SELF.EventOffset =  EventNum
            IF UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED'
                PUTINI('Debuger','EventOffset',SELF.EventOffset)
                RETURN
            END
        END
    END

    SELF.Debug('Could not find a working offset for .EventOffset')
    SELF.EventOffset =  -2
    PUTINI('Debuger','EventOffset',SELF.EventOffset)




UltimateDebug.SetDebugEvent     PROCEDURE(SIGNED Event)
    CODE
    SELF.DebugEvent =  Event


UltimateDebug.SetHotKey         PROCEDURE(UNSIGNED HotKey)
    CODE
    0{PROP:Alrt,255} =  HotKey
    SELF.HotKey      =  HotKey
    SELF.SetDebugEvent(EVENT:AlertKey)


UltimateDebug.SetPurgeTime      PROCEDURE(LONG PurgeTime)
    CODE
    IF PurgeTime <= 0
        SELF.PurgeStarTime =  0
    ELSE
        SELF.PurgeStarTime =  SELF.CalcStarDate(0, PurgeTime)
    END


UltimateDebug.IgnoreEvent       PROCEDURE(SIGNED Event)
X                                   LONG,AUTO
    CODE
    CLEAR(SELF.IgnoreEventQ)
    SELF.IgnoreEventQ.EventNo =  Event
    ADD(SELF.IgnoreEventQ, SELF.IgnoreEventQ.EventNo)

    ! Purge existing logged events
    LOOP X = RECORDS(SELF.EventQ) TO 1 BY -1
        GET(SELF.EventQ, X)
        IF SELF.EventQ.EventNo = Event
            DELETE(SELF.EventQ)
        END
    END



UltimateDebug.TakeEvent         PROCEDURE
    CODE
!	? !UltimateDebug.Debug('UltimateDebug.TakeEvent: DebugEvent='& SELF.DebugEvent &'; Event='& EVENT() &'-'& SELF.GetEventName(EVENT()) &'; Keycode='& KEYCODE())
    CASE EVENT()
    OF 0
    OROF EVENT:Suspend
    OROF EVENT:Resume
!		?   !Self.Debug('...Ignore It')
        !Ignore it
    OF SELF.DebugEvent
!		?   !Self.Debug('...Debug Event  '& SELF.DebugEvent &'/'& EVENT:AlertKey &'  ' & KEYCODE() &'/'& SELF.HotKey)
        IF SELF.DebugEvent <> EVENT:AlertKey |
                OR KEYCODE() = SELF.HotKey
            SELF.Debug('')
        ELSE
            SELF.LogEvent
        END
    ELSE
!		?   !SELF.Debug('...Logger')
        IF SELF.DebugEvent <> EVENT:AlertKey     |
                OR EVENT()         <> EVENT:PreAlertKey  |
                OR KEYCODE()       <> SELF.HotKey
            SELF.LogEvent
        END
    END

UltimateDebug.LogEvent          PROCEDURE
    CODE
    SELF.IgnoreEventQ.EventNo =  EVENT()
    GET(SELF.IgnoreEventQ, SELF.IgnoreEventQ.EventNo)
    IF ERRORCODE() <> 0
        CLEAR(SELF.EventQ)
        SELF.EventQ.Date      =  FORMAT(TODAY(), @D10)
        SELF.EventQ.Time      =  FORMAT(CLOCK(), @T6)
        SELF.EventQ.StarDate  =  SELF.CalcStarDate()
        SELF.EventQ.EventNo   =  EVENT()
        SELF.EventQ.EventName =  SELF.GetEventName(EVENT())
        SELF.EventQ.FieldFeq  =  FIELD()
!		SELF.EventQ.FieldName = SELF.GetControlName(FIELD()) TODO
        SELF.EventQ.Keycode   =  KEYCODE()
        ADD(SELF.EventQ, SELF.EventQ.StarDate)
    END
    ! Purge old events
    LOOP WHILE SELF.PurgeStarTime <> 0 AND RECORDS(SELF.EventQ)
        GET(SELF.EventQ, 1)
        IF SELF.EventQ.StarDate > SELF.CalcStarDate() - SELF.PurgeStarTime THEN BREAK.
        DELETE(SELF.EventQ)
    END


UltimateDebug.CalcStarDate      PROCEDURE(<LONG D>,<LONG T>)!,REAL
    CODE
    IF OMITTED(D) THEN D = TODAY().
    IF OMITTED(T) THEN T = CLOCK().
    RETURN D + (T-1)/8640000


UltimateDebug.GetEventName      PROCEDURE(SIGNED Event)!,STRING
    CODE
    CASE Event

        ! Field-dependent events

    OF 01H;  RETURN 'Accepted'
    OF 02H;  RETURN 'NewSelection'
    OF 02H;  RETURN 'ScrollUp'
    OF 04H;  RETURN 'ScrollDown'
    OF 05H;  RETURN 'PageUp'
    OF 06H;  RETURN 'PageDown'
    OF 07H;  RETURN 'ScrollTop'
    OF 08H;  RETURN 'ScrollBottom'
    OF 09H;  RETURN 'Locate'

    OF 01H;  RETURN 'MouseDown'
    OF 0aH;  RETURN 'MouseUp'
    OF 0bH;  RETURN 'MouseIn'
    OF 0cH;  RETURN 'MouseOut'
    OF 0dH;  RETURN 'MouseMove'
    OF 0eH;  RETURN 'VBXevent'
    OF 0fH;  RETURN 'AlertKey'
    OF 10H;  RETURN 'PreAlertKey'
    OF 11H;  RETURN 'Dragging'
    OF 12H;  RETURN 'Drag'
    OF 13H;  RETURN 'Drop'
    OF 14H;  RETURN 'ScrollDrag'
    OF 15H;  RETURN 'TabChanging'
    OF 16H;  RETURN 'Expanding'
    OF 17H;  RETURN 'Contracting'
    OF 18H;  RETURN 'Expanded'
    OF 19H;  RETURN 'Contracted'
    OF 1AH;  RETURN 'Rejected'
    OF 1BH;  RETURN 'DroppingDown'
    OF 1CH;  RETURN 'DroppedDown'
    OF 1DH;  RETURN 'ScrollTrack'
    OF 1EH;  RETURN 'ColumnResize'

    OF 101H;  RETURN 'Selected'
    OF 102H;  RETURN 'Selecting'

        ! Field-independent events (FIELD() returns 0)

    OF 201H;  RETURN 'CloseWindow'
    OF 202H;  RETURN 'CloseDown'
    OF 203H;  RETURN 'OpenWindow'
    OF 204H;  RETURN 'OpenFailed'
    OF 205H;  RETURN 'LoseFocus'
    OF 206H;  RETURN 'GainFocus'

    OF 208H;  RETURN 'Suspend'
    OF 209H;  RETURN 'Resume'
    OF 20AH;  RETURN 'Notify'

    OF 20BH;  RETURN 'Timer'
    OF 20CH;  RETURN 'DDErequest'
    OF 20DH;  RETURN 'DDEadvise'
    OF 20EH;  RETURN 'DDEdata'
    OF 20FH;  RETURN 'DDEexecute'
    OF 210H;  RETURN 'DDEpoke'
    OF 211H;  RETURN 'DDEclosed'

    OF 220H;  RETURN 'Move'
    OF 221H;  RETURN 'Size'
    OF 222H;  RETURN 'Restore'
    OF 223H;  RETURN 'Maximize'
    OF 224H;  RETURN 'Iconize'
    OF 225H;  RETURN 'Completed'
    OF 230H;  RETURN 'Moved'
    OF 231H;  RETURN 'Sized'
    OF 232H;  RETURN 'Restored'
    OF 233H;  RETURN 'Maximized'
    OF 234H;  RETURN 'Iconized'
    OF 235H;  RETURN 'Docked'
    OF 236H;  RETURN 'Undocked'

    OF 240H;  RETURN 'BuildFile'
    OF 241H;  RETURN 'BuildKey'
    OF 242H;  RETURN 'BuildDone'

        ! User-definable events

    OF 3FFH;  RETURN 'DoResize'
    OF 400H;  RETURN 'User'
    END
    RETURN '???'
 
!-----------------------------------------------------------------------                    
UltimateDebug.AddUserEvent      PROCEDURE(STRING argEventName,LONG argEventEquate)

    CODE
    
    IF ~SELF.UserEventNameQ &= NULL
        IF SELF.GetUserEvent(argEventEquate)
            SELF.UserEventNameQ.EventName =  argEventName
            PUT(SELF.UserEventNameQ)
        ELSE
            SELF.UserEventNameQ.EventEquate =  argEventEquate
            SELF.UserEventNameQ.EventName   =  argEventName
            ADD(SELF.UserEventNameQ)
        END
    END
    

!-----------------------------------------------------------------------------------------!
UltimateDebug.GetUserEvent      PROCEDURE(LONG argEventEquate)!string
    CODE
    IF ~SELF.UserEventNameQ &= NULL
        SELF.UserEventNameQ.EventEquate =  argEventEquate
        GET(SELF.UserEventNameQ, SELF.UserEventNameQ.EventEquate)
        RETURN CHOOSE( ERRORCODE()=0, SELF.UserEventNameQ.EventName, '')
    ELSE
        RETURN ''
    END  
!-----------------------------------------------------------------------------------------!
UltimateDebug.GetEventDescr     PROCEDURE(LONG argEvent)!,string  !prototype set to default to -1
lcl:Retval                          LIKE(qtUserEventName.EventName)
lcl:EventNum                        UNSIGNED  
lcl:Position                        LONG

    CODE
    IF argEvent = -1
        argEvent =  EVENT()
    END
    lcl:RetVal =  SELF.GetUserEvent( argEvent )
    IF ~lcl:RetVal
        CASE argEvent
        OF Event:User                      ; lcl:RetVal = 'EVENT:User'
        OF Event:User + 1 TO Event:Last    ; lcl:RetVal = 'EVENT:User + '& argEvent - Event:User
        OF Event:APP                       ; lcl:RetVal = 'EVENT:App'
        OF Event:APP  + 1 TO Event:APP_LAST; lcl:RetVal = 'EVENT:App + ' & argEvent - Event:APP

        ELSE                                  
            IF SELF.EventOffset = -2   !indicates could not find a valid offset
                lcl:RetVal =  'EVENT['& argEvent &']'
            ELSE
                lcl:EventNum =  argEvent + SELF.EventOffset  ! 1400h (pre c6) or A000h (c6) !EVENT:FIRST equate(01400h)/(0A000h)
                debugerNameMessage(lcl:RetVal, lcl:EventNum)
            END
        END
    END   
     
    RETURN lcl:RetVal    ![7:LEN(CLIP(lcl:RetVal))]
    

!-----------------------------------------------------------------------------------------!
UltimateDebug.DecToHex          PROCEDURE(LONG pNumber)  !,STRING 
!-----------------------------------------------------------------------------------------!

Hexchars                        STRING(16)
hexdisplaystring                STRING(30)  
stringposition                  SHORT  
number                          LONG  
tempnumber                      LONG  

    CODE

    Hexchars = '0123456789ABCDEF'
    clear(hexdisplaystring)
    stringposition = len(hexdisplaystring)
    tempnumber = pNumber
    
    LOOP WHILE (tempnumber AND stringposition)  
        hexdisplaystring[stringposition] = hexchars[tempnumber%16 + 1]
        tempnumber = tempnumber/16
        stringposition -= 1   
    END
       
    RETURN CLIP(LEFT(hexdisplaystring)) & 'h'

!-----------------------------------------------------------------------------------------!
UltimateDebug.GetFEQDescr       PROCEDURE(SIGNED argFEQ)!,string  !prototype set to default to -1

lcl:Retval                          CSTRING(40) !<--- some arbitrary length
lcl:FEQ                             SIGNED
szRef                               &CSTRING

    CODE
    lcl:FEQ =  CHOOSE(argFEQ = -1, FIELD(), argFEQ)
    COMPILE('** C55+ **',_C55_)
    lcl:RetVal =  debugerGetFieldName(lcl:FEQ)
    !END-COMPILE('** C55+ **',_C55_)
    OMIT('** C55+ **',_C55_)
    szRef     &=  debugerGetFieldName(lcl:FEQ)
    lcl:RetVal =  szRef
    !END-OMIT('** C55+ **',_C55_)

    RETURN lcl:RetVal !CLIP(lcl:RetVal) 
  
!-----------------------------------------------------------------------------------------! 
!UltimateDebug.DebugEvent  
!Used to show Events as they are processed.
!Code by Mark Goldbert
!-----------------------------------------------------------------------------------------!
  
UltimateDebug.DebugEvent        PROCEDURE(<STRING pDebugProcedure>)

cs_DebugString                      string(600)
cl_Offset                           LONG
cb_DebugNoCR                        BYTE(0)
cs_Event                            STRING(4)
cs_Procedure                        STRING(20)
cl_ProcedureOffset                  LONG


    CODE
    
    
!!    IF cs_Procedure
!!        cl_ProcedureOffset =  LEN(CLIP(cs_DebugString)) + 2
!!        cs_DebugString     =  cs_Procedure[1:cl_ProcedureOffset] & cs_DebugString
!!		
!!    END
    
!!    cs_Procedure   =  pDebugProcedure
    SELF.BeginTemporaryPrefix('[Events]')
    cs_Event       =  EVENT()
      
    cs_DebugString =  'Event*' & cs_Event & ' ' & CLIP(SELF.DecToHex(cs_Event)) & ' ' & SUB(SELF.GetEventDescr(),7,100) & ','
	
    if self.ShowField or self.ShowAll
        cs_DebugString =  CLIP(cs_DebugString) & 'Field*' & SELF.GetFEQDescr() & ' = ' & FIELD() & ','  
    end
  
    if self.ShowFocus or self.ShowAll 
        cs_DebugString =  CLIP(cs_DebugString) & 'Focus*' & SELF.GetFEQDescr(FOCUS()) & ' =' & FOCUS() & ',' 
    end
  
    if self.ShowSelected or self.ShowAll
        cs_DebugString =  CLIP(cs_DebugString) & 'Selected*' & SELF.GetFEQDescr(SELECTED()) & ' =' & SELECTED() & ','
    end
  
    if self.ShowSelStart or self.ShowAll
        cs_DebugString =  CLIP(cs_DebugString) & 'SelStart*' & FOCUS(){prop:SelStart} & ','
    end
  
    if self.ShowSelEnd or self.ShowAll   
        cs_DebugString =  CLIP(cs_DebugString) & 'SelEnd*' & FOCUS(){prop:SelEnd} & ',' 
    end
  
    if self.ShowKeyCode or self.ShowAll 
        cs_DebugString =  CLIP(cs_DebugString) & 'KeyCode*' & KEYCODE() & ',' 
    end
  
    if self.ShowError or self.ShowAll  
        cs_DebugString =  CLIP(cs_DebugString) & 'Error*' & ERRORCODE() & '-' & CLIP(ERROR()) & ','  
    end
  
    if self.ShowThread or self.ShowAll  
        cs_DebugString =  CLIP(cs_DebugString) & 'Thread*' & THREAD() & ','  
    end
    
    if self.ShowAcceptAll or self.ShowAll  
        cs_DebugString =  CLIP(cs_DebugString) & 'AcceptAll*' & 0{prop:AcceptAll} & ',' 
    end  
    
    if self.ShowContents or self.ShowAll 
        cs_DebugString =  CLIP(cs_DebugString) & 'Contents*' & CONTENTS(FOCUS()) & ','
    end
  
    if self.ShowScreenText or self.ShowAll 
        cs_DebugString =  CLIP(cs_DebugString) & 'ScreenText*' & FOCUS(){prop:ScreenText} & ','  
    end       
    
!!    cs_DebugString =  cs_DebugString[1:cl_Offset]
!!    cb_DebugNoCR   =  SELF.DebugNoCR
!!    SELF.DebugNoCR =  TRUE
    SELF.DEBUG(clip(cs_DebugString),'',TRUE) 
!!    SELF.DebugNoCR =  cb_DebugNoCR
    SELF.EndTemporaryPrefix()
   
	
	
UltimateDebug.GPF               PROCEDURE()

    CODE
    DebugBreak()
    
    
UltimateDebug.TurnOnTracing   PROCEDURE(FILE pFile)

Count                               LONG
ColumnNames                         STRING(20000)

    CODE
    IF SELF.DebugOff = TRUE AND SELF.SaveToFile = FALSE
    ELSE
        pFile{PROP:Profile} = 'DEBUG:'
        pFile{PROP:Details} = TRUE
        IF pFile{PROP:Driver} = 'MSSQL'
            pFile{PROP:LogSQL} = TRUE
        END
        SELF.GetFieldNames(pFile)
        
    END
        
    
UltimateDebug.TurnoffTracing          PROCEDURE(FILE pFile)

    CODE
    
    IF SELF.DebugOff = TRUE AND SELF.SaveToFile = FALSE
    ELSE
        pFile{PROP:Profile} = ''
        pFile{PROP:Details} = FALSE    
        IF pFile{PROP:Driver} = 'MSSQL'
            pFile{PROP:LogSQL} = FALSE
        END
        
    END
    
    
UltimateDebug.GetFieldNames      PROCEDURE(FILE pFile)

P                                   String(5)
R                                   Long
Cnt                                 Long
H                                   String(100)
Q                                   Long
U                                   String(100)
FList                               String(5000)
ColumnName                          STRING(100)
ColumnExternalName                  STRING(100)

    CODE
    
    Loop R = 1 To pFile{PROP:Fields}  
        If Cnt Then
            Cnt -=  1
            Cycle
        End
        If pFile{PROP:Name,R} Then
            H =  pFile{PROP:Name,R}
        Else
            H =  pFile{PROP:Label,R} 
        End
        
        ColumnExternalName = pFile{PROP:Name,R}
        ColumnName = pFile{PROP:Label,R}
        IF ColumnExternalName = ''
            ColumnExternalName = ColumnName
        END
        
        IF ~Clip(H) THEN
            BREAK
        ELSE
            If pFile{Prop:Over,R} = 0 AND pFile{Prop:Type,R} = 'GROUP' Then
                Cycle
            End
            If pFile{Prop:Over,R} <> 0 Then
                If pFile{Prop:Type,R} = 'GROUP' Then
                    Cnt =  pFile{Prop:Fields,R}
                End
                Cycle
            End
            
            IF ~InList(Upper(U),'GROUP','SEQNOSTR') Then
                FList =  Clip(FList) &  UPPER(Clip(ColumnName)) & '|' & CLIP(ColumnExternalName) & ','
            END
            
        END  
        
    END
    
    IF FList
        FList =  FList[1:Len(Clip(FList))-1]
    END
     
    SELF.BeginTemporaryPrefix('[||FieldNames||]')
    SELF.Debug('[||FieldNames||]' & FList,,TRUE)
    SELF.EndTemporaryPrefix()
    
    RETURN
    
    
UltimateDebug.SetPrefix         PROCEDURE(STRING pPrefix)

    CODE
    
    SELF.DebugPrefix = pPrefix
    
    
UltimateDebug.BeginTemporaryPrefix      PROCEDURE(STRING pPrefix)

    CODE
    
    SELF.TemporaryPrefixQueue.Prefix = SELF.DebugPrefix
    ADD(SELF.TemporaryPrefixQueue)
    SELF.DebugPrefix = pPrefix
    
    
UltimateDebug.EndTemporaryPrefix        PROCEDURE()

    CODE
    
    GET(SELF.TemporaryPrefixQueue,RECORDS(SELF.TemporaryPrefixQueue))
    IF ~ERROR()
        SELF.DebugPrefix = SELF.TemporaryPrefixQueue.Prefix
        DELETE(SELF.TemporaryPrefixQueue)
    END
    
    
!-------------------------------------------------------------------------------------------------------------------------------  
DebugerAssertHook               PROCEDURE(UNSIGNED LineNumber, STRING filename, STRING argMSG) 
!-------------------------------------------------------------------------------------------------------------------------------
                                    !Code taken from Mark Goldbergs Debuger class.
                                    !Note: ASSERT will ONLY call this procedure if you have compiled with Debugging On or  asserts=>on
                                    !FYI: I found doc. re: asserts on in the C60help.hlp under ASSERT()

                                    !Useage:   Assert(0,eqDBG&'Message to display')
                                    !Purpose:  Will add MODULE/LineNumber to output
                                    !          also makes it easy to call the debuger, from modules with an empty member, hence no debuger instance in scope

                                    !Updated: Size(eqDBG) replaced with LEN(eqDBG) so this code can be used in C5
                                    !         Aug 8, 2005 - added .MatchAssertMsg logic, moved GPF logic to a ROUTINE
                                    !
                 

DEBUGMSG                            BYTE,AUTO
Matched                             LONG,AUTO
LocalPrefix                         STRING(100)
EndPrefix                           LONG
LocalMessage                        CSTRING(10000) 

    CODE    
!!    STOP(LineNumber & '  ' & filename & '  ' & argMSG)
    LocalMessage = argMSG
    IF LEN(LocalMessage) >= LEN(eqDBG) AND LocalMessage[ 1 : LEN(eqDBG) ] = eqDBG
        DEBUGMSG =  TRUE
        IF LEN(LocalMessage) = LEN(eqDBG) !added Nov 7, 2003 (after getting index out of range errors)
            LocalMessage = ''
        ELSE
            LocalMessage =  LocalMessage[ LEN(eqDBG) + 1 : SIZE(LocalMessage) ]
        END
    ELSE
        DEBUGMSG =  FALSE
    END
    IF DEBUGMSG 
        LocalPrefix = '[Assert]'
        IF LEN(CLIP(LocalMessage)) > 4
            IF LocalMessage[1:2] = '[*'
                EndPrefix = INSTRING('*]',LocalMessage,1,1) - 1
                IF EndPrefix > 3
                    LocalPrefix = LocalMessage[3:EndPrefix]
                    LocalMessage = SUB(LocalMessage,EndPrefix + 3,100000)
                END
            END
        END
        !Added Ultimate Debug View Info here.
        LocalMessage =  '[||]X ' & CLIP(LocalPrefix) & ',T ' & LineNumber  & ',P ' & CLIP(fileName) &  '[||]' & CLIP(LocalMessage) !& ' L:' & LineNumber 
        OutputDebugString( LocalMessage )
         
        
    ELSE  
        CASE MESSAGE('Assertion failed in ['& CLIP(filename) &'] @['& LineNumber &']'& |
                CHOOSE(LEN(CLIP(LocalMessage))>0,'||'& CLIP(LocalMessage),''),               |
                'ASSERT', ICON:Exclamation, '&Continue|&Halt',1)
        OF 1; !do nothing
        OF 2; DO DebugerAssertHook::GPFNOW
        END !case
    END


!----------------------------------------------------
DebugerAssertHook::GPFNOW       ROUTINE

    SYSTEM{prop:asserthook}  = 0  !Stop recursive calls into assert handler
    SYSTEM{prop:asserthook2} = 0  !Stop recursive calls into assert handler
    debugBreak()


!-----------------------------------------------    
UltimateDebug.InitAssertHook    PROCEDURE
!-----------------------------------------------
    CODE
    SYSTEM{prop:asserthook2} = ADDRESS(DebugerAssertHook)

