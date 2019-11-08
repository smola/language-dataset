
  PROGRAM

OMIT('***')
 * Created with Clarion 10.0
 * User: davidgriffiths
 * Date: 3/05/2017
 * Time: 10:08 AM
 * 
 * To change this template use Tools | Options | Coding | Edit Standard Headers.
 ***
    include('equates.clw'),once
    Include('EventEqu.Clw'),Once

                    MAP   
                        include('eventmap.clw') 
                        include('cwutil.inc'),once
                    END           
      
modQ                QUEUE   ,type
Item                    string(30)
Add                     LONG
Minus                   LONG
total                   LONG   
ButtonItemControl       LONG
ButtonUpControl         LONG
ButtonDownControl       LONG  
TextControl             LONG

                    end           

modClass            CLASS
Q                       &ModQ     ! ref to modQ
Init                    PROCEDURE   
Destruct                PROCEDURE
LoadQ                   PROCEDURE  !LOAD THE Q WITH DATA   
AddQRecord              procedure(string _item,long _default)
buildButtons            PROCEDURE      
CopyButtonDown          PROCEDURE(long _newButton,long _oldbutton, long _gap)  
AcceptCheck             PROCEDURE 
                    end  
posClass            class,TYPE
x                       LONG
y                       LONG
w                       LONG
h                       LONG
ControlId               LONG     
                    end



counter             LONG

Window              WINDOW('Please Select'),AT(,,214,123),GRAY,IMM,AUTO,FONT('Tahoma',14),VSCROLL, |
                            RESIZE
                        BUTTON('Cheese'),AT(26,2,83),USE(?ButtonItem),SCROLL
                        TEXT,AT(133,3,20,14),USE(?TEXT1),SCROLL
                        BUTTON,AT(2,2,21,14),USE(?BUTTONUp),ICON('plus.ico'),SCROLL
                        BUTTON,AT(111,2,19,14),USE(?BUTTONDown),ICON('minus.ico'),SCROLL
                        BUTTON,AT(167,2,18,14),USE(?ButtonCancel),MSG('cancel'),STD(STD:Close), |
                                ICON(ICON:Cross),TIP('Cancel')
                        BUTTON,AT(188,2,17,14),USE(?ButtonOk),MSG('Accept and close'),STD(STD:Close), |
                                ICON(ICON:Tick),TIP('Accept and close')
                        BUTTON,AT(181,18,21,14),USE(?BUTTONListUp),ICON('arrow_up.ico')
                        BUTTON,AT(182,108,19,14),USE(?BUTTONListDown),ICON('arrow_down.ico')
                        SLIDER,AT(167,18,,104),USE(?SLIDER1),TRN,RANGE(0,100),VERTICAL
                    END
control1            posClass
control2            posClass
Thiswindow          posclass

    CODE          
        open(Window)
        Thiswindow.ControlId = 0
        modClass.Init      
        ?SLIDER1{PROP:RangeLow} = 0
        ?SLIDER1{PROP:RangeHigh} =255
        ACCEPT  
            
            modClass.acceptCheck  
            case ACCEPTED()
            of ?BUTTONListDown
                window{PROP:YOrigin} =window{PROP:YOrigin} + 16
            of ?BUTTONListUp
                window{PROP:YOrigin} =window{PROP:YOrigin} - 16     
            of ?SLIDER1
                ds_OutputDebugString('slider=' & ?SLIDER1{PROP:SliderPos})
                window{prop:vscrollpos} = ?SLIDER1{PROP:SliderPos}
            END    
            case event() 
            of EVENT:Scrolldrag
                 ds_OutputDebugString('event scrolldrag')
            of EVENT:Selected
                control1.ControlId = SELECTED()
                if control1.ControlId
                END
                GETPOSITION(control1.ControlId,control1.x,control1.y,control1.w,control1.h)
            END
            
            counter +=1
!            window{PROP:Text} ='window Height=' &  window{PROP:Height} & |
!                    ' PROP:clientheight=' & window{PROP:clientheight} & |
!                    ' PROP:clienty=' & window{PROP:clienty}   & |
!                    ' counter=' & counter  
            GETPOSITION(thiswindow.ControlId,Thiswindow.x,Thiswindow.y,Thiswindow.w,Thiswindow.h)
            ds_OutputDebugString(' PROP:YOrigin=' & 0{PROP:YOrigin})
            ds_OutputDebugString(' PROP:clientHeight=' & 0{PROP:clientHeight})
            ds_OutputDebugString(' PROP:Height=' & 0{PROP:Height})
            ds_OutputDebugString(' PROP:headerHeight=' & 0{PROP:HeaderHeight})
            ds_OutputDebugString(' prop:vscrollpos=' & 0{prop:vscrollpos})
            ds_OutputDebugString(' height=' & thiswindow.h)
            
            
        END
        
                           
modClass.AcceptCheck        PROCEDURE  
fieldAccepted                   LONG
i                               LONG    

    code
        If event()  = EVENT:Accepted
            fieldAccepted = FIELD()
            loop i = 1 to records(self.q)
                get(self.q,i)
                if self.q.ButtonDownControl = fieldAccepted
                    self.q.total -= 1
                    if self.q.total < 0 
                        self.q.total = 0
                    END 
                    put(self.q)
                    self.q.TextControl{prop:text} = self.q.total
                    break
                END
                if self.q.ButtonUpControl = fieldAccepted  
                    self.q.total +=1   
                    put(self.q)
                    self.q.TextControl{prop:text} = self.q.total
                    break
                END
                
            END
        END
modClass.init       PROCEDURE
    code
        self.Q &= new(modQ)  
        self.LoadQ   
        self.buildButtons

modClass.Destruct       PROCEDURE     
    code
        DISPOSE(self.Q )       
        
modClass.LoadQ      PROCEDURE
    code         
        loop 3 times
            self.addQrecord( 'Tomato' ,1)
            self.addQrecord( 'Onion' ,0)
            self.addQrecord( 'Chilli Sauce' ,0)
            self.addQrecord( 'Cheese' ,1)
            self.addQrecord( 'Pulled Pollo' ,1) 
            self.addQrecord( 'Bacon' ,0) 
            self.addQrecord( 'Aioli' ,0) 
            self.addQrecord( 'Rocket' ,0) 
        END
        
        
modclass.AddQRecord         PROCEDURE(string _item,long _default)
    code
        clear(self.q)
        self.q.Item = _item
        self.q.total = _default
        add(self.q)
modClass.buildButtons       PROCEDURE
i                               LONG     
x                               LONG
y                               long
h                               LONG
w                               LONG
oldItemButton                   LONG
OldTextControl                  LONG
OldUpButton                     LONG
OldDownButton                   LONG

    code  
        GETPOSITION(?ButtonItem,x,y,w,h)     ! get pos of first button
        loop i = 1 to records(self.Q)     
            get(self.q,i)
            if i = 1
                ?ButtonItem{PROP:Text} = self.q.Item
                ?TEXT1{PROP:Text} = self.q.total
                self.q.ButtonItemControl = ?ButtonItem  
                self.q.TextControl = ?TEXT1
                self.q.ButtonDownControl = ?BUTTONDown
                self.q.ButtonUpControl = ?BUTTONUp
                put(self.q) 
                oldItemButton = ?ButtonItem
                OldTextControl = ?TEXT1       
                OldUpButton = ?BUTTONUp
                OldDownButton = ?BUTTONDown
            ELSE  
                !--- Item button
                self.q.ButtonItemControl = create(0,create:button)
                self.CopyButtonDown(self.q.ButtonItemControl,oldItemButton,2)
                self.q.ButtonItemControl{prop:text} = self.q.Item
                oldItemButton = self.q.ButtonItemControl
                !  Text Control
                self.q.TextControl = create(0,CREATE:text)
                self.CopyButtonDown(self.q.TextControl,OldTextControl,2)
                self.q.TextControl{prop:text} = self.q.total
                OldTextControl = self.q.TextControl
                ! down button
                self.q.ButtonDownControl = create(0,CREATE:button)
                self.CopyButtonDown(self.q.ButtonDownControl,OldDownButton,2)
                OldDownButton = self.q.ButtonDownControl
                ! up button
                self.q.ButtonUpControl = create(0,CREATE:button)
                self.CopyButtonDown(self.q.ButtonUpControl,OldUpButton,2)
                OldUpButton = self.q.ButtonUpControl
                
                ! Save control
                put(self.q)
                
            END                           
            
        END
        
    !!! <Summary>
    !!! <para>Copy new button just below the old button </para>
    !!! <param name="_gap">this is the distance below the the old button</param>
    !!! <returns>nothing</returns>
    !!! </Summary>
modClass.CopyButtonDown     PROCEDURE(long _newButton,long _oldbutton, long _gap)
x                               LONG
y                               long
h                               LONG
w                               LONG
    code   
        _newButton{PROP:Text} = _oldbutton{PROP:Text}
        _newButton{PROP:icon} = _oldbutton{PROP:icon}
        _newButton{PROP:scroll} = _oldbutton{PROP:scroll}
        GETPOSITION(_oldbutton,x,y,w,h)   
        !just move the control down the same hight plus the gap
        y = y + h + _gap           
        SETPOSITION(_newButton,x,y,w,h)
        UNHIDE(_newButton)
        
