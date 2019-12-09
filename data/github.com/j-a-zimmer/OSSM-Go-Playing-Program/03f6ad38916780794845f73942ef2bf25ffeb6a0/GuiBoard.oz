functor

import  QTk at 'x-oz://system/wp/QTk.ozf'
   Application System OS Browser
   %JAZTools System
export  GBoard

define

%T = JAZTools
%Pr = {T.setWriter System.showInfo} % Browser.browse}

Itgr = Float.toInt

Flt = Int.toFloat


class GBoard  % needs addButton(s) 
   feat  display stones scale size conversion_const
         line_thickness marker_radius marker_handles
   attr LastPlay CelebrateMark ShouldCelebrate
   
   meth init(Player<=_
             Size<=19 
             Scale<=30.0 
             LClickAction<=_ 
             RClickAction<=_ 
             GenericButtonAction<=_
             Clr<=c(255 215 104))
       % feature and var defs for init
    
       self.scale= Scale  
       self.size= Size+2
       StarpointRadius=       {Itgr Scale/10.0 }
       StoneRadius=           {Itgr Scale/2.2 }
       self.marker_radius=    {Itgr Scale/6.6 }
       self.line_thickness=   {Itgr Scale/20.0 }
       self.conversion_const= self.scale*0.6  
       NumBoardPixels=        {Itgr Scale*{Flt Size}}
       self.marker_handles = {Array.new 0 self.size*self.size-1 nil}
       ShouldCelebrate := false

       Board = canvas(
                   handle:self.display 
                   width:NumBoardPixels
                   height:NumBoardPixels
                   background:Clr )
       Quit = button(text: 'Quit' 
                     action: proc {$} {Application.exit 0} end)
       Generic = button(text: 'Pass'
                        action: GenericButtonAction)
       
       RightButtonBar
       if {IsDet Player} then
          Save = button(text: 'Save'
                           action: proc {$} {Player.talker sendCommand("!save_game")} end)
          Load = button(text: 'Load'
                           action: proc {$} {Player.talker sendCommand("!load_game")} end)
          NewGameB = button(text: 'New Game'
                           action: proc {$} {Player.talker sendCommand("!new_game")} end)
       in
         RightButtonBar = lr(Save Load NewGameB Quit)
       else
         RightButtonBar = Quit
       end
       
       ButtonBar
       if {IsDet GenericButtonAction} then 
          ButtonBar = lr(Generic RightButtonBar) 
       else 
          ButtonBar = RightButtonBar
       end
       
       Window = {self buildWindow(Board ButtonBar NumBoardPixels $)}
       
       X1 Y1 X2 Y2 % bounding rectangle of colored board

       % proc defs for init
       
       proc {DrawLines}
         
         proc {Draw2Lines I}
            X1v Y1v X2v Y2v % endpoints of vertical line
            X1h Y1h X2h Y2h % endpoints of horizontal line
         in
             % horizontal line
               {self BoardToGui(I 1 X1h Y1h)}  
               {self BoardToGui(I Size X2h Y2h)}
               {self.display create( line X1h Y1h X2h Y2h)}
             % vertical line 
               {self BoardToGui(1 I X1v Y1v)} 
               {self BoardToGui(Size I X2v Y2v)}
               {self.display create( line X1v Y1v X2v Y2v )}
         end

       in % DrawLines
         {For 1 Size 1 proc {$ I} {Draw2Lines I} end}
       end % DrawLines

       proc {PutStone R C V}
         %% 1 <= R,C <= self.size
         {Array.put (self.stones) (R*(self.size)+C) V}
       end

       proc {InsertVacantStones}
          R = StoneRadius
       in
          {For 1 self.size-2 1 proc {$ X}
             {For 1 self.size-2 1 proc {$ Y}
                AStone CenterX CenterY
               in
                {self BoardToGui( X Y CenterX CenterY)}
                {self.display create( 
                    oval 
                    CenterX-R CenterY-R       CenterX+R CenterY+R
                    handle:AStone             fill:nil
                    width:self.line_thickness outline:nil 
                )}
                {PutStone X Y AStone}
          end} end} 
       end

       proc {DrawStarpoints}

          PointList = if    Size == 19 then  [4 10 16]
                      elseif Size == 13 then [4 7 10]
                      elseif Size == 9  then [3 7]
                      else                   nil 
                      end 

          proc {DrawStarpoint X Y}
             CenterX CenterY
             R = StarpointRadius
          in
             {self BoardToGui(X Y CenterX CenterY)}
             { self.display create( 
                       oval 
                       CenterX-R CenterY-R 
                       CenterX+R CenterY+R 
                       fill:black) }
          end % DrawStarpoint

       in % proc DrawStarpoints

          {ForAll 
             PointList 
             proc {$ X}
                 { ForAll 
                     PointList 
                     proc {$ Y} {DrawStarpoint X Y} end}
          end}
       
      end % DrawStarpoints
     
     proc {BindMouseClicks}
         LClickr RClickr
         proc {IgnoreClick R C} skip end
      in % BindMouseClicks   
         if {IsDet LClickAction} then
            LClickr = LClickAction 
         else
            LClickr = IgnoreClick
         end
         if {IsDet RClickAction} then
            RClickr = RClickAction
         else
            RClickr = IgnoreClick
         end
         { self.display bind(event:"<ButtonPress>"
                      args:[int(b) int(x) int(y)]
                      action:proc {$ B R C}
                               Rbd Cbd
                             in 
                               {self MouseToBoard(R C Rbd Cbd)}
                               if B==3 orelse B==2 
                                then {RClickr Rbd Cbd} 
                               elseif B==1
                                then {LClickr Rbd Cbd}
                               end
                             end
         ) }
      end % BindMouseClicks

    in  % init method
      {self BoardToGui(1  1 X1 Y1)} 
      {self BoardToGui(Size Size X2 Y2)}
      {self.display create( rectangle X1 Y1 X2 Y2 )}
      self.stones = {NewArray 0 ((self.size)*(self.size)-1) nil}
      {DrawLines}
      {DrawStarpoints}
      {InsertVacantStones}
      {BindMouseClicks}
      {Window show}
    
    end % init method
    
    meth startCelebrate(A<=true)
      ShouldCelebrate := A
    end
    
    meth celebrate(R<=0 C<=0 P<=0)
      if (@ShouldCelebrate) then
         Rc = {OS.rand} mod 256
         Gc = {OS.rand} mod 256
         Bc = {OS.rand} mod 256
       in
         if {IsDet @CelebrateMark} then
            {self clearMarks(@CelebrateMark)}
         end
         
         CelebrateMark := {self addMarks([(R#C#useless)#c(Rc Gc Bc)] $)}
      
         case P 
         of 0 then
            if (R == (self.size-2)) then
               {self celebrate(R C 1)}
            else
               {Delay 50}
               {self celebrate(R+1 C P)}
            end
         [] 1 then
            if (C == (self.size-2)) then
               {self celebrate(R C 2)}
            else
               {Delay 50}
               {self celebrate(R C+1 P)}
            end
         [] 2 then
            if (R == 1) then
               {self celebrate(R C 3)}
            else
               {Delay 50}
               {self celebrate(R-1 C P)}
            end
         [] 3 then
            if (C == 1) then
               {self celebrate(R C 0)}
            else
               {Delay 50}
               {self celebrate(R C-1 P)}
            end
         end
      end
    end
    
    meth buildWindow(Board Buttons NumPixels ?Window)
      Window = {QTk.build td(Board Buttons)}
    end

    meth get(R C ?Z)
      %% 1 <= R,C <= self.size
      {Array.get (self.stones) (R*self.size+C) Z} 
    end
    
    meth reset()
       {For 1 self.size-2 1 proc {$ X}
          {For 1 self.size-2 1 proc {$ Y}
            {self put(X Y vacant)}
       end} end} 
       
      if {IsDet @CelebrateMark} then
         {self clearMarks(@CelebrateMark)}
      end
      
      if {IsDet @LastPlay} then
         {self clearMarks(@LastPlay)}
      end
    end

    meth put( X Y Color)
       AStone= {self get(X Y $)}
    in
      if {IsDet @LastPlay} then
         {self clearMarks(@LastPlay)}
      end
      
      LastPlay := {self addMarks([(X#Y#useless)#c(255 0 0)] $)}
    
       if Color==vacant then
          {AStone set(fill:nil)}
       elseif {Or Color==black Color==white} then
          {AStone set(fill:Color)}
       else 
          {Raise gboard('trying to put bad color @'#X#','#Y)}
       end
    end
    
    meth addMarks(MarkerList ?Tag)

       proc {DrawMarker (R#C#SClr)#Clr}
          Rad = self.marker_radius
          CenterR CenterC
          Pos = R*self.size+C
       in % DrawMarker 
          if {Not {Or (Pos<0) (Pos>(self.size*self.size-2))}} then
             {self BoardToGui(R C CenterR CenterC)}
             {Array.put 
                self.marker_handles 
                (R*self.size+C)
                {self.display create( 
                   rect CenterR-Rad CenterC-Rad CenterR+Rad CenterC+Rad
                   handle: $
                   fill:Clr
                   tag:Tag
                   width:self.line_thickness outline:nil )
                }
             }
          end
       end % DrawMarker
    
    in % addMarks
       Tag = {self.display newTag($)}
       {List.forAll MarkerList DrawMarker}
    end % addMarks

    meth changeMark(R C Clr)
       try
         {{Array.get self.marker_handles R*self.size+C} set(fill:Clr)}
       catch _ then
         raise 'attempt to change nonexistent marker at '#R#','#C 
       end
    end
         
    end

    meth clearMarks(Tag)
       try {Tag delete} catch _ then skip end
    end       

    meth MouseToBoard(X Y ?BoardX BoardY) 
      XG YG
    in % X Y are relative to window's root
       % XG XG are relative to board/canvas's root
       % BoardY BoardY are row and col numbers
      {self.display canvasx(X XG)}
      {self.display canvasy(Y YG)}
      {self GuiToBoard( XG YG BoardX BoardY )}
    end
     
    meth GuiToBoard( GuiX GuiY ?BoardX ?BoardY ) 
      BoardX =  
        {Itgr ({Flt GuiY}-self.conversion_const)/self.scale + 1.0}
      BoardY =  
        {Itgr ({Flt GuiX}-self.conversion_const)/self.scale + 1.0}
    end

    meth BoardToGui( BoardX BoardY ?GuiX ?GuiY)
       GuiX = 
        {Itgr self.conversion_const+{Flt (BoardY-1)}*self.scale} 
       GuiY = 
        {Itgr self.conversion_const+{Flt (BoardX-1)}*self.scale} 
    end

end % GBoard 

end

