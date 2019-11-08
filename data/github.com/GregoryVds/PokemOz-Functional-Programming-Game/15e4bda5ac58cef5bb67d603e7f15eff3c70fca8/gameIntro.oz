functor
import
   Module
   Lib           at 'lib.ozf'
   Strings       at 'strings.ozf'
export
   GetUserChoice
define
   [QTk]={Module.link ["x-oz://system/wp/QTk.ozf"]}
   ImageLibrary = {QTk.loadImageLibrary "ImageLibrary.ozf"}

   DefaultChosenPokemozLabel = "none"
   DefaultChosenName = ""

   fun {GetImage Name}
      {ImageLibrary get(name:Name image:$)}
   end

   fun {Title Msg}
      TitleFont = {QTk.newFont font(family:helvetica size:30 weight:bold)}
   in
      message(aspect:600 init:Msg glue:new padx:20 pady:20 bg:white font:TitleFont)
   end

   fun {Subtitle Msg}
      SubTitleFont = {QTk.newFont font(family:helvetica size:15 weight:bold)}
   in
      message(aspect:1000 init:Msg padx:20 pady:20 glue:ew bg:white font:SubTitleFont)
   end

   PlayerName
   ChosenPokemoz
   SachaCanvasHandle
   PokemozCanvasHandle
   ChoiceLabelHandle
   StartGameBtnHandle
   NameTextHandle

   NewGameMsg       = {Title Strings.newGame}
   ChooseNameMsg    = {Subtitle Strings.chooseName}
   ChoosePokemozMsg = {Subtitle Strings.choosePokemoz}
   NameText         = text(init:DefaultChosenName glue:n width:15 height:2 return:PlayerName
                           borderwidth:0 highlightthickness:0 action:CheckInputs handle:NameTextHandle)

   ChoiceLabel   = lr(label(init:Strings.chosenPokemoz bg:white)
                      label(init:DefaultChosenPokemozLabel handle:ChoiceLabelHandle bg:white return:ChosenPokemoz text:DefaultChosenPokemozLabel))

   StartGameBtn  = button(text:Strings.startGame padx:150 pady:20 glue:new bg:white
                          action:toplevel#close state:disabled handle:StartGameBtnHandle)

   SachaCanvas   = canvas(handle:SachaCanvasHandle width:400 height:350 bg:white borderwidth:0 highlightthickness:0)
   PokemozCanvas = canvas(handle:PokemozCanvasHandle width:300 height:600 bg:white borderwidth:0 highlightthickness:0)

   MainLayout = td(title:Strings.title bg:white
		   NewGameMsg
		   lr(
		      bg:white
		      glue:new
		      td(bg:white glue:new
			 ChoosePokemozMsg
			 PokemozCanvas
			)
		      td(bg:white glue:new
			 ChooseNameMsg
			 NameText
			 SachaCanvas
			 ChoiceLabel
			 StartGameBtn
			)
		      )
		  )

   proc {CheckInputs}
      NameVarText    = {NameTextHandle    get(1:$)}
      PokemozVarText = {ChoiceLabelHandle get(1:$)}
   in
      if NameVarText==DefaultChosenName orelse PokemozVarText==DefaultChosenPokemozLabel then
	       {StartGameBtnHandle set(state:disabled)}
      else
	       {StartGameBtnHandle set(state:normal)}
      end
   end

   proc {SetChoosenPokemonText X Y} Name in
     if Y < 180     then Name = "Bulbasoz"
     elseif Y < 360 then Name = "Charmandoz"
     else                Name = "Oztirtle"
     end
     {ChoiceLabelHandle set(Name)}
     {CheckInputs}
   end

   proc {DisplayPokemoz}
     {PokemozCanvasHandle create(image 75  0   anchor:nw image:{GetImage pokemoz_bulbasoz})}
     {PokemozCanvasHandle create(text  150 160 anchor:center text:"Bulbasoz - Grass" justify:center)}
     {PokemozCanvasHandle create(image 75  180 anchor:nw image:{GetImage pokemoz_charmandoz})}
     {PokemozCanvasHandle create(text  150 340 anchor:center text:"Charmandoz - Fire" justify:center)}
     {PokemozCanvasHandle create(image 75  360 anchor:nw image:{GetImage pokemoz_oztirtle})}
     {PokemozCanvasHandle create(text  150 520 anchor:center text:"Ozirtle - Water" justify:center)}
   end

   proc {DisplaySachaImage}
     {SachaCanvasHandle   create(image 110 50  anchor:nw image:{GetImage sacha_large})}
   end

   proc {GetUserChoice Name PokemozName}
      Window = {QTk.build MainLayout}
   in
      {PokemozCanvasHandle bind(event:"<1>" action:SetChoosenPokemonText args:[int(x) int(y)])}
      {DisplaySachaImage}
      {DisplayPokemoz}
      {QTk.flush}
      {NameTextHandle set(DefaultChosenName)}
      {Window show(wait:true modal:true)}
      Name        = PlayerName
      PokemozName = {String.toAtom {Lib.downcase ChosenPokemoz}}
   end
end
