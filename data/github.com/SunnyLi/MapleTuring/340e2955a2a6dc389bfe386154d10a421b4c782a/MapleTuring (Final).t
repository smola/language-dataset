%%%%%%%%%%%%%%%%%
%% MapleTuring %%
%% By Sunny Li %%
%%%%%%%%%%%%%%%%%

% Date of creation: 6.3.2010
% V1.0 updated on 4.8.2012

/*=====================================================================
 This program is created based on the popular MMORPG game MapleStory
 and I have used the server selection page as menu. All origional image
 and sound(excluding the one in quiz) are extracted from MapleStory and
 edited on photoshop.
 =====================================================================*/

% When in game, type in name and press enter to login.


%===== Settings =======================================================================================
View.Set ("graphics:780;570,position:center;center,nobuttonbar,title:Maple Turing,nocursor")
%======================================================================================================

%======================================================================================================
% Sound
%======================================================================================================
% Music
var music : string
var finished : boolean

process bgMusic
    finished := false
    loop
	exit when finished
	Music.PlayFile ("Sound/Music/" + music + ".mp3")
    end loop
end bgMusic

% Reminder of how to fork and stop music
% fork bgMusic

% Stop the music
/*finished := true
 Music.PlayFileStop*/

 
% UI sound
var soundFile : string
var soundFinished : boolean

process sounds
    soundFinished := false
    loop
	exit when soundFinished
	Music.PlayFile ("Sound/Sound/" + soundFile + ".mp3")
    end loop
end sounds

% Reminder of how to fork and stop sound
% fork sounds

% Stop the sound
/*soundFinished := true
 Music.PlayFileStop*/

%======================================================================================================
% Mouse
%======================================================================================================
% for Mouse.Where
var mouseX, mouseY, mouseButton : int

%======================================================================================================
% Keyboard
%======================================================================================================
% Key inputs
var key : array char of boolean
var ch : string (1)

%======================================================================================================
% Text
%======================================================================================================
var font : array 1 .. 15 of int
font (1) := Font.New ("system:10")
font (2) := Font.New ("computer:8")
font (3) := Font.New ("Times New Roman:10")
font (4) := Font.New ("Times New Roman:10:bold")
font (5) := Font.New ("Times New Roman:12")
font (6) := Font.New ("Times New Roman:12:bold")
font (7) := Font.New ("Times New Roman:12:bold,italic")
font (8) := Font.New ("Times New Roman:13:bold")
font (9) := Font.New ("Times New Roman:15")
font (10) := Font.New ("Times New Roman:15:bold")
font (11) := Font.New ("Times New Roman:15:bold,italic")
font (12) := Font.New ("Times New Roman:17:bold,italic")
font (13) := Font.New ("Times New Roman:21")
font (14) := Font.New ("Times New Roman:21:bold")
font (15) := Font.New ("Arial:80:bold,italic")

%======================================================================================================
% User Info
%======================================================================================================
% The login name typed
var name : string := ""
% This is usesless...for the game I'm gonna do maybe?
var level : int := 1

%======================================================================================================
% Other Info
%======================================================================================================
% Content Select Scroll Position
var scrollPosition : int := 1
% Exit Content Select
var toExit : int := 0
% Keep track of which option is selected
var selectDown : int := 0
% Quiz
var win : boolean := false

%======================================================================================================
% Images
%======================================================================================================
% Start Up
var nexonLogo : int := Pic.FileNew ("Image/start up/nexon_logo.jpg")
var wizetLogo : int := Pic.FileNew ("Image/start up/wizet_logo.gif")
var msLogo : int := Pic.FileNew ("Image/start up/Title.logo.0.1.gif")
var fantasticStory : int := Pic.FileNew ("Image/start up/fantastic story.jpg")

% Login page
var boarder : int := Pic.FileNew ("Image/Login/Common.frame.gif")
var background : int := Pic.FileNew ("Image/Login/WorldSelect.birthday.0.0.jpeg")
var title : int := Pic.FileNew ("Image/Login/Title.MSTitle.gif")

var loginNormalBox : int := Pic.FileNew ("Image/Login/title.signboard.0.0.gif")
var loginNormalBox1 : int := Pic.FileNew ("Image/Login/title.signboard.0.1.bmp")
var loginNormal : int := Pic.FileNew ("Image/Login/Title.BtLogin.normal.0.gif")
var loginNormalMouseOver : int := Pic.FileNew ("Image/Login/Title.BtLogin.mouseover.0.gif")
var loginNormalPressed : int := Pic.FileNew ("Image/Login/Title.BtLogin.pressed.0.gif")
var loginNormalDisabled : int := Pic.FileNew ("Image/Login/Title.BtLogin.disabled.0.gif")

% Content Select
var signboard : int := Pic.FileNew ("Image/Content Select/WorldSelect.signboard.0.0.gif")
var signboard1 : int := Pic.FileNew ("Image/Content Select/WorldSelect.signboard.2.0.gif")
var csBg : int := Pic.FileNew ("Image/Content Select/WorldSelect.moonbunny.0.0.gif")
var selectDisable : int := Pic.FileNew ("Image/Content Select/WorldSelect.BtWorld.e.disabled.0.gif")
var selectNormal : int := Pic.FileNew ("Image/Content Select/WorldSelect.BtWorld.e.enabled.0.gif")
var selectFocus : int := Pic.FileNew ("Image/Content Select/Select Focus.gif")
var textLesson : int := Pic.FileNew ("Image/Content Select/lesson.gif")
var textCredit : int := Pic.FileNew ("Image/Content Select/credit.gif")
var textQuiz : int := Pic.FileNew ("Image/Content Select/quiz.gif")
var scroll1 : int := Pic.FileNew ("Image/Content Select/WorldSelect.scroll.0.1.gif")
var scroll2 : int := Pic.FileNew ("Image/Content Select/WorldSelect.scroll.0.2.gif")
var scroll3 : int := Pic.FileNew ("Image/Content Select/WorldSelect.scroll.0.3.gif")
var backDisabled : int := Pic.FileNew ("Image/Content Select/Common.BtStart.disabled.0.gif")
var backMouseOver : int := Pic.FileNew ("Image/Content Select/Common.BtStart.mouseOver.0.gif")
var backNormal : int := Pic.FileNew ("Image/Content Select/Common.BtStart.normal.0.gif")
var backPressed : int := Pic.FileNew ("Image/Content Select/Common.BtStart.pressed.0.gif")
var creditNormal : int := Pic.FileNew ("Image/Content Select/button1up.gif")
var creditMouseOver : int := Pic.FileNew ("Image/Content Select/button1over.gif")
var quizNormal : int := Pic.FileNew ("Image/Content Select/button2up.gif")
var quizMouseOver : int := Pic.FileNew ("Image/Content Select/button2over.gif")

% Loading
var loadingBar1 : int := Pic.FileNew ("Image/Loading/Notice.Loading.backgrnd.gif")
var loadingBar2 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.0.gif")
var loadingBar3 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.1.gif")
var loadingBar4 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.2.gif")
var loadingBar5 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.3.gif")
var loadingBar6 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.4.gif")
var loadingBar7 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.5.gif")
var loadingBar8 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.6.gif")
var loadingBar9 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.7.gif")
var loadingBar10 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.8.gif")
var loadingBar11 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.9.gif")
var loadingBar12 : int := Pic.FileNew ("Image/Loading/Notice.Loading.bar.10.gif")

% Maps
var gameBG1 : int := Pic.FileNew ("Image/Game/Map/Henesys1.bmp")

% UI bar
var userUIbarBG : int := Pic.FileNew ("Image/Game/UI/background.gif")
var userUIbar : int := Pic.FileNew ("Image/Game/UI/background2.gif")
var healthBar : int := Pic.FileNew ("Image/Game/UI/bar.gif")
var chatTarget : int := Pic.FileNew ("Image/Game/UI/chatTarget.gif")
var graduation : int := Pic.FileNew ("Image/Game/UI/graduation.gif")


%======================================================================================================
% Procedures
%======================================================================================================
% Login
procedure loginNormalClick
    fork sounds
    soundFile := "UI/ScrollUp"
    Pic.Draw (loginNormalMouseOver, 587, 473, picCopy)
    delay (50)
    Pic.Draw (loginNormalPressed, 587, 473, picCopy)
    delay (50)
    Pic.Draw (loginNormal, 587, 473, picCopy)
    delay (50)
end loginNormalClick

procedure loginNormalMouseEffect1
    %fork sounds
    %soundFile := "UI/BtMouseOver"
    Pic.Draw (loginNormalMouseOver, 587, 473, picCopy)
    %delay (300)
    %soundFinished := true
end loginNormalMouseEffect1

procedure loginNormalMouseEffect2
    fork sounds
    soundFile := "UI/BtMouseClick"
    Pic.Draw (loginNormalPressed, 587, 473, picCopy)
    delay (100)
    Pic.Draw (loginNormalMouseOver, 587, 473, picCopy)
    delay (100)
    soundFinished := true
end loginNormalMouseEffect2



% Content Select
procedure scrollDown
    View.Set ("nooffscreenonly")
    Pic.Draw (signboard1, 125, 355, picMerge)
    delay (100)
    Pic.Draw (scroll1, 125, 86, picMerge)
    delay (100)
    Pic.Draw (scroll2, 125, 85, picMerge)
    delay (100)
    Pic.Draw (scroll3, 125, 90, picMerge)
    delay (100)
    scrollPosition := 0
    View.Set ("offscreenonly")
end scrollDown

procedure scrollUp
    View.Set ("nooffscreenonly")
    Pic.Draw (scroll3, 125, 90, picMerge)
    delay (100)
    Pic.Draw (scroll2, 125, 85, picMerge)
    delay (100)
    Pic.Draw (scroll1, 125, 86, picMerge)
    delay (100)
    Pic.Draw (signboard1, 125, 355, picMerge)
    delay (100)
    scrollPosition := 1
    View.Set ("offscreenonly")
end scrollUp



% Loading (for fun)
procedure loading
    View.Set ("nooffscreenonly")
    Pic.Draw (loadingBar1, maxx div 3 - 25, 150, picMerge)
    Pic.Draw (loadingBar12, maxx div 2 - 21, 188, picMerge)
    delay (300)
    Pic.Draw (loadingBar2, maxx div 2 - 21, 188, picMerge)
    delay (400)
    
    fork sounds
    soundFile := "UI/Enchant"
    
    Pic.Draw (loadingBar3, maxx div 2 - 21, 188, picMerge)
    delay (500)
    Pic.Draw (loadingBar4, maxx div 2 - 21, 188, picMerge)
    delay (400)
    Pic.Draw (loadingBar5, maxx div 2 - 21, 188, picMerge)
    delay (200)
    Pic.Draw (loadingBar6, maxx div 2 - 21, 188, picMerge)
    delay (500)
    Pic.Draw (loadingBar7, maxx div 2 - 21, 188, picMerge)
    delay (200)
    Pic.Draw (loadingBar8, maxx div 2 - 21, 188, picMerge)
    delay (100)
    Pic.Draw (loadingBar9, maxx div 2 - 21, 188, picMerge)
    Pic.Draw (loadingBar10, maxx div 2 - 21, 188, picMerge)
    delay (300)
    Pic.Draw (loadingBar11, maxx div 2 - 21, 188, picMerge)
    delay (500)

    soundFinished := true
    Music.PlayFileStop
end loading



%======================================================================================================
% Start Up
%======================================================================================================
procedure startUp

    View.Set ("offscreenonly")

    colorback (black)
    cls
    Font.Draw ("Created by:", 200, 370, font (14), white)
    delay (100)

    fork bgMusic
    music := "UI/nxlogo"

    for gap : 1 .. 260
	View.Update
	drawfillbox (0, 0, maxx, maxy, black)
	Font.Draw ("Created by:", 170, 360, font (14), white)
	Font.Draw ("S", 150, 250, font (15), gap div 5)
	Font.Draw ("u", 150 + (gap div 4), 250, font (15), gap div 6)
	Font.Draw ("n", 150 + (gap div 2), 250, font (15), gap div 15 * 5)
	Font.Draw ("n", 150 + (gap div 4 * 3), 250, font (15), gap div 7)
	Font.Draw ("y", 150 + gap, 250, font (15), gap div 8)

	Font.Draw ("L", 150 + gap * 4 div 3, 250, font (15), gap div 9 * 2)
	Font.Draw ("i", 150 + gap * 3 div 2 + 25, 250, font (15), gap div 10 * 5 div 4)
    end for
    delay (1200)

    delay (0) %Lessen sound glitch
    finished := true
    Music.PlayFileStop

    %Extra just to make sure
    delay (1)
    finished := true
    Music.PlayFileStop

    fork bgMusic
    music := "UI/wzlogo"

    delay (500)
    colorback (white)
    cls
    Font.Draw ("Image and Data extracted from:", 50, 500, font (13), black)
    Pic.Draw (msLogo, 186, 340, picMerge)
    Font.Draw ("by", 350, 355, font (13), black)
    Pic.Draw (nexonLogo, 100, 165, picCopy)
    Pic.Draw (wizetLogo, 420, 180, picCopy)
    Font.Draw ("and also credit to", 300, 110, font (8), black)
    Pic.Draw (fantasticStory, 222, 50, picMerge)
    View.Update

    delay (5000)
    finished := true
    Music.PlayFileStop
    delay (100)

    View.Set ("nooffscreenonly")

end startUp


%======================================================================================================
% Login Page
%======================================================================================================

procedure loginNormalScreen

    toExit := 0

    %Sound
    delay (100) %avoid sound glitch
    fork bgMusic
    music := "UI/title"

    %Login
    Pic.Draw (background, -30, 0, picCopy)
    Pic.Draw (title, 180, 225, picMerge)
    Pic.Draw (loginNormalBox, 330, 337, picMerge)
    Pic.Draw (loginNormal, 587, 473, picCopy)
    Pic.Draw (boarder, -20, -30, picMerge)
    Font.Draw ("No Pass Required", 450, 475, font (7), white)
    Font.Draw ("Brought to you by Sunny Li", 400, 435, font (8), white)

    loop
	Input.KeyDown (key)
	Mouse.Where (mouseX, mouseY, mouseButton)


	%Login from Keyboard

	if length (name) > 0 then
	    if key (KEY_ENTER) then
		finished := true
		Music.PlayFileStop
		delay (100)
		loginNormalClick
		exit
	    end if
	else
	    Pic.Draw (loginNormalDisabled, 587, 473, picCopy)
	end if
	if key (KEY_BACKSPACE) then
	    if length (name) > 0 then
		name := name (1 .. length (name) - 1)
	    end if
	end if
	%ch := ""
	%if hasch then
	getch (ch)
	if ord (ch) = 8 or ord (ch) = 10 or length (name) = 15 then
	    ch := ""
	end if
	%end if
	name := name + ch


	%Login from Mouse (Not Recommended)
	/*To use the Mouse option you should enable "if hasch then" statement,
	 the 3 comment above, or keep inputting a key and use the mouse which
	 also work.*/

	if mouseX > 592 and mouseX < 675 and mouseY > 475 and mouseY < 517 then
	    if mouseButton = 0 then
		loginNormalMouseEffect1
		/*fork sounds
		 soundFile := "UI/BtMouseOver"
		 Pic.Draw (loginNormalMouseOver, 587, 473, picCopy)
		 delay(?)*/
	    else
		finished := true
		Music.PlayFileStop
		delay (100)
		loginNormalMouseEffect2
		/*fork sounds
		 soundFile := "UI/BtMouseClick"
		 Pic.Draw (loginNormalPressed, 587, 473, picCopy)
		 delay (?)
		 Pic.Draw (loginNormalMouseOver, 587, 473, picCopy)
		 delay (?)*/
		exit
	    end if
	else
	    Pic.Draw (loginNormal, 587, 473, picCopy)
	end if


	Pic.Draw (loginNormalBox1, 437, 498, picCopy)
	Font.Draw (name, 450, 505, font (1), white)
    end loop
    cls
end loginNormalScreen


%======================================================================================================
% Table of Content
%======================================================================================================
% Procedures
procedure lesson1
    Font.Draw ("Lesson 1:", 210, 338, font (7), black)
    Font.Draw ("Locate", 210, 310, font (14), black)
    Font.Draw ("In Turing, in order to output a character anywhere on", 250, 280, font (5), black)
    Font.Draw ("the screen, we must first learn the procedure", 210, 260, font (5), black)
    Font.Draw ("locate.", 480, 260, font (7), black)
    Font.Draw ("Syntax:   locate ( row, column : int )", 210, 230, font (6), black)
    Font.Draw ("The locate procedure moves the cursor to the given row", 250, 200, font (5), black)
    Font.Draw ("and column so the next output will appear at that location.", 210, 180, font (5), black)
    Font.Draw ("There are 25 rows and 80 columns. Row 1 is the top of the", 210, 160, font (5), black)
    Font.Draw ("screen and Column 1 is the left side of the screen.", 210, 140, font (5), black)
end lesson1

procedure lesson2
    Font.Draw ("Lesson 2:", 210, 338, font (7), black)
    Font.Draw ("Clear Screen", 210, 310, font (14), black)
    Font.Draw ("In Turing, to display a new graphic, we need to learn", 250, 280, font (5), black)
    Font.Draw ("to clear the window with procedure", 210, 260, font (5), black)
    Font.Draw ("cls.", 427, 260, font (7), black)
    Font.Draw ("Syntax:   cls", 210, 230, font (6), black)
    Font.Draw ("The cls procedure clears the window and reset the", 250, 200, font (5), black)
    Font.Draw ("cursor back to Row 1, Colume 1. Also, by using cls, the", 210, 180, font (5), black)
    Font.Draw ("output window will be set to the current background color.", 210, 160, font (5), black)
end lesson2

procedure lesson3
    Font.Draw ("Lesson 3:", 210, 338, font (7), black)
    Font.Draw ("Display in Colors", 210, 310, font (14), black)
    Font.Draw ("In Turing, we can display our results in color by using", 250, 280, font (5), black)
    Font.Draw ("the predefined procedure", 210, 260, font (5), black)
    Font.Draw ("color.", 365, 260, font (7), black)
    Font.Draw ("Syntax:   color (Color : int)", 210, 230, font (6), black)
    Font.Draw ("The procedure color is used to change the color of", 250, 200, font (5), black)
    Font.Draw ("the characters that are to be displayed. colour is the alternate", 210, 180, font (5), black)
    Font.Draw ("spelling. Turing has 255 predefined colors", 210, 160, font (5), black)
end lesson3

procedure lesson4
    Font.Draw ("Lesson 4:", 210, 338, font (7), black)
    Font.Draw ("Background Color", 210, 310, font (14), black)
    Font.Draw ("In Turing, we can also set the background color for", 250, 280, font (5), black)
    Font.Draw ("each displayed character by using the predefined procedure", 210, 260, font (5), black)
    Font.Draw ("colorback.", 210, 240, font (7), black)
    Font.Draw ("Syntax:   colorback (Color : int)", 210, 210, font (6), black)
    Font.Draw ("The colorback procedure is used to change the text's", 250, 180, font (5), black)
    Font.Draw ("or the screen's background color. The alternate spelling is", 210, 160, font (5), black)
    Font.Draw ("colourback.", 210, 140, font (5), black)
end lesson4

procedure lesson5
    Font.Draw ("Lesson 5:", 210, 338, font (7), black)
    Font.Draw ("Hide Cursor", 210, 310, font (14), black)
    Font.Draw ("In Turing, we can hide the cusor by using the predefined", 240, 280, font (5), black)
    Font.Draw ("procedure", 210, 260, font (5), black)
    Font.Draw ("setscreen (\"nocursor\")", 275, 260, font (7), black)
    Font.Draw ("and reveal again by using", 430, 260, font (5), black)
    Font.Draw ("the procedure", 210, 240, font (5), black)
    Font.Draw ("setscreen (\"cursor\").", 298, 240, font (7), black)
    Font.Draw ("Syntax:   setscreen (\"nocursor\") and setscreen (\"cursor\")", 210, 210, font (6), black)
    Font.Draw ("This procedure is particularly useful in animated graphics.", 240, 180, font (5), black)
end lesson5

procedure lesson6
    Font.Draw ("Lesson 6:", 210, 338, font (7), black)
    Font.Draw ("Animating Graphics", 210, 310, font (14), black)
    Font.Draw ("In Turing, we can create animation by moving still", 250, 280, font (5), black)
    Font.Draw ("images to different position each time.", 210, 260, font (5), black)
end lesson6

procedure lesson7
    Font.Draw ("Lesson 7:", 210, 338, font (7), black)
    Font.Draw ("Delay", 210, 310, font (14), black)
    Font.Draw ("In Turing, we can pause the program from executing", 250, 280, font (5), black)
    Font.Draw ("for a duration of time by using the statement", 210, 260, font (5), black)
    Font.Draw ("delay.", 477, 260, font (7), black)
    Font.Draw ("Syntax:   delay (duration : int)", 210, 230, font (6), black)
    Font.Draw ("Duration is measured in milliseconds, so delay (1000)", 250, 200, font (5), black)
    Font.Draw ("equals to a 1 second delay. The statement delay is a very", 210, 180, font (5), black)
    Font.Draw ("useful in animation.", 210, 160, font (5), black)
end lesson7

procedure lesson8
    Font.Draw ("Lesson 8:", 210, 338, font (7), black)
    Font.Draw ("Pause for User Input", 210, 310, font (14), black)
    Font.Draw ("In Turing, we can also pause until a user inputs a single", 250, 280, font (5), black)
    Font.Draw ("character. To do this we need to learn the procedure", 210, 260, font (5), black)
    Font.Draw ("getch.", 530, 260, font (7), black)
    Font.Draw ("Syntax:   getch ( var ch : string ( 1 ) )", 210, 230, font (6), black)
    Font.Draw ("Also, we can stop the user input from being displayed on", 250, 200, font (5), black)
    Font.Draw ("screen by using", 210, 180, font (5), black)
    Font.Draw ("setscreen (\"noecho\")", 307, 180, font (7), black)
    Font.Draw ("and redisplay it by using", 450, 180, font (5), black)
    Font.Draw ("setscreen (\"echo\")", 210, 160, font (7), black)
    Font.Draw ("Syntax:   setscreen (\"noecho\") and setscreen (\"echo\")", 210, 130, font (6), black)
end lesson8

procedure quizGame
    loop
	loop
	    cls
	    color (white)

	    locate (17, 10)
	    put "Question 1."
	    locate (18, 10)
	    put "To output characters anywhere in the window, we use:"
	    locate (19, 10)
	    put "1.The mouse \t2.Use space bar \t3.Procedure locate \t4.You can't"
	    getch (ch)
	    if ch not= '3' then
		exit
	    end if

	    cls
	    locate (17, 5)
	    put "Question 2."
	    locate (18, 5)
	    put "Locate row 18, coloum 25:"
	    locate (19, 5)
	    put "1.locate (25,18) \t2.locate (18,25) \t3.where (18,25) \t4.where (25,18)"
	    getch (ch)
	    if ch not= '2' then
		exit
	    end if

	    cls
	    locate (17, 10)
	    put "Question 3."
	    locate (18, 10)
	    put "How do you clear window?"
	    locate (19, 10)
	    put "1.cls \t\t2.clear.window \t3.clear.screen \t4.clw"
	    getch (ch)
	    if ch not= '1' then
		exit
	    end if

	    cls
	    locate (16, 10)
	    put "Question 4."
	    locate (17, 10)
	    put "How do you give your text color?"
	    locate (18, 10)
	    put "1.write the color number in bracket behind the text"
	    locate (19, 10)
	    put "2.first do color (#)"
	    locate (20, 10)
	    put "3.first do colorword (#)"
	    locate (21, 10)
	    put "4.Impossible"
	    getch (ch)
	    if ch not= '2' then
		exit
	    end if

	    cls
	    locate (17, 10)
	    put "Question 5."
	    locate (18, 10)
	    put "Whats the maximum color number available in Turing? \t0-#"
	    locate (19, 10)
	    put "1.156 \t2.256 \t3.255 \t4.unlimited"
	    getch (ch)
	    if ch not= '3' then
		exit
	    end if

	    cls
	    locate (16, 10)
	    put "Question 6."
	    locate (17, 10)
	    put "How do you give background color?"
	    locate (18, 10)
	    put "1.colorback (#)"
	    locate (19, 10)
	    put "2.use color procedure"
	    locate (20, 10)
	    put "3.draw a colored box that fills the whole screen"
	    locate (21, 10)
	    put "4.Impossible"
	    getch (ch)
	    if ch not= '1' then
		exit
	    end if

	    cls
	    locate (16, 10)
	    put "Question 7."
	    locate (17, 10)
	    put "How do you hide and show the cursor?"
	    locate (18, 10)
	    put "1.To hide: noCursor, To show: showCursor"
	    locate (19, 10)
	    put "2.To hide: hideCursor, To show: cursor"
	    locate (20, 10)
	    put "3.To hide: setscreen (\"hidecursor\"), To show: setscreen (\"showcursor\")"
	    locate (21, 10)
	    put "4.To hide: setscreen (\"nocursor\"), To show: setscreen (\"cursor\")"
	    getch (ch)
	    if ch not= '4' then
		exit
	    end if

	    cls
	    locate (16, 10)
	    put "Question 8."
	    locate (17, 10)
	    put "How do you make animation in turing?"
	    locate (18, 10)
	    put "1.Use animation tools"
	    locate (19, 10)
	    put "2.Get those moving pictures on the internet"
	    locate (20, 10)
	    put "3.By having a series of still pictures slightly moved each time."
	    locate (21, 10)
	    put "4.You can't do animation in turing..."
	    getch (ch)
	    if ch not= '3' then
		exit
	    end if

	    cls
	    locate (17, 10)
	    put "Question 9."
	    locate (18, 10)
	    put "To pause the program, we use delay (duration). What unit is the duration measured in?"
	    locate (19, 10)
	    put "1.seconds \t2.milliseconds \t3.minutes \t4.microseconds"
	    getch (ch)
	    if ch not= '2' then
		exit
	    end if

	    cls
	    locate (17, 10)
	    put "Question 10."
	    locate (18, 10)
	    put "How can we pause the program until the user presses a key? (best option)"
	    locate (19, 10)
	    put "1.delay \t2.get \t\t3.getch (reply) \t4.get and delay"
	    getch (ch)
	    if ch not= '3' then
		exit
	    end if

	    win := true
	    exit
	end loop
	exit when win = true
	cls
	Font.Draw ("INCORRECT!!!", maxx div 2 - 50, maxy div 2, font (14), white)
	delay (2000)
    end loop
    cls
    Font.Draw ("YOU WIN!!!", maxx div 2 - 50, maxy div 2, font (14), white)
    delay (3000)
end quizGame

procedure quiz
    Font.Draw ("Quiz Time!", 210, 325, font (14), black)
    if mouseX > 320 and mouseX < 442 and mouseY > 225 and mouseY < 246 then
	Pic.Draw (quizMouseOver, 320, 225, picMerge)
	if mouseButton = 1 then
	    delay (100)
	    finished := true
	    Music.PlayFileStop
	    loading
	    %Extra just to make sure
	    delay (100)
	    finished := true
	    Music.PlayFileStop
	    %Quiz Here
	    delay (0)
	    fork bgMusic
	    music := "Final Countdown"
	    colorback (black)
	    cls
	    Font.Draw ("Welcome to the Quiz", maxx div 3, maxy div 2, font (14), white)
	    delay (3000)
	    cls
	    Font.Draw ("You will be asked 10 questions.", maxx div 3, maxy div 2, font (13), white)
	    delay (5000)
	    cls
	    Font.Draw ("If you answer them incorrectly,", maxx div 3, maxy div 2, font (13), white)
	    delay (3000)
	    cls
	    Font.Draw ("The Quiz will RESTART!!!", maxx div 3, maxy div 2, font (13), white)
	    delay (3000)
	    cls
	    Font.Draw ("So lets begin: 3", maxx div 3, maxy div 2, font (13), white)
	    delay (1000)
	    cls
	    Font.Draw ("So lets begin: 2", maxx div 3, maxy div 2, font (13), white)
	    delay (1000)
	    cls
	    Font.Draw ("So lets begin: 1", maxx div 3, maxy div 2, font (13), white)
	    delay (1000)
	    cls
	    Font.Draw ("Start!", maxx div 2 + 10, maxy div 2, font (13), white)
	    delay (1000)
	    win := false
	    quizGame
	    delay (100)
	    finished := true
	    Music.PlayFileStop
	    %Extra Just to make sure
	    delay (1)
	    finished := true
	    Music.PlayFileStop
	    delay (1)
	    fork bgMusic
	    music := "AboveTheTreetops"
	    View.Set ("offscreenonly")
	end if
    else
	Pic.Draw (quizNormal, 320, 225, picMerge)
    end if
end quiz

procedure showCredit
    Font.Draw ("Credit", 210, 325, font (14), black)
    Font.Draw ("Programmed and edited by:", 210, 286, font (9), black)
    Font.Draw ("Sunny Li", 450, 286, font (11), black)
    Font.Draw ("Image and Sound extracted from: Maplestory", 210, 250, font (8), black)
    Font.Draw ("Original Artworks by: Wizet", 210, 230, font (8), black)
    Font.Draw ("Soundtracks (excluding in quiz) by: Wizet", 210, 210, font (8), black)
    Font.Draw ("Original Game Owned by Nexon Corporation", 210, 170, font (8), black)
    Font.Draw ("COPYRIGHT C 2003 NEXON CORPORATION.", 210, 148, font (5), black)
    if mouseX > 420 and mouseX < 542 and mouseY > 322 and mouseY < 343 then
	Pic.Draw (creditMouseOver, 420, 322, picMerge)
	if mouseButton = 1 then
	    delay (100)
	    finished := true
	    Music.PlayFileStop
	    startUp
	    fork bgMusic
	    music := "AboveTheTreetops"
	    View.Set ("offscreenonly")
	end if
    else
	Pic.Draw (creditNormal, 420, 322, picMerge)
    end if
end showCredit



procedure contentSelect

    View.Set ("offscreenonly")

    toExit := 0

    for screenDrop : 0 .. 57
	Pic.Draw (csBg, -94, 570 - screenDrop * 9, picCopy)
	Pic.Draw (signboard1, 125, 868 - screenDrop * 9, picMerge)
	Pic.Draw (signboard, 48, 990 - screenDrop * 9, picMerge)
	Pic.Draw (background, -30, 0 - screenDrop * 10, picCopy)
	Pic.Draw (title, 180, 225 - screenDrop * 10, picMerge)
	Pic.Draw (loginNormalBox, 330, 337 - screenDrop * 10, picMerge)
	Pic.Draw (loginNormal, 587, 473 - screenDrop * 10, picCopy)
	Font.Draw ("No Pass Required", 450, 475 - screenDrop * 10, font (7), white)
	Font.Draw ("Brought to you by Sunny Li", 400, 435 - screenDrop * 10, font (8), white)
	Font.Draw (name, 450, 505 - screenDrop * 10, font (1), white)
	Pic.Draw (boarder, -20, -30, picMerge)
	View.Update
    end for

    delay (800)
    soundFinished := true
    Music.PlayFileStop

    delay (100) %Required to avoid sound glitch
    fork bgMusic
    music := "AboveTheTreetops"

    loop
	Input.KeyDown (key)
	Mouse.Where (mouseX, mouseY, mouseButton)
	Pic.Draw (csBg, -94, 57, picCopy)

	if scrollPosition = 1 then
	    Pic.Draw (signboard1, 125, 355, picMerge)
	    Font.Draw ("Use mouse to select", 300, 373, font (8), black)
	    Font.Draw ("or input the lesson #, Q as Quiz, and C to see Credit", 240, 360, font (4), black)
	else
	    Pic.Draw (scroll3, 125, 90, picMerge)
	end if
	Pic.Draw (signboard, 48, 477, picMerge)
	Pic.Draw (boarder, -20, -30, picMerge)

	if mouseX > 0 and mouseX < 100 and mouseY > 120 and mouseY < 157 or key (KEY_ESC) then
	    if mouseButton = 1 or key (KEY_ESC) then
		Pic.Draw (backPressed, -18, 110, picMerge)
		View.Update
		toExit := 1
		delay (100)
	    else
		Pic.Draw (backMouseOver, -18, 110, picMerge)
	    end if
	else
	    Pic.Draw (backNormal, -18, 110, picMerge)
	end if

	ch := ""
	if hasch then
	    getch (ch)
	end if

	%When pressed the Exit button in content select, toExit becomes one...so this is what it'll happen
	if toExit = 1 then
	    for screenRaise : 0 .. 57
		Pic.Draw (csBg, -94, 57 + screenRaise * 9, picCopy)
		Pic.Draw (signboard1, 125, 355 + screenRaise * 9, picMerge)
		Pic.Draw (signboard, 48, 477 + screenRaise * 9, picMerge)
		Pic.Draw (background, -30, -570 + screenRaise * 10, picCopy)
		Pic.Draw (title, 180, -345 + screenRaise * 10, picMerge)
		Pic.Draw (loginNormalBox, 330, -233 + screenRaise * 10, picMerge)
		Pic.Draw (loginNormal, 587, -97 + screenRaise * 10, picCopy)
		Font.Draw ("No Pass Required", 450, -95 + screenRaise * 10, font (7), white)
		Font.Draw ("Brought to you by Sunny Li", 400, -135 + screenRaise * 10, font (8), white)
		Font.Draw (name, 450, -65 + screenRaise * 10, font (1), white)
		Pic.Draw (boarder, -20, -30, picMerge)
		View.Update
	    end for
	    setscreen ("nooffscreenonly")
	    exit
	end if

	%Close Scroll Thingy
	if mouseX > 75 and mouseX < 165 and mouseY > 75 and mouseY < 543
		or mouseX > 608 and mouseX < 695 and mouseY > 75 and mouseY < 543
		or mouseX > 75 and mouseX < 695 and mouseY > 75 and mouseY < 90
		or mouseX > 75 and mouseX < 695 and mouseY > 485 and mouseY < 543
		or ch = "0" then
	    if scrollPosition not= 1 and mouseButton = 1 or scrollPosition not= 1 and ch = "0" then
		scrollUp
		%drawfillbox (75,75,695,543,black)
		selectDown := 0
	    end if
	end if

	%Lesson 1 (Locate)
	if mouseX > 168 and mouseX < 192 and mouseY > 388 and mouseY < 482 or ch = '1' then
	    Pic.Draw (selectNormal, 168, 383, picMerge)
	    Pic.Draw (selectFocus, 165, 382, picMerge)
	    Pic.Draw (textLesson, 169, 400, picMerge)
	    Font.Draw ("1", 173, 393, font (12), white)
	    if selectDown = 1 then
		lesson1
	    elsif mouseButton = 1 or ch = '1' then
		Pic.Draw (selectNormal, 168, 383, picMerge)
		Pic.Draw (textLesson, 170, 400, picMerge)
		Font.Draw ("1", 174, 393, font (12), white)
		scrollDown
		selectDown := 1
	    end if
	else
	    if selectDown = 1 then
		Pic.Draw (selectNormal, 168, 383, picMerge)
		Pic.Draw (textLesson, 170, 400, picMerge)
		Font.Draw ("1", 174, 393, font (12), white)
		lesson1
	    else
		Pic.Draw (selectNormal, 168, 388, picMerge)
		Pic.Draw (textLesson, 170, 405, picMerge)
		Font.Draw ("1", 174, 398, font (12), white)
	    end if
	end if

	%Lesson 2
	if mouseX > 195 and mouseX < 219 and mouseY > 388 and mouseY < 482 or ch = '2' then
	    Pic.Draw (selectNormal, 195, 383, picMerge)
	    Pic.Draw (selectFocus, 192, 382, picMerge)
	    Pic.Draw (textLesson, 196, 400, picMerge)
	    Font.Draw ("2", 200, 393, font (12), white)
	    if selectDown = 2 then
		lesson2
	    elsif mouseButton = 1 or ch = '2' then
		Pic.Draw (selectNormal, 195, 383, picMerge)
		Pic.Draw (textLesson, 197, 400, picMerge)
		Font.Draw ("2", 201, 393, font (12), white)
		scrollDown
		selectDown := 2
	    end if
	else
	    if selectDown = 2 then
		Pic.Draw (selectNormal, 195, 383, picMerge)
		Pic.Draw (textLesson, 197, 400, picMerge)
		Font.Draw ("2", 201, 393, font (12), white)
		lesson2
	    else
		Pic.Draw (selectNormal, 195, 388, picMerge)
		Pic.Draw (textLesson, 197, 405, picMerge)
		Font.Draw ("2", 201, 398, font (12), white)
	    end if
	end if

	%Lesson 3
	if mouseX > 222 and mouseX < 246 and mouseY > 388 and mouseY < 482 or ch = '3' then
	    Pic.Draw (selectNormal, 222, 383, picMerge)
	    Pic.Draw (selectFocus, 219, 382, picMerge)
	    Pic.Draw (textLesson, 223, 400, picMerge)
	    Font.Draw ("3", 227, 393, font (12), white)
	    if selectDown = 3 then
		lesson3
	    elsif mouseButton = 1 or ch = '3' then
		Pic.Draw (selectNormal, 222, 383, picMerge)
		Pic.Draw (textLesson, 224, 400, picMerge)
		Font.Draw ("3", 228, 393, font (12), white)
		scrollDown
		selectDown := 3
	    end if
	else
	    if selectDown = 3 then
		Pic.Draw (selectNormal, 222, 383, picMerge)
		Pic.Draw (textLesson, 224, 400, picMerge)
		Font.Draw ("3", 228, 393, font (12), white)
		lesson3
	    else
		Pic.Draw (selectNormal, 222, 388, picMerge)
		Pic.Draw (textLesson, 224, 405, picMerge)
		Font.Draw ("3", 228, 398, font (12), white)
	    end if
	end if

	%Lesson 4
	if mouseX > 249 and mouseX < 273 and mouseY > 388 and mouseY < 482 or ch = '4' then
	    Pic.Draw (selectNormal, 249, 383, picMerge)
	    Pic.Draw (selectFocus, 246, 382, picMerge)
	    Pic.Draw (textLesson, 250, 400, picMerge)
	    Font.Draw ("4", 254, 393, font (12), white)
	    if selectDown = 4 then
		lesson4
	    elsif mouseButton = 1 or ch = '4' then
		Pic.Draw (selectNormal, 249, 383, picMerge)
		Pic.Draw (textLesson, 251, 400, picMerge)
		Font.Draw ("4", 255, 393, font (12), white)
		scrollDown
		selectDown := 4
	    end if
	else
	    if selectDown = 4 then
		Pic.Draw (selectNormal, 249, 383, picMerge)
		Pic.Draw (textLesson, 251, 400, picMerge)
		Font.Draw ("4", 255, 393, font (12), white)
		lesson4
	    else
		Pic.Draw (selectNormal, 249, 388, picMerge)
		Pic.Draw (textLesson, 251, 405, picMerge)
		Font.Draw ("4", 255, 398, font (12), white)
	    end if
	end if

	%Lesson 5
	if mouseX > 276 and mouseX < 300 and mouseY > 388 and mouseY < 482 or ch = '5' then
	    Pic.Draw (selectNormal, 276, 383, picMerge)
	    Pic.Draw (selectFocus, 273, 382, picMerge)
	    Pic.Draw (textLesson, 277, 400, picMerge)
	    Font.Draw ("5", 281, 393, font (12), white)
	    if selectDown = 5 then
		lesson5
	    elsif mouseButton = 1 or ch = '5' then
		Pic.Draw (selectNormal, 276, 383, picMerge)
		Pic.Draw (textLesson, 278, 400, picMerge)
		Font.Draw ("5", 282, 393, font (12), white)
		scrollDown
		selectDown := 5
	    end if
	else
	    if selectDown = 5 then
		Pic.Draw (selectNormal, 276, 383, picMerge)
		Pic.Draw (textLesson, 278, 400, picMerge)
		Font.Draw ("5", 282, 393, font (12), white)
		lesson5
	    else
		Pic.Draw (selectNormal, 276, 388, picMerge)
		Pic.Draw (textLesson, 278, 405, picMerge)
		Font.Draw ("5", 282, 398, font (12), white)
	    end if
	end if

	%Lesson 6
	if mouseX > 303 and mouseX < 327 and mouseY > 388 and mouseY < 482 or ch = '6' then
	    Pic.Draw (selectNormal, 303, 383, picMerge)
	    Pic.Draw (selectFocus, 300, 382, picMerge)
	    Pic.Draw (textLesson, 304, 400, picMerge)
	    Font.Draw ("6", 308, 393, font (12), white)
	    if selectDown = 6 then
		lesson6
	    elsif mouseButton = 1 or ch = '6' then
		Pic.Draw (selectNormal, 303, 383, picMerge)
		Pic.Draw (textLesson, 305, 400, picMerge)
		Font.Draw ("6", 309, 393, font (12), white)
		scrollDown
		selectDown := 6
	    end if
	else
	    if selectDown = 6 then
		Pic.Draw (selectNormal, 303, 383, picMerge)
		Pic.Draw (textLesson, 305, 400, picMerge)
		Font.Draw ("6", 309, 393, font (12), white)
		lesson6
	    else
		Pic.Draw (selectNormal, 303, 388, picMerge)
		Pic.Draw (textLesson, 305, 405, picMerge)
		Font.Draw ("6", 309, 398, font (12), white)
	    end if
	end if

	%Lesson 7
	if mouseX > 330 and mouseX < 354 and mouseY > 388 and mouseY < 482 or ch = '7' then
	    Pic.Draw (selectNormal, 330, 383, picMerge)
	    Pic.Draw (selectFocus, 327, 382, picMerge)
	    Pic.Draw (textLesson, 331, 400, picMerge)
	    Font.Draw ("7", 335, 393, font (12), white)
	    if selectDown = 7 then
		lesson7
	    elsif mouseButton = 1 or ch = '7' then
		Pic.Draw (selectNormal, 330, 383, picMerge)
		Pic.Draw (textLesson, 332, 400, picMerge)
		Font.Draw ("7", 336, 393, font (12), white)
		scrollDown
		selectDown := 7
	    end if
	else
	    if selectDown = 7 then
		Pic.Draw (selectNormal, 330, 383, picMerge)
		Pic.Draw (textLesson, 332, 400, picMerge)
		Font.Draw ("7", 336, 393, font (12), white)
		lesson7
	    else
		Pic.Draw (selectNormal, 330, 388, picMerge)
		Pic.Draw (textLesson, 332, 405, picMerge)
		Font.Draw ("7", 336, 398, font (12), white)
	    end if
	end if

	%Lesson 8
	if mouseX > 357 and mouseX < 381 and mouseY > 388 and mouseY < 482 or ch = '8' then
	    Pic.Draw (selectNormal, 357, 383, picMerge)
	    Pic.Draw (selectFocus, 354, 382, picMerge)
	    Pic.Draw (textLesson, 358, 400, picMerge)
	    Font.Draw ("8", 362, 393, font (12), white)
	    if selectDown = 8 then
		lesson8
	    elsif mouseButton = 1 or ch = '8' then
		Pic.Draw (selectNormal, 357, 383, picMerge)
		Pic.Draw (textLesson, 359, 400, picMerge)
		Font.Draw ("8", 363, 393, font (12), white)
		scrollDown
		selectDown := 8
	    end if
	else
	    if selectDown = 8 then
		Pic.Draw (selectNormal, 357, 383, picMerge)
		Pic.Draw (textLesson, 359, 400, picMerge)
		Font.Draw ("8", 363, 393, font (12), white)
		lesson8
	    else
		Pic.Draw (selectNormal, 357, 388, picMerge)
		Pic.Draw (textLesson, 359, 405, picMerge)
		Font.Draw ("8", 363, 398, font (12), white)
	    end if
	end if

	%Quiz
	if mouseX > 411 and mouseX < 435 and mouseY > 388 and mouseY < 482 or ch = 'q' or ch = 'Q' then
	    Pic.Draw (selectNormal, 411, 383, picMerge)
	    Pic.Draw (selectFocus, 408, 382, picMerge)
	    Pic.Draw (textQuiz, 412, 400, picMerge)
	    if selectDown = 9 then
		quiz
	    elsif mouseButton = 1 or ch = 'q' or ch = 'Q' then
		Pic.Draw (selectNormal, 411, 383, picMerge)
		Pic.Draw (textQuiz, 413, 400, picMerge)
		scrollDown
		selectDown := 9
	    end if
	else
	    if selectDown = 9 then
		Pic.Draw (selectNormal, 411, 383, picMerge)
		Pic.Draw (textQuiz, 413, 400, picMerge)
		quiz
	    else
		Pic.Draw (selectNormal, 411, 388, picMerge)
		Pic.Draw (textQuiz, 413, 405, picMerge)
	    end if
	end if

	%Credit
	if mouseX > 546 and mouseX < 570 and mouseY > 388 and mouseY < 482 or ch = 'c' or ch = 'C' then
	    Pic.Draw (selectNormal, 546, 383, picMerge)
	    Pic.Draw (selectFocus, 543, 382, picMerge)
	    Pic.Draw (textCredit, 547, 400, picMerge)
	    if selectDown = 10 then
		showCredit
	    elsif mouseButton = 1 or ch = 'c' or ch = 'C' then
		Pic.Draw (selectNormal, 546, 383, picMerge)
		Pic.Draw (textCredit, 548, 400, picMerge)
		scrollDown
		selectDown := 10
	    end if
	else
	    if selectDown = 10 then
		Pic.Draw (selectNormal, 546, 383, picMerge)
		Pic.Draw (textCredit, 548, 400, picMerge)
		showCredit
	    else
		Pic.Draw (selectNormal, 546, 388, picMerge)
		Pic.Draw (textCredit, 548, 405, picMerge)
	    end if
	end if

	%Empty Boards (Dark Wood)
	/*Pic.Draw (selectDisable, 168, 388, picMerge)
	 Pic.Draw (selectDisable, 195, 388, picMerge)
	 Pic.Draw (selectDisable, 222, 388, picMerge)
	 Pic.Draw (selectDisable, 249, 388, picMerge)
	 Pic.Draw (selectDisable, 276, 388, picMerge)
	 Pic.Draw (selectDisable, 303, 388, picMerge)
	 Pic.Draw (selectDisable, 330, 388, picMerge)
	 Pic.Draw (selectDisable, 357, 388, picMerge)*/
	Pic.Draw (selectDisable, 384, 388, picMerge)
	%Pic.Draw (selectDisable, 411, 388, picMerge)
	Pic.Draw (selectDisable, 438, 388, picMerge)
	Pic.Draw (selectDisable, 465, 388, picMerge)
	Pic.Draw (selectDisable, 492, 388, picMerge)
	Pic.Draw (selectDisable, 519, 388, picMerge)
	%Pic.Draw (selectDisable, 546, 388, picMerge)
	Pic.Draw (selectDisable, 573, 388, picMerge)

	View.Update
	cls
    end loop

    delay (100)
    finished := true
    Music.PlayFileStop

    View.Set ("nooffscreenonly")

end contentSelect


%======================================================================================================
% Ingame (Abandoned) (Maybe later?)
%======================================================================================================

/*
 %UI bar
 Pic.Draw (userUIbarBG, 0, 0, picMerge)
 Pic.Draw (userUIbar, 3, 0, picMerge)
 Pic.Draw (healthBar, 217, 2, picMerge)
 Pic.Draw (graduation, 217, 2, picMerge)
 Pic.Draw (chatTarget, 3, 43, picMerge)
 Font.Draw ("Chat Disabled", 10, 50, font (2), white)

 %User
 Font.Draw (name, 88, 8, font (2), white)


 %Level
 Font.Draw ("Level ", 25, 25, font (4), white)
 procedure levelNum
 drawfillbox (57, 25, 80, 35, 126)
 Font.Draw (intstr (level), 57, 25, font (4), white)
 end levelNum

 %Fonts
 Font.Draw ("HP", 150, 55, font (7), white)
 Font.Draw ("MP", 300, 55, font (7), white)

 loop
 levelNum
 level := level + 1
 delay (25)
 exit when level > 200
 end loop

 View.Update
 */


%======================================================================================================
% Page Mangement
%======================================================================================================

startUp
loop
    loginNormalScreen
    contentSelect
    exit when toExit = 0
end loop
