%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Programmer: Jeremy Dong
%Program Name: Sudoku
%Date: N/A
%Course:  ICS3CU1  Final Project 15%
%Teacher:  M. Ianni
%Descriptions:  A 9x9 grid will appear on the screen.
%               The player selects one grid and enters a number for that grid.
%               The objective is to fill the grid with digits so that each column,
%               each row, and each of the nine 3 subgrids that
%               compose the grid contains all of the digits from 1 to 9.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% files/code folder
include "files/code/includes.t"

%  This is an exemplar of a main program file for Connect 4.  The main program is not complete
%  for example it does not handle the case where the gameboard is full and therefore no winner.
%  It will not run because variables have not been declared and subprograms have not been written.

loop
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % procedure to set all initial global variable with file scope
    % even if already set (located in MyGlobalVars.t)
    setInitialGameValues
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % A.      display title screen
    %displayIntroWindow
    %
    %
    % B.      Ask user if they want instructions displayed on the screen
    setscreen ("graphics:640;640")
    greeting
    if YesToInstructions = "y" or YesToInstructions = "Y" then
	displayInstructionWindow   % The Instruction screen will display and pause the porgram
    end if

    cls
    Pic.ScreenLoad ("Images/s.bmp", 80, 500, picCopy)
    locate (10, 1)

    playgame
    
    cls
    Pic.ScreenLoad ("Images/s.bmp", 80, 500, picCopy)
    locate (10, 1)
    var key : string (1)
    put "Thank you for playing. Time used: ", round(t / 1000), "s\n"
    put "Play Again? (y/n) " ..
    getch (key)
    if key = "N" or key = "n" then
    cls
    Pic.ScreenLoad ("Images/s.bmp", 80, 500, picCopy)
    locate (20, 18)
    put "Sudoku by Jeremy Dong. All rights Reserved."
    locate (30, 23)
    put "\"Dream, as if you never wake up.\""
    exit
    end if

end loop
