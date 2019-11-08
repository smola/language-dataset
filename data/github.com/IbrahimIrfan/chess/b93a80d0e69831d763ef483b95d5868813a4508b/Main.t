%***********************************************************************%
%                          PROGRAM HEADER                               %
%***********************************************************************%
%                                                                       %
% PROGRAMMER'S NAME:    Ibrahim Irfan                                   %
%                                                                       %
% PROGRAM NAME:         ISP - Chess Game                                %
%                                                                       %
% CLASS:                ICS3U1-02                                       %
%                                                                       %
% TEACHER:              Mrs. Barsan                                     %
%                                                                       %
% DUE DATE:             Friday June 17, 2016                            %
%                                                                       %
%***********************************************************************%
% WHAT THE PROGRAM DOES                                                 %
%                                                                       %
% This program simulates a game of chess between two users. The user    %
% can move a piece by clicking on it first to see the available moves,  %
% and then clicking on a highlighted tile to move there. The first user %
% to take the opponent's king or make the other forfeit wins. All       %
% classic chess rules apply.                                            %
%                                                                       %
%***********************************************************************%
% PROCEDURES AND FUNCTIONS                                              %
%                                                                       %
% Refer to the specific procedures and functions for brief descriptions %
% of each.                                                              %
%                                                                       %
%***********************************************************************%
% PROGRAM LIMITATIONS                                                   %
%                                                                       %
% The program does not have a one player mode for a user to play        %
% against an AI. Also, the users only have the choice between being     %
% black or white pieces (no brown, etc). Also, there is no (working)    %
% checkmate detection.                                                  %
%                                                                       %
%***********************************************************************%
% EXTENSIONS AND IMPROVEMENTS                                           %
%                                                                       %
% 1. One player mode with AI.                                           %
% 2. More colours than just black and white (give the users options).   %
% 3. Working checkmate detction.                                        %
% 4. Restrict the user from making a move that would kill their king on %
%    the next turn.                                                     %
%                                                                       %
%***********************************************************************%

% include other turing files
include "Variables.t"
include "DrawPiece.t"
include "Intro.t"
include "Rules.t"
include "Init.t"
include "DrawScore.t"
include "DetectSpriteOverlap.t"
include "CheckCollision.t"
include "AssignMove.t"
include "AssignHighlight.t"
include "MovePiece.t"
include "CheckClick.t"
include "CheckDetection.t"
/*
Checkmate detection has been commented out since it is laggy and does not work consistently
To test out the checkmate, remove the comment below of %include "CheckMateDetection.t",
and in "Animate.t" remove the comments around the 2 long blocks of code which are commented out.
*/
%include "CheckMateDetection.t"
include "Animate.t"
include "WinDetection.t"

% main
loop
    intro_screen
    % if user presses rules, then display rules, if play then start game
    if x > 250 and y > 240 and x < 400 and y < 300 then
	rules_screen
    elsif x > 50 and y > 240 and x < 200 and y < 300 then
	init_board
	draw_score
	loop
	    exit when detect_win not= 2  % exit when someone wins or forfeits
	    if turn = 0 then  % white turn
		loop
		    exit when detect_win not= 2 % exit when someone wins or forfeits
		    % if move has finished then update score/turn and exit
		    if turn = 1 then
			if check = true then
			    drawfillbox (420, 30, 600, 55, red)
			else
			    drawfillbox (420, 30, 600, 55, blue)
			end if
			Font.Draw ("Black's turn", 450, 40, font4, white)
			draw_score
			exit
		    end if
		    Mouse.Where (x, y, button)
		    % if the user clicks somewhere, check all white pieces and forfeit button
		    if button = 1 then
			for i : 0 .. 15
			    check_click ( ^ (pointers (i)))
			    % exit if forfeit is clicked
			    if detect_win not= 2 then
				exit
			    end if
			    animate (pointers (i))
			end for
		    end if
		end loop
	    else % black turn
		loop
		    exit when detect_win not= 2 % exit when someone wins or forfeits
		    % if move has finished then update score/turn and exit
		    if turn = 0 then
			if check = true then
			    drawfillbox (420, 30, 600, 55, red)
			else
			    drawfillbox (420, 30, 600, 55, blue)
			end if
			Font.Draw ("White's turn", 450, 40, font4, white)
			draw_score
			exit
		    end if
		    Mouse.Where (x, y, button)
		    % if the user clicks somewhere, check all black pieces and forfeit button
		    if button = 1 then
			for i : 16 .. 31
			    check_click ( ^ (pointers (i)))
			    % exit if forfeit is clicked
			    if detect_win not= 2 then
				exit
			    end if
			    animate (pointers (i))
			end for
		    end if
		end loop
	    end if
	end loop
    end if
end loop
