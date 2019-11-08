
HAI 1.4

  OBTW
    A simple TicTacToe game.
    By Rodrigo Caldeira.
    
    To play, run lci tictactoe.lol and inform the cell with 2
    characters, using 0-index.
    
    For example, to play on first cell, inform 00. On middle cell, 11, and so on...
    
    The program exits on draw, win or if inform "Q"
    
  TLDR
  
  CAN HAS STRING?
  
  BTW BOARD CLASS (OR SO...)
  O HAI IM BOARDGAEM
  
    I HAS A PLAYER ITZ 1
    I HAS A HASWINNER ITZ FAIL
  
    BTW NO CONSTRUCTOR ON LOLCODE. WORKAROUND :)
    HOW IZ I INITZ
    
      BTW CREATES THE VARS POS_0_0, POS_0_1, ETC.
      IM IN YR COLS UPPIN YR X TIL BOTH SAEM X AN 3
        IM IN YR ROWS UPPIN YR Y TIL BOTH SAEM Y AN 3
          ME HAS A SRS SMOOSH "POS_" AN X AN "_" AN Y MKAY ITZ "_"        
        IM OUTTA YR ROWS
      IM OUTTA YR COLS
    IF U SAY SO
    
    BTW DRAW THE BOARD
    HOW IZ I DRAW
      
      I HAS A ROW_1 ITZ SMOOSH "_" AN ME'Z POS_0_0 ...
        AN "|_" AN ME'Z POS_0_1 AN "|_" AN ME'Z POS_0_2 MKAY
        
      I HAS A ROW_2 ITZ SMOOSH "_" AN ME'Z POS_1_0 ...
        AN "|_" AN ME'Z POS_1_1 AN "|_" AN ME'Z POS_1_2 MKAY
        
      I HAS A ROW_3 ITZ SMOOSH " " AN ME'Z POS_2_0 ...
        AN "| " AN ME'Z POS_2_1 AN "| " AN ME'Z POS_2_2 MKAY
      
      VISIBLE ""
      
      VISIBLE ROW_1
      VISIBLE ROW_2
      VISIBLE ROW_3
      
      VISIBLE ""
           
    IF U SAY SO
    
    BTW GET THE OPTION INFORMED BY THE PLAYER AND... PLAY :D
    HOW IZ I PLAY YR OPTION
    
      BTW RETURNS IF "Q"
      BOTH SAEM OPTION AN "Q", O RLY?, YA RLY, FOUND YR WIN, OIC
    
      BTW IF THE OPTION IS A VALID ONE
      I IZ ME'Z VALID_OPTION YR OPTION MKAY, O RLY?
        YA RLY
        
          BTW THE FIRST CHAR OF THE OPTION
          I HAS A ONEST_CHAR ITZ I IZ STRING'Z AT YR OPTION AN YR 0 MKAY
          
          BTW THE SECOND CHAR OF THE OPTION
          I HAS A TWOTH_CHAR ITZ I IZ STRING'Z AT YR OPTION AN YR 1 MKAY
          
          BTW CHECK WHICH PLAYER IS PLAYING NOW
          I HAS A CHECKBOXLOL
          
          BOTH SAEM ME'Z PLAYER AN 1, O RLY?
            YA RLY, CHECKBOXLOL R "X" BTW PLAYER 1
            NO WAI, CHECKBOXLOL R "O" BTW PLAYER 2
          OIC
          
          BTW GET THE EQUIVALENT POSITION USING THE OPTION
          I HAS A POS ITZ SMOOSH "POS_" AN ONEST_CHAR AN "_" AN TWOTH_CHAR MKAY
          
          BTW IF THE POSITION INFORMED IS EMPTY
          BOTH SAEM ME'Z SRS POS AN "_", O RLY?
            YA RLY
              ME'Z SRS POS R CHECKBOXLOL
              
              I HAS A FULLROW1 ITZ FAIL
              I HAS A FULLROW2 ITZ FAIL
              I HAS A FULLROW3 ITZ FAIL

              I HAS A FULLCOL1 ITZ FAIL
              I HAS A FULLCOL2 ITZ FAIL
              I HAS A FULLCOL3 ITZ FAIL
              
              I HAS A FULLCROSS1 ITZ FAIL
              I HAS A FULLCROSS2 ITZ FAIL
              
              BTW CHECK THE ROWS
              BOTH SAEM ME'Z POS_0_0 AN CHECKBOXLOL, O RLY?
                YA RLY, BOTH SAEM ME'Z POS_0_1 AN CHECKBOXLOL, O RLY?
                  YA RLY, BOTH SAEM ME'Z POS_0_2 AN CHECKBOXLOL, O RLY?
                    YA RLY, FULLROW1 R WIN
                  OIC
                OIC
              OIC
              
              BOTH SAEM ME'Z POS_1_0 AN CHECKBOXLOL, O RLY?
                YA RLY, BOTH SAEM ME'Z POS_1_1 AN CHECKBOXLOL, O RLY?
                  YA RLY, BOTH SAEM ME'Z POS_1_2 AN CHECKBOXLOL, O RLY?
                    YA RLY, FULLROW2 R WIN
                  OIC
                OIC
              OIC
              
              BOTH SAEM ME'Z POS_2_0 AN CHECKBOXLOL, O RLY?
                YA RLY, BOTH SAEM ME'Z POS_2_1 AN CHECKBOXLOL, O RLY?
                  YA RLY, BOTH SAEM ME'Z POS_2_2 AN CHECKBOXLOL, O RLY?
                    YA RLY, FULLROW3 R WIN
                  OIC
                OIC
              OIC
              
              BTW CHECK THE COLS
              BOTH SAEM ME'Z POS_0_0 AN CHECKBOXLOL, O RLY?
                YA RLY, BOTH SAEM ME'Z POS_1_0 AN CHECKBOXLOL, O RLY?
                  YA RLY, BOTH SAEM ME'Z POS_2_0 AN CHECKBOXLOL, O RLY?
                    YA RLY, FULLCOL1 R WIN
                  OIC
                OIC
              OIC
              
              BOTH SAEM ME'Z POS_0_1 AN CHECKBOXLOL, O RLY?
                YA RLY, BOTH SAEM ME'Z POS_1_1 AN CHECKBOXLOL, O RLY?
                  YA RLY, BOTH SAEM ME'Z POS_2_1 AN CHECKBOXLOL, O RLY?
                    YA RLY, FULLCOL2 R WIN
                  OIC
                OIC
              OIC
              
              BOTH SAEM ME'Z POS_0_2 AN CHECKBOXLOL, O RLY?
                YA RLY, BOTH SAEM ME'Z POS_1_2 AN CHECKBOXLOL, O RLY?
                  YA RLY, BOTH SAEM ME'Z POS_2_2 AN CHECKBOXLOL, O RLY?
                    YA RLY, FULLCOL3 R WIN
                  OIC
                OIC
              OIC
              
              BTW CHECK THE CROSSES
              BOTH SAEM ME'Z POS_0_0 AN CHECKBOXLOL, O RLY?
                YA RLY, BOTH SAEM ME'Z POS_1_1 AN CHECKBOXLOL, O RLY?
                  YA RLY, BOTH SAEM ME'Z POS_2_2 AN CHECKBOXLOL, O RLY?
                    YA RLY, FULLCROSS1 R WIN
                  OIC
                OIC
              OIC
              
              BOTH SAEM ME'Z POS_0_2 AN CHECKBOXLOL, O RLY?
                YA RLY, BOTH SAEM ME'Z POS_1_1 AN CHECKBOXLOL, O RLY?
                  YA RLY, BOTH SAEM ME'Z POS_2_0 AN CHECKBOXLOL, O RLY?
                    YA RLY, FULLCROSS2 R WIN
                  OIC
                OIC
              OIC
              
              BTW RETURNS IF HASWINNER, LOL
              FULLROW1, O RLY?, YA RLY, ME'Z HASWINNER R WIN, GTFO, OIC
              FULLROW2, O RLY?, YA RLY, ME'Z HASWINNER R WIN, GTFO, OIC
              FULLROW3, O RLY?, YA RLY, ME'Z HASWINNER R WIN, GTFO, OIC
              
              FULLCOL1, O RLY?, YA RLY, ME'Z HASWINNER R WIN, GTFO, OIC
              FULLCOL2, O RLY?, YA RLY, ME'Z HASWINNER R WIN, GTFO, OIC
              FULLCOL3, O RLY?, YA RLY, ME'Z HASWINNER R WIN, GTFO, OIC
              
              FULLCROSS1, O RLY?, YA RLY, ME'Z HASWINNER R WIN, GTFO, OIC
              FULLCROSS2, O RLY?, YA RLY, ME'Z HASWINNER R WIN, GTFO, OIC

              BOTH SAEM ME'Z PLAYER AN 1, O RLY?
                YA RLY, ME'Z PLAYER R 2
                NO WAI, ME'Z PLAYER R 1
              OIC
            NO WAI BTW THE PLAYER INFORMED A NOT EMPTY CELL (duh)
              VISIBLE "MAN, U STUPID! ALREADY PLAYED THERE! TRY AGAIN"
          OIC            
              
        NO WAI BTW THE PLAYER INFORMED AN INVALID OPTION
          VISIBLE "U DUMB! WORNG OPNTION! TRY AGAIN"
      OIC
      
    IF U SAY SO
    
    BTW CHECKS IF THE OPTION INFORMED IS VALID :D
    HOW IZ I VALID_OPTION YR OPTION
    
      BTW GET THE OPTION'S LENGTH
      I HAS A LEN ITZ I IZ STRING'Z LEN YR OPTION MKAY
      
      BTW RETURNS FALSE IF THE LENGTH != 2
      DIFFRINT LEN AN 2, O RLY?
        YA RLY, FOUND YR FAIL
      OIC
      
      BTW SAME ABOVE... ROLL BACK A LITTLE :P
      I HAS A ONEST_CHAR ITZ I IZ STRING'Z AT YR OPTION AN YR 0 MKAY
      I HAS A TWOTH_CHAR ITZ I IZ STRING'Z AT YR OPTION AN YR 1 MKAY
      
      BTW CHECK THE FIRST CHAR. IF != 0 OR 1 OR 2, RETURN FALSE
      ONEST_CHAR, WTF?
        OMG "0"
        OMG "1"
        OMG "2"
          GTFO
        OMGWTF, FOUND YR FAIL
      OIC
      
      BTW SAME, SAME, SAME...
      TWOTH_CHAR, WTF?
        OMG "0"
        OMG "1"
        OMG "2"
          GTFO
        OMGWTF, FOUND YR FAIL
      OIC
      
      BTW YAY! PLAYER INFORMED A VERY WELL AND EXPECTED OPTION!
      FOUND YR WIN
    
    IF U SAY SO
    
    BTW CHECK IF THE GAME IS OVER WITHOUT ANY WINNER
    BTW IF THERE IS AT LEAST ONE EMPTY CELL, RETURNS TRUE
    HOW IZ I IZDRAWZ
      
      IM IN YR COLS UPPIN YR X TIL BOTH SAEM X AN 3
      
        IM IN YR ROWS UPPIN YR Y TIL BOTH SAEM Y AN 3

          BOTH SAEM ME'Z SRS SMOOSH "POS_" AN X AN "_" AN Y MKAY AN "_", O RLY?
            YA RLY, FOUND YR FAIL
          OIC
      
        IM OUTTA YR ROWS
        
      IM OUTTA YR COLS
      
      FOUND YR WIN
      
    IF U SAY SO
  
  KTHX
  
  BTW NEW INSTANCE
  I HAS A BOARD ITZ LIEK A BOARDGAEM
  
  BTW LOL! INITS
  I IZ BOARD'Z INITZ MKAY
  
  I HAS A OPTION ITZ A YARN
  
  IM IN YR ETERNAL_LOOP
    I IZ BOARD'Z DRAW MKAY

    BOARD'Z HASWINNER, O RLY?
      YA RLY
        VISIBLE SMOOSH "YAY! PLAYER " AN BOARD'Z PLAYER AN " WINS!" MKAY
        GTFO
      NO WAI
        I IZ BOARD'Z IZDRAWZ MKAY, O RLY?
          YA RLY        
            VISIBLE "DRAW LOL! U 2 SO DUMB! BYE!"
            GTFO
        OIC
    OIC
    
    VISIBLE SMOOSH "PLAYER " AN BOARD'Z PLAYER ...
      AN ", GIMMEH OPTION PLZ (Q TO BYE):: " MKAY!
    GIMMEH OPTION
    
    I IZ BOARD'Z PLAY YR OPTION MKAY
    
    BOTH SAEM OPTION AN "Q", O RLY?
      YA RLY
        VISIBLE "K, BYE"
        GTFO
    OIC
    
  IM OUTTA YR ETERNAL_LOOP

KTHXBYE
