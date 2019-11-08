(****************************************************************************)
(*                                                                          *)
(**)                              MODULE T3;                              (**)
(*                                                                          *)
(****************************************************************************)
(* Copyright (c) 1996, Thomas Schumacher                                    *)
(*                     JKU Linz                                             *)
(*                     e-Mail: k3084e9@c210.edvz.uni-linz.ac.at             *)
(****************************************************************************)
(* PURPOSE: To learn, how to write a simple Windows application in Oberon-2 *)
(*          Furthermore: Implementation of a simple game, known as          *)
(*                       "Doddelschach" in Austria                          *)
(* FUNCTIONS:                                                               *)
(*   WinMain() - calls initialization function, processes message loop      *)
(*   InitApplication() - initializes window data and registers window       *)
(*   InitInstance() - saves instance handle and creates main window         *)
(*   MainWndProc() - processes messages                                     *)
(*   About() - processes messages for "About" dialog box                    *)
(*                                                                          *)
(* COMMENTS:                                                                *)
(*   Windows can have several copies of your application running at the     *)
(*   same time.  The variable hInst keeps track of which instance this      *)
(*   application is so that processing will be to the correct window.       *)
(****************************************************************************)

IMPORT
   W := Windows, SYSTEM, Utils, Strings, R := Random (*Zufallsgen.*);

CONST
  IDM_GAMENEW = 100;
  IDM_GAMEEXIT = 101;
  IDM_ABOUT = 200;
  mainMenuName = "T3Menu";
  windowClassName = "T3WClass";
  windowTitleText = "T3!";
  
  FIELDWIDTH = 100; (* Größe eines Feldes des Spielfelds *)

VAR
  hInst: W.HANDLE;
  ringCursor: W.HCURSOR;
     (* Cursor für den Spieler *)
  
  gamefieldStartPosX, gamefieldStartPosY: INTEGER;
     (* Koordinaten der Client Area *)
  gamefield: ARRAY 3,3 OF INTEGER;
     (* Repräsentiert das Spielfeld: 0=kein Stein gesetzt (leer)
                                     1=Spieler 1 Stein gesetzt
                                     2=Spieler 2 Stein gesetzt *)
  playerFields: ARRAY 2,8 OF INTEGER;
     (* Enthält die Anzahl der von den Spielern gesetzten Steine 
        pro Zeile/Spalte/Diagonale:
           [x, 0..7]: x=0: Spieler 1; x=1: Spieler 2
           [x, 0..2]: Spaltensumme der gesetzten Steine
           [x, 3..5]: Zeilensumme der gesetzten Steine
           [x, 6]: Diagonalsumme \ der gesetzten Steine
           [x, 7]: Diagonalsumme / der gesetzten Steine *)
  possibleSetFieldX: ARRAY 5,3*3 OF INTEGER;
  possibleSetFieldY: ARRAY 5,3*3 OF INTEGER;
     (* [a, 3*3]: a=Prioritätsstufe (Erklärung siehe Dokumentation
                 (0=höchste Priorität, 4=niedrigste Priorität)
        [0..4, b]: x/y-Koordinate des möglichen Zuges *)
  possibleSetFieldCounter: ARRAY 5 OF INTEGER;
     (* Anzahl der in der Prioritätsstufe eingetragenen, möglichen Züge *)
  stonesSetCounter: INTEGER;
     (* Anzahl der insgesamt von beiden Spielern gesetzten Steine *)
  exitGame: BOOLEAN;
     (* TRUE, wenn das Spiel zu verlassen ist *)
  humanWonCounter, computerWonCounter, remisCounter: INTEGER;
     (* Anzahl der Spiele, die der Spieler/Computer gewonnen hat
        bzw. unentschieden ausgegangen sind *)


PROCEDURE ^ShowModalDialog(name: ARRAY OF CHAR; dp: W.DLGPROC; 
                           hWndParent: W.HWND; hInst: W.HINSTANCE);
PROCEDURE ^ComputerSets(hWnd: W.HWND): BOOLEAN;


PROCEDURE GameInitialisation(hWnd: W.HWND);
(* Initialisiert das Spielfeld, setzt die Zähler und das Feld mit den 
   Steinsummen zurück *)
VAR 
  i,j:INTEGER;
  r: BOOLEAN;
BEGIN
  stonesSetCounter := 0;
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      gamefield[i,j]:=0
    END; (*FOR*)
  END; (*FOR*)

  FOR j:=0 TO 7 DO
    playerFields[0,j]:=0;
    playerFields[1,j]:=0;
  END; (*FOR*)
  exitGame := FALSE;
  
  r := ComputerSets(hWnd);
END GameInitialisation;



PROCEDURE DrawStone(hWnd: W.HWND; 
                    playerNr: INTEGER; nrOfXField, nrOfYField: INTEGER);
(* Zeichnet den Spielstein des jeweiligen Spielers (playerNr=1: Kreis, 
   playerNr=2: Kreuz) in das Spielfeld mit der 
   Koordinate nrOfXField, nrOfYField *)
VAR
  hDc: W.HDC;
  ps : W.PAINTSTRUCT;
  hPenOld: W.HPEN;
  hPen: W.HPEN;
  r : LONGINT;

  centerX, centerY: INTEGER; (* Koordinaten des Feldmittelpunkts,
                                an den der Stein gezeichnet werden soll *)
  stoneHalfwidth: INTEGER; (* Halbe Breite des Steines (bzw. Radius *)
BEGIN
  hDc := W.GetDC(hWnd);

  centerX := gamefieldStartPosX + nrOfXField * FIELDWIDTH + FIELDWIDTH DIV 2;
  centerY := gamefieldStartPosY + nrOfYField * FIELDWIDTH + FIELDWIDTH DIV 2; 
  stoneHalfwidth := (FIELDWIDTH * 4) DIV 10;

  IF playerNr=1 THEN (* Zeichne Kreis *)
    hPen := W.CreatePen(W.PS_SOLID, 5, (255*256+70)*256+150);
    hPenOld := W.SelectObject(hDc, hPen);

    r := W.Ellipse(hDc, centerX-stoneHalfwidth, centerY-stoneHalfwidth, 
                        centerX+stoneHalfwidth, centerY+stoneHalfwidth);
    
    hPenOld := W.SelectObject(hDc, hPenOld);
    r := W.DeleteObject(hPen);
  ELSIF playerNr=2 THEN (* Zeichne Kreuz *)
    hPen := W.CreatePen(W.PS_SOLID, 5, (70*256+255)*256+100);
    hPenOld := W.SelectObject(hDc, hPen);

    r := W.MoveTo(hDc, centerX-stoneHalfwidth, centerY-stoneHalfwidth);
    r := W.LineTo(hDc, centerX+stoneHalfwidth, centerY+stoneHalfwidth);
    r := W.MoveTo(hDc, centerX+stoneHalfwidth, centerY-stoneHalfwidth);
    r := W.LineTo(hDc, centerX-stoneHalfwidth, centerY+stoneHalfwidth);

    hPenOld := W.SelectObject(hDc, hPenOld);
    r := W.DeleteObject(hPen);
  END; (* IF *)

  r := W.ReleaseDC(hWnd, hDc);
  RETURN;
END DrawStone;



PROCEDURE DrawGamefield(hWnd: W.HWND);
(* Zeichnet das Spielfeld mit den aktuell gesetzten Steinen *)
VAR
  hDc: W.HDC;
  ps : W.PAINTSTRUCT;
  clRect: W.RECT;
  hPenOld: W.HPEN;
  hPen: W.HPEN;
  r : LONGINT;
  
  gamefieldStartX, gamefieldStartY: INTEGER;
     (* Stelle in Client Area, an der das Spielfeld gezeichnet werden soll *)
  i,j: INTEGER;
BEGIN
  hDc := W.BeginPaint(hWnd, SYSTEM.ADR(ps));

  (* Startposition des Spielfelds merken: *)
  W.GetClientRect(hWnd, SYSTEM.ADR(clRect));
  gamefieldStartPosX := (clRect.right - clRect.left - 3 * FIELDWIDTH) DIV 2; 
  gamefieldStartPosY := (clRect.bottom - clRect.top - 3 * FIELDWIDTH) DIV 2;

  (* Aktuelle Stiftfarbe sichern und neue setzen: *)
  hPen := W.CreatePen(W.PS_SOLID, 3, (150*256+100)*256+150);
  hPenOld := W.SelectObject(hDc, hPen);
  
  (* Linien zeichnen: *)
  gamefieldStartX := (clRect.right - clRect.left - 3 * FIELDWIDTH) DIV 2;
  gamefieldStartY := (clRect.bottom - clRect.top - 3 * FIELDWIDTH) DIV 2;
  FOR i:=0 TO 3 DO
    (* Horizontale Linie(n): *)
    r := W.MoveTo(hDc, gamefieldStartX, gamefieldStartY + i * FIELDWIDTH);
    r := W.LineTo(hDc, gamefieldStartX + 3 * FIELDWIDTH, 
                       gamefieldStartY + i * FIELDWIDTH);

    (* Vertikale Linie(n): *)
    r := W.MoveTo(hDc, gamefieldStartX + i * FIELDWIDTH, gamefieldStartY);
    r := W.LineTo(hDc, gamefieldStartX + i * FIELDWIDTH, 
                       gamefieldStartY + 3 * FIELDWIDTH);
  END; (* FOR *)
  
  (* Steine zeichnen: *)
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      DrawStone(hWnd, gamefield[i][j], i, j)
    END; (*FOR*)
  END; (*FOR*)

  (* Vorherige Stiftfarbe zurückladen: *)
  hPenOld := W.SelectObject(hDc, hPenOld);
  r := W.DeleteObject(hPen);
  
  W.EndPaint(hWnd, SYSTEM.ADR(ps));
  RETURN;
END DrawGamefield;



PROCEDURE GetFieldCoords(mousePosX, mousePosY: INTEGER; 
                         VAR nrOfXField, nrOfYField: INTEGER): BOOLEAN;
(* Stellt anhand der übergebenen Mauskoordinaten (mousePosX, mousePosY) fest,
   welches Feld im Spielfeld (nrOfXField, nrOfYField) getroffen wurde.
   Gibt TRUE zurück, falls ein Feld getroffen wurde, in dem noch kein Stein
   gesetzt wurde, sonst FALSE *)
BEGIN
  nrOfXField := (mousePosX - gamefieldStartPosX) DIV FIELDWIDTH;
  nrOfYField := (mousePosY - gamefieldStartPosY) DIV FIELDWIDTH;
  RETURN (nrOfXField >= 0) & (nrOfXField <=2) 
         & (nrOfYField >= 0) & (nrOfYField <=2) 
         & (gamefield[nrOfXField, nrOfYField]=0); (* kein Stein gesetzt *)
END GetFieldCoords;




PROCEDURE PlayerSetsOn(playerNr: INTEGER; nrOfXField, nrOfYField: INTEGER);
(* Setzt den Stein des Spieler playerNr in das durch nrOfXField, nrOfYField
   angegebene Feld *)
BEGIN
  (* Stein im Spielfeld setzen: *)
  gamefield[nrOfXField, nrOfYField] := playerNr;
  INC(stonesSetCounter);

  (* Zeilensumme erhöhen: *)
  INC(playerFields[playerNr-1, nrOfXField]);
  (* Spaltensumme erhöhen: *)
  INC(playerFields[playerNr-1, 3+nrOfYField]);
  (* Diagonalsumme erhöhen: \ *)
  IF nrOfXField=nrOfYField THEN 
    INC(playerFields[playerNr-1, 6]);
  END; (* IF *)
  (* Diagonalesumme erhöhen: / *)
  IF ((nrOfXField=1) & (nrOfYField=1)) OR (ABS(nrOfXField-nrOfYField)=2) THEN 
    INC(playerFields[playerNr-1, 7]);
  END; (* IF *)
END PlayerSetsOn;



PROCEDURE PlayerWon(playerNr: INTEGER): BOOLEAN;
(* Liefert TRUE zurück, wenn der Spieler playerNr gewonnen hat *)
VAR
  i: INTEGER;
BEGIN
  (* Nachsehen, ob in einer Zeile/Spalte/Diagonale drei Steine des Spielers
     sind: *)
  FOR i:=0 TO 7 DO
    IF playerFields[playerNr-1, i]=3 THEN 
      RETURN TRUE;
    END; (* IF *);
  END; (* FOR *)
  RETURN FALSE;
END PlayerWon;



PROCEDURE ComputerFindAndSetBestDraw(hWnd: W.HWND);
(* Stellt die für den Computer setzbaren Felder fest und trägt sie der 
   Priorität nach ins Feld der möglichen Züge (possibleSetFieldX/Y) ein *)
VAR
  i,j, randomNr: INTEGER;
BEGIN
  (* Zähler und Feld der möglichen Züge initialisieren: *)
  FOR i:=0 TO 4 DO
    possibleSetFieldCounter[i] := 0;
    FOR j:=0 TO 3*3-1 DO
      possibleSetFieldX[i,j]:=0;
      possibleSetFieldY[i,j]:=0;
    END; (* FOR *)
  END; (* FOR *)

  (* Die besten Züge der Priorität nach ermitteln: *)
  FOR i:=0 TO 2 DO
    FOR j:=0 TO 2 DO
      IF gamefield[i,j]=0 THEN
        (* Falls bereits zwei eigene Steine in Zeile/Spalte/Diagonale, dann
           letzten dazusetzen und gewinnen: *)
        IF (playerFields[1, i]=2) OR (playerFields[1, j+3]=2) 
           OR ((i=j) & (playerFields[1, 6]=2)) 
           OR (((ABS(i-j)=2) OR ((i=1) & (j=1))) 
               & (playerFields[1, 7]=2)) THEN
          possibleSetFieldX[0, possibleSetFieldCounter[0]] := i;
          possibleSetFieldY[0, possibleSetFieldCounter[0]] := j;
          INC(possibleSetFieldCounter[0]);
        END; (* IF *)
        
        (* Falls Gegner bereits zwei Steine in Zeile/Spalte/Diagonale, dann
           Stein dazusetzen und dessen Sieg verhindern: *)
        IF (playerFields[0, i]=2) OR (playerFields[0, j+3]=2)
           OR ((i=j) & (playerFields[0, 6]=2)) 
           OR (((ABS(i-j)=2) OR ((i=1) & (j=1))) 
               & (playerFields[0, 7]=2)) THEN
          possibleSetFieldX[1, possibleSetFieldCounter[1]] := i;
          possibleSetFieldY[1, possibleSetFieldCounter[1]] := j;
          INC(possibleSetFieldCounter[1]);
        END; (* IF *)

        (* Falls der Gegner noch keinen Stein gesetzt, aber bereits eigener 
           Stein in Zeile/Spalte/Diagonale gesetzt ist, dann dazusetzen
           (günstiger Zug) *)
        IF ((playerFields[0, i]=0) & (playerFields[1,i]=1)) 
           OR ((playerFields[0, j+3]=0) & (playerFields[1, j+3]=1))
           OR ((i=j) & ((playerFields[0, 6]=0) & (playerFields[1, 6]=1))) 
           OR (((ABS(i-j)=2) OR ((i=1) & (j=1))) 
               & (playerFields[0, 7]=0) & (playerFields[1, 7]=1)) THEN
          possibleSetFieldX[2, possibleSetFieldCounter[2]] := i;
          possibleSetFieldY[2, possibleSetFieldCounter[2]] := j;
          INC(possibleSetFieldCounter[2]);
        END; (* IF *)

        (* Falls Gegner noch keinen Stein in Zeile/Spalte/Diagonale gesetzt 
           hat, dazusetzen: *)
        IF (playerFields[0, i]=0) OR (playerFields[0, j+3]=0)
            OR ((i=j) & (playerFields[0, 6]=0)) 
            OR (((ABS(i-j)=2) OR ((i=1) & (j=1))) 
                & (playerFields[0, 7]=0)) THEN
          possibleSetFieldX[3, possibleSetFieldCounter[3]] := i;
          possibleSetFieldY[3, possibleSetFieldCounter[3]] := j;
          INC(possibleSetFieldCounter[3]);
        ELSE
          (* Restliche (relativ ungünstige) Züge: *)
          possibleSetFieldX[4, possibleSetFieldCounter[4]] := i;
          possibleSetFieldY[4, possibleSetFieldCounter[4]] := j;
          INC(possibleSetFieldCounter[4]);
        END; (* IF *)
      END; (* IF *)
    END; (* FOR *)
  END; (* FOR *)
  
  (* Für einen der besten Züge entscheiden: *)
  FOR i:=0 TO 4 DO
    IF possibleSetFieldCounter[i]>0 THEN
      randomNr := SHORT(ENTIER(R.Uniform()*(possibleSetFieldCounter[i]))); 
      PlayerSetsOn(2, possibleSetFieldX[i, randomNr], 
                      possibleSetFieldY[i, randomNr]);
      DrawStone(hWnd, 2, possibleSetFieldX[i, randomNr], 
                         possibleSetFieldY[i, randomNr]);
      RETURN;
    END;
  END; (* FOR *)
END ComputerFindAndSetBestDraw;



PROCEDURE SetMinMaxInfo(hWnd: W.HWND; lParam: LONGINT);
(* Setzt die Struktur, die Windows benötigt, um die minimale Fenstergröße
   (beim Ziehen der Ränder) zu ermitteln *)
TYPE
  mmiPtr = POINTER TO W.MINMAXINFO;
VAR
  lpmmi: mmiPtr;
  windowRect: W.RECT; (* Ausdehnung des Windows *)
  clRect: W.RECT; (* Ausdehnung der Client Area *)
BEGIN
  (* Feststellen der Ausdehnungen des Windows: *)
  W.GetWindowRect(hWnd, SYSTEM.ADR(windowRect));
  W.GetClientRect(hWnd, SYSTEM.ADR(clRect));
  lpmmi := SYSTEM.VAL(mmiPtr, lParam);
  lpmmi.ptMinTrackSize.x := 3 * FIELDWIDTH 
                            + windowRect.right - windowRect.left 
                            - (clRect.right - clRect.left)
                            + 10; (* Rand zw. Windowgrenze u. Spielfeld *)
  lpmmi.ptMinTrackSize.y := 3 * FIELDWIDTH 
                            + windowRect.bottom - windowRect.top 
                            - (clRect.bottom - clRect.top)
                            + 10; (* Rand zw. Windowgrenze u. Spielfeld *)
END SetMinMaxInfo;



PROCEDURE [WINDOWS] GameFinished*(
                      hDlg: W.HWND;   (* window handle of the dialog box *)
                      message: W.WORD;(* type of message                 *)
                      wParam: W.WORD; (* message-specific information    *)
                      lParam: LONGINT
                    ): W.BOOL;
BEGIN
  IF message = W.WM_INITDIALOG THEN       (* message: initialize dialog box *)
    RETURN W.true
  ELSIF message = W.WM_COMMAND THEN       (* message: received a command    *)
    IF (wParam = W.IDYES) THEN
      W.EndDialog(hDlg, W.true);          (* Exits the dialog box           *)
      stonesSetCounter := 0;
    ELSIF (wParam = W.IDABORT) THEN
      W.EndDialog(hDlg, W.true);          (* Exits the dialog box           *)
      exitGame := TRUE;
    END;
    RETURN W.true
  END;
  RETURN W.false                          (* Didn't process a message       *)
END GameFinished;



PROCEDURE ShowWinningsInTitle(hWnd: W.HWND);
(* Zeigt die Anzahl der Spieler/Computer-gewonnen Spiele bzw. die 
   unentschieden beendeten Spiele in der Window-Titelzeile an *)
VAR
  titleText: ARRAY 256 OF CHAR;
  humanWonCounterAsString, computerWonCounterAsString, remisCounterAsString
     : ARRAY 5 OF CHAR;
BEGIN
  Strings.Str(humanWonCounter, humanWonCounterAsString);
  Strings.Str(computerWonCounter, computerWonCounterAsString);
  Strings.Str(remisCounter, remisCounterAsString);
  titleText := "T3!   You Won: ";
  Strings.Append(titleText, humanWonCounterAsString);
  Strings.Append(titleText, " / I Won: ");
  Strings.Append(titleText, computerWonCounterAsString);
  Strings.Append(titleText, " / Remis: ");
  Strings.Append(titleText, remisCounterAsString);
  W.SetWindowText(hWnd, SYSTEM.ADR(titleText));
END ShowWinningsInTitle;



PROCEDURE NoOneWon(hWnd: W.HWND);
(* Aktualisiert Titelzeile des Windows, zeigt den Remis-Dialog an und 
   initialisert ein neues Spiel bzw. beendet die Applikation *)
VAR
  r: LONGINT;
BEGIN
  INC(remisCounter);
  ShowWinningsInTitle(hWnd);  
  ShowModalDialog("RemisBox", GameFinished, hWnd, hInst);
  IF stonesSetCounter=0 THEN
    GameInitialisation(hWnd);
    W.InvalidateRect(hWnd, W.NULL, W.true); 
  ELSIF exitGame THEN
      r := W.DestroyWindow(hWnd)
  END; (* IF *)
END NoOneWon;


PROCEDURE ComputerWonGame(hWnd: W.HWND);
(* Aktualisiert Titelzeile des Windows, zeigt den Verloren-Dialog an und 
   initialisert ein neues Spiel bzw. beendet die Applikation *)
VAR
  r: LONGINT;
BEGIN
  INC(computerWonCounter);
  ShowWinningsInTitle(hWnd);  
  ShowModalDialog("IWonBox", GameFinished, hWnd, hInst);
  IF stonesSetCounter=0 THEN
    GameInitialisation(hWnd);
    W.InvalidateRect(hWnd, W.NULL, W.true); 
  ELSIF exitGame THEN
      r := W.DestroyWindow(hWnd)
  END; (* IF *)
END ComputerWonGame;


PROCEDURE HumanWonGame(hWnd: W.HWND);
(* Aktualisiert Titelzeile des Windows, zeigt den Gewonnen-Dialog an und 
   initialisert ein neues Spiel bzw. beendet die Applikation *)
VAR
  r: LONGINT;
BEGIN
  INC(humanWonCounter);
  ShowWinningsInTitle(hWnd);  
  ShowModalDialog("YouWonBox", GameFinished, hWnd, hInst);
  IF stonesSetCounter=0 THEN
    GameInitialisation(hWnd);
    W.InvalidateRect(hWnd, W.NULL, W.true); 
  ELSIF exitGame THEN
      r := W.DestroyWindow(hWnd)
  END; (* IF *)
END HumanWonGame;



PROCEDURE [WINDOWS] GameNew*(
                       hDlg: W.HWND;     (* window handle of the dialog box *)
                       message: W.WORD;  (* type of message                 *)
                       wParam: W.WORD;   (* message-specific information    *)
                       lParam: LONGINT
                    ): W.BOOL;
BEGIN
  IF message = W.WM_INITDIALOG THEN       (* message: initialize dialog box *)
    RETURN W.true                                                           
  ELSIF message = W.WM_COMMAND THEN       (* message: received a command    *)
    IF (wParam = W.IDYES) THEN
      W.EndDialog(hDlg, W.true);          (* Exits the dialog box           *)
      stonesSetCounter := 0;
    ELSIF (wParam = W.IDNO) THEN
      W.EndDialog(hDlg, W.true)           (* Exits the dialog box           *)
    END;
    RETURN W.true
  END;
  RETURN W.false                          (* Didn't process a message       *)
END GameNew;




PROCEDURE [WINDOWS] GameExit*(
                       hDlg: W.HWND;     (* window handle of the dialog box *)
                       message: W.WORD;  (* type of message                 *)
                       wParam: W.WORD;   (* message-specific information    *)
                       lParam: LONGINT
                    ): W.BOOL;
BEGIN
  IF message = W.WM_INITDIALOG THEN       (* message: initialize dialog box *)
    RETURN W.true
  ELSIF message = W.WM_COMMAND THEN       (* message: received a command    *)
    IF (wParam = W.IDABORT) THEN
      W.EndDialog(hDlg, W.true);          (* Exits the dialog box           *)
      exitGame := TRUE;
    ELSIF (wParam = W.IDNO) THEN
      W.EndDialog(hDlg, W.true);          (* Exits the dialog box           *)
      exitGame := FALSE;
    END;
    RETURN W.true
  END;
  RETURN W.false                          (* Didn't process a message       *)
END GameExit;



PROCEDURE HumanSets(hWnd: W.HWND; mouseXPos, mouseYPos: INTEGER): BOOLEAN;
(* Setzt gemäß den Koordinaten mouseXPos, mouseYPos den Spielerstein, 
   stellt fest, ob das Spiel zu Ende ist (gewonnen/unentschieden) und
   initialisiert bei Bedarf ein neues Spiel.
   Liefert TRUE zurück, wenn das Spiel fortgesetzt werden soll,
   FALSE falls es beendet ist (gewonnen/verloren/unentschieden) *)
VAR
  nrOfXField, nrOfYField: INTEGER;
BEGIN
  IF GetFieldCoords(mouseXPos, mouseYPos, nrOfXField, nrOfYField) THEN
    PlayerSetsOn(1, nrOfXField, nrOfYField);
    DrawStone(hWnd, 1, nrOfXField, nrOfYField);
  ELSE
    RETURN FALSE;
  END; (* IF *)
  
  IF PlayerWon(1) THEN
    HumanWonGame(hWnd);
    GameInitialisation(hWnd);
    W.InvalidateRect(hWnd, W.NULL, W.true); 
    RETURN FALSE;
  END; (* IF *)

  (* Prüfen, ob Remis: *)
  IF stonesSetCounter=3*3 THEN
    NoOneWon(hWnd);
    RETURN FALSE;
  END; (* IF *)
  
  RETURN TRUE;
END HumanSets;


PROCEDURE ComputerSets(hWnd: W.HWND): BOOLEAN;
(* Läßt den Computer setzen,
   stellt fest, ob das Spiel zu Ende ist (gewonnen/unentschieden) und
   initialisiert bei Bedarf ein neues Spiel.
   Liefert TRUE zurück, wenn das Spiel fortgesetzt werden soll,
   FALSE falls es beendet ist (gewonnen/verloren/unentschieden) *)
BEGIN
  (* Auf die beste Stelle im Spielfeld setzen: *)
  ComputerFindAndSetBestDraw(hWnd);
  
  (* Feststellen, ob gewonnen und ggf. neues Spiel einleiten: *)
  IF PlayerWon(2) THEN
    ComputerWonGame(hWnd);
    GameInitialisation(hWnd);
    W.InvalidateRect(hWnd, W.NULL, W.true); 
    RETURN FALSE;
  END; (* IF *)

  (* Prüfen, ob Remis: *)
  IF stonesSetCounter=3*3 THEN
    NoOneWon(hWnd);
    RETURN FALSE;
  END; (* IF *)
  
  RETURN TRUE;
END ComputerSets;



PROCEDURE UserClick(hWnd: W.HWND; mouseXPos, mouseYPos: INTEGER);
(* Ruft das Behandeln des Setzens des Spielers und anschließend das Setzen des 
   Computers auf. (Inkludiert den gesamten Spielablauf mit Graphikausgabe,
   Spiel-Handling und Dialogen) *)
BEGIN
  IF ~HumanSets(hWnd, mouseXPos, mouseYPos) THEN RETURN;
  END; (* IF *)
  IF ~ComputerSets(hWnd) THEN RETURN;
  END; (* IF *)
END UserClick;



(****************************************************************************)
(* FUNCTION: About(HWND, unsigned, WORD, LONG)                              *)
(*                                                                          *)
(* PURPOSE:  Processes messages for "About" dialog box                      *)
(*                                                                          *)
(* MESSAGES:                                                                *)
(*   W.WM_INITDIALOG - initialize dialog box                                *)
(*   W.WM_COMMAND    - Input received                                       *)
(*                                                                          *)
(* COMMENTS:                                                                *)
(*   No initialization is needed for this particular dialog box, but TRUE   *)
(*   must be returned to Windows.                                           *)
(*                                                                          *)
(*   Wait for user to click on "Ok" button, then close the dialog box.      *)
(****************************************************************************)

PROCEDURE [WINDOWS] About*(
                       hDlg: W.HWND;   (* window handle of the dialog box *)
                       message: W.WORD;(* type of message                 *)
                       wParam: W.WORD; (* message-specific information    *)
                       lParam: LONGINT
                    ): W.BOOL;
BEGIN
  IF message = W.WM_INITDIALOG THEN       (* message: initialize dialog box *)
    RETURN W.true
  ELSIF message = W.WM_COMMAND THEN       (* message: received a command    *)
    IF (wParam = W.IDOK) THEN
      W.EndDialog(hDlg, W.true)           (* Exits the dialog box           *)
    END;
    RETURN W.true
  END;
  RETURN W.false                          (* Didn't process a message       *)
END About;




PROCEDURE ShowModalDialog(name: ARRAY OF CHAR; dp: W.DLGPROC; 
                          hWndParent: W.HWND; hInst: W.HINSTANCE);
VAR
  r:    LONGINT;
BEGIN
  r := W.DialogBox(hInst,             (* current instance                 *)
                   SYSTEM.ADR(name),  (* resource to use                  *)
                   hWndParent,        (* parent handle                    *)
                   dp);               (* About() instance address         *)
END ShowModalDialog;




(****************************************************************************)
(* FUNCTION: MainWndProc(HWND, unsigned, WORD, LONG)                        *)
(*                                                                          *)
(* PURPOSE:  Processes messages                                             *)
(*                                                                          *)
(* MESSAGES:                                                                *)
(*   W.WM_COMMAND    - application menu (About dialog box)                  *)
(*   W.WM_DESTROY    - destroy window                                       *)
(*                                                                          *)
(* COMMENTS:                                                                *)
(*   To process the IDM_ABOUT message, call MakeProcInstance() to get the   *)
(*   current instance address of the About() function.  Then call Dialog    *)
(*   box which will create the box according to the information in your     *)
(*   generic.rc file and turn control over to the About() function.  When   *)
(*   it returns, free the intance address.                                  *)
(****************************************************************************)
PROCEDURE [WINDOWS] MainWndProc*(hWnd: W.HWND;   (* window handle           *)
                                 message: W.WORD;(* type of message         *)
                                 wParam: W.WORD; (* additional information  *)
                                 lParam: LONGINT (* additional information  *)
                                ): LONGINT;

VAR
    i: INTEGER;
    r: LONGINT;

BEGIN
  IF message = W.WM_COMMAND THEN          (* command from application menu  *)
    IF wParam = IDM_ABOUT THEN
      ShowModalDialog("AboutBox", About, hWnd, hInst);
    ELSIF wParam = IDM_GAMENEW THEN
      IF stonesSetCounter>0 THEN (* Spiel ist im Gange *)
        ShowModalDialog("NewGameBox", GameNew, hWnd, hInst);
        IF stonesSetCounter=0 THEN (* neues Spiel gewünscht *)
          GameInitialisation(hWnd);
          W.InvalidateRect(hWnd, W.NULL, W.true); 
        END; (* IF *)
      END; (* IF *)
    ELSIF wParam = IDM_GAMEEXIT THEN
      IF ((stonesSetCounter>0) & (~exitGame)) THEN
        ShowModalDialog("ExitGameBox", GameExit, hWnd, hInst);
        IF stonesSetCounter=0 THEN 
          GameInitialisation(hWnd);
          W.InvalidateRect(hWnd, W.NULL, W.true); 
        END; (* IF *)
      END; (* IF *)
      IF exitGame THEN
        r := W.DestroyWindow(hWnd);
      END; (* IF *)
    ELSE                                  (* Lets Windows process it        *)
      RETURN W.DefWindowProc(hWnd, message, wParam, lParam)
    END
  ELSIF message = W.WM_PAINT THEN
    DrawGamefield(hWnd);
  ELSIF message = W.WM_SIZE THEN
    W.InvalidateRect(hWnd, W.NULL, W.true); 
  ELSIF message = W.WM_LBUTTONDOWN THEN
    UserClick(hWnd, Utils.LoWord(lParam), Utils.HiWord(lParam));
  ELSIF message = W.WM_GETMINMAXINFO THEN
    SetMinMaxInfo(hWnd, lParam); 
  ELSIF message = W.WM_DESTROY THEN      (* message: window being destroyed *)
    r := W.DestroyCursor(ringCursor);
    W.PostQuitMessage(0);
 ELSE                                   (* Passes it on if unproccessed    *)
    RETURN W.DefWindowProc(hWnd, message, wParam, lParam)
  END;
  RETURN W.NULL
END MainWndProc;


(****************************************************************************)
(* FUNCTION: InitApplication(HANDLE)                                        *)
(*                                                                          *)
(* PURPOSE: Initializes window data and registers window class              *)
(*                                                                          *)
(* COMMENTS:                                                                *)
(*   This function is called at initialization time only if no other        *)
(*   instances of the application are running.  This function performs      *)
(*   initialization tasks that can be done once for any number of running   *)
(*   instances.                                                             *)
(*                                                                          *)
(*   In this case, we initialize a window class by filling out a data       *)
(*   structure of type WNDCLASS and calling the Windows RegisterClass()     *)
(*   function.  Since all instances of this application use the same window *)
(*   class, we only need to do this when the first instance is initialized. *)
(****************************************************************************)
PROCEDURE InitApplication(hInstance: W.HANDLE): BOOLEAN;
  VAR
    wc: W.WNDCLASS;
    mainWndProc: PROCEDURE [WINDOWS] (
                    hWnd: W.HWND; message: W.WORD;
                    wParam: W.WORD; lParam: LONGINT
                 ): LONGINT;

BEGIN                                  (* Fill in window class structure    *)
                                       (* with parameters that describe     *)
                                       (* the main window.                  *)
  mainWndProc := MainWndProc;
  wc.style := W.NULL;                  (* Class style(s).                   *)
  
  wc.lpfnWndProc := MainWndProc;       (* Function to retrieve messages for *)
                                       (* windows of this class.            *)
  wc.cbClsExtra := 0;                  (* No per-class extra data.          *)
  wc.cbWndExtra := 0;                  (* No per-window extra data.         *)
  wc.hInstance := hInstance;           (* Application that owns the class.  *)
  wc.hIcon := W.LoadIcon(hInstance, SYSTEM.ADR("T3ICON"));
  ringCursor := SYSTEM.VAL(W.HCURSOR, 
                           W.LoadIcon(hInstance, SYSTEM.ADR("T3CURSOR")));
  wc.hCursor := ringCursor;
  wc.hbrBackground := W.GetStockObject(W.WHITE_BRUSH);
  wc.lpszMenuName := SYSTEM.ADR(mainMenuName);    (* Name of menu resource 
                                                     in .RC file *)
  wc.lpszClassName := SYSTEM.ADR(windowClassName);   (* Name used in call 
                                                        to CreateWindow *)
                                          (* Register the window class and  *)
                                          (*  return success/failure code.  *)
  RETURN W.RegisterClass(SYSTEM.ADR(wc)) # 0
END InitApplication;


(****************************************************************************)
(* FUNCTION:  InitInstance(HANDLE, int)                                     *)
(*                                                                          *)
(* PURPOSE:  Saves instance handle and creates main window                  *)
(*                                                                          *)
(* COMMENTS:                                                                *)
(*   This function is called at initialization time for every instance of   *)
(*   this application.  This function performs initialization tasks that    *)
(*   cannot be shared by multiple instances.                                *)
(*                                                                          *)
(*   In this case, we save the instance handle in a static variable and     *)
(*   create and display the main program window.                            *)
(****************************************************************************)
PROCEDURE InitInstance(
             hInstance:W.HANDLE; (* Current instance identifier.      *)
             nCmdShow: INTEGER   (* Param for first ShowWindow() call *)
          ): BOOLEAN;

  VAR r: INTEGER; hWnd: W.HWND;        (* Main window handle.               *)
BEGIN                                  (* Save the instance handle in       *)
                                       (* static variable, which will be    *)
                                       (* used in many subsequence calls    *)
                                       (* from this application to Windows. *)
  hInst := hInstance;
                                       (* Create a main window for this     *)
                                       (*  application instance.            *)
  hWnd := W.CreateWindow(SYSTEM.ADR(windowClassName),
                                       (* See RegisterClass() call.         *)
                         SYSTEM.ADR(windowTitleText),
                                       (* Text for window title bar.        *)
                         W.WS_OVERLAPPEDWINDOW, (* Window style.            *)
                         W.CW_USEDEFAULT, (* Default horizontal position.   *)
                         W.CW_USEDEFAULT, (* Default vertical position.     *)
                         W.CW_USEDEFAULT, (* Default width.                 *)
                         W.CW_USEDEFAULT, (* Default height.                *)
                         W.NULL,       (* Overlapped windows have no parent *)
                         W.NULL,       (* Use the window class menu.        *)
                         hInstance,    (* This instance owns this window.   *)
                         W.NULL);      (* Pointer not needed.               *)

                                       (* If window could not be created,   *)
                                       (* return "failure"                  *)
  IF hWnd = 0 THEN RETURN FALSE END;
                                       (* Make the window visible; update   *)
                                       (* its client area; and return       *)
                                       (* "success"                         *)
  r := W.ShowWindow(hWnd, nCmdShow);   (* Show the window                   *)
  
  GameInitialisation(hWnd);
  humanWonCounter    := 0;
  computerWonCounter := 0;
  remisCounter       := 0;

  W.UpdateWindow(hWnd);                (* Sends WM_PAINT message            *)
  RETURN TRUE
END InitInstance;


(****************************************************************************)
(* FUNCTION: WinMain(HANDLE, HANDLE, LPSTR, int)                            *)
(*                                                                          *)
(* PURPOSE: calls initialization function, processes message loop           *)
(*                                                                          *)
(* COMMENTS:                                                                *)
(*   Windows recognizes this function by name as the initial entry point    *)
(*   for the program.  This function calls the application initialization   *)
(*   routine, if no other instance of the program is running, and always    *)
(*   calls the instance initialization routine.  It then executes a message *)
(*   retrieval and dispatch loop that is the top-level control structure    *)
(*   for the remainder of execution.  The loop is terminated when a WM_QUIT *)
(*   message is received, at which time this function exits the application *)
(*   instance by returning the value passed by PostQuitMessage().           *)
(*                                                                          *)
(*   If this function must abort before entering the message loop, it       *)
(*   returns the conventional value W.NULL.                                 *)
(****************************************************************************)
PROCEDURE [WINDOWS] WinMain*(hInstance: W.HANDLE;    (* current instance    *)
                             hPrevInstance: W.HANDLE;(* previous instance   *)
                             lpCmdLine: W.LPSTR;     (* command line        *)
                             nCmdShow: INTEGER       (* show-window type    *)
                            ): INTEGER;              (*   (open/icon)       *)
  VAR r: LONGINT; msg: W.MSG;             (* message                        *)
BEGIN
  IF hPrevInstance = 0 THEN              (* Other instances of app running? *)
    IF ~InitApplication(hInstance) THEN  (* Initialize shared things        *)
      RETURN W.false;                    (* Exits if unable to initialize   *)
    END;
  END;
                                          (* Perform initializations that   *)
                                          (* apply to a specific instance   *)
  IF ~InitInstance(hInstance, nCmdShow) THEN RETURN W.false END;
                                          (* Acquire and dispatch messages  *)
                                          (* until a WM_QUIT message is     *)
                                          (* received.                      *)
  WHILE W.GetMessage(SYSTEM.ADR(msg),     (* message structure              *)
                     W.NULL,              (* handle of window receiving ms  *)
                     W.NULL,              (* lowest message to examine      *)
                     W.NULL) # 0 DO       (* highest message to examine     *)

    r := W.TranslateMessage(SYSTEM.ADR(msg)); (* Translates virt. key codes *)
    r := W.DispatchMessage(SYSTEM.ADR(msg))   (* Dispatches message to win. *)
  END;
  RETURN msg.wParam
END WinMain;

END T3.
