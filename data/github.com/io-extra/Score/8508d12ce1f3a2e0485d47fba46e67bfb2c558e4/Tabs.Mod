MODULE Tabs;

  IMPORT SYSTEM, Files, Fonts, Notes;

  CONST 
    replace* = 0; insert* = 1; delete* = 2; unmark* = 3;  (*op-codes*)

  TYPE 
    Method* = POINTER TO MethodDesc;
    Phrase* = POINTER TO PhraseDesc;
    PhraseDesc* = RECORD
      x*, y*, w*, h*, d*, p*: INTEGER;
      sx*, sy*: REAL;
      sw*, sh*: INTEGER;
      selected*, marked*: BOOLEAN;
      do*: Method;
      next: Phrase
    END;

    Tab* = POINTER TO TabDesc;
    Notifier* = PROCEDURE (T: Tab; op: INTEGER; beg, end: LONGINT);
    TabDesc* = RECORD
      len*: LONGINT;
      changed*: BOOLEAN;
      flattened*: BOOLEAN;
      sel*, first: Phrase;
      notify*: Notifier
    END;

    Msg* = RECORD END ;
    WidMsg* = RECORD (Msg) w*: INTEGER END ;
    ColorMsg* = RECORD (Msg) col*: INTEGER END ;
    FontMsg* = RECORD (Msg) fnt*: Fonts.Font END ;
    

    bmpHeaderEntry = RECORD
        a: INTEGER;
        b: INTEGER;
        c: INTEGER;
        d: INTEGER
    END;

    jpgHeaderEntry = RECORD
        a: INTEGER;
        b: INTEGER;
        c: INTEGER;
        d: INTEGER
    END;

    pngHeaderEntry = RECORD
        a: INTEGER;
        b: INTEGER;
        c: INTEGER;
        d: INTEGER
    END;

    MethodDesc* = RECORD
(*        module*, allocator*: Name;
        new*: Modules.Command;             *)
        copy*: PROCEDURE (from, to: Phrase);
        draw*, change*: PROCEDURE (P: Phrase; VAR msg: Msg);
        selectable*: PROCEDURE (P: Phrase; x, y: INTEGER): BOOLEAN;
        read*: PROCEDURE (P: Phrase; VAR r: Files.Rider); (*; VAR c: Context); *)
        write*: PROCEDURE (P: Phrase; cno: INTEGER; VAR r: Files.Rider); (*; VAR c: Context);*)
        print*: PROCEDURE (P: Phrase; x, y: INTEGER)
      END ;

    
VAR 
 
  res*: INTEGER;
  width: INTEGER;
  new*: Phrase; 
  LineMethod*, CapMethod*, MacMethod* : Method;

  PROCEDURE New*(P: Phrase);
  BEGIN   new := P 
  END New;

  PROCEDURE Add*(T: Tab; P: Phrase);
  BEGIN P.marked := FALSE; P.selected := TRUE; P.next := T.first;
    T.first := P; T.sel := P; T.changed := TRUE
  END Add;

  PROCEDURE ThisPhrase*(T: Tab; x, y: INTEGER): Phrase;
    VAR P: Phrase;
  BEGIN P := T.first;
    WHILE (P # NIL) & ~P.do.selectable(P, x ,y) DO P := P.next END ;
    RETURN P
  END ThisPhrase;

  PROCEDURE SelectPhrase*(T: Tab; P: Phrase);
  BEGIN
    IF P # NIL THEN P.selected := TRUE; T.sel := P END
  END SelectPhrase;

  PROCEDURE SelectArea*(T: Tab; x0, y0, x1, y1: INTEGER);
    VAR P: Phrase; t: INTEGER;
  BEGIN P := T.first;
    IF x1 < x0 THEN t := x0; x0 := x1; x1 := t END ;
    IF y1 < y0 THEN t := y0; y0 := y1; y1 := t END ;
    WHILE P # NIL DO
      IF (x0 <= P.x) & (P.x + P.w <= x1) & (y0 <= P.y) & (P.y + P.h <= y1) THEN
        P.selected := TRUE; T.sel := P
      END ;
      P := P.next
    END ;
    IF T.sel # NIL THEN  END
  END SelectArea;

  PROCEDURE Draw*(T: Tab; VAR m: Msg);
    VAR P: Phrase;
  BEGIN P := T.first;
    WHILE P # NIL DO P.do.draw(P, m); P := P.next END
  END Draw;

(*
  PROCEDURE List*(T: Tab);
    VAR P: Phrase; tag: INTEGER;
  BEGIN P := T.first;
    WHILE P # NIL DO
      Texts.Write(XW, 9X); Texts.WriteHex(XW, ORD(P)); Texts.Write(XW, 9X);
      Texts.WriteInt(XW, P.x, 5); Texts.WriteInt(XW, P.y, 5); Texts.WriteInt(XW, P.w, 5); Texts.WriteInt(XW, P.h, 5);
      Texts.Write(XW, "/"); SYSTEM.GET(ORD(P)-8, tag); Texts.WriteHex(XW, tag);
      SYSTEM.GET(ORD(P)-4, tag); Texts.WriteHex(XW, tag); Texts.WriteLn(XW); P := P.next
    END ;
    Texts.Append(Oberon.Log, XW.buf)
  END List;
*)

  (*----------------procedures operating on selection -------------------*)

  PROCEDURE Deselect*(T: Tab);
    VAR P: Phrase;
  BEGIN P := T.first; T.sel := NIL; 
    WHILE P # NIL DO P.selected := FALSE; P := P.next END
  END Deselect;

  PROCEDURE DrawSel*(T: Tab; VAR m: Msg);
    VAR P: Phrase;
  BEGIN P := T.first;
    WHILE P # NIL DO
      IF P.selected THEN P.do.draw(P, m) END ;
      P := P.next
    END
  END DrawSel;

  PROCEDURE Change*(T: Tab; VAR m: Msg);
    VAR P: Phrase;
  BEGIN P := T.first; T.changed := TRUE;
    WHILE P # NIL DO
      IF P.selected THEN P.do.change(P, m) END ;
      P := P.next
    END
  END Change;



  PROCEDURE WriteFile*(T: Tab; name: ARRAY OF CHAR);
    VAR f: Files.File; w: Files.Rider; 
  BEGIN f := Files.New(name); Files.Set(w, f, 0); 
  (*  Files.Write(w, GraphFileId); InitContext(c); StoreElems(w, c, T.first);  *)
    Files.Register(f)
  END WriteFile;


  PROCEDURE SetWidth*(w: INTEGER);
  BEGIN width := w
  END SetWidth;


  PROCEDURE Open*(T: Tab; name: ARRAY OF CHAR);
    VAR tag: CHAR;
          b: BYTE;
          i: INTEGER;
      f: Files.File; r: Files.Rider; 
  BEGIN T.first := NIL; T.sel := NIL; T.changed := FALSE; f := Files.Old(name);
    IF f # NIL THEN
      Files.Set(r, f, 0);
      Files.ReadByte(r, b); i:=b;
      Files.ReadByte(r, b); i:=i+(b*100H);
     
    ELSE res := 2
    END
  END Open;



  PROCEDURE Delete*(T: Tab);
    VAR P, pred: Phrase;
  BEGIN T.sel := NIL; T.changed := TRUE; P := T.first;
    WHILE (P # NIL) & P.selected DO P := P.next END ;
    T.first := P;
    IF P # NIL THEN
      pred := P; P := P.next;
      WHILE P # NIL DO
        IF P.selected THEN pred.next := P.next ELSE pred := P END ;
        P := P.next
      END
    END
  END Delete;






  PROCEDURE InstallDrawMethods*(drawLine, drawCaption, drawMacro: PROCEDURE (P: Phrase; VAR msg: Msg));
  BEGIN LineMethod.draw := drawLine; CapMethod.draw := drawCaption; MacMethod.draw := drawMacro
  END InstallDrawMethods;

BEGIN 
END Tabs.

