MODULE merge_rsx;

IMPORT IOConsts, ChanConsts, IOChan, RndFile;
IMPORT SYSTEM, Strings, WholeStr;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

CONST ibLen = 12*1024*1024;
      obLen =   1024;
      rbLen = 4*1024;
      runLimit = 256; 

TYPE  Key = CARDINAL;

VAR 
    inbuf, aux:  ARRAY [1..ibLen] OF Key; 
    output: IOChan.ChanId;
    outPtr: INTEGER;
    outBuf: ARRAY [1..obLen] OF Key; 

  PROCEDURE OpenOut;
  VAR res  : ChanConsts.OpenResults;
  BEGIN     
    RndFile.OpenClean(output, 'output', RndFile.old+RndFile.raw, res);
    outPtr := 0;
  END OpenOut;

  PROCEDURE FlushOut;
  BEGIN
    IOChan.RawWrite(output, SYSTEM.ADR(outBuf), outPtr*SIZE(Key));
    outPtr := 0;
  END FlushOut;

  PROCEDURE PutOut(x : Key);
  BEGIN     
    INC(outPtr);
    outBuf[outPtr] := x;
    IF outPtr = obLen THEN FlushOut END;
  END PutOut;

  PROCEDURE CloseOut;
  BEGIN     
    IF outPtr > 0 THEN FlushOut END;
    RndFile.Close(output);
  END CloseOut;

  TYPE 
     RunBuf = ARRAY [0..rbLen-1] OF CHAR;
     Run = RECORD
        ch: IOChan.ChanId;
        ptr: CARDINAL;
        len: CARDINAL;
        last: Key;
        buf: POINTER TO RunBuf; 
      END;
  VAR 
    runCount: INTEGER;
    run:  ARRAY [1..runLimit] OF Run;
    
  PROCEDURE NewRun;
  VAR res  : ChanConsts.OpenResults;
      name: ARRAY [0..16] OF CHAR;
  BEGIN
    WholeStr.IntToStr(runCount, name);
    Strings.Insert("run", 0, name);
    INC(runCount);
    WITH run[runCount] DO
      RndFile.OpenClean(ch, name, RndFile.write+RndFile.read+RndFile.old+RndFile.raw, res);
      ptr := 0;
      len := 0;
      last:= 0;
      NEW(buf);
    END
  END NewRun;

  PROCEDURE FlushRun(VAR r:Run);
  BEGIN
    IOChan.RawWrite(r.ch, SYSTEM.ADR(r.buf^), r.ptr);
    r.ptr := 0;
  END FlushRun;

  PROCEDURE ToRun(x: Key);
  VAR dx: Key;
  BEGIN
    WITH run[runCount] DO
      IF ptr + 4 >= SIZE(RunBuf) THEN FlushRun(run[runCount]) END;
      dx := x - last;
      WHILE (dx > 127) DO
        buf^[ptr] := CHR(dx MOD 128 + 128);
        INC(ptr);
        dx := dx DIV 128
      END;
      buf^[ptr] := CHR(dx);
      INC(ptr);
      last := x;
    END
  END ToRun;

  PROCEDURE FillRunBuf(VAR r:Run);
  BEGIN
    r.ptr := 0;
    IOChan.RawRead(r.ch, SYSTEM.ADR(r.buf^), SIZE(RunBuf), r.len);
    IF r.len = 0 THEN  RndFile.Close(r.ch) END;
  END FillRunBuf;

  PROCEDURE ResetRuns;
   VAR i : INTEGER;
  BEGIN
    FOR i := 1 TO runCount DO
      IF run[i].ptr > 0 THEN  FlushRun(run[i]) END;
      RndFile.SetPos(run[i].ch, RndFile.StartPos(run[i].ch));
      FillRunBuf(run[i]);
      run[i].last:= 0;
    END;
  END ResetRuns;

  PROCEDURE NextByte(VAR r:Run):CHAR;
  VAR b : CHAR;
  BEGIN
    IF r.ptr >= r.len THEN
      IF r.len = 0 THEN  RETURN 0C END;
      FillRunBuf(r);
    END;
    b := r.buf^[r.ptr];
    INC(r.ptr);
    RETURN b
  END NextByte;

  PROCEDURE FromRun(i: INTEGER; VAR res:Key):BOOLEAN;
  VAR b, x, s:CARDINAL;
  BEGIN
    x := 0; s := 1;
    b := ORD(NextByte(run[i]));
    WHILE b > 127 DO
      INC(x, (b - 128)*s);
      s := s * 128;
      b := ORD(NextByte(run[i]));
    END;
    INC(x, b * s);
    res := run[i].last + x;
    run[i].last := res;
    RETURN run[i].len > 0;
  END FromRun;

  PROCEDURE SortInBuff(len: INTEGER);
  VAR k: CARDINAL;
      i, s, t: INTEGER;
      pos0,pos1,pos2,pos3 : ARRAY [0..255] OF INTEGER;
  BEGIN
     FOR k := 0 TO 255 DO pos0[k] := 0 END;
     FOR k := 0 TO 255 DO pos1[k] := 0 END;
     FOR k := 0 TO 255 DO pos2[k] := 0 END;
     FOR k := 0 TO 255 DO pos3[k] := 0 END;

     FOR i := 1 TO len DO
       INC(pos0[inbuf[i] MOD 256]);
       INC(pos1[inbuf[i] DIV 256 MOD 256]);
       INC(pos2[inbuf[i] DIV (256*256) MOD 256]);
       INC(pos3[inbuf[i] DIV (256*256*256) MOD 256]);
     END;
     s := 1; FOR k := 0 TO 255 DO t := s + pos0[k]; pos0[k] := s; s := t; END;
     s := 1; FOR k := 0 TO 255 DO t := s + pos1[k]; pos1[k] := s; s := t; END;
     s := 1; FOR k := 0 TO 255 DO t := s + pos2[k]; pos2[k] := s; s := t; END;
     s := 1; FOR k := 0 TO 255 DO t := s + pos3[k]; pos3[k] := s; s := t; END;

     FOR i := 1 TO len DO k:= inbuf[i] MOD 256;  aux[pos0[k]] := inbuf[i]; INC(pos0[k]) END;
     FOR i := 1 TO len DO k:= aux[i] DIV 256 MOD 256;  inbuf[pos1[k]] := aux[i]; INC(pos1[k]) END;
     FOR i := 1 TO len DO k:= inbuf[i] DIV (256*256) MOD 256; aux[pos2[k]] := inbuf[i]; INC(pos2[k])  END;
     FOR i := 1 TO len DO k:= aux[i] DIV (256*256*256) MOD 256; inbuf[pos3[k]] := aux[i]; INC(pos3[k]) END;
  END  SortInBuff;

  PROCEDURE Distribute;
  VAR  i, len, locsRead : CARDINAL;
       input: IOChan.ChanId;
       res  : ChanConsts.OpenResults;
  BEGIN
    RndFile.OpenOld(input, "input", RndFile.raw, res);
    runCount := 0;
    LOOP
      IOChan.RawRead(input, SYSTEM.ADR(inbuf), SIZE(inbuf), locsRead);
      IF locsRead < 4 THEN EXIT END;
      len := locsRead DIV 4;
      SortInBuff(len);
      NewRun;
      FOR i := 1 TO len DO ToRun(inbuf[i]) END;
    END;
    RndFile.Close(input);
  END Distribute;

TYPE HElem = RECORD key,idx:CARDINAL; END; 
VAR 
   heap: ARRAY [1..runLimit] OF HElem;

  PROCEDURE sift(i, upper: CARDINAL);
    VAR j:CARDINAL; x: HElem;
  BEGIN
    x := heap[i];
    j := 2*i;
    IF  (j < upper) & (heap[j].key > heap[j+1].key) THEN INC(j) END;
    WHILE (j <= upper) & (x.key > heap[j].key) DO
      heap[i] := heap[j];  
      i := j;  
      j := 2*j;
      IF (j < upper) & (heap[j].key > heap[j+1].key) THEN INC(j) END
    END;
    heap[i] := x
  END sift;

  PROCEDURE Merge;
    VAR i, done  : INTEGER; ok:BOOLEAN;
         res: ChanConsts.OpenResults;
         x : Key;
  BEGIN
    OpenOut;
    ResetRuns;
    FOR i := 1 TO runCount DO heap[i].idx:=i; ok := FromRun(i, heap[i].key) END;
    FOR i := runCount DIV 2 TO 1 BY -1 DO sift(i, runCount) END;
    done:=0;
    REPEAT
      PutOut(heap[1].key);
      ok := FromRun(heap[1].idx, heap[1].key);
      IF ~ok THEN
        heap[1] := heap[runCount];
        DEC(runCount);
      END;
      sift(1, runCount);
    UNTIL runCount < 1;
    CloseOut; 
  END Merge;


BEGIN
  Distribute; 
  Merge;
END merge_rsx.
