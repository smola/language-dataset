MODULE ORL;  (*Oberon boot linker/loader for RISC / AP 1.11.19 FPGA Oberon 2013*)
  IMPORT SYSTEM, Kernel, Files, Modules, Texts, Oberon;
  CONST versionkey = 1X; versionkey0 = 0X; MT = 12; DescSize = 80; MnLength = 32;
    noerr* = 0; nofile* = 1; badversion* = 2; badkey* = 3; badfile* = 4; nospace* = 5;

  TYPE  (*copied from Modules for use as cross linker/loader*)
    Module* = POINTER TO ModDesc;
    Command* = PROCEDURE;
    ModuleName* = ARRAY MnLength OF CHAR;
    ModDesc* = RECORD
      name*: ModuleName;
      next*: Module;
      key*, num*, size*, refcnt*: INTEGER;
      data*, code*, imp*, cmd*, ent*, ptr*, unused: INTEGER  (*addresses*)
    END ;

  VAR root: Module;
    MTOrg, AllocPtr, Start, limit, res*: INTEGER;
    importing*, imported*: ModuleName;
    W: Texts.Writer;

  PROCEDURE MakeFileName(VAR FName: ARRAY OF CHAR; name, ext: ARRAY OF CHAR);
    VAR i, j: INTEGER;
  BEGIN i := 0; j := 0;  (*assume name suffix less than 4 characters*)
    WHILE (i < MnLength-5) & (name[i] > 0X) DO FName[i] := name[i]; INC(i) END ;
    REPEAT FName[i]:= ext[j]; INC(i); INC(j) UNTIL ext[j] = 0X;
    FName[i] := 0X
  END MakeFileName;

  PROCEDURE ThisFile(name: ARRAY OF CHAR): Files.File;
    VAR F: Files.File;
      filename: ModuleName;
  BEGIN MakeFileName(filename, name, ".rsc"); F := Files.Old(filename); RETURN F
  END ThisFile;

  PROCEDURE error(n: INTEGER; name: ARRAY OF CHAR);
  BEGIN res := n; importing := name
  END error;

  PROCEDURE Check(s: ARRAY OF CHAR);
    VAR i: INTEGER; ch: CHAR;
  BEGIN ch := s[0]; res := 1; i := 1;
    IF (ch >= "A") & (ch <= "Z") OR (ch >= "a") & (ch <= "z") THEN
      REPEAT ch := s[i]; INC(i)
      UNTIL ~((ch >= "0") & (ch <= "9") OR (ch >= "A") & (ch <= "Z")
        OR (ch >= "a") & (ch <= "z") OR (ch = ".")) OR (i = MnLength);
      IF (i < MnLength) & (ch = 0X) THEN res := 0 END
    END
  END Check;

  PROCEDURE LinkOne(name: ARRAY OF CHAR; VAR newmod: Module);
    (*search module in list; if not found, link module.
      res = noerr: already present or linked;
      res = nofile: file not available;
      res = badversion: bad file version;
      res = badkey: key conflict;
      res = badfile: corrupted file;
      res = nospace: insufficient space*)
    VAR mod, impmod: Module;
      i, n, key, impkey, mno, nofimps, size: INTEGER;
      p, u, v, w: INTEGER;  (*addresses*)
      ch: CHAR;
      body: Command;
      fixorgP, fixorgD, fixorgT: INTEGER;
      disp, adr, inst, pno, vno, dest, offset: INTEGER;
      name1, impname: ModuleName;
      F: Files.File; R: Files.Rider;
      import: ARRAY 16 OF Module;
  BEGIN mod := root; error(noerr, name); nofimps := 0;
    WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END ;
    IF mod = NIL THEN (*link*)
      Check(name);
      IF res = noerr THEN F := ThisFile(name) ELSE F := NIL END ;
      IF F # NIL THEN
        Files.Set(R, F, 0); Files.ReadString(R, name1); Files.ReadInt(R, key); Files.Read(R, ch);
        Files.ReadInt(R, size); importing := name1;
        IF (ch = versionkey) (*regular module*) OR (ch = versionkey0) (*standalone program*) THEN
          Files.ReadString(R, impname);   (*imports*)
          WHILE (impname[0] # 0X) & (res = noerr) DO
            Files.ReadInt(R, impkey);
            LinkOne(impname, impmod); import[nofimps] := impmod; importing := name1;
            IF res = noerr THEN
              IF impmod.key = impkey THEN INC(impmod.refcnt); INC(nofimps)
              ELSE error(badkey, name1); imported := impname
              END
            END ;
            Files.ReadString(R, impname)
          END
        ELSE error(badversion, name1)
        END
      ELSE error(nofile, name)
      END ;
      IF res = noerr THEN
        INC(size, DescSize);
        IF AllocPtr + size < limit THEN (*allocate*)
          p := AllocPtr; mod := SYSTEM.VAL(Module, p);
          AllocPtr := (p + size + 3) DIV 4 * 4; mod.size := AllocPtr - p;
          IF root = NIL THEN mod.num := 1 ELSE mod.num := root.num + 1 END ;
          mod.next := root; root := mod
        ELSE error(nospace, name1)
        END
      END ;
      IF res = noerr THEN (*read file*)
        INC(p, DescSize); (*allocate descriptor*)
        mod.name := name; mod.key := key; mod.refcnt := 0;
        mod.data := p;  (*data*)
        SYSTEM.PUT(mod.num * 4 + MTOrg, p);  (*module table entry*)
        Files.ReadInt(R, n);
        WHILE n > 0 DO Files.ReadInt(R, w); SYSTEM.PUT(p, w); INC(p, 4); DEC(n, 4) END ;  (*type descriptors*)
        Files.ReadInt(R, n);
        WHILE n > 0 DO SYSTEM.PUT(p, 0); INC(p, 4); DEC(n, 4) END ;  (*variable space*)
        Files.ReadInt(R, n);
        WHILE n > 0 DO Files.Read(R, ch); SYSTEM.PUT(p, ch); INC(p); DEC(n) END ;   (*strings*)
        mod.code := p;  (*program*)
        Files.ReadInt(R, n);
        WHILE n > 0 DO Files.ReadInt(R, w); SYSTEM.PUT(p, w); INC(p, 4); DEC(n) END ;  (*program code*)
        mod.imp := p;  (*copy imports*)
        i := 0;
        WHILE i < nofimps DO
          SYSTEM.PUT(p, import[i]); INC(p, 4); INC(i)
        END ;
        mod.cmd := p;  (*commands*) Files.Read(R, ch);
        WHILE ch # 0X DO
          REPEAT SYSTEM.PUT(p, ch); INC(p); Files.Read(R, ch) UNTIL ch = 0X;
          REPEAT SYSTEM.PUT(p, 0X); INC(p) UNTIL p MOD 4 = 0;
          Files.ReadInt(R, n); SYSTEM.PUT(p, n); INC(p, 4); Files.Read(R, ch)
        END ;
        REPEAT SYSTEM.PUT(p, 0X); INC(p) UNTIL p MOD 4 = 0;
        mod.ent := p;  (*entries*)
        Files.ReadInt(R, n);
        WHILE n > 0 DO Files.ReadInt(R, w); SYSTEM.PUT(p, w); INC(p, 4); DEC(n) END ;
        mod.ptr := p;  (*pointer references*)
        Files.ReadInt(R, w);
        WHILE w >= 0 DO SYSTEM.PUT(p, mod.data + w - Start); INC(p, 4); Files.ReadInt(R, w) END ;
        SYSTEM.PUT(p, 0); INC(p, 4);
        Files.ReadInt(R, fixorgP); Files.ReadInt(R, fixorgD); Files.ReadInt(R, fixorgT);
        Files.ReadInt(R, w); body := SYSTEM.VAL(Command, mod.code + w - Start);
        Files.Read(R, ch);
        IF ch # "O" THEN mod := NIL; error(badfile, name) END
      END ;
      IF res = noerr THEN (*fixup of BL*)
        adr := mod.code + fixorgP*4;
        WHILE adr # mod.code DO
          SYSTEM.GET(adr, inst);
          mno := inst DIV 100000H MOD 10H;
          pno := inst DIV 1000H MOD 100H;
          disp := inst MOD 1000H;
          SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
          SYSTEM.GET(impmod.ent + pno*4, dest); dest := dest + impmod.code;
          offset := (dest - adr - 4) DIV 4;
          SYSTEM.PUT(adr, (offset MOD 1000000H) + 0F7000000H);
          adr := adr - disp*4
        END ;
        (*fixup of LDR/STR/ADD*)
        adr := mod.code + fixorgD*4;
        WHILE adr # mod.code DO
          SYSTEM.GET(adr, inst);
          mno := inst DIV 10000H MOD 100H;
          disp := inst MOD 10000H;
          IF mno = 0 THEN (*global*)
            SYSTEM.PUT(adr, (inst DIV 1000000H) * 1000000H + (mod.data - Start) DIV 10000H); (*MOVu*)
            SYSTEM.GET(adr+4, inst);
            SYSTEM.PUT(adr+4, inst + (mod.data - Start) MOD 10000H) (*IOR*)
          ELSE (*import*)
            SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
            SYSTEM.PUT(adr, (inst DIV 1000000H) * 1000000H + (impmod.data - Start) DIV 10000H); (*MOVu*)
            SYSTEM.GET(adr+4, inst);
            SYSTEM.PUT(adr+4, inst + (impmod.data - Start) MOD 10000H); (*IOR*)
            SYSTEM.GET(adr+8, inst); vno := inst MOD 100H;
            SYSTEM.GET(impmod.ent + vno*4, offset);
            IF ODD(inst DIV 100H) THEN offset := offset + impmod.code - impmod.data END ;
            SYSTEM.PUT(adr+8, inst DIV 10000H * 10000H + offset)
          END ;
          adr := adr - disp*4
        END ;
        (*fixup of type descriptors*)
        adr := mod.data + fixorgT*4;
        WHILE adr # mod.data DO
          SYSTEM.GET(adr, inst);
          mno := inst DIV 1000000H MOD 10H;
          vno := inst DIV 1000H MOD 1000H;
          disp := inst MOD 1000H;
          IF mno = 0 THEN (*global*) inst := mod.data - Start + vno
          ELSE (*import*)
            SYSTEM.GET(mod.imp + (mno-1)*4, impmod);
            SYSTEM.GET(impmod.ent + vno*4, offset);
            inst := impmod.data - Start + offset
          END ;
          SYSTEM.PUT(adr, inst); adr := adr - disp*4
        END ;
        SYSTEM.PUT(Start, body) (*module initialization body*)
      ELSIF res >= badkey THEN importing := name;
        WHILE nofimps > 0 DO DEC(nofimps); DEC(import[nofimps].refcnt) END
      END
    END ;
    newmod := mod
  END LinkOne;

  PROCEDURE Link*;  (*link multiple object files together and create a single boot file F.bin from them*)
    VAR i, x: INTEGER;
      F: Files.File; R: Files.Rider;
      S: Texts.Scanner;
      M, p: Module;
      name: ModuleName;
  BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
    IF S.class = Texts.Name THEN
      root := NIL; Start := Modules.AllocPtr; MTOrg := Start + 32; AllocPtr :=  Start + 256; i := Start;
      WHILE i < AllocPtr DO SYSTEM.PUT(i, 0); INC(i, 4) END ;
      REPEAT LinkOne(S.s, M); Texts.Scan(S) UNTIL (S.class # Texts.Name) OR (res # noerr);
      IF res = noerr THEN p := root; Texts.WriteString(W, "  linking");
        WHILE p # NIL DO (*fixup*) Texts.Write(W, " "); Texts.WriteString(W, p.name);
          M := p; p := p.next;
          IF M.next # NIL THEN M.next := SYSTEM.VAL(Module, SYSTEM.VAL(INTEGER, M.next) - Start) END ;
          M.data := M.data - Start;
          SYSTEM.PUT(M.num * 4 + MTOrg, M.data);  (*module table entry*)
          M.code := M.code - Start;
          i := M.imp;
          WHILE i < M.cmd DO SYSTEM.GET(i, x); SYSTEM.PUT(i, x - Start); INC(i, 4) END ;
          M.imp := M.imp - Start;
          M.cmd := M.cmd - Start;
          M.ent := M.ent - Start;
          M.ptr := M.ptr - Start
        END ;
        SYSTEM.GET(Start, x);
        SYSTEM.PUT(Start, 0E7H * 1000000H + (x DIV 4) - 1);  (*branch instruction to the initialization body of the top module*)
        SYSTEM.PUT(Start + 12, Kernel.MemLim);  (*limit of available memory; typically overwritten by the boot loader*)
        SYSTEM.PUT(Start + 16, AllocPtr - Start);  (*address of the end of the module space loaded*)
        SYSTEM.PUT(Start + 20, SYSTEM.VAL(INTEGER, root) - Start);  (*current root of the links of loaded modules*)
        SYSTEM.PUT(Start + 24, Kernel.stackOrg);  (*current limit of the module area; typically overwritten by the boot loader*)
        SYSTEM.PUT(Start + 252, 12345678H);  (*fingerprint*)
        MakeFileName(name, S.s, ".bin"); F := Files.New(name); Files.Set(R, F, 0); i := Start;
        WHILE i < AllocPtr DO SYSTEM.GET(i, x); Files.WriteInt(R, x); INC(i, 4) END ;
        Texts.WriteInt(W, AllocPtr - Start, 7); Files.Register(F)
      ELSE Texts.WriteString(W, "Link error:  "); Texts.WriteString(W, importing);
        IF res = nofile THEN Texts.WriteString(W, " module not found")
        ELSIF res = badversion THEN Texts.WriteString(W, " bad version")
        ELSIF res = badkey THEN Texts.WriteString(W, " imports ");
          Texts.WriteString(W, imported); Texts.WriteString(W, " with bad key")
        ELSIF res = badfile THEN Texts.WriteString(W, " corrupted obj file")
        ELSIF res = nospace THEN Texts.WriteString(W, " insufficient space")
        END
      END
    ELSE Texts.WriteString(W, "Usage: ORL.Link [module...] topmodule")
    END ;
    Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
  END Link;

  PROCEDURE Load*;  (*load prelinked boot file F.bin onto the boot area of the local disk, starting at sector 2*)
    VAR i, secno: LONGINT; b: BYTE;
      F: Files.File; R: Files.Rider;
      S: Texts.Scanner;
      buf: ARRAY Kernel.SectorLength OF BYTE;
  BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
    IF S.class = Texts.Name THEN (*input file name*)
      Texts.WriteString(W, "  loading "); Texts.WriteString(W, S.s); F := Files.Old(S.s);
      IF F # NIL THEN Texts.WriteString(W, " onto boot area"); Texts.WriteInt(W, Files.Length(F), 7);
        secno := 2; i := 0; Files.Set(R, F, 0); Files.ReadByte(R, b);
        WHILE ~R.eof DO buf[i] := b; INC(i);
          IF i = Kernel.SectorLength THEN Kernel.PutSector(secno*29, buf); INC(secno); i := 0 END ;
          Files.ReadByte(R, b)
        END ;
        IF i > 0 THEN
          WHILE i < Kernel.SectorLength DO buf[i] := 0; INC(i) END ;
          Kernel.PutSector(secno*29, buf)
        END
      ELSE Texts.WriteString(W, " not found")
      END
    ELSE Texts.WriteString(W, "Usage: ORL.Load M.bin")
    END ;
    Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
  END Load;

BEGIN Texts.OpenWriter(W); Texts.WriteString(W, "OR Boot linker/loader  AP 1.11.19");
  Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf); limit := Kernel.stackOrg - Kernel.stackSize
END ORL.
