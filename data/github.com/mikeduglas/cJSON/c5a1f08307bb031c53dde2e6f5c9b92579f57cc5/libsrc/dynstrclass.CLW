  MEMBER
  MAP
  END
  
  INCLUDE('dynstr.inc'), ONCE
  INCLUDE('dynstrclass.inc'), ONCE

!region IDynStr

DynStr.Construct    PROCEDURE()
  CODE
  SELF.ds &= NewDynStr()
  
DynStr.Destruct     PROCEDURE()
  CODE
  SELF.Kill()
  DisposeDynStr(SELF.ds)
  
DynStr.GetInterface           PROCEDURE()
  CODE
  RETURN SELF.ds
  
DynStr.Str          PROCEDURE()
  CODE
  RETURN SELF.ds.Str()
  
DynStr.CStr         PROCEDURE()
  CODE
  RETURN SELF.ds.CStr()
  
DynStr.Kill         PROCEDURE()
  CODE
  SELF.ds.Kill()
  
DynStr.Cat          PROCEDURE(STRING s)
  CODE
  SELF.ds.Cat(s)
  
DynStr.Cat          PROCEDURE(*STRING s)
  CODE
  SELF.ds.Cat(s)
  
DynStr.Cat          PROCEDURE(*DECIMAL s)
  CODE
  SELF.ds.Cat(s)
  
DynStr.Cat          PROCEDURE(*CSTRING s)
  CODE
  SELF.ds.Cat(s)
  
DynStr.CatNum       PROCEDURE(LONG l,USHORT Base=10)
  CODE
  SELF.ds.CatNum(l, Base)
  
DynStr.CatChr       PROCEDURE(BYTE chr)
  CODE
  SELF.ds.CatChr(chr)
  
DynStr.Clip         PROCEDURE()
  CODE
  SELF.ds.Clip()
  
DynStr.Trunc        PROCEDURE(LONG newlen)
  CODE
  SELF.ds.Trunc(newlen)
  
DynStr.StrLen       PROCEDURE()
  CODE
  RETURN SELF.ds.StrLen()
  
DynStr.BinaryToText PROCEDURE(STRING s)
  CODE
  SELF.ds.BinaryToText(s)
  
DynStr.TextToBinary PROCEDURE(STRING s,LONG pos=1)
  CODE
  RETURN SELF.ds.TextToBinary(s, pos)
  
DynStr.PadTo        PROCEDURE(LONG l)
  CODE
  SELF.ds.PadTo(l)
  
DynStr.CopyTo       PROCEDURE(*STRING s,LONG limit=-1)
  CODE
  SELF.ds.CopyTo(s, limit)
  
DynStr.CopyTo              PROCEDURE(*CSTRING s,LONG limit=-1)
  CODE
  SELF.ds.CopyTo(s, limit)
  
DynStr.GetChr       PROCEDURE(LONG index)
  CODE
  RETURN SELF.ds.GetChr(index)
  
DynStr.CStrRef             PROCEDURE()
  CODE
  RETURN SELF.ds.CStrRef()
  
DynStr.CDynStrRef   PROCEDURE()
  CODE
  RETURN SELF.ds.CDynStrRef()
  
DynStr.Release      PROCEDURE()
  CODE
  SELF.ds.Release()

!endregion IDynStr

!region new stuff
  
DynStr.Replace      PROCEDURE(STRING pPattern, STRING pReplacement)
tmp                   DynStr

frompos               LONG, AUTO
topos                 LONG, AUTO

srcLen                LONG, AUTO
patternLen            LONG, AUTO


  CODE
  srcLen = SELF.StrLen()
  patternLen = LEN(pPattern)
  
  frompos = 1
  LOOP
    topos = INSTRING(pPattern, SELF.Str(), 1, frompos)
    IF topos = 0
      IF frompos <= srcLen
        tmp.Cat(SUB(SELF.Str(), frompos, srcLen - frompos + 1))
      END
      
      BREAK
    END
    
    IF frompos < topos
      tmp.Cat(SUB(SELF.Str(), frompos, topos - frompos))
    END
    
    tmp.Cat(pReplacement)
    frompos = topos + patternLen
  END
  
  SELF.Trunc(0)
  SELF.Cat(tmp.Str())
  
  RETURN SELF.Str()

!endregion new stuff
