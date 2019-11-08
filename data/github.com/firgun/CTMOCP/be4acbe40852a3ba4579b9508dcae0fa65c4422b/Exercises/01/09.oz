declare
fun {NewStore}
   D={NewDictionary}
   C={NewCell 0}
   proc {Put K V}
      if {Not {Dictionary.member D K}} then
	 C:=@C+1
      end
      D.K:=V
   end
   fun {Get K} D.K end
   fun {Size} @C end
in
   storeobject(put:Put get:Get size:Size)
end
proc {Put S K V} {S.put K V} end
fun {Get S K} {S.get K} end
fun {Size S} {S.size} end

declare
S={NewStore}
{Put S 2 [1 2]}
{Browse {Get S 2}}

declare FastPascal FasterPascal ShiftLeft ShiftRight AddList

fun {FastPascal N}
   if N==1 then [1]
   else L={FastPascal N-1} in
      {AddList {ShiftLeft L} {ShiftRight L}}
   end
end

local S in
   S={NewStore}
   {Put S 1 [1]}
   fun {FasterPascal N}
      if N>{Size S} then
	 for I in {Size S}+1..N do L={Get S I-1} in
	    {Put S I {AddList {ShiftLeft L} {ShiftRight L}}}
	 end
      end
      {Get S N}
   end
end

fun {ShiftLeft L}
   case L of H|T then
      H|{ShiftLeft T}
   else [0] end
end

fun {ShiftRight L} 0|L end

fun {AddList L1 L2}
   case L1 of H1|T1 then
      case L2 of H2|T2 then
	 H1+H2|{AddList T1 T2}
      end
   else nil end
end

declare
N=4

{Browse {FastPascal N}}
{Browse {FastPascal N}}

{Browse {FasterPascal N}}
{Browse {FasterPascal N}}

declare NewStore Get Set Size
fun {NewStore}
   C S PutIn GetIn Put Get Size in
   C={NewCell 0}
   S={NewCell nil}
   fun {PutIn L K V}
      case L of H|T then
	 case H of Key|Val then
	    if Key==K then (K|V)|T
	    else (Key|Val)|{PutIn T K V} end
	 end
      else
	 C:=@C+1
	 [K|V]
      end
   end
   fun {GetIn L K}
      case L of H|T then
	 case H of Key|Val then
	    if Key==K then Val else {GetIn T K} end
	 end
      else false end
   end
   fun {Put K V} S:={PutIn @S K V} end
   fun {Get K} {GetIn @S K} end
   fun {Size} @C end
   storeobject(put:Put get:Get size:Size)
end

{Browse 'NewStore'}

declare
S={NewStore}
{Browse {S.size}}
{Browse {S.put 1 2}}
{Browse {S.get 1}}
{Browse {S.size}}