{Browse 9999*9999}

declare
V=9999*9999
{Browse V*V}

{Browse 1*2*3*4*5*6*7*8*9*10}

declare
fun {Fact N}
   if N==0 then 1 else N*{Fact N-1} end
end

{Browse {Fact 10}}

declare
fun {Comb N R}
   {Fact N} div ({Fact R}*{Fact N-R})
end

{Browse [5 6 7 8]}

declare
H=5
T=[6 7 8]
{Browse H|T}

declare
L=[5 6 7 8]
{Browse L.1}
{Browse L.2}

declare
L=[5 6 7 8]
case L of H|T then {Browse H} {Browse T} end

declare Pascal AddList ShiftLeft ShiftRight
fun {Pascal N}
   if N==1 then [1]
   else
      {AddList {ShiftLeft {Pascal N-1}}
       {ShiftRight {Pascal N-1}}}
   end
end

fun {ShiftLeft L}
   case L of H|T then
      H|{ShiftLeft T}
   else [0] end
end

fun {ShiftRight L}
   0|L end

fun {AddList L1 L2}
   case L1 of H1|T1 then
      case L2 of H2|T2 then
         H1+H2|{AddList T1 T2}
      end
   else nil end
end

fun {FastPascal N}
   if N==1 then [1]
   else L in
      L = {FastPascal N-1}
      {AddList {ShiftLeft L} {ShiftRight L}}
   end
end


{Browse {FastPascal 20}}

declare Ints
fun lazy {Ints N}
   N|{Ints N=1}
end

L={Ints 0}
{Browse L}

{Browse L.3}

declare
fun lazy {PascalList Row}
   Row|{PascalList
        {AddList {ShiftLeft Row}
         {ShiftRight Row}}}
end

declare
L={PascalList [1]}
{Browse L}

{Browse L.1}
{Browse L.2.1}

declare
fun {PascalList2 N Row}
   if N==1 then [Row]
   else
      Row|{PascalList2 N-1
           {AddList {ShiftLeft Row}
            {ShiftRight Row}}}
   end
end

{Browse {PascalList2 11 [1]}}

declare GenericPascal OpList
fun {GenericPascal Op N}
   if N==1 then [1]
   else L in
      L={GenericPascal Op N-1}
      {OpList Op {ShiftLeft L} {ShiftRight L}}
   end
end

declare
fun {OpList Op L1 L2}
   case L1 of H1|T1 then
      case L2 of H2|T2 then
         {Op H1 H2}|{OpList Op T1 T2}
      end
   else nil end
end

declare
fun {Add X Y} X+Y end
declare
fun {FastPascal N} {GenericPascal Add N} end
declare
fun {Xor X Y} if X==Y then 0 else 1 end end

declare
thread P in
   P={Pascal 30}
   {Browse P}
end
{Browse 99*99}

declare X in
thread {Delay 10000} X=99 end
{Browse start} {Browse X*x}

declare X in
thread {Browse start} {Browse X*X} end
{Delay 10000} X=99

declare
C={NewCell 0}
C:=@C+1
{Browse @C}

declare
C={NewCell 0}
fun {FastPascal N}
   C:=@C+1
   {GenericPascal Add N}
end

declare
local C in
   C={NewCell 0}
   fun {Bump}
      C:=@C+1
      @C
   end
   fun {Read}
      @C
   end
end

{Browse {Bump}}

declare
fun {FastPascal N}
   {Browse {Bump}}
   {GenericPascal Add N}
end

declare
fun {NewCounter}
   C Bump Read in
   C={NewCell 0}
   fun {Bump}
      C:=@C+1
      @C
   end
   fun {Read}
      @C
   end
   counter(bump:Bump read:Read)
end

declare
Ctr1={NewCounter}
Ctr2={NewCounter}

{Browse {Ctr1.bump}}

declare
C={NewCell 0}
thread
   C:=1
end
thread
   C:=2
end

declare
C={NewCell 0}
thread I in
   I=@C
   C:=I+1
end
thread J in
   J=@C
   C:=J+1
end

declare
C={NewCell 0}
L={NewLock}
thread
   lock L then I in
      I=@C
      C:=I+1
   end
end
thread
   lock L then J in
      J=@C
      C:=J+1
   end
end


declare
C = 10
