
functor
import System(showInfo:ShowInfo)
define
   Size= 100000
   Half = 50000
   L
   T = {MakeTuple t Size}
   fun {MakeListDescending Len}
      if Len == 0
      then nil
      else
         if Len > Half
         then Len|{MakeListDescending Len-1}
         else Len+1|{MakeListDescending Len-1}
         end
      end
   end
   T1
   T2
   proc {WalkList I}
      N = {Nth L I}
   in
      if N == Half
      then {ShowInfo "ENDLIST"}
      else {WalkList N}
      end
   end
   proc {WalkTup I}
      N = T.I
   in
      if N == Half
      then {ShowInfo "ENDTUP"}
      else {WalkTup N}
      end
   end
in
   for I in 1..Size do
      if I > Half
      then T.I = (Size-I+2)
      else T.I = (Size-I+1)
      end
   end
   L = {MakeListDescending Size}
   /*for I in 1..Size do
      {ShowInfo " T "#I#": "#T.I}
   end
   for I in L do
      {ShowInfo " L:"#I}
   end
   */
   T2 = {Time.time}
   {WalkTup 1}
   {ShowInfo {Time.time}-T2}

   T1 = {Time.time}
   {WalkList 1}
   {ShowInfo {Time.time}-T1}



end
